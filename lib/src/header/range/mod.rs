use crate::graph::GRAPH;
use crate::primitives::{
    uint_as_rational, BlockEpochStart, BlockFlags, BlockHash, BlockHeight, BlockNonce, BlockTarget,
    BlockTime, BlockWeight, ChainId, ChainwebVersion, PayloadHash, ToHeaderBytes, BLOCK_DELAY,
    CHAINWEB_VERSION, EPOCH_LENGTH, MAX_TARGET,
};
use blake2::{Blake2s256, Digest};
use compact::CompactLayer;
use ethnum::u256;
use num_rational::BigRational;

pub mod compact;

// -------------------------------------------------------------------------- //
// Internal Indexes

// Graph changes happen very rarely. We do not create internal indexes for them.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Epoch(u64);

impl Epoch {
    pub fn start_height(self) -> BlockHeight {
        BlockHeight(self.0 * EPOCH_LENGTH)
    }

    pub fn end_height(self) -> BlockHeight {
        BlockHeight((self.0 + 1) * EPOCH_LENGTH - 1)
    }

    pub fn pred(self) -> Epoch {
        Epoch(self.0 - 1)
    }

    pub fn succ(self) -> Epoch {
        Epoch(self.0 + 1)
    }
}

impl BlockHeight {
    pub fn epoch(self) -> Epoch {
        Epoch(self.0 / EPOCH_LENGTH)
    }

    pub fn is_epoch_start(self) -> bool {
        self.0 % EPOCH_LENGTH == 0
    }

    pub fn is_epoch_end(self) -> bool {
        (self.0 + 1) % EPOCH_LENGTH == 0
    }

    pub fn pred(self) -> BlockHeight {
        BlockHeight(self.0 - 1)
    }

    pub fn succ(self) -> BlockHeight {
        BlockHeight(self.0 + 1)
    }
    pub fn epoch_offset(self) -> u64 {
        self.0 % EPOCH_LENGTH
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct EpochIdx<const CHAIN_COUNT: usize>(usize);

impl<const CHAIN_COUNT: usize> EpochIdx<CHAIN_COUNT> {
    pub fn parent(self) -> EpochIdx<CHAIN_COUNT> {
        EpochIdx(self.0 - CHAIN_COUNT)
    }
    pub fn child(self) -> EpochIdx<CHAIN_COUNT> {
        EpochIdx(self.0 + CHAIN_COUNT)
    }
    pub fn new(epoch: Epoch, chain: ChainId) -> EpochIdx<CHAIN_COUNT> {
        EpochIdx(epoch.0 as usize * CHAIN_COUNT + chain.0 as usize)
    }
    pub fn row(epoch: Epoch) -> impl Iterator<Item = EpochIdx<CHAIN_COUNT>> {
        let start = epoch.0 as usize * CHAIN_COUNT;
        let end = start + CHAIN_COUNT;
        (start..end).map(|i| EpochIdx(i))
    }
}

struct EpochTable<const CHAIN_COUNT: usize, T> {
    v: Vec<T>,
    offset: usize,
}

impl<const CHAIN_COUNT: usize, T> std::ops::Index<EpochIdx<CHAIN_COUNT>>
    for EpochTable<CHAIN_COUNT, T>
{
    type Output = T;
    fn index(&self, idx: EpochIdx<CHAIN_COUNT>) -> &T {
        &self.v[idx.0 - self.offset * CHAIN_COUNT]
    }
}

impl<const CHAIN_COUNT: usize, T> EpochTable<CHAIN_COUNT, T> {
    pub fn at(&self, idx: EpochIdx<CHAIN_COUNT>) -> &T {
        &self.v[idx.0]
    }
    pub fn get(&self, epoch: Epoch, chain: ChainId) -> &T {
        &self.at(EpochIdx::<CHAIN_COUNT>::new(epoch, chain))
    }
    pub fn row(&self, epoch: Epoch) -> impl Iterator<Item = &T> {
        EpochIdx::<CHAIN_COUNT>::row(epoch).map(|i| self.at(i))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HeightIdx<const CHAIN_COUNT: usize>(usize);

impl<const CHAIN_COUNT: usize> HeightIdx<CHAIN_COUNT> {
    pub fn parent(self) -> HeightIdx<CHAIN_COUNT> {
        HeightIdx(self.0 - CHAIN_COUNT)
    }
    pub fn child(self) -> HeightIdx<CHAIN_COUNT> {
        HeightIdx(self.0 + CHAIN_COUNT)
    }
    pub fn new(height: BlockHeight, chain: ChainId) -> HeightIdx<CHAIN_COUNT> {
        HeightIdx(height.0 as usize * CHAIN_COUNT + chain.0 as usize)
    }
    pub fn row(height: BlockHeight) -> impl Iterator<Item = HeightIdx<CHAIN_COUNT>> {
        let start = height.0 as usize * CHAIN_COUNT;
        let end = start + CHAIN_COUNT;
        (start..end).map(|i| HeightIdx(i))
    }
    pub fn epoch_end_row(epoch: Epoch) -> impl Iterator<Item = HeightIdx<CHAIN_COUNT>> {
        HeightIdx::<CHAIN_COUNT>::row(epoch.end_height())
    }
}

struct HeightTable<const CHAIN_COUNT: usize, T> {
    v: Vec<T>,
    offset: usize,
}

impl<const CHAIN_COUNT: usize, T> std::ops::Index<HeightIdx<CHAIN_COUNT>>
    for HeightTable<CHAIN_COUNT, T>
{
    type Output = T;
    fn index(&self, idx: HeightIdx<CHAIN_COUNT>) -> &T {
        &self.v[idx.0 - self.offset * CHAIN_COUNT]
    }
}

impl<const CHAIN_COUNT: usize, T> HeightTable<CHAIN_COUNT, T> {
    pub fn at(&self, idx: HeightIdx<CHAIN_COUNT>) -> &T {
        &self.v[idx.0]
    }
    pub fn get(&self, height: BlockHeight, chain: ChainId) -> &T {
        &self.at(HeightIdx::<CHAIN_COUNT>::new(height, chain))
    }
    pub fn row(&self, height: BlockHeight) -> impl Iterator<Item = &T> {
        HeightIdx::<CHAIN_COUNT>::row(height).map(|i| self.at(i))
    }
    pub fn epoch_end_row(&self, epoch: Epoch) -> impl Iterator<Item = &T> {
        HeightIdx::<CHAIN_COUNT>::epoch_end_row(epoch).map(|i| self.at(i))
    }
}

// -------------------------------------------------------------------------- //
// Block Ranges

/// A column oriented encoding for ranges of headers.
///
/// The storage format supports constant time lookups for all properties of
/// individual headers.
///
/// Common properties of headers are deduplicated.
///
/// Individual properties are stored in seperate two-dimensional arrays, where
/// the first index is the chain and the second index is the history dimension.
/// The history dimension is either the height or the epoch.
///
pub struct BlockRange<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> {
    /// block height of the first block in the range (inclusive)
    start_height: BlockHeight,

    /// block height of the last block in the range (exclusive)
    end_height: BlockHeight,

    // constants
    flags: BlockFlags,
    version: ChainwebVersion,

    // Properties that are indexed by chain x epoch
    /// target for the epoch
    target: EpochTable<CHAIN_COUNT, BlockTarget>,
    /// start time of the epoch in microseconds since POSIX epoch
    /// This equals the creation time of the first block in the epoch.
    epoch_start: EpochTable<CHAIN_COUNT, BlockEpochStart>,

    /// start weight of the epoch.
    /// The weight of a block is the start weight in the epoch plus the target multiplied by
    /// the number of blocks since the start of the epoch.
    weight: EpochTable<CHAIN_COUNT, BlockWeight>,

    // Properties that ndexed by chain x height
    /// creation time of the block in microseconds since POSIX epoch
    time: HeightTable<CHAIN_COUNT, BlockTime>,
    /// nonce of the block
    nonce: HeightTable<CHAIN_COUNT, BlockNonce>,
    /// payload hash of the block
    payload: HeightTable<CHAIN_COUNT, PayloadHash>,
    /// hash of the block, which is computed from the properties of the header.
    hash: HeightTable<CHAIN_COUNT, BlockHash>,
}

// -------------------------------------------------------------------------- //
// Indexing

impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    /// The (gensis based) epoch at the start of the range.
    fn start_epoch(&self) -> Epoch {
        self.start_height.epoch()
    }

    fn start_epoch_offset(&self) -> u64 {
        self.start_height.0 % EPOCH_LENGTH
    }
}

// -------------------------------------------------------------------------- //
// Initialize Block Range

impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    // TODO
    pub fn from_compact_range() {}
    pub fn from_layer() {}
}

// -------------------------------------------------------------------------- //
// Get Block Properties

pub struct HeaderView<'a> {
    pub flags: BlockFlags,
    pub time: BlockTime,
    pub parent: &'a BlockHash,
    pub adjacents: Vec<(ChainId, &'a BlockHash)>,
    pub target: &'a BlockTarget,
    pub payload: &'a PayloadHash,
    pub chain: ChainId,
    pub weight: BlockWeight,
    pub height: BlockHeight,
    pub version: ChainwebVersion,
    pub epoch_start: BlockEpochStart,
    pub nonce: BlockNonce,
    pub hash: &'a BlockHash,
}

impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    // TODO: provide accessors for individual properties

    /// The weight of a block. It is computed from the initial weight and target of the epoch.
    pub fn get_weight(&self, height: BlockHeight, chain: ChainId) -> BlockWeight {
        let epoch_start_weight = self.weight.get(height.epoch(), chain);
        let target = self.target.get(height.epoch(), chain);
        let epoch_offset = height.epoch_offset();
        BlockWeight(epoch_start_weight.0 + target.0 * u256::from(epoch_offset))
    }

    pub fn get_header<'a>(&'a self, chain: ChainId, height: BlockHeight) -> HeaderView<'a> {
        let idx = HeightIdx::<CHAIN_COUNT>::new(height, chain);
        HeaderView {
            flags: self.flags,
            time: *self.time.at(idx),
            parent: self.hash.get(height.pred(), chain),
            adjacents: GRAPH[chain.0 as usize]
                .iter()
                .map(|&i| {
                    let parent = height.pred();
                    (ChainId(i), self.hash.get(parent, ChainId(i)))
                })
                .collect(),
            target: self.target.get(height.epoch(), chain),
            payload: self.payload.get(height, chain),
            chain,
            weight: self.get_weight(height, chain),
            height,
            version: CHAINWEB_VERSION,
            epoch_start: *self.epoch_start.get(height.epoch(), chain),
            nonce: *self.nonce.get(height, chain),
            hash: self.hash.get(height, chain),
        }
    }
}

// -------------------------------------------------------------------------- //
// Difficulty Adjustment

use std::iter::once;

// FIXME: this depends on GRAPH.
impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    /// Compute the targets for a new epoch.
    pub fn target_row<'a>(&'a self, epoch: Epoch) -> impl Iterator<Item = BlockTarget> + 'a {
        let degree = uint_as_rational(GRAPH_DEGREE + 1);
        let tmp_targets: Vec<BlockTarget> = self.local_adjust_row(epoch).collect();
        GRAPH.iter().enumerate().map(move |(cid, parent_idxs)| {
            let sum: BigRational = parent_idxs
                .iter()
                .map(|&x| x as usize)
                .chain(once(cid))
                .map(|idx| tmp_targets[idx].as_rational())
                .sum();
            BlockTarget::from_rational(&((sum / &degree).floor()))
        })
    }

    /// Locally adjust the target of a header.
    fn local_adjust(
        parent_target: &BlockTarget,
        parent_epoch_start: BlockEpochStart,
        parent_epoch_end: BlockTime,
    ) -> BlockTarget {
        let actual_duration: BigRational =
            parent_epoch_end.as_rational() - parent_epoch_start.as_rational();
        let target_duration: BigRational = uint_as_rational(EPOCH_LENGTH * BLOCK_DELAY);
        let quotient = (actual_duration / target_duration) * parent_target.as_rational();
        let new_target = quotient.ceil().min(MAX_TARGET.as_rational());
        BlockTarget::from_rational(&new_target)
    }

    /// Intermediate locally adjusted targets for all chains.
    fn local_adjust_row<'a>(&'a self, epoch: Epoch) -> impl Iterator<Item = BlockTarget> + 'a {
        let parent_targets = self.target.row(epoch.pred());
        let parent_epoch_starts = self.epoch_start.row(epoch.pred());
        let parent_epoch_ends = self.time.epoch_end_row(epoch.pred());

        parent_targets
            .zip(parent_epoch_starts)
            .zip(parent_epoch_ends)
            .map(|((t, s), e)| Self::local_adjust(t, *s, *e))
    }
}

// -------------------------------------------------------------------------- //
// Compute Weight

impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    /// Compute the weight of a new block.
    ///
    /// The new weight is the weight of the parent plus the difficulty of the parent multiplied by the
    /// EPOCH_LENGTH.
    ///
    /// ```haskell
    /// weight :: ParentHeader -> HM.HashMap ChainId ParentHeader -> Word256
    /// weight p@(ParentHeader ph) =
    ///    _blockWeight ph + targetToDifficulty (powTarget p adj t)
    /// ```
    ///
    pub fn weight_row<'a>(&'a self, epoch: Epoch) -> impl Iterator<Item = BlockWeight> + 'a {
        let parent_weights = self.weight.row(epoch.pred());
        let targets = self.target_row(epoch.pred());
        parent_weights
            .zip(targets)
            .map(|(w, t)| BlockWeight(w.0 + t.0 * u256::from(EPOCH_LENGTH)))
    }
}

// -------------------------------------------------------------------------- //
// Compute Epoch Start

impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    /// Compute the start time of a new epoch.
    ///
    /// The start time of the new epoch is the end time of the last block in the previous epoch.
    ///
    /// ```haskell
    /// epochStart :: ParentHeader -> Word64
    /// epochStart (ParentHeader ph)
    ///     -- (special cases that apply during chain graph transitions are omitted)
    ///     | isLastInEpoch = _blockCreationTime ph
    ///     | otherwise = _blockEpochStart ph
    ///  where
    ///    isLastInEpoch = (_blockHeight ph + 1) `mod` _WINDOW_WIDTH_ == 0
    /// ```
    ///
    pub fn epoch_start_row<'a>(
        &'a self,
        epoch: Epoch,
    ) -> impl Iterator<Item = BlockEpochStart> + 'a {
        self.time
            .epoch_end_row(epoch.pred())
            .map(|t| BlockEpochStart(*t))
    }
}

// -------------------------------------------------------------------------- //
// Compute Hash

use crate::header::merkletree::header_root;

impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    pub fn hash_row<'a>(&'a self, height: BlockHeight) -> impl Iterator<Item = BlockHash> + 'a {
        let headers = (0..CHAIN_COUNT).map(move |cid| self.get_header(ChainId(cid as u32), height));
        headers.map(|hdr| header_root(&hdr))
    }
}

// -------------------------------------------------------------------------- //
// Check PoW

impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    pub fn check_pow(&self, chain: ChainId, height: BlockHeight) -> bool {
        let hdr = self.get_header(chain, height);
        let mut hasher = Blake2s256::new();
        hasher.update(&hdr.flags.to_header_bytes());
        hasher.update(&hdr.time.to_header_bytes());
        hasher.update(hdr.parent.to_header_bytes());
        hasher.update((GRAPH_DEGREE as u16).to_le_bytes());
        for (chain, hash) in hdr.adjacents.iter() {
            hasher.update(chain.0.to_header_bytes());
            hasher.update(hash.0.to_header_bytes());
        }
        hasher.update(&hdr.target.to_header_bytes());
        hasher.update(hdr.payload.to_header_bytes());
        hasher.update(&hdr.chain.to_header_bytes());
        hasher.update(&hdr.weight.to_header_bytes());
        hasher.update(&hdr.height.to_header_bytes());
        hasher.update(&hdr.version.to_header_bytes());
        hasher.update(&hdr.epoch_start.to_header_bytes());
        hasher.update(&hdr.nonce.to_header_bytes());
        let hash = hasher.finalize();
        u256::from_le_bytes(hash.into()) < hdr.target.0
    }

    pub fn check_pow_row(&self, height: BlockHeight) -> bool {
        for chain in 0..CHAIN_COUNT {
            if !self.check_pow(ChainId(chain), height) {
                return false;
            }
        }
        true
    }
}

// -------------------------------------------------------------------------- //
// Extend Block Range

impl<const CHAIN_COUNT: usize, const GRAPH_DEGREE: usize> BlockRange<CHAIN_COUNT, GRAPH_DEGREE> {
    /// Extend the block range by a new row of blocks
    ///
    pub fn extend_block_range(&mut self, layer: &CompactLayer<CHAIN_COUNT>) {
        self.time.v.extend(layer.time.iter().cloned());
        self.nonce.v.extend(layer.nonce.iter().cloned());
        self.payload.v.extend(layer.payload.iter().cloned());

        self.target.v.extend(self.target_row(self.end_height.epoch()));
        self.epoch_start.v.extend(self.epoch_start_row(self.end_height.epoch()));
        self.weight.v.extend(self.weight_row(self.end_height.epoch()));
        self.check_pow_row(self.end_height);
        self.hash.v.extend(self.hash_row(self.end_height));
        self.end_height = self.end_height.succ();
    }
}
