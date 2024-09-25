
use crate::primitives::*;

// -------------------------------------------------------------------------- //
// Compact Block Range

pub struct CompactHeader {
    time: BlockTime,
    nonce: BlockNonce,
    payload: PayloadHash,
}

pub struct CompactLayer<const CHAIN_COUNT: usize> {
    pub time: [BlockTime; CHAIN_COUNT],
    pub nonce: [BlockNonce; CHAIN_COUNT],
    pub payload: [PayloadHash; CHAIN_COUNT],
}

// pub struct CompactBlockRange<const CHAIN_COUNT: u32, const GRAPH_DEGREE: u32> {
//     start_height: BlockHeight,
//     end_height: BlockHeight,
// 
//     // constant
//     flags: BlockFlags,
//     version: ChainwebVersion,
// 
//     // Values for headers at start epoch indexed by chain
//     target: [BlockTarget; CHAIN_COUNT as usize],
//     epoche_start: [BlockEpochStart; CHAIN_COUNT as usize],
//     weight: [BlockWeight; CHAIN_COUNT as usize],
// 
//     // Values for headers at start height indexed by chain
//     hash: [BlockHash; CHAIN_COUNT as usize],
//     parents: [BlockHash; CHAIN_COUNT as usize],
// 
//     // Values for end height indexed by chain
//     root: [BlockHash; CHAIN_COUNT as usize],
// 
//     // Indexed by height
//     // CHAIN_COUNT x range_length() - 1
//     time: Vec<BlockTime>,
//     nonce: Vec<BlockNonce>,
//     payload: Vec<PayloadHash>,
// }
