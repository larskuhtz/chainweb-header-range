use crate::header::range::HeaderView;
use crate::primitives::BlockHash;
use crate::primitives::ToHeaderBytes;
use crate::primitives::ChainwebHash;
use merkle_log::tree::{MerkleHash, MerkleLogEntry, MerkleTree};
use sha2::digest::Output;
use sha2::Sha512_256;

// Tag values can be found here: https://github.com/kadena-io/chainweb-node/wiki/Chainweb-Merkle-Tree#chainweb-merkle-hash-function
const CHAIN_ID_TAG: u16 = 0x0002;
const BLOCK_HEIGHT_TAG: u16 = 0x0003;
const BLOCK_WEIGHT_TAG: u16 = 0x0004;
const FEATURE_FLAGS_TAG: u16 = 0x0006;
const BLOCK_CREATION_TIME_TAG: u16 = 0x0007;
const CHAINWEB_VERSION_TAG: u16 = 0x0008;
const HASH_TARGET_TAG: u16 = 0x0011;
const EPOCH_START_TIME_TAG: u16 = 0x0019;
const BLOCK_NONCE_TAG: u16 = 0x0020;

// Hash functions for Merkle tree nodes
// cf. https://github.com/kadena-io/chainweb-node/wiki/Chainweb-Merkle-Tree#merke-log-trees
pub type Hash = Sha512_256;

pub fn tag_bytes(tag: u16) -> [u8; 2] {
    tag.to_be_bytes()
}

fn mk_root_entry(hash: &[u8]) -> MerkleLogEntry<Hash> {
    let r: Output<Hash> = Output::<Hash>::clone_from_slice(hash);
    MerkleLogEntry::TreeLeaf(MerkleHash(r))
}

fn mk_data_entry(tag: u16, data: &[u8]) -> MerkleLogEntry<Hash> {
    MerkleLogEntry::DataLeaf([&tag_bytes(tag), data].concat())
}

pub fn header_root(hdr: &HeaderView) -> BlockHash {

    let adjacents = &hdr.adjacents;

    let entries = [
        mk_data_entry(FEATURE_FLAGS_TAG, hdr.flags.to_header_bytes().as_slice()),
        mk_data_entry(BLOCK_CREATION_TIME_TAG, hdr.time.to_header_bytes().as_slice()),
        mk_root_entry(hdr.parent.to_header_bytes().as_slice()),
        mk_data_entry(HASH_TARGET_TAG, hdr.target.to_header_bytes().as_slice()),
        mk_root_entry(hdr.payload.to_header_bytes().as_slice()),
        mk_data_entry(CHAIN_ID_TAG, hdr.chain.to_header_bytes().as_slice()),
        mk_data_entry(BLOCK_WEIGHT_TAG, hdr.weight.to_header_bytes().as_slice()),
        mk_data_entry(BLOCK_HEIGHT_TAG, hdr.height.to_header_bytes().as_slice()),
        mk_data_entry(CHAINWEB_VERSION_TAG, hdr.version.to_header_bytes().as_slice()),
        mk_data_entry(EPOCH_START_TIME_TAG, hdr.epoch_start.to_header_bytes().as_slice()),
        mk_data_entry(BLOCK_NONCE_TAG, hdr.nonce.to_header_bytes().as_slice()),
        mk_root_entry(adjacents[0].1.to_header_bytes().as_slice()),
        mk_root_entry(adjacents[1].1.to_header_bytes().as_slice()),
        mk_root_entry(adjacents[2].1.to_header_bytes().as_slice()),
    ];
    let tree = MerkleTree::<Hash>::new(&entries);

    BlockHash(ChainwebHash(tree.root().0.into()))
}
