/// The degree of the chain graph. This the number of adjacent parents for
/// each block header.
pub const TWENTY_CHAIN_GRAPH_DEGREE: u32 = 3;
pub const TWENTY_CHAIN_GRAPH_ORDER: u32 = 20;

pub type TwentyChainGraphType =
    [[u32; TWENTY_CHAIN_GRAPH_DEGREE as usize]; TWENTY_CHAIN_GRAPH_ORDER as usize];

/// The chain graph for the 20-chain network, sorted by chain ID, then
/// from the lowest chain ID to the highest parent chain ID.
pub const TWENTY_CHAIN_GRAPH: TwentyChainGraphType = [
    [5, 10, 15],
    [6, 11, 16],
    [7, 12, 17],
    [8, 13, 18],
    [9, 14, 19],
    [0, 7, 8],
    [1, 8, 9],
    [2, 5, 9],
    [3, 5, 6],
    [4, 6, 7],
    [0, 11, 19],
    [1, 10, 12],
    [2, 11, 13],
    [3, 12, 14],
    [4, 13, 15],
    [0, 14, 16],
    [1, 15, 17],
    [2, 16, 18],
    [3, 17, 19],
    [4, 10, 18],
];

// -------------------------------------------------------------------------- //
// Current Graph

// Graph Constants
pub const CHAIN_COUNT: u32 = TWENTY_CHAIN_GRAPH_ORDER;
pub const GRAPH_DEGREE: u32 = TWENTY_CHAIN_GRAPH_DEGREE;
pub const GRAPH: TwentyChainGraphType = TWENTY_CHAIN_GRAPH;
