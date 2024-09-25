use ethnum::u256;
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::BigRational;

// -------------------------------------------------------------------------- //
// Primitives Types

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ChainId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChainwebHash(pub [u8; 32]);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PayloadHash(pub ChainwebHash);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockHash(pub ChainwebHash);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockTime(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockNonce(pub [u8; 8]);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockWeight(pub u256);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockHeight(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockEpochStart(pub BlockTime);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockTarget(pub u256);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockFlags(pub [u8; 8]);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ChainwebVersion(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Nonce(pub u64);

// -------------------------------------------------------------------------- //
// Constants

// Chainweb Version Constants
pub const EPOCH_LENGTH: u64 = 120;
pub const CHAINWEB_VERSION: ChainwebVersion = ChainwebVersion(0x05);

pub const BLOCK_DELAY: u64 = 30;
pub const WINDOW_WIDTH: u64 = 120;

// -------------------------------------------------------------------------- //
// Target

pub fn u256_as_rational(x: u256) -> BigRational {
    BigRational::from(BigInt::from_bytes_le(
        num_bigint::Sign::Plus,
        &x.to_le_bytes(),
    ))
}

pub fn u256_from_rational(x: &BigRational) -> u256 {
    let (_, mut bytes) = x.numer().to_bytes_le();
    bytes.resize(32, 0);
    let x: [u8; 32] = bytes.try_into().unwrap();
    u256::from_le_bytes(x)
}

pub const MAX_TARGET: BlockTarget = BlockTarget(u256::MAX);

impl BlockTarget {
    pub fn as_rational(&self) -> BigRational {
        u256_as_rational(self.0)
    }

    pub fn from_rational(x: &BigRational) -> Self {
        BlockTarget(u256_from_rational(x))
    }
}

// -------------------------------------------------------------------------- //
// BlockTime

impl BlockTime {
    pub fn as_rational(self) -> BigRational {
        BigRational::from(BigInt::from(self.0))
    }
}

impl BlockEpochStart {
    pub fn as_rational(self) -> BigRational {
        self.0.as_rational()
    }
}

pub fn uint_as_rational<T: Integer>(x: T) -> BigRational
where
    BigInt: From<T>,
{
    BigRational::from(BigInt::from(x))
}

// -------------------------------------------------------------------------- //
// Difficulty

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Difficulty(pub u256);

/// Compute Difficulty from Target
///
/// ```haskell
/// targetToDifficulty :: Word256 -> Word256
/// targetToDifficulty target = (2^256 - 1) `div` target
/// ```
///
pub fn difficulty(target: &BlockTarget) -> Difficulty {
    Difficulty(MAX_TARGET.0 / target.0)
}

impl Difficulty {
    pub fn as_rational(&self) -> BigRational {
        u256_as_rational(self.0)
    }

    pub fn from_rational(x: &BigRational) -> Self {
        Difficulty(u256_from_rational(x))
    }
}

// -------------------------------------------------------------------------- //
// Serialization of Block Headers

pub trait ToHeaderBytes {
    fn to_header_bytes(&self) -> Vec<u8>;
}

impl ToHeaderBytes for ChainwebHash {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_vec()
    }
}

impl ToHeaderBytes for BlockHash {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}

impl ToHeaderBytes for u256 {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl ToHeaderBytes for u64 {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl ToHeaderBytes for u32 {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl ToHeaderBytes for ChainId {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}

impl ToHeaderBytes for BlockTime {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}

impl ToHeaderBytes for BlockNonce {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_vec()
    }
}

impl ToHeaderBytes for BlockWeight {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}

impl ToHeaderBytes for BlockHeight {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}

impl ToHeaderBytes for BlockEpochStart {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}

impl ToHeaderBytes for BlockTarget {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}

impl ToHeaderBytes for BlockFlags {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_vec()
    }
}

impl ToHeaderBytes for ChainwebVersion {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}

impl ToHeaderBytes for PayloadHash {
    fn to_header_bytes(&self) -> Vec<u8> {
        self.0.to_header_bytes()
    }
}
