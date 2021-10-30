let xor (a:i64) (b:i64):i64 = a ^ b

let defaultSalt:i64 = -2578643520546668380  -- 0xdc36d1615b7400a4

let combine (h1:i64) (h2:i64):i64 = (h1 * 16777619) `xor` h2

-- | Hashables parameterized of a base type.
module type hashable = {
  type ty
  val hashWithSalt : i64 -> ty -> i64
  val hash : ty -> i64
}

-- | Given a type that can convert to i64 construct a hashable module.
module mk_hashable(T: ty): (hashable with ty = T.t) = {
  type ty = T.t
  let  hashWithSalt (salt:i64) (x:t) = salt `combine` hash x
  let  hash (x:t) = hashWithSalt defaultSalt (T.to_i64 x)
}

let hash_1d 'h       [d] (xs:      [d]h.ty):i64 =
    reduce h.hashWithSalt defaultSalt xs

let hash_2d 'h    [w][d] (xs:   [w][d]h.ty):i64 =
    reduce h.hashWithSalt defaultSalt (map hash_1d xs)

let hash_3d 'h [h][w][d] (xs:[h][w][d]h.ty):i64 =
    reduce h.hashWithSalt defaultSalt (map hash_2d xs)
