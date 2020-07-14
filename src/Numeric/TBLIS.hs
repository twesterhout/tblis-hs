module Numeric.TBLIS
  ( addInplace,
    TblisTensor (..),
    TblisType (..),
  )
where

import Control.Monad.ST
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed.Mutable as U
import Foreign.Ptr (nullPtr)
import Numeric.TBLIS.Internal

data IndexTable s = IndexTable !(U.MVector s (Char, Int)) !Int

mkIndexTable :: Int -> ST s (IndexTable s)
mkIndexTable capacity
  | capacity >= 0 = U.unsafeNew capacity >>= \v -> return (IndexTable v 0)
  | otherwise = error $ "invalid capacity: " <> show capacity

elemIndex :: U.Unbox a => (a -> Bool) -> Int -> U.MVector s a -> ST s (Maybe Int)
elemIndex p !n !v' = go v' 0
  where
    go !v !i
      | i < n =
        (p <$> U.read v i) >>= \haveFound ->
          if haveFound
            then return (Just i)
            else go v (i + 1)
      | otherwise = return Nothing

addIndex :: IndexTable s -> (Char, Int) -> ST s (IndexTable s)
addIndex t@(IndexTable v size) !(c, dim)
  | size <= U.length v =
    elemIndex ((== c) . fst) size v >>= \case
      Just i ->
        snd <$> U.read v i >>= \dim' ->
          if dim == dim'
            then return t
            else error $ "different dimensions encountered for index " <> show c <> ": " <> show dim' <> " != " <> show dim
      Nothing -> U.write v size (c, dim) >> return (IndexTable v (size + 1))
  | otherwise = error $ "IndexTable is full"

addInplace :: forall a. TblisType a => TblisTensor a -> ByteString -> TblisTensor a -> ByteString -> IO ()
addInplace a idxA b idxB = check $
  withTblisTensor a $ \a' -> withTblisTensor b $ \b' ->
    B.useAsCString idxA $ \idxA' -> B.useAsCString idxB $ \idxB' ->
      tblis_tensor_add nullPtr nullPtr a' idxA' b' idxB'
  where
    combine :: ByteString -> TblisTensor a -> [(Char, Int)]
    combine s t = zip (B.unpack s) (size (t :: TblisTensor a))
    check = runST $ do
      t <- mkIndexTable $ B.length idxA + B.length idxB
      !_ <- foldlM addIndex t $ combine idxA a <> combine idxB b
      return id
