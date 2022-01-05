module Numeric.TBLIS.Types
  ( TblisLenType,
    TblisStrideType,
    TblisLabelType,
    TblisComm,
    TblisConfig,
    TblisType (..),
    SomeTblisType (..),
    IsTblisType (..),
    TblisScalar (..),
    TblisTensor (..),
    -- mkTblisTensor,
  )
where

-- import Control.Monad.Primitive
-- import Control.Monad.ST
import Data.Complex
-- import Data.Primitive.PrimArray
-- import Data.Primitive.Types (Prim)
import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (pred)

type TblisLenType = CPtrdiff

type TblisStrideType = CPtrdiff

type TblisLabelType = CChar

data TblisComm

data TblisConfig

data TblisType :: (Type -> Type) where
  TblisFloat :: TblisType Float
  TblisDouble :: TblisType Double
  TblisComplexFloat :: TblisType (Complex Float)
  TblisComplexDouble :: TblisType (Complex Double)

deriving stock instance Show (TblisType a)

deriving stock instance Eq (TblisType a)

data SomeTblisType where
  SomeTblisType :: IsTblisType a => TblisType a -> SomeTblisType

-- typedef enum
-- {
--     TYPE_SINGLE   = 0,
--     TYPE_FLOAT    = TYPE_SINGLE,
--     TYPE_DOUBLE   = 1,
--     TYPE_SCOMPLEX = 2,
--     TYPE_DCOMPLEX = 3
-- } type_t;
instance Enum (TblisType a) where
  fromEnum x = case x of
    TblisFloat -> 0
    TblisDouble -> 1
    TblisComplexFloat -> 2
    TblisComplexDouble -> 3
  toEnum = error "toEnum not implemented for TblisType"

class (Num a, Storable a) => IsTblisType a where
  tblisTypeOf :: proxy a -> TblisType a

instance IsTblisType Float where tblisTypeOf _ = TblisFloat

instance IsTblisType Double where tblisTypeOf _ = TblisDouble

instance IsTblisType (Complex Float) where tblisTypeOf _ = TblisComplexFloat

instance IsTblisType (Complex Double) where tblisTypeOf _ = TblisComplexDouble

newtype TblisScalar a = TblisScalar a
  deriving stock (Read, Show, Generic)
  deriving newtype (Eq, Ord, Num)

instance IsTblisType a => Storable (TblisScalar a) where
  sizeOf _ = 16 + 8
  {-# INLINE sizeOf #-}
  alignment _ = 8
  {-# INLINE alignment #-}
  peek p = case tblisTypeOf (Proxy :: Proxy a) of
    TblisFloat -> TblisScalar <$> peekByteOff p 0
    TblisDouble -> TblisScalar <$> peekByteOff p 0
    TblisComplexFloat -> TblisScalar <$> peekByteOff p 0
    TblisComplexDouble -> TblisScalar <$> peekByteOff p 0
  {-# INLINE peek #-}
  poke p (TblisScalar x) = do
    pokeByteOff p 0 x
    pokeByteOff p 16
      . (fromIntegral :: Int -> CInt)
      . fromEnum
      . tblisTypeOf
      $ (Proxy :: Proxy a)
  {-# INLINE poke #-}

data TblisTensor a = TblisTensor
  { tblisTensorConj :: !Bool,
    tblisTensorScalar :: !(TblisScalar a),
    tblisTensorData :: !(Ptr a),
    tblisTensorNDim :: !Int,
    tblisTensorLen :: !(Ptr TblisLenType),
    tblisTensorStride :: !(Ptr TblisStrideType)
  }

-- type_t type;
-- int conj;
-- tblis_scalar scalar;
-- void* data;
-- unsigned ndim;
-- len_type* len;
-- stride_type* stride;
instance IsTblisType a => Storable (TblisTensor a) where
  sizeOf _ = 4 + 4 + 24 + 8 + 8 + 8 + 8
  alignment _ = 8
  poke p x = do
    pokeByteOff p 0 . (fromIntegral :: Int -> CInt) . fromEnum $ tblisTypeOf (Proxy @a)
    pokeByteOff p 4 . (fromBool :: Bool -> CInt) $ tblisTensorConj x
    pokeByteOff p 8 $ tblisTensorScalar x
    pokeByteOff p 32 $ tblisTensorData x
    pokeByteOff p 40 . (fromIntegral :: Int -> CUInt) $ tblisTensorNDim x
    pokeByteOff p 48 $ tblisTensorLen x
    pokeByteOff p 56 $ tblisTensorStride x
  peek _ = error $ "peek is not implemented for TblisTensor"

-- peek p =
--   TblisTensor <$> peekByteOff p 4

-- mkShape :: (Prim a, Integral a) => (Int -> Bool) -> Int -> [Int] -> PrimArray a
-- mkShape pred n xs = runST $ do
--   arr <- newAlignedPinnedPrimArray n
--   let go [] !i
--         | i == n = return ()
--         | otherwise = error $ "list length is smaller than the specified size"
--       go (y : ys) !i
--         | i < n && pred y = writePrimArray arr i (fromIntegral y) >> go ys (i + 1)
--         | i >= n = error $ "list length is greated than the specified size"
--         | otherwise = error $ "encountered an invalid element: " <> show y
--   go xs 0
--   unsafeFreezePrimArray arr
--
-- mkLenAndStride :: [Int] -> [Int] -> (PrimArray TblisLenType, PrimArray TblisStrideType)
-- mkLenAndStride shape strides = (mkShape (>= 0) n shape, mkShape (> 0) n strides)
--   where
--     !n = length shape
--
-- mkTblisTensor :: IsTblisType a => Ptr a -> [Int] -> [Int] -> TblisTensor a
-- mkTblisTensor p shape strides =
--   let (len, stride) = mkLenAndStride shape strides
--    in TblisTensor False (TblisScalar 1) p len stride
