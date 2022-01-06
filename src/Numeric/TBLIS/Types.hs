module Numeric.TBLIS.Types
  ( TblisError (..),
    TblisLenType,
    TblisStrideType,
    TblisLabelType,
    TblisComm,
    TblisConfig,
    TblisType (..),
    SomeTblisType (..),
    IsTblisType (..),
    TblisScalar (..),
    TblisTensor (..),
    tblisToTypedTensor,
    TblisTypedTensor,
    -- mkTblisTensor,
  )
where

-- import Control.Monad.Primitive
-- import Control.Monad.ST

-- import Data.Primitive.PrimArray
-- import Data.Primitive.Types (Prim)

import Control.Monad (when)
import DLPack (IsDLDataType)
import Data.Complex (Complex)
import qualified Data.Complex
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import Foreign.C.Types
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr
import Foreign.Storable
import qualified GHC.Exts as GHC
import GHC.Stack
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (pred)

natToInt :: forall n. KnownNat n => Int
natToInt = fromIntegral $ GHC.TypeLits.natVal (Proxy @n)
{-# INLINE natToInt #-}

type TblisLenType = CPtrdiff

type TblisStrideType = CPtrdiff

type TblisLabelType = CChar

data TblisComm

data TblisConfig

newtype TblisError = TblisError Text
  deriving stock (Show, Eq)

-- instance Error TblisError where
--   strMsg = TblisError . toText
--   {-# INLINE strMsg #-}

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

class (Num a, Storable a, IsDLDataType a) => IsTblisType a where
  tblisTypeOf :: proxy a -> TblisType a

instance IsTblisType Float where tblisTypeOf _ = TblisFloat

instance IsTblisType Double where tblisTypeOf _ = TblisDouble

instance IsTblisType (Complex Float) where tblisTypeOf _ = TblisComplexFloat

instance IsTblisType (Complex Double) where tblisTypeOf _ = TblisComplexDouble

newtype TblisScalar a = TblisScalar a
  deriving stock (Read, Show)
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
    tblisTensorScalar :: {-# UNPACK #-} !(TblisScalar a),
    tblisTensorData :: {-# UNPACK #-} !(Ptr a),
    tblisTensorNDim :: {-# UNPACK #-} !Int,
    tblisTensorLen :: {-# UNPACK #-} !(Ptr TblisLenType),
    tblisTensorStride :: {-# UNPACK #-} !(Ptr TblisStrideType)
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

newtype TblisTypedTensor (rank :: Nat) (a :: Type) = TblisTypedTensor (TblisTensor a)

tblisToTypedTensor :: forall r a. (HasCallStack, KnownNat r) => TblisTensor a -> TblisTypedTensor r a
tblisToTypedTensor t
  | tblisTensorNDim t == natToInt @r = TblisTypedTensor t
  | otherwise =
    error $
      "tensor has wrong rank: " <> show (tblisTensorNDim t)
        <> "; expected "
        <> show (natToInt @r)

conjugateScalar :: forall a. IsTblisType a => TblisScalar a -> TblisScalar a
conjugateScalar (TblisScalar z) = TblisScalar $
  case tblisTypeOf (Proxy @a) of
    TblisFloat -> z
    TblisDouble -> z
    TblisComplexFloat -> Data.Complex.conjugate z
    TblisComplexDouble -> Data.Complex.conjugate z
{-# INLINE conjugateScalar #-}

tensorShape :: TblisTensor a -> [Int]
tensorShape t = unsafePerformIO $ do
  let helper i = let !i' = fromIntegral i in i'
  fmap helper <$> peekArray (tblisTensorNDim t) (tblisTensorLen t)

tensorIndex :: IsTblisType a => TblisTensor a -> [Int] -> IO a
tensorIndex t index = do
  stride <- fmap fromIntegral <$> peekArray (tblisTensorNDim t) (tblisTensorStride t)
  let shape = tensorShape t
  -- <- fmap fromIntegral <$> peekArray (tblisTensorNDim t) (tblisTensorLen t)
  when (any id $ zipWith (\i n -> i < 0 || i >= n) index shape) $
    error $ "invalid index: " <> show index
  let !linearIndex = sum $ zipWith (*) stride index
  !s <- (tblisTensorScalar t *) . TblisScalar <$> peekElemOff (tblisTensorData t) linearIndex
  let (TblisScalar !z) = if (tblisTensorConj t) then conjugateScalar s else s
  pure z
{-# NOINLINE tensorIndex #-}

flattenTensorToList :: IsTblisType a => TblisTensor a -> [a]
flattenTensorToList t = unsafePerformIO $ do
  -- <- fmap fromIntegral <$> peekArray (tblisTensorNDim t) (tblisTensorLen t)
  let !shape = tensorShape t
      indices = sequence . fmap (\n -> [0 .. n - 1]) $ shape
      f i = do !x <- tensorIndex t i; pure x
  mapM f indices

-- tensorToList1D :: forall a. Prim a => TblisTypedTensor 1 a -> [a]
-- tensorToList1D t = runST $
--   withForeignPtr' (tensorData t) $ \p ->
--     go p [] (stride * (extent - 1))
--   where
--     !stride = indexPrimArray (tensorStrides t) 0
--     !extent = indexPrimArray (tensorShape t) 0
--     go :: PrimMonad m => Ptr a -> [a] -> Int -> m [a]
--     go !p acc !i
--       | i >= 0 = do
--         !x <- P.readOffPtr p i
--         go p (x : acc) (i - stride)
--       | otherwise = touch t >> pure acc
--
-- tensorFromList1D :: forall a. (HasCallStack, Prim a) => Int -> [a] -> Tensor 'CPU 1 a
-- tensorFromList1D n xs = unsafePerformIO $ do
--   t <- newTensor [n]
--   withForeignPtr (tensorData t) $ \p -> do
--     let go !i []
--           | i == n = pure ()
--           | otherwise = error $ "list is shorter than expected"
--         go !i (y : ys)
--           | i < n = P.writeOffPtr p i y >> go (i + 1) ys
--           | otherwise = error "list is longer than expected"
--     go 0 xs
--   pure t
--
-- instance IsTblisType a => GHC.IsList (TblisTypedTensor 1 a) where
--   type Item (TblisTypedTensor 1 a) = a
--   toList = tensorToList1D
--   fromList xs = tensorFromList1D (length xs) xs
--   fromListN n xs = tensorFromList1D n xs

splitAt' :: HasCallStack => Int -> [a] -> ([a], [a])
splitAt' = go
  where
    go 0 [] = ([], [])
    go 1 (x : xs) = ([x], xs)
    go m (x : xs) = (x : xs', xs'')
      where
        (xs', xs'') = go (m - 1) xs
    go _ [] = error "wrong list length"

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go
  where
    go [] = []
    go xs@(_ : _) = let (ys, zs) = splitAt' n xs in ys : go zs

-- listShape2D :: [[a]] -> [Int]
-- listShape2D [] = [0, 0]
-- listShape2D xs@(x : _) = [length xs, length x]
-- {-# INLINE listShape2D #-}

-- listShape3D :: [[[a]]] -> [Int]
-- listShape3D [] = [0, 0, 0]
-- listShape3D xs@(x : _) = length xs : listShape2D x
--
-- listShape4D :: [[[a]]] -> [Int]
-- listShape4D [] = [0, 0, 0, 0]
-- listShape4D xs@(x : _) = length xs : listShape3D x
--
-- listShape5D :: [[[[a]]]] -> [Int]
-- listShape5D [] = [0, 0, 0, 0, 0]
-- listShape5D xs@(x : _) = length xs : listShape4D x

instance IsTblisType a => GHC.IsList (TblisTypedTensor 1 a) where
  type Item (TblisTypedTensor 1 a) = a
  toList (TblisTypedTensor t) = flattenTensorToList t
  fromList _ = error "IsList instance of TblisTypedTensor does not implement fromList"

instance IsTblisType a => GHC.IsList (TblisTypedTensor 2 a) where
  type Item (TblisTypedTensor 2 a) = [a]
  toList (TblisTypedTensor t) = case tensorShape t of
    [_, d₁] -> chunksOf d₁ (flattenTensorToList t)
    _ -> error "impossible: tensor must be rank 2"
  fromList _ = error "IsList instance of TblisTypedTensor does not implement fromList"

instance IsTblisType a => GHC.IsList (TblisTypedTensor 3 a) where
  type Item (TblisTypedTensor 3 a) = [[a]]
  toList (TblisTypedTensor t) = case tensorShape t of
    [_, d₁, d₂] -> chunksOf d₁ . chunksOf d₂ $ flattenTensorToList t
    _ -> error "impossible: tensor must be rank 3"
  fromList _ = error "IsList instance of TblisTypedTensor does not implement fromList"

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
