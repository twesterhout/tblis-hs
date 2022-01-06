{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Numeric.TBLIS
  ( tblisAdd,
    tblisAdd',
    tblisTensorAdd,
    tblisMult,
    tblisMult',
    tblisTensorMult,
    tblisScale,
    tblisConjugate,
    tblisFromDLTensor,
    tblisGetNumberThreads,
    tblisSetNumberThreads,
    tblisSingle,
    tblisParallel,
    tblisDefaultConfig,
    TblisTypedTensor,
    tblisToTypedTensor,
    TblisTensor (..),
    TblisScalar (..),
    TblisError (..),
    IsTblisType,
    TblisComm,
    TblisConfig,
  )
where

import Control.Monad (when)
import Control.Monad.Primitive
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import DLPack
import Data.Bits (toIntegralSized)
import Data.Int (Int64)
import Data.Proxy
import Data.Text (pack, unpack)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable (Storable, sizeOf)
import GHC.Stack
import Numeric.TBLIS.Types

toDLDataType :: forall a. IsDLDataType a => TblisType a -> DLDataType
toDLDataType _ = dlDataTypeOf (Proxy :: Proxy a)
{-# INLINE toDLDataType #-}

{-
fromDLDataType :: DLDataType -> Maybe SomeTblisType
fromDLDataType dtype
  | dtype == toDLDataType TblisFloat = Just (SomeTblisType TblisFloat)
  | dtype == toDLDataType TblisDouble = Just (SomeTblisType TblisDouble)
  | dtype == toDLDataType TblisComplexFloat = Just (SomeTblisType TblisComplexFloat)
  | dtype == toDLDataType TblisComplexDouble = Just (SomeTblisType TblisComplexDouble)
  | otherwise = Nothing
{-# INLINE fromDLDataType #-}
-}

requireType :: IsDLDataType a => TblisType a -> DLTensor -> Either TblisError ()
requireType !dtype !t
  | toDLDataType dtype == dlTensorDType t = Right ()
  | otherwise =
    Left . TblisError . pack $
      "DLTensor has wrong type: " <> show (dlTensorDType t) <> "; expected "
        <> show (toDLDataType dtype)
{-# INLINE requireType #-}

checkCoercibleToInt64 :: forall i proxy. (Integral i, Storable i) => proxy i -> Either TblisError ()
checkCoercibleToInt64 _
  | sizeOf (undefined :: i) == sizeOf (undefined :: Int64) = Right ()
  | otherwise = Left . TblisError $ "TblisLenType or TblisStrideType are not 64-bit"
{-# INLINE checkCoercibleToInt64 #-}

requireDevice :: DLTensor -> Either TblisError ()
requireDevice t = case dlDeviceType (dlTensorDevice t) of
  DLCPU -> Right ()
  DLCUDAHost -> Right ()
  device ->
    Left . TblisError . pack $
      "DLTensor is located on wrong device: " <> show device
        <> "; TBLIS only supports CPU tensors"
{-# INLINE requireDevice #-}

-- | Conjugate a tensor
tblisConjugate :: TblisTensor a -> TblisTensor a
tblisConjugate t = t {tblisTensorConj = not (tblisTensorConj t)}
{-# INLINE tblisConjugate #-}

-- | Scale a tensor
tblisScale :: IsTblisType a => a -> TblisTensor a -> TblisTensor a
tblisScale a t = t {tblisTensorScalar = TblisScalar a * tblisTensorScalar t}
{-# INLINE tblisScale #-}

-- | Convert @DLTensor@ to @TblisTensor@
tblisFromDLTensor ::
  forall a.
  IsTblisType a =>
  DLTensor ->
  Either TblisError (TblisTensor a)
tblisFromDLTensor t = do
  checkCoercibleToInt64 (Proxy @TblisLenType)
  checkCoercibleToInt64 (Proxy @TblisStrideType)
  requireType (tblisTypeOf (Proxy @a)) t
  requireDevice t
  pure $
    TblisTensor
      { tblisTensorConj = False,
        tblisTensorScalar = TblisScalar 1,
        tblisTensorData = dlTensorData t `plusPtr` fromIntegral (dlTensorByteOffset t),
        tblisTensorNDim = dlTensorNDim t,
        tblisTensorLen = castPtr (dlTensorShape t),
        tblisTensorStride = castPtr (dlTensorStrides t)
      }
{-# INLINE tblisFromDLTensor #-}

foreign import ccall unsafe "tblis_get_num_threads"
  tblis_get_num_threads :: IO CUInt

foreign import ccall unsafe "tblis_set_num_threads"
  tblis_set_num_threads :: CUInt -> IO ()

-- | Get number of threads which TBLIS uses for parallel execution
tblisGetNumberThreads :: IO Int
tblisGetNumberThreads = fromIntegral <$> tblis_get_num_threads

-- | Set number of threads which TBLIS will use for parallel execution
tblisSetNumberThreads :: HasCallStack => Int -> IO ()
tblisSetNumberThreads n = case toIntegralSized n of
  Just n' -> tblis_set_num_threads n'
  Nothing -> error $ "specified an invalid number of threads: " <> show n

foreign import ccall "&tblis_single"
  tblis_single :: Ptr TblisComm

-- | TBLIS communicator which ensures parallel execution. It is used by default
tblisParallel :: Ptr TblisComm
tblisParallel = nullPtr
{-# INLINE tblisParallel #-}

-- | TBLIS communicator which ensures single-threaded execution.
tblisSingle :: Ptr TblisComm
tblisSingle = tblis_single
{-# INLINE tblisSingle #-}

-- | Default TBLIS config.
tblisDefaultConfig :: Ptr TblisConfig
tblisDefaultConfig = nullPtr
{-# INLINE tblisDefaultConfig #-}

-- void tblis_tensor_add(const tblis_comm* comm, const tblis_config* cfg,
--                       const tblis_tensor* A, const label_type* idx_A,
--                             tblis_tensor* B, const label_type* idx_B);
foreign import ccall "tblis_tensor_add"
  tblis_tensor_add :: Ptr TblisComm -> Ptr TblisConfig -> Ptr (TblisTensor a) -> Ptr TblisLabelType -> Ptr (TblisTensor a) -> Ptr TblisLabelType -> IO ()

tblisAdd ::
  (IsTblisType a, PrimMonad m) =>
  a ->
  DLTensor ->
  String ->
  a ->
  DLTensor ->
  String ->
  ExceptT TblisError m ()
tblisAdd = tblisAdd' tblisParallel tblisDefaultConfig
{-# INLINE tblisAdd #-}

tblisAdd' ::
  (IsTblisType a, PrimMonad m) =>
  Ptr TblisComm ->
  Ptr TblisConfig ->
  a ->
  DLTensor ->
  String ->
  a ->
  DLTensor ->
  String ->
  ExceptT TblisError m ()
tblisAdd' c_comm c_config α a indexA β b indexB = do
  !tensorA <- except $ tblisScale α <$> tblisFromDLTensor a
  !tensorB <- except $ tblisScale β <$> tblisFromDLTensor b
  tblisTensorAdd c_comm c_config tensorA indexA tensorB indexB
{-# INLINE tblisAdd' #-}

checkRank :: Char -> Int -> String -> Either TblisError ()
checkRank t rank index
  | length index == rank = Right ()
  | otherwise =
    Left . TblisError . pack $
      "tensor " <> show t <> " has rank " <> show rank <> ", but "
        <> show (length index)
        <> " indices were provided: "
        <> show index

checkArgsAdd :: Int -> String -> Int -> String -> Either TblisError ()
checkArgsAdd rankA indexA rankB indexB = do
  checkRank 'A' rankA indexA
  checkRank 'B' rankB indexB
  let !okay = all (\i -> elem i indexA) indexB
  when (not okay) $
    Left . TblisError . pack $
      "invalid indices: " <> show indexA <> " and " <> show indexB

eitherToError :: (HasCallStack, PrimMonad m) => Either TblisError a -> m a
eitherToError x = withFrozenCallStack $ case x of
  Right a -> pure a
  Left (TblisError e) -> error (unpack e)

tblisTensorAdd ::
  (IsTblisType a, PrimMonad m) =>
  Ptr TblisComm ->
  Ptr TblisConfig ->
  TblisTensor a ->
  String ->
  TblisTensor a ->
  String ->
  ExceptT TblisError m ()
tblisTensorAdd c_comm c_config a indexA b indexB = do
  except $ checkArgsAdd (tblisTensorNDim a) indexA (tblisTensorNDim b) indexB
  lift $
    unsafeIOToPrim $
      with a $ \c_a ->
        withCString indexA $ \c_indexA ->
          with b $ \c_b ->
            withCString indexB $ \c_indexB ->
              tblis_tensor_add c_comm c_config c_a c_indexA c_b c_indexB
{-# INLINE tblisTensorAdd #-}

foreign import ccall "tblis_tensor_mult"
  tblis_tensor_mult :: Ptr TblisComm -> Ptr TblisConfig -> Ptr (TblisTensor a) -> Ptr TblisLabelType -> Ptr (TblisTensor a) -> Ptr TblisLabelType -> Ptr (TblisTensor a) -> Ptr TblisLabelType -> IO ()

tblisMult ::
  (IsTblisType a, PrimMonad m) =>
  a ->
  DLTensor ->
  String ->
  a ->
  DLTensor ->
  String ->
  a ->
  DLTensor ->
  String ->
  ExceptT TblisError m ()
tblisMult = tblisMult' tblisParallel tblisDefaultConfig
{-# INLINE tblisMult #-}

tblisMult' ::
  (IsTblisType a, PrimMonad m) =>
  Ptr TblisComm ->
  Ptr TblisConfig ->
  a ->
  DLTensor ->
  String ->
  a ->
  DLTensor ->
  String ->
  a ->
  DLTensor ->
  String ->
  ExceptT TblisError m ()
tblisMult' c_comm c_config α a indexA β b indexB γ c indexC = do
  !tensorA <- except $ tblisScale α <$> tblisFromDLTensor a
  !tensorB <- except $ tblisScale β <$> tblisFromDLTensor b
  !tensorC <- except $ tblisScale γ <$> tblisFromDLTensor c
  tblisTensorMult c_comm c_config tensorA indexA tensorB indexB tensorC indexC
{-# INLINE tblisMult' #-}

checkArgsMult :: Int -> String -> Int -> String -> Int -> String -> Either TblisError ()
checkArgsMult rankA indexA rankB indexB rankC indexC = do
  checkRank 'A' rankA indexA
  checkRank 'B' rankB indexB
  checkRank 'C' rankC indexC
  let !okay = all (\i -> elem i indexA || elem i indexB) indexC
  when (not okay) $
    Left . TblisError . pack $
      "invalid indices: " <> show indexA <> ", " <> show indexB <> ", and " <> show indexC

tblisTensorMult ::
  (IsTblisType a, PrimMonad m) =>
  Ptr TblisComm ->
  Ptr TblisConfig ->
  TblisTensor a ->
  String ->
  TblisTensor a ->
  String ->
  TblisTensor a ->
  String ->
  ExceptT TblisError m ()
tblisTensorMult c_comm c_config a indexA b indexB c indexC = do
  except $
    checkArgsMult (tblisTensorNDim a) indexA (tblisTensorNDim b) indexB (tblisTensorNDim c) indexC
  lift $
    unsafeIOToPrim $
      with a $ \c_a ->
        withCString indexA $ \c_indexA ->
          with b $ \c_b ->
            withCString indexB $ \c_indexB ->
              with c $ \c_c ->
                withCString indexC $ \c_indexC ->
                  tblis_tensor_mult c_comm c_config c_a c_indexA c_b c_indexB c_c c_indexC
{-# INLINE tblisTensorMult #-}
