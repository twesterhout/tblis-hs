{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Numeric.TBLIS
  ( tblisAdd,
    tblisAdd',
    tblisGetNumberThreads,
    tblisSetNumberThreads,
    tblisSingle,
    tblisParallel,
    tblisDefaultConfig,
  )
where

import Control.Monad.Primitive
import DLPack
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Numeric.TBLIS.Types

toDLDataType :: forall a. IsDLDataType a => TblisType a -> DLDataType
toDLDataType _ = dlDataTypeOf (Proxy :: Proxy a)
{-# INLINE toDLDataType #-}

fromDLDataType :: DLDataType -> Maybe SomeTblisType
fromDLDataType dtype
  | dtype == toDLDataType TblisFloat = Just (SomeTblisType TblisFloat)
  | dtype == toDLDataType TblisDouble = Just (SomeTblisType TblisDouble)
  | dtype == toDLDataType TblisComplexFloat = Just (SomeTblisType TblisComplexFloat)
  | dtype == toDLDataType TblisComplexDouble = Just (SomeTblisType TblisComplexDouble)
  | otherwise = Nothing
{-# INLINE fromDLDataType #-}

requireType :: IsDLDataType a => TblisType a -> DLTensor -> Either Text ()
requireType !dtype !t
  | toDLDataType dtype == dlTensorDType t = Right ()
  | otherwise =
    Left $
      "DLTensor has wrong type: " <> show (dlTensorDType t) <> "; expected "
        <> show (toDLDataType dtype)
{-# INLINE requireType #-}

requireDevice :: DLTensor -> Either Text ()
requireDevice t = case dlDeviceType (dlTensorDevice t) of
  DLCPU -> Right ()
  DLCUDAHost -> Right ()
  device ->
    Left $
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

tblisFromDLTensor ::
  forall a.
  ( Coercible TblisLenType Int64,
    Coercible TblisStrideType Int64,
    IsDLDataType a,
    IsTblisType a
  ) =>
  DLTensor ->
  Either Text (TblisTensor a)
tblisFromDLTensor t = do
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
  TblisTensor a ->
  String ->
  TblisTensor a ->
  String ->
  m ()
tblisAdd = tblisAdd' tblisParallel tblisDefaultConfig
{-# INLINE tblisAdd #-}

tblisAdd' ::
  (IsTblisType a, PrimMonad m) =>
  Ptr TblisComm ->
  Ptr TblisConfig ->
  TblisTensor a ->
  String ->
  TblisTensor a ->
  String ->
  m ()
tblisAdd' c_comm c_config a indexA b indexB = unsafeIOToPrim $
  with a $ \c_a ->
    withCString indexA $ \c_indexA ->
      with b $ \c_b ->
        withCString indexB $ \c_indexB ->
          tblis_tensor_add c_comm c_config c_a c_indexA c_b c_indexB
{-# INLINEABLE tblisAdd' #-}

tblisPermute :: PrimMonad m => [Int] -> DLTensor -> DLTensor -> m ()
tblisPermute permutation tensorA tensorB = undefined
