{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Numeric.TBLIS
  ( tblisAdd,
  )
where

import Control.Monad.Primitive
import DLPack
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Numeric.TBLIS.Types

toDLDataType :: TblisType a -> DLDataType
toDLDataType TblisFloat = DLDataType {dlDataTypeCode = DLFloat, dlDataTypeBits = 32, dlDataTypeLanes = 1}
toDLDataType TblisDouble = DLDataType {dlDataTypeCode = DLFloat, dlDataTypeBits = 64, dlDataTypeLanes = 1}
toDLDataType TblisComplexFloat = DLDataType {dlDataTypeCode = DLComplex, dlDataTypeBits = 64, dlDataTypeLanes = 1}
toDLDataType TblisComplexDouble = DLDataType {dlDataTypeCode = DLComplex, dlDataTypeBits = 128, dlDataTypeLanes = 1}
{-# INLINE toDLDataType #-}

requireType :: HasCallStack => TblisType a -> DLTensor -> b -> b
requireType !dtype !t
  | toDLDataType dtype == dlTensorDType t = id
  | otherwise =
    error $
      "DLTensor has wrong type: " <> show (dlTensorDType t) <> "; expected " <> show (toDLDataType dtype)
{-# INLINE requireType #-}

requireDevice :: HasCallStack => DLTensor -> b -> b
requireDevice t = case dlDeviceType (dlTensorDevice t) of
  DLCPU -> id
  DLCUDAHost -> id
  device ->
    error $
      "DLTensor is located on wrong device: " <> show device <> "; TBLIS only supports CPU tensors"
{-# INLINE requireDevice #-}

mkTblisTensor ::
  forall a.
  ( Coercible TblisLenType Int64,
    Coercible TblisStrideType Int64,
    IsTblisType a
  ) =>
  a ->
  Bool ->
  DLTensor ->
  TblisTensor a
mkTblisTensor α conj t =
  requireType (tblisTypeOf (Proxy @a)) t $
    requireDevice t $
      TblisTensor
        { tblisTensorConj = conj,
          tblisTensorScalar = TblisScalar α,
          tblisTensorData = dlTensorData t `plusPtr` fromIntegral (dlTensorByteOffset t),
          tblisTensorNDim = dlTensorNDim t,
          tblisTensorLen = castPtr (dlTensorShape t),
          tblisTensorStride = castPtr (dlTensorStrides t)
        }

-- void tblis_tensor_add(const tblis_comm* comm, const tblis_config* cfg,
--                       const tblis_tensor* A, const label_type* idx_A,
--                             tblis_tensor* B, const label_type* idx_B);
foreign import ccall "tblis_tensor_add"
  tblis_tensor_add :: Ptr TblisComm -> Ptr TblisConfig -> Ptr (TblisTensor a) -> Ptr TblisLabelType -> Ptr (TblisTensor a) -> Ptr TblisLabelType -> IO ()

tensorAdd :: (IsTblisType a, PrimMonad m) => TblisTensor a -> String -> TblisTensor a -> String -> m ()
tensorAdd a indexA b indexB = unsafeIOToPrim $
  with a $ \c_a ->
    withCString indexA $ \c_indexA ->
      with b $ \c_b ->
        withCString indexB $ \c_indexB ->
          tblis_tensor_add nullPtr nullPtr c_a c_indexA c_b c_indexB

tblisAdd :: forall a m. (IsTblisType a, PrimMonad m) => a -> Bool -> DLTensor -> String -> a -> DLTensor -> String -> m ()
tblisAdd α conjA tensorA indexA β tensorB indexB =
  tensorAdd
    (mkTblisTensor α conjA tensorA)
    indexA
    (mkTblisTensor β False tensorB)
    indexB
