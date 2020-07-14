{-# LANGUAGE CPP #-}

module Numeric.TBLIS.Internal
  ( TblisTensor(..),
    TblisType(..),
    withTblisTensor,
    tblis_tensor_add,
    tblis_tensor_mult,
  )
where

import Data.Complex
import Foreign
import Foreign.C.Types

-- We pretend like we don't have C99, because otherwise tblis uses complex which
-- c2hs cannot parse. Setting -std=c89 in cabal doesn't work since then C++
-- comments cannot be parsed. Yes, it's a nightmare...
#ifdef __STDC_VERSION__
#  undef __STDC_VERSION__
#endif
#include "tblis/tblis.h"
#include "wrapper.h"

type CLenType = {#type len_type#}
type CStrideType = {#type stride_type#}
type CLabelType = {#type label_type#}

data TblisTensor a = TblisTensor { ptr :: !(Ptr a), size :: ![Int], stride :: ![Int], scale :: !(Maybe a) }

class TblisType a where
  tblis_init_tensor :: Ptr () -> CUInt -> Ptr CLenType -> Ptr a -> Ptr CStrideType -> IO ()
  tblis_init_tensor_scaled :: Ptr () -> a -> CUInt -> Ptr CLenType -> Ptr a -> Ptr CStrideType -> IO ()

instance TblisType CFloat where
  tblis_init_tensor = {#call unsafe tblis_init_tensor_s#}
  tblis_init_tensor_scaled = {#call unsafe tblis_init_tensor_scaled_s#}

instance TblisType Float where
  tblis_init_tensor p n size dataPtr stride = tblis_init_tensor_s p n size (castPtr dataPtr) stride
  tblis_init_tensor_scaled p value n size dataPtr stride =
    tblis_init_tensor_scaled_s p (coerce value) n size (castPtr dataPtr) stride

instance TblisType (Complex CFloat) where
  tblis_init_tensor p n size dataPtr stride =
    {#call unsafe tblis_init_tensor_c#} p n size (castPtr dataPtr) stride
  tblis_init_tensor_scaled p value n size dataPtr stride = with value $ \valuePtr ->
    {#call unsafe tblis_hs_init_tensor_scaled_c#} p (castPtr valuePtr) n size (castPtr dataPtr) stride

instance TblisType (Complex Float) where
  tblis_init_tensor p n size dataPtr stride =
    tblis_init_tensor_c p n size (castPtr dataPtr) stride
  tblis_init_tensor_scaled p value n size dataPtr stride = with value $ \valuePtr ->
    tblis_hs_init_tensor_scaled_c p (castPtr valuePtr) n size (castPtr dataPtr) stride

instance TblisType CDouble where
  tblis_init_tensor = {#call unsafe tblis_init_tensor_d#}
  tblis_init_tensor_scaled = {#call unsafe tblis_init_tensor_scaled_d#}

instance TblisType Double where
  tblis_init_tensor p n size dataPtr stride = tblis_init_tensor_d p n size (castPtr dataPtr) stride
  tblis_init_tensor_scaled p value n size dataPtr stride =
    tblis_init_tensor_scaled_d p (coerce value) n size (castPtr dataPtr) stride

instance TblisType (Complex CDouble) where
  tblis_init_tensor p n size dataPtr stride =
    {#call unsafe tblis_init_tensor_z#} p n size (castPtr dataPtr) stride
  tblis_init_tensor_scaled p value n size dataPtr stride = with value $ \valuePtr ->
    {#call unsafe tblis_hs_init_tensor_scaled_z#} p (castPtr valuePtr) n size (castPtr dataPtr) stride

instance TblisType (Complex Double) where
  tblis_init_tensor p n size dataPtr stride =
    tblis_init_tensor_z p n size (castPtr dataPtr) stride
  tblis_init_tensor_scaled p value n size dataPtr stride = with value $ \valuePtr ->
    tblis_hs_init_tensor_scaled_z p (castPtr valuePtr) n size (castPtr dataPtr) stride

allocaTensor :: (Ptr () -> IO a) -> IO a
allocaTensor = allocaBytesAligned {#sizeof tblis_tensor#} {#alignof tblis_tensor#}

withTblisTensor :: TblisType a => TblisTensor a -> (Ptr () -> IO b) -> IO b
withTblisTensor (TblisTensor dataPtr size stride scale) f =
  allocaTensor $ \p ->
    withArray (fromIntegral <$> size) $ \size' ->
      withArray (fromIntegral <$> stride) $ \stride' -> do
        let !ndim = fromIntegral $ length size
        case scale of
          Just value -> tblis_init_tensor_scaled p value ndim size' dataPtr stride'
          Nothing -> tblis_init_tensor p ndim size' dataPtr stride'
        f p

{#fun unsafe tblis_tensor_add
  { `Ptr ()', `Ptr ()', `Ptr ()', id `Ptr CLabelType',
                        `Ptr ()', id `Ptr CLabelType' } -> `()' #}

{#fun unsafe tblis_tensor_mult
  { `Ptr ()', `Ptr ()', `Ptr ()', id `Ptr CLabelType',
                        `Ptr ()', id `Ptr CLabelType',
                        `Ptr ()', id `Ptr CLabelType' } -> `()' #}

