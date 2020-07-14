#include "wrapper.h"

void tblis_hs_init_tensor_scaled_c(tblis_tensor *t, const scomplex *scalar,
                                   unsigned ndim, len_type *len, scomplex *data,
                                   stride_type *stride) {
  tblis_init_tensor_scaled_c(t, *scalar, ndim, len, data, stride);
}

void tblis_hs_init_tensor_scaled_z(tblis_tensor *t, const dcomplex *scalar,
                                   unsigned ndim, len_type *len, dcomplex *data,
                                   stride_type *stride) {
  tblis_init_tensor_scaled_z(t, *scalar, ndim, len, data, stride);
}
