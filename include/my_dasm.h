///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : my_dasm.h
/// Author  : Evgeny Khirin <>
/// Description : DynASM configurations for KLisp.
///-----------------------------------------------------------------------------
#ifndef __my_dasm_h__
#define __my_dasm_h__

//==============================================================================
// Set DynASM memory management macros. Must be defined before including DynASM
// files.
//==============================================================================
#define DASM_M_GROW(ctx, t, p, sz, need)                  \
  do {                                                    \
    size_t _old_sz = (sz), _sz = _old_sz, _need = (need); \
    if (_sz < _need) {                                    \
      if (_sz < 16) _sz = 16;                             \
      while (_sz < _need) _sz += _sz;                     \
      (p) = (t *)lisp_realloc((p), _old_sz, _sz);         \
      (sz) = _sz;                                         \
    }                                                     \
  } while(0)

#define DASM_M_FREE(ctx, p, sz)

//==============================================================================
// DynASM glue definitions.
//==============================================================================
#define DASM_EXTERN(ctx, addr, idx, type)   (SIGNAL_INTERNAL_ERROR(), 0)

#define DASM_FDEF INLINE

#ifdef DEBUG
#  define DASM_CHECKS 1
#endif // DEBUG

//==============================================================================
// include DynASM
//==============================================================================
#include "dynasm/dasm_proto.h"
#include "dynasm/dasm_x86.h"

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
INLINE void my_dasm_link(Dst_DECL, long * size) {
  size_t s;
  if (dasm_link(Dst, &s) != 0) {
    SIGNAL_INTERNAL_ERROR();
  }
  *size = s;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
INLINE void my_dasm_encode(Dst_DECL, void * buf) {
  if (dasm_encode(Dst, buf) != 0) {
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
INLINE void my_dasm_finish(Dst_DECL, function_t * fn, void * old_bcode) {
  my_dasm_link(Dst, &fn->bc_size);
  if (fn->bc_size <= fn->bc_capacity) {
    fn->bcode = old_bcode;
  } else {
    fn->bcode = lisp_alloc_atomic(fn->bc_size, NULL);
    fn->bc_capacity = fn->bc_size;
  }
  my_dasm_encode(Dst, fn->bcode);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
INLINE unsigned my_dasm_alloc_label(Dst_DECL, frame_t * env) {
  unsigned dl_id = env->dl_id++;
  if (env->dl_id >= env->dl_capacity) {
    env->dl_capacity = env->dl_id + env->dl_id;
    dasm_growpc(Dst, env->dl_capacity);
  }
  return dl_id;
}

#endif // __my_dasm_h__
