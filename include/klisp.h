///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : klisp.h
/// Author  : Evgeny Khirin <>
/// Description : Khirin Lisp public definitions. Include this file
/// always first.
///-----------------------------------------------------------------------------
#ifndef __klisp_h__
#define __klisp_h__

#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <gmp.h>
#include <pthread.h>
#include <assert.h>
#include <setjmp.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <ucontext.h>

#include <unictype.h>
#include <uniconv.h>
#include <unicase.h>
#include <unistr.h>

#include <endian.h>
#include <byteswap.h>

//==============================================================================
// Compiler definitions
//==============================================================================
//------------------------------------------------------------------------------
// inline
//------------------------------------------------------------------------------
#ifdef __cplusplus
#define INLINE    inline
#else
#define INLINE    static inline
#endif

#include "dlist.h"

//------------------------------------------------------------------------------
// extern "C"
//------------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

//==============================================================================
// ALIGNUP - aligns number on boundary, which is power 2.
//==============================================================================
#define ALIGNUP(x, b)   (((long)(x) + (b) - 1) & (-b))

//==============================================================================
// IS_POW2 - returns true if number is power of 2.
//==============================================================================
#define IS_POW2(n) ((long)(n) > 0 && ((long)(n) & ((long)(n) - 1)) == 0)

//==============================================================================
// Lisp general object
//==============================================================================
typedef long                  term;

//==============================================================================
// Lisp functions interface
//==============================================================================
typedef term (*lisp_fun_t)(long nargs, const term * args);

//==============================================================================
// line size of level 1 of data cache
//==============================================================================
#define DATACACHE_LINESIZE    64

//==============================================================================
// Global terms
//==============================================================================
extern term g_package_class_name;
extern term g_lisp_package;
extern term g_kw_package;       // keyword package
extern term g_package_var;

extern term g_quote;
extern term g_quasiquote;
extern term g_unquote;
extern term g_unquote_splicing;
extern term g_function;

extern term g_unbound_marker;
extern term g_unbound_fun_marker;

extern term g_true;

// stream classes
extern term g_stream_class_name;
extern term g_binary_stream_class_name;
extern term g_file_stream_class_name;
extern term g_custom_stream_class_name;

// standard streams variables
extern term g_stdin_var;
extern term g_stdout_var;
extern term g_stderr_var;

// hashmap class name
extern term g_hashmap_class_name;

// treemap class name
extern term g_treemap_class_name;

// avlmap class name
extern term g_avlmap_class_name;

// threads classes
extern term g_thread_class_name;
extern term g_mutex_class_name;
extern term g_condition_class_name;
extern term g_rwlock_class_name;
extern term g_semaphore_class_name;
extern term g_tqueue_class_name;

// sockets
extern term g_socket_class_name;
extern term g_socket_stream_class_name;

// regular expressions
extern term g_regex_class_name;

// random state
extern term g_random_state_var;

//==============================================================================
// Forward declarations
//==============================================================================
INLINE term __ulong_to_bigint_term(unsigned long v);
INLINE term make_binary_from_binary(const uint8_t * p, long len);
INLINE double __term_to_double(term t);
INLINE term long_to_term(term t);
INLINE term __long_to_fixnum_term(long x);

//==============================================================================
// Error management
//==============================================================================
//------------------------------------------------------------------------------
// Exceptions list
//------------------------------------------------------------------------------
#define EXCEPTIONS_LIST                                              \
  DEF_EXCEPTION(not_implemented)                                     \
  DEF_EXCEPTION(internal_error)                                      \
  DEF_EXCEPTION(max_alloc_size_exceeded)                             \
  DEF_EXCEPTION(out_of_memory)                                       \
  DEF_EXCEPTION(out_of_range)                                        \
  DEF_EXCEPTION(immutable_object)                                    \
  DEF_EXCEPTION(out_of_capacity)                                     \
  DEF_EXCEPTION(syntax_error)                                        \
  DEF_EXCEPTION(invalid_utf8_string)                                 \
  DEF_EXCEPTION(unexpected_eof)                                      \
  DEF_EXCEPTION(unbound_variable)                                    \
  DEF_EXCEPTION(unbound_function)                                    \
  DEF_EXCEPTION(keyword_symbol)                                      \
  DEF_EXCEPTION(invalid_arg)                                         \
  DEF_EXCEPTION(system_error)                                        \
  DEF_EXCEPTION(invalid_args_count)                                  \
  DEF_EXCEPTION(duplicate_tag)                                       \
  DEF_EXCEPTION(invalid_tag)                                         \
  DEF_EXCEPTION(not_function)                                        \
  DEF_EXCEPTION(no_catch)                                            \
  DEF_EXCEPTION(loop_in_list)                                        \
  DEF_EXCEPTION(invalid_block)                                       \
  DEF_EXCEPTION(closure_in_macrolet)                                 \
  DEF_EXCEPTION(stack_aligment)                                      \
  DEF_EXCEPTION(bad_signature)                                       \
  DEF_EXCEPTION(invalid_utf32_char)                                  \
  DEF_EXCEPTION(invalid_key_args)                                    \
  DEF_EXCEPTION(different_list_length)                               \
  DEF_EXCEPTION(invalid_printf_format)                               \
  DEF_EXCEPTION(too_few_printf_args)                                 \
  DEF_EXCEPTION(too_much_printf_args)                                \
  DEF_EXCEPTION(networking)                                          \
  DEF_EXCEPTION(timeout)                                             \
  DEF_EXCEPTION(unsupported_stream_operation)                        \
  DEF_EXCEPTION(regex_error)                                         \
  DEF_EXCEPTION(invalid_request_line)                                \
  DEF_EXCEPTION(invalid_url_encoding)                                \
  DEF_EXCEPTION(http_headers_too_large)                              \
  DEF_EXCEPTION(invalid_http_headers)                                \
  DEF_EXCEPTION(error)                                               \
  DEF_EXCEPTION(invalid_random_state)                                \
  DEF_EXCEPTION(disassembler_failed)

//------------------------------------------------------------------------------
// Declare exceptions
//------------------------------------------------------------------------------
#define DEF_EXCEPTION(e)                        \
  extern term g_ ## e;

EXCEPTIONS_LIST

#undef DEF_EXCEPTION

//------------------------------------------------------------------------------
// Signals Lisp error
//------------------------------------------------------------------------------
term lisp_signal(term label, term value) __attribute__ ((__noreturn__));
void lisp_signal_system_error() __attribute__ ((__noreturn__));
term lisp_throw(term tag, term value) __attribute__ ((__noreturn__));
term lisp_error(long nargs, const term * args) __attribute__ ((__noreturn__));
void lisp_error_sz(term label, const char * fmt, ...) __attribute__ ((__noreturn__));

//-------------------------------------------------------------------
// INTERNAL_ERROR macro
//-------------------------------------------------------------------
#define SIGNAL_INTERNAL_ERROR(fmt, ...)                                 \
  lisp_error_sz(g_internal_error, "Internal error (%s:%d): " fmt,       \
                __FILE__, __LINE__, ## __VA_ARGS__)

  //#define SIGNAL_INTERNAL_ERROR(...) __SIGNAL_INTERNAL_ERROR(__VA_ARGS__)

//-------------------------------------------------------------------
// NOT_IMPLEMENTED macro
//-------------------------------------------------------------------
#define SIGNAL_NOT_IMPLEMENTED()                                        \
  lisp_signal(g_not_implemented,                                        \
              cons(make_binary_from_binary((const uint8_t *)__FILE__,   \
                                           sizeof(__FILE__) - 1),       \
                   __long_to_fixnum_term(__LINE__)))

//-------------------------------------------------------------------
// TRACE
//-------------------------------------------------------------------
#define TRACE(msg, ...)                         \
  do {                                          \
    printf(msg "\n", ## __VA_ARGS__);           \
    fflush(stdout);                             \
  } while (0)

//==============================================================================
// Lisp objects
//==============================================================================
typedef enum term_type_e {
  null_e,                       // nil
  symbol_e,                     // symbol
  fixnum_e,                     // fixnum
  bigint_e,                     // bigint
  double_e,                     // double
  binary_e,                     // binary - vector of bytes
  ustring_e,                    // UTF-32 string - vector of UTF-32 characters
  vector_e,                     // vector
  cons_e,                       // cons
  // Objects of types below can be compared by eq function only
  __first_non_comparable_type__ = cons_e + 1,
  lambda_e = __first_non_comparable_type__,                     // lambda
  macro_e,                      // macro
  custom_e,                     // custom
} term_type_e;

//------------------------------------------------------------------------------
// nil - null tagged pointer
//------------------------------------------------------------------------------
#define nil    7

//------------------------------------------------------------------------------
// is_null
//------------------------------------------------------------------------------
#define is_null(x)    ((x) == nil)

//------------------------------------------------------------------------------
// Objects storage scheme. According to C standard malloc should return pointer
// aligned at least on 8 bytes boundary (size of double).
// 3 low bits of term has following encoding:
//   xx0 - fixnum, stored in term itself.
//   001 - pointer to symbol_t
//   011 - pointer to cons_t
//   101 - pointer to double
//   111 - pointer to other Lisp object. Object MUST have long, containing
//         term_type_e, as its first field.
//------------------------------------------------------------------------------
INLINE term_type_e get_term_type(term x) {
  if (is_null(x)) {
    return null_e;
  }
  if ((x & (term)1) == 0) {
    return fixnum_e;
  }
  switch (x & (term)7) {
  case 1:
    return symbol_e;
  case 3:
    return cons_e;
  case 5:
    return double_e;
  default:
    return (term_type_e)*((long *)(x ^ (term)7));
  }
}

//------------------------------------------------------------------------------
// is_integer
//------------------------------------------------------------------------------
INLINE int is_integer(term x) {
  term_type_e t = get_term_type(x);
  return t == fixnum_e || t == bigint_e;
}

//------------------------------------------------------------------------------
// is_number
//------------------------------------------------------------------------------
INLINE int is_number(term x) {
  term_type_e t = get_term_type(x);
  return t == fixnum_e || t == bigint_e || t == double_e;
}

//------------------------------------------------------------------------------
// Internal function: converts C pointer to Lisp object
//------------------------------------------------------------------------------
// INLINE term __pointer_to_term(const void * x) {
//   if (((long)x & 7) != 0) {
//     SIGNAL_INTERNAL_ERROR();
//   }
//   return (term)x | (term)7;
// }
#define __pointer_to_term(x)    (assert(((long)(x) & 7) == 0), (term)(x) | (term)7)

//------------------------------------------------------------------------------
// Internal function: converts Lisp object to C pointer
//------------------------------------------------------------------------------
#define __term_to_pointer(x)    (void *)((x) ^ (term)7)

//==============================================================================
// Fixnums
//==============================================================================
#define FIXNUM_MAX          (LONG_MAX >> 1)
#define FIXNUM_MIN          (LONG_MIN >> 1)

#define is_fixnum(x)    (((x) & (term)1) == 0)

INLINE term __long_to_fixnum_term(long x) {
  assert(x >= FIXNUM_MIN && x <= FIXNUM_MAX);
  return x << 1;
}

INLINE term __ulong_to_fixnum_term(unsigned long x) {
  assert(x >= 0 && x <= FIXNUM_MAX);
  return x << 1;
}

INLINE long __fixnum_term_to_long(term x) {
  assert(get_term_type(x) == fixnum_e);
  return x >> 1;
}

//==============================================================================
// Garbage collector C API
//==============================================================================
//------------------------------------------------------------------------------
// Object finalizer. Finalizer is called when object is about to be
// collected. It may be called from any thread. So finalizer must not acquire
// user visible locks.
//------------------------------------------------------------------------------
typedef void (* finalizer_t)(void *);

//------------------------------------------------------------------------------
// Garbage collector API
//------------------------------------------------------------------------------
void gc_init(long heap_size);
// if allocated memory is not atomic, than gc_alloc fills the memory with 0 bytes.
void * gc_alloc(long size, finalizer_t finalizer, int atomic);
void gc_collect();
void gc_add_current_thread();
void gc_remove_current_thread();
void gc_lock();
void gc_unlock();
double gc_time();
long gc_threads();
long gc_live_ptrs();
long gc_ptrs_capacity();
long gc_heap_size();

//------------------------------------------------------------------------------
// MAX_ALLOC
//------------------------------------------------------------------------------
#define MAX_ALLOC    FIXNUM_MAX

//------------------------------------------------------------------------------
// Allocates memory from heap and checks parameters and return value.
// Allocated memory may contain pointers. Block of memory may be
// reallocated.
//------------------------------------------------------------------------------
INLINE void * lisp_alloc(unsigned long size, finalizer_t fn) {
  if (size > MAX_ALLOC) {
    lisp_signal(g_max_alloc_size_exceeded, __ulong_to_bigint_term(size));
  }
  void * r = gc_alloc(size, fn, 0);
  if (r == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(size));
  }
  assert(((long)r & 7) == 0);
  return r;
}

//------------------------------------------------------------------------------
// Allocates memory from heap and checks parameters and return value
// Allocated memory can not contain pointers. Block of memory may be
// reallocated.
//------------------------------------------------------------------------------
INLINE void * lisp_alloc_atomic(unsigned long size, finalizer_t fn) {
  if (size > MAX_ALLOC) {
    lisp_signal(g_max_alloc_size_exceeded, __ulong_to_bigint_term(size));
  }
  void * r = gc_alloc(size, fn, 1);
  if (r == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(size));
  }
  assert(((long)r & 7) == 0);
  return r;
}

//------------------------------------------------------------------------------
// Reallocates pointer, previosly allocated with lisp_alloc
//------------------------------------------------------------------------------
INLINE void * lisp_realloc(void * p, unsigned long old_size, unsigned long new_size) {
  if (new_size == 0) {
    return NULL;
  }
  if (new_size > MAX_ALLOC) {
    lisp_signal(g_max_alloc_size_exceeded, __ulong_to_bigint_term(new_size));
  }
  void * r = gc_alloc(new_size, NULL, 0);
  if (r == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(new_size));
  }
  assert(((long)r & 7) == 0);
  memcpy(r, p, old_size);
  return r;
}

//------------------------------------------------------------------------------
// Reallocates pointer, previosly allocated with lisp_alloc_atomic
//------------------------------------------------------------------------------
INLINE void * lisp_realloc_atomic(void * p, unsigned long old_size, unsigned long new_size) {
  if (new_size == 0) {
    return NULL;
  }
  if (new_size > MAX_ALLOC) {
    lisp_signal(g_max_alloc_size_exceeded, __ulong_to_bigint_term(new_size));
  }
  void * r = gc_alloc(new_size, NULL, 1);
  if (r == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(new_size));
  }
  assert(((long)r & 7) == 0);
  memcpy(r, p, old_size);
  return r;
}

//------------------------------------------------------------------------------
// Allocates executable memory from heap and checks parameters and return value.
//------------------------------------------------------------------------------
INLINE void * lisp_alloc_executable(unsigned long size) {
  return lisp_alloc_atomic(size, NULL);
}

//------------------------------------------------------------------------------
// Reallocates pointer, previosly allocated with lisp_alloc_executable
//------------------------------------------------------------------------------
INLINE void * lisp_realloc_executable(void * p, unsigned long old_size, unsigned long new_size) {
  return lisp_realloc_atomic(p, old_size, new_size);
}

//==============================================================================
// Bigints
//==============================================================================
typedef struct bigint_t bigint_t;

struct bigint_t {
  long        type;
  mpz_t       mpz;
  long        hash;
  int         hash_valid;
};

#define is_bigint(x)    (get_term_type(x) == bigint_e)

//------------------------------------------------------------------------------
// bigint_init
//------------------------------------------------------------------------------
INLINE void bigint_init(bigint_t * b) {
  b->type = bigint_e;
  mpz_init(b->mpz);
  b->hash_valid = 0;
}

//------------------------------------------------------------------------------
// bigint_init_si
//------------------------------------------------------------------------------
INLINE void bigint_init_si(bigint_t * b, long v) {
  b->type = bigint_e;
  mpz_init_set_si(b->mpz, v);
  b->hash_valid = 0;
}

//------------------------------------------------------------------------------
// bigint_init_ui
//------------------------------------------------------------------------------
INLINE void bigint_init_ui(bigint_t * b, unsigned long v) {
  b->type = bigint_e;
  mpz_init_set_ui(b->mpz, v);
  b->hash_valid = 0;
}

//------------------------------------------------------------------------------
// __long_to_hash_code - internal function
//------------------------------------------------------------------------------
INLINE long __long_to_hash_code(long l) {
  return (unsigned long)l % (FIXNUM_MAX + 1);
}

//------------------------------------------------------------------------------
// bigint_hash_code
//------------------------------------------------------------------------------
INLINE long bigint_hash_code(const bigint_t * b) {
  if (b->hash_valid) {
    return b->hash;
  }
  long h = 0;
  long size = abs(b->mpz->_mp_size);
  long i;
  for (i = 0; i < size; ++i) {
    h += b->mpz->_mp_d[i];
  }
  ((bigint_t *)b)->hash = __long_to_hash_code(h);
  ((bigint_t *)b)->hash_valid = 1;
  return b->hash;
}

#if defined(__i386__)
//------------------------------------------------------------------------------
// X86 helper functions
//------------------------------------------------------------------------------
INLINE void __bigint_assign_int64(bigint_t * b, int64_t v) {
  uint64_t uv = v > 0 ? v : -v;
  mpz_import(b->mpz, 1, 1, sizeof(uv), 0, 0, &uv);
  if (v < 0) {
    mpz_neg(b->mpz, b->mpz);
  }
}

INLINE void __bigint_assign_uint64(bigint_t * b, uint64_t v) {
  mpz_import(b->mpz, 1, 1, sizeof(v), 0, 0, &v);
}

INLINE int64_t __bigint_get_int64(const bigint_t * b) {
  uint64_t r = 0;
  mpz_export(&r, NULL, 1, sizeof(r), 0, 0, b->mpz);
  if (mpz_sgn(b->mpz) < 0) {
    return -r;
  }
  return r;
}

INLINE uint64_t __bigint_get_uint64(const bigint_t * b) {
  uint64_t r = 0;
  mpz_export(&r, NULL, 1, sizeof(r), 0, 0, b->mpz);
  return r;
}

INLINE int __bigint_fits_int64(const bigint_t * b) {
  if (mpz_sizeinbase(b->mpz, 2) > 64) {
    return 0;
  }
  uint64_t v = __bigint_get_uint64(b);
  if (mpz_sgn(b->mpz) >= 0) {
    if (v > INT64_MAX) {
      return 0;
    }
    return 1;
  }
  // b is negative
  if (v > ((uint64_t)INT64_MAX + 1)) {
    return 0;
  }
  return 1;
}

INLINE int __bigint_fits_uint64(const bigint_t * b) {
  if (mpz_sgn(b->mpz) < 0) {
    // b is negative
    return 0;
  }
  if (mpz_sizeinbase(b->mpz, 2) > 64) {
    return 0;
  }
  return 1;
}

#elif defined (__x86_64__)

//------------------------------------------------------------------------------
// AMD64 helper functions
//------------------------------------------------------------------------------
INLINE void __bigint_assign_int64(bigint_t * b, int64_t v) {
  mpz_init_set_si(b->mpz, v);
}

INLINE void __bigint_assign_uint64(bigint_t * b, uint64_t v) {
  mpz_init_set_ui(b->mpz, v);
}

INLINE int64_t __bigint_get_int64(const bigint_t * b) {
  return mpz_get_si(b->mpz);
}

INLINE uint64_t __bigint_get_uint64(const bigint_t * b) {
  return mpz_get_ui(b->mpz);
}

INLINE int __bigint_fits_int64(const bigint_t * b) {
  return mpz_fits_slong_p(b->mpz);
}

INLINE int __bigint_fits_uint64(const bigint_t * b) {
  return mpz_fits_ulong_p(b->mpz);
}

#else // __x86_64__

#error Unknown CPU architecture

#endif

//------------------------------------------------------------------------------
// Internal bigint operations
//------------------------------------------------------------------------------
INLINE const bigint_t * __term_to_bigint(term x) {
  assert(get_term_type(x) == bigint_e);
  return (const bigint_t *)__term_to_pointer(x);
}

INLINE term __long_to_bigint_term(long v) {
  bigint_t * b = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
  bigint_init_si(b, v);
  return __pointer_to_term(b);
}

INLINE term __ulong_to_bigint_term(unsigned long v) {
  bigint_t * b = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
  bigint_init_ui(b, v);
  return __pointer_to_term(b);
}

INLINE term __int64_to_bigint_term(int64_t v) {
  bigint_t * b = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
  bigint_init(b);
  __bigint_assign_int64(b, v);
  return __pointer_to_term(b);
}

INLINE term __uint64_to_bigint_term(uint64_t v) {
  bigint_t * b = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
  bigint_init(b);
  __bigint_assign_uint64(b, v);
  return __pointer_to_term(b);
}

INLINE term __bigint_to_bigint_term(const bigint_t * v) {
  bigint_t * b = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
  bigint_init(b);
  mpz_set(b->mpz, v->mpz);
  return __pointer_to_term(b);
}

INLINE term __double_to_bigint_term(double v) {
  bigint_t * b = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
  bigint_init(b);
  mpz_set_d(b->mpz, v);
  return __pointer_to_term(b);
}

INLINE term __make_bigint() {
  bigint_t * b = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
  bigint_init(b);
  return __pointer_to_term(b);
}

//------------------------------------------------------------------------------
// Public integers operations
//------------------------------------------------------------------------------
INLINE const bigint_t * term_to_bigint(term x) {
  if (!is_bigint(x)) {
    lisp_signal(g_invalid_arg, x);
  }
  return __term_to_bigint(x);
}

INLINE term long_to_term(long x) {
  if (x > FIXNUM_MAX || x < FIXNUM_MIN) {
    return __long_to_bigint_term(x);
  }
  return __long_to_fixnum_term(x);
}

INLINE term ulong_to_term(unsigned long x) {
  if (x > FIXNUM_MAX) {
    return __ulong_to_bigint_term(x);
  }
  return __long_to_fixnum_term(x);
}

INLINE term int64_to_term(int64_t v) {
  if (v >= FIXNUM_MIN && v <= FIXNUM_MAX) {
    return __long_to_fixnum_term(v);
  }
  return __int64_to_bigint_term(v);
}

INLINE term uint64_to_term(uint64_t v) {
  if (v <= (uint64_t)FIXNUM_MAX) {
    return __long_to_fixnum_term(v);
  }
  return __uint64_to_bigint_term(v);
}

INLINE int64_t term_to_int64(term t) {
  switch (get_term_type(t)) {
  case fixnum_e:
    return __fixnum_term_to_long(t);
  case bigint_e:
    {
      const bigint_t * b = __term_to_bigint(t);
      if (!__bigint_fits_int64(b)) {
        lisp_signal(g_out_of_range, t);
      }
      return __bigint_get_int64(b);
    }
  default:
    lisp_signal(g_invalid_arg, t);
  }
}

INLINE uint64_t term_to_uint64(term t) {
  switch (get_term_type(t)) {
  case fixnum_e:
    {
      long f = __fixnum_term_to_long(t);
      if (f < 0) {
        lisp_signal(g_out_of_range, t);
      }
      return f;
    }
  case bigint_e:
    {
      const bigint_t * b = __term_to_bigint(t);
      if (!__bigint_fits_uint64(b)) {
        lisp_signal(g_out_of_range, t);
      }
      return __bigint_get_uint64(b);
    }
  default:
    lisp_signal(g_invalid_arg, t);
  }
}

INLINE long term_to_long(term t) {
  int64_t res = term_to_int64(t);
  if (res < LONG_MIN || res > LONG_MAX) {
    lisp_signal(g_out_of_range, t);
  }
  return res;
}

INLINE unsigned long term_to_ulong(term t) {
  uint64_t res = term_to_uint64(t);
  if (res > ULONG_MAX) {
    lisp_signal(g_out_of_range, t);
  }
  return res;
}

INLINE int term_to_int(term t) {
  int res = term_to_long(t);
  if (res < INT_MIN || res > INT_MAX) {
    lisp_signal(g_out_of_range, t);
  }
  return res;
}

INLINE int term_to_uint(term t) {
  unsigned res = term_to_ulong(t);
  if (res > UINT_MAX) {
    lisp_signal(g_out_of_range, t);
  }
  return res;
}

INLINE uint8_t term_to_uint8(term t) {
  uint64_t v = term_to_uint64(t);
  if (v > UINT8_MAX) {
    lisp_signal(g_invalid_arg, t);
  }
  return v;
}

INLINE int8_t term_to_int8(term t) {
  int64_t v = term_to_int64(t);
  if (v < INT8_MIN || v > INT8_MAX) {
    lisp_signal(g_invalid_arg, t);
  }
  return v;
}

INLINE uint16_t term_to_uint16(term t) {
  uint64_t v = term_to_uint64(t);
  if (v > UINT16_MAX) {
    lisp_signal(g_invalid_arg, t);
  }
  return v;
}

INLINE int16_t term_to_int16(term t) {
  int64_t v = term_to_int64(t);
  if (v < INT16_MIN || v > INT16_MAX) {
    lisp_signal(g_invalid_arg, t);
  }
  return v;
}

INLINE uint32_t term_to_uint32(term t) {
  uint64_t v = term_to_uint64(t);
  if (v > UINT32_MAX) {
    lisp_signal(g_invalid_arg, t);
  }
  return v;
}

INLINE int32_t term_to_int32(term t) {
  int64_t v = term_to_int64(t);
  if (v < INT32_MIN || v > INT32_MAX) {
    lisp_signal(g_invalid_arg, t);
  }
  return v;
}

INLINE term int8_to_term(int8_t v) {
  return int64_to_term(v);
}

INLINE term uint8_to_term(uint8_t v) {
  return uint64_to_term(v);
}

INLINE term int16_to_term(int16_t v) {
  return int64_to_term(v);
}

INLINE term uint16_to_term(uint16_t v) {
  return uint64_to_term(v);
}

INLINE term int32_to_term(int32_t v) {
  return int64_to_term(v);
}

INLINE term uint32_to_term(uint32_t v) {
  return uint64_to_term(v);
}

//------------------------------------------------------------------------------
// double term operations
//------------------------------------------------------------------------------
#define is_double(x)    (((x) & (term)7) == 5)

INLINE double __term_to_double(term t) {
  assert(get_term_type(t) == double_e);
  return *(double *)(t ^ (term)5);
}

INLINE double term_to_double(term t) {
  switch (get_term_type(t)) {
  case fixnum_e:
    return __fixnum_term_to_long(t);
  case bigint_e:
    {
      const bigint_t * b = __term_to_bigint(t);
      return mpz_get_d(b->mpz);
    }
  case double_e:
    return __term_to_double(t);
  default:
    lisp_signal(g_invalid_arg, t);
  }
}

INLINE term double_to_term(double v) {
  if (isfinite(v)) {
    long l = v;
    if (l == v && l >= FIXNUM_MIN && l <= FIXNUM_MAX) {
      return __long_to_fixnum_term(l);
    }
  }
  double * p = (double *)lisp_alloc_atomic(sizeof(double), NULL);
  *p = v;
  return (term)p | (term)5;
}

//==============================================================================
// General comparison
//==============================================================================
//-------------------------------------------------------------------
// Function: (compare x y) ==> fixnum
//
// Parameters: x - comparable object
//             y - comparable object
//
// Compares two objects according following rules (objects may be different types):
//   nil < symbol < number < binary < ustring < vector < cons
//
// Symbols are compared by their packages names first. If names of packages are
// equal, than symbols are compared by names.
//
// Containers objects of same type are compared lexicographically.
//
// Objects of other types may be tested for equalence only with eq, eql or equal
// functions.
//
// Returns negative fixnum number if x < y, positive fixnum number if x > y and
// zero if x equal to y.
// -------------------------------------------------------------------
long term_cmp(term l, term r);

//==============================================================================
// Hash code
//==============================================================================
//------------------------------------------------------------------------------
// Function: (hash-code x) ==> positive fixnum
//
// Parameters: x - object
//
// Returns hash code of Lisp object. Hash code is positive fixnum.
//------------------------------------------------------------------------------
long term_hash_code(term x);

INLINE term lisp_hash_code(term x) {
  return __ulong_to_fixnum_term(term_hash_code(x));
}

//==============================================================================
// Equivalence functions
//==============================================================================
//------------------------------------------------------------------------------
// Function: (eq x y) ==> bool
//
// Parameters: x - object
//             y - object
//
// It is necessary to know how Lisp objects are represented internally in
// computer, in order to understand eq function.
//
// All objects in Lisp are referenced by machine word. Each machine word
// associated with Lisp object has type tag. Lisp reserves few lower bits of
// machine word for type tag.  Objects those fit remaining bits of machine word are
// stored immediately in that word, while other are stored in some location of
// computer memory and address to that location is stored in machine
// word.
//
// Nil and small integers (fixnums) are always stored in machine word by Lisp.
// Objects of other types are represented by their address in computer memory.
//
// Function eq is fastest comparison function and just compares two machine words
// that represent Lisp objects. It returns true, if they are equal. That means it
// returns true, if both objects are nil, or both objects are same small integers
// or they address same identical memory addres of Lisp object.
//
// Examples:
//   (eq () ())       ; t
//   (eq 'x 'x)       ; t
//   (eq 'x 'y)       ; nil
//   (eq (symbol "x") (symbol "x")) ; nil
//   (eq '(1) '(1))   ; undefined
//   (eq (list 1) (list 1)) ; nil
//   (eq 0 0)         ; t, because computer requires at least three words of
//                     ; memory to hold that expression. So at least 2-bits will be
//                     ; available for fixnums. And integers in range [-2, 1] are
//                     ; always stored in machine word.
//   (eq 2 2)         ; undefined
//   (eq "" "")       ; undefined
//   (eq #v(1) #v(1))     ; undefined
//   (eq #"a" #"a")   ; undefined
//------------------------------------------------------------------------------
#define eq(x, y)    ((x) == (y))

INLINE term lisp_eq(term x, term y) {
  if (eq(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (eql x y) ==> bool
//
// Parameters: x - object
//             y - object
//
// The value of eql is true of two objects, x and y, in the folowing cases:
//   1. If x and y are eq.
//   2. If x and y are both numbers of the same type and the same value.
// Otherwise the value of eql is false.
//------------------------------------------------------------------------------
INLINE int eql(term l, term r) {
  if (eq(l, r)) {
    return 1;
  }
  term_type_e tp = get_term_type(l);
  if (get_term_type(r) != tp) {
    return 0;
  }
  switch (tp) {
  case bigint_e:
    return mpz_cmp(__term_to_bigint(l)->mpz, __term_to_bigint(r)->mpz) == 0;
  case double_e:
    return __term_to_double(l) == __term_to_double(r);
  default:
    return 0;
  }
}

//------------------------------------------------------------------------------
// Function: (equal x y) ==> bool
//
// Parameters: x - object
//             y - object
//
// The value of equal is true of two objects, x and y, in the folowing cases:
//   1. If x and y are eql.
//   2. x and y are numbers and are numerically equal.
//   3. If x and y are containers of same type and same size, and each element of x
//      is equal to corresponding element of y.
// Otherwise the value of equal is false.
//------------------------------------------------------------------------------
INLINE int equal(term x, term y) {
  if (eql(x, y)) {
    return 1;
  }
  term_type_e xtp = get_term_type(x);
  term_type_e ytp = get_term_type(y);
  if (xtp != ytp || xtp == fixnum_e || xtp == bigint_e ||
      xtp == double_e || xtp == symbol_e) {
    // already handled by eql
    return 0;
  }
  if (xtp >= __first_non_comparable_type__ ||
      ytp >= __first_non_comparable_type__) {
    return 0;
  }
  return term_cmp(x, y) == 0;
}

//==============================================================================
// Conses
//==============================================================================
typedef struct cons_t      cons_t;

struct cons_t {
  term          first;
  term          second;
  int           immutable;
};

//------------------------------------------------------------------------------
// Function: (consp x) ==> bool
//
// Parameters: x - object
//
// Returns t if object is pair.
//------------------------------------------------------------------------------
#define is_cons(x)   (get_term_type(x) == cons_e)

//------------------------------------------------------------------------------
// cons_init
//------------------------------------------------------------------------------
INLINE void cons_init(cons_t * p, term first, term second) {
  p->first = first;
  p->second = second;
  p->immutable = 0;
}

//------------------------------------------------------------------------------
// __pointer_to_cons_term
//------------------------------------------------------------------------------
// INLINE term __pointer_to_cons_term(const void * x) {
//   if (((long)x & 7) != 0) {
//     SIGNAL_INTERNAL_ERROR();
//   }
//   return (term)x | (term)3;
// }
#define __pointer_to_cons_term(x)    (assert(((long)(x) & 7) == 0), (term)(x) | (term)3)

//------------------------------------------------------------------------------
// __term_to_cons
//------------------------------------------------------------------------------
INLINE cons_t * __term_to_cons(term t) {
  assert(get_term_type(t) == cons_e);
  return (cons_t *)(t ^ (term)3);
}

//------------------------------------------------------------------------------
// Function: (cons x y) ==> (x . y)
//------------------------------------------------------------------------------
INLINE term cons(term first, term second) {
  cons_t * p = (cons_t *)lisp_alloc(sizeof(cons_t), NULL);
  cons_init(p, first, second);
  return __pointer_to_cons_term(p);
}

//------------------------------------------------------------------------------
// Function: (list &rest args) ==> proper list
// Always returns fresh list.
//------------------------------------------------------------------------------
INLINE term lisp_list(long nargs, const term * args) {
  if (nargs == 0) {
    return nil;
  }
  // Pointers returned by lisp_alloc are aligned on 8 byte boundary and
  // __pointer_to_* functions also expect that term will be aligned on same
  // boundary.
  long cons_size = ALIGNUP(sizeof(cons_t), 8);
  cons_t * res = (cons_t *)lisp_alloc(cons_size * nargs, NULL);
  cons_t * p = res;
  long i;
  long end = nargs - 1;
  for (i = 0; i < end; ++i) {
    cons_t * next = (cons_t *)((uint8_t *)p + cons_size);
    cons_init(p, args[i], __pointer_to_cons_term(next));
    p = next;
  }
  cons_init(p, args[i], nil);
  return __pointer_to_cons_term(res);
}

//------------------------------------------------------------------------------
// cons_hash_code
//------------------------------------------------------------------------------
INLINE long cons_hash_code(const cons_t * p) {
  // hash code of pair can not be memoized.
  return __long_to_hash_code(term_hash_code(p->first) +
                             term_hash_code(p->second));
}

//------------------------------------------------------------------------------
// cons_cmp
// returns negative if l < r,
// returns 0 if l = r,
// returns positive if l > r.
//------------------------------------------------------------------------------
INLINE long cons_cmp(const cons_t * l, const cons_t * r) {
  long cmp_rc = term_cmp(l->first, r->first);
  if (cmp_rc != 0) {
    return cmp_rc;
  }
  return term_cmp(l->second, r->second);
}

//------------------------------------------------------------------------------
// Cons term decoding
//------------------------------------------------------------------------------
INLINE const cons_t * __get_cons_for_read(term t) {
  assert(get_term_type(t) == cons_e);
  return __term_to_cons(t);
}

INLINE cons_t * __get_cons_for_write(term t) {
  assert(get_term_type(t) == cons_e);
  cons_t * p = __term_to_cons(t);
  if (p->immutable) {
    lisp_signal(g_immutable_object, t);
  }
  return p;
}

INLINE const cons_t * get_cons_for_read(term t) {
  if (!is_cons(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __get_cons_for_read(t);
}

INLINE cons_t * get_cons_for_write(term t) {
  if (!is_cons(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __get_cons_for_write(t);
}

//------------------------------------------------------------------------------
// Function: (car x) ==> object
//------------------------------------------------------------------------------
INLINE term car(term t) {
  return get_cons_for_read(t)->first;
}

//------------------------------------------------------------------------------
// Function: (set-car x val) ==> x
//------------------------------------------------------------------------------
INLINE term set_car(term t, term val) {
  get_cons_for_write(t)->first = val;
  return t;
}

//------------------------------------------------------------------------------
// Function: (cdr x) ==> object
//------------------------------------------------------------------------------
INLINE term cdr(term t) {
  return get_cons_for_read(t)->second;
}

//------------------------------------------------------------------------------
// Function: (set-cdr x val) ==> x
//------------------------------------------------------------------------------
INLINE term set_cdr(term t, term val) {
  get_cons_for_write(t)->second = val;
  return t;
}

//------------------------------------------------------------------------------
// car and cdr combinations
//------------------------------------------------------------------------------
INLINE term caar(term t)        {return car (car (t ));}
INLINE term cadr(term t)        {return car (cdr (t ));}
INLINE term cdar(term t)        {return cdr (car (t ));}
INLINE term cddr(term t)        {return cdr (cdr (t ));}
INLINE term caaar(term t)       {return car (car (car (t )));}
INLINE term caadr(term t)       {return car (car (cdr (t )));}
INLINE term cadar(term t)       {return car (cdr (car (t )));}
INLINE term caddr(term t)       {return car (cdr (cdr (t )));}
INLINE term cdaar(term t)       {return cdr (car (car (t )));}
INLINE term cdadr(term t)       {return cdr (car (cdr (t )));}
INLINE term cddar(term t)       {return cdr (cdr (car (t )));}
INLINE term cdddr(term t)       {return cdr (cdr (cdr (t )));}
INLINE term caaaar(term t)      {return car (car (car (car (t ))));}
INLINE term caaadr(term t)      {return car (car (car (cdr (t ))));}
INLINE term caadar(term t)      {return car (car (cdr (car (t ))));}
INLINE term caaddr(term t)      {return car (car (cdr (cdr (t ))));}
INLINE term cadaar(term t)      {return car (cdr (car (car (t ))));}
INLINE term cadadr(term t)      {return car (cdr (car (cdr (t ))));}
INLINE term caddar(term t)      {return car (cdr (cdr (car (t ))));}
INLINE term cadddr(term t)      {return car (cdr (cdr (cdr (t ))));}
INLINE term cdaaar(term t)      {return cdr (car (car (car (t ))));}
INLINE term cdaadr(term t)      {return cdr (car (car (cdr (t ))));}
INLINE term cdadar(term t)      {return cdr (car (cdr (car (t ))));}
INLINE term cdaddr(term t)      {return cdr (car (cdr (cdr (t ))));}
INLINE term cddaar(term t)      {return cdr (cdr (car (car (t ))));}
INLINE term cddadr(term t)      {return cdr (cdr (car (cdr (t ))));}
INLINE term cdddar(term t)      {return cdr (cdr (cdr (car (t ))));}
INLINE term cddddr(term t)      {return cdr (cdr (cdr (cdr (t ))));}

//------------------------------------------------------------------------------
// fast list creation
//------------------------------------------------------------------------------
#define LIST_1(x1)                      cons(x1, nil)
#define LIST_2(x1, x2)                  cons(x1, LIST_1(x2))
#define LIST_3(x1, x2, x3)              cons(x1, LIST_2(x2, x3))
#define LIST_4(x1, x2, x3, x4)          cons(x1, LIST_3(x2, x3, x4))
#define LIST_5(x1, x2, x3, x4, x5)      cons(x1, LIST_4(x2, x3, x4, x5))
#define LIST_6(x1, x2, x3, x4, x5, x6)  cons(x1, LIST_5(x2, x3, x4, x5, x6))

//------------------------------------------------------------------------------
// macro wrapper for lisp_list
//------------------------------------------------------------------------------
#define LIST(...)                                           \
  ({const term __args__[] = {__VA_ARGS__};                  \
    lisp_list(sizeof(__args__) / sizeof(term), __args__);})

//-------------------------------------------------------------------
// Function: (nreverse list) ==> list
// Destructive reverse of proper list.
//-------------------------------------------------------------------
term nreverse(term seq);

//-------------------------------------------------------------------
// Function: (reverse list) ==> list
// Non-destructive reverse of proper list. Fresh list is returned.
//-------------------------------------------------------------------
term reverse(term seq);

//-------------------------------------------------------------------
// internal function, returns length of proper list
// Detects simple loops in list - loops based on CDR field. Does not detects
// loops based on CAR field.
//-------------------------------------------------------------------
long list_length(term seq);

//------------------------------------------------------------------------------
// Function: (copy-list seq) ==> list
//------------------------------------------------------------------------------
term copy_list(term seq);

//==============================================================================
// UTF-32 characters.
//==============================================================================
#define UCHAR_CODE_LIMIT   1114112

INLINE uint32_t term_to_uchar(term x) {
  uint32_t c32 = term_to_uint32(x);
  if (u32_check(&c32, 1) != NULL) {
    lisp_signal(g_invalid_utf32_char, x);
  }
  return c32;
}

//==============================================================================
// UTF-32 strings
//==============================================================================
typedef struct ustring_t      ustring_t;

struct ustring_t {
  long            type;
  uint32_t *      data;
  long            size;
  long            capacity;
  long            max_capacity;
  long            hash;
  int             hash_valid;
  int             immutable;
};

#define is_ustring(x)   (get_term_type(x) == ustring_e)

//------------------------------------------------------------------------------
// USTRING_MAX_CAPACITY
//------------------------------------------------------------------------------
#define USTRING_MAX_CAPACITY   (long)(MAX_ALLOC / sizeof(uint32_t))

//------------------------------------------------------------------------------
// ustring_init
//------------------------------------------------------------------------------
INLINE void ustring_init(ustring_t * s) {
  s->type = ustring_e;
  s->immutable = 0;
  s->data = NULL;
  s->size = 0;
  s->capacity = 0;
  s->max_capacity = USTRING_MAX_CAPACITY;
  s->hash_valid = 0;
}

//------------------------------------------------------------------------------
// Marks ustring as immutable
//------------------------------------------------------------------------------
INLINE void ustring_immune(const ustring_t * v) {
  ((ustring_t *)v)->immutable = 1;
}

//------------------------------------------------------------------------------
// Calculates new capacity for array based data types
//------------------------------------------------------------------------------
INLINE long __calc_new_capacity(long size, long capacity, long max_capacity,
                                long delta) {
  if (delta > max_capacity - size) {
    lisp_signal(g_out_of_capacity, __long_to_fixnum_term(max_capacity));
  }
  long new_capacity;
  if (delta >= capacity) {
    new_capacity = capacity + delta + (delta >> 1);
  } else {
    // Use geometric grow for capacities below 32 elements and fibonachi like
    // grow for higher capacities
    if (capacity < 32) {
      new_capacity = capacity + capacity;
    } else {
      new_capacity = capacity + (capacity >> 1);
    }
  }
  if (new_capacity > max_capacity) {
    new_capacity = max_capacity;
  }
  assert(new_capacity - size >= delta);
  return new_capacity;
}

//------------------------------------------------------------------------------
// ustring_ensure_capacity
//------------------------------------------------------------------------------
INLINE void __ustring_realloc(ustring_t * v, long delta) {
  long new_capacity = __calc_new_capacity(v->size, v->capacity, v->max_capacity, delta);
  if (v->data == NULL) {
    v->data = (uint32_t *)lisp_alloc_atomic(new_capacity * sizeof(uint32_t), NULL);
  } else {
    v->data = (uint32_t *)lisp_realloc_atomic(v->data,
                                              v->capacity * sizeof(uint32_t),
                                              new_capacity * sizeof(uint32_t));
  }
  v->capacity = new_capacity;
}

INLINE void ustring_ensure_capacity(ustring_t * v, long delta) {
  if (delta < 0) {
    lisp_signal(g_invalid_arg, long_to_term(delta));
  }
  long curr_space = v->capacity - v->size;
  if (curr_space >= delta) {
    return;
  }
  __ustring_realloc(v, delta - curr_space);
}

//------------------------------------------------------------------------------
// ustring_set_max_capacity
//------------------------------------------------------------------------------
INLINE void ustring_set_max_capacity(ustring_t * v, long max_capacity) {
  if ((unsigned long)max_capacity > USTRING_MAX_CAPACITY || max_capacity < v->size) {
    lisp_signal(g_out_of_range, long_to_term(max_capacity));
  }
  v->max_capacity = max_capacity;
}

//------------------------------------------------------------------------------
// ustring_at
//------------------------------------------------------------------------------
INLINE uint32_t ustring_at(const ustring_t * v, long i) {
  if ((unsigned long)i >= (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  return v->data[i];
}

//------------------------------------------------------------------------------
// ustring_set_at
//------------------------------------------------------------------------------
INLINE void ustring_set_at(ustring_t * v, long i, uint32_t x) {
  if ((unsigned long)i >= (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  v->hash_valid = 0;
  v->data[i] = x;
}

//------------------------------------------------------------------------------
// ustring_remove
//------------------------------------------------------------------------------
INLINE void ustring_remove(ustring_t * v, long start, long count) {
  if (count == 0) {
    return;
  }
  if ((unsigned long)start > (unsigned long)v->size ||
      (unsigned long)count > (unsigned long)(v->size - start)) {
    lisp_signal(g_out_of_range, cons(long_to_term(start), long_to_term(count)));
  }
  v->hash_valid = 0;
  long end = start + count;
  if (end == v->size) {
    v->size = start;
    return;
  }
  uint32_t * src = v->data + end;
  uint32_t * dest = v->data + start;
  long n = v->size - end;
  long i;
  for (i = 0; i < n; ++i) {
    *dest++ = *src++;
  }
  v->size -= count;
}

//------------------------------------------------------------------------------
// ustring_truncate
//------------------------------------------------------------------------------
INLINE void ustring_truncate(ustring_t * v, long start) {
  if ((unsigned long)start > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(start));
  }
  v->hash_valid = 0;
  v->size = start;
}

//------------------------------------------------------------------------------
// ustring_check
//------------------------------------------------------------------------------
INLINE long ustring_check(const uint32_t * p, long len) {
  const uint32_t * res = u32_check(p, len);
  if (res == NULL) {
    return -1;
  }
  return res - p;
}

//------------------------------------------------------------------------------
// ustring_cmp
// returns negative if l < r,
// returns 0 if l = r,
// returns positive if l > r.
//------------------------------------------------------------------------------
INLINE long ustring_cmp(const ustring_t * l, const ustring_t * r) {
  long i = 0;
  long cmp_rc = 0;
  while (i < l->size && i < r->size && (cmp_rc = (long)l->data[i] - (long)r->data[i]) == 0) {
    ++i;
  }
  if (cmp_rc != 0) {
    return cmp_rc;
  }
  return l->size - r->size;
}

//------------------------------------------------------------------------------
// ustring_append_uchar
//------------------------------------------------------------------------------
INLINE void ustring_append_uchar(ustring_t * v, uint32_t e) {
  ustring_ensure_capacity(v, 1);
  v->hash_valid = 0;
  v->data[v->size] = e;
  v->size += 1;
}

//------------------------------------------------------------------------------
// ustring_append_ustring
//------------------------------------------------------------------------------
INLINE void ustring_append_ustring(ustring_t * v, const uint32_t * p, long len) {
  ustring_ensure_capacity(v, len);
  v->hash_valid = 0;
  uint32_t * dest = v->data + v->size;
  long i;
  for (i = 0; i < len; ++i) {
    *dest++ = *p++;
  }
  v->size += len;
}

//------------------------------------------------------------------------------
// ustring_append_binary
//------------------------------------------------------------------------------
INLINE void ustring_append_binary(ustring_t * v, const uint8_t * s, long len,
                                  const char * encoding) {
  ustring_ensure_capacity(v, len);
  size_t ulen = len;
  uint32_t * resbuf = u32_conv_from_encoding(encoding, iconveh_error,
                                             (const char *)s, len, NULL,
                                             v->data + v->size, &ulen);
  if (resbuf == NULL) {
    lisp_signal_system_error();
  }
  if (resbuf != v->data) {
    free(resbuf);
    SIGNAL_INTERNAL_ERROR();
  }
  v->hash_valid = 0;
  v->size += ulen;
}

//------------------------------------------------------------------------------
// ustring_append_utf8_binary
//------------------------------------------------------------------------------
INLINE void ustring_append_utf8_binary(ustring_t * v, const uint8_t * s, long len) {
  if (u8_check(s, len) != NULL) {
    lisp_signal(g_invalid_utf8_string, make_binary_from_binary(s, len));
  }
  ustring_ensure_capacity(v, len);
  size_t ulen = len;
  uint32_t * resbuf = u8_to_u32(s, len, v->data + v->size, &ulen);
  if (resbuf == NULL) {
    lisp_signal_system_error();
  }
  if (resbuf != v->data) {
    free(resbuf);
    SIGNAL_INTERNAL_ERROR();
  }
  v->hash_valid = 0;
  v->size += ulen;
}

//------------------------------------------------------------------------------
// __ustring_make_hole
//------------------------------------------------------------------------------
INLINE void __ustring_make_hole(ustring_t * v, long pos, long len) {
  if ((unsigned long)pos > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(pos));
  }
  ustring_ensure_capacity(v, len);
  v->hash_valid = 0;
  uint32_t * src = v->data + pos;
  uint32_t * dest = src + len;
  long n = v->size - pos;
  long i;
  for (i = 0; i < n; ++i) {
    *dest-- = *src--;
  }
  v->size += len;
}

//------------------------------------------------------------------------------
// ustring_insert
//------------------------------------------------------------------------------
INLINE void ustring_insert_uchar(ustring_t * v, long pos, uint32_t e) {
  __ustring_make_hole(v, pos, 1);
  v->data[pos] = e;
}

//------------------------------------------------------------------------------
// ustring_insert_ustring
//------------------------------------------------------------------------------
INLINE void ustring_insert_ustring(ustring_t * v, long pos,
                                 const uint32_t * p, long len) {
  __ustring_make_hole(v, pos, len);
  uint32_t * dest = v->data + pos;
  long i;
  for (i = 0; i < len; ++i) {
    *dest++ = *p++;
  }
}

//------------------------------------------------------------------------------
// ustring_insert_binary
//------------------------------------------------------------------------------
INLINE void ustring_insert_binary(ustring_t * v, long pos, const uint8_t * s,
                                  long len, const char * encoding) {
  ustring_t tmp;
  ustring_init(&tmp);
  ustring_append_binary(&tmp, s, len, encoding);
  ustring_insert_ustring(v, pos, tmp.data, tmp.size);
}

//------------------------------------------------------------------------------
// ustring_insert_utf8_binary
//------------------------------------------------------------------------------
INLINE void ustring_insert_utf8_binary(ustring_t * v, long pos, const uint8_t * s, long len) {
  ustring_t tmp;
  ustring_init(&tmp);
  ustring_append_utf8_binary(&tmp, s, len);
  ustring_insert_ustring(v, pos, tmp.data, tmp.size);
}

//------------------------------------------------------------------------------
// ustring_hash_code
//------------------------------------------------------------------------------
INLINE long ustring_hash_code(const ustring_t * v) {
  if (v->hash_valid) {
    return v->hash;
  }
  long hash = 0;
  long i;
  for (i = 0; i < v->size; ++i) {
    hash += v->data[i];
  }
  ((ustring_t *)v)->hash = __long_to_hash_code(hash);
  ((ustring_t *)v)->hash_valid = 1;
  return hash;
}

//-------------------------------------------------------------------
// ustring_get_c_str
//-------------------------------------------------------------------
INLINE const uint32_t * ustring_get_c_str(const ustring_t * v) {
  ustring_t * s = (ustring_t *) v;
  ustring_ensure_capacity(s, 1);
  s->data[s->size] = 0;
  return s->data;
}

//-------------------------------------------------------------------
// ustring_find_uchar
//-------------------------------------------------------------------
INLINE long ustring_find_uchar(const ustring_t * v, uint32_t c, long pos) {
  if ((unsigned long)pos > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(pos));
  }
  long i;
  for (i = pos; i < v->size; ++i) {
    if (c == v->data[i]) {
      return i;
    }
  }
  return -1;
}

//-------------------------------------------------------------------
// ustring_find_ustring
//-------------------------------------------------------------------
INLINE long ustring_find_ustring(const ustring_t * v, const uint32_t * data,
                               long len, long pos) {
  if ((unsigned long)pos > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(pos));
  }
  if (len <= 0) {
    lisp_signal(g_out_of_range, long_to_term(len));
  }
  long end = v->size - len;
  while (1) {
    pos = ustring_find_uchar(v, data[0], pos);
    if (pos < 0) {
      return -1;
    }
    if (pos > end) {
      return -1;
    }
    long i;
    for (i = 1; i < len; ++i) {
      if (v->data[pos + i] != data[i]) {
        break;
      }
    }
    if (i == len) {
      return pos;
    }
    ++pos;
  }
  SIGNAL_INTERNAL_ERROR();
}

//-------------------------------------------------------------------
// ustring_reverse
//-------------------------------------------------------------------
INLINE void ustring_reverse(ustring_t * s) {
  s->hash_valid = 0;
  long i = 0;
  long j = s->size - 1;
  for (; i < j; ++i, --j) {
    uint32_t tmp = s->data[i];
    s->data[i] = s->data[j];
    s->data[j] = tmp;
  }
}

//-------------------------------------------------------------------
// ustring_starts
//-------------------------------------------------------------------
INLINE int ustring_starts(const ustring_t * str, const ustring_t * prefix) {
  if (str->size < prefix->size) {
    return 0;
  }
  long i;
  for (i = 0; i < prefix->size; ++i) {
    if (str->data[i] != prefix->data[i]) {
      return 0;
    }
  }
  return 1;
}

//-------------------------------------------------------------------
// ustring term operations
//-------------------------------------------------------------------
INLINE term make_ustring() {
  ustring_t * s = (ustring_t *)lisp_alloc(sizeof(ustring_t), NULL);
  ustring_init(s);
  return __pointer_to_term(s);
}

INLINE term make_ustring_from_ustring(const uint32_t * p, long len) {
  ustring_t * s = (ustring_t *)lisp_alloc(sizeof(ustring_t), NULL);
  ustring_init(s);
  ustring_append_ustring(s, p, len);
  return __pointer_to_term(s);
}

INLINE const ustring_t * __get_ustring_for_read(term t) {
  assert(get_term_type(t) == ustring_e);
  return (ustring_t *)__term_to_pointer(t);
}

INLINE ustring_t * __get_ustring_for_write(term t) {
  assert(get_term_type(t) == ustring_e);
  ustring_t * p = (ustring_t *)__term_to_pointer(t);
  if (p->immutable) {
    lisp_signal(g_immutable_object, t);
  }
  p->hash_valid = 0;
  return p;
}

INLINE const ustring_t * get_ustring_for_read(term t) {
  if (!is_ustring(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __get_ustring_for_read(t);
}

INLINE ustring_t * get_ustring_for_write(term t) {
  if (!is_ustring(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __get_ustring_for_write(t);
}

//==============================================================================
// Integer endian conversion
//==============================================================================
#if __BYTE_ORDER == __BIG_ENDIAN
#   define int16_to_be(x)      (x)
#   define be_to_int16(x)      (x)
#   define int16_to_le(x)      bswap_16(x)
#   define le_to_int16(x)      bswap_16(x)

#   define uint16_to_be(x)     (x)
#   define be_to_uint16(x)     (x)
#   define uint16_to_le(x)     bswap_16(x)
#   define le_to_uint16(x)     bswap_16(x)

#   define int32_to_be(x)      (x)
#   define be_to_int32(x)      (x)
#   define int32_to_le(x)      bswap_32(x)
#   define le_to_int32(x)      bswap_32(x)

#   define uint32_to_be(x)     (x)
#   define be_to_uint32(x)     (x)
#   define uint32_to_le(x)     bswap_32(x)
#   define le_to_uint32(x)     bswap_32(x)

#   define int64_to_be(x)      (x)
#   define be_to_int64(x)      (x)
#   define int64_to_le(x)      bswap_64(x)
#   define le_to_int64(x)      bswap_64(x)

#   define uint64_to_be(x)     (x)
#   define be_to_uint64(x)     (x)
#   define uint64_to_le(x)     bswap_64(x)
#   define le_to_uint64(x)     bswap_64(x)
#elif __BYTE_ORDER == __LITTLE_ENDIAN
#   define int16_to_be(x)      bswap_16(x)
#   define be_to_int16(x)      bswap_16(x)
#   define int16_to_le(x)      (x)
#   define le_to_int16(x)      (x)

#   define uint16_to_be(x)     bswap_16(x)
#   define be_to_uint16(x)     bswap_16(x)
#   define uint16_to_le(x)     (x)
#   define le_to_uint16(x)     (x)

#   define int32_to_be(x)      bswap_32(x)
#   define be_to_int32(x)      bswap_32(x)
#   define int32_to_le(x)      (x)
#   define le_to_int32(x)      (x)

#   define uint32_to_be(x)     bswap_32(x)
#   define be_to_uint32(x)     bswap_32(x)
#   define uint32_to_le(x)     (x)
#   define le_to_uint32(x)     (x)

#   define int64_to_be(x)      bswap_64(x)
#   define be_to_int64(x)      bswap_64(x)
#   define int64_to_le(x)      (x)
#   define le_to_int64(x)      (x)

#   define uint64_to_be(x)     bswap_64(x)
#   define be_to_uint64(x)     bswap_64(x)
#   define uint64_to_le(x)     (x)
#   define le_to_uint64(x)     (x)
#else
#   error Unknow byte order.
#endif

//==============================================================================
// Binaries
//==============================================================================
typedef struct binary_t   binary_t;

struct binary_t {
  long            type;
  uint8_t *       data;
  long            size;
  long            capacity;
  long            max_capacity;
  long            hash;
  long            ihash;
  int             hash_valid;
  int             immutable;
};

#define is_binary(x)   (get_term_type(x) == binary_e)

//------------------------------------------------------------------------------
// BINARY_MAX_CAPACITY
//------------------------------------------------------------------------------
#define BINARY_MAX_CAPACITY   (long)(MAX_ALLOC / sizeof(uint8_t))

//------------------------------------------------------------------------------
// binary_init
//------------------------------------------------------------------------------
INLINE void binary_init(binary_t * b) {
  b->type = binary_e;
  b->immutable = 0;
  b->data = NULL;
  b->size = 0;
  b->capacity = 0;
  b->max_capacity = BINARY_MAX_CAPACITY;
  b->hash_valid = 0;
}

//------------------------------------------------------------------------------
// Marks binary as immutable
//------------------------------------------------------------------------------
INLINE void binary_immune(const binary_t * v) {
  ((binary_t *)v)->immutable = 1;
}

//------------------------------------------------------------------------------
// binary_ensure_capacity
//------------------------------------------------------------------------------
INLINE void __binary_realloc(binary_t * v, long delta) {
  long new_capacity = __calc_new_capacity(v->size, v->capacity, v->max_capacity, delta);
  if (v->data == NULL) {
    v->data = (uint8_t *)lisp_alloc_atomic(new_capacity, NULL);
  } else {
    v->data = (uint8_t *)lisp_realloc_atomic(v->data, v->capacity, new_capacity);
  }
  v->capacity = new_capacity;
}

INLINE void binary_ensure_capacity(binary_t * v, long delta) {
  if (delta < 0) {
    lisp_signal(g_invalid_arg, long_to_term(delta));
  }
  long curr_space = v->capacity - v->size;
  if (curr_space >= delta) {
    return;
  }
  __binary_realloc(v, delta - curr_space);
}

//------------------------------------------------------------------------------
// binary_set_max_capacity
//------------------------------------------------------------------------------
INLINE void binary_set_max_capacity(binary_t * v, long max_capacity) {
  if ((unsigned long)max_capacity > BINARY_MAX_CAPACITY ||
      max_capacity < v->size) {
    lisp_signal(g_out_of_range, long_to_term(max_capacity));
  }
  v->max_capacity = max_capacity;
}

//------------------------------------------------------------------------------
// binary_at
//------------------------------------------------------------------------------
INLINE uint8_t binary_at(const binary_t * v, long i) {
  if ((unsigned long)i >= (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  return v->data[i];
}

//------------------------------------------------------------------------------
// binary_get_int8
//------------------------------------------------------------------------------
INLINE int8_t binary_get_int8(const binary_t * v, long i) {
  return binary_at(v, i);
}

//------------------------------------------------------------------------------
// binary_get_uint8
//------------------------------------------------------------------------------
INLINE uint8_t binary_get_uint8(const binary_t * v, long i) {
  return binary_get_int8(v, i);
}

//------------------------------------------------------------------------------
// binary_get_int16
//------------------------------------------------------------------------------
INLINE int16_t binary_get_int16(const binary_t * v, long i) {
  if ((unsigned long)v->size < sizeof(int16_t) ||
      (unsigned long)i > (unsigned long)(v->size - sizeof(int16_t))) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  return *(int16_t *)(v->data + i);
}

//------------------------------------------------------------------------------
// binary_get_int16_le
//------------------------------------------------------------------------------
INLINE int16_t binary_get_int16_le(const binary_t * v, long i) {
  return le_to_int16(binary_get_int16(v, i));
}

//------------------------------------------------------------------------------
// binary_get_int16_be
//------------------------------------------------------------------------------
INLINE int16_t binary_get_int16_be(const binary_t * v, long i) {
  return be_to_int16(binary_get_int16(v, i));
}

//------------------------------------------------------------------------------
// binary_get_uint16
//------------------------------------------------------------------------------
INLINE uint16_t binary_get_uint16(const binary_t * v, long i) {
  return binary_get_int16(v, i);
}

//------------------------------------------------------------------------------
// binary_get_uint16_le
//------------------------------------------------------------------------------
INLINE uint16_t binary_get_uint16_le(const binary_t * v, long i) {
  return le_to_uint16(binary_get_uint16(v, i));
}

//------------------------------------------------------------------------------
// binary_get_uint16_be
//------------------------------------------------------------------------------
INLINE uint16_t binary_get_uint16_be(const binary_t * v, long i) {
  return be_to_uint16(binary_get_uint16(v, i));
}

//------------------------------------------------------------------------------
// binary_get_int32
//------------------------------------------------------------------------------
INLINE int32_t binary_get_int32(const binary_t * v, long i) {
  if ((unsigned long)v->size < sizeof(int32_t) ||
      (unsigned long)i > (unsigned long)(v->size - sizeof(int32_t))) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  return *(int32_t *)(v->data + i);
}

//------------------------------------------------------------------------------
// binary_get_int32_le
//------------------------------------------------------------------------------
INLINE int32_t binary_get_int32_le(const binary_t * v, long i) {
  return le_to_int32(binary_get_int32(v, i));
}

//------------------------------------------------------------------------------
// binary_get_int32_be
//------------------------------------------------------------------------------
INLINE int32_t binary_get_int32_be(const binary_t * v, long i) {
  return be_to_int32(binary_get_int32(v, i));
}

//------------------------------------------------------------------------------
// binary_get_uint32
//------------------------------------------------------------------------------
INLINE uint32_t binary_get_uint32(const binary_t * v, long i) {
  return binary_get_int32(v, i);
}

//------------------------------------------------------------------------------
// binary_get_uint32_le
//------------------------------------------------------------------------------
INLINE uint32_t binary_get_uint32_le(const binary_t * v, long i) {
  return le_to_uint32(binary_get_uint32(v, i));
}

//------------------------------------------------------------------------------
// binary_get_uint32_be
//------------------------------------------------------------------------------
INLINE uint32_t binary_get_uint32_be(const binary_t * v, long i) {
  return be_to_uint32(binary_get_uint32(v, i));
}

//------------------------------------------------------------------------------
// binary_get_int64
//------------------------------------------------------------------------------
INLINE int64_t binary_get_int64(const binary_t * v, long i) {
  if ((unsigned long)v->size < sizeof(int64_t) ||
      (unsigned long)i > (unsigned long)(v->size - sizeof(int64_t))) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  return *(int64_t *)(v->data + i);
}

//------------------------------------------------------------------------------
// binary_get_int64_le
//------------------------------------------------------------------------------
INLINE int64_t binary_get_int64_le(const binary_t * v, long i) {
  return le_to_int64(binary_get_int64(v, i));
}

//------------------------------------------------------------------------------
// binary_get_int64_be
//------------------------------------------------------------------------------
INLINE int64_t binary_get_int64_be(const binary_t * v, long i) {
  return be_to_int64(binary_get_int64(v, i));
}

//------------------------------------------------------------------------------
// binary_get_uint64
//------------------------------------------------------------------------------
INLINE uint64_t binary_get_uint64(const binary_t * v, long i) {
  return binary_get_int64(v, i);
}

//------------------------------------------------------------------------------
// binary_get_uint64_le
//------------------------------------------------------------------------------
INLINE uint64_t binary_get_uint64_le(const binary_t * v, long i) {
  return le_to_uint64(binary_get_uint64(v, i));
}

//------------------------------------------------------------------------------
// binary_get_uint64_be
//------------------------------------------------------------------------------
INLINE uint64_t binary_get_uint64_be(const binary_t * v, long i) {
  return be_to_uint64(binary_get_uint64(v, i));
}

//------------------------------------------------------------------------------
// binary_set_at
//------------------------------------------------------------------------------
INLINE void binary_set_at(binary_t * v, long i, uint8_t x) {
  if ((unsigned long)i >= (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  v->hash_valid = 0;
  v->data[i] = x;
}

//------------------------------------------------------------------------------
// binary_set_int8
//------------------------------------------------------------------------------
INLINE void binary_set_int8(binary_t * v, long i, int8_t x) {
  binary_set_at(v, i, x);
}

//------------------------------------------------------------------------------
// binary_set_uint8
//------------------------------------------------------------------------------
INLINE void binary_set_uint8(binary_t * v, long i, uint8_t x) {
  binary_set_int8(v, i, x);
}

//------------------------------------------------------------------------------
// binary_set_int16
//------------------------------------------------------------------------------
INLINE void binary_set_int16(binary_t * v, long i, int16_t x) {
  if ((unsigned long)v->size < sizeof(int16_t) ||
      (unsigned long)i > (unsigned long)(v->size - sizeof(int16_t))) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  v->hash_valid = 0;
  *(int16_t *)(v->data + i) = x;
}

//------------------------------------------------------------------------------
// binary_set_int16_le
//------------------------------------------------------------------------------
INLINE void binary_set_int16_le(binary_t * v, long i, int16_t x) {
  binary_set_int16(v, i, int16_to_le(x));
}

//------------------------------------------------------------------------------
// binary_set_int16_be
//------------------------------------------------------------------------------
INLINE void binary_set_int16_be(binary_t * v, long i, int16_t x) {
  binary_set_int16(v, i, int16_to_be(x));
}

//------------------------------------------------------------------------------
// binary_set_uint16
//------------------------------------------------------------------------------
INLINE void binary_set_uint16(binary_t * v, long i, uint16_t x) {
  binary_set_int16(v, i, x);
}

//------------------------------------------------------------------------------
// binary_set_uint16_le
//------------------------------------------------------------------------------
INLINE void binary_set_uint16_le(binary_t * v, long i, uint16_t x) {
  binary_set_uint16(v, i, uint16_to_le(x));
}

//------------------------------------------------------------------------------
// binary_set_uint16_be
//------------------------------------------------------------------------------
INLINE void binary_set_uint16_be(binary_t * v, long i, uint16_t x) {
  binary_set_uint16(v, i, uint16_to_be(x));
}

//------------------------------------------------------------------------------
// binary_set_int32
//------------------------------------------------------------------------------
INLINE void binary_set_int32(binary_t * v, long i, int32_t x) {
  if ((unsigned long)v->size < sizeof(int32_t) ||
      (unsigned long)i > (unsigned long)(v->size - sizeof(int32_t))) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  v->hash_valid = 0;
  *(int32_t *)(v->data + i) = x;
}

//------------------------------------------------------------------------------
// binary_set_int32_le
//------------------------------------------------------------------------------
INLINE void binary_set_int32_le(binary_t * v, long i, int32_t x) {
  binary_set_int32(v, i, int32_to_le(x));
}

//------------------------------------------------------------------------------
// binary_set_int32_be
//------------------------------------------------------------------------------
INLINE void binary_set_int32_be(binary_t * v, long i, int32_t x) {
  binary_set_int32(v, i, int32_to_be(x));
}

//------------------------------------------------------------------------------
// binary_set_uint32
//------------------------------------------------------------------------------
INLINE void binary_set_uint32(binary_t * v, long i, uint32_t x) {
  binary_set_int32(v, i, x);
}

//------------------------------------------------------------------------------
// binary_set_uint32_le
//------------------------------------------------------------------------------
INLINE void binary_set_uint32_le(binary_t * v, long i, uint32_t x) {
  binary_set_uint32(v, i, uint32_to_le(x));
}

//------------------------------------------------------------------------------
// binary_set_uint32_be
//------------------------------------------------------------------------------
INLINE void binary_set_uint32_be(binary_t * v, long i, uint32_t x) {
  binary_set_uint32(v, i, uint32_to_be(x));
}

//------------------------------------------------------------------------------
// binary_set_int64
//------------------------------------------------------------------------------
INLINE void binary_set_int64(binary_t * v, long i, int64_t x) {
  if ((unsigned long)v->size < sizeof(int64_t) ||
      (unsigned long)i > (unsigned long)(v->size - sizeof(int64_t))) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  v->hash_valid = 0;
  *(int64_t *)(v->data + i) = x;
}

//------------------------------------------------------------------------------
// binary_set_int64_le
//------------------------------------------------------------------------------
INLINE void binary_set_int64_le(binary_t * v, long i, int64_t x) {
  binary_set_int64(v, i, int64_to_le(x));
}

//------------------------------------------------------------------------------
// binary_set_int64_be
//------------------------------------------------------------------------------
INLINE void binary_set_int64_be(binary_t * v, long i, int64_t x) {
  binary_set_int64(v, i, int64_to_be(x));
}

//------------------------------------------------------------------------------
// binary_set_uint64
//------------------------------------------------------------------------------
INLINE void binary_set_uint64(binary_t * v, long i, uint64_t x) {
  binary_set_int64(v, i, x);
}

//------------------------------------------------------------------------------
// binary_set_uint64_le
//------------------------------------------------------------------------------
INLINE void binary_set_uint64_le(binary_t * v, long i, uint64_t x) {
  binary_set_uint64(v, i, uint64_to_le(x));
}

//------------------------------------------------------------------------------
// binary_set_uint64_be
//------------------------------------------------------------------------------
INLINE void binary_set_uint64_be(binary_t * v, long i, uint64_t x) {
  binary_set_uint64(v, i, uint64_to_be(x));
}

//------------------------------------------------------------------------------
// binary_remove
//------------------------------------------------------------------------------
INLINE void binary_remove(binary_t * v, long start, long count) {
  if (count == 0) {
    return;
  }
  if ((unsigned long)start > (unsigned long)v->size ||
      (unsigned long)count > (unsigned long)(v->size - start)) {
    lisp_signal(g_out_of_range, cons(long_to_term(start), long_to_term(count)));
  }
  v->hash_valid = 0;
  long end = start + count;
  if (end == v->size) {
    v->size = start;
    return;
  }
  memmove(v->data + start, v->data + end, v->size - end);
  v->size -= count;
}

//------------------------------------------------------------------------------
// binary_truncate
//------------------------------------------------------------------------------
INLINE void binary_truncate(binary_t * v, long start) {
  if ((unsigned long)start > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(start));
  }
  v->hash_valid = 0;
  v->size = start;
}

//------------------------------------------------------------------------------
// returns negative if l < r,
// returns 0 if l = r,
// returns positive if l > r.
//------------------------------------------------------------------------------
INLINE long binary_cmp(const binary_t * l, const binary_t * r) {
  long min_len;
  if (l->size < r->size) {
    min_len = l->size;
  } else {
    min_len = r->size;
  }
  long cmp_rc = memcmp(l->data, r->data, min_len);
  if (cmp_rc != 0) {
    return cmp_rc;
  }
  return l->size - r->size;
}

//------------------------------------------------------------------------------
// Compares two binaries ignoring case
// returns negative if l < r,
// returns 0 if l = r,
// returns positive if l > r.
//------------------------------------------------------------------------------
INLINE long binary_icmp(const binary_t * l, const binary_t * r) {
  long min_len;
  if (l->size < r->size) {
    min_len = l->size;
  } else {
    min_len = r->size;
  }
  long cmp_rc = strncasecmp((char *)l->data, (char *)r->data, min_len);
  if (cmp_rc != 0) {
    return cmp_rc;
  }
  return l->size - r->size;
}

//------------------------------------------------------------------------------
// binary_append_int8
//------------------------------------------------------------------------------
INLINE void binary_append_int8(binary_t * v, int8_t e) {
  binary_ensure_capacity(v, 1);
  v->hash_valid = 0;
  v->data[v->size] = e;
  v->size += 1;
}

//-------------------------------------------------------------------
// binary_append_uint8
//-------------------------------------------------------------------
INLINE void binary_append_uint8(binary_t * v, uint8_t e) {
  binary_append_int8(v, e);
}

//------------------------------------------------------------------------------
// binary_append_int16
//------------------------------------------------------------------------------
INLINE void binary_append_int16(binary_t * v, int16_t e) {
  binary_ensure_capacity(v, sizeof(e));
  v->hash_valid = 0;
  *(int16_t *)(v->data + v->size) = e;
  v->size += sizeof(e);
}

//------------------------------------------------------------------------------
// binary_append_int16_le
//------------------------------------------------------------------------------
INLINE void binary_append_int16_le(binary_t * v, int16_t e) {
  binary_append_int16(v, int16_to_le(e));
}

//------------------------------------------------------------------------------
// binary_append_int16_be
//------------------------------------------------------------------------------
INLINE void binary_append_int16_be(binary_t * v, int16_t e) {
  binary_append_int16(v, int16_to_be(e));
}

//------------------------------------------------------------------------------
// binary_append_uint16
//------------------------------------------------------------------------------
INLINE void binary_append_uint16(binary_t * v, uint16_t e) {
  binary_append_int16(v, e);
}

//------------------------------------------------------------------------------
// binary_append_uint16_le
//------------------------------------------------------------------------------
INLINE void binary_append_uint16_le(binary_t * v, uint16_t e) {
  binary_append_uint16(v, uint16_to_le(e));
}

//------------------------------------------------------------------------------
// binary_append_uint16_be
//------------------------------------------------------------------------------
INLINE void binary_append_uint16_be(binary_t * v, uint16_t e) {
  binary_append_uint16(v, uint16_to_be(e));
}

//------------------------------------------------------------------------------
// binary_append_int32
//------------------------------------------------------------------------------
INLINE void binary_append_int32(binary_t * v, int32_t e) {
  binary_ensure_capacity(v, sizeof(e));
  v->hash_valid = 0;
  *(int32_t *)(v->data + v->size) = e;
  v->size += sizeof(e);
}

//------------------------------------------------------------------------------
// binary_append_int32_le
//------------------------------------------------------------------------------
INLINE void binary_append_int32_le(binary_t * v, int32_t e) {
  binary_append_int32(v, int32_to_le(e));
}

//------------------------------------------------------------------------------
// binary_append_int32_be
//------------------------------------------------------------------------------
INLINE void binary_append_int32_be(binary_t * v, int32_t e) {
  binary_append_int32(v, int32_to_be(e));
}

//------------------------------------------------------------------------------
// binary_append_uint32
//------------------------------------------------------------------------------
INLINE void binary_append_uint32(binary_t * v, uint32_t e) {
  binary_append_int32(v, e);
}

//------------------------------------------------------------------------------
// binary_append_uint32_le
//------------------------------------------------------------------------------
INLINE void binary_append_uint32_le(binary_t * v, uint32_t e) {
  binary_append_uint32(v, uint32_to_le(e));
}

//------------------------------------------------------------------------------
// binary_append_uint32_be
//------------------------------------------------------------------------------
INLINE void binary_append_uint32_be(binary_t * v, uint32_t e) {
  binary_append_uint32(v, uint32_to_be(e));
}

//------------------------------------------------------------------------------
// binary_append_int64
//------------------------------------------------------------------------------
INLINE void binary_append_int64(binary_t * v, int64_t e) {
  binary_ensure_capacity(v, sizeof(e));
  v->hash_valid = 0;
  *(int64_t *)(v->data + v->size) = e;
  v->size += sizeof(e);
}

//------------------------------------------------------------------------------
// binary_append_int64_le
//------------------------------------------------------------------------------
INLINE void binary_append_int64_le(binary_t * v, int64_t e) {
  binary_append_int64(v, int64_to_le(e));
}

//------------------------------------------------------------------------------
// binary_append_int64_be
//------------------------------------------------------------------------------
INLINE void binary_append_int64_be(binary_t * v, int64_t e) {
  binary_append_int64(v, int64_to_be(e));
}

//------------------------------------------------------------------------------
// binary_append_uint64
//------------------------------------------------------------------------------
INLINE void binary_append_uint64(binary_t * v, uint64_t e) {
  binary_append_int64(v, e);
}

//------------------------------------------------------------------------------
// binary_append_uint64_le
//------------------------------------------------------------------------------
INLINE void binary_append_uint64_le(binary_t * v, uint64_t e) {
  binary_append_uint64(v, uint64_to_le(e));
}

//------------------------------------------------------------------------------
// binary_append_uint64_be
//------------------------------------------------------------------------------
INLINE void binary_append_uint64_be(binary_t * v, uint64_t e) {
  binary_append_uint64(v, uint64_to_be(e));
}

//------------------------------------------------------------------------------
// binary_append_binary
//------------------------------------------------------------------------------
INLINE void binary_append_binary(binary_t * v, const uint8_t * p, long len) {
  binary_ensure_capacity(v, len);
  v->hash_valid = 0;
  memcpy(v->data + v->size, p, len);
  v->size += len;
}

//------------------------------------------------------------------------------
// binary_append_sz
//------------------------------------------------------------------------------
INLINE void binary_append_sz(binary_t * v, const char * p) {
  binary_append_binary(v, (uint8_t *)p, strlen(p));
}

//------------------------------------------------------------------------------
// binary_append_uchar
//------------------------------------------------------------------------------
INLINE void binary_append_uchar(binary_t * v, uint32_t e, const char * encoding,
                                enum iconv_ilseq_handler ilseq_handler) {
  binary_ensure_capacity(v, MB_LEN_MAX);
  size_t enc_len = MB_LEN_MAX;
  void * res = u32_conv_to_encoding(encoding, ilseq_handler, &e, 1, NULL,
                                    (char *)(v->data + v->size), &enc_len);
  if (res == NULL) {
    lisp_signal_system_error();
  }
  if (res != v->data) {
    free(res);
    SIGNAL_INTERNAL_ERROR();
  }
  v->hash_valid = 0;
  v->size += enc_len;
}

//-------------------------------------------------------------------
// binary_append_uchar_utf8
//-------------------------------------------------------------------
INLINE void binary_append_uchar_utf8(binary_t * b, uint32_t c) {
  // max length of UTF-8 character is 4 bytes
  binary_ensure_capacity(b, 4);
  size_t ulen = 4;
  void * res = u32_to_u8(&c, 1, b->data + b->size, &ulen);
  if (res == NULL) {
    lisp_signal_system_error();
  }
  if (res != b->data) {
    free(res);
    SIGNAL_INTERNAL_ERROR();
  }
  b->hash_valid = 0;
  b->size += ulen;
}

//------------------------------------------------------------------------------
// binary_append_ustring
//------------------------------------------------------------------------------
INLINE void binary_append_ustring(binary_t * v, const uint32_t * s, long len,
                                  const char * encoding, enum iconv_ilseq_handler ilseq_handler) {
  binary_ensure_capacity(v, len * MB_LEN_MAX);
  size_t enc_len = len * MB_LEN_MAX;
  void * res = u32_conv_to_encoding(encoding, ilseq_handler, s, len, NULL,
                                    (char *)(v->data + v->size), &enc_len);
  if (res == NULL) {
    lisp_signal_system_error();
  }
  if (res != v->data) {
    free(res);
    SIGNAL_INTERNAL_ERROR();
  }
  v->hash_valid = 0;
  v->size += enc_len;
}

//------------------------------------------------------------------------------
// binary_append_ustring_utf8
//------------------------------------------------------------------------------
INLINE void binary_append_ustring_utf8(binary_t * b, const uint32_t * s, long len) {
  // max length of UTF-8 character is 4 bytes
  size_t ulen = len * 4;
  binary_ensure_capacity(b, ulen);
  void * res = u32_to_u8(s, len, b->data + b->size, &ulen);
  if (res == NULL) {
    lisp_signal_system_error();
  }
  if (res != b->data) {
    free(res);
    SIGNAL_INTERNAL_ERROR();
  }
  b->hash_valid = 0;
  b->size += ulen;
}

//------------------------------------------------------------------------------
// __binary_make_hole
//------------------------------------------------------------------------------
INLINE void __binary_make_hole(binary_t * v, long pos, long len) {
  if ((unsigned long)pos > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(pos));
  }
  binary_ensure_capacity(v, len);
  v->hash_valid = 0;
  memmove(v->data + pos + len, v->data + pos, v->size - pos);
  v->size += len;
}

//------------------------------------------------------------------------------
// binary_insert_uint8
//------------------------------------------------------------------------------
INLINE void binary_insert_uint8(binary_t * v, long pos, uint8_t e) {
  __binary_make_hole(v, pos, 1);
  v->data[pos] = e;
}

//------------------------------------------------------------------------------
// binary_insert_binary
//------------------------------------------------------------------------------
INLINE void binary_insert_binary(binary_t * v, long pos,
                                 const uint8_t * p, long len) {
  __binary_make_hole(v, pos, len);
  memcpy(v->data + pos, p, len);
}

//------------------------------------------------------------------------------
// binary_insert_uchar
//------------------------------------------------------------------------------
INLINE void binary_insert_uchar(binary_t * v, long pos, uint32_t e,
                                const char * encoding, enum iconv_ilseq_handler ilseq_handler) {
  char buf[MB_LEN_MAX + 16];
  size_t enc_len = sizeof(buf);
  void * res = u32_conv_to_encoding(encoding, ilseq_handler, &e, 1, NULL, buf, &enc_len);
  if (res == NULL) {
    lisp_signal_system_error();
  }
  if (res != v->data) {
    free(res);
    SIGNAL_INTERNAL_ERROR();
  }
  binary_insert_binary(v, pos, (uint8_t *)buf, enc_len);
}

//------------------------------------------------------------------------------
// binary_insert_uchar_utf8
//------------------------------------------------------------------------------
INLINE void binary_insert_uchar_utf8(binary_t * v, long pos, uint32_t e) {
  uint8_t buf[4];
  size_t ulen = 4;
  void * res = u32_to_u8(&e, 1, buf, &ulen);
  if (res == NULL) {
    lisp_signal_system_error();
  }
  if (res != buf) {
    free(res);
    SIGNAL_INTERNAL_ERROR();
  }
  binary_insert_binary(v, pos, buf, ulen);
}

//------------------------------------------------------------------------------
// binary_insert_ustring
//------------------------------------------------------------------------------
INLINE void binary_insert_ustring(binary_t * v, long pos, const uint32_t * s, long len,
                                  const char * encoding, enum iconv_ilseq_handler ilseq_handler) {
  binary_t tmp;
  binary_init(&tmp);
  binary_append_ustring(&tmp, s, len, encoding, ilseq_handler);
  binary_insert_binary(v, pos, tmp.data, tmp.size);
}

//------------------------------------------------------------------------------
// binary_insert_ustring_utf8
//------------------------------------------------------------------------------
INLINE void binary_insert_ustring_utf8(binary_t * v, long pos, const uint32_t * s, long len) {
  binary_t tmp;
  binary_init(&tmp);
  binary_append_ustring_utf8(&tmp, s, len);
  binary_insert_binary(v, pos, tmp.data, tmp.size);
}

//------------------------------------------------------------------------------
// binary_hash_code
//------------------------------------------------------------------------------
INLINE long binary_hash_code(const binary_t * v) {
  if (v->hash_valid) {
    return v->hash;
  }
  long hash = 0;
  long ihash = 0;
  long i;
  for (i = 0; i < v->size; ++i) {
    hash = hash * 131 + v->data[i];
    ihash = ihash * 131 + toupper(v->data[i]);
  }
  ((binary_t *)v)->hash = __long_to_hash_code(hash);
  ((binary_t *)v)->ihash = __long_to_hash_code(ihash);
  ((binary_t *)v)->hash_valid = 1;
  return v->hash;
}

//------------------------------------------------------------------------------
// binary_hash_code
//------------------------------------------------------------------------------
INLINE long binary_ihash_code(const binary_t * v) {
  if (v->hash_valid) {
    return v->ihash;
  }
  binary_hash_code(v);
  return v->ihash;
}

//-------------------------------------------------------------------
// binary_find_char
//-------------------------------------------------------------------
INLINE long binary_find_char(const binary_t * v, uint8_t c, long pos) {
  if ((unsigned long)pos > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(pos));
  }
  long i;
  for (i = pos; i < v->size; ++i) {
    if (c == v->data[i]) {
      return i;
    }
  }
  return -1;
}

//-------------------------------------------------------------------
// binary_find_binary
//-------------------------------------------------------------------
INLINE long binary_find_binary(const binary_t * v, const uint8_t * data,
                               long len, long pos) {
  if ((unsigned long)pos > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(pos));
  }
  if (len <= 0) {
    lisp_signal(g_out_of_range, long_to_term(len));
  }
  long end = v->size - len;
  while (1) {
    pos = binary_find_char(v, data[0], pos);
    if (pos < 0) {
      return -1;
    }
    if (pos > end) {
      return -1;
    }
    long i;
    for (i = 1; i < len; ++i) {
      if (v->data[pos + i] != data[i]) {
        break;
      }
    }
    if (i == len) {
      return pos;
    }
    ++pos;
  }
  SIGNAL_INTERNAL_ERROR();
}

//-------------------------------------------------------------------
// binary_reverse
//-------------------------------------------------------------------
INLINE void binary_reverse(binary_t * s) {
  s->hash_valid = 0;
  long i = 0;
  long j = s->size - 1;
  for (; i < j; ++i, --j) {
    uint8_t tmp = s->data[i];
    s->data[i] = s->data[j];
    s->data[j] = tmp;
  }
}

//-------------------------------------------------------------------
// binary_starts
//-------------------------------------------------------------------
INLINE int binary_starts(const binary_t * str, const binary_t * prefix) {
  if (str->size < prefix->size) {
    return 0;
  }
  long i;
  for (i = 0; i < prefix->size; ++i) {
    if (str->data[i] != prefix->data[i]) {
      return 0;
    }
  }
  return 1;
}

//-------------------------------------------------------------------
// binary_istarts
//-------------------------------------------------------------------
INLINE int binary_istarts(const binary_t * str, const binary_t * prefix) {
  if (str->size < prefix->size) {
    return 0;
  }
  long i;
  for (i = 0; i < prefix->size; ++i) {
    if (toupper(str->data[i]) != toupper(prefix->data[i])) {
      return 0;
    }
  }
  return 1;
}

//-------------------------------------------------------------------
// binary_to_upper
//-------------------------------------------------------------------
INLINE void binary_to_upper(binary_t * v) {
  v->hash_valid = 0;
  long i;
  for (i = 0; i < v->size; ++i) {
    v->data[i] = toupper(v->data[i]);
  }
}

//-------------------------------------------------------------------
// binary_to_lower
//-------------------------------------------------------------------
INLINE void binary_to_lower(binary_t * v) {
  v->hash_valid = 0;
  long i;
  for (i = 0; i < v->size; ++i) {
    v->data[i] = tolower(v->data[i]);
  }
}

//-------------------------------------------------------------------
// binary_append_object
//-------------------------------------------------------------------
void binary_append_object(binary_t * buf, term t);

//-------------------------------------------------------------------
// binary_decode_object
//-------------------------------------------------------------------
term binary_decode_object(const binary_t * buf, long offset);

//------------------------------------------------------------------------------
// binary specific methods
//------------------------------------------------------------------------------
INLINE const char * binary_get_c_str(const binary_t * b) {
  binary_t * v = (binary_t *)b;
  binary_ensure_capacity(v, 1);
  v->data[v->size] = 0;
  return (char *)v->data;
}

//------------------------------------------------------------------------------
// binary term operations
//------------------------------------------------------------------------------
INLINE term make_binary() {
  binary_t * v = (binary_t *)lisp_alloc(sizeof(binary_t), NULL);
  binary_init(v);
  return __pointer_to_term(v);
}

INLINE term make_binary_from_binary(const uint8_t * p, long len) {
  binary_t * v = (binary_t *)lisp_alloc(sizeof(binary_t), NULL);
  binary_init(v);
  binary_append_binary(v, p, len);
  return __pointer_to_term(v);
}

INLINE term make_binary_from_sz(const char * p) {
  return make_binary_from_binary((uint8_t *)p, strlen(p));
}

INLINE const binary_t * __get_binary_for_read(term t) {
  assert(get_term_type(t) == binary_e);
  return (binary_t *)__term_to_pointer(t);
}

INLINE binary_t * __get_binary_for_write(term t) {
  assert(get_term_type(t) == binary_e);
  binary_t * p = (binary_t *)__term_to_pointer(t);
  if (p->immutable) {
    lisp_signal(g_immutable_object, t);
  }
  p->hash_valid = 0;
  return p;
}

INLINE const binary_t * get_binary_for_read(term t) {
  if (!is_binary(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __get_binary_for_read(t);
}

INLINE binary_t * get_binary_for_write(term t) {
  if (!is_binary(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __get_binary_for_write(t);
}

//==============================================================================
// Vectors
//==============================================================================
typedef struct vector_t   vector_t;

struct vector_t {
  long            type;
  term *          data;
  long            size;
  long            capacity;
  long            max_capacity;
  long            hash;
  int             hash_valid;
  int             immutable;
};

#define is_vector(x)   (get_term_type(x) == vector_e)

//------------------------------------------------------------------------------
// VECTOR_MAX_CAPACITY
//------------------------------------------------------------------------------
#define VECTOR_MAX_CAPACITY   (long)(MAX_ALLOC / sizeof(term))

//------------------------------------------------------------------------------
// vector_init
//------------------------------------------------------------------------------
INLINE void vector_init(vector_t * s) {
  s->type = vector_e;
  s->immutable = 0;
  s->data = NULL;
  s->size = 0;
  s->capacity = 0;
  s->max_capacity = VECTOR_MAX_CAPACITY;
  s->hash_valid = 0;
}

//------------------------------------------------------------------------------
// Marks vector as immutable
//------------------------------------------------------------------------------
INLINE void vector_immune(const vector_t * v) {
  ((vector_t *)v)->immutable = 1;
}

//------------------------------------------------------------------------------
// vector_ensure_capacity
//------------------------------------------------------------------------------
INLINE void __vector_realloc(vector_t * v, long delta) {
  long new_capacity = __calc_new_capacity(v->size, v->capacity, v->max_capacity, delta);
  if (v->data == NULL) {
    v->data = (term *)lisp_alloc(new_capacity * sizeof(term), NULL);
  } else {
    v->data = (term *)lisp_realloc(v->data,
                                   v->capacity * sizeof(term),
                                   new_capacity * sizeof(term));
  }
  v->capacity = new_capacity;
}

INLINE void vector_ensure_capacity(vector_t * v, long delta) {
  if (delta < 0) {
    lisp_signal(g_invalid_arg, long_to_term(delta));
  }
  long curr_space = v->capacity - v->size;
  if (curr_space >= delta) {
    return;
  }
  __vector_realloc(v, delta - curr_space);
}

//------------------------------------------------------------------------------
// vector_set_max_capacity
//------------------------------------------------------------------------------
INLINE void vector_set_max_capacity(vector_t * v, long max_capacity) {
  if ((unsigned long)max_capacity > VECTOR_MAX_CAPACITY || max_capacity < v->size) {
    lisp_signal(g_out_of_range, long_to_term(max_capacity));
  }
  v->max_capacity = max_capacity;
}

//------------------------------------------------------------------------------
// vector_at
//------------------------------------------------------------------------------
INLINE term vector_at(const vector_t * v, long i) {
  if ((unsigned long)i >= (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  return v->data[i];
}

//------------------------------------------------------------------------------
// vector_set_at
//------------------------------------------------------------------------------
INLINE void vector_set_at(vector_t * v, long i, term x) {
  if ((unsigned long)i >= (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(i));
  }
  v->hash_valid = 0;
  v->data[i] = x;
}

//------------------------------------------------------------------------------
// vector_remove
//------------------------------------------------------------------------------
INLINE void vector_remove(vector_t * v, long start, long count) {
  if (count == 0) {
    return;
  }
  if ((unsigned long)start > (unsigned long)v->size ||
      (unsigned long)count > (unsigned long)(v->size - start)) {
    lisp_signal(g_out_of_range, cons(long_to_term(start), long_to_term(count)));
  }
  v->hash_valid = 0;
  long end = start + count;
  if (end == v->size) {
    v->size = start;
    long i;
    for (i = start; i < end; ++i) {
      v->data[i] = nil;
    }
    return;
  }
  term * src = v->data + end;
  term * dest = v->data + start;
  long n = v->size - end;
  long i;
  for (i = 0; i < n; ++i) {
    *dest++ = *src++;
  }
  v->size -= count;
  end = v->size + count;
  for (i = v->size; i < end; ++i) {
    v->data[i] = nil;
  }
}

//------------------------------------------------------------------------------
// vector_truncate
//------------------------------------------------------------------------------
INLINE void vector_truncate(vector_t * v, long start) {
  if (start == v->size) {
    return;
  }
  if ((unsigned long)start > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(start));
  }
  v->hash_valid = 0;
  long end = v->size;
  v->size = start;
  long i;
  for (i = start; i < end; ++i) {
    v->data[i] = nil;
  }
}

//------------------------------------------------------------------------------
// vector_clear
//------------------------------------------------------------------------------
INLINE void vector_clear(vector_t * v) {
  vector_truncate(v, 0);
}

//------------------------------------------------------------------------------
// vector_assign
//------------------------------------------------------------------------------
INLINE void vector_assign(vector_t * v, long n, const term * args) {
  long i;
  if ((unsigned long)v->size > (unsigned long)n) {
    for (i = v->size; i < n; ++i) {
      v->data[i] = nil;
    }
  }
  vector_ensure_capacity(v, n);
  for (i = 0; i < n; ++i) {
    v->data[i] = args[i];
  }
  v->hash_valid = 0;
  v->size = n;
}

//------------------------------------------------------------------------------
// vector_cmp
// returns negative if l < r,
// returns 0 if l = r,
// returns positive if l > r.
//------------------------------------------------------------------------------
INLINE long vector_cmp(const vector_t * l, const vector_t * r) {
  long i = 0;
  long cmp_rc = 0;
  while (i < l->size && i < r->size && (cmp_rc = term_cmp(l->data[i], r->data[i])) == 0) {
    ++i;
  }
  if (cmp_rc != 0) {
    return cmp_rc;
  }
  return l->size - r->size;
}

//------------------------------------------------------------------------------
// vector_append_term
//------------------------------------------------------------------------------
INLINE void vector_append_term(vector_t * v, term e) {
  vector_ensure_capacity(v, 1);
  v->hash_valid = 0;
  v->data[v->size] = e;
  v->size += 1;
}

//------------------------------------------------------------------------------
// vector_append_vector
//------------------------------------------------------------------------------
INLINE void vector_append_vector(vector_t * v, const term * p, long len) {
  vector_ensure_capacity(v, len);
  v->hash_valid = 0;
  term * dest = v->data + v->size;
  long i;
  for (i = 0; i < len; ++i) {
    *dest++ = *p++;
  }
  v->size += len;
}

//------------------------------------------------------------------------------
// __vector_make_hole
//------------------------------------------------------------------------------
INLINE void __vector_make_hole(vector_t * v, long pos, long len) {
  if ((unsigned long)pos > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(pos));
  }
  vector_ensure_capacity(v, len);
  v->hash_valid = 0;
  term * src = v->data + pos;
  term * dest = src + len;
  long n = v->size - pos;
  long i;
  for (i = 0; i < n; ++i) {
    *dest-- = *src--;
  }
  v->size += len;
}

//------------------------------------------------------------------------------
// vector_insert
//------------------------------------------------------------------------------
INLINE void vector_insert_term(vector_t * v, long pos, term e) {
  __vector_make_hole(v, pos, 1);
  v->data[pos] = e;
}

//------------------------------------------------------------------------------
// vector_insert_vector
//------------------------------------------------------------------------------
INLINE void vector_insert_vector(vector_t * v, long pos,
                                 const term * p, long len) {
  __vector_make_hole(v, pos, len);
  term * dest = v->data + pos;
  long i;
  for (i = 0; i < len; ++i) {
    *dest++ = *p++;
  }
}

//------------------------------------------------------------------------------
// vector_hash_code
//------------------------------------------------------------------------------
INLINE long vector_hash_code(const vector_t * v) {
  if (v->hash_valid) {
    return v->hash;
  }
  long hash = 0;
  long i;
  for (i = 0; i < v->size; ++i) {
    hash += term_hash_code(v->data[i]);
  }
  ((vector_t *)v)->hash = __long_to_hash_code(hash);
  ((vector_t *)v)->hash_valid = 1;
  return hash;
}

//-------------------------------------------------------------------
// vector_find
//-------------------------------------------------------------------
INLINE long vector_find(const vector_t * v, term x, long pos, lisp_fun_t testfn) {
  if ((unsigned long)pos > (unsigned long)v->size) {
    lisp_signal(g_out_of_range, long_to_term(pos));
  }
  long i;
  for (i = pos; i < v->size; ++i) {
    term args[2] = {x, v->data[i]};
    if (!is_null(testfn(2, args))) {
      return i;
    }
  }
  return -1;
}

//-------------------------------------------------------------------
// Function: (vector-sort seq &optional (test 'compare) start count) ==> seq
// Test is (lambda (x y) and must return negative fixnum if x < y, zero if x = y
// and positive fixnum if x > y.
//-------------------------------------------------------------------
term lisp_vector_sort(term seq, term test, term start, term count);

//-------------------------------------------------------------------
// Function: (vector-stable-sort seq &optional (test 'compare) start count) ==> seq
// Test is (lambda (x y)) and must return negative fixnum if x < y, zero if x = y
// and positive fixnum if x > y.
//-------------------------------------------------------------------
term lisp_vector_stable_sort(term seq, term start, term count, term test);

//-------------------------------------------------------------------
// vector_reverse
//-------------------------------------------------------------------
INLINE void vector_reverse(vector_t * s) {
  s->hash_valid = 0;
  long i = 0;
  long j = s->size - 1;
  for (; i < j; ++i, --j) {
    term tmp = s->data[i];
    s->data[i] = s->data[j];
    s->data[j] = tmp;
  }
}

//------------------------------------------------------------------------------
// Function: (vector-to-list seq) ==> proper list
// Parameters: seq - vector
//
// Constructs list from vector's elements.
//------------------------------------------------------------------------------
term lisp_vector_to_list(term seq);

//------------------------------------------------------------------------------
// vector term operations
//------------------------------------------------------------------------------
INLINE term make_vector() {
  vector_t * s = (vector_t *)lisp_alloc(sizeof(vector_t), NULL);
  vector_init(s);
  return __pointer_to_term(s);
}

INLINE term make_vector_from_vector(const term * p, long len) {
  vector_t * s = (vector_t *)lisp_alloc(sizeof(vector_t), NULL);
  vector_init(s);
  vector_append_vector(s, p, len);
  return __pointer_to_term(s);
}

INLINE const vector_t * __get_vector_for_read(term t) {
  assert(get_term_type(t) == vector_e);
  return (vector_t *)__term_to_pointer(t);
}

INLINE vector_t * __get_vector_for_write(term t) {
  assert(get_term_type(t) == vector_e);
  vector_t * p = (vector_t *)__term_to_pointer(t);
  if (p->immutable) {
    lisp_signal(g_immutable_object, t);
  }
  p->hash_valid = 0;
  return p;
}

INLINE const vector_t * get_vector_for_read(term t) {
  if (!is_vector(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __get_vector_for_read(t);
}

INLINE vector_t * get_vector_for_write(term t) {
  if (!is_vector(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __get_vector_for_write(t);
}

//==============================================================================
// MUTEXES
//==============================================================================
typedef pthread_mutex_t    mutex_t;

#define MUTEX_INITIALIZER             PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP
#define RECURSIVE_MUTEX_INITIALIZER   PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP

INLINE int mutex_trylock(mutex_t * m) {
  int rc;
  if ((rc = pthread_mutex_trylock(m)) == 0) {
    return 1;
  }
  if (rc == EBUSY) {
    return 0;
  }
  errno = rc;
  lisp_signal_system_error();
}

INLINE void mutex_lock(mutex_t * m) {
  if (mutex_trylock(m)) {
    return;
  }
  int rc;
  if ((rc = pthread_mutex_lock(m)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
}

INLINE void mutex_unlock(mutex_t * m) {
  int rc;
  if ((rc = pthread_mutex_unlock(m)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
}

//-------------------------------------------------------------------
// Function: (mutex-create &optional name) ==> mutex
//-------------------------------------------------------------------
term lisp_mutex_create(term name);

//-------------------------------------------------------------------
// Function: (recursive-mutex-create &optional name) ==> mutex
//-------------------------------------------------------------------
term lisp_recursive_mutex_create(term name);

//-------------------------------------------------------------------
// Function: (mutex-destroy mutex) ==> nil
//-------------------------------------------------------------------
term lisp_mutex_destroy(term mutex);

//-------------------------------------------------------------------
// Function: (mutex-lock mutex) ==> mutex
//-------------------------------------------------------------------
term lisp_mutex_lock(term mutex);

//-------------------------------------------------------------------
// Function: (mutex-trylock mutex) ==> bool
//-------------------------------------------------------------------
term lisp_mutex_trylock(term mutex);

//-------------------------------------------------------------------
// Function: (mutex-unlock mutex) ==> mutex
//-------------------------------------------------------------------
term lisp_mutex_unlock(term mutex);

//-------------------------------------------------------------------
// Function: (mutex-name mutex) ==> object
//-------------------------------------------------------------------
term lisp_mutex_name(term mutex);

//==============================================================================
// CONDITIONS
//==============================================================================
//-------------------------------------------------------------------
// Function: (condition-create &optional name) ==> condition
//-------------------------------------------------------------------
term lisp_condition_create(term name);

//-------------------------------------------------------------------
// Function: (condition-destroy condition) ==> nil
//-------------------------------------------------------------------
term lisp_condition_destroy(term condition);

//-------------------------------------------------------------------
// Function: (condition-signal condition) ==> condition
//-------------------------------------------------------------------
term lisp_condition_signal(term condition);

//-------------------------------------------------------------------
// Function: (condition-wait condition mutex) ==> condition
//-------------------------------------------------------------------
term lisp_condition_wait(term condition, term mutex);

//-------------------------------------------------------------------
// Function: (condition-name condition) ==> object
//-------------------------------------------------------------------
term lisp_condition_name(term condition);

//==============================================================================
// RWLOCKS - read-write locks
//==============================================================================
//-------------------------------------------------------------------
// Function: (rwlock-create &optional name) ==> rwlock
//-------------------------------------------------------------------
term lisp_rwlock_create(term name);

//-------------------------------------------------------------------
// Function: (rwlock-destroy rwlock) ==> nil
//-------------------------------------------------------------------
term lisp_rwlock_destroy(term rwlock);

//-------------------------------------------------------------------
// Function: (rwlock-rdlock rwlock) ==> rwlock
//-------------------------------------------------------------------
term lisp_rwlock_rdlock(term rwlock);

//-------------------------------------------------------------------
// Function: (rwlock-wrlock rwlock) ==> rwlock
//-------------------------------------------------------------------
term lisp_rwlock_wrlock(term rwlock);

//-------------------------------------------------------------------
// Function: (rwlock-try-rdlock rwlock) ==> bool
//-------------------------------------------------------------------
term lisp_rwlock_try_rdlock(term rwlock);

//-------------------------------------------------------------------
// Function: (rwlock-try-wrlock rwlock) ==> bool
//-------------------------------------------------------------------
term lisp_rwlock_try_wrlock(term rwlock);

//-------------------------------------------------------------------
// Function: (rwlock-unlock rwlock) ==> rwlock
//-------------------------------------------------------------------
term lisp_rwlock_unlock(term rwlock);

//-------------------------------------------------------------------
// Function: (rwlock-name rwlock) ==> object
//-------------------------------------------------------------------
term lisp_rwlock_name(term rwlock);

//==============================================================================
// SEMAPHORES
//==============================================================================
//-------------------------------------------------------------------
// Function: (semaphore-create &optional name value) ==> semaphore
//-------------------------------------------------------------------
term lisp_semaphore_create(term name, term value);

//-------------------------------------------------------------------
// Function: (semaphore-destroy semaphore) ==> nil
//-------------------------------------------------------------------
term lisp_semaphore_destroy(term semaphore);

//-------------------------------------------------------------------
// Function: (semaphore-post semaphore) ==> semaphore
//-------------------------------------------------------------------
term lisp_semaphore_post(term semaphore);

//-------------------------------------------------------------------
// Function: (semaphore-wait semaphore) ==> semaphore
//-------------------------------------------------------------------
term lisp_semaphore_wait(term semaphore);

//-------------------------------------------------------------------
// Function: (semaphore-name semaphore) ==> object
//-------------------------------------------------------------------
term lisp_semaphore_name(term semaphore);

//-------------------------------------------------------------------
// Function: (semaphore-value semaphore) ==> integer
//-------------------------------------------------------------------
term lisp_semaphore_value(term semaphore);

//==============================================================================
// TQUEUE
//==============================================================================
//-------------------------------------------------------------------
// Function: (tqueue-create &optional name (alloc-pool-size 256)) ==> tqueue
//-------------------------------------------------------------------
term lisp_tqueue_create(term name, term alloc_pool_size);

//-------------------------------------------------------------------
// Function: (tqueue-push tqueue object) ==> tqueue
//-------------------------------------------------------------------
term lisp_tqueue_push(term tqueue, term object);

//-------------------------------------------------------------------
// Function: (tqueue-pop tqueue) ==> object
//-------------------------------------------------------------------
term lisp_tqueue_pop(term tqueue);

//-------------------------------------------------------------------
// Function: (tqueue-name tqueue) ==> object
//-------------------------------------------------------------------
term lisp_tqueue_name(term tqueue);

//-------------------------------------------------------------------
// Function: (tqueue-emptyp tqueue) ==> bool
//-------------------------------------------------------------------
term lisp_tqueue_emptyp(term tqueue);

//==============================================================================
// THREADS
//==============================================================================
//-------------------------------------------------------------------
// Function: (thread-create fn args &key name stack-size (detached t)
//                          suppress-closure-warning) ==> thread
//-------------------------------------------------------------------
term lisp_thread_create(term fn, term args, term name, term stack_size,
                        term detached, term suppress_closure_warning);

//-------------------------------------------------------------------
// Function: (thread-join thread) ==> object
//-------------------------------------------------------------------
term lisp_thread_join(term thread);

//-------------------------------------------------------------------
// Function: (thread-name thread) ==> object
//-------------------------------------------------------------------
term lisp_thread_name(term thread);

//-------------------------------------------------------------------
// Gateway to evaluate Lisp in context of foreign C thread.
//-------------------------------------------------------------------
term exec_in_lisp_context(term (*fn)(void *), void * p, int is_main_thread);

//==============================================================================
// Global variables
//==============================================================================
extern mutex_t        g_symbols_mutex;
extern dlist_t        g_global_symbols;

//==============================================================================
// Functions
//==============================================================================
//------------------------------------------------------------------------------
// struct function_t
//------------------------------------------------------------------------------
typedef struct function_t function_t;

// Runtime closure frame
typedef struct closure_env_t closure_env_t;
struct closure_env_t {
  closure_env_t *   next;       // MUST be first member, useed by code generator
  long              size;
  term              data[0];
};

struct function_t {
  long          type;
  // keep nargs and bytecode fields close to begining of structure: used by compiler's
  // code generator
  long          nreq_args;      // Number of required arguments
  long          nopt_args;      // Number of optional arguments
  long          nkey_args;      // Number of keyword arguments
  long          frame_size;     // Frame size for function: parameters + local variables
  lisp_fun_t    bcode;          // Function entry point
  const function_t * enclosed;  // enclosed function (to protect it from GC)
  closure_env_t * enclosed_env; // environment of enclosed function
  long          bc_capacity;
  long          bc_size;
  term          name;           // Function name
  term          origin_ll;      // Original lambda list
  term          key_arg_names;  // Names of keyword arguments
  term          compiled_body;
  term *        opt_arg_funs;   // Default values of optional arguments
  term *        key_arg_funs;   // Default values of keyword arguments
  int           has_rest_arg;
  int           closure_type;   // 0 - RAX points on closure_env_t on function
                                // entry.
                                // 1 - RAX points on closure's function_t on
                                // function entry. It happens if enclosed
                                // function creates new environment or function
                                // has non-trivial arguments list: &optional,
                                // &rest or &key arguments.
};

#define is_lambda(x)    (get_term_type(x) == lambda_e)

#define is_macro(x)    (get_term_type(x) == macro_e)

INLINE int is_function(term x) {
  term_type_e tp = get_term_type(x);
  return (tp == lambda_e || tp == macro_e);
}

//------------------------------------------------------------------------------
// function_init
//------------------------------------------------------------------------------
INLINE void function_init(function_t * f, term_type_e type) {
  f->type = type;
  f->nreq_args = 0;
  f->nopt_args = 0;
  f->nkey_args = 0;
  f->frame_size = 0;
  f->bcode = NULL;
  f->enclosed = NULL;
  f->enclosed_env = NULL;
  f->bc_capacity = 0;
  f->bc_size = 0;
  f->name = nil;
  f->origin_ll = nil;
  f->key_arg_names = nil;
  f->compiled_body = nil;
  f->opt_arg_funs = NULL;
  f->key_arg_funs = NULL;
  f->has_rest_arg = 0;
  f->closure_type = 0;
}

//------------------------------------------------------------------------------
// function_reset
//------------------------------------------------------------------------------
INLINE void function_reset(function_t * f) {
  f->nreq_args = 0;
  f->nopt_args = 0;
  f->nkey_args = 0;
  f->frame_size = 0;
  f->enclosed = NULL;
  f->enclosed_env = NULL;
  f->bc_size = 0;
  //  f->name = nil; do not reset the name
  f->origin_ll = nil;
  f->key_arg_names = nil;
  f->compiled_body = nil;
  f->opt_arg_funs = NULL;
  f->key_arg_funs = NULL;
  f->has_rest_arg = 0;
  f->closure_type = 0;
}

//------------------------------------------------------------------------------
// closure_init
//------------------------------------------------------------------------------
INLINE void __closure_init(function_t * f, const function_t * enclosed, closure_env_t * env) {
  f->type = enclosed->type;
  f->nreq_args = enclosed->nreq_args;
  f->nopt_args = enclosed->nopt_args;
  f->nkey_args = enclosed->nkey_args;
  f->frame_size = enclosed->frame_size;
  f->bcode = NULL;
  f->enclosed = enclosed;
  f->enclosed_env = env;
  f->bc_capacity = 0;
  f->bc_size = 0;
  f->name = enclosed->name;
  f->origin_ll = enclosed->origin_ll;
  f->key_arg_names = enclosed->key_arg_names;
  f->compiled_body = nil;
  f->opt_arg_funs = enclosed->opt_arg_funs;
  f->key_arg_funs = enclosed->key_arg_funs;
  f->has_rest_arg = enclosed->has_rest_arg;
  f->closure_type = enclosed->closure_type;
}

#define is_closure(fn)    ((fn)->enclosed_env != NULL)

//------------------------------------------------------------------------------
// function term operations
//------------------------------------------------------------------------------
INLINE function_t * __term_to_function(term t) {
  assert(is_function(t));
  return (function_t *)__term_to_pointer(t);
}

INLINE function_t * term_to_function(term t) {
  if (!is_function(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __term_to_function(t);
}

INLINE function_t * term_to_lambda(term t) {
  term_type_e tp = get_term_type(t);
  if (tp != lambda_e) {
    lisp_signal(g_invalid_arg, t);
  }
  return __term_to_function(t);
}

INLINE function_t * term_to_macro(term t) {
  if (get_term_type(t) != macro_e) {
    lisp_signal(g_invalid_arg, t);
  }
  return __term_to_function(t);
}

INLINE term make_lambda() {
  function_t * fn = (function_t *)lisp_alloc(sizeof(function_t), NULL);
  function_init(fn, lambda_e);
  return __pointer_to_term(fn);
}

INLINE term make_macro() {
  function_t * fn = (function_t *)lisp_alloc(sizeof(function_t), NULL);
  function_init(fn, macro_e);
  return __pointer_to_term(fn);
}

//------------------------------------------------------------------------------
// make_c_lambda and make_c_macro
// Makes C function accessible from Lisp as lambda or macro
// Parameters:
//   lisp_proto - prototype of function in Lisp: (fn args). For example, car
//     function will have following prototype: "(car x)".
//   cfn - pointer to C function. Interface of function depends on value of
//     check_args argument. It must have lisp_fun_t interface, if
//     check_args is false. In that case function must validate and parse
//     arguments by itself. If check_args is true, then code for parsing
//     and validating arguments is generated automatically using specified
//     lambda list. And The C code in that case is called as regular C function
//     after all arguments are parsed and bound to corresponding C arguments.
// Return function term.
//------------------------------------------------------------------------------
term make_c_lambda(const char * lisp_proto, const void * cfn, int check_args);
term make_c_lambda_proto(term lisp_proto, const void * cfn, int check_args);
term make_c_macro(const char * lisp_proto, const void * cfn, int check_args);

//------------------------------------------------------------------------------
// make_global_c_lambda and make_global_c_macro are same as make_c_lambda and
// make_c_macro, but also calls def_global_fun in order to bind the function to
// symbol and make it global.
// Return symbol, to which function is bound.
//------------------------------------------------------------------------------
term make_global_c_lambda(const char * lisp_proto, const void * cfn, int check_args);
term make_global_c_macro(const char * lisp_proto, const void * cfn, int check_args);

//------------------------------------------------------------------------------
// Function: (kl::def-global-fun sym fn) ==> sym
//------------------------------------------------------------------------------
term def_global_fun(term sym, term fn);

//------------------------------------------------------------------------------
// Function: (kl::def-global-var sym val) ==> sym
//------------------------------------------------------------------------------
term def_global_var(term sym, term val);

//------------------------------------------------------------------------------
// Function: (kl::undef-global-var sym) ==> sym
//------------------------------------------------------------------------------
term undef_global_var(term sym);

//==============================================================================
// Packages
//==============================================================================
//------------------------------------------------------------------------------
// Searches package by name, if it is not found, then it is created.
//------------------------------------------------------------------------------
term package_find_create(term name);

term package_find(const binary_t * name);

//------------------------------------------------------------------------------
// Returns name of package
//------------------------------------------------------------------------------
term package_get_name(term package);

//------------------------------------------------------------------------------
// Searches symbol
//------------------------------------------------------------------------------
term package_find_symbol(term package, const binary_t * name);

//------------------------------------------------------------------------------
// Searches symbol by name, if it is not found, then it is created.
//------------------------------------------------------------------------------
term package_find_create_symbol(term package, term name);

//------------------------------------------------------------------------------
// Removes symbol from package, called by GC
//------------------------------------------------------------------------------
void __package_remove_symbol(term package, const binary_t * name);

//------------------------------------------------------------------------------
// Returns list, containing all packages objects.
//------------------------------------------------------------------------------
term get_all_packages();

//------------------------------------------------------------------------------
// Returns vector, containing all package symbols.
//------------------------------------------------------------------------------
term package_symbols(term package);

//==============================================================================
// Symbols
//==============================================================================
typedef struct symbol_t   symbol_t;

struct symbol_t {
  dlist_node_t    root_node;    // global symbols reside in list to protect them
                                // from collecting by GC
  term            name;
  term            package;      // package where symbol is interned. If symbol
                                // is not interned, than package is nil
  term            value;
  term            fun;
  term            plist;
  long            dyn_idx;      // Dynamic binding index
  int             is_root;
  int             exported;
  // function trampoline is build such that it may be redirected in single
  // atomic assignment
#if defined(__x86_64__)
  int64_t         ftramp[3];
#elif defined(__i386__)
  int32_t         ftramp[2];    // function trampoline
#else
#error Unknown CPU architecture
#endif
};

#define is_symbol(x)    (get_term_type(x) == symbol_e)

//------------------------------------------------------------------------------
// __signal_unbound_function - internal function
//------------------------------------------------------------------------------
void __signal_unbound_function();

//------------------------------------------------------------------------------
// __pointer_to_symbol_term
//------------------------------------------------------------------------------
// INLINE term __pointer_to_symbol_term(const void * x) {
//   if (((long)x & 7) != 0) {
//     SIGNAL_INTERNAL_ERROR();
//   }
//   return (term)x | (term)1;
// }
#define __pointer_to_symbol_term(x)    (assert(((long)(x) & 7) == 0), (term)(x) | (term)1)

//------------------------------------------------------------------------------
// __term_to_symbol
//------------------------------------------------------------------------------
INLINE symbol_t * __term_to_symbol(term t) {
  assert(get_term_type(t) == symbol_e);
  return (symbol_t *)(t ^ (term)1);
}

//------------------------------------------------------------------------------
// symbol_init
//------------------------------------------------------------------------------
INLINE void symbol_init(symbol_t * s, term name) {
  s->root_node.next = s->root_node.prev = NULL;
  binary_immune(get_binary_for_read(name));
  s->name = name;
  s->value = g_unbound_marker;
  s->fun = g_unbound_fun_marker;
  s->plist = nil;
  s->dyn_idx = -1;
  s->package = nil;
  s->is_root = 0;
  s->exported = 0;
  binary_immune(get_binary_for_read(name));
#if defined(__x86_64__)
  // mov r11, __signal_unbound_function
  // jmp r11
  s->ftramp[0] = 0xBB49000000000000;
  s->ftramp[1] = (long)__signal_unbound_function;
  s->ftramp[2] = 0xE3FF41;
#elif defined(__i386__)
  // jmp __signal_unbound_function
  long offset = (long)__signal_unbound_function - (long)&s->ftramp[2];
  s->ftramp[0] = 0xE9000000;
  s->ftramp[1] = offset;
#else
#error Unknown CPU architecture
#endif
}

//------------------------------------------------------------------------------
// Protects global variable from garbage collection.
// Should be called when g_symbols_mutex is acquired
//------------------------------------------------------------------------------
INLINE void __lisp_add_global_symbol(symbol_t * s) {
  gc_lock();
  dlist_push_back(&g_global_symbols, &s->root_node);
  gc_unlock();
}

//------------------------------------------------------------------------------
// Unprotect global variable
// Should be called when g_symbols_mutex is acquired
//------------------------------------------------------------------------------
INLINE void __lisp_remove_global_symbol(symbol_t * s) {
  gc_lock();
  dlist_remove(&g_global_symbols, &s->root_node);
  gc_unlock();
}

//------------------------------------------------------------------------------
// symbol_get_ftramp
//------------------------------------------------------------------------------
INLINE void * symbol_get_ftramp(const symbol_t * s) {
#if defined(__x86_64__)
  return (uint8_t *)&s->ftramp[1] - 2;
#elif defined(__i386__)
  return (uint8_t *)&s->ftramp[1] - 1;
#else
#error Unknown CPU architecture
#endif
}

//------------------------------------------------------------------------------
// symbol_is_keyword
//------------------------------------------------------------------------------
INLINE int symbol_is_keyword(const symbol_t * v) {
  return eq(v->package, g_kw_package);
}

//------------------------------------------------------------------------------
// symbol_is_value_bound
//------------------------------------------------------------------------------
INLINE int symbol_is_value_bound(const symbol_t * v) {
  return !eq(v->value, g_unbound_marker);
}

//------------------------------------------------------------------------------
// symbol_is_function_bound
//------------------------------------------------------------------------------
INLINE int symbol_is_function_bound(const symbol_t * v) {
  return !eq(v->fun, g_unbound_fun_marker);
}

//------------------------------------------------------------------------------
// symbol_get_value
//------------------------------------------------------------------------------
INLINE term symbol_get_value(const symbol_t * v) {
  if (!symbol_is_value_bound(v)) {
    lisp_signal(g_unbound_variable, __pointer_to_symbol_term(v));
  }
  return v->value;
}

//------------------------------------------------------------------------------
// symbol_get_function
//------------------------------------------------------------------------------
INLINE term symbol_get_function(const symbol_t * v) {
  if (!symbol_is_function_bound(v)) {
    lisp_signal(g_unbound_function, __pointer_to_symbol_term(v));
  }
  return v->fun;
}

//------------------------------------------------------------------------------
// symbol_unbind_value
//------------------------------------------------------------------------------
INLINE void symbol_unbind_value(symbol_t * v) {
  mutex_lock(&g_symbols_mutex);
  v->value = g_unbound_marker;
  if (v->fun == g_unbound_fun_marker && v->is_root) {
    v->is_root = 0;
    __lisp_remove_global_symbol(v);
  }
  mutex_unlock(&g_symbols_mutex);
}

//------------------------------------------------------------------------------
// symbol_unbind_function
//------------------------------------------------------------------------------
INLINE void symbol_unbind_function(symbol_t * v) {
  mutex_lock(&g_symbols_mutex);
#if defined(__x86_64__)
  v->ftramp[1] = (long)__signal_unbound_function;
#elif defined(__i386__)
  long offset = (long)__signal_unbound_function - (long)&v->ftramp[2];
  v->ftramp[1] = offset;
#else
#error Unknown CPU architecture
#endif
  v->fun = g_unbound_fun_marker;
  if (v->value == g_unbound_marker && v->is_root) {
    v->is_root = 0;
    __lisp_remove_global_symbol(v);
  }
  mutex_unlock(&g_symbols_mutex);
}

//------------------------------------------------------------------------------
// symbol_bind_value
//------------------------------------------------------------------------------
INLINE void symbol_bind_value(symbol_t * v, term value) {
  if (symbol_is_keyword(v)) {
    lisp_signal(g_keyword_symbol, __pointer_to_symbol_term(v));
  }
  mutex_lock(&g_symbols_mutex);
  v->value = value;
  if (!v->is_root) {
    v->is_root = 1;
    __lisp_add_global_symbol(v);
  }
  mutex_unlock(&g_symbols_mutex);
}

//------------------------------------------------------------------------------
// symbol_bind_function
//------------------------------------------------------------------------------
INLINE void symbol_bind_function(symbol_t * v, term fun, const function_t * lfn) {
  mutex_lock(&g_symbols_mutex);
  v->fun = fun;
#if defined(__x86_64__)
  v->ftramp[1] = (long)lfn->bcode;
#elif defined(__i386__)
  long offset = (long)lfn->bcode - (long)&v->ftramp[2];
  v->ftramp[1] = offset;
#else
#error Unknown CPU architecture
#endif
  if (!v->is_root) {
    v->is_root = 1;
    __lisp_add_global_symbol(v);
  }
  mutex_unlock(&g_symbols_mutex);
}

//------------------------------------------------------------------------------
// symbol properties
//------------------------------------------------------------------------------
void symbol_set_prop(symbol_t * v, term key, term val);
term symbol_get_prop(const symbol_t * v, term key, term dflt);
term symbol_rm_prop(symbol_t * v, term key);

//------------------------------------------------------------------------------
// symbol dynamic binding
//------------------------------------------------------------------------------
long __symbol_alloc_dyn_idx(symbol_t * s);

INLINE long __symbol_get_alloc_dyn_idx(symbol_t * s) {
  long dyn_idx = s->dyn_idx;
  if (dyn_idx == -1) {
    return __symbol_alloc_dyn_idx(s);
  }
  return dyn_idx;
}

//------------------------------------------------------------------------------
// symbol GC finalizer
//------------------------------------------------------------------------------
void symbol_finalizer(void * p);

//------------------------------------------------------------------------------
// symbol term operations
//------------------------------------------------------------------------------
INLINE term __make_symbol(term name) {
  symbol_t * s = (symbol_t *)lisp_alloc(sizeof(symbol_t), symbol_finalizer);
  symbol_init(s, name);
  return __pointer_to_symbol_term(s);
}

INLINE term make_symbol(term name) {
  const binary_t * b = get_binary_for_read(name);
  return __make_symbol(make_binary_from_binary(b->data, b->size));
}

INLINE term make_symbol_from_binary(const uint8_t * pname, long name_len) {
  return __make_symbol(make_binary_from_binary((uint8_t *)pname, name_len));
}

INLINE term make_symbol_from_sz(const char * name) {
  return make_symbol_from_binary((uint8_t *)name, strlen(name));
}

INLINE symbol_t * term_to_symbol(term t) {
  if (!is_symbol(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  return __term_to_symbol(t);
}

//==============================================================================
// Custom types
//==============================================================================
typedef struct custom_t   custom_t;

struct custom_t {
  long            type;
  term            cl_name;      // class name of custom object
  void *          value;
};

#define is_custom(x)    (get_term_type(x) == custom_e)

//-------------------------------------------------------------------
// custom term operations
//-------------------------------------------------------------------
INLINE const custom_t * __term_to_custom(term t) {
  assert(get_term_type(t) == custom_e);
  return (custom_t *)__term_to_pointer(t);
}

INLINE void * term_custom_value(term t, term cl_name) {
  if (!is_custom(t)) {
    lisp_signal(g_invalid_arg, t);
  }
  const custom_t * c = __term_to_custom(t);
  if (!eq(c->cl_name, cl_name)) {
    lisp_signal(g_invalid_arg, t);
  }
  return c->value;
}

INLINE term make_custom(term cl_name, void * value) {
  custom_t * c = (custom_t *)lisp_alloc(sizeof(custom_t), NULL);
  c->type = custom_e;
  c->cl_name = cl_name;
  c->value = value;
  return __pointer_to_term(c);
}

//==============================================================================
// Lisp virtual machine.
// That what is remain from bytecode epoch.
// Now it is used to hold global thread variables.
//==============================================================================
//-------------------------------------------------------------------
// values_t
//-------------------------------------------------------------------
typedef struct values_t values_t;

struct values_t {
  term *  data;
  long    size;
  long    capacity;
  long    immutable;            // immutable counter
};

INLINE void values_init(values_t * v, term * data, long capacity) {
  v->data = data;
  v->size = 0;
  v->capacity = capacity;
  v->immutable = 0;
}

INLINE void values_immune(values_t * v) {
  if (v != NULL) {
    v->immutable += 1;
  }
}

INLINE void values_unimmune(values_t * v) {
  if (v != NULL) {
    v->immutable -= 1;
  }
}

INLINE void values_copy(values_t * src, values_t * dst) {
  if (src == NULL || dst == NULL || dst->immutable) {
    return;
  }
  if (dst->capacity < src->size) {
    src->size = dst->capacity;
  }
  long i;
  // clear superfluous destination values
  for (i = src->size; i< dst->size; ++i) {
    dst->data[i] = nil;
  }
  // copy values
  for (i = 0; i< src->size; ++i) {
    dst->data[i] = src->data[i];
  }
  dst->size = src->size;
}

//-------------------------------------------------------------------
// GC internal definitions used in VM
//-------------------------------------------------------------------
#define GC_NDESCR_IN_VM     32

//-------------------------------------------------------------------
// struct gc_descr_t
//-------------------------------------------------------------------
typedef struct gc_descr_t gc_descr_t;

struct gc_descr_t {
  uint8_t *     start;
  uint8_t *     end;
  gc_descr_t *  next;           // used in mark
  finalizer_t   finalizer;
  unsigned int  color:1;
  unsigned int  atomic:1;
};

//-------------------------------------------------------------------
// vm_t
//-------------------------------------------------------------------
typedef struct vm_t vm_t;
extern __thread vm_t * g_vm;
typedef struct sig_frame_t sig_frame_t;

struct vm_t {
  term *              denv;           // dynamic environment
  long                denv_size;      // size of dynamic environment
  sig_frame_t *       sig_frame;      // last signal frame
  term                macro_env;      // compiler's environment, when macro is invoked
  values_t *          values;
  //------------------------------
  // PRINTF helper variables
  //------------------------------
  term                printf_stream;
  binary_t *          printf_cfmt;
  binary_t *          printf_res; // make it pointer since it is passed to
                                  // __pointer_to_term and must be properly aligned;
  uint8_t             __storage_for_printf_res[sizeof(binary_t) + 8];
  uint8_t             __storage_for_printf_cfmt[sizeof(binary_t) + 8];
  //------------------------------
  // ERROR helper variables
  //------------------------------
  term                error_stream;
  //------------------------------
  // GC helper variables
  //------------------------------
  dlist_node_t        root_node;     // node in global list of VMs
  pthread_t           thread;        // native thread to which VM belongs
  ucontext_t *        context;       // pointer on context of thread suspended by signal
  const void *        stack_top;     // top of thread's stack, set by suspend signal
                                     // handler or before entering GC lock wait.
  gc_descr_t          ptrs[GC_NDESCR_IN_VM];
  uint8_t * volatile  avail_mem;
  volatile long       avail_size;
  pthread_spinlock_t  alloc_spin;
  volatile int        nptrs;
};

INLINE void vm_init(vm_t * vm) {
  vm->denv = NULL;
  vm->denv_size = 0;
  vm->sig_frame = NULL;
  vm->macro_env = nil;
  vm->values = NULL;
  // PRINTF helper variables
  vm->printf_stream = nil;
  vm->printf_cfmt = (binary_t *)ALIGNUP((uintptr_t)vm->__storage_for_printf_cfmt, 8);
  binary_init(vm->printf_cfmt);
  vm->printf_res = (binary_t *)ALIGNUP((uintptr_t)vm->__storage_for_printf_res, 8);
  binary_init(vm->printf_res);
  // ERROR helper variables
  vm->error_stream = nil;
  // GC helper variables
  vm->root_node.next = vm->root_node.prev = NULL;
  vm->thread = pthread_self();
  vm->context = NULL;
  vm->stack_top = NULL;
  memset(vm->ptrs, 0, sizeof(vm->ptrs));
  vm->avail_mem = NULL;
  vm->avail_size = 0;
  pthread_spin_init(&vm->alloc_spin, 0);
  vm->nptrs = 0;
}

//-------------------------------------------------------------------
// returns current VM
//-------------------------------------------------------------------
INLINE vm_t * vm_get_current() {
  return g_vm;
}

//-------------------------------------------------------------------
// signal frame
//-------------------------------------------------------------------
typedef enum sig_frame_kind_e {
  sig_frame_catch_e,
  sig_frame_lisp_handler_e,
  sig_frame_c_handler_e,
  sig_frame_c_unwind_e,
  sig_frame_lisp_unwind_e
} sig_frame_kind_e;

struct sig_frame_t {
  sig_frame_t *     next;
  sig_frame_kind_e  kind;
};

// General frame uninstall function - uninstalls last signal frame in VM
INLINE void vm_frame_uninstall(vm_t * vm) {
  vm->sig_frame = vm->sig_frame->next;
}

//-------------------------------------------------------------------
// catch frame. May be installed by both Lisp and C code.
//-------------------------------------------------------------------
typedef struct catch_frame_t catch_frame_t;

struct catch_frame_t {
  sig_frame_t header;
  term        tag;             // catch tag
  term        value;           // catch value, returned by throw
  sigjmp_buf  sigbuf;          // catch is implemented with sigsetjmp/siglongjmp
};

INLINE void catch_frame_install(catch_frame_t * f, vm_t * vm, term tag) {
  f->header.next = vm->sig_frame;
  f->header.kind = sig_frame_catch_e;
  f->tag = tag;
  f->value = g_unbound_marker;
  vm->sig_frame = &f->header;
}

INLINE void catch_frame_uninstall(catch_frame_t * f, vm_t * vm) {
  vm->sig_frame = f->header.next;
}

//-------------------------------------------------------------------
// C unwind frame
// This is for unwinding in C code
//-------------------------------------------------------------------
typedef struct c_unwind_frame_t c_unwind_frame_t;
typedef void (* c_unwind_callback)(void *);

struct c_unwind_frame_t {
  sig_frame_t         header;
  c_unwind_callback   callback;
  void *              arg;
  int                 done;     // ensures that callback called once only
};

INLINE void c_unwind_frame_install(c_unwind_frame_t * f, vm_t * vm,
                                   c_unwind_callback callback, void * arg) {
  f->header.next = vm->sig_frame;
  f->header.kind = sig_frame_c_unwind_e;
  f->callback = callback;
  f->arg = arg;
  f->done = 0;
  vm->sig_frame = &f->header;
}

//-------------------------------------------------------------------
// lisp unwind frame
// this frame and callback are created by Lisp compiler for 'unwind-protect special form.
//-------------------------------------------------------------------
typedef struct lisp_unwind_frame_t lisp_unwind_frame_t;
typedef void (* lisp_unwind_callback)(void * env, void * params, void * local_vars);

struct lisp_unwind_frame_t {
  sig_frame_t           header;
  lisp_unwind_callback  callback;
  void *                env;
  void *                params;
  void *                local_vars;
  int                   done;   // ensures that callback called once only
};

INLINE void lisp_unwind_frame_install(lisp_unwind_frame_t * f, vm_t * vm,
                                      lisp_unwind_callback callback, void * env,
                                      void * params, void * local_vars) {
  f->header.next = vm->sig_frame;
  f->header.kind = sig_frame_lisp_unwind_e;
  f->callback = callback;
  f->env = env;
  f->params = params;
  f->local_vars = local_vars;
  f->done = 0;
  vm->sig_frame = &f->header;
}

//-------------------------------------------------------------------
// Lisp signal handler frame
// This frame may be installed by both Lisp and C code (use make_c_lambda).
// Signal handler has following prototype (lambda (label value)). Signal
// handlers are called before the stack is unwinded.  All signal handlers are
// called by lisp_signal function untill signal is handled. If signal is
// handled, then handler will never return. Usually, signal handler will call
// lisp_throw function in order to unwind stack and pass control to stable
// code. That what is handler-case macro does. If signal is not handled,
// lisp_signal calls function from kl:*unhandled-signal-hook*. Unhandled signal
// hook must not return. If it is returned then Lisp is terminated abnormally.
//-------------------------------------------------------------------
typedef struct sig_lisp_handler_frame_t sig_lisp_handler_frame_t;

struct sig_lisp_handler_frame_t {
  sig_frame_t   header;
  term          handler;
};

INLINE void sig_lisp_handler_frame_install(sig_lisp_handler_frame_t * f, vm_t * vm, term handler) {
  f->header.next = vm->sig_frame;
  f->header.kind = sig_frame_lisp_handler_e;
  f->handler = handler;
  vm->sig_frame = &f->header;
}

//-------------------------------------------------------------------
// C signal handler frame
// This frame may be installed by C code only.
//-------------------------------------------------------------------
typedef struct sig_c_handler_frame_t sig_c_handler_frame_t;
typedef void (* c_sig_handler_t)(term label, term value);

struct sig_c_handler_frame_t {
  sig_frame_t       header;
  c_sig_handler_t   handler;
};

INLINE void sig_c_handler_frame_install(sig_c_handler_frame_t * f, vm_t * vm,
                                        c_sig_handler_t handler) {
  f->header.next = vm->sig_frame;
  f->header.kind = sig_frame_c_handler_e;
  f->handler = handler;
  vm->sig_frame = &f->header;
}

//-----------------------------------------------------------------
// unwind protect macros.
// IMPORTANT: do not perform 'return' from protected region, because it won't
// restore signal handler. Perform 'goto __unwind_protect_end__;' instead, or do
// something else.
//-----------------------------------------------------------------
#define UNWIND_PROTECT_BEGIN(fn, arg)                           \
  do {                                                          \
    vm_t * __vm__ = vm_get_current();                           \
    c_unwind_frame_t  __frame__;                                \
    c_unwind_frame_install(&__frame__, __vm__, fn, arg);

#define UNWIND_PROTECT_END                                      \
   __unwind_protect_end__:                                      \
    vm_frame_uninstall(__vm__);                                 \
    __frame__.callback(__frame__.arg);                          \
    if (0) {                                                    \
      /* make compiler happy: do not warn unused label */       \
      goto __unwind_protect_end__;                              \
    }                                                           \
  } while (0)

//-----------------------------------------------------------------
// catch macros
// IMPORTANT: do not perform 'return' from protected region, because it won't
// restore signal handler. Perform 'goto __catch_end__;' instead, or do
// something else.
//-----------------------------------------------------------------
#define CATCH_BEGIN(tag)                                        \
  do {                                                          \
    vm_t * __vm__ = vm_get_current();                           \
    catch_frame_t __frame__;                                    \
    catch_frame_install(&__frame__, __vm__, tag);               \
    if (sigsetjmp(__frame__.sigbuf, 0) == 0) {


#define CATCH_END_EX(value_var_addr)                            \
    } else {                                                    \
      *(term *)(value_var_addr) = __frame__.value;              \
      if (0) {                                                  \
        /* make compiler happy: do not warn unused label */     \
        goto __catch_end__;                                     \
      }                                                         \
    }                                                           \
     __catch_end__:                                             \
    catch_frame_uninstall(&__frame__, __vm__);                  \
} while (0)

#define CATCH_END                                               \
    } else {                                                    \
      if (0) {                                                  \
        /* make compiler happy: do not warn unused label */     \
        goto __catch_end__;                                     \
      }                                                         \
    }                                                           \
     __catch_end__:                                             \
    catch_frame_uninstall(&__frame__, __vm__);                  \
} while (0)

//-----------------------------------------------------------------
// handler-bind macros
// IMPORTANT: do not perform 'return' from protected region, because it won't
// restore signal handler. Perform 'goto __handler_bind_end__;' instead, or do
// something else.
//-----------------------------------------------------------------
#define HANDLER_BIND_BEGIN(fn)                                  \
  do {                                                          \
    vm_t * __vm__ = vm_get_current();                           \
    sig_c_handler_frame_t  __frame__;                           \
    sig_c_handler_frame_install(&__frame__, __vm__, fn);

#define HANDLER_BIND_END                                        \
   __handler_bind_end__:                                        \
    vm_frame_uninstall(__vm__);                                 \
    if (0) {                                                    \
      /* make compiler happy: do not warn unused label */       \
      goto __handler_bind_end__;                                \
    }                                                           \
  } while (0)

//===================================================================
// multiple return values
// if multiple values protocol is changed, do not forget to update 'kl::%values'
// function in native.c
//===================================================================
#define MULTIPLE_VALUES_LIMIT   16

// internal function
INLINE void __vm_clear_values() {
  vm_t * vm = vm_get_current();
  if (vm->values == NULL || vm->values->immutable) {
    return;
  }
  long i;
  for (i = 0; i < vm->values->size; ++i) {
    vm->values->data[i] = nil;
  }
  vm->values->size = 0;
}

//-------------------------------------------------------------------
// Function: (values res &rest other) ==> multiple values
//-------------------------------------------------------------------
INLINE term values(long n, const term * args) {
  if (n < 1) {
    lisp_signal(g_invalid_arg, __long_to_fixnum_term(n));
  }
  vm_t * vm = vm_get_current();
  if (vm->values == NULL || vm->values->immutable) {
    return args[0];
  }
  if (n > vm->values->capacity) {
    n = vm->values->capacity;
  }
  if (n == 1) {
    return args[0];
  }
  long i;
  // clear previous values
  for (i = n; i < vm->values->size; ++i) {
    vm->values->data[i] = nil;
  }
  // set new values
  for (i = 0; i < n; ++i) {
    vm->values->data[i] = args[i];
  }
  vm->values->size = n;
  return args[0];
}

//-------------------------------------------------------------------
// macro wrapper for values
//-------------------------------------------------------------------
#define VALUES(...)                                         \
  ({const term __args__[] = {__VA_ARGS__};                  \
    values(sizeof(__args__) / sizeof(term), __args__);})

//-------------------------------------------------------------------
// vm_value_get
//-------------------------------------------------------------------
INLINE term vm_value_get(long i, term dflt) {
  if (i < 0) {
    lisp_signal(g_invalid_arg, long_to_term(i));
  }
  vm_t * vm = vm_get_current();
  if (vm->values == NULL || i > vm->values->size) {
    return dflt;
  }
  return vm->values->data[i];
}

//==============================================================================
// Dynamic environment functions.
//==============================================================================
//------------------------------------------------------------------------------
// __vm_set_dynamic_binding
// This function is part of implementation of dynamic-let special form
//------------------------------------------------------------------------------
INLINE term __vm_set_dynamic_binding(term sym, term val) {
  vm_t * vm = vm_get_current();
  long dyn_idx = __symbol_get_alloc_dyn_idx(term_to_symbol(sym));
  if (dyn_idx >= vm->denv_size) {
    if (vm->denv == NULL) {
      long denv_size = dyn_idx + 1;
      vm->denv = (term *)lisp_alloc(sizeof(term) * denv_size, NULL);
      long i;
      for (i = 0; i < denv_size; ++i) {
        vm->denv[i] = g_unbound_marker;
      }
      vm->denv_size = denv_size;
    } else {
      long denv_size = vm->denv_size * 2;
      if (dyn_idx >= denv_size) {
        denv_size = dyn_idx + 1;
      }
      vm->denv = (term *)lisp_realloc(vm->denv,
                                      vm->denv_size * sizeof(term),
                                      denv_size * sizeof(term));
      long i;
      for (i = vm->denv_size; i < denv_size; ++i) {
        vm->denv[i] = g_unbound_marker;
      }
      vm->denv_size = denv_size;
    }
  }
  term old_val = vm->denv[dyn_idx];
  vm->denv[dyn_idx] = val;
  return old_val;
};

//------------------------------------------------------------------------------
// vm_get_dynamic
// returns value of dynamic variable
//------------------------------------------------------------------------------
INLINE term vm_get_dynamic(term sym) {
  const vm_t * vm = vm_get_current();
  symbol_t * s = term_to_symbol(sym);
  if (s->dyn_idx == -1 || s->dyn_idx >= vm->denv_size ||
      eq(vm->denv[s->dyn_idx], g_unbound_marker)) {
    return symbol_get_value(s);
  }
  return vm->denv[s->dyn_idx];
}

//------------------------------------------------------------------------------
// vm_set_dynamic
// sets value of dynamic variable
//------------------------------------------------------------------------------
INLINE term vm_set_dynamic(term sym, term val) {
  const vm_t * vm = vm_get_current();
  symbol_t * s = term_to_symbol(sym);
  if (s->dyn_idx == -1 || s->dyn_idx >= vm->denv_size ||
      eq(vm->denv[s->dyn_idx], g_unbound_marker)) {
    if (!symbol_is_value_bound(s)) {
      lisp_signal(g_unbound_variable, sym);
    }
    symbol_bind_value(s, val);
    return val;
  }
  vm->denv[s->dyn_idx] = val;
  return val;
}

//------------------------------------------------------------------------------
// vm_is_dynamic_bound
// returns true, if dynamic variable is bound with dynamic-let
//------------------------------------------------------------------------------
INLINE int vm_is_dynamic_bound(const symbol_t * s) {
  const vm_t * vm = vm_get_current();
  if (s->dyn_idx == -1 || s->dyn_idx >= vm->denv_size ||
      eq(vm->denv[s->dyn_idx], g_unbound_marker)) {
    return 0;
  }
  return 1;
}

//==============================================================================
// Evaluation functions
//==============================================================================
//------------------------------------------------------------------------------
// Function: (eval form) ==> object
// Parameters: form - object
//
// Evaluates form in the current dynamic environment and the null lexical
// environment.
//------------------------------------------------------------------------------
term eval(term form);

//-------------------------------------------------------------------
// Function: (apply fn args)
//-------------------------------------------------------------------
term apply(term fn, term args);

//-------------------------------------------------------------------
// Function: (apply-vector fn args)
//-------------------------------------------------------------------
term apply_vector(term fn, term args);

//-------------------------------------------------------------------
// Function: (funcall fn &rest args)
//-------------------------------------------------------------------
INLINE term funcall(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  const function_t * lfn;
  if (is_symbol(*args)) {
    lfn = term_to_function(symbol_get_function(term_to_symbol(*args)));
  } else {
    lfn = term_to_function(*args);
  }
  __vm_clear_values();
  return lfn->bcode(nargs - 1, args + 1);
}

//------------------------------------------------------------------------------
// macro wrapper for funcall
//------------------------------------------------------------------------------
#define FUNCALL(...)                                           \
  ({const term __args__[] = {__VA_ARGS__};                     \
    funcall(sizeof(__args__) / sizeof(term), __args__);})

//-------------------------------------------------------------------
// Function: (macroexpand-1 exp &optional env) ==> object
//-------------------------------------------------------------------
term macroexpand_1(term exp, term env);

//-------------------------------------------------------------------
// Function: (macroexpand exp &optional env) ==> object
//-------------------------------------------------------------------
term macroexpand(term exp, term env);

//-------------------------------------------------------------------
// Function: (compiler-macroexpand-1 exp &optional env) ==> object
//-------------------------------------------------------------------
term compiler_macroexpand_1(term exp, term env);

//-------------------------------------------------------------------
// Function: (compiler-macroexpand exp &optional env) ==> object
//-------------------------------------------------------------------
term compiler_macroexpand(term exp, term env);

//------------------------------------------------------------------------------
// Function: (kl::%define-symbol-macro sym expansion) ==> sym
//------------------------------------------------------------------------------
term define_symbol_macro(term sym, term expansion);

//-------------------------------------------------------------------
// Function: (symbol-macro sym &optional not-found) ==> object
//-------------------------------------------------------------------
term symbol_macro(term sym, term not_found);

//-------------------------------------------------------------------
// Function: (kl::compiler-macro sym &optional not-found) ==> object
//-------------------------------------------------------------------
term compiler_macro(term sym, term not_found);

//------------------------------------------------------------------------------
// Function: (repl &optional (package *package*)) ==> nil
//------------------------------------------------------------------------------
term repl(term package);

//------------------------------------------------------------------------------
// Function: (load path &optional verbose) ==> object
//------------------------------------------------------------------------------
term load(term path, term verbose);

//==============================================================================
// Streams. All streams operations are blocking. All library stream functions
// lock stream for exclusive usage and perform their input-output atomically. If
// stream is used in different threads and multiple I/O operations should be
// done atomically, then user should take care to synchronize its usage with
// stream-lock and stream-unlock functions.
//==============================================================================
typedef struct stream_t    stream_t;

struct stream_t {
  mutex_t   mutex;              // stream lock
  term      class_name;         // name of stream class
  term      name;               // stream name, used in print operations
  term      ungot_char;

  //----------------------------------------------------------------------------
  // Reads single byte from stream and returns it. Returns nil in case of end-of-file
  //----------------------------------------------------------------------------
  term (* read_byte)(stream_t * stream);

  //----------------------------------------------------------------------------
  // Writes byte to stream. Error is signaled, if byte cann't be written.
  //----------------------------------------------------------------------------
  void (* write_byte)(stream_t * stream, term byte);

  //----------------------------------------------------------------------------
  // Tries to read maximum 'count' bytes from stream into buffer. Nil is
  // returned in case of end of file.
  // ----------------------------------------------------------------------------
  term (* read_binary)(stream_t * stream, term count, term buf);

  //----------------------------------------------------------------------------
  // Tries to write at least one byte of 'buf' to stream. Error is signaled, if
  // nothing is written to stream. Returns number of bytes written to stream.
  //----------------------------------------------------------------------------
  term (* write_binary)(stream_t * stream, term buf, term buf_offset, term count);

  //----------------------------------------------------------------------------
  // Flush ensures that, if stream performs output buffering, then all modified
  // data from buffer are transmitted to operating system.
  //----------------------------------------------------------------------------
  void (* flush)(stream_t * stream);

  //----------------------------------------------------------------------------
  // Sync ensures that all modified data from both user-space and operating
  // system buffers are transferred to physical media. The call blocks until the
  // device reports that the transfer has completed. It also flushes metadata
  // information associated with the stream.
  //----------------------------------------------------------------------------
  void (* sync)(stream_t * stream);

  //----------------------------------------------------------------------------
  // datasync is similar to sync, but does not flush modified metadata unless
  // that metadata is needed in order to allow a subsequent data retrieval to be
  // correctly handled.
  //----------------------------------------------------------------------------
  void (* datasync)(stream_t * stream);

  //----------------------------------------------------------------------------
  // Flushes all modified data and closes stream. Stream becomes unusable.
  //----------------------------------------------------------------------------
  void (* close)(stream_t * stream);
};

//------------------------------------------------------------------------------
// creates empty stream term
//------------------------------------------------------------------------------
term make_stream(long alloc_size, term class_name, term name, finalizer_t finalizer);

//------------------------------------------------------------------------------
// term_to_stream
//------------------------------------------------------------------------------
INLINE stream_t * term_to_stream(term t) {
  return (stream_t *)term_custom_value(t, g_stream_class_name);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
void __reset_stream(stream_t * stream);

//------------------------------------------------------------------------------
// Function:
// (kl:stream-create &key
//  name
//  (read-byte (lambda ()
//               (signal 'unsupported-operation 'read-byte)))
//  (write-byte (lambda (byte)
//                (declare (ignore byte))
//                (signal 'unsupported-operation 'write-byte)))
//  (read-binary (lambda (count buf)
//                 (declare (ignore count buf))
//                 (signal 'unsupported-operation 'read-binary)))
//  (write-binary (lambda (buf buf-offset count)
//                  (declare (ignore buf buf-offset count))
//                  (signal 'unsupported-operation 'write-binary)))
//  (flush (lambda ()
//          (signal 'unsupported-operation 'flush)))
//  (sync (lambda ()
//          (signal 'unsupported-operation 'sync)))
//  (datasync (lambda ()
//              (signal 'unsupported-operation 'datasync)))
//  (close (lambda ()
//           (signal 'unsupported-operation 'close))))
//------------------------------------------------------------------------------
term make_custom_stream(term name, term read_byte, term write_byte,
                        term read_binary, term write_binary, term flush,
                        term sync, term datasync, term close);

//------------------------------------------------------------------------------
// Function: (stream-lock stream) ==> stream
//
// Parameters: stream - stream
//
// Locks stream.
//------------------------------------------------------------------------------
INLINE term stream_lock(term stream) {
  mutex_lock(&term_to_stream(stream)->mutex);
  return stream;
}

//------------------------------------------------------------------------------
// Function: (stream-unlock stream) ==> stream
//
// Parameters: stream - stream
//
// Unlocks stream.
//------------------------------------------------------------------------------
INLINE term stream_unlock(term stream) {
  mutex_unlock(&term_to_stream(stream)->mutex);
  return stream;
}

//------------------------------------------------------------------------------
// Function: (stream-name stream) ==> object
//
// Parameters: stream - stream
//
// Returns name of stream.
//------------------------------------------------------------------------------
INLINE term stream_name(term stream) {
  return term_to_stream(stream)->name;
}

//------------------------------------------------------------------------------
// Function: (unread-byte byte &optional (stream *stdin*)) ==> nil
//
// Parameters: stream - stream
//             byte - integer in range 0 - 255 or nil
//
// Pushes back byte to stream, where it is available for subsequent read
// operations. Pushed back characters will be returned in reverse order; only
// one pushback is guaranteed.
//------------------------------------------------------------------------------
term unread_byte(term byte, term stream);

//------------------------------------------------------------------------------
// Function: (read-byte &optional (stream *stdin*)) ==> integer in range 0-255 or nil
//
// Parameters: stream - stream
//
// Reads single byte from stream and returns it. Returns nil in case of end-of-file
//------------------------------------------------------------------------------
term read_byte(term stream);

//------------------------------------------------------------------------------
// Function: (write-byte byte &optional (stream *stdout*)) ==> nil
//
// Parameters: stream - stream
//             byte - integer in range 0 - 255
//
// Writes byte to stream. Error is signaled, if byte cann't be written.
//------------------------------------------------------------------------------
term write_byte(term byte, term stream);

//------------------------------------------------------------------------------
// Function: (read-binary count &optional (stream *stdin*)
//                        (buf (binary))) ==> buf or nil
//
// Parameters: stream - stream
//             count - max number of bytes to read
//             buf - binary, read data are added to end of buffer.
//
// Tries to read maximum 'count' bytes from stream. Returns buffer. nil is
// returned in case of end-of-file.
//------------------------------------------------------------------------------
term read_binary(term count, term stream, term buf);

//------------------------------------------------------------------------------
// Function: (read-exact count &optional (stream *stdin*) (buf (binary))) ==> buf
//
// Parameters: stream - stream
//             count - max number of bytes to read
//             buf - binary, read data are added to end of buffer.
//
// Reads exact 'count' bytes from stream. Error is signaled if less bytes are read.
//------------------------------------------------------------------------------
term read_exact(term count, term stream, term buf);

//------------------------------------------------------------------------------
// Function: (write-binary buf &optional (stream *stdout*) (buf-offset 0)
//                         (count (- (binary-length buf) buf-offset))) ==> integer
//
// Parameters: buf - binary.
//             stream - stream
//             buf-offset - integer. Offset within buffer, from which data are
//               written to stream.
//             count - maximal number of bytes, that should be written to stream.
//
// Tries to write at least one byte of 'buf' to stream. Error is signaled, if
// nothing is written to stream. Returns number of bytes written to stream.
//------------------------------------------------------------------------------
term write_binary(term buf, term stream, term buf_offset, term count);

//------------------------------------------------------------------------------
// Function: (write-exact buf &optional (stream *stdout*)
//                        buf-offset count) ==> nil
//
// Parameters: buf - binary.
//             stream - stream
//             buf-offset - integer. Offset within buffer, from which data are
//               written to stream. Default is 0.
//             count - exact number of bytes, that should be written to
//               stream. Default is write all bytes starting from offset.
//
// Writes exact number of buffer bytes, starting from given offset, to stream.
//------------------------------------------------------------------------------
term write_exact(term buf, term stream, term buf_offset, term count);

INLINE term write_whole_binary(term buf, term stream) {
  return write_exact(buf, stream, __long_to_fixnum_term(0),
                     __long_to_fixnum_term(get_binary_for_read(buf)->size));
}

//------------------------------------------------------------------------------
// Function: (flush &optional (stream *stdout*)) ==> nil
//
// Parameters: stream - stream
//
// Flush ensures that, if stream performs output buffering, then all modified
// data from buffer are transmitted to operating system.
//------------------------------------------------------------------------------
term stream_flush(term stream);

//------------------------------------------------------------------------------
// Function: (sync &optional (stream *stdout*)) ==> nil
//
// Parameters: stream - stream
//
// Sync ensures that all modified data from both user-space and operating
// system buffers are transferred to physical media. The call blocks until the
// device reports that the transfer has completed. It also flushes metadata
// information associated with the stream.
//------------------------------------------------------------------------------
term stream_sync(term stream);

//------------------------------------------------------------------------------
// Function: (datasync &optional (stream *stdout*)) ==> nil
//
// Parameters: stream - stream
//
// datasync is similar to sync, but does not flush modified metadata unless
// that metadata is needed in order to allow a subsequent data retrieval to be
// correctly handled.
//------------------------------------------------------------------------------
term stream_datasync(term stream);

//------------------------------------------------------------------------------
// Function: (close stream) ==> nil
//
// Parameters: stream - stream
//
// Flushes all modified data and closes stream. Stream becomes unusable.
//------------------------------------------------------------------------------
term stream_close(term stream);

//------------------------------------------------------------------------------
// Function: (read-line &optional (stream *stdin*) max-len buffer) ==> buffer or nil
//
// Parameters: stream - stream.
//             max-len - integer. Maximal length of read line.
//             buffer - binary. If supplied then line is added to buffer.
//
// Reads characters from stream until new line character #\"\\n\" or end of file is
// encountered. Returns read line or nil in case of end of file. If no new line is
// found and number of characters exceeds max-len, then error is signaled"
//------------------------------------------------------------------------------
term read_line(term stream, term max_len, term buffer);

//==============================================================================
// Binary stream
//==============================================================================
term make_binary_stream(term content);

INLINE term make_binary_stream_from_sz(const char * content) {
  return make_binary_stream(make_binary_from_sz(content));
}

term binary_stream_rewind(term stream, term reset_content, term new_content);

term binary_stream_content(term stream);

//==============================================================================
// File stream
//==============================================================================
term make_file_stream(term pathname, term flags, term mode);
term make_file_stream_from_fd(int fd, term name);

INLINE term make_file_stream_sz(const char * pathname, int flags, int mode) {
  return make_file_stream(make_binary_from_sz(pathname), __long_to_fixnum_term(flags),
                          __long_to_fixnum_term(mode));
}

term file_pwrite(term file, term buffer, term file_offset, term buf_offset, term count);
term file_pread(term file, term offset, term count, term buf);
term file_seek(term file, term offset, term whence);

//==============================================================================
// printf
//
// Positional parameters '%m$' and '*m$' are not supported at this stage.
//
// lisp_printf_sz nad lisp_fprintf_sz support both standard C printf format
// specifiers and Lisp format specifiers.
//
// lisp_printf and lisp_fprintf support only Lisp format specifiers.
//
// C format specifiers are with % and Lisp format specifiers are prefixed with ~.
// Syntax of common format specifiers is same. Following Lisp format specfiers are
// supported:
//   ~d, ~i - same as C %d specfier. Length modifiers, like l, h etc., are pronhibited,
//        because size of Lisp is always known. Nor flags or precision are
//        supportted at this stage.
//   ~o, ~x, ~X - same as C %o, %x and %X specfiers. Length modifiers, like l, h
//        etc., are pronhibited, because size of Lisp is always known. Nor flags
//        or precision are supportted at this stage.
//   ~e, ~E - same as C %e and %E specfiers. Length modifiers, like L, are
//        pronhibited, because size of Lisp is always known. Nor flags
//        or precision are supportted at this stage.
//   ~f, ~F - same as C %f and %F specfiers. Length modifiers, like L, are
//        pronhibited, because size of Lisp is always known. Nor flags
//        or precision are supportted at this stage.
//   ~g, ~G - same as C %g and %G specfiers. Length modifiers, like L, are
//        pronhibited, because size of Lisp is always known. Nor flags
//        or precision are supportted at this stage.
//   ~c - same as C %c specfier. Nor flags or precision are supportted at this stage.
//   ~s - lisp object is printed as prin1 function. Nor flags or precision are
//        supportted at this stage.
//   ~S - lisp opject is printed as pprin1 function. Nor flags or precision are
//        supportted at this stage.
//   ~a - lisp object is printed as princ function. Nor flags or precision are
//        supportted at this stage.
//   ~~ - '~' is written. No argument is converted.
//==============================================================================
term __lisp_fprintf(term stream, term fmt_term, long nargs, const term * args);

term lisp_printf(long nargs, const term * args);
term lisp_fprintf(long nargs, const term * args);

//------------------------------------------------------------------------------
// lisp_printf_sz and lisp_fprintf_sz
//------------------------------------------------------------------------------
long lisp_vfprintf_sz(term stream, const char * fmt, va_list args);

INLINE long lisp_printf_sz(const char * fmt, ...) {
  va_list args;
  va_start(args, fmt);
  long res = lisp_vfprintf_sz(vm_get_dynamic(g_stdout_var), fmt, args);
  va_end(args);
  return res;
}

INLINE long lisp_fprintf_sz(term stream, const char * fmt, ...) {
  va_list args;
  va_start(args, fmt);
  long res = lisp_vfprintf_sz(stream, fmt, args);
  va_end(args);
  return res;
}

//------------------------------------------------------------------------------
// fast output of C strings to stream
//------------------------------------------------------------------------------
void lisp_fputs(const uint8_t * data, long size, term stream);

INLINE void lisp_fputsz(const char * str, term stream) {
  lisp_fputs((const uint8_t *)str, strlen(str), stream);
}

//-------------------------------------------------------------------
// PUT_LITERAL_STR
//-------------------------------------------------------------------
#define PUT_LITERAL_STR(str, stream)                        \
  lisp_fputs((const uint8_t *)str, sizeof(str) - 1, stream)

//==============================================================================
// Print functions
//==============================================================================
term prin1(term x, term stream);
term pprin1(term x, term stream);

term princ(term x, term stream);

INLINE term print(term x, term stream) {
  lisp_fprintf_sz(stream, "\n~s ", x);
  return x;
}

INLINE term pprint(term x, term stream) {
  lisp_fprintf_sz(stream, "\n~S ", x);
  return x;
}

//==============================================================================
// DEBUG_PRINT
//==============================================================================
#define DEBUG_PRINT(...)                        \
  do {                                          \
    vm_t * vm = vm_get_current();               \
    values_immune(vm->values);                  \
    lisp_printf_sz(__VA_ARGS__);                \
    values_unimmune(vm->values);                \
  } while (0)

//==============================================================================
// Lisp parser
//==============================================================================
//------------------------------------------------------------------------------
// Function: (read &optional (stream *stdin*) eof) ==> object
//
// Parameters: stream - stream
//             eof - object returned in case of end of file.
//
// Returns object parsed from input stream or value of eof parameter in case of
// end of file.
//------------------------------------------------------------------------------
term lisp_read(term stream, term eof);

//==============================================================================
// Numeric functions
//==============================================================================
//-------------------------------------------------------------------
// Function: (1- x) ==> number
//-------------------------------------------------------------------
term one_minus(term x);

//-------------------------------------------------------------------
// Function: (1+ x) ==> number
//-------------------------------------------------------------------
term one_plus(term x);

//------------------------------------------------------------------------------
// Function: (make-random-state seed) ==> *random-state*
// Parameters: seed - non-negative integer. Only low order 32 bits are used.
//------------------------------------------------------------------------------
term make_random_state(term seed);

//==============================================================================
// Other public C API
//==============================================================================
void lisp_global_init(int argc, const char * * argv);
void __packages_init();              // internal function
void __eval_init();                  // internal function
void __native_init();                // internal function
void __thread_init();                // internal function
term __deep_immune_literals(term x); // internal function
void __http_init();                  // internal function

//------------------------------------------------------------------------------
// Function: (intern name &key (package *package*) exported) ==> symbol
//
// Parameters: name - binary
//             package - symbol | binary | package
//             exported - bool
//
// Function intern enters a symbol named binary into package. If package is
// ommited, then symbol interned into current package. If symbol with such name
// is already exist in package, the this symbol is returned. If symbol is newly
// created, then its exporting attribute is set according to exported flag.
//------------------------------------------------------------------------------
term intern(term name, term package, term exported);

//------------------------------------------------------------------------------
// Function: (kl::resolve-symbol name) ==> symbol
//
// Parameters: name - binary
//
// Resolves symbol using reader algorithm.
// Reader's symbol resolution algorithm, when it encounters binary representation
// of symbol:
//   1. If symbol has package prefix:
//        a. If prefix is "#", create new uninterned symbol.
//        b. If prefix is empty "", assume that package is keyword package.
//        c. Look symbol in corresponding package. If symbol is found, then ensure
//        that ":" and "::" correspond to symbol's exporting attribute.
//        d. Intern symbol into package and assign exporting attribute according to
//        ":" or "::" syntax.
//   2. Look symbol in current package. If symbol is found, then it is returned.
//   3. Look symbol in exported symbols of "kl" package. If symbol is found, then
//      it is returned.
//   4. Intern not exported symbol in current package and return it.
//------------------------------------------------------------------------------
term resolve_symbol(term name);

INLINE term resolve_symbol_from_sz(const char * name) {
  return resolve_symbol(make_binary_from_sz(name));
}

INLINE term make_global_var_from_sz(const char * name, term value) {
  term sym = resolve_symbol_from_sz(name);
  symbol_bind_value(term_to_symbol(sym), value);
  return sym;
}

//==============================================================================
// hashmap
//==============================================================================
//------------------------------------------------------------------------
// Function: (hashmap-create &optional (test 'equal) (hash-code 'hash-code)) ==> hashmap
//------------------------------------------------------------------------
term hashmap_create(term test, term hash_code);

//------------------------------------------------------------------------
// Function: (hashmap-insert tbl key value &optional no-old-value) => old-value
//------------------------------------------------------------------------
term hashmap_insert(term tbl, term key, term value, term no_old_value);

//------------------------------------------------------------------------
// Function: (hashmap-lookup tbl key &optional not-found) =>  object
//------------------------------------------------------------------------
term hashmap_lookup(term tbl, term key, term not_found);

//------------------------------------------------------------------------
// Function: (hashmap-remove tbl key &optional no-old-value) => old-value
//------------------------------------------------------------------------
term hashmap_remove(term tbl, term key, term no_old_value);

//------------------------------------------------------------------------
// Function: (hashmap-size tbl) => integer
//------------------------------------------------------------------------
term hashmap_size(term tbl);

//------------------------------------------------------------------------
// Function: (hashmap-clear tbl) => tbl
//------------------------------------------------------------------------
term hashmap_clear(term tbl);

//------------------------------------------------------------------------
// Function: (hashmap-to-vector tbl &optional vec) ==> vector
// Each element of vector is cons, where car is key and cdr is value.
//------------------------------------------------------------------------
term hashmap_to_vector(term tbl, term vec);

//------------------------------------------------------------------------
// Function: (hashmap-to-list tbl) ==> list
//------------------------------------------------------------------------
term hashmap_to_list(term tbl);

//------------------------------------------------------------------------
// Function: (hashmap-do tbl fn) ==> nil
//
// Parameters: tbl - hashmap
//             fn - (lambda (key val)) ==> bool
//
// Invokes 'fn' for each key-value in hashmap, value returned by 'fn' is
// ignored.
//------------------------------------------------------------------------
term hashmap_do(term tbl, term fn);

//==============================================================================
// treemap
//==============================================================================
//------------------------------------------------------------------------
// Function: (treemap-create &optional (test 'compare)) ==> treemap
// Test is (lambda (x y)) and must return negative fixnum if x < y, zero if x = y
// and positive fixnum if x > y.
//------------------------------------------------------------------------
term treemap_create(term test);

//------------------------------------------------------------------------
// Function: (treemap-insert tbl key value &optional no-old-value) => old-value
//------------------------------------------------------------------------
term treemap_insert(term tbl, term key, term value, term no_old_value);

//------------------------------------------------------------------------
// Function: (treemap-lookup tbl key &optional not-found) =>  object
//------------------------------------------------------------------------
term treemap_lookup(term tbl, term key, term not_found);

//------------------------------------------------------------------------
// Function: (treemap-remove tbl key &optional no-old-value) => old-value
//------------------------------------------------------------------------
term treemap_remove(term tbl, term key, term no_old_value);

//------------------------------------------------------------------------
// Function: (treemap-size tbl) => integer
//------------------------------------------------------------------------
term treemap_size(term tbl);

//------------------------------------------------------------------------
// Function: (treemap-clear tbl) => tbl
//------------------------------------------------------------------------
term treemap_clear(term tbl);

//------------------------------------------------------------------------
// Function: (treemap-to-vector tbl &optional vec) ==> vector
// Each element of vector is cons, where car is key and cdr is value.
//------------------------------------------------------------------------
term treemap_to_vector(term tbl, term vec);

//------------------------------------------------------------------------
// Function: (treemap-to-list tbl) ==> list
//------------------------------------------------------------------------
term treemap_to_list(term tbl);

//------------------------------------------------------------------------
// Function: (treemap-do tbl fn) ==> nil
//
// Parameters: tbl - treemap
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in treemap, value returned by 'fn' is
// ignored.
//------------------------------------------------------------------------
term treemap_do(term tbl, term fn);

//------------------------------------------------------------------------
// Function: (treemap-do-lower-bound tbl key fn) ==> nil
//
// Parameters: tbl - treemap
//             key - object.
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in treemap, value returned by 'fn' is
// ignored.
// Traversing is started from the first element in the treemap whose key is not
// considered to go before given key (i.e., either it is equivalent or goes
// after). Returns last value returned by fn.
// ------------------------------------------------------------------------
term treemap_do_lower_bound(term tbl, term key, term fn);

//------------------------------------------------------------------------
// Function: (treemap-do-upper-bound tbl key fn) ==> nil
//
// Parameters: tbl - treemap
//             key - object.
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in treemap, value returned by 'fn' is
// ignored.
// Traversing is started from the first element in the treemap whose key is
// considered to go after given key.
// ------------------------------------------------------------------------
term treemap_do_upper_bound(term tbl, term key, term fn);

//==============================================================================
// avlmap
//==============================================================================
//------------------------------------------------------------------------
// Function: (avlmap-create &optional (test 'compare)) ==> avlmap
// Test is (lambda (x y)) and must return negative fixnum if x < y, zero if x = y
// and positive fixnum if x > y.
//------------------------------------------------------------------------
term avlmap_create(term test);

//------------------------------------------------------------------------
// Function: (avlmap-insert tbl key value &optional no-old-value) => old-value
//------------------------------------------------------------------------
term avlmap_insert(term tbl, term key, term value, term no_old_value);

//------------------------------------------------------------------------
// Function: (avlmap-lookup tbl key &optional not-found) =>  object
//------------------------------------------------------------------------
term avlmap_lookup(term tbl, term key, term not_found);

//------------------------------------------------------------------------
// Function: (avlmap-remove tbl key &optional no-old-value) => old-value
//------------------------------------------------------------------------
term avlmap_remove(term tbl, term key, term no_old_value);

//------------------------------------------------------------------------
// Function: (avlmap-size tbl) => integer
//------------------------------------------------------------------------
term avlmap_size(term tbl);

//------------------------------------------------------------------------
// Function: (avlmap-clear tbl) => tbl
//------------------------------------------------------------------------
term avlmap_clear(term tbl);

//------------------------------------------------------------------------
// Function: (avlmap-to-vector tbl &optional vec) ==> vector
// Each element of vector is cons, where car is key and cdr is value.
//------------------------------------------------------------------------
term avlmap_to_vector(term tbl, term vec);

//------------------------------------------------------------------------
// Function: (avlmap-to-list tbl) ==> list
//------------------------------------------------------------------------
term avlmap_to_list(term tbl);

//------------------------------------------------------------------------
// Function: (avlmap-do tbl fn) ==> nil
//
// Parameters: tbl - avlmap
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in avlmap, value returned by 'fn' is
// ignored.
//------------------------------------------------------------------------
term avlmap_do(term tbl, term fn);

//------------------------------------------------------------------------
// Function: (avlmap-do-lower-bound tbl key fn) ==> nil
//
// Parameters: tbl - avlmap
//             key - object.
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in avlmap, value returned by 'fn' is
// ignored.
// Traversing is started from the first element in the avlmap whose key is not
// considered to go before given key (i.e., either it is equivalent or goes
// after). Returns last value returned by fn.
// ------------------------------------------------------------------------
term avlmap_do_lower_bound(term tbl, term key, term fn);

//------------------------------------------------------------------------
// Function: (avlmap-do-upper-bound tbl key fn) ==> nil
//
// Parameters: tbl - avlmap
//             key - object.
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in avlmap, value returned by 'fn' is
// ignored.
// Traversing is started from the first element in the avlmap whose key is
// considered to go after given key.
// ------------------------------------------------------------------------
term avlmap_do_upper_bound(term tbl, term key, term fn);

//==============================================================================
// helper functions for embedded compiler macros
//==============================================================================
//-------------------------------------------------------------------
// Function: (zerop x) ==> bool
//-------------------------------------------------------------------
term zerop(term x);

//-------------------------------------------------------------------
// Function: (kl::%= x y) ==> bool
//-------------------------------------------------------------------
term lisp_num_equal_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (kl::%/= x y) ==> bool
//-------------------------------------------------------------------
term lisp_num_not_equal_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (kl::%< x y) ==> bool
//-------------------------------------------------------------------
term lisp_num_less_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (kl::%<= x y) ==> bool
//-------------------------------------------------------------------
term lisp_num_less_equal_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (kl::%> x y) ==> bool
//-------------------------------------------------------------------
term lisp_num_greater_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (kl::%>= x y) ==> bool
//-------------------------------------------------------------------
term lisp_num_greater_equal_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (kl::%+ x y)
//-------------------------------------------------------------------
term lisp_add_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (kl::%* x y)
//-------------------------------------------------------------------
term lisp_mul_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (neg x)
// Negates number
//-------------------------------------------------------------------
term lisp_neg(term x);

//-------------------------------------------------------------------
// Function: (kl::%- x y)
//-------------------------------------------------------------------
term lisp_sub_internal(term x, term y);

//-------------------------------------------------------------------
// Function: (kl::%div numerator denominator)
//-------------------------------------------------------------------
term lisp_div_int_internal(term numerator, term a);

//==============================================================================
// RESOLVER
//==============================================================================
//------------------------------------------------------------------------------
// Function: (gethostbyname name &optional (address-family AF-INET)) ==> string
//------------------------------------------------------------------------------
term lisp_gethostbyname(term name, term addr_family);

//==============================================================================
// SOCKETS
//==============================================================================
//------------------------------------------------------------------------------
// Function: (socket-close socket) ==> nil
//------------------------------------------------------------------------------
term socket_close(term sock);

//------------------------------------------------------------------------------
// Function: (socket-set-timeout socket timeout) ==> socket
//------------------------------------------------------------------------------
term socket_set_timeout(term sock, term timeout);

//------------------------------------------------------------------------------
// Function: (socket-select-read socket timeout) ==> bool
//------------------------------------------------------------------------------
term socket_select_read(term sock, term timeout);

//------------------------------------------------------------------------------
// Function: (socket-select-write socket timeout) ==> bool
//------------------------------------------------------------------------------
term socket_select_write(term sock, term timeout);

//------------------------------------------------------------------------------
// Function: (socket-accept socket) ==> socket or nil
// Returns nil if socket timeout is expired
//------------------------------------------------------------------------------
term socket_accept(term sock);

//------------------------------------------------------------------------------
// Function: (socket-peer-address socket) ==> (values ip-string port-num)
//------------------------------------------------------------------------------
term socket_peer_address(term sock);

//==============================================================================
// TCP specific functions
//==============================================================================
//------------------------------------------------------------------------------
// Function: (tcp-connect host port &key timeout nodelay
//                        bind-addr bind-port) ==> socket
// timeout becomes socket operations timeout.
//------------------------------------------------------------------------------
term tcp_connect(term host, term port, term timeout, term nodelay,
                 term bind_addr, term bind_port);

//------------------------------------------------------------------------------
// Function: (tcp-listen port &key bind_addr reuse-address (backlog 5) timeout) ==> socket
// timeout becomes socket operations timeout.
//------------------------------------------------------------------------------
term tcp_listen(term port, term bind_addr, term reuse_address, term backlog,
                term timeout);

//==============================================================================
// SOCKET STREAM
//==============================================================================
//------------------------------------------------------------------------------
// Function: (make-socket-stream socket &optional timeout name) ==> stream
//------------------------------------------------------------------------------
term make_socket_stream(term sock, term timeout, term name);

//==============================================================================
// FILE SYSTEM
//==============================================================================
//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
INLINE int is_absoulte_path(const binary_t * s) {
  if (s->size == 0) {
    lisp_signal(g_invalid_arg, __pointer_to_term(s));
  }
  return s->data[0] == '/';
}

//==============================================================================
// REGULAR EXPRESSIONS
//==============================================================================
//------------------------------------------------------------------------------
// Function: (regcomp pattern &key extented icase nosub newline) => regex
//------------------------------------------------------------------------------
term lisp_regcomp(term pattern, term extended, term icase, term nosub, term newline);

//------------------------------------------------------------------------------
// Function: (regexec regex str &key (str-offset 0) nmatch not-bol not-eol) ==> bool
// If nmatch is not null, then list of matches with maximal length 'nmatch' is
// returned. Each match is described as pair (start-offset . end-offset).
//------------------------------------------------------------------------------
term lisp_regexec(term regex, term str, term str_offset, term nmatch, term not_bol,
                  term not_eol);

//------------------------------------------------------------------------------
// extern "C"
//------------------------------------------------------------------------------
#ifdef __cplusplus
}
#endif

//------------------------------------------------------------------------------
// C++
//------------------------------------------------------------------------------
#ifdef __cplusplus

#include <new> // for placement new

//------------------------------------------------------------------------------
// GC allocator for std classes
//------------------------------------------------------------------------------
struct gc_true_type {};
struct gc_false_type {};

template <class gc_tp>
struct gc_type_traits {
  gc_false_type gc_is_ptr_free;
};

# define GC_DECLARE_PTRFREE(T) \
template<> struct gc_type_traits<T> { gc_true_type gc_is_ptr_free; }

GC_DECLARE_PTRFREE(char);
GC_DECLARE_PTRFREE(signed char);
GC_DECLARE_PTRFREE(unsigned char);
GC_DECLARE_PTRFREE(signed short);
GC_DECLARE_PTRFREE(unsigned short);
GC_DECLARE_PTRFREE(signed int);
GC_DECLARE_PTRFREE(unsigned int);
GC_DECLARE_PTRFREE(signed long);
GC_DECLARE_PTRFREE(unsigned long);
GC_DECLARE_PTRFREE(signed long long);
GC_DECLARE_PTRFREE(unsigned long long);
GC_DECLARE_PTRFREE(float);
GC_DECLARE_PTRFREE(double);
GC_DECLARE_PTRFREE(long double);
/* The client may want to add others.   */

// In the following gc_tp is gc_true_type if we are allocating a
// pointer-free object.
template <class gc_tp>
inline void * gc_selective_alloc(size_t n, gc_tp) {
  return lisp_alloc(n, NULL);
}

template <>
inline void * gc_selective_alloc<gc_true_type>(size_t n, gc_true_type) {
  return lisp_alloc_atomic(n, NULL);
}

/* Now the public gc_allocator<T> class:
 */
template <class gc_tp>
class gc_allocator {
public:
  typedef size_t        size_type;
  typedef ptrdiff_t     difference_type;
  typedef gc_tp *       pointer;
  typedef const gc_tp * const_pointer;
  typedef gc_tp &       reference;
  typedef const gc_tp & const_reference;
  typedef gc_tp         value_type;

  template <class gc_tp1> struct rebind {
    typedef gc_allocator<gc_tp1> other;
  };

  gc_allocator()  {}
  gc_allocator(const gc_allocator&) throw() {}
  template <class gc_tp1> gc_allocator(const gc_allocator<gc_tp1>&) throw() {}
  ~gc_allocator() throw() {}

  pointer address(reference gc_x) const { return &gc_x; }
  const_pointer address(const_reference gc_x) const { return &gc_x; }

  // gc_n is permitted to be 0.  The C++ standard says nothing about what
  // the return value is when gc_n == 0.
  gc_tp* allocate(size_type gc_n, const void* = 0) {
    gc_type_traits<gc_tp> traits;
    return static_cast<gc_tp *>
      (gc_selective_alloc(gc_n * sizeof(gc_tp), traits.gc_is_ptr_free));
  }

  // __p is not permitted to be a null pointer.
  void deallocate(pointer __p, size_type gc_n) { }

  size_type max_size() const throw() {
    return size_t(-1) / sizeof(gc_tp);
  }

  void construct(pointer __p, const gc_tp & __val) { new(__p) gc_tp(__val); }
  void destroy(pointer __p) { __p->~gc_tp(); }
};

template<>
class gc_allocator<void> {
  typedef size_t        size_type;
  typedef ptrdiff_t     difference_type;
  typedef void *        pointer;
  typedef const void *  const_pointer;
  typedef void          value_type;

  template <class gc_tp1> struct rebind {
    typedef gc_allocator<gc_tp1> other;
  };
};

template <class gc_t1, class gc_t2>
inline bool operator==(const gc_allocator<gc_t1> &, const gc_allocator<gc_t2> &) {
  return true;
}

template <class gc_t1, class gc_t2>
inline bool operator!=(const gc_allocator<gc_t1> &, const gc_allocator<gc_t2> &) {
  return false;
}

//------------------------------------------------------------------------------
// GC new and delete operators
//------------------------------------------------------------------------------
enum gc_e {
  GC
};

enum gc_atomic_e {
  GC_ATOMIC
};

inline void * operator new(size_t size, gc_e) {
  return lisp_alloc(size, NULL);
}

inline void * operator new(size_t size, gc_atomic_e) {
  return lisp_alloc_atomic(size, NULL);
}

inline void * operator new[](size_t size, gc_e) {
  return lisp_alloc(size, NULL);
}

inline void * operator new[](size_t size, gc_atomic_e) {
  return lisp_alloc_atomic(size, NULL);
}

#endif  // __cplusplus

#endif // __klisp_h__
