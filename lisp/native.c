///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : native.c
/// Author  : Evgeny Khirin <>
/// Description : Builtin Lisp functions.
///-----------------------------------------------------------------------------

#include "klisp.h"

#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <float.h>
#include <ieee754.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/select.h>

#include <sys/socket.h>

#include <capstone/capstone.h>             // X86 disassembler

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
static term g_null = nil;             // 'null
static term g_fixnum = nil;           // 'fixnum
static term g_bigint = nil;           // 'bigint
static term g_double = nil;           // 'double
static term g_symbol = nil;           // 'symbol
static term g_binary = nil;           // 'binary
static term g_ustring = nil;          // 'ustring
static term g_cons = nil;             // 'cons
static term g_vector = nil;           // 'vector
static term g_lambda = nil;           // 'lambda
static term g_macro = nil;            // 'macro
static term g_custom = nil;           // 'custom

static term g_double_zero = nil;
static term g_positive_fixnum_min = nil;

static mutex_t           g_symbols_macros_mutex = MUTEX_INITIALIZER;
static term              g_symbols_macros_tbl = nil;
static mutex_t           g_compiler_macros_mutex = MUTEX_INITIALIZER;
static term              g_compiler_macros_tbl = nil;

// *load-pathname*
static term g_load_pathname = nil;

//------------------------------------------------------------------------------
// Special form: (signal label &optional value)
//------------------------------------------------------------------------------
term lisp_signal(term label, term value) {
  vm_t * vm = vm_get_current();
  sig_frame_t * f = vm->sig_frame;
  term args[3] = {nil, label, value};
  while (f != NULL) {
    switch (f->kind) {
    case sig_frame_lisp_handler_e:
      args[0] = ((sig_lisp_handler_frame_t *) f)->handler;
      funcall(3, args);
      break;
    case sig_frame_c_handler_e:
      ((sig_c_handler_frame_t *) f)->handler(label, value);
      break;
    default:
      ;
    }
    f = f->next;
  }
  // Signal is not handled - it is bug in program, because each thread sets up
  // signal handler. Abort program.
  abort();
  // something is defenetly wrong - loop infinitly
  while (1) {
  }
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
void lisp_signal_system_error() {
  int err = errno;
  // if (err == 4) {
  //   abort();
  // }
  char buf[128];
#ifdef _GNU_SOURCE
  const char * p = strerror_r(err, buf, sizeof(buf));
  lisp_signal(g_system_error,
              LIST_2(long_to_term(err), make_binary_from_sz(p)));
#elif defined _POSIX_SOURCE
  if (strerror_r(err, buf, sizeof(buf)) == 0) {
    lisp_signal(g_system_error,
                LIST_2(long_to_term(err), make_binary_from_sz(buf)));
  } else {
    lisp_signal(g_system_error, long_to_term(err));
  }
#else
#error "Nor _GNU_SOURCE or _POSIX_SOURCE macros are provided"
#endif
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void unwind_stack(catch_frame_t * target, sig_frame_t * f) {
  vm_t * vm = vm_get_current();
  values_t * values = vm->values;
  values_immune(vm->values);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)values_unimmune, vm->values) {
    while (f != (sig_frame_t *)target) {
      if (f->kind == sig_frame_c_unwind_e) {
        c_unwind_frame_t * c = (c_unwind_frame_t *)f;
        if (!c->done) {
          c->done = 1;
          c->callback(c->arg);
        }
      } else if (f->kind == sig_frame_lisp_unwind_e) {
        lisp_unwind_frame_t * c = (lisp_unwind_frame_t *)f;
        if (!c->done) {
          c->done = 1;
          c->callback(c->env, c->params, c->local_vars);
        }
      }
      f = f->next;
    }
  } UNWIND_PROTECT_END;
  if (values != vm->values) {
    values_copy(values, vm->values);
  }
}

//------------------------------------------------------------------------------
// Special form: (throw tag &optional value)
//------------------------------------------------------------------------------
term lisp_throw(term tag, term value) {
  vm_t * vm = vm_get_current();
  // check if there is suitable handler
  sig_frame_t * f = vm->sig_frame;
  while (f != NULL) {
    if (f->kind == sig_frame_catch_e) {
      catch_frame_t * c = (catch_frame_t *) f;
      if (eq(tag, c->tag)) {
        // there is handler
        unwind_stack(c, vm->sig_frame);
        c->value = value;
        siglongjmp(c->sigbuf, __long_to_fixnum_term(1));
      }
    }
    f = f->next;
  }
  // there is no handler, signal error
  lisp_signal(g_no_catch, LIST_2(tag, value));
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
void lisp_error_sz(term label, const char * fmt, ...) {
  vm_t * vm = vm_get_current();
  if (is_null(vm->error_stream)) {
    vm->error_stream = make_binary_stream(make_binary());
  } else {
    binary_stream_rewind(vm->error_stream, g_true, nil);
  }
  va_list args;
  va_start(args, fmt);
  lisp_vfprintf_sz(vm->error_stream, fmt, args);
  va_end(args);
  lisp_signal(label, binary_stream_content(vm->error_stream));
}

//------------------------------------------------------------------------------
// Function: (error fmt &rest args)
//------------------------------------------------------------------------------
term lisp_error(long nargs, const term * args) {
  if (nargs < 2) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  vm_t * vm = vm_get_current();
  if (is_null(vm->error_stream)) {
    vm->error_stream = make_binary_stream(make_binary());
  } else {
    binary_stream_rewind(vm->error_stream, g_true, nil);
  }
  __lisp_fprintf(vm->error_stream, args[1], nargs - 2, args + 2);
  lisp_signal(args[0], binary_stream_content(vm->error_stream));
}

//------------------------------------------------------------------------------
// Function: (type-of x)
// Returns symbol corresponding to type of x.
//------------------------------------------------------------------------------
static term lisp_type_of(term x) {
  switch (get_term_type(x)) {
  case null_e:
    return g_null;
  case fixnum_e:
    return g_fixnum;
  case bigint_e:
    return g_bigint;
  case double_e:
    return g_double;
  case symbol_e:
    return g_symbol;
  case lambda_e:
    return g_lambda;
  case macro_e:
    return g_macro;
  case binary_e:
    return g_binary;
  case ustring_e:
    return g_ustring;
  case cons_e:
    return g_cons;
  case vector_e:
    return g_vector;
  case custom_e:
    return g_custom;
  }
  SIGNAL_INTERNAL_ERROR();
}

//------------------------------------------------------------------------------
// Function: (eql x y) ==> bool
//------------------------------------------------------------------------------
static term lisp_eql(term x, term y) {
  if (eql(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (equal x y) ==> bool
//------------------------------------------------------------------------------
static term lisp_equal(term x, term y) {
  if (equal(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (compare x y) ==> integer
//------------------------------------------------------------------------------
static term compare(term x, term y) {
  long res = term_cmp(x, y);
  if (res < 0) {
    return __long_to_fixnum_term(-1);
  }
  if (res > 0) {
    return __long_to_fixnum_term(1);
  }
  return __long_to_fixnum_term(0);
}

//------------------------------------------------------------------------------
// Function: (def-global-var sym val) ==> sym
//------------------------------------------------------------------------------
term def_global_var(term sym, term val) {
  if (!eq(symbol_macro(sym, g_unbound_marker), g_unbound_marker)) {
    lisp_fprintf_sz(vm_get_dynamic(g_stderr_var),
                    "\nWARNING: global variable ~s conflicts with symbol macro",
                    sym);
  }
  symbol_t * s = term_to_symbol(sym);
  symbol_bind_value(s, val);
  return sym;
}

//------------------------------------------------------------------------------
// Function: (undef-global-var sym) ==> object
//------------------------------------------------------------------------------
term undef_global_var(term sym) {
  symbol_t * s = term_to_symbol(sym);
  term val = symbol_get_value(s);
  symbol_unbind_value(s);
  return val;
}

//------------------------------------------------------------------------------
// Function: (def-global-fun sym fn) ==> sym
//------------------------------------------------------------------------------
term def_global_fun(term sym, term fn) {
  symbol_t * s = term_to_symbol(sym);
  function_t * lfn = term_to_function(fn);
  if (is_null(lfn->name)) {
    lfn->name = sym;
  }
  symbol_bind_function(s, fn, lfn);
  return sym;
}

//------------------------------------------------------------------------------
// Function: (undef-global-fun sym) ==> sym
//------------------------------------------------------------------------------
static term undef_global_fun(term sym) {
  symbol_t * s = term_to_symbol(sym);
  if (!symbol_is_function_bound(s)) {
    lisp_signal(g_unbound_function, sym);
  }
  symbol_unbind_function(s);
  return sym;
}

//------------------------------------------------------------------------------
// Function: (null x) ==> bool
//------------------------------------------------------------------------------
static term lisp_is_null(term x) {
  if (is_null(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (fixnump x) ==> bool
//------------------------------------------------------------------------------
static term fixnump(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    return g_true;
  default:
    return nil;
  }
}

//------------------------------------------------------------------------------
// Function: (bigintp x) ==> bool
//------------------------------------------------------------------------------
static term bigintp(term x) {
  switch (get_term_type(x)) {
  case bigint_e:
    return g_true;
  default:
    return nil;
  }
}

//------------------------------------------------------------------------------
// Function: (integerp x) ==> bool
//------------------------------------------------------------------------------
static term integerp(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
    return g_true;
  default:
    return nil;
  }
}

//------------------------------------------------------------------------------
// Function: (doublep x) ==> bool
//------------------------------------------------------------------------------
static term doublep(term x) {
  if (is_double(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (numberp x) ==> bool
// Returns t if x is either integer or float.
//------------------------------------------------------------------------------
static term numberp(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
  case double_e:
    return g_true;
  default:
    return nil;
  }
}

//------------------------------------------------------------------------------
// Function: (finitep x) ==> bool
//------------------------------------------------------------------------------
static term finitep(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
    return g_true;
  case double_e:
    if (isfinite(__term_to_double(x))) {
      return g_true;
    }
    return nil;
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (infinitep x) ==> bool
//------------------------------------------------------------------------------
static term infinitep(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
    return nil;
  case double_e:
    if (isinf(__term_to_double(x))) {
      return g_true;
    }
    return nil;
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (nanp x) ==> bool
//------------------------------------------------------------------------------
static term nanp(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
    return nil;
  case double_e:
    if (isnan(__term_to_double(x))) {
      return g_true;
    }
    return nil;
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (make-random-state seed) ==> random-state
// Parameters: seed - non-negative integer. Only low order 32 bits are used.
//------------------------------------------------------------------------------
term make_random_state(term seed) {
  term state = make_vector();
  vector_t * v = get_vector_for_write(state);
  vector_ensure_capacity(v, 3);
  // set the high order 32-bits of Xi to the argument seed.  The low order
  // 16-bits are set to the arbitrary value 0x330E.
  uint32_t s = term_to_uint64(seed);
  v->data[0] = uint16_to_term(0x330E);
  v->data[1] = uint16_to_term(s & 0xFFFF);
  v->data[2] = uint16_to_term(s >> 16);
  v->size = 3;
  return state;
}

//------------------------------------------------------------------------------
// Function: (random limit) ==> integer in range [0, limit)
// Parameters: limit - fixnum
//------------------------------------------------------------------------------
static term lisp_random(term limit) {
  term state = vm_get_dynamic(g_random_state_var);
  vector_t * v = get_vector_for_write(state);
  if (v->size != 3) {
    lisp_signal(g_invalid_random_state, state);
  }
  uint64_t s = (term_to_uint64(v->data[2]) << 32) |
    (term_to_uint64(v->data[1]) << 16) | term_to_uint64(v->data[0]);
  // compute next state. Since only 48-bits are actualy used, so "mod m"
  // operation may be ommited.
  s = s * UINT64_C(0x5DEECE66D) + 0xB;
  uint16_t x[3];
  x[0] = s;
  x[1] = s >> 16;
  x[2] = s >> 32;
  // Store next state
  v->data[0] = uint16_to_term(x[0]);
  v->data[1] = uint16_to_term(x[1]);
  v->data[2] = uint16_to_term(x[2]);
  // Construct a positive double with the 48 random bits distributed over its
  // fractional part so the resulting FP number is [0.0,1.0).
  union ieee754_double d;
  d.ieee.negative = 0;
  d.ieee.exponent = IEEE754_DOUBLE_BIAS;
  d.ieee.mantissa0 = (x[2] << 4) | (x[1] >> 12);
  d.ieee.mantissa1 = ((x[1] & 0xfff) << 20) | (x[0] << 4);
  // Please note the lower 4 bits of mantissa1 are always 0.
  return __long_to_fixnum_term(__fixnum_term_to_long(limit) * (d.d - 1.0));
}

//------------------------------------------------------------------------------
// Function: (symbolp x)
//------------------------------------------------------------------------------
static term symbolp(term x) {
  if (is_symbol(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (binaryp x) ==> bool
//------------------------------------------------------------------------------
static term binaryp(term x) {
  if (is_binary(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (ustringp x)
//------------------------------------------------------------------------------
static term ustringp(term x) {
  if (is_ustring(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (consp x) ==> bool
//------------------------------------------------------------------------------
static term consp(term x) {
  if (is_cons(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (vectorp x)
//------------------------------------------------------------------------------
static term vectorp(term x) {
  if (is_vector(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (macrop x) ==> bool
//------------------------------------------------------------------------------
static term macrop(term x) {
  if (is_macro(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (lambdap x)
//------------------------------------------------------------------------------
static term lambdap(term x) {
  if (is_lambda(x)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (closurep x)
//------------------------------------------------------------------------------
static term closurep(term fn) {
  if (is_closure(term_to_function(fn))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (ucharp x) ==> bool
//------------------------------------------------------------------------------
static term ucharp(term x) {
  if (!is_fixnum(x)) {
    return nil;
  }
  long c = __fixnum_term_to_long(x);
  if ((unsigned long)c > UCHAR_CODE_LIMIT) {
    return nil;
  }
  uint32_t c32 = c;
  if (u32_check(&c32, 1) != NULL) {
    return nil;
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (char-alphap x)
//------------------------------------------------------------------------------
static term char_alphap(term x) {
  if (isalpha(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-alphap x)
//------------------------------------------------------------------------------
static term uchar_alphap(term x) {
  if (uc_is_alpha(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-alphanumericp x)
//------------------------------------------------------------------------------
static term char_alphanumericp(term x) {
  if (isalnum(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-alphanumericp x)
//------------------------------------------------------------------------------
static term uchar_alphanumericp(term x) {
  if (uc_is_alnum(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-controlp x)
//------------------------------------------------------------------------------
static term char_controlp(term x) {
  if (iscntrl(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-controlp x)
//------------------------------------------------------------------------------
static term uchar_controlp(term x) {
  if (uc_is_cntrl(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-digitp x)
//------------------------------------------------------------------------------
static term char_digitp(term x) {
  if (isdigit(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-digitp x)
//------------------------------------------------------------------------------
static term uchar_digitp(term x) {
  if (uc_is_digit(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-graphp x)
//------------------------------------------------------------------------------
static term char_graphp(term x) {
  if (isgraph(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-graphp x)
//------------------------------------------------------------------------------
static term uchar_graphp(term x) {
  if (uc_is_graph(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-lowerp x)
//------------------------------------------------------------------------------
static term char_lowerp(term x) {
  if (islower(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-lowerp x)
//------------------------------------------------------------------------------
static term uchar_lowerp(term x) {
  if (uc_is_lower(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-printp x)
//------------------------------------------------------------------------------
static term char_printp(term x) {
  if (isprint(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-printp x)
//------------------------------------------------------------------------------
static term uchar_printp(term x) {
  if (uc_is_print(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-punctp x)
//------------------------------------------------------------------------------
static term char_punctp(term x) {
  if (ispunct(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-punctp x)
//------------------------------------------------------------------------------
static term uchar_punctp(term x) {
  if (uc_is_punct(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-spacep x)
//------------------------------------------------------------------------------
static term char_spacep(term x) {
  if (isspace(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-spacep x)
//------------------------------------------------------------------------------
static term uchar_spacep(term x) {
  if (uc_is_space(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-upperp x)
//------------------------------------------------------------------------------
static term char_upperp(term x) {
  if (isupper(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-upperp x)
//------------------------------------------------------------------------------
static term uchar_upperp(term x) {
  if (uc_is_upper(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-xdigitp x)
//------------------------------------------------------------------------------
static term char_xdigitp(term x) {
  if (isxdigit(term_to_uint8(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (uchar-xdigitp x)
//------------------------------------------------------------------------------
static term uchar_xdigitp(term x) {
  if (uc_is_xdigit(term_to_uchar(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (char-to-lower x)
//------------------------------------------------------------------------------
static term char_to_lower(term x) {
  return uint8_to_term(tolower(term_to_uint8(x)));
}

//------------------------------------------------------------------------------
// Function: (char-to-upper x)
//------------------------------------------------------------------------------
static term char_to_upper(term x) {
  return uint8_to_term(toupper(term_to_uint8(x)));
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int num_less(term x, term y) {
  if (!is_number(x)) {
    lisp_signal(g_invalid_arg, x);
  }
  if (!is_number(y)) {
    lisp_signal(g_invalid_arg, y);
  }
  return term_cmp(x, y) < 0;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int num_greater_equal(term x, term y) {
  return !num_less(x, y);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int num_greater(term x, term y) {
  return num_less(y, x);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int num_less_equal(term x, term y) {
  return !num_greater(x, y);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int num_equal(term x, term y) {
  if (!is_number(x)) {
    lisp_signal(g_invalid_arg, x);
  }
  if (!is_number(y)) {
    lisp_signal(g_invalid_arg, y);
  }
  return term_cmp(x, y) == 0;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int num_not_equal(term x, term y) {
  return !num_equal(x, y);
}

//------------------------------------------------------------------------------
// Function: (= num &rest args) ==> bool
//------------------------------------------------------------------------------
static term lisp_num_equal(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  term num = *args++;
  --nargs;
  long i;
  for (i = 0; i < nargs; ++i) {
    if (num_not_equal(num, args[i])) {
      return nil;
    }
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (kl::%= x y) ==> bool
//------------------------------------------------------------------------------
term lisp_num_equal_internal(term x, term y) {
  if (num_equal(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int num_compare(const void * p1, const void * p2) {
  term x = *(const term *)p1;
  term y = *(const term *)p2;
  if (!is_number(x)) {
    lisp_signal(g_invalid_arg, x);
  }
  if (!is_number(y)) {
    lisp_signal(g_invalid_arg, y);
  }
  return term_cmp(x, y);
}

//------------------------------------------------------------------------------
// Function: (/= num &rest args) ==> bool
//------------------------------------------------------------------------------
static term lisp_num_not_equal(long nargs, const term * args) {
  // NUmbers must be sorted before comparison
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  if (nargs == 1) {
    if (!is_number(*args)) {
      lisp_signal(g_invalid_arg, *args);
    }
    return g_true;
  }
  if (nargs == 2) {
    if (num_equal(args[0], args[1])) {
      return nil;
    }
    return g_true;
  }
  vector_t a;
  vector_init(&a);
  vector_ensure_capacity(&a, nargs);
  long i;
  for (i = 0; i < nargs; ++i) {
    a.data[i] = args[i];
  }
  a.size = nargs;
  qsort(a.data, nargs, sizeof(term), num_compare);
  long end = nargs - 1;
  for (i = 0; i < end; ++i) {
    if (num_equal(a.data[i], a.data[i + 1])) {
      return nil;
    }
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (kl::%/= x y) ==> bool
//------------------------------------------------------------------------------
term lisp_num_not_equal_internal(term x, term y) {
  if (num_not_equal(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (< num &rest args) ==> bool
//------------------------------------------------------------------------------
static term lisp_num_less(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  term num = *args++;
  --nargs;
  long i;
  for (i = 0; i < nargs; ++i) {
    if (!num_less(num, args[i])) {
      return nil;
    }
    num = args[i];
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (kl::%< x y) ==> bool
//------------------------------------------------------------------------------
term lisp_num_less_internal(term x, term y) {
  if (num_less(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (<= num &rest args) ==> bool
//------------------------------------------------------------------------------
static term lisp_num_less_equal(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  term num = *args++;
  --nargs;
  long i;
  for (i = 0; i < nargs; ++i) {
    if (!num_less_equal(num, args[i])) {
      return nil;
    }
    num = args[i];
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (kl::%<= x y) ==> bool
//------------------------------------------------------------------------------
term lisp_num_less_equal_internal(term x, term y) {
  if (num_less_equal(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (> num &rest args) ==> bool
//------------------------------------------------------------------------------
static term lisp_num_greater(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  term num = *args++;
  --nargs;
  long i;
  for (i = 0; i < nargs; ++i) {
    if (!num_greater(num, args[i])) {
      return nil;
    }
    num = args[i];
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (kl::%> x y) ==> bool
//------------------------------------------------------------------------------
term lisp_num_greater_internal(term x, term y) {
  if (num_greater(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (>= num &rest args) ==> bool
//------------------------------------------------------------------------------
static term lisp_num_greater_equal(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  term num = *args++;
  --nargs;
  long i;
  for (i = 0; i < nargs; ++i) {
    if (!num_greater_equal(num, args[i])) {
      return nil;
    }
    num = args[i];
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (kl::%>= x y) ==> bool
//------------------------------------------------------------------------------
term lisp_num_greater_equal_internal(term x, term y) {
  if (num_greater_equal(x, y)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (zerop x) ==> bool
//------------------------------------------------------------------------------
term zerop(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    if (x == __long_to_fixnum_term(0)) {
      return g_true;
    }
    return nil;
  case bigint_e:
    // bigint never may be 0
    return nil;
  case double_e:
    if (__term_to_double(x) == 0) {
      return g_true;
    }
    return nil;
  default:
    return lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (minusp x) ==> bool
//------------------------------------------------------------------------------
static term minusp(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    if (x < 0) {
      return g_true;
    }
    return nil;
  case bigint_e:
    if (mpz_sgn(__term_to_bigint(x)->mpz) < 0) {
      return g_true;
    }
    return nil;
  case double_e:
    if (__term_to_double(x) < 0) {
      return g_true;
    }
    return nil;
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (plusp x) ==> bool
//------------------------------------------------------------------------------
static term plusp(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    if (x > 0) {
      return g_true;
    }
    return nil;
  case bigint_e:
    if (mpz_sgn(__term_to_bigint(x)->mpz) > 0) {
      return g_true;
    }
    return nil;
  case double_e:
    if (__term_to_double(x) > 0) {
      return g_true;
    }
    return nil;
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (1+ x) ==> number
//------------------------------------------------------------------------------
term one_plus(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    if (x == __long_to_fixnum_term(FIXNUM_MAX)) {
      // overflow
      return __long_to_bigint_term(FIXNUM_MAX + 1);
    }
    return  x + __long_to_fixnum_term(1);
  case bigint_e:
    {
      const bigint_t * bx = __term_to_bigint(x);
      bigint_t * res = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
      bigint_init(res);
      mpz_add_ui(res->mpz, bx->mpz, 1);
      if (mpz_fits_slong_p(res->mpz) && mpz_cmp_si(res->mpz, FIXNUM_MIN) >= 0) {
        return __long_to_fixnum_term(mpz_get_si(res->mpz));
      }
      return __pointer_to_term(res);
    }
  case double_e:
    return double_to_term(__term_to_double(x) + 1);
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (1- x) ==> bool
//------------------------------------------------------------------------------
term one_minus(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    if (x == __long_to_fixnum_term(FIXNUM_MIN)) {
      // overflow
      return __long_to_bigint_term(FIXNUM_MIN - 1);
    }
    return  x - __long_to_fixnum_term(1);
  case bigint_e:
    {
      const bigint_t * bx = __term_to_bigint(x);
      bigint_t * res = (bigint_t *)lisp_alloc(sizeof(bigint_t), NULL);
      bigint_init(res);
      mpz_sub_ui(res->mpz, bx->mpz, 1);
      if (mpz_fits_slong_p(res->mpz) && mpz_cmp_si(res->mpz, FIXNUM_MAX) <= 0) {
        return __long_to_fixnum_term(mpz_get_si(res->mpz));
      }
      return __pointer_to_term(res);
    }
  case double_e:
    return double_to_term(__term_to_double(x) - 1);
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (kl::%+ x y)
//------------------------------------------------------------------------------
term lisp_add_internal(term x, term y) {
  switch (get_term_type(x)) {
  case fixnum_e:
    switch (get_term_type(y)) {
    case fixnum_e:
      return long_to_term(__fixnum_term_to_long(x) + __fixnum_term_to_long(y));
    case bigint_e:
      {
        term bt = __long_to_bigint_term(__fixnum_term_to_long(x));
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        mpz_add(br->mpz, br->mpz, __term_to_bigint(y)->mpz);
        return bt;
      }
    case double_e:
      return double_to_term(__term_to_double(y) + __fixnum_term_to_long(x));
    default:
      lisp_signal(g_invalid_arg, y);
    }
    break;

  case bigint_e:
    switch (get_term_type(y)) {
    case fixnum_e:
      {
        term bt = __bigint_to_bigint_term(__term_to_bigint(x));
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        long y1 = __fixnum_term_to_long(y);
        if (y1 > 0) {
          mpz_add_ui(br->mpz, br->mpz, y1);
        } else {
          mpz_sub_ui(br->mpz, br->mpz, -y1);
        }
        return bt;
      }
    case bigint_e:
      {
        term bt = __bigint_to_bigint_term(__term_to_bigint(x));
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        mpz_add(br->mpz, br->mpz, __term_to_bigint(y)->mpz);
        return bt;
      }
    case double_e:
      {
        term bt = __bigint_to_bigint_term(__term_to_bigint(x));
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        return double_to_term(__term_to_double(y) + mpz_get_d(br->mpz));
      }
    default:
      lisp_signal(g_invalid_arg, y);
    }
    break;

  case double_e:
    switch (get_term_type(y)) {
    case fixnum_e:
      return double_to_term(__term_to_double(x) + __fixnum_term_to_long(y));
    case bigint_e:
      return double_to_term(__term_to_double(x) + mpz_get_d(__term_to_bigint(y)->mpz));
    case double_e:
      return double_to_term(__term_to_double(x) + __term_to_double(y));
    default:
      lisp_signal(g_invalid_arg, y);
    }
    break;

  default:
    lisp_signal(g_invalid_arg, x);
  }
  SIGNAL_INTERNAL_ERROR();
}

//------------------------------------------------------------------------------
// Function: (+ &rest args)
//------------------------------------------------------------------------------
static term lisp_add(long nargs, const term * args) {
  term_type_e rtp = fixnum_e;
  long fr = 0;
  term bt = nil;                      // bigint term
  bigint_t * br = NULL;
  double dr = 0;
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (rtp) {
    case fixnum_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          long x = __fixnum_term_to_long(a);
          long s = fr + x;
          // check overflow
          if (s >= FIXNUM_MIN && s <= FIXNUM_MAX) {
            fr = s;
          } else {
            rtp = bigint_e;
            bt = __long_to_bigint_term(fr);
            br = (bigint_t *)__term_to_bigint(bt);
            if (x > 0) {
              mpz_add_ui(br->mpz, br->mpz, x);
            } else {
              mpz_sub_ui(br->mpz, br->mpz, -x);
            }
          }
        }
        break;
      case bigint_e:
        rtp = bigint_e;
        bt = __long_to_bigint_term(fr);
        br = (bigint_t *)__term_to_bigint(bt);
        mpz_add(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      case double_e:
        rtp = double_e;
        dr = __term_to_double(a) + fr;
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case bigint_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          long x = __fixnum_term_to_long(a);
          if (x > 0) {
            mpz_add_ui(br->mpz, br->mpz, x);
          } else {
            mpz_sub_ui(br->mpz, br->mpz, -x);
          }
        }
        break;
      case bigint_e:
        mpz_add(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      case double_e:
        rtp = double_e;
        dr = __term_to_double(a) + mpz_get_d(br->mpz);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case double_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        dr += __fixnum_term_to_long(a);
        break;
      case bigint_e:
        dr += mpz_get_d(__term_to_bigint(a)->mpz);
        break;
      case double_e:
        dr += __term_to_double(a);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    default:
      SIGNAL_INTERNAL_ERROR();
    }
  }
  switch (rtp) {
  case fixnum_e:
    return __long_to_fixnum_term(fr);
  case bigint_e:
    if (mpz_fits_slong_p(br->mpz) &&
        mpz_cmp_si(br->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(br->mpz, FIXNUM_MAX) <= 0) {
      return __long_to_fixnum_term(mpz_get_si(br->mpz));
    }
    return bt;
  case double_e:
    return double_to_term(dr);
  default:
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// Function: (%neg x)
// Negates number
//------------------------------------------------------------------------------
term lisp_neg(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    {
      long v = __fixnum_term_to_long(x);
      if (v == FIXNUM_MIN) {
        return g_positive_fixnum_min;
      }
      return __long_to_fixnum_term(-v);
    }
  case bigint_e:
    {
      term r = __long_to_bigint_term(0);
      mpz_neg(((bigint_t *)__term_to_bigint(r))->mpz, __term_to_bigint(x)->mpz);
      return r;
    }
  case double_e:
    return double_to_term(-__term_to_double(x));
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (kl::%- x y)
//------------------------------------------------------------------------------
term lisp_sub_internal(term x, term y) {
  switch (get_term_type(x)) {
  case fixnum_e:
    {
      long fr = __fixnum_term_to_long(x);
      switch (get_term_type(y)) {
      case fixnum_e:
        {
          long y1 = __fixnum_term_to_long(y);
          long s = fr - y1;
          // check overflow
          if (s >= FIXNUM_MIN && s <= FIXNUM_MAX) {
            return __long_to_fixnum_term(s);
          }
          term bt = __long_to_bigint_term(fr);
          bigint_t * br = (bigint_t *)__term_to_bigint(bt);
          if (y1 > 0) {
            mpz_sub_ui(br->mpz, br->mpz, y1);
          } else {
            mpz_add_ui(br->mpz, br->mpz, -y1);
          }
          return bt;
        }
      case bigint_e:
        {
          term bt = __long_to_bigint_term(fr);
          bigint_t * br = (bigint_t *)__term_to_bigint(bt);
          mpz_sub(br->mpz, br->mpz, __term_to_bigint(y)->mpz);
          return bt;
        }
      case double_e:
        return double_to_term(fr - __term_to_double(y));
      default:
        lisp_signal(g_invalid_arg, y);
      }
    }

  case bigint_e:
    switch (get_term_type(y)) {
    case fixnum_e:
      {
        term bt = __bigint_to_bigint_term(__term_to_bigint(x));
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        long y1 = __fixnum_term_to_long(y);
        if (y1 > 0) {
          mpz_sub_ui(br->mpz, br->mpz, y1);
        } else {
          mpz_add_ui(br->mpz, br->mpz, -y1);
        }
        return bt;
      }
    case bigint_e:
      {
        term bt = __bigint_to_bigint_term(__term_to_bigint(x));
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        mpz_sub(br->mpz, br->mpz, __term_to_bigint(y)->mpz);
        return bt;
      }
    case double_e:
      return double_to_term(mpz_get_d(__term_to_bigint(x)->mpz) - __term_to_double(y));
    default:
      lisp_signal(g_invalid_arg, y);
    }

  case double_e:
    switch (get_term_type(y)) {
    case fixnum_e:
      return double_to_term(__term_to_double(x) - __fixnum_term_to_long(y));
    case bigint_e:
      return double_to_term(__term_to_double(x) - mpz_get_d(__term_to_bigint(y)->mpz));
    case double_e:
      return double_to_term(__term_to_double(x) - __term_to_double(y));
    default:
      lisp_signal(g_invalid_arg, y);
    }

  default:
    lisp_signal(g_invalid_arg, x);
  }
  SIGNAL_INTERNAL_ERROR();
}

//------------------------------------------------------------------------------
// Function: (- minuend &rest args)
//------------------------------------------------------------------------------
static term lisp_sub(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  term minuend = *args++;
  --nargs;
  if (nargs == 0) {
    return lisp_neg(minuend);
  }
  term_type_e rtp = get_term_type(minuend);
  long fr = 0;
  term bt = nil;                      // bigint term
  bigint_t * br = NULL;
  double dr = 0;
  // set result type
  switch (rtp) {
  case fixnum_e:
    fr = __fixnum_term_to_long(minuend);
    break;
  case bigint_e:
    bt = __bigint_to_bigint_term(__term_to_bigint(minuend));
    br = (bigint_t *)__term_to_bigint(bt);
    break;
  case double_e:
    dr = __term_to_double(minuend);
    break;
  default:
    lisp_signal(g_invalid_arg, minuend);
  }
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (rtp) {
    case fixnum_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          long x = __fixnum_term_to_long(a);
          long s = fr - x;
          // check overflow
          if (s >= FIXNUM_MIN && s <= FIXNUM_MAX) {
            fr = s;
          } else {
            rtp = bigint_e;
            bt = __long_to_bigint_term(fr);
            br = (bigint_t *)__term_to_bigint(bt);
            if (x > 0) {
              mpz_sub_ui(br->mpz, br->mpz, x);
            } else {
              mpz_add_ui(br->mpz, br->mpz, -x);
            }
          }
        }
        break;
      case bigint_e:
        rtp = bigint_e;
        bt = __long_to_bigint_term(fr);
        br = (bigint_t *)__term_to_bigint(bt);
        mpz_sub(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      case double_e:
        rtp = double_e;
        dr = fr - __term_to_double(a);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case bigint_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          long x = __fixnum_term_to_long(a);
          if (x > 0) {
            mpz_sub_ui(br->mpz, br->mpz, x);
          } else {
            mpz_add_ui(br->mpz, br->mpz, -x);
          }
        }
        break;
      case bigint_e:
        mpz_sub(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      case double_e:
        rtp = double_e;
        dr = mpz_get_d(br->mpz) - __term_to_double(a);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case double_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        dr -= __fixnum_term_to_long(a);
        break;
      case bigint_e:
        dr -= mpz_get_d(__term_to_bigint(a)->mpz);
        break;
      case double_e:
        dr -= __term_to_double(a);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    default:
      SIGNAL_INTERNAL_ERROR();
    }
  }
  switch (rtp) {
  case fixnum_e:
    return __long_to_fixnum_term(fr);
  case bigint_e:
    if (mpz_fits_slong_p(br->mpz) &&
        mpz_cmp_si(br->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(br->mpz, FIXNUM_MAX) <= 0) {
      return __long_to_fixnum_term(mpz_get_si(br->mpz));
    }
    return bt;
  case double_e:
    return double_to_term(dr);
  default:
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// Function: (kl::%* x y)
//------------------------------------------------------------------------------
term lisp_mul_internal(term x, term y) {
  switch (get_term_type(x)) {
  case fixnum_e:
    {
      long fr = __fixnum_term_to_long(x);
      switch (get_term_type(y)) {
      case fixnum_e:
        {
          long y1 = __fixnum_term_to_long(y);
          long s = fr * y1;
          // check overflow
          if (s >= FIXNUM_MIN && s <= FIXNUM_MAX && y1 != 0 && s / y1 == fr) {
            return __long_to_fixnum_term(s);
          }
          term bt = __long_to_bigint_term(fr);
          bigint_t * br = (bigint_t *)__term_to_bigint(bt);
          mpz_mul_si(br->mpz, br->mpz, y1);
          return bt;
        }
      case bigint_e:
        {
          term bt = __long_to_bigint_term(fr);
          bigint_t * br = (bigint_t *)__term_to_bigint(bt);
          mpz_mul(br->mpz, br->mpz, __term_to_bigint(y)->mpz);
          return bt;
        }
      case double_e:
        return double_to_term(__term_to_double(y) * fr);
      default:
        lisp_signal(g_invalid_arg, y);
      }
    }

  case bigint_e:
    switch (get_term_type(y)) {
    case fixnum_e:
      {
        term bt = __bigint_to_bigint_term(__term_to_bigint(x));
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        mpz_mul_si(br->mpz, br->mpz, __fixnum_term_to_long(y));
        return bt;
      }
    case bigint_e:
      {
        term bt = __bigint_to_bigint_term(__term_to_bigint(x));
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        mpz_mul(br->mpz, br->mpz, __term_to_bigint(y)->mpz);
        return bt;
      }
    case double_e:
      return double_to_term(__term_to_double(y) * mpz_get_d(__term_to_bigint(x)->mpz));
    default:
      lisp_signal(g_invalid_arg, y);
    }

  case double_e:
    switch (get_term_type(y)) {
    case fixnum_e:
      return double_to_term(__term_to_double(x) * __fixnum_term_to_long(y));
    case bigint_e:
      return double_to_term(__term_to_double(x) * mpz_get_d(__term_to_bigint(y)->mpz));
    case double_e:
      return double_to_term(__term_to_double(x) * __term_to_double(y));
    default:
      lisp_signal(g_invalid_arg, y);
    }
    break;

  default:
    lisp_signal(g_invalid_arg, x);
  }
  SIGNAL_INTERNAL_ERROR();
}

//------------------------------------------------------------------------------
// Function: (* &rest args)
//------------------------------------------------------------------------------
static term lisp_mul(long nargs, const term * args) {
  term_type_e rtp = fixnum_e;
  long fr = 1;
  term bt = nil;                      // bigint term
  bigint_t * br = NULL;
  double dr = 0;
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (rtp) {
    case fixnum_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          long x = __fixnum_term_to_long(a);
          long s = fr * x;
          // check overflow
          if (s >= FIXNUM_MIN && s <= FIXNUM_MAX && x != 0 && s / x == fr) {
            fr = s;
          } else {
            rtp = bigint_e;
            bt = __long_to_bigint_term(fr);
            br = (bigint_t *)__term_to_bigint(bt);
            mpz_mul_si(br->mpz, br->mpz, x);
          }
        }
        break;
      case bigint_e:
        rtp = bigint_e;
        bt = __long_to_bigint_term(fr);
        br = (bigint_t *)__term_to_bigint(bt);
        mpz_mul(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      case double_e:
        rtp = double_e;
        dr = __term_to_double(a) * fr;
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case bigint_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        mpz_mul_si(br->mpz, br->mpz, __fixnum_term_to_long(a));
        break;
      case bigint_e:
        mpz_mul(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      case double_e:
        rtp = double_e;
        dr = __term_to_double(a) * mpz_get_d(br->mpz);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case double_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        dr *= __fixnum_term_to_long(a);
        break;
      case bigint_e:
        dr *= mpz_get_d(__term_to_bigint(a)->mpz);
        break;
      case double_e:
        dr *= __term_to_double(a);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    default:
      SIGNAL_INTERNAL_ERROR();
    }
  }
  switch (rtp) {
  case fixnum_e:
    return __long_to_fixnum_term(fr);
  case bigint_e:
    return bt;
  case double_e:
    return double_to_term(dr);
  default:
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// Function: (/ numerator &rest args)
//------------------------------------------------------------------------------
static term div_float(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  term numerator = *args++;
  --nargs;
  double r;
  switch (get_term_type(numerator)) {
  case fixnum_e:
    r = __fixnum_term_to_long(numerator);
    break;
  case bigint_e:
    r = mpz_get_d(__term_to_bigint(numerator)->mpz);
    break;
  case double_e:
    r = __term_to_double(numerator);
    break;
  default:
    lisp_signal(g_invalid_arg, numerator);
  }
  if (nargs == 0) {
    return double_to_term(1 / r);
  }
  long i;
  for (i = 0; i < nargs; ++i) {
    if (r == 0) {
      assert(!is_null(g_double_zero));
      return g_double_zero;
    }
    term a = args[i];
    switch (get_term_type(a)) {
    case fixnum_e:
      r /= __fixnum_term_to_long(a);
      break;
    case bigint_e:
      r /= mpz_get_d(__term_to_bigint(a)->mpz);
      break;
    case double_e:
      r /= __term_to_double(a);
      break;
    default:
      lisp_signal(g_invalid_arg, a);
    }
  }
  return double_to_term(r);
}

//------------------------------------------------------------------------------
// Function: (kl::%div numerator denominator)
//------------------------------------------------------------------------------
term lisp_div_int_internal(term numerator, term a) {
  term_type_e rtp = get_term_type(numerator);
  long fr = 0;
  term bt = nil;                      // bigint term
  bigint_t * br = NULL;
  // set result type
  switch (rtp) {
  case fixnum_e:
    fr = __fixnum_term_to_long(numerator);
    break;
  case bigint_e:
    bt = __bigint_to_bigint_term(__term_to_bigint(numerator));
    br = (bigint_t *)__term_to_bigint(bt);
    break;
  default:
    lisp_signal(g_invalid_arg, numerator);
  }
  switch (rtp) {
  case fixnum_e:
    switch (get_term_type(a)) {
    case fixnum_e:
      {
        long x = __fixnum_term_to_long(a);
        long s = fr / x;
        // overflow may occur only when fr is FIXNUM_MIN and x is -1,
        // in that case result will be -FIXNUM_MIN
        if (s == -FIXNUM_MIN) {
          return g_positive_fixnum_min;
        }
        return __long_to_fixnum_term(s);
      }
    case bigint_e:
      // bigint always is bigger then fixnum, so division will be 0
      return __long_to_fixnum_term(0);
    default:
      lisp_signal(g_invalid_arg, a);
    }

  case bigint_e:
    switch (get_term_type(a)) {
    case fixnum_e:
      {
        long x = __fixnum_term_to_long(a);
        if (x > 0) {
          mpz_tdiv_q_ui(br->mpz, br->mpz, x);
        } else {
          mpz_tdiv_q_ui(br->mpz, br->mpz, -x);
          mpz_neg(br->mpz, br->mpz);
        }
        // check if result is 0
        if (mpz_sgn(br->mpz) == 0) {
          return __long_to_fixnum_term(0);
        }
      }
      break;
    case bigint_e:
      mpz_tdiv_q(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
      // check if result is 0
      if (mpz_sgn(br->mpz) == 0) {
        return __long_to_fixnum_term(0);
      }
      break;
    default:
      lisp_signal(g_invalid_arg, a);
    }
    if (mpz_fits_slong_p(br->mpz) &&
        mpz_cmp_si(br->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(br->mpz, FIXNUM_MAX) <= 0) {
      return __long_to_fixnum_term(mpz_get_si(br->mpz));
    }
    return bt;

  default:
    SIGNAL_INTERNAL_ERROR();
  }
  SIGNAL_INTERNAL_ERROR();
}

//------------------------------------------------------------------------------
// Function: (div numerator &rest args)
//------------------------------------------------------------------------------
static term div_int(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  term numerator = *args++;
  --nargs;
  term_type_e rtp = get_term_type(numerator);
  long fr = 0;
  term bt = nil;                      // bigint term
  bigint_t * br = NULL;
  // set result type
  switch (rtp) {
  case fixnum_e:
    fr = __fixnum_term_to_long(numerator);
    break;
  case bigint_e:
    bt = __bigint_to_bigint_term(__term_to_bigint(numerator));
    br = (bigint_t *)__term_to_bigint(bt);
    break;
  default:
    lisp_signal(g_invalid_arg, numerator);
  }
  if (nargs == 0) {
    switch (rtp) {
    case fixnum_e:
      return __long_to_fixnum_term(1 / fr);
    case bigint_e:
      return __long_to_fixnum_term(0);
    default:
      lisp_signal(g_invalid_arg, numerator);
    }
  }
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (rtp) {
    case fixnum_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          long x = __fixnum_term_to_long(a);
          long s = fr / x;
          // overflow may occur only when fr is FIXNUM_MIN and x is -1,
          // in that case result will be -FIXNUM_MIN
          if (s == -FIXNUM_MIN) {
            rtp = bigint_e;
            bt = __long_to_bigint_term(-FIXNUM_MIN);
            br = (bigint_t *)__term_to_bigint(bt);
          } else {
            fr = s;
            if (fr == 0) {
              return __long_to_fixnum_term(0);
            }
          }
        }
        break;
      case bigint_e:
        // bigint always is bigger then fixnum, so division will be 0
        return __long_to_fixnum_term(0);
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case bigint_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          long x = __fixnum_term_to_long(a);
          if (x > 0) {
            mpz_tdiv_q_ui(br->mpz, br->mpz, x);
          } else {
            mpz_tdiv_q_ui(br->mpz, br->mpz, -x);
            mpz_neg(br->mpz, br->mpz);
          }
          // check if result is 0
          if (mpz_sgn(br->mpz) == 0) {
            return __long_to_fixnum_term(0);
          }
        }
        break;
      case bigint_e:
        mpz_tdiv_q(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        // check if result is 0
        if (mpz_sgn(br->mpz) == 0) {
          return __long_to_fixnum_term(0);
        }
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    default:
      SIGNAL_INTERNAL_ERROR();
    }
  }
  switch (rtp) {
  case fixnum_e:
    return __long_to_fixnum_term(fr);
  case bigint_e:
    if (mpz_fits_slong_p(br->mpz) &&
        mpz_cmp_si(br->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(br->mpz, FIXNUM_MAX) <= 0) {
      return __long_to_fixnum_term(mpz_get_si(br->mpz));
    }
    return bt;
  default:
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// Function: (rem number divisor)
//------------------------------------------------------------------------------
static term lisp_rem(term number, term divisor) {
  switch (get_term_type(number)) {
  case fixnum_e:
    switch (get_term_type(divisor)) {
    case fixnum_e:
      return __long_to_fixnum_term(__fixnum_term_to_long(number) % __fixnum_term_to_long(divisor));
    case bigint_e:
      // bigint is always bigger then fixnum, so remainder will be number
      return number;
    default:
      lisp_signal(g_invalid_arg, divisor);
    }
    break;

  case bigint_e:
    switch (get_term_type(divisor)) {
    case fixnum_e:
      {
        term r = __long_to_bigint_term(__fixnum_term_to_long(divisor));
        bigint_t * b = (bigint_t *)__term_to_bigint(r);
        mpz_tdiv_r(b->mpz, __term_to_bigint(number)->mpz, b->mpz);
        if (mpz_fits_slong_p(b->mpz) &&
            mpz_cmp_si(b->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(b->mpz, FIXNUM_MAX) <= 0) {
          return __long_to_fixnum_term(mpz_get_si(b->mpz));
        }
        return r;
      }
    case bigint_e:
      {
        term r = __long_to_bigint_term(0);
        bigint_t * b = (bigint_t *)__term_to_bigint(r);
        mpz_tdiv_r(b->mpz, __term_to_bigint(number)->mpz, __term_to_bigint(divisor)->mpz);
        if (mpz_fits_slong_p(b->mpz) &&
            mpz_cmp_si(b->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(b->mpz, FIXNUM_MAX) <= 0) {
          return __long_to_fixnum_term(mpz_get_si(b->mpz));
        }
        return r;
      }
    default:
      lisp_signal(g_invalid_arg, divisor);
    }
    break;

  default:
    lisp_signal(g_invalid_arg, number);
  }
}

//------------------------------------------------------------------------------
// Function: (abs x) ==> number
//------------------------------------------------------------------------------
static term lisp_abs(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    {
      long f = __fixnum_term_to_long(x);
      if (f == FIXNUM_MIN) {
        return g_positive_fixnum_min;
      }
      if (f < 0) {
        return __long_to_fixnum_term(-f);
      }
      return x;
    }
  case bigint_e:
    {
      const bigint_t * b = __term_to_bigint(x);
      if (mpz_sgn(b->mpz) >= 0) {
        return x;
      } else {
        term r = __long_to_bigint_term(0);
        bigint_t * br = (bigint_t *)__term_to_bigint(r);
        mpz_neg(br->mpz, b->mpz);
        return r;
      }
    }
  case double_e:
    {
      double d = __term_to_double(x);
      if (d >= 0) {
        return x;
      } else {
        return double_to_term(-d);
      }
    }
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (band &rest args) ==> integer
//------------------------------------------------------------------------------
static term band(long nargs, const term * args) {
  term_type_e rtp = fixnum_e;
  long fr = -1;
  term bt = nil;                      // bigint term
  bigint_t * br = NULL;
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (rtp) {
    case fixnum_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        fr &= __fixnum_term_to_long(a);
        // check zero
        if (fr == 0) {
          return __long_to_fixnum_term(0);
        }
        break;
      case bigint_e:
        rtp = bigint_e;
        bt = __long_to_bigint_term(fr);
        br = (bigint_t *)__term_to_bigint(bt);
        mpz_and(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        // check zero
        if (mpz_sgn(br->mpz) == 0) {
          return __long_to_fixnum_term(0);
        }
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case bigint_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          bigint_t x;
          bigint_init_si(&x, __fixnum_term_to_long(a));
          mpz_and(br->mpz, br->mpz, x.mpz);
          mpz_clear(x.mpz);
        }
        break;
      case bigint_e:
        mpz_and(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      // check zero
      if (mpz_sgn(br->mpz) == 0) {
        return __long_to_fixnum_term(0);
      }
      break;

    default:
      SIGNAL_INTERNAL_ERROR();
    }
  }
  switch (rtp) {
  case fixnum_e:
    return __long_to_fixnum_term(fr);
  case bigint_e:
    if (mpz_fits_slong_p(br->mpz) &&
        mpz_cmp_si(br->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(br->mpz, FIXNUM_MAX) <= 0) {
      return __long_to_fixnum_term(mpz_get_si(br->mpz));
    }
    return bt;
  default:
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// Function: (bor &rest args) ==> integer
//------------------------------------------------------------------------------
static term bor(long nargs, const term * args) {
  term_type_e rtp = fixnum_e;
  long fr = 0;
  term bt = nil;                      // bigint term
  bigint_t * br = NULL;
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (rtp) {
    case fixnum_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        fr |= __fixnum_term_to_long(a);
        break;
      case bigint_e:
        rtp = bigint_e;
        bt = __long_to_bigint_term(fr);
        br = (bigint_t *)__term_to_bigint(bt);
        mpz_ior(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case bigint_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          bigint_t x;
          bigint_init_si(&x, __fixnum_term_to_long(a));
          mpz_ior(br->mpz, br->mpz, x.mpz);
          mpz_clear(x.mpz);
        }
        break;
      case bigint_e:
        mpz_ior(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    default:
      SIGNAL_INTERNAL_ERROR();
    }
  }
  switch (rtp) {
  case fixnum_e:
    return __long_to_fixnum_term(fr);
  case bigint_e:
    return bt;
  default:
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// Function: (bxor &rest args) ==> integer
//------------------------------------------------------------------------------
static term bxor(long nargs, const term * args) {
  term_type_e rtp = fixnum_e;
  long fr = 0;
  term bt = nil;                      // bigint term
  bigint_t * br = NULL;
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (rtp) {
    case fixnum_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        fr ^= __fixnum_term_to_long(a);
        break;
      case bigint_e:
        rtp = bigint_e;
        bt = __long_to_bigint_term(fr);
        br = (bigint_t *)__term_to_bigint(bt);
        mpz_xor(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    case bigint_e:
      switch (get_term_type(a)) {
      case fixnum_e:
        {
          bigint_t x;
          bigint_init_si(&x, __fixnum_term_to_long(a));
          mpz_xor(br->mpz, br->mpz, x.mpz);
          mpz_clear(x.mpz);
        }
        break;
      case bigint_e:
        mpz_xor(br->mpz, br->mpz, __term_to_bigint(a)->mpz);
        break;
      default:
        lisp_signal(g_invalid_arg, a);
      }
      break;

    default:
      SIGNAL_INTERNAL_ERROR();
    }
  }
  switch (rtp) {
  case fixnum_e:
    return __long_to_fixnum_term(fr);
  case bigint_e:
    if (mpz_fits_slong_p(br->mpz) &&
        mpz_cmp_si(br->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(br->mpz, FIXNUM_MAX) <= 0) {
      return __long_to_fixnum_term(mpz_get_si(br->mpz));
    }
    return bt;
  default:
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// Function: (bnot x) ==> integer
//------------------------------------------------------------------------------
static term bnot(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
    return __long_to_fixnum_term(~__fixnum_term_to_long(x));
  case bigint_e:
    {
      term r = __long_to_bigint_term(0);
      bigint_t * b = (bigint_t *)__term_to_bigint(r);
      mpz_com(b->mpz, __term_to_bigint(x)->mpz);
      return r;
    }
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (bshl num count)
//------------------------------------------------------------------------------
term bshl(term num, term count) {
  long c = term_to_long(count);
  if (c == 0) {
    return num;
  }
  if (c < 0) {
    lisp_signal(g_invalid_arg, count);
  }
  switch (get_term_type(num)) {
  case fixnum_e:
    {
      long fr = __fixnum_term_to_long(num);
      if (c >= LONG_BIT) {
        term bt = __long_to_bigint_term(fr);
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        mpz_mul_2exp(br->mpz, br->mpz, c);
        return bt;
      } else {
        long s = fr << c;
        // check overflow
        if ((s >> c) == fr) {
          return long_to_term(s);
        }
        term bt = __long_to_bigint_term(fr);
        bigint_t * br = (bigint_t *)__term_to_bigint(bt);
        mpz_mul_2exp(br->mpz, br->mpz, c);
        return bt;
      }
    }
  case bigint_e:
    {
      term bt = __bigint_to_bigint_term(__term_to_bigint(num));
      bigint_t * br = (bigint_t *)__term_to_bigint(bt);
      mpz_mul_2exp(br->mpz, br->mpz, c);
      return bt;
    }
  default:
    lisp_signal(g_invalid_arg, num);
  }
}

//------------------------------------------------------------------------------
// Function: (bshr num count)
//------------------------------------------------------------------------------
term bshr(term num, term count) {
  long c = term_to_long(count);
  if (c == 0) {
    return num;
  }
  if (c < 0) {
    lisp_signal(g_invalid_arg, count);
  }
  switch (get_term_type(num)) {
  case fixnum_e:
    if (c >= LONG_BIT) {
      return __long_to_fixnum_term(0);
    } else {
      return long_to_term(__fixnum_term_to_long(num) >> c);
    }
  case bigint_e:
    {
      term bt = __bigint_to_bigint_term(__term_to_bigint(num));
      bigint_t * br = (bigint_t *)__term_to_bigint(bt);
      mpz_tdiv_q_2exp(br->mpz, br->mpz, c);
      if (mpz_fits_slong_p(br->mpz) &&
          mpz_cmp_si(br->mpz, FIXNUM_MIN) >= 0 && mpz_cmp_si(br->mpz, FIXNUM_MAX) <= 0) {
        return __long_to_fixnum_term(mpz_get_si(br->mpz));
      }
      return bt;
    }
  default:
    lisp_signal(g_invalid_arg, num);
  }
}

//------------------------------------------------------------------------------
// Function: (symbol-create name) ==> symbol
//------------------------------------------------------------------------------
static term lisp_symbol_create(term name) {
  return make_symbol(name);
}

//------------------------------------------------------------------------------
// Function: (gensym &optional (x "g")) ==> symbol
// Parameters: x - string
//
// Creates and returns a fresh, uninterned symbol, as if by calling
// symbol-create. (The only difference between gensym and symbol-create is in how
// the new-symbol's name is determined.)
//
// The name of the new-symbol is the concatenation of a prefix, which defaults
// to "g", and a suffix, which is the decimal representation of a number that
// defaults to the value of internal gensym's counter.
//
// If prefix is supplied, then it is used as a prefix instead of "g" for this
// call to gensym only.
//------------------------------------------------------------------------------
static term gensym(term x) {
  static long counter = 0;
  long suffix;
  if ((suffix = __sync_add_and_fetch(&counter, 1)) > FIXNUM_MAX) {
    counter = 0;
  }
  char buf[32];
  int buflen = snprintf(buf, sizeof(buf), "%ld", suffix);
  const binary_t * s = get_binary_for_read(x);
  binary_t name;
  binary_init(&name);
  binary_ensure_capacity(&name, s->size + buflen);
  binary_append_binary(&name, s->data, s->size);
  binary_append_binary(&name, (uint8_t *)buf, buflen);
  return make_symbol_from_binary(name.data, name.size);
}

//------------------------------------------------------------------------------
// Function: (intern name &optional (package *package*) exported) ==> symbol
//------------------------------------------------------------------------------
term intern(term name, term package, term exported) {
  switch (get_term_type(package)) {
  case symbol_e:
    package = package_find_create(__term_to_symbol(package)->name);
    break;
  case binary_e:
    package = package_find_create(package);
    break;
  default:
    ;
  }
  term res = package_find_create_symbol(package, name);
  if (!is_null(exported)) {
    __term_to_symbol(res)->exported = 1;
  }
  return res;
}

//------------------------------------------------------------------------------
// Function: (unintern sym) ==> sym
//------------------------------------------------------------------------------
static term unintern(term sym) {
  symbol_t * s = term_to_symbol(sym);
  term package = s->package;
  s->package = nil;
  __package_remove_symbol(package, get_binary_for_read(s->name));
  return sym;
}

//------------------------------------------------------------------------------
// Function: (internedp x)
//------------------------------------------------------------------------------
static term internedp(term x) {
  if (!is_null(term_to_symbol(x)->package)) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static term resolve_symbol_in_package(term name_term,
                                      const binary_t * name, long pos) {
  if (pos == 1 && name->data[0] == '#') {
    // #: notation new symbol should be generated
    if (name->size == 2) {
      lisp_signal(g_syntax_error, name_term);
    }
    return make_symbol_from_binary(name->data + 2, name->size - 2);
  }
  term package;
  if (pos == 0) {
    // keyword
    package = g_kw_package;
  } else {
    binary_t pname;
    binary_init(&pname);
    pname.data = name->data;
    pname.size = pos;
    package = package_find(&pname);
    if (is_null(package)) {
      package = package_find_create(make_binary_from_binary(pname.data, pname.size));
    }
  }
  binary_t sname;
  binary_init(&sname);
  sname.data = name->data + pos + 1;
  sname.size = name->size - pos - 1;
  if (sname.size == 0) {
    lisp_signal(g_invalid_arg, name_term);
  }
  int exported = 1;
  if (sname.data[0] == ':') {
    // access to private symbol - package::symbol
    exported = 0;
    sname.data += 1;
    sname.size -= 1;
    if (sname.size == 0) {
      lisp_signal(g_invalid_arg, name_term);
    }
  }
  term sym = package_find_create_symbol(package, make_binary_from_binary(sname.data, sname.size));
  if (exported) {
    symbol_t * s = __term_to_symbol(sym);
    // make symbol exported
    s->exported = 1;
  }
  return sym;
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static term resolve_symbol_str(term name_term, const binary_t * name) {
  // 1. Check if symbol has package prefix ':'.
  long pos = binary_find_char(name, ':', 0);
  if (pos >= 0) {
    return resolve_symbol_in_package(name_term, name, pos);
  }
  // 2. Look symbol in current package. If symbol is found, then it is returned.
  term package = vm_get_dynamic(g_package_var);
  term res = package_find_symbol(package, name);
  if (!is_null(res)) {
    return res;
  }
  // 3. Look symbol in exported symbols of "kl" package. If symbol is found,
  // then it is returned.
  if (package != g_lisp_package) {
    res = package_find_symbol(g_lisp_package, name);
    if (!is_null(res) && __term_to_symbol(res)->exported) {
      return res;
    }
  }
  // 4. Intern not exported symbol in current package and return it.
  return package_find_create_symbol(package, name_term);
}

//------------------------------------------------------------------------------
// Function: (kl::resolve-symbol name) ==> symbol
//------------------------------------------------------------------------------
term resolve_symbol(term name) {
  return resolve_symbol_str(name, get_binary_for_read(name));
}

//------------------------------------------------------------------------------
// Function: (export sym) ==> sym
//------------------------------------------------------------------------------
static term lisp_export(term sym) {
  term_to_symbol(sym)->exported = 1;
  return sym;
}

//------------------------------------------------------------------------------
// Function: (unexport sym) ==> sym
//------------------------------------------------------------------------------
static term lisp_unexport(term sym) {
  term_to_symbol(sym)->exported = 0;
  return sym;
}

//------------------------------------------------------------------------------
// Function: (exportedp sym) ==> bool
//------------------------------------------------------------------------------
static term exportedp(term sym) {
  if (term_to_symbol(sym)->exported) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (kl::find-create-package name) ==> name
//------------------------------------------------------------------------------
static term lisp_find_create_package(term name) {
  switch (get_term_type(name)) {
  case binary_e:
    return package_find_create(name);
  case symbol_e:
    return package_find_create(__term_to_symbol(name)->name);
  default:
    lisp_signal(g_invalid_arg, name);
  }
}

//------------------------------------------------------------------------------
// Function: (kl::package-symbols package) ==> vector
//------------------------------------------------------------------------------
term lisp_package_symbols(term package) {
  switch (get_term_type(package)) {
  case symbol_e:
    package = package_find_create(__term_to_symbol(package)->name);
    break;
  case binary_e:
    package = package_find_create(package);
    break;
  default:
    ;
  }
  return package_symbols(package);
}

//------------------------------------------------------------------------------
// Function: (symbol-name x)
//------------------------------------------------------------------------------
static term lisp_symbol_name(term x) {
  return term_to_symbol(x)->name;
}

//------------------------------------------------------------------------------
// Function: (symbol-boundp x) ==> bool
//------------------------------------------------------------------------------
static term lisp_symbol_boundp(term x) {
  if (symbol_is_value_bound(term_to_symbol(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (symbol-fboundp x) ==> bool
//------------------------------------------------------------------------------
static term lisp_symbol_fboundp(term x) {
  if (symbol_is_function_bound(term_to_symbol(x))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (symbol-dynamic-boundp sym) ==> bool
//------------------------------------------------------------------------------
static term dynamic_boundp(term sym) {
  if (vm_is_dynamic_bound(term_to_symbol(sym))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (symbol-function x) ==> lambda or macro
//------------------------------------------------------------------------------
static term lisp_symbol_function(term x) {
  return symbol_get_function(term_to_symbol(x));
}

//------------------------------------------------------------------------------
// Function: (symbol-value x) ==> object
//------------------------------------------------------------------------------
static term lisp_symbol_value(term x) {
  return symbol_get_value(term_to_symbol(x));
}

//------------------------------------------------------------------------------
// Function: (symbol-set-prop sym key val) ==> sym
//------------------------------------------------------------------------------
static term lisp_symbol_set_prop(term sym, term key, term val) {
  symbol_set_prop(term_to_symbol(sym), key, val);
  return sym;
}

//------------------------------------------------------------------------------
// Function: (symbol-get-prop sym key &optional default-val) ==> object
//------------------------------------------------------------------------------
static term lisp_symbol_get_prop(term sym, term key, term dflt) {
  return symbol_get_prop(term_to_symbol(sym), key, dflt);
}

//------------------------------------------------------------------------------
// Function: (symbol-rm-prop sym key) ==> object or nil
//------------------------------------------------------------------------------
static term lisp_symbol_rm_prop(term sym, term key) {
  return symbol_rm_prop(term_to_symbol(sym), key);
}

//------------------------------------------------------------------------------
// Function: (symbol-get-prop-list sym) ==> list
//------------------------------------------------------------------------------
static term lisp_symbol_get_prop_list(term sym) {
  return term_to_symbol(sym)->plist;
}

//------------------------------------------------------------------------------
// Function: (symbol-package sym) ==> package
//------------------------------------------------------------------------------
static term lisp_symbol_package(term sym) {
  return term_to_symbol(sym)->package;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void mutex_unlock_on_unwind(void * m) {
  mutex_unlock((mutex_t *)m);
}

//------------------------------------------------------------------------------
// Function: (kl::%define-symbol-macro sym expansion) ==> sym
//------------------------------------------------------------------------------
term define_symbol_macro(term sym, term expansion) {
  if (!is_null(lisp_symbol_boundp(sym))) {
    lisp_fprintf_sz(vm_get_dynamic(g_stderr_var),
                    "\nWARNING: symbol macro ~s conflicts with global variable",
                    sym);
  }
  mutex_lock(&g_symbols_macros_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_symbols_macros_mutex) {
    hashmap_insert(g_symbols_macros_tbl, sym, __deep_immune_literals(expansion), nil);
  } UNWIND_PROTECT_END;
  return sym;
}

//------------------------------------------------------------------------------
// Function: (symbol-macro sym &optional not-found) ==> object
//------------------------------------------------------------------------------
term symbol_macro(term sym, term not_found) {
  if (is_null(g_symbols_macros_tbl)) {
    return not_found;
  }
  term macro = nil;
  mutex_lock(&g_symbols_macros_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_symbols_macros_mutex) {
    macro = hashmap_lookup(g_symbols_macros_tbl, sym, not_found);
  } UNWIND_PROTECT_END;
  return macro;
}

//------------------------------------------------------------------------------
// Function: (undefine-symbol-macro sym) ==> bool
//------------------------------------------------------------------------------
static term undefine_symbol_macro(term sym) {
  term macro = nil;
  mutex_lock(&g_symbols_macros_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_symbols_macros_mutex) {
    macro = hashmap_remove(g_symbols_macros_tbl, sym, g_unbound_marker);
  } UNWIND_PROTECT_END;
  if (eq(macro, g_unbound_marker)) {
    return nil;
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (all-symbol-macros) ==> list
//------------------------------------------------------------------------------
static term all_symbol_macros() {
  term res;
  mutex_lock(&g_symbols_macros_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_symbols_macros_mutex) {
    res = hashmap_to_list(g_symbols_macros_tbl);
  } UNWIND_PROTECT_END;
  return res;
}

//------------------------------------------------------------------------------
// Function: (keywordp sym) ==> object
//------------------------------------------------------------------------------
static term keywordp(term sym) {
  if (get_term_type(sym) != symbol_e) {
    return nil;
  }
  if (symbol_is_keyword(__term_to_symbol(sym))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (define-compiler-macro sym fn) ==> sym
//
// Compiler macro is a way to optimize function invocation, using special high
// optimized primitives, which are handled by compiler in special way.
//
// Compiler macro MUST generate code, which mimic function behavior: invocation
// is replaced by 'let' special form and arguments MUST be evaluated from left
// to right and bound to fresh local variables of the 'let' form and then those
// variables MUST be used in macro expansion instead arguments.
//
// Some macros may group literal values together and precompute them, if it will
// not break emulation of function invocation behavior.
//
// For example, expression (+ x y z), may be expanded into (%+ x (%+ y z)),
// where '%+' is embedded compiler primitive, optimized to work with fixnums.
// Expression (+ 1 2), may be expanded into 3 by compiler macro. And expression
// (+ x 1 2 y 5 z), may be first rewritten as (+ 8 x y z) and then expanded by
// compiler macro.
//------------------------------------------------------------------------------
static term define_compiler_macro(term sym, term fn) {
  if (!is_macro(fn)) {
    lisp_signal(g_invalid_arg, fn);
  }
  function_t * lfn = __term_to_function(fn);
  if (is_null(lfn->name)) {
    lfn->name = sym;
  }
  mutex_lock(&g_compiler_macros_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_compiler_macros_mutex) {
    hashmap_insert(g_compiler_macros_tbl, sym, fn, nil);
  } UNWIND_PROTECT_END;
  return sym;
}

//------------------------------------------------------------------------------
// Function: (compiler-macro sym &optional not-found) ==> object
//------------------------------------------------------------------------------
term compiler_macro(term sym, term not_found) {
  term macro = nil;
  mutex_lock(&g_compiler_macros_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_compiler_macros_mutex) {
    macro = hashmap_lookup(g_compiler_macros_tbl, sym, not_found);
  } UNWIND_PROTECT_END;
  return macro;
}

//------------------------------------------------------------------------------
// Function: (undefine-compiler-macro sym &optional not-found) ==> object
//------------------------------------------------------------------------------
static term undefine_compiler_macro(term sym, term not_found) {
  term macro = nil;
  mutex_lock(&g_compiler_macros_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_compiler_macros_mutex) {
    macro = hashmap_remove(g_compiler_macros_tbl, sym, g_unbound_marker);
  } UNWIND_PROTECT_END;
  if (eq(macro, g_unbound_marker)) {
    return not_found;
  }
  return macro;
}

//------------------------------------------------------------------------------
// Function: (all-compiler-macros) ==> list
//------------------------------------------------------------------------------
static term all_compiler_macros() {
  term res;
  mutex_lock(&g_compiler_macros_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_compiler_macros_mutex) {
    res = hashmap_to_list(g_compiler_macros_tbl);
  } UNWIND_PROTECT_END;
  return res;
}

//------------------------------------------------------------------------------
// Disassembler unwind-protect support
//------------------------------------------------------------------------------
typedef struct cs_unwind_info_t cs_unwind_info_t;
struct cs_unwind_info_t {
  csh handle;
  cs_insn * insn;
  long count;
};

static void cs_unwind_info_init(cs_unwind_info_t * info) {
  info->handle = 0;
  info->insn = NULL;
  info->count = 0;
}

static void cs_unwind_callback(void * p) {
  cs_unwind_info_t * i = (cs_unwind_info_t *)p;
  cs_free(i->insn, i->count);
  cs_close(&i->handle);
}

//------------------------------------------------------------------------------
// Function: (kl:disassemble fn &key (syntax 'intel) (stream *stdout*)) ==> fn
//------------------------------------------------------------------------------
static term g_intel = nil;
static term g_att = nil;

static void disassemble_recurs(term fn, term syntax, term stream, int disasm_args) {
  const function_t * lfn;
  if (is_symbol(fn)) {
    lfn = term_to_function(symbol_get_function(term_to_symbol(fn)));
  } else {
    lfn = term_to_function(fn);
  }
  if (lfn->bc_size == 0) {
    lisp_fprintf_sz(stream, "\n  external function %p", lfn->bcode);
    return;
  }
  cs_unwind_info_t csinfo;
  cs_unwind_info_init(&csinfo);
#if defined(__x86_64__)
  #define DISASM_MAX_INSTRUCTION_LEN  15
  if (cs_open(CS_ARCH_X86, CS_MODE_64, &csinfo.handle) != CS_ERR_OK) {
    lisp_signal(g_disassembler_failed, nil);
  }
#elif defined(__i386__)
  #define DISASM_MAX_INSTRUCTION_LEN  15
  if (cs_open(CS_ARCH_X86, CS_MODE_32, &csinfo.handle) != CS_ERR_OK) {
    lisp_signal(g_disassembler_failed, nil);
  }
#else
#error Unknown CPU architecture
#endif
  UNWIND_PROTECT_BEGIN(cs_unwind_callback, &csinfo) {
    if (eq(syntax, g_intel)) {
      cs_option(csinfo.handle, CS_OPT_SYNTAX, CS_OPT_SYNTAX_INTEL);
    } else if (eq(syntax, g_att)) {
      cs_option(csinfo.handle, CS_OPT_SYNTAX, CS_OPT_SYNTAX_ATT);
    } else {
      lisp_signal(g_invalid_arg, syntax);
    }
    lisp_fprintf_sz(stream, "\n~s", fn);
    lisp_fprintf_sz(stream, "\nintermediate code:\n  ~s", lfn->compiled_body);
    PUT_LITERAL_STR("\nmachine code:\n", stream);
    csinfo.count = cs_disasm(csinfo.handle, (const uint8_t *)lfn->bcode, lfn->bc_size,
                             (uint64_t)(uintptr_t)lfn->bcode, 0, &csinfo.insn);
    if (csinfo.count <= 0) {
      lisp_signal(g_disassembler_failed, nil);
    }
    unsigned j;
    for (j = 0; j < csinfo.count; ++j) {
      unsigned int ins_len = csinfo.insn[j].size;
      const uint8_t * ins_addr = (const uint8_t *)(uintptr_t)csinfo.insn[j].address;
      unsigned i;
      lisp_fprintf_sz(stream, "  %p  ", ins_addr);
      for (i = 0; i < ins_len; ++i) {
        lisp_fprintf_sz(stream, "%02X", ins_addr[i]);
      }
      for (; i < DISASM_MAX_INSTRUCTION_LEN; ++i) {
        lisp_fprintf_sz(stream, "  ", ins_addr[i]);
      }
      lisp_fprintf_sz(stream, "  %s %s\n", csinfo.insn[j].mnemonic, csinfo.insn[j].op_str);
    }
    if (lfn->enclosed != NULL) {
      // dump function environment
      const closure_env_t * env = lfn->enclosed_env;
      lisp_fprintf_sz(stream, "\nclosure environment (%p):\n", env);
      long i;
      for (i = 0; i < env->size; ++i) {
        lisp_fprintf_sz(stream, "  %d: \t~s\n", i, env->data[i]);
      }
      // disassemble enclosed function
      disassemble_recurs(__pointer_to_term(lfn->enclosed), syntax, stream, 0);
    }
    if (disasm_args) {
      if (lfn->nopt_args != 0) {
        // disassemble optional arguments functions
        long i;
        for (i = 0; i < lfn->nopt_args; ++i) {
          disassemble_recurs(lfn->opt_arg_funs[i], syntax, stream, 1);
        }
      }
      if (lfn->nkey_args != 0) {
        // disassemble keyword arguments functions
        long i;
        for (i = 0; i < lfn->nkey_args; ++i) {
          disassemble_recurs(lfn->key_arg_funs[i], syntax, stream, 1);
        }
      }
    }
  } UNWIND_PROTECT_END;
}

static term disassemble(term fn, term syntax, term stream) {
  disassemble_recurs(fn, syntax, stream, 1);
  return fn;
}

//------------------------------------------------------------------------------
// tag to throw to repl
//------------------------------------------------------------------------------
static term g_repl_throw_tag = nil;

//------------------------------------------------------------------------------
// repl unhandled signal handler
//------------------------------------------------------------------------------
static void repl_unhandled_signal_handler(term label, term value) {
  lisp_throw(g_repl_throw_tag, VALUES(g_repl_throw_tag, label, value));
}

//------------------------------------------------------------------------------
// Function: (repl &optional (package *package*)) ==> nil
//------------------------------------------------------------------------------
term repl(term package) {
  switch (get_term_type(package)) {
  case binary_e:
    package = package_find_create(package);
    break;
  case symbol_e:
    package = package_find_create(__term_to_symbol(package)->name);
    break;
  default:
    ;
  }
  term old_package = __vm_set_dynamic_binding(g_package_var, package);
  vm_t * vm = vm_get_current();
  values_t * old_values = vm->values;
  term my_values_data[MULTIPLE_VALUES_LIMIT];
  values_t my_values;
  if (old_values == NULL) {
    values_init(&my_values, my_values_data, MULTIPLE_VALUES_LIMIT);
    vm->values = &my_values;
  }
  term catched_val;
 again:
  catched_val = g_unbound_marker;
  //  vm->values->immutable = 0;
  //  __vm_clear_values();
  CATCH_BEGIN(g_repl_throw_tag) {
    HANDLER_BIND_BEGIN(repl_unhandled_signal_handler) {
      while (1) {
        lisp_fprintf_sz(vm_get_dynamic(g_stdout_var), "\n~a> ",
                        package_get_name(vm_get_dynamic(g_package_var)));
        term t = lisp_read(vm_get_dynamic(g_stdin_var), g_unbound_marker);
        if (eq(t, g_unbound_marker)) {
          goto __catch_end__;
        }
        term res = eval(t);
        if (vm->values->size == 0) {
          lisp_fprintf_sz(vm_get_dynamic(g_stdout_var), "\n~S", res);
        } else {
          term vals = lisp_list(vm->values->size, vm->values->data);
          while (!is_null(vals)) {
            lisp_fprintf_sz(vm_get_dynamic(g_stdout_var), "\n~S", car(vals));
            vals = cdr(vals);
          }
        }
      }
    } HANDLER_BIND_END;
  } CATCH_END_EX(&catched_val);
  if (eq(catched_val, g_repl_throw_tag)) {
    lisp_fprintf_sz(vm_get_dynamic(g_stderr_var), "\nrepl: unhandled signal: (~s ~s)",
                    vm->values->data[1], vm->values->data[2]);
    goto again;
  }
  __vm_set_dynamic_binding(g_package_var, old_package);
  vm->values = old_values;
  __vm_clear_values();
  return nil;
}

//------------------------------------------------------------------------------
// Function: (binary-length seq) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_length(term seq) {
  return __long_to_fixnum_term(get_binary_for_read(seq)->size);
}

//------------------------------------------------------------------------------
// Function: (binary-ref seq pos) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref(term seq, term pos) {
  return __long_to_fixnum_term(binary_at(get_binary_for_read(seq), term_to_long(pos)));
}

//------------------------------------------------------------------------------
// Function: (binary-set seq pos val) ==> val
//------------------------------------------------------------------------------
static term lisp_binary_set(term seq, term pos, term val) {
  binary_set_at(get_binary_for_write(seq), term_to_long(pos), term_to_uint8(val));
  return val;
}

//------------------------------------------------------------------------------
// Function: (binary-append-char seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_char(term seq, term x) {
  binary_append_uint8(get_binary_for_write(seq), term_to_uint8(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-binary dest src &optional src-offset count) ==> dest
//------------------------------------------------------------------------------
static term lisp_binary_append_binary(term dest, term src, term src_offset, term count) {
  const binary_t * v = get_binary_for_read(src);
  long offset = 0;
  if (!is_null(src_offset)) {
    offset = term_to_long(src_offset);
    if ((unsigned long)offset > (unsigned long)v->size) {
      lisp_signal(g_out_of_range, src_offset);
    }
  }
  unsigned long n = v->size - offset;
  if (!is_null(count)) {
    long new_n = term_to_long(count);
    if ((unsigned long)new_n > n) {
      lisp_signal(g_out_of_range, count);
    }
    n = new_n;
  }
  binary_append_binary(get_binary_for_write(dest), v->data + offset, n);
  return dest;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static enum iconv_ilseq_handler validate_iconv_ilseq_handler(term error_mode) {
  switch (term_to_long(error_mode)) {
  case iconveh_error:
    return iconveh_error;
  case iconveh_question_mark:
    return iconveh_question_mark;
  case iconveh_escape_sequence:
    return iconveh_escape_sequence;
  default:
    lisp_signal(g_invalid_arg, error_mode);
  }
}

//------------------------------------------------------------------------------
// Function: (binary-append-uchar seq x &key encoding (error-mode iconveh_error)) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uchar(term seq, term x,
                                     term encoding, term error_mode) {
  const char * e;
  if (is_null(encoding)) {
    e = locale_charset();
  } else {
    e = binary_get_c_str(get_binary_for_read(encoding));
  }
  binary_append_uchar(get_binary_for_write(seq), term_to_uchar(x),
                      e, validate_iconv_ilseq_handler(error_mode));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uchar-utf8 seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uchar_utf8(term seq, term x) {
  binary_append_uchar_utf8(get_binary_for_write(seq), term_to_uchar(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-ustring dest src &key encoding
//                                  (error-mode iconveh_error)) ==> dest
//------------------------------------------------------------------------------
static term lisp_binary_append_ustring(term dest, term src,
                                       term encoding, term error_mode) {
  const ustring_t * v = get_ustring_for_read(src);
  const char * e;
  if (is_null(encoding)) {
    e = locale_charset();
  } else {
    e = binary_get_c_str(get_binary_for_read(encoding));
  }
  binary_append_ustring(get_binary_for_write(dest), v->data, v->size, e,
                        validate_iconv_ilseq_handler(error_mode));
  return dest;
}

//------------------------------------------------------------------------------
// Function: (binary-append-ustring-utf8 dest src) ==> dest
//------------------------------------------------------------------------------
static term lisp_binary_append_ustring_utf8(term dest, term src) {
  const ustring_t * v = get_ustring_for_read(src);
  binary_append_ustring_utf8(get_binary_for_write(dest), v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (binary-ensure-capacity seq delta) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_ensure_capacity(term seq, term delta) {
  binary_ensure_capacity(get_binary_for_write(seq), term_to_long(delta));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-clear seq) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_clear(term seq) {
  binary_truncate(get_binary_for_write(seq), 0);
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-remove seq start &optional count) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_remove(term seq, term start, term count) {
  if (is_null(count)) {
    binary_truncate(get_binary_for_write(seq), term_to_long(start));
    return seq;
  }
  binary_remove(get_binary_for_write(seq), term_to_long(start), term_to_long(count));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-insert-char seq pos x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_insert_char(term seq, term pos, term x) {
  binary_insert_uint8(get_binary_for_write(seq), term_to_long(pos), term_to_uint8(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-insert-binary dest dest-pos src) ==> dest
//------------------------------------------------------------------------------
static term lisp_binary_insert_binary(term dest, term dest_pos, term src) {
  const binary_t * v = get_binary_for_read(src);
  binary_insert_binary(get_binary_for_write(dest), term_to_long(dest_pos),
                       v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (binary-insert-uchar seq pos x &key encoding
//                                (error-mode iconveh_error)) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_insert_uchar(term seq, term pos, term x, term encoding, term error_mode) {
  const char * e;
  if (is_null(encoding)) {
    e = locale_charset();
  } else {
    e = binary_get_c_str(get_binary_for_read(encoding));
  }
  binary_insert_uchar(get_binary_for_write(seq), term_to_long(pos), term_to_uchar(x), e,
                      validate_iconv_ilseq_handler(error_mode));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-insert-uchar-utf8 seq pos x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_insert_uchar_utf8(term seq, term pos, term x) {
  binary_insert_uchar_utf8(get_binary_for_write(seq), term_to_long(pos), term_to_uchar(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-insert-ustring dest dest-pos src &key encoding
//                                  (error-mode iconveh_error)) ==> dest
//------------------------------------------------------------------------------
static term lisp_binary_insert_ustring(term dest, term dest_pos, term src,
                                       term encoding, term error_mode) {
  const ustring_t * v = get_ustring_for_read(src);
  const char * e;
  if (is_null(encoding)) {
    e = locale_charset();
  } else {
    e = binary_get_c_str(get_binary_for_read(encoding));
  }
  binary_insert_ustring(get_binary_for_write(dest), term_to_long(dest_pos),
                        v->data, v->size, e,
                        validate_iconv_ilseq_handler(error_mode));
  return dest;
}

//------------------------------------------------------------------------------
// Function: (binary-insert-ustring-utf8 dest dest-pos src) ==> dest
//------------------------------------------------------------------------------
static term lisp_binary_insert_ustring_utf8(term dest, term dest_pos,
                                            term src) {
  const ustring_t * v = get_ustring_for_read(src);
  binary_insert_ustring_utf8(get_binary_for_write(dest),
                             term_to_long(dest_pos),
                             v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (binary-nreverse seq) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_nreverse(term seq) {
  binary_reverse(get_binary_for_write(seq));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-to-upper seq) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_to_upper(term seq) {
  binary_to_upper(get_binary_for_write(seq));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-to-lower seq) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_to_lower(term seq) {
  binary_to_lower(get_binary_for_write(seq));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-max-capacity seq capacity)
//------------------------------------------------------------------------------
static term lisp_binary_set_max_capacity(term seq, term capacity) {
  binary_set_max_capacity(get_binary_for_write(seq), term_to_long(capacity));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-find-char seq ch &optional (start 0)) ==> integer or nil
//------------------------------------------------------------------------------
static term lisp_binary_find_char(term seq, term ch, term start) {
  const binary_t * s = get_binary_for_read(seq);
  long pos = binary_find_char(s, term_to_uint8(ch), term_to_long(start));
  if (pos < 0) {
    return nil;
  }
  return __long_to_fixnum_term(pos);
}

//------------------------------------------------------------------------------
// Function: (binary-find-binary seq str &optional (start 0)) ==> fixnum or nil
//------------------------------------------------------------------------------
static term lisp_binary_find_binary(term seq, term str, term start) {
  const binary_t * s = get_binary_for_read(seq);
  const binary_t * w = get_binary_for_read(str);
  long pos = binary_find_binary(s, w->data, w->size, term_to_long(start));
  if (pos < 0) {
    return nil;
  }
  return __long_to_fixnum_term(pos);
}

//------------------------------------------------------------------------------
// Function: (binary-starts str prefix) ==> boolean
//------------------------------------------------------------------------------
static term lisp_binary_starts(term str, term prefix) {
  if (binary_starts(get_binary_for_read(str),
                    get_binary_for_read(prefix))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (binary-istarts str prefix) ==> boolean
//------------------------------------------------------------------------------
static term lisp_binary_istarts(term str, term prefix) {
  if (binary_istarts(get_binary_for_read(str),
                     get_binary_for_read(prefix))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term binary_concat_va(term seq, long nargs, const term * args) {
  binary_t * b = get_binary_for_write(seq);
  binary_ensure_capacity(b, nargs);
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (get_term_type(a)) {
    case fixnum_e:
      binary_append_uint8(b, term_to_uint8(a));
      break;
    case binary_e:
      {
        const binary_t * x = __get_binary_for_read(a);
        binary_append_binary(b, x->data, x->size);
      }
      break;
    default:
      lisp_signal(g_invalid_arg, a);
    }
  }
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-concat seq &rest args) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_concat(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  return binary_concat_va(*args, nargs - 1, args + 1);
}

//------------------------------------------------------------------------------
// Function: (binary &rest args) ==> binary
//------------------------------------------------------------------------------
static term lisp_binary(long nargs, const term * args) {
  return binary_concat_va(make_binary(), nargs, args);
}

//------------------------------------------------------------------------------
// Function: (binary-create size &key (init-element 0) max-capacity) ==> binary
//------------------------------------------------------------------------------
static term lisp_binary_create(term size_term, term init_element, term max_capacity) {
  term res = make_binary();
  binary_t * b = __get_binary_for_write(res);
  if (!is_null(max_capacity)) {
    binary_set_max_capacity(b, term_to_long(max_capacity));
  }
  long size = term_to_long(size_term);
  binary_ensure_capacity(b, size);
  memset(b->data, term_to_uint8(init_element), size);
  b->size = size;
  return res;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int8 seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int8(term seq, term x) {
  binary_append_int8(get_binary_for_write(seq), term_to_int8(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint8 seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint8(term seq, term x) {
  binary_append_uint8(get_binary_for_write(seq), term_to_uint8(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int8 seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int8(term seq, term offset) {
  return int8_to_term(binary_get_int8(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint8 seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint8(term seq, term offset) {
  return uint8_to_term(binary_get_uint8(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-set-int8 seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int8(term seq, term offset, term x) {
  binary_set_int8(get_binary_for_write(seq), term_to_long(offset), term_to_int8(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint8 seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint8(term seq, term offset, term x) {
  binary_set_uint8(get_binary_for_write(seq), term_to_long(offset), term_to_uint8(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int16-le seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int16_le(term seq, term x) {
  binary_append_int16_le(get_binary_for_write(seq), term_to_int16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int16-be seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int16_be(term seq, term x) {
  binary_append_int16_be(get_binary_for_write(seq), term_to_int16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int16-me seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int16_me(term seq, term x) {
  binary_append_int16(get_binary_for_write(seq), term_to_int16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int16-le seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int16_le(term seq, term offset) {
  return int16_to_term(binary_get_int16_le(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int16-be seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int16_be(term seq, term offset) {
  return int16_to_term(binary_get_int16_be(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int16-me seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int16_me(term seq, term offset) {
  return int16_to_term(binary_get_int16(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-set-int16-le seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int16_le(term seq, term offset, term x) {
  binary_set_int16_le(get_binary_for_write(seq), term_to_long(offset), term_to_int16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-int16-be seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int16_be(term seq, term offset, term x) {
  binary_set_int16_be(get_binary_for_write(seq), term_to_long(offset), term_to_int16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-int16-me seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int16_me(term seq, term offset, term x) {
  binary_set_int16(get_binary_for_write(seq), term_to_long(offset), term_to_int16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint16-le seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint16_le(term seq, term x) {
  binary_append_uint16_le(get_binary_for_write(seq), term_to_uint16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint16-be seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint16_be(term seq, term x) {
  binary_append_uint16_be(get_binary_for_write(seq), term_to_uint16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint16-me seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint16_me(term seq, term x) {
  binary_append_uint16(get_binary_for_write(seq), term_to_uint16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint16-le seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint16_le(term seq, term offset) {
  return uint16_to_term(binary_get_uint16_le(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint16-be seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint16_be(term seq, term offset) {
  return uint16_to_term(binary_get_uint16_be(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint16-me seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint16_me(term seq, term offset) {
  return uint16_to_term(binary_get_uint16(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint16-le seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint16_le(term seq, term offset, term x) {
  binary_set_uint16_le(get_binary_for_write(seq), term_to_long(offset), term_to_uint16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint16-be seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint16_be(term seq, term offset, term x) {
  binary_set_uint16_be(get_binary_for_write(seq), term_to_long(offset), term_to_uint16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint16-me seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint16_me(term seq, term offset, term x) {
  binary_set_uint16(get_binary_for_write(seq), term_to_long(offset), term_to_uint16(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int32-le seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int32_le(term seq, term x) {
  binary_append_int32_le(get_binary_for_write(seq), term_to_int32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int32-be seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int32_be(term seq, term x) {
  binary_append_int32_be(get_binary_for_write(seq), term_to_int32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int32-me seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int32_me(term seq, term x) {
  binary_append_int32(get_binary_for_write(seq), term_to_int32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int32-le seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int32_le(term seq, term offset) {
  return int32_to_term(binary_get_int32_le(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int32-be seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int32_be(term seq, term offset) {
  return int32_to_term(binary_get_int32_be(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int32-me seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int32_me(term seq, term offset) {
  return int32_to_term(binary_get_int32(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-set-int32-le seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int32_le(term seq, term offset, term x) {
  binary_set_int32_le(get_binary_for_write(seq), term_to_long(offset), term_to_int32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-int32-be seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int32_be(term seq, term offset, term x) {
  binary_set_int32_be(get_binary_for_write(seq), term_to_long(offset), term_to_int32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-int32-me seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int32_me(term seq, term offset, term x) {
  binary_set_int32(get_binary_for_write(seq), term_to_long(offset), term_to_int32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint32-le seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint32_le(term seq, term x) {
  binary_append_uint32_le(get_binary_for_write(seq), term_to_uint32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint32-be seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint32_be(term seq, term x) {
  binary_append_uint32_be(get_binary_for_write(seq), term_to_uint32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint32-me seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint32_me(term seq, term x) {
  binary_append_uint32(get_binary_for_write(seq), term_to_uint32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint32-le seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint32_le(term seq, term offset) {
  return uint32_to_term(binary_get_uint32_le(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint32-be seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint32_be(term seq, term offset) {
  return uint32_to_term(binary_get_uint32_be(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint32-me seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint32_me(term seq, term offset) {
  return uint32_to_term(binary_get_uint32(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint32-le seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint32_le(term seq, term offset, term x) {
  binary_set_uint32_le(get_binary_for_write(seq), term_to_long(offset), term_to_uint32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint32-be seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint32_be(term seq, term offset, term x) {
  binary_set_uint32_be(get_binary_for_write(seq), term_to_long(offset), term_to_uint32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint32-me seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint32_me(term seq, term offset, term x) {
  binary_set_uint32(get_binary_for_write(seq), term_to_long(offset), term_to_uint32(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int64-le seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int64_le(term seq, term x) {
  binary_append_int64_le(get_binary_for_write(seq), term_to_int64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int64-be seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int64_be(term seq, term x) {
  binary_append_int64_be(get_binary_for_write(seq), term_to_int64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-int64-me seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_int64_me(term seq, term x) {
  binary_append_int64(get_binary_for_write(seq), term_to_int64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int64-le seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int64_le(term seq, term offset) {
  return int64_to_term(binary_get_int64_le(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int64-be seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int64_be(term seq, term offset) {
  return int64_to_term(binary_get_int64_be(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-int64-me seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_int64_me(term seq, term offset) {
  return int64_to_term(binary_get_int64(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-set-int64-le seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int64_le(term seq, term offset, term x) {
  binary_set_int64_le(get_binary_for_write(seq), term_to_long(offset), term_to_int64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-int64-be seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int64_be(term seq, term offset, term x) {
  binary_set_int64_be(get_binary_for_write(seq), term_to_long(offset), term_to_int64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-int64-me seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_int64_me(term seq, term offset, term x) {
  binary_set_int64(get_binary_for_write(seq), term_to_long(offset), term_to_int64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint64-le seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint64_le(term seq, term x) {
  binary_append_uint64_le(get_binary_for_write(seq), term_to_uint64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint64-be seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint64_be(term seq, term x) {
  binary_append_uint64_be(get_binary_for_write(seq), term_to_uint64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-uint64-me seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_uint64_me(term seq, term x) {
  binary_append_uint64(get_binary_for_write(seq), term_to_uint64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint64-le seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint64_le(term seq, term offset) {
  return uint64_to_term(binary_get_uint64_le(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint64-be seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint64_be(term seq, term offset) {
  return uint64_to_term(binary_get_uint64_be(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-ref-uint64-me seq offset) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ref_uint64_me(term seq, term offset) {
  return uint64_to_term(binary_get_uint64(get_binary_for_write(seq), term_to_long(offset)));
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint64-le seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint64_le(term seq, term offset, term x) {
  binary_set_uint64_le(get_binary_for_write(seq), term_to_long(offset), term_to_uint64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint64-be seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint64_be(term seq, term offset, term x) {
  binary_set_uint64_be(get_binary_for_write(seq), term_to_long(offset), term_to_uint64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-set-uint64-me seq offset x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_set_uint64_me(term seq, term offset, term x) {
  binary_set_int64(get_binary_for_write(seq), term_to_long(offset), term_to_uint64(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-append-object seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_binary_append_object(term seq, term x) {
  binary_append_object(get_binary_for_write(seq), x);
  return seq;
}

//------------------------------------------------------------------------------
// Function: (binary-decode-object seq offset) ==> obj
//------------------------------------------------------------------------------
static term lisp_binary_decode_object(term seq, term offset) {
  return binary_decode_object(get_binary_for_read(seq), term_to_long(offset));
}

//------------------------------------------------------------------------------
// Function: (binary-compare x y) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_compare(term x, term y) {
  return __long_to_fixnum_term(binary_cmp(get_binary_for_read(x),
                                          get_binary_for_read(y)));
}

//------------------------------------------------------------------------------
// Function: (binary-icompare x y) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_icompare(term x, term y) {
  return __long_to_fixnum_term(binary_icmp(get_binary_for_read(x),
                                           get_binary_for_read(y)));
}

//------------------------------------------------------------------------------
// Function: (binary-ihash-code seq) ==> integer
//------------------------------------------------------------------------------
static term lisp_binary_ihash_code(term seq) {
  return long_to_term(binary_ihash_code(get_binary_for_read(seq)));
}

//------------------------------------------------------------------------------
// Function: (ustring-length seq) ==> integer
//------------------------------------------------------------------------------
static term lisp_ustring_length(term seq) {
  return __long_to_fixnum_term(get_ustring_for_read(seq)->size);
}

//------------------------------------------------------------------------------
// Function: (ustring-ref seq pos) ==> char
//------------------------------------------------------------------------------
static term lisp_ustring_ref(term seq, term pos) {
  return uint32_to_term(ustring_at(get_ustring_for_read(seq), term_to_long(pos)));
}

//------------------------------------------------------------------------------
// Function: (ustring-set seq pos val) ==> val
//------------------------------------------------------------------------------
static term lisp_ustring_set(term seq, term pos, term val) {
  ustring_set_at(get_ustring_for_write(seq), term_to_long(pos), term_to_uchar(val));
  return val;
}

//------------------------------------------------------------------------------
// Function: (ustring-append-uchar seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_ustring_append_uchar(term seq, term x) {
  ustring_append_uchar(get_ustring_for_write(seq), term_to_uchar(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (ustring-append-binary dest src &optional encoding) ==> dest
//------------------------------------------------------------------------------
static term lisp_ustring_append_binary(term dest, term src, term encoding) {
  const binary_t * v = get_binary_for_read(src);
  const char * e;
  if (is_null(encoding)) {
    e = locale_charset();
  } else {
    e = binary_get_c_str(get_binary_for_read(encoding));
  }
  ustring_append_binary(get_ustring_for_write(dest), v->data, v->size, e);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (ustring-append-utf8-binary dest src) ==> dest
//------------------------------------------------------------------------------
static term lisp_ustring_append_utf8_binary(term dest, term src) {
  const binary_t * v = get_binary_for_read(src);
  ustring_append_utf8_binary(get_ustring_for_write(dest), v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (ustring-append-ustring dest src) ==> dest
//------------------------------------------------------------------------------
static term lisp_ustring_append_ustring(term dest, term src) {
  const ustring_t * v = get_ustring_for_read(src);
  ustring_append_ustring(get_ustring_for_write(dest), v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (ustring-ensure-capacity seq delta) ==> seq
//------------------------------------------------------------------------------
static term lisp_ustring_ensure_capacity(term seq, term delta) {
  ustring_ensure_capacity(get_ustring_for_write(seq), term_to_long(delta));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (ustring-clear seq) ==> seq
//------------------------------------------------------------------------------
static term lisp_ustring_clear(term seq) {
  ustring_truncate(get_ustring_for_write(seq), 0);
  return seq;
}

//------------------------------------------------------------------------------
// Function: (ustring-remove seq start &optional count) ==> seq
//------------------------------------------------------------------------------
static term lisp_ustring_remove(term seq, term start, term count) {
  if (is_null(count)) {
    ustring_truncate(get_ustring_for_write(seq), term_to_long(start));
    return seq;
  }
  ustring_remove(get_ustring_for_write(seq), term_to_long(start), term_to_long(count));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (ustring-insert-uchar seq pos x) ==> seq
//------------------------------------------------------------------------------
static term lisp_ustring_insert_uchar(term seq, term pos, term x) {
  ustring_insert_uchar(get_ustring_for_write(seq), term_to_long(pos), term_to_uchar(x));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (ustring-insert-binary dest dest-pos src
//                                  &optional encoding) ==> dest
//------------------------------------------------------------------------------
static term lisp_ustring_insert_binary(term dest, term dest_pos, term src,
                                       term encoding) {
  const binary_t * v = get_binary_for_read(src);
  const char * e;
  if (is_null(encoding)) {
    e = locale_charset();
  } else {
    e = binary_get_c_str(get_binary_for_read(encoding));
  }
  ustring_insert_binary(get_ustring_for_write(dest), term_to_long(dest_pos),
                        v->data, v->size, e);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (ustring-insert-utf8-binary dest dest-pos src) ==> dest
//------------------------------------------------------------------------------
static term lisp_ustring_insert_utf8_binary(term dest, term dest_pos,
                                            term src) {
  const binary_t * v = get_binary_for_read(src);
  ustring_insert_utf8_binary(get_ustring_for_write(dest),
                             term_to_long(dest_pos),
                             v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (ustring-insert-ustring dest dest-pos src) ==> dest
//------------------------------------------------------------------------------
static term lisp_ustring_insert_ustring(term dest, term dest_pos, term src) {
  const ustring_t * v = get_ustring_for_read(src);
  ustring_insert_ustring(get_ustring_for_write(dest), term_to_long(dest_pos),
                         v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (ustring-nreverse seq) ==> seq
//------------------------------------------------------------------------------
static term lisp_ustring_nreverse(term seq) {
  ustring_reverse(get_ustring_for_write(seq));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (ustring-set-max-capacity seq capacity) ==> seq
//------------------------------------------------------------------------------
static term lisp_ustring_set_max_capacity(term seq, term capacity) {
  ustring_set_max_capacity(get_ustring_for_write(seq), term_to_long(capacity));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (ustring-find-uchar seq chr &optional (start 0)) ==> fixnum or nil
//------------------------------------------------------------------------------
static term lisp_ustring_find_uchar(term seq, term chr, term start) {
  const ustring_t * s = get_ustring_for_read(seq);
  long pos = ustring_find_uchar(s, term_to_uchar(chr), term_to_long(start));
  if (pos < 0) {
    return nil;
  }
  return __long_to_fixnum_term(pos);
}

//------------------------------------------------------------------------------
// Function: (ustring-find-ustring seq str &optional (start 0)) ==> fixnum or nil
//------------------------------------------------------------------------------
static term lisp_ustring_find_ustring(term seq, term str, term start) {
  const ustring_t * s = get_ustring_for_read(seq);
  const ustring_t * w = get_ustring_for_read(str);
  long pos = ustring_find_ustring(s, w->data, w->size, term_to_long(start));
  if (pos < 0) {
    return nil;
  }
  return __long_to_fixnum_term(pos);
}

//------------------------------------------------------------------------------
// Function: (ustring-starts str prefix) ==> boolean
//------------------------------------------------------------------------------
static term lisp_ustring_starts(term str, term prefix) {
  if (ustring_starts(get_ustring_for_read(str),
                     get_ustring_for_read(prefix))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term ustring_concat_va(term seq, long nargs, const term * args) {
  ustring_t * b = get_ustring_for_write(seq);
  ustring_ensure_capacity(b, nargs);
  long i;
  for (i = 0; i < nargs; ++i) {
    term a = args[i];
    switch (get_term_type(a)) {
    case fixnum_e:
      ustring_append_uchar(b, term_to_uchar(a));
      break;
    case ustring_e:
      {
        const ustring_t * x = __get_ustring_for_read(a);
        ustring_append_ustring(b, x->data, x->size);
      }
      break;
    default:
      lisp_signal(g_invalid_arg, a);
    }
  }
  return seq;
}

//------------------------------------------------------------------------------
// Function: (ustring-concat seq &rest args) ==> seq
//------------------------------------------------------------------------------
static term lisp_ustring_concat(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  return ustring_concat_va(*args, nargs - 1, args + 1);
}

//------------------------------------------------------------------------------
// Function: (ustring &rest args) ==> ustring
//------------------------------------------------------------------------------
static term lisp_ustring(long nargs, const term * args) {
  return ustring_concat_va(make_ustring(), nargs, args);
}

//------------------------------------------------------------------------------
// Function: (ustring-create size &key (init-element #" ") max-capacity) ==> string
//------------------------------------------------------------------------------
static term lisp_ustring_create(term size_term, term init_element, term max_capacity) {
  term res = make_ustring();
  ustring_t * b = __get_ustring_for_write(res);
  if (!is_null(max_capacity)) {
    ustring_set_max_capacity(b, term_to_long(max_capacity));
  }
  long size = term_to_long(size_term);
  ustring_ensure_capacity(b, size);
  long i;
  uint32_t c = term_to_uchar(init_element);
  for (i = 0; i < size; ++i) {
    b->data[i] = c;
  }
  b->size = size;
  return res;
}

//------------------------------------------------------------------------------
// Function: (ustring-compare s1 s2) ==> integer
//------------------------------------------------------------------------------
static term lisp_ustring_compare(term s1, term s2) {
  return __long_to_fixnum_term(ustring_cmp(get_ustring_for_read(s1), get_ustring_for_read(s2)));
}

//------------------------------------------------------------------------------
// internal function, returns length of proper list
// Detects simple loops in list - loops based on CDR field. Does not detects
// loops based on CAR field.
//------------------------------------------------------------------------------
long list_length(term seq) {
  long len = 0;
  term check_seq = seq;
  while (!is_null(seq)) {
    if ((++len & 1) == 0) {
      check_seq = cdr(check_seq);
    }
    seq = cdr(seq);
    if (eq(check_seq, seq)) {
      lisp_signal(g_loop_in_list, nil);
    }
  }
  return len;
}

//------------------------------------------------------------------------------
// Function: (length seq) ==> integer
//------------------------------------------------------------------------------
static term lisp_length(term seq) {
  return __long_to_fixnum_term(list_length(seq));
}

//------------------------------------------------------------------------------
// Function: (copy-list seq) ==> list
//------------------------------------------------------------------------------
term copy_list(term seq) {
  if (is_null(seq)) {
    return seq;
  }
  term res = nil;
  term * p = &res;
  do {
    *p = cons(car(seq), nil);
    p = &__get_cons_for_write(*p)->second;
    seq = cdr(seq);
  } while (is_cons(seq));
  if (!is_null(seq)) {
    *p = seq;
  }
  return res;
}

//------------------------------------------------------------------------------
// Function: (nreverse list) ==> list
// Destructive reverse of proper list.
//------------------------------------------------------------------------------
term nreverse(term l) {
  term r = nil;
  while (!is_null(l)) {
    cons_t * p = get_cons_for_write(l);
    term saved = l;
    l = p->second;
    p->second = r;
    r = saved;
  }
  return r;
}

//------------------------------------------------------------------------------
// Function: (reverse list) ==> list
// Non-destructive reverse of proper list. Fresh list is returned.
//------------------------------------------------------------------------------
term reverse(term seq) {
  term r = nil;
  while (!is_null(seq)) {
    r = cons(car(seq), r);
    seq = cdr(seq);
  }
  return r;
}

//--------------------------------------------------------------------------
// Function: (append &rest args) ==> list
//--------------------------------------------------------------------------
static term append(long nargs, const term * args) {
  if (nargs == 0) {
    return nil;
  }
  term res = nil;
  term * p = &res;
  long i;
  long end = nargs - 1;
  for (i = 0; i < end; ++i) {
    term l = args[i];
    while (!is_null(l)) {
      *p = LIST_1(car(l));
      l = cdr(l);
      p = &__get_cons_for_write(*p)->second;
    }
  }
  *p = args[end];
  return res;
}

//--------------------------------------------------------------------------
// Function: (mapcar fn &rest args) ==> list
//--------------------------------------------------------------------------
static term mapcar(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  const function_t * lfn;
  if (is_symbol(*args)) {
    lfn = term_to_function(symbol_get_function(term_to_symbol(*args)));
  } else {
    lfn = term_to_function(*args);
  }
  ++args;
  --nargs;
  if (nargs == 0) {
    return nil;
  }
  // copy arguments
  long i;
  term lists[nargs];
  for (i = 0; i < nargs; ++i) {
    lists[i] = args[i];
  }
  term res = nil;
  term * p = &res;
  // apply function to cars of all lists
  while (!is_null(lists[0])) {
    term cars[nargs];
    // prepare arguments to function call
    for (i = 0; i < nargs; ++i) {
      cars[i] = car(lists[i]);
      lists[i] = cdr(lists[i]);
    }
    // call function
    *p = LIST_1(lfn->bcode(nargs, cars));
    p = &__get_cons_for_write(*p)->second;
  }
  // ensure that all lists are of same length
  for (i = 0; i < nargs; ++i) {
    if (!is_null(lists[i])) {
      lisp_signal(g_different_list_length, __long_to_fixnum_term(i));
    }
  }
  return res;
}

//------------------------------------------------------------------------------
// Function: (vector-length seq) ==> integer
//------------------------------------------------------------------------------
static term lisp_vector_length(term seq) {
  return __long_to_fixnum_term(get_vector_for_read(seq)->size);
}

//------------------------------------------------------------------------------
// Function: (vector-ref seq pos) ==> object
//------------------------------------------------------------------------------
static term lisp_vector_ref(term seq, term pos) {
  return vector_at(get_vector_for_read(seq), term_to_long(pos));
}

//------------------------------------------------------------------------------
// Function: (vector-set seq pos val) ==> val
//------------------------------------------------------------------------------
static term lisp_vector_set(term seq, term pos, term val) {
  vector_set_at(get_vector_for_write(seq), term_to_long(pos), val);
  return val;
}

//------------------------------------------------------------------------------
// Function: (vector-append seq x) ==> seq
//------------------------------------------------------------------------------
static term lisp_vector_append(term seq, term x) {
  vector_append_term(get_vector_for_write(seq), x);
  return seq;
}

//------------------------------------------------------------------------------
// Function: (vector-append-list dest src) ==> dest
//------------------------------------------------------------------------------
static term lisp_vector_append_list(term dest, term src) {
  vector_t * s1 = get_vector_for_write(dest);
  if (is_null(src)) {
    return dest;
  }
  vector_ensure_capacity(s1, list_length(src));
  while (!is_null(src)) {
    const cons_t * p = get_cons_for_read(src);
    vector_append_term(s1, p->first);
    src = p->second;
  }
  return dest;
}

//------------------------------------------------------------------------------
// Function: (vector-append-vector dest src) ==> dest
//------------------------------------------------------------------------------
static term lisp_vector_append_vector(term dest, term src) {
  const vector_t * v = get_vector_for_read(src);
  vector_append_vector(get_vector_for_write(dest), v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (vector-ensure-capacity seq delta) ==> seq
//------------------------------------------------------------------------------
static term lisp_vector_ensure_capacity(term seq, term delta) {
  vector_ensure_capacity(get_vector_for_write(seq), term_to_long(delta));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (vector-clear seq) ==> seq
//------------------------------------------------------------------------------
static term lisp_vector_clear(term seq) {
  vector_truncate(get_vector_for_write(seq), 0);
  return seq;
}

//------------------------------------------------------------------------------
// Function: (vector-remove seq start &optional count) ==> seq
//------------------------------------------------------------------------------
static term lisp_vector_remove(term seq, term start, term count) {
  if (is_null(count)) {
    vector_truncate(get_vector_for_write(seq), term_to_long(start));
    return seq;
  }
  vector_remove(get_vector_for_write(seq), term_to_long(start), term_to_long(count));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (vector-insert seq pos x) ==> seq
//------------------------------------------------------------------------------
static term lisp_vector_insert(term seq, term pos, term x) {
  vector_insert_term(get_vector_for_write(seq), term_to_long(pos), x);
  return seq;
}

//------------------------------------------------------------------------------
// Function: (vector-insert-list dest dest-pos src) ==> vec
//------------------------------------------------------------------------------
static term lisp_vector_insert_list(term dest, term dest_pos, term src) {
  vector_t * v = get_vector_for_write(dest);
  if (is_null(src)) {
    return dest;
  }
  long dpos = term_to_long(dest_pos);
  __vector_make_hole(v, dpos, list_length(src));
  while (!is_null(src)) {
    const cons_t * p = get_cons_for_read(src);
    v->data[dpos++] = p->first;
    src = p->second;
  }
  return dest;
}

//------------------------------------------------------------------------------
// Function: (vector-insert-vector dest dest-pos src) ==> dest
//------------------------------------------------------------------------------
static term lisp_vector_insert_vector(term dest, term dest_pos, term src) {
  const vector_t * v = get_vector_for_read(src);
  vector_insert_vector(get_vector_for_write(dest), term_to_long(dest_pos),
                       v->data, v->size);
  return dest;
}

//------------------------------------------------------------------------------
// Function: (vector-nreverse seq) ==> seq
//------------------------------------------------------------------------------
static term lisp_vector_nreverse(term seq) {
  vector_reverse(get_vector_for_write(seq));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (vector-set-max-capacity seq capacity) ==> seq
//------------------------------------------------------------------------------
static term lisp_vector_set_max_capacity(term seq, term capacity) {
  vector_set_max_capacity(get_vector_for_write(seq), term_to_long(capacity));
  return seq;
}

//------------------------------------------------------------------------------
// Function: (vector-find seq obj &key (start 0) (test 'eql)) ==> integer or nil
//------------------------------------------------------------------------------
static term lisp_vector_find(term seq, term obj, term start, term test) {
  const function_t * lfn;
  if (is_symbol(test)) {
    lfn = term_to_lambda(symbol_get_function(term_to_symbol(test)));
  } else {
    lfn = term_to_lambda(test);
  }
  const vector_t * s = get_vector_for_read(seq);
  long pos = vector_find(s, obj, term_to_long(start), lfn->bcode);
  if (pos < 0) {
    return nil;
  }
  return __long_to_fixnum_term(pos);
}

//------------------------------------------------------------------------------
// Internal function.
//------------------------------------------------------------------------------
static term vector_concat_va(term seq, long nargs, const term * args) {
  vector_t * s = get_vector_for_write(seq);
  vector_ensure_capacity(s, nargs);
  long i;
  for (i = 0; i < nargs; ++i) {
    vector_append_term(s, args[i]);
  }
  return seq;
}

//------------------------------------------------------------------------------
// Function: (vector-concat seq &rest args) ==> seq
//------------------------------------------------------------------------------
static term lisp_vector_concat(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  return vector_concat_va(*args, nargs - 1, args + 1);
}

//------------------------------------------------------------------------------
// Function: (vector &rest args) ==> vector
//------------------------------------------------------------------------------
static term lisp_vector(long nargs, const term * args) {
  return vector_concat_va(make_vector(), nargs, args);
}

//------------------------------------------------------------------------------
// Function: (vector-create size &key init-element max-capacity) ==> vector
//------------------------------------------------------------------------------
static term lisp_vector_create(term size_term, term init_element, term max_capacity) {
  term res = make_vector();
  vector_t * b = __get_vector_for_write(res);
  if (!is_null(max_capacity)) {
    vector_set_max_capacity(b, term_to_long(max_capacity));
  }
  long size = term_to_long(size_term);
  vector_ensure_capacity(b, size);
  long i;
  for (i = 0; i < size; ++i) {
    b->data[i] = init_element;
  }
  b->size = size;
  return res;
}

//------------------------------------------------------------------------------
// Function: (vector-to-list seq) ==> proper list
// Parameters: seq - vector
//
// Constructs list from vector's elements.
//------------------------------------------------------------------------------
term lisp_vector_to_list(term seq) {
  const vector_t * s = get_vector_for_read(seq);
  term res = nil;
  term * p = &res;
  long i;
  for (i = 0; i < s->size; ++i) {
    *p = LIST_1(s->data[i]);
    p = &__get_cons_for_write(*p)->second;
  }
  return res;
}

//------------------------------------------------------------------------------
// Function: (function-lambda-list fn) ==> object
//------------------------------------------------------------------------------
static term lisp_function_lambda_list(term fn) {
  return term_to_function(fn)->origin_ll;
}

//------------------------------------------------------------------------------
// Function: (kl::function-compiled-body fn) ==> object
//------------------------------------------------------------------------------
static term lisp_function_compiled_body(term fn) {
  return term_to_function(fn)->compiled_body;
}

//------------------------------------------------------------------------------
// Function: (get-universal-time) ==> integer
// Returns universal calendar time in seconds elapsed since some Epoch. For
// example, since 1970-01-01 00:00:00 +0000 (UTC).
//------------------------------------------------------------------------------
static term get_universal_time() {
  time_t t = time(NULL);
  if (t == (time_t)-1) {
    lisp_signal_system_error();
  }
  return long_to_term(t);
}

//------------------------------------------------------------------------------
// Function: (time-zone-info) ==> (values time-zone daylight-p)
//------------------------------------------------------------------------------
static term time_zone_info() {
  tzset();
  return VALUES(long_to_term(timezone / 3600), daylight ? g_true : nil);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int c_week_of_day_to_lisp(int wday) {
  if (wday == 0) {
    return 6;
  }
  return wday - 1;
}

//------------------------------------------------------------------------------
// Function: (get-decoded-time) ==>
//   (values seconds minutes hours day-of-month month year day-of-week daylight-p time-zone)
// Returns current calendar time, represented as a decoded time.
//------------------------------------------------------------------------------
static term get_decoded_time() {
  time_t t = time(NULL);
  struct tm res;
  localtime_r(&t, &res);
  return VALUES(long_to_term(res.tm_sec), long_to_term(res.tm_min), long_to_term(res.tm_hour),
                long_to_term(res.tm_mday), long_to_term(res.tm_mon + 1), long_to_term(1900 + res.tm_year),
                long_to_term(c_week_of_day_to_lisp(res.tm_wday)),
                res.tm_isdst ? g_true : nil, long_to_term(timezone / 3600));
}

//------------------------------------------------------------------------------
// Function: (decode-universal-time universal-time &optional time-zone) ==>
//   (values seconds minutes hours day-of-month month year day-of-week daylight-p time-zone)
//------------------------------------------------------------------------------
static term decode_universal_time(term universal_time, term tz) {
  time_t t = term_to_long(universal_time);
  struct tm res;
  if (is_null(tz)) {
    localtime_r(&t, &res);
    return VALUES(long_to_term(res.tm_sec), long_to_term(res.tm_min), long_to_term(res.tm_hour),
                  long_to_term(res.tm_mday), long_to_term(res.tm_mon + 1), long_to_term(1900 + res.tm_year),
                  long_to_term(c_week_of_day_to_lisp(res.tm_wday)),
                  res.tm_isdst ? g_true : nil, long_to_term(timezone / 3600));
  }
  double dbl_tz = term_to_double(tz);
  if (dbl_tz < -24 || dbl_tz > 24) {
    lisp_signal(g_invalid_arg, tz);
  }
  t -= dbl_tz * 3600;
  gmtime_r(&t, &res);
  return VALUES(long_to_term(res.tm_sec), long_to_term(res.tm_min), long_to_term(res.tm_hour),
                long_to_term(res.tm_mday), long_to_term(res.tm_mon + 1), long_to_term(1900 + res.tm_year),
                long_to_term(c_week_of_day_to_lisp(res.tm_wday)), nil, tz);
}

//------------------------------------------------------------------------------
// Function: (encode-universal-time second minute hour day-of-month month year
//                                  &optional time-zone) ==> integer
//------------------------------------------------------------------------------
static term encode_universal_time(term seconds, term minutes, term hours, term day_of_month,
                                  term month, term year, term tz) {
  struct tm tm;
  tm.tm_sec = term_to_int(seconds);
  tm.tm_min = term_to_int(minutes);
  tm.tm_hour = term_to_int(hours);
  tm.tm_mday = term_to_int(day_of_month);
  tm.tm_mon = term_to_int(month) - 1;
  tm.tm_year = term_to_int(year) - 1900;
  if (is_null(tz)) {
    tm.tm_isdst = -1;
  } else {
    tm.tm_isdst = 0;
  }
  time_t res = mktime(&tm);
  if (res == (time_t)-1) {
    lisp_signal_system_error();
  }
  if (!is_null(tz)) {
    res = res + term_to_double(tz) * 3600 - timezone;
  }
  return long_to_term(res);
}

//------------------------------------------------------------------------------
// Function: (clock-time-msec clock-id) ==> integer
// Returns value of specified system clock in miliseconds
//------------------------------------------------------------------------------
static term clock_time_msec(term clock_id) {
  struct timespec ts = {0};
  if (clock_gettime(term_to_long(clock_id), &ts) != 0) {
    lisp_signal_system_error();
  }
  uint64_t r = (uint64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
  return uint64_to_term(r);
}

//------------------------------------------------------------------------------
// Function: (absolute-pathp path) ==> bool
//------------------------------------------------------------------------------
static term lisp_absolute_pathp(term path) {
  if (is_absoulte_path(get_binary_for_read(path))) {
    return g_true;
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (absolute-path path) ==> binary
//------------------------------------------------------------------------------
static term lisp_absolute_path(term path) {
  const binary_t * p = get_binary_for_read(path);
  if (is_absoulte_path(p)) {
    return path;
  }
  char cwd[PATH_MAX + 1];
  if (getcwd(cwd, sizeof(cwd)) == NULL) {
    lisp_signal_system_error();
  }
  term r = make_binary_from_sz(cwd);
  binary_t * s = __get_binary_for_write(r);
  binary_append_uint8(s, '/');
  binary_append_binary(s, p->data, p->size);
  return r;
}

//------------------------------------------------------------------------------
// Function: (dirname path) ==> binary
//------------------------------------------------------------------------------
static term lisp_dirname(term path) {
  const binary_t * sp = get_binary_for_read(path);
  if (is_absoulte_path(sp)) {
    long len = 1;
    long i;
    for (i = 1; i < sp->size; ++i) {
      if (sp->data[i] == '/') {
        len = i;
      }
    }
    return make_binary_from_binary(sp->data, len);
  }
  long len = 0;
  long i;
  for (i = 0; i < sp->size; ++i) {
    if (sp->data[i] == '/') {
      len = i;
    }
  }
  if (len > 0) {
    return make_binary_from_binary(sp->data, len);
  }
  return make_binary_from_binary((uint8_t *)".", 1);
}

//------------------------------------------------------------------------------
// Function: (getcwd) ==> binary
//------------------------------------------------------------------------------
static term lisp_getcwd() {
  char cwd[PATH_MAX];
  if (getcwd(cwd, sizeof(cwd)) == NULL) {
    lisp_signal_system_error();
  }
  return make_binary_from_sz(cwd);
}

//------------------------------------------------------------------------------
// load unwind info
//------------------------------------------------------------------------------
typedef struct load_unwind_info_t load_unwind_info_t;
struct load_unwind_info_t {
  term load_path;
  term fs;
  term package;
};

static void load_unwind_fn(void * v) {
  load_unwind_info_t * info = (load_unwind_info_t *)v;
  __vm_set_dynamic_binding(g_load_pathname, info->load_path);
  __vm_set_dynamic_binding(g_package_var, info->package);
  if (is_null(info->fs)) {
    stream_close(info->fs);
  }
}

//------------------------------------------------------------------------------
// Function: (load path &optional verbose) ==> object
//------------------------------------------------------------------------------
term load(term path, term verbose) {
  term abs_path = lisp_absolute_path(path);
  const binary_t * s = get_binary_for_read(abs_path);
  term res = nil;
  load_unwind_info_t info;
  info.fs = nil;
  info.package = __vm_set_dynamic_binding(g_package_var, vm_get_dynamic(g_package_var));
  info.load_path = __vm_set_dynamic_binding(g_load_pathname, abs_path);
  UNWIND_PROTECT_BEGIN(load_unwind_fn, &info) {
    info.fs = make_file_stream_sz(binary_get_c_str(s), O_RDONLY, 0);
    while (1) {
      term t = lisp_read(info.fs, g_unbound_marker);
      if (eq(t, g_unbound_marker)) {
        break;
      }
      if (!is_null(verbose)) {
        pprint(t, vm_get_dynamic(g_stdout_var));
      }
      res = eval(t);
    }
  } UNWIND_PROTECT_END;
  return res;
}

//------------------------------------------------------------------------------
// Function: (macro-env) ==> object
//------------------------------------------------------------------------------
static term macro_env() {
  return vm_get_current()->macro_env;
}

//------------------------------------------------------------------------------
// Function: (m:exp x) ==> double
//------------------------------------------------------------------------------
static term lisp_exp(term x) {
  return double_to_term(exp(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:pow base power) ==> double
//------------------------------------------------------------------------------
static term lisp_pow(term base, term power) {
  return double_to_term(pow(term_to_double(base), term_to_double(power)));
}

//------------------------------------------------------------------------------
// Function: (m:acos x) ==> double
//------------------------------------------------------------------------------
static term lisp_acos(term x) {
  return double_to_term(acos(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:acosh x) ==> double
//------------------------------------------------------------------------------
static term lisp_acosh(term x) {
  return double_to_term(acosh(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:asin x) ==> double
//------------------------------------------------------------------------------
static term lisp_asin(term x) {
  return double_to_term(asin(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:asinh x) ==> double
//------------------------------------------------------------------------------
static term lisp_asinh(term x) {
  return double_to_term(asinh(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:atan x) ==> double
//------------------------------------------------------------------------------
static term lisp_atan(term x) {
  return double_to_term(atan(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:atan2 y x) ==> double
//------------------------------------------------------------------------------
static term lisp_atan2(term y, term x) {
  return double_to_term(atan2(term_to_double(2), term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:atanh x) ==> double
//------------------------------------------------------------------------------
static term lisp_atanh(term x) {
  return double_to_term(atanh(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:cbrt x) ==> double
//------------------------------------------------------------------------------
static term lisp_cbrt(term x) {
  return double_to_term(cbrt(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:ceil x) ==> number
//------------------------------------------------------------------------------
static term lisp_ceil(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
    return x;
  case double_e:
    {
      double d = term_to_double(x);
      if (!isfinite(d)) {
        return x;
      }
      d = ceil(d);
      if (d >= FIXNUM_MIN && d <= FIXNUM_MAX) {
        long l = d;
        if (l >= FIXNUM_MIN && l <= FIXNUM_MAX) {
          return __long_to_fixnum_term(l);
        }
      }
      return __double_to_bigint_term(d);
    }
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (m:cos x) ==> double
//------------------------------------------------------------------------------
static term lisp_cos(term x) {
  return double_to_term(cos(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:cosh x) ==> double
//------------------------------------------------------------------------------
static term lisp_cosh(term x) {
  return double_to_term(cosh(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:erf x) ==> double
//------------------------------------------------------------------------------
static term lisp_erf(term x) {
  return double_to_term(erf(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:erfc x) ==> double
//------------------------------------------------------------------------------
static term lisp_erfc(term x) {
  return double_to_term(erfc(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:exp2 x) ==> double
//------------------------------------------------------------------------------
static term lisp_exp2(term x) {
  return double_to_term(exp2(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:expm1 x) ==> double
//------------------------------------------------------------------------------
static term lisp_expm1(term x) {
  return double_to_term(expm1(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:floor x) ==> number
//------------------------------------------------------------------------------
static term lisp_floor(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
    return x;
  case double_e:
    {
      double d = term_to_double(x);
      if (!isfinite(d)) {
        return x;
      }
      d = floor(d);
      if (d >= FIXNUM_MIN && d <= FIXNUM_MAX) {
        long l = d;
        if (l >= FIXNUM_MIN && l <= FIXNUM_MAX) {
          return __long_to_fixnum_term(l);
        }
      }
      return __double_to_bigint_term(d);
    }
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (m:log x) ==> double
//------------------------------------------------------------------------------
static term lisp_log(term x) {
  return double_to_term(log(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:log10 x) ==> double
//------------------------------------------------------------------------------
static term lisp_log10(term x) {
  return double_to_term(log10(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:log2 x) ==> double
//------------------------------------------------------------------------------
static term lisp_log2(term x) {
  return double_to_term(log2(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:round x) ==> number
//------------------------------------------------------------------------------
static term lisp_round(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
    return x;
  case double_e:
    {
      double d = term_to_double(x);
      if (!isfinite(d)) {
        return x;
      }
      d = round(d);
      if (d >= FIXNUM_MIN && d <= FIXNUM_MAX) {
        long l = d;
        if (l >= FIXNUM_MIN && l <= FIXNUM_MAX) {
          return __long_to_fixnum_term(l);
        }
      }
      return __double_to_bigint_term(d);
    }
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (m:sin x) ==> double
//------------------------------------------------------------------------------
static term lisp_sin(term x) {
  return double_to_term(sin(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:sinh x) ==> double
//------------------------------------------------------------------------------
static term lisp_sinh(term x) {
  return double_to_term(sinh(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:sqrt x) ==> double
//------------------------------------------------------------------------------
static term lisp_sqrt(term x) {
  return double_to_term(sqrt(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:tan x) ==> double
//------------------------------------------------------------------------------
static term lisp_tan(term x) {
  return double_to_term(tan(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:tanh x) ==> double
//------------------------------------------------------------------------------
static term lisp_tanh(term x) {
  return double_to_term(tanh(term_to_double(x)));
}

//------------------------------------------------------------------------------
// Function: (m:trunc x) ==> number
//------------------------------------------------------------------------------
static term lisp_trunc(term x) {
  switch (get_term_type(x)) {
  case fixnum_e:
  case bigint_e:
    return x;
  case double_e:
    {
      double d = term_to_double(x);
      if (!isfinite(d)) {
        return x;
      }
      d = trunc(d);
      if (d >= FIXNUM_MIN && d <= FIXNUM_MAX) {
        long l = d;
        if (l >= FIXNUM_MIN && l <= FIXNUM_MAX) {
          return __long_to_fixnum_term(l);
        }
      }
      return __double_to_bigint_term(d);
    }
  default:
    lisp_signal(g_invalid_arg, x);
  }
}

//------------------------------------------------------------------------------
// Function: (file-open-flags) ==> list
//------------------------------------------------------------------------------
static term g_file_open_flags = nil;
static term file_open_flags() {
  return g_file_open_flags;
}

//------------------------------------------------------------------------------
// Function: (file-open-modes) ==> list
//------------------------------------------------------------------------------
static term g_file_open_modes = nil;
static term file_open_modes() {
  return g_file_open_modes;
}

//------------------------------------------------------------------------------
// Function: (file-access pathname mode) ==> bool
//------------------------------------------------------------------------------
static term file_access(term pathname, term mode) {
  const binary_t * p = get_binary_for_read(pathname);
  if (access(binary_get_c_str(p), term_to_long(mode)) == 0) {
    return g_true;
  }
  switch (errno) {
  case EACCES:
  case ENOENT:
    return nil;
  default:
    lisp_signal_system_error();
  }
}

//------------------------------------------------------------------------------
// Function: (file-access-modes) ==> list
//------------------------------------------------------------------------------
static term g_file_access_modes = nil;
static term file_access_modes() {
  return g_file_access_modes;
}

//------------------------------------------------------------------------------
// Function: (getenv name &optional default) ==> string or default
//------------------------------------------------------------------------------
static term lisp_getenv(term name, term dflt) {
  const binary_t * n = get_binary_for_read(name);
  char * env = getenv(binary_get_c_str(n));
  if (env == NULL) {
    return dflt;
  }
  return make_binary_from_sz(env);
}

//------------------------------------------------------------------------------
// Function: (setenv name value overwrite) ==> name
//------------------------------------------------------------------------------
static term lisp_setenv(term name, term value, term overwrite) {
  const binary_t * n = get_binary_for_read(name);
  const binary_t * v = get_binary_for_read(value);
  if (setenv(binary_get_c_str(n), binary_get_c_str(v),
             !is_null(overwrite)) != 0) {
    lisp_signal_system_error();
  }
  return name;
}

//------------------------------------------------------------------------------
// Function: (unsetenv name) ==> name
//------------------------------------------------------------------------------
static term lisp_unsetenv(term name) {
  const binary_t * n = get_binary_for_read(name);
  if (unsetenv(binary_get_c_str(n)) != 0) {
    lisp_signal_system_error();
  }
  return name;
}

//------------------------------------------------------------------------------
// Function: (clearenv) ==> nil
//------------------------------------------------------------------------------
static term lisp_clearenv() {
  if (clearenv() != 0) {
    lisp_signal_system_error();
  }
  return nil;
}

//------------------------------------------------------------------------------
// Function: (gc) ==> nil
//------------------------------------------------------------------------------
static term lisp_gc() {
  gc_collect();
  return nil;
}

//------------------------------------------------------------------------------
// Function: (gc-time) ==> double
// Returns GC collection time in seconds.
//------------------------------------------------------------------------------
static term lisp_gc_time() {
  return double_to_term(gc_time());
}

//------------------------------------------------------------------------------
// Function: (gc-threads) ==> integer
//------------------------------------------------------------------------------
static term lisp_gc_threads() {
  return long_to_term(gc_threads());
}

//------------------------------------------------------------------------------
// Function: (gc-live-ptrs) ==> integer
//------------------------------------------------------------------------------
static term lisp_gc_live_ptrs() {
  return long_to_term(gc_live_ptrs());
}

//------------------------------------------------------------------------------
// Function: (gc-ptrs-capacity) ==> integer
//------------------------------------------------------------------------------
static term lisp_gc_ptrs_capacity() {
  return long_to_term(gc_ptrs_capacity());
}

//------------------------------------------------------------------------------
// Function: (gc-heap-size) ==> integer
//------------------------------------------------------------------------------
static term lisp_gc_heap_size() {
  return long_to_term(gc_heap_size());
}

//------------------------------------------------------------------------------
// Function: (sleep milliseconds) ==> nil
//------------------------------------------------------------------------------
static term lisp_sleep(term milliseconds) {
  long msecs = term_to_long(milliseconds);
  struct timespec req = {msecs / 1000, (msecs % 1000) * 1000000};
  struct timespec rem;
  int rc;
 again:
  if ((rc = clock_nanosleep(CLOCK_MONOTONIC, 0, &req, &rem)) != 0) {
    if (rc == EINTR) {
      req = rem;
      goto again;
    }
    errno = rc;
    lisp_signal_system_error();
  }
  return nil;
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
void __native_init() {
  g_double_zero = double_to_term(0);
  g_positive_fixnum_min = __long_to_bigint_term(-FIXNUM_MIN);

  // create symbols macros table
  term eq_fn = lisp_symbol_function(make_global_c_lambda("(kl:eq x y)", lisp_eq, 1));
  term hashcode_fn = lisp_symbol_function(make_global_c_lambda("(kl:hash-code x)", lisp_hash_code, 1));
  g_symbols_macros_tbl = hashmap_create(eq_fn, hashcode_fn);
  // populate symbols macros table with predefined macros
  hashmap_insert(g_symbols_macros_tbl, g_true, LIST_2(g_quote, g_true), nil);

  // create compiler macros table
  g_compiler_macros_tbl = hashmap_create(eq_fn, hashcode_fn);

  // init global symbols
  g_null = resolve_symbol_from_sz("kl:null");
  g_fixnum = resolve_symbol_from_sz("kl:fixnum");
  g_bigint = resolve_symbol_from_sz("kl:bigint");
  g_double = resolve_symbol_from_sz("kl:double");
  g_symbol = resolve_symbol_from_sz("kl:symbol");
  g_binary = resolve_symbol_from_sz("kl:binary");
  g_ustring = resolve_symbol_from_sz("kl:ustring");
  g_cons = resolve_symbol_from_sz("kl:cons");
  g_vector = resolve_symbol_from_sz("kl:vector");
  g_lambda = resolve_symbol_from_sz("kl:lambda");
  g_macro = resolve_symbol_from_sz("kl:macro");
  g_custom = resolve_symbol_from_sz("kl:custom");

  // Unicode conversion error handling
  define_symbol_macro(resolve_symbol_from_sz("kl:iconveh_error"),
                      __long_to_fixnum_term(iconveh_error));
  define_symbol_macro(resolve_symbol_from_sz("kl:iconveh_question_mark"),
                      __long_to_fixnum_term(iconveh_question_mark));
  define_symbol_macro(resolve_symbol_from_sz("kl:iconveh_escape_sequence"),
                      __long_to_fixnum_term(iconveh_escape_sequence));

  // disassembler syntax modes
  g_intel = resolve_symbol_from_sz("kl:intel");
  g_att = resolve_symbol_from_sz("kl:att");

  // GENERAL FUNCTIONS
  make_global_c_lambda("(kl:type-of x)", lisp_type_of, 1);
  // eq created above
  //  make_global_c_lambda("(kl:eq x y)", lisp_eq);
  make_global_c_lambda("(kl:eql x y)", lisp_eql, 1);
  make_global_c_lambda("(kl:equal x y)", lisp_equal, 1);
  make_global_c_lambda("(kl:compare x y)", compare, 1);
  make_global_c_lambda("(kl:def-global-var sym val)", def_global_var, 1);
  make_global_c_lambda("(kl:undef-global-var sym)", undef_global_var, 1);
  make_global_c_lambda("(kl:def-global-fun sym fn)", def_global_fun, 1);
  make_global_c_lambda("(kl:undef-global-fun sym)", undef_global_fun, 1);
  make_global_c_lambda("(kl:not x)", lisp_is_null, 1);

  make_global_c_lambda("(kl:macrop x)", macrop, 1);
  make_global_c_lambda("(kl:lambdap x)", lambdap, 1);
  make_global_c_lambda("(kl:closurep fn)", closurep, 1);

  // NUMBERS
  make_global_c_lambda("(kl:= num &rest other)", lisp_num_equal, 0);
  make_global_c_lambda("(kl:/= num &rest other)", lisp_num_not_equal, 0);
  // create 'kl::%%/=' to avoid infinite loop in compiler macro expansion
  make_global_c_lambda("(kl::%%/= num &rest other)", lisp_num_not_equal, 0);
  make_global_c_lambda("(kl:< num &rest other)", lisp_num_less, 0);
  make_global_c_lambda("(kl:<= num &rest other)", lisp_num_less_equal, 0);
  make_global_c_lambda("(kl:> num &rest other)", lisp_num_greater, 0);
  make_global_c_lambda("(kl:>= num &rest other)", lisp_num_greater_equal, 0);
  make_global_c_lambda("(kl:zerop x)", zerop, 1);
  make_global_c_lambda("(kl:minusp x)", minusp, 1);
  make_global_c_lambda("(kl:plusp x)", plusp, 1);
  make_global_c_lambda("(kl:1+ x)", one_plus, 1);
  make_global_c_lambda("(kl:1- x)", one_minus, 1);
  make_global_c_lambda("(kl:+ &rest args)", lisp_add, 0);
  make_global_c_lambda("(kl:- minuend &rest subtrahends)", lisp_sub, 0);
  make_global_c_lambda("(kl:* &rest args)", lisp_mul, 0);
  make_global_c_lambda("(kl:/ numerator &rest denominators)", div_float, 0);
  make_global_c_lambda("(kl:div numerator &rest denominators)", div_int, 0);
  make_global_c_lambda("(kl:rem number divisor)", lisp_rem, 1);
  make_global_c_lambda("(kl:abs x)", lisp_abs, 1);
  make_global_c_lambda("(kl:band &rest args)", band, 0);
  make_global_c_lambda("(kl:bor &rest args)", bor, 0);
  make_global_c_lambda("(kl:bxor &rest args)", bxor, 0);
  make_global_c_lambda("(kl:bnot x)", bnot, 1);
  make_global_c_lambda("(kl:bshl num count)", bshl, 1);
  make_global_c_lambda("(kl:bshr num count)", bshr, 1);
  make_global_c_lambda("(kl:fixnump x)", fixnump, 1);
  make_global_c_lambda("(kl:bigintp x)", bigintp, 1);
  make_global_c_lambda("(kl:integerp x)", integerp, 1);
  make_global_c_lambda("(kl:doublep x)", doublep, 1);
  make_global_c_lambda("(kl:numberp x)", numberp, 1);
  make_global_c_lambda("(kl:finitep x)", finitep, 1);
  make_global_c_lambda("(kl:infinitep x)", infinitep, 1);
  make_global_c_lambda("(kl:nanp x)", nanp, 1);
  make_global_c_lambda("(kl:make-random-state seed)", make_random_state, 1);
  make_global_c_lambda("(kl:random limit)", lisp_random, 1);

  // MATHEMATICAL functions, defined in <math.h>
  make_global_c_lambda("(m:exp x)", lisp_exp, 1);
  make_global_c_lambda("(m:pow base power)", lisp_pow, 1);
  make_global_c_lambda("(m:acos x)", lisp_acos, 1);
  make_global_c_lambda("(m:acosh x)", lisp_acosh, 1);
  make_global_c_lambda("(m:asin x)", lisp_asin, 1);
  make_global_c_lambda("(m:asinh x)", lisp_asinh, 1);
  make_global_c_lambda("(m:atan x)", lisp_atan, 1);
  make_global_c_lambda("(m:atan2 y x)", lisp_atan2, 1);
  make_global_c_lambda("(m:atanh x)", lisp_atanh, 1);
  make_global_c_lambda("(m:cbrt x)", lisp_cbrt, 1);
  make_global_c_lambda("(m:ceil x)", lisp_ceil, 1);
  make_global_c_lambda("(m:cos x)", lisp_cos, 1);
  make_global_c_lambda("(m:cosh x)", lisp_cosh, 1);
  make_global_c_lambda("(m:erf x)", lisp_erf, 1);
  make_global_c_lambda("(m:erfc x)", lisp_erfc, 1);
  make_global_c_lambda("(m:exp2 x)", lisp_exp2, 1);
  make_global_c_lambda("(m:expm1 x)", lisp_expm1, 1);
  make_global_c_lambda("(m:floor x)", lisp_floor, 1);
  make_global_c_lambda("(m:log x)", lisp_log, 1);
  make_global_c_lambda("(m:log10 x)", lisp_log10, 1);
  make_global_c_lambda("(m:log2 x)", lisp_log2, 1);
  make_global_c_lambda("(m:round x)", lisp_round, 1);
  make_global_c_lambda("(m:sin x)", lisp_sin, 1);
  make_global_c_lambda("(m:sinh x)", lisp_sinh, 1);
  make_global_c_lambda("(m:sqrt x)", lisp_sqrt, 1);
  make_global_c_lambda("(m:tan x)", lisp_tan, 1);
  make_global_c_lambda("(m:tanh x)", lisp_tanh, 1);
  make_global_c_lambda("(m:trunc x)", lisp_trunc, 1);

  // SYMBOLS & PACKAGES
  make_global_c_lambda("(kl:symbolp x)", symbolp, 1);
  make_global_c_lambda("(kl:symbol-create name)", lisp_symbol_create, 1);
  make_global_c_lambda("(kl:gensym &optional (x \"g\"))", gensym, 1);
  make_global_c_lambda("(kl:intern name &optional (package *package*) exported)", intern, 1);
  make_global_c_lambda("(kl:unintern sym)", unintern, 1);
  make_global_c_lambda("(kl:internedp x)", internedp, 1);
  make_global_c_lambda("(kl::resolve-symbol name)", resolve_symbol, 1);
  make_global_c_lambda("(kl:export sym)", lisp_export, 1);
  make_global_c_lambda("(kl:unexport sym)", lisp_unexport, 1);
  make_global_c_lambda("(kl:exportedp sym)", exportedp, 1);
  make_global_c_lambda("(kl::find-create-package name)", lisp_find_create_package, 1);
  make_global_c_lambda("(kl:package-name package)", package_get_name, 1);
  make_global_c_lambda("(kl:symbol-name x)", lisp_symbol_name, 1);
  make_global_c_lambda("(kl:symbol-boundp x)", lisp_symbol_boundp, 1);
  make_global_c_lambda("(kl:symbol-fboundp x)", lisp_symbol_fboundp, 1);
  make_global_c_lambda("(kl:symbol-dynamic-boundp sym)", dynamic_boundp, 1);
  make_global_c_lambda("(kl:symbol-function x)", lisp_symbol_function, 1);
  make_global_c_lambda("(kl:symbol-value x)", lisp_symbol_value, 1);
  make_global_c_lambda("(kl:symbol-set-prop sym key val)", lisp_symbol_set_prop, 1);
  make_global_c_lambda("(kl:symbol-get-prop sym key &optional default-val)", lisp_symbol_get_prop, 1);
  make_global_c_lambda("(kl:symbol-rm-prop sym key)", lisp_symbol_rm_prop, 1);
  make_global_c_lambda("(kl:symbol-get-prop-list sym)", lisp_symbol_get_prop_list, 1);
  make_global_c_lambda("(kl:symbol-package sym)", lisp_symbol_package, 1);
  make_global_c_lambda("(kl::%define-symbol-macro sym expansion)", define_symbol_macro, 1);
  make_global_c_lambda("(kl:symbol-macro sym &optional not-found)", symbol_macro, 1);
  make_global_c_lambda("(kl:undefine-symbol-macro sym)", undefine_symbol_macro, 1);
  make_global_c_lambda("(kl:all-symbol-macros)", all_symbol_macros, 1);
  make_global_c_lambda("(kl:keywordp sym)", keywordp, 1);
  make_global_c_lambda("(kl::all-packages)", get_all_packages, 1);
  make_global_c_lambda("(kl::package-symbols package)", lisp_package_symbols, 1);

  // CHARS
  make_global_c_lambda("(kl:char-alphap x)", char_alphap, 1);
  make_global_c_lambda("(kl:char-alphanumericp x)", char_alphanumericp, 1);
  make_global_c_lambda("(kl:char-controlp x)", char_controlp, 1);
  make_global_c_lambda("(kl:char-digitp x)", char_digitp, 1);
  make_global_c_lambda("(kl:char-graphp x)", char_graphp, 1);
  make_global_c_lambda("(kl:char-lowerp x)", char_lowerp, 1);
  make_global_c_lambda("(kl:char-printp x)", char_printp, 1);
  make_global_c_lambda("(kl:char-punctp x)", char_punctp, 1);
  make_global_c_lambda("(kl:char-spacep x)", char_spacep, 1);
  make_global_c_lambda("(kl:char-upperp x)", char_upperp, 1);
  make_global_c_lambda("(kl:char-xdigitp x)", char_xdigitp, 1);
  make_global_c_lambda("(kl:char-to-lower x)", char_to_lower, 1);
  make_global_c_lambda("(kl:char-to-upper x)", char_to_upper, 1);

  // Unicode CHARS
  make_global_c_lambda("(kl:ucharp x)", ucharp, 1);
  make_global_c_lambda("(kl:uchar-alphap x)", uchar_alphap, 1);
  make_global_c_lambda("(kl:uchar-alphanumericp x)", uchar_alphanumericp, 1);
  make_global_c_lambda("(kl:uchar-controlp x)", uchar_controlp, 1);
  make_global_c_lambda("(kl:uchar-digitp x)", uchar_digitp, 1);
  make_global_c_lambda("(kl:uchar-graphp x)", uchar_graphp, 1);
  make_global_c_lambda("(kl:uchar-lowerp x)", uchar_lowerp, 1);
  make_global_c_lambda("(kl:uchar-printp x)", uchar_printp, 1);
  make_global_c_lambda("(kl:uchar-punctp x)", uchar_punctp, 1);
  make_global_c_lambda("(kl:uchar-spacep x)", uchar_spacep, 1);
  make_global_c_lambda("(kl:uchar-upperp x)", uchar_upperp, 1);
  make_global_c_lambda("(kl:uchar-xdigitp x)", uchar_xdigitp, 1);

  // Unicode STRINGS
  make_global_c_lambda("(kl:ustringp x)", ustringp, 1);
  make_global_c_lambda("(kl:ustring &rest args)", lisp_ustring, 0);
  make_global_c_lambda("(kl:ustring-create size &key (init-element #\" \") max-capacity)",
                       lisp_ustring_create, 1);
  make_global_c_lambda("(kl:ustring-length seq)", lisp_ustring_length, 1);
  make_global_c_lambda("(kl:ustring-ref seq pos)", lisp_ustring_ref, 1);
  make_global_c_lambda("(kl:ustring-set seq pos val)", lisp_ustring_set, 1);
  make_global_c_lambda("(kl:ustring-append-uchar seq x)", lisp_ustring_append_uchar, 1);
  make_global_c_lambda("(kl:ustring-append-binary dest src &optional encoding)",
                       lisp_ustring_append_binary, 1);
  make_global_c_lambda("(kl:ustring-append-utf8-binary dest src)", lisp_ustring_append_utf8_binary, 1);
  make_global_c_lambda("(kl:ustring-append-ustring dest src)", lisp_ustring_append_ustring, 1);
  make_global_c_lambda("(kl:ustring-ensure-capacity seq delta)", lisp_ustring_ensure_capacity, 1);
  make_global_c_lambda("(kl:ustring-clear seq)", lisp_ustring_clear, 1);
  make_global_c_lambda("(kl:ustring-remove seq start &optional count)", lisp_ustring_remove, 1);
  make_global_c_lambda("(kl:ustring-insert-uchar seq pos x)", lisp_ustring_insert_uchar, 1);
  make_global_c_lambda("(kl:ustring-insert-binary dest dest-pos src &optional encoding)",
                       lisp_ustring_insert_binary, 1);
  make_global_c_lambda("(kl:ustring-insert-utf8-binary dest dest-pos src)",
                       lisp_ustring_insert_utf8_binary, 1);
  make_global_c_lambda("(kl:ustring-insert-ustring dest dest-pos src)",
                       lisp_ustring_insert_ustring, 1);
  make_global_c_lambda("(kl:ustring-nreverse seq)", lisp_ustring_nreverse, 1);
  make_global_c_lambda("(kl:ustring-set-max-capacity seq capacity)", lisp_ustring_set_max_capacity, 1);
  make_global_c_lambda("(kl:ustring-concat seq &rest args)", lisp_ustring_concat, 0);
  make_global_c_lambda("(kl:ustring-compare s1 s2)", lisp_ustring_compare, 1);
  make_global_c_lambda("(kl:ustring-find-uchar seq chr &optional (start 0))", lisp_ustring_find_uchar, 1);
  make_global_c_lambda("(kl:ustring-find-ustring seq str &optional (start 0))", lisp_ustring_find_ustring, 1);
  make_global_c_lambda("(kl:ustring-starts str prefix)", lisp_ustring_starts, 1);

  // BINARIES
  make_global_c_lambda("(kl:binaryp x)", binaryp, 1);
  make_global_c_lambda("(kl:binary &rest args)", lisp_binary, 0);
  make_global_c_lambda("(kl:binary-create size &key (init-element 0) max-capacity)", lisp_binary_create, 1);
  make_global_c_lambda("(kl:binary-length seq)", lisp_binary_length, 1);
  make_global_c_lambda("(kl:binary-ref seq pos)", lisp_binary_ref, 1);
  make_global_c_lambda("(kl:binary-set seq pos val)", lisp_binary_set, 1);
  make_global_c_lambda("(kl:binary-append-char seq x)", lisp_binary_append_char, 1);
  make_global_c_lambda("(kl:binary-append-binary dest src &optional src-offset count)",
                       lisp_binary_append_binary, 1);
  make_global_c_lambda("(kl:binary-append-uchar seq x &key encoding (error-mode iconveh_error))",
                       lisp_binary_append_uchar, 1);
  make_global_c_lambda("(kl:binary-append-uchar-utf8 seq x)", lisp_binary_append_uchar_utf8, 1);
  make_global_c_lambda("(kl:binary-append-ustring dest src &key encoding (error-mode iconveh_error))",
                       lisp_binary_append_ustring, 1);
  make_global_c_lambda("(kl:binary-append-ustring-utf8 dest src)", lisp_binary_append_ustring_utf8, 1);
  make_global_c_lambda("(kl:binary-ensure-capacity seq delta)", lisp_binary_ensure_capacity, 1);
  make_global_c_lambda("(kl:binary-clear seq)", lisp_binary_clear, 1);
  make_global_c_lambda("(kl:binary-remove seq start &optional count)", lisp_binary_remove, 1);
  make_global_c_lambda("(kl:binary-insert-char seq pos x)", lisp_binary_insert_char, 1);
  make_global_c_lambda("(kl:binary-insert-binary dest dest-pos src)",
                       lisp_binary_insert_binary, 1);
  make_global_c_lambda("(kl:binary-insert-uchar seq pos x &key encoding (error-mode iconveh_error))",
                       lisp_binary_insert_uchar, 1);
  make_global_c_lambda("(kl:binary-insert-uchar-utf8 seq pos x)", lisp_binary_insert_uchar_utf8, 1);
  make_global_c_lambda("(kl:binary-insert-ustring dest dest-pos src &key encoding (error-mode iconveh_error))",
                       lisp_binary_insert_ustring, 1);
  make_global_c_lambda("(kl:binary-insert-ustring-utf8 dest dest-pos src)",
                        lisp_binary_insert_ustring_utf8, 1);
  make_global_c_lambda("(kl:binary-nreverse seq)", lisp_binary_nreverse, 1);
  make_global_c_lambda("(kl:binary-set-max-capacity seq capacity)", lisp_binary_set_max_capacity, 1);
  make_global_c_lambda("(kl:binary-find-char seq chr &optional (start 0))", lisp_binary_find_char, 1);
  make_global_c_lambda("(kl:binary-find-binary seq chr &optional (start 0))", lisp_binary_find_binary, 1);
  make_global_c_lambda("(kl:binary-starts str prefix)", lisp_binary_starts, 1);
  make_global_c_lambda("(kl:binary-istarts str prefix)", lisp_binary_istarts, 1);
  make_global_c_lambda("(kl:binary-concat seq &rest args)", lisp_binary_concat, 0);
  make_global_c_lambda("(kl:binary-to-upper seq &key (start 0) len)", lisp_binary_to_upper, 1);
  make_global_c_lambda("(kl:binary-to-lower seq &key (start 0) len)", lisp_binary_to_lower, 1);

  // BINARY ENCODING/DECODING
  make_global_c_lambda("(kl:binary-append-int8 seq x)", lisp_binary_append_int8, 1);
  make_global_c_lambda("(kl:binary-ref-int8 seq offset)", lisp_binary_ref_int8, 1);
  make_global_c_lambda("(kl:binary-append-uint8 seq x)", lisp_binary_append_uint8, 1);
  make_global_c_lambda("(kl:binary-ref-uint8 seq offset)", lisp_binary_ref_uint8, 1);
  make_global_c_lambda("(kl:binary-set-int8 seq offset x)", lisp_binary_set_int8, 1);
  make_global_c_lambda("(kl:binary-set-uint8 seq offset x)", lisp_binary_set_uint8, 1);
  make_global_c_lambda("(kl:binary-append-int16-le seq x)", lisp_binary_append_int16_le, 1);
  make_global_c_lambda("(kl:binary-append-int16-be seq x)", lisp_binary_append_int16_be, 1);
  make_global_c_lambda("(kl:binary-append-int16-me seq x)", lisp_binary_append_int16_me, 1);
  make_global_c_lambda("(kl:binary-ref-int16-le seq offset)", lisp_binary_ref_int16_le, 1);
  make_global_c_lambda("(kl:binary-ref-int16-be seq offset)", lisp_binary_ref_int16_be, 1);
  make_global_c_lambda("(kl:binary-ref-int16-me seq offset)", lisp_binary_ref_int16_me, 1);
  make_global_c_lambda("(kl:binary-set-int16-le seq offset x)", lisp_binary_set_int16_le, 1);
  make_global_c_lambda("(kl:binary-set-int16-be seq offset x)", lisp_binary_set_int16_be, 1);
  make_global_c_lambda("(kl:binary-set-int16-me seq offset x)", lisp_binary_set_int16_me, 1);
  make_global_c_lambda("(kl:binary-append-uint16-le seq x)", lisp_binary_append_uint16_le, 1);
  make_global_c_lambda("(kl:binary-append-uint16-be seq x)", lisp_binary_append_uint16_be, 1);
  make_global_c_lambda("(kl:binary-append-uint16-me seq x)", lisp_binary_append_uint16_me, 1);
  make_global_c_lambda("(kl:binary-ref-uint16-le seq offset)", lisp_binary_ref_uint16_le, 1);
  make_global_c_lambda("(kl:binary-ref-uint16-be seq offset)", lisp_binary_ref_uint16_be, 1);
  make_global_c_lambda("(kl:binary-ref-uint16-me seq offset)", lisp_binary_ref_uint16_me, 1);
  make_global_c_lambda("(kl:binary-set-uint16-le seq offset x)", lisp_binary_set_uint16_le, 1);
  make_global_c_lambda("(kl:binary-set-uint16-be seq offset x)", lisp_binary_set_uint16_be, 1);
  make_global_c_lambda("(kl:binary-set-uint16-me seq offset x)", lisp_binary_set_uint16_me, 1);
  make_global_c_lambda("(kl:binary-append-int32-le seq x)", lisp_binary_append_int32_le, 1);
  make_global_c_lambda("(kl:binary-append-int32-be seq x)", lisp_binary_append_int32_be, 1);
  make_global_c_lambda("(kl:binary-append-int32-me seq x)", lisp_binary_append_int32_me, 1);
  make_global_c_lambda("(kl:binary-ref-int32-le seq offset)", lisp_binary_ref_int32_le, 1);
  make_global_c_lambda("(kl:binary-ref-int32-be seq offset)", lisp_binary_ref_int32_be, 1);
  make_global_c_lambda("(kl:binary-ref-int32-me seq offset)", lisp_binary_ref_int32_me, 1);
  make_global_c_lambda("(kl:binary-set-int32-le seq offset x)", lisp_binary_set_int32_le, 1);
  make_global_c_lambda("(kl:binary-set-int32-be seq offset x)", lisp_binary_set_int32_be, 1);
  make_global_c_lambda("(kl:binary-set-int32-me seq offset x)", lisp_binary_set_int32_me, 1);
  make_global_c_lambda("(kl:binary-append-uint32-le seq x)", lisp_binary_append_uint32_le, 1);
  make_global_c_lambda("(kl:binary-append-uint32-be seq x)", lisp_binary_append_uint32_be, 1);
  make_global_c_lambda("(kl:binary-append-uint32-me seq x)", lisp_binary_append_uint32_me, 1);
  make_global_c_lambda("(kl:binary-ref-uint32-le seq offset)", lisp_binary_ref_uint32_le, 1);
  make_global_c_lambda("(kl:binary-ref-uint32-be seq offset)", lisp_binary_ref_uint32_be, 1);
  make_global_c_lambda("(kl:binary-ref-uint32-me seq offset)", lisp_binary_ref_uint32_me, 1);
  make_global_c_lambda("(kl:binary-set-uint32-le seq offset x)", lisp_binary_set_uint32_le, 1);
  make_global_c_lambda("(kl:binary-set-uint32-be seq offset x)", lisp_binary_set_uint32_be, 1);
  make_global_c_lambda("(kl:binary-set-uint32-me seq offset x)", lisp_binary_set_uint32_me, 1);
  make_global_c_lambda("(kl:binary-append-int64-le seq x)", lisp_binary_append_int64_le, 1);
  make_global_c_lambda("(kl:binary-append-int64-be seq x)", lisp_binary_append_int64_be, 1);
  make_global_c_lambda("(kl:binary-append-int64-me seq x)", lisp_binary_append_int64_me, 1);
  make_global_c_lambda("(kl:binary-ref-int64-le seq offset)", lisp_binary_ref_int64_le, 1);
  make_global_c_lambda("(kl:binary-ref-int64-be seq offset)", lisp_binary_ref_int64_be, 1);
  make_global_c_lambda("(kl:binary-ref-int64-me seq offset)", lisp_binary_ref_int64_me, 1);
  make_global_c_lambda("(kl:binary-set-int64-le seq offset x)", lisp_binary_set_int64_le, 1);
  make_global_c_lambda("(kl:binary-set-int64-be seq offset x)", lisp_binary_set_int64_be, 1);
  make_global_c_lambda("(kl:binary-set-int64-me seq offset x)", lisp_binary_set_int64_me, 1);
  make_global_c_lambda("(kl:binary-append-uint64-le seq x)", lisp_binary_append_uint64_le, 1);
  make_global_c_lambda("(kl:binary-append-uint64-be seq x)", lisp_binary_append_uint64_be, 1);
  make_global_c_lambda("(kl:binary-append-uint64-me seq x)", lisp_binary_append_uint64_me, 1);
  make_global_c_lambda("(kl:binary-ref-uint64-le seq offset)", lisp_binary_ref_uint64_le, 1);
  make_global_c_lambda("(kl:binary-ref-uint64-be seq offset)", lisp_binary_ref_uint64_be, 1);
  make_global_c_lambda("(kl:binary-ref-uint64-me seq offset)", lisp_binary_ref_uint64_me, 1);
  make_global_c_lambda("(kl:binary-set-uint64-le seq offset x)", lisp_binary_set_uint64_le, 1);
  make_global_c_lambda("(kl:binary-set-uint64-be seq offset x)", lisp_binary_set_uint64_be, 1);
  make_global_c_lambda("(kl:binary-set-uint64-me seq offset x)", lisp_binary_set_uint64_me, 1);
  make_global_c_lambda("(kl:binary-append-object seq x)", lisp_binary_append_object, 1);
  make_global_c_lambda("(kl:binary-decode-object seq offset)", lisp_binary_decode_object, 1);
  make_global_c_lambda("(kl:binary-compare x y)", lisp_binary_compare, 1);
  make_global_c_lambda("(kl:binary-icompare x y)", lisp_binary_icompare, 1);
  make_global_c_lambda("(kl:binary-ihash-code x)", lisp_binary_ihash_code, 1);

  // VECTORS
  make_global_c_lambda("(kl:vectorp x)", vectorp, 1);
  make_global_c_lambda("(kl:vector &rest args)", lisp_vector, 0);
  make_global_c_lambda("(kl:vector-create size &key init-element max-capacity)", lisp_vector_create, 1);
  make_global_c_lambda("(kl:vector-length seq)", lisp_vector_length, 1);
  make_global_c_lambda("(kl:vector-ref seq pos)", lisp_vector_ref, 1);
  make_global_c_lambda("(kl:vector-set seq pos val)", lisp_vector_set, 1);
  make_global_c_lambda("(kl:vector-append seq x)", lisp_vector_append, 1);
  make_global_c_lambda("(kl:vector-append-list dest src)", lisp_vector_append_list, 1);
  make_global_c_lambda("(kl:vector-append-vector dest src)",
                       lisp_vector_append_vector, 1);
  make_global_c_lambda("(kl:vector-ensure-capacity seq delta)", lisp_vector_ensure_capacity, 1);
  make_global_c_lambda("(kl:vector-clear seq)", lisp_vector_clear, 1);
  make_global_c_lambda("(kl:vector-remove seq start &optional count)", lisp_vector_remove, 1);
  make_global_c_lambda("(kl:vector-insert seq pos x)", lisp_vector_insert, 1);
  make_global_c_lambda("(kl:vector-insert-list vec pos seq)", lisp_vector_insert_list, 1);
  make_global_c_lambda("(kl:vector-insert-vector dest dest-pos src)", lisp_vector_insert_vector, 1);
  make_global_c_lambda("(kl:vector-nreverse seq)", lisp_vector_nreverse, 1);
  make_global_c_lambda("(kl:vector-set-max-capacity seq capacity)", lisp_vector_set_max_capacity, 1);
  make_global_c_lambda("(kl:vector-find seq obj &key (start 0) (test 'eql))", lisp_vector_find, 1);
  make_global_c_lambda("(kl:vector-concat seq &rest args)", lisp_vector_concat, 0);
  make_global_c_lambda("(kl:vector-to-list seq)", lisp_vector_to_list, 1);
  make_global_c_lambda("(kl:vector-sort seq &optional (test 'compare) start count)",
                       lisp_vector_sort, 1);
  make_global_c_lambda("(kl:vector-stable-sort seq &optional (test 'compare) start count)",
                       lisp_vector_stable_sort, 1);

  // CONSES
  make_global_c_lambda("(kl:null x)", lisp_is_null, 1);
  make_global_c_lambda("(kl:consp x)", consp, 1);
  make_global_c_lambda("(kl:list &rest args)", lisp_list, 0);
  make_global_c_lambda("(kl:cons x y)", cons, 1);
  make_global_c_lambda("(kl:car x)", car, 1);
  make_global_c_lambda("(kl:cdr x)", cdr, 1);
  make_global_c_lambda("(kl:set-car x val)", set_car, 1);
  make_global_c_lambda("(kl:set-cdr x val)", set_cdr, 1);
  make_global_c_lambda("(kl:caar x)", caar, 1);
  make_global_c_lambda("(kl:cadr x)", cadr, 1);
  make_global_c_lambda("(kl:cdar x)", cdar, 1);
  make_global_c_lambda("(kl:cddr x)", cddr, 1);
  make_global_c_lambda("(kl:caaar x)", caaar, 1);
  make_global_c_lambda("(kl:caadr x)", caadr, 1);
  make_global_c_lambda("(kl:cadar x)", cadar, 1);
  make_global_c_lambda("(kl:caddr x)", caddr, 1);
  make_global_c_lambda("(kl:cdaar x)", cdaar, 1);
  make_global_c_lambda("(kl:cdadr x)", cdadr, 1);
  make_global_c_lambda("(kl:cddar x)", cddar, 1);
  make_global_c_lambda("(kl:cdddr x)", cdddr, 1);
  make_global_c_lambda("(kl:caaaar x)", caaaar, 1);
  make_global_c_lambda("(kl:caaadr x)", caaadr, 1);
  make_global_c_lambda("(kl:caadar x)", caadar, 1);
  make_global_c_lambda("(kl:caaddr x)", caaddr, 1);
  make_global_c_lambda("(kl:cadaar x)", cadaar, 1);
  make_global_c_lambda("(kl:cadadr x)", cadadr, 1);
  make_global_c_lambda("(kl:caddar x)", caddar, 1);
  make_global_c_lambda("(kl:cadddr x)", cadddr, 1);
  make_global_c_lambda("(kl:cdaaar x)", cdaaar, 1);
  make_global_c_lambda("(kl:cdaadr x)", cdaadr, 1);
  make_global_c_lambda("(kl:cdadar x)", cdadar, 1);
  make_global_c_lambda("(kl:cdaddr x)", cdaddr, 1);
  make_global_c_lambda("(kl:cddaar x)", cddaar, 1);
  make_global_c_lambda("(kl:cddadr x)", cddadr, 1);
  make_global_c_lambda("(kl:cdddar x)", cdddar, 1);
  make_global_c_lambda("(kl:cddddr x)", cddddr, 1);
  make_global_c_lambda("(kl:length seq)", lisp_length, 1);
  make_global_c_lambda("(kl:copy-list seq)", copy_list, 1);
  make_global_c_lambda("(kl:nreverse list)", nreverse, 1);
  make_global_c_lambda("(kl:reverse list)", reverse, 1);
  make_global_c_lambda("(kl:append &rest args)", append, 0);
  make_global_c_lambda("(kl:mapcar fn &rest args)", mapcar, 0);

  // FILESYSTEM FUNCTIONS
  make_global_c_lambda("(kl:file-access pathname mode)", file_access, 1);
  make_global_c_lambda("(kl:file-access-modes)", file_access_modes, 1);
  make_global_c_lambda("(kl:absolute-pathp path)", lisp_absolute_pathp, 1);
  make_global_c_lambda("(kl:absolute-path path)", lisp_absolute_path, 1);
  make_global_c_lambda("(kl:dirname path)", lisp_dirname, 1);
  make_global_c_lambda("(kl:getcwd)", lisp_getcwd, 1);

  // ENVIRONMENT FUNCTIONS
  make_global_c_lambda("(kl:getenv name &optional default)", lisp_getenv, 1);
  make_global_c_lambda("(kl:setenv name value overwrite)", lisp_setenv, 1);
  make_global_c_lambda("(kl:unsetenv name)", lisp_unsetenv, 1);
  make_global_c_lambda("(kl:clearenv)", lisp_clearenv, 1);

  // FUNCTIONS
  make_global_c_lambda("(kl:function-lambda-list fn)", lisp_function_lambda_list, 1);
  make_global_c_lambda("(kl::function-compiled-body fn)", lisp_function_compiled_body, 1);

  // EVALUATION FUNCTIONS
  define_symbol_macro(resolve_symbol_from_sz("kl:MULTIPLE-VALUES-LIMIT"),
                      __long_to_fixnum_term(MULTIPLE_VALUES_LIMIT));
  make_global_c_lambda("(kl:eval x)", eval, 1);
  make_global_c_lambda("(kl:apply fn args)", apply, 1);
  make_global_c_lambda("(kl:apply-vector fn args)", apply_vector, 1);
  make_global_c_lambda("(kl:funcall fn &rest args)", funcall, 0);
  make_global_c_lambda("(kl:macro-env)", macro_env, 1);
  make_global_c_lambda("(kl:signal label &optional value)", lisp_signal, 1);
  make_global_c_lambda("(kl:error label fmt &rest args)", lisp_error, 0);
  make_global_c_lambda("(kl:macroexpand-1 exp &optional env)", macroexpand_1, 1);
  make_global_c_lambda("(kl:macroexpand exp &optional env)", macroexpand, 1);
  make_global_c_lambda("(kl:compiler-macroexpand-1 exp &optional env)", compiler_macroexpand_1, 1);
  make_global_c_lambda("(kl:compiler-macroexpand exp &optional env)", compiler_macroexpand, 1);
  make_global_c_lambda("(kl:define-compiler-macro sym fn)", define_compiler_macro, 1);
  make_global_c_lambda("(kl:compiler-macro sym &optional not-found)", compiler_macro, 1);
  make_global_c_lambda("(kl:undefine-compiler-macro sym &optional not-found)", undefine_compiler_macro, 1);
  make_global_c_lambda("(kl:all-compiler-macros)", all_compiler_macros, 1);
  make_global_c_lambda("(kl:load path &optional verbose)", load, 1);
  make_global_c_lambda("(kl:values res &rest other)", values, 0);
  make_global_c_lambda("(kl:disassemble fn &key (syntax 'intel) (stream *stdout*))", disassemble, 1);
  make_global_c_lambda("(kl:repl &optional (package *package*))", repl, 1);

  // STREAMS
  make_global_c_lambda("(kl:stream-create &key"
                       " name"
                       " (read-byte (lambda () "
                       "              (signal 'unsupported-operation 'read-byte)))"
                       " (write-byte (lambda (byte) "
                       "               (declare (ignore byte))"
                       "               (signal 'unsupported-operation 'write-byte)))"
                       " (read-binary (lambda (count buf)"
                       "               (declare (ignore count buf))"
                       "               (signal 'unsupported-operation 'read-binary)))"
                       " (write-binary (lambda (buf buf-offset count)"
                       "                 (declare (ignore buf buf-offset count))"
                       "                 (signal 'unsupported-operation 'write-binary)))"
                       " (flush (lambda ()"
                       "         (signal 'unsupported-operation 'flush)))"
                       " (sync (lambda ()"
                       "         (signal 'unsupported-operation 'sync)))"
                       " (datasync (lambda ()"
                       "             (signal 'unsupported-operation 'datasync)))"
                       " (close (lambda ()"
                       "          (signal 'unsupported-operation 'close))))",
                       make_custom_stream, 1);
   make_global_c_lambda("(kl:stream-lock stream)", stream_lock, 1);
  make_global_c_lambda("(kl:stream-unlock stream)", stream_unlock, 1);
  make_global_c_lambda("(kl:stream-name stream)", stream_name, 1);
  make_global_c_lambda("(kl:unread-byte byte &optional (stream *stdin*))", unread_byte, 1);
  make_global_c_lambda("(kl:read-byte &optional (stream *stdin*))", read_byte, 1);
  make_global_c_lambda("(kl:write-byte byte &optional (stream *stdout*))", write_byte, 1);
  make_global_c_lambda("(kl:read-binary count &optional (stream *stdin*) (buf (binary)))", read_binary, 1);
  make_global_c_lambda("(kl:read-exact count &optional (stream *stdin*) (buf (binary)))", read_exact, 1);
  make_global_c_lambda("(kl:write-binary buf &optional (stream *stdout*) (buf-offset 0) "
                       "(count (- (binary-length buf) buf-offset)))", write_binary, 1);
  make_global_c_lambda("(kl:write-exact buf &optional (stream *stdout*) buf-offset count)",
                       write_exact, 1);
  make_global_c_lambda("(kl:flush &optional (stream *stdout*))", stream_flush, 1);
  make_global_c_lambda("(kl:sync &optional (stream *stdout*))", stream_sync, 1);
  make_global_c_lambda("(kl:datasync &optional (stream *stdout*))", stream_datasync, 1);
  make_global_c_lambda("(kl:close stream)", stream_close, 1);
  make_global_c_lambda("(kl:read-line &optional (stream *stdin*) max-len buffer)", read_line, 1);
  make_global_c_lambda("(kl:prin1 x &optional (stream *stdout*))", prin1, 1);
  make_global_c_lambda("(kl:pprin1 x &optional (stream *stdout*))", pprin1, 1);
  make_global_c_lambda("(kl:princ x &optional (stream *stdout*))", princ, 1);
  make_global_c_lambda("(kl:print x &optional (stream *stdout*))", print, 1);
  make_global_c_lambda("(kl:pprint x &optional (stream *stdout*))", pprint, 1);
  make_global_c_lambda("(kl:read &optional (stream *stdin*) eof)", lisp_read, 1);
  make_global_c_lambda("(kl:printf str &rest objects)", lisp_printf, 0);
  make_global_c_lambda("(kl:fprintf stream str &rest objects)", lisp_fprintf, 0);

  // Binary streams
  make_global_c_lambda("(kl:binary-stream-create &optional (content (binary)))", make_binary_stream, 1);
  make_global_c_lambda("(kl:binary-stream-rewind stream &optional reset-content new-content)",
                       binary_stream_rewind, 1);
  make_global_c_lambda("(kl:binary-stream-content stream)", binary_stream_content, 1);

  // File streams
  make_global_c_lambda("(kl:file-stream-create pathname flags &optional (mode 0))", make_file_stream, 1);
  make_global_c_lambda("(kl:file-open-flags)", file_open_flags, 1);
  make_global_c_lambda("(kl:file-open-modes)", file_open_modes, 1);
  make_global_c_lambda("(kl:file-pwrite file file-offset buf &optional (buf-offset 0) "
                       "(count (- (binary-length buf) buf-offset)))", file_pwrite, 1);
  make_global_c_lambda("(kl:file-pread file offset count &optional (buf (binary)))",
                       file_pread, 1);
  make_global_c_lambda("(kl:file-seek file offset whence)", file_seek, 1);

  // HASHMAP
  // hash-code created above
  // make_global_c_lambda("(kl:hash-code x)", lisp_hash_code, 1);
  make_global_c_lambda("(kl:hashmap-create &optional (test 'equal) (hash-code 'hash-code))",
                       hashmap_create, 1);
  make_global_c_lambda("(kl:hashmap-insert tbl key value &optional no-old-value)",
                       hashmap_insert, 1);
  make_global_c_lambda("(kl:hashmap-lookup tbl key &optional not-found)",
                       hashmap_lookup, 1);
  make_global_c_lambda("(kl:hashmap-remove tbl key &optional no-old-value)",
                       hashmap_remove, 1);
  make_global_c_lambda("(kl:hashmap-size tbl)", hashmap_size, 1);
  make_global_c_lambda("(kl:hashmap-clear tbl)", hashmap_clear, 1);
  make_global_c_lambda("(kl:hashmap-to-vector tbl &optional vec)", hashmap_to_vector, 1);
  make_global_c_lambda("(kl:hashmap-to-list tbl)", hashmap_to_list, 1);
  make_global_c_lambda("(kl:hashmap-do tbl fn)", hashmap_do, 1);

  // TREEMAP
  make_global_c_lambda("(kl:treemap-create &optional (test 'compare))",
                       treemap_create, 1);
  make_global_c_lambda("(kl:treemap-insert tbl key value &optional no-old-value)",
                       treemap_insert, 1);
  make_global_c_lambda("(kl:treemap-lookup tbl key &optional not-found)", treemap_lookup, 1);
  make_global_c_lambda("(kl:treemap-remove tbl key &optional no-old-value)",
                       treemap_remove, 1);
  make_global_c_lambda("(kl:treemap-size tbl)", treemap_size, 1);
  make_global_c_lambda("(kl:treemap-clear tbl)", treemap_clear, 1);
  make_global_c_lambda("(kl:treemap-to-vector tbl &optional vec)", treemap_to_vector, 1);
  make_global_c_lambda("(kl:treemap-to-list tbl)", treemap_to_list, 1);
  make_global_c_lambda("(kl:treemap-do tbl fn)", treemap_do, 1);
  make_global_c_lambda("(kl:treemap-do-lower-bound tbl key fn)",
                       treemap_do_lower_bound, 1);
  make_global_c_lambda("(kl:treemap-do-upper-bound tbl key fn)",
                       treemap_do_upper_bound, 1);

  // AVLMAP
  make_global_c_lambda("(kl:avlmap-create &optional (test 'compare))",
                       avlmap_create, 1);
  make_global_c_lambda("(kl:avlmap-insert tbl key value &optional no-old-value)",
                       avlmap_insert, 1);
  make_global_c_lambda("(kl:avlmap-lookup tbl key &optional not-found)", avlmap_lookup, 1);
  make_global_c_lambda("(kl:avlmap-remove tbl key &optional no-old-value)",
                       avlmap_remove, 1);
  make_global_c_lambda("(kl:avlmap-size tbl)", avlmap_size, 1);
  make_global_c_lambda("(kl:avlmap-clear tbl)", avlmap_clear, 1);
  make_global_c_lambda("(kl:avlmap-to-vector tbl &optional vec)", avlmap_to_vector, 1);
  //  make_global_c_lambda("(kl:avlmap-to-list tbl)", avlmap_to_list, 1);
  // make_global_c_lambda("(kl:avlmap-do tbl fn)", avlmap_do, 1);
  // make_global_c_lambda("(kl:avlmap-do-lower-bound tbl key fn)",
  //                      avlmap_do_lower_bound, 1);
  // make_global_c_lambda("(kl:avlmap-do-upper-bound tbl key fn)",
  //                      avlmap_do_upper_bound, 1);

  // THREADS
  define_symbol_macro(resolve_symbol_from_sz("kl:THREAD-MIN-STACK-SIZE"),
                      __long_to_fixnum_term(PTHREAD_STACK_MIN));
  make_global_c_lambda("(kl:thread-create fn args &key name stack-size "
                       "(detached t) suppress-closure-warning)",
                       lisp_thread_create, 1);
  make_global_c_lambda("(kl:thread-join thread)", lisp_thread_join, 1);
  make_global_c_lambda("(kl:thread-name thread)", lisp_thread_name, 1);
  make_global_c_lambda("(kl:sleep milliseconds)", lisp_sleep, 1);

  // MUTEXES
  make_global_c_lambda("(kl:mutex-create &optional name)", lisp_mutex_create, 1);
  make_global_c_lambda("(kl:recursive-mutex-create &optional name)", lisp_recursive_mutex_create, 1);
  make_global_c_lambda("(kl:mutex-destroy mutex)", lisp_mutex_destroy, 1);
  make_global_c_lambda("(kl:mutex-lock mutex)", lisp_mutex_lock, 1);
  make_global_c_lambda("(kl:mutex-trylock mutex)", lisp_mutex_trylock, 1);
  make_global_c_lambda("(kl:mutex-unlock mutex)", lisp_mutex_unlock, 1);
  make_global_c_lambda("(kl:mutex-name mutex)", lisp_mutex_name, 1);

  // CONDITIONS
  make_global_c_lambda("(kl:condition-create &optional name)", lisp_condition_create, 1);
  make_global_c_lambda("(kl:condition-destroy condition)", lisp_condition_destroy, 1);
  make_global_c_lambda("(kl:condition-signal condition)", lisp_condition_signal, 1);
  make_global_c_lambda("(kl:condition-wait condition mutex)", lisp_condition_wait, 1);
  make_global_c_lambda("(kl:condition-name condition)", lisp_condition_name, 1);

  // RWLOCKS
  make_global_c_lambda("(kl:rwlock-create &optional name)", lisp_rwlock_create, 1);
  make_global_c_lambda("(kl:rwlock-destroy rwlock)", lisp_rwlock_destroy, 1);
  make_global_c_lambda("(kl:rwlock-rdlock rwlock)", lisp_rwlock_rdlock, 1);
  make_global_c_lambda("(kl:rwlock-wrlock rwlock)", lisp_rwlock_wrlock, 1);
  make_global_c_lambda("(kl:rwlock-try-rdlock rwlock)", lisp_rwlock_try_rdlock, 1);
  make_global_c_lambda("(kl:rwlock-try-wrlock rwlock)", lisp_rwlock_try_wrlock, 1);
  make_global_c_lambda("(kl:rwlock-unlock rwlock)", lisp_rwlock_unlock, 1);
  make_global_c_lambda("(kl:rwlock-name rwlock)", lisp_rwlock_name, 1);

  // SEMAPHORES
  make_global_c_lambda("(kl:semaphore-create &optional name value)", lisp_semaphore_create, 1);
  make_global_c_lambda("(kl:semaphore-destroy semaphore)", lisp_semaphore_destroy, 1);
  make_global_c_lambda("(kl:semaphore-post semaphore)", lisp_semaphore_post, 1);
  make_global_c_lambda("(kl:semaphore-wait semaphore)", lisp_semaphore_wait, 1);
  make_global_c_lambda("(kl:semaphore-name semaphore)", lisp_semaphore_name, 1);
  make_global_c_lambda("(kl:semaphore-value semaphore)", lisp_semaphore_value, 1);

  // TQUEUE
  make_global_c_lambda("(kl:tqueue-create &optional name (alloc-pool-size 256))",
                       lisp_tqueue_create, 1);
  make_global_c_lambda("(kl:tqueue-push tqueue object)", lisp_tqueue_push, 1);
  make_global_c_lambda("(kl:tqueue-pop tqueue)", lisp_tqueue_pop, 1);
  make_global_c_lambda("(kl:tqueue-name tqueue)", lisp_tqueue_name, 1);
  make_global_c_lambda("(kl:tqueue-emptyp tqueue)", lisp_tqueue_emptyp, 1);

  // SOCKETS constants
  define_symbol_macro(resolve_symbol_from_sz("kl:AF_INET"),
                      __long_to_fixnum_term(AF_INET));
  define_symbol_macro(resolve_symbol_from_sz("kl:AF_INET6"),
                      __long_to_fixnum_term(AF_INET6));
  define_symbol_macro(resolve_symbol_from_sz("kl:SOCK_STREAM"),
                      __long_to_fixnum_term(SOCK_STREAM));
  define_symbol_macro(resolve_symbol_from_sz("kl:SOCK_DGRAM"),
                      __long_to_fixnum_term(SOCK_DGRAM));

  // RESOLVER
  make_global_c_lambda("(kl:gethostbyname name &optional (address-family AF_INET))",
                       lisp_gethostbyname, 1);

  // SOCKETS
  make_global_c_lambda("(kl:socket-close socket)", socket_close, 1);
  make_global_c_lambda("(kl:socket-set-timeout socket timeout)", socket_set_timeout, 1);
  make_global_c_lambda("(kl:socket-select-read socket timeout)", socket_select_read, 1);
  make_global_c_lambda("(kl:socket-select-write socket timeout)", socket_select_write, 1);
  make_global_c_lambda("(kl:socket-accept socket)", socket_accept, 1);
  make_global_c_lambda("(kl:socket-peer-address socket)", socket_peer_address, 1);
  make_global_c_lambda("(kl:socket-stream-create socket &optional timeout name)",
                       make_socket_stream, 1);

  // TCP
  make_global_c_lambda("(kl:tcp-connect host port &key timeout nodelay bind-addr bind-port)",
                       tcp_connect, 1);
  make_global_c_lambda("(kl:tcp-listen port &key bind-address (reuse-address t) (backlog 128) timeout)",
                       tcp_listen, 1);

  // GARBAGE COLLECTION
  make_global_c_lambda("(kl:gc)", lisp_gc, 1);
  make_global_c_lambda("(kl::gc-time)", lisp_gc_time, 1);
  make_global_c_lambda("(kl::gc-threads)", lisp_gc_threads, 1);
  make_global_c_lambda("(kl::gc-live-ptrs)", lisp_gc_live_ptrs, 1);
  make_global_c_lambda("(kl::gc-ptrs-capacity)", lisp_gc_ptrs_capacity, 1);
  make_global_c_lambda("(kl::gc-heap-size)", lisp_gc_heap_size, 1);

  // REGULAR EXPRESSIONS
  make_global_c_lambda("(kl:regcomp pattern &key extented icase nosub newline)",
                       lisp_regcomp, 1);
  make_global_c_lambda("(kl:regexec regex str &key (str-offset 0) nmatch not-bol not-eol)",
                       lisp_regexec, 1);

  // Time date functions
  make_global_c_lambda("(kl:get-universal-time)", get_universal_time, 1);
  make_global_c_lambda("(kl:time-zone-info)", time_zone_info, 1);
  make_global_c_lambda("(kl:get-decoded-time)", get_decoded_time, 1);
  make_global_c_lambda("(kl:decode-universal-time universal-time &optional time-zone)",
                       decode_universal_time, 1);
  make_global_c_lambda("(kl:encode-universal-time second minute hour day-of-month month year "
                       "&optional time-zone)", encode_universal_time, 1);
  make_global_c_lambda("(kl:clock-time-msec clock-id)", clock_time_msec, 1);

  // Init prdefined macros
  define_symbol_macro(resolve_symbol_from_sz("kl:MOST-POSITIVE-FIXNUM"),
                      __long_to_fixnum_term(FIXNUM_MAX));
  define_symbol_macro(resolve_symbol_from_sz("kl:MOST-NEGATIVE-FIXNUM"),
                      __long_to_fixnum_term(FIXNUM_MIN));
  define_symbol_macro(resolve_symbol_from_sz("kl:USTRING-MAX-CAPACITY"),
                      __long_to_fixnum_term(USTRING_MAX_CAPACITY));
  define_symbol_macro(resolve_symbol_from_sz("kl:BINARY-MAX-CAPACITY"),
                      __long_to_fixnum_term(BINARY_MAX_CAPACITY));
  define_symbol_macro(resolve_symbol_from_sz("kl:VECTOR-MAX-CAPACITY"),
                      __long_to_fixnum_term(VECTOR_MAX_CAPACITY));
  define_symbol_macro(resolve_symbol_from_sz("kl:MOST-POSITIVE-DOUBLE-FLOAT"),
                      double_to_term(DBL_MAX));
  define_symbol_macro(resolve_symbol_from_sz("kl:LEAST-POSITIVE-DOUBLE-FLOAT"),
                      double_to_term(DBL_MIN));
  define_symbol_macro(resolve_symbol_from_sz("kl:MOST-NEGATIVE-DOUBLE-FLOAT"),
                      double_to_term(-DBL_MAX));
  define_symbol_macro(resolve_symbol_from_sz("kl:LEAST-NEGATIVE-DOUBLE-FLOAT"),
                      double_to_term(-DBL_MIN));
  define_symbol_macro(resolve_symbol_from_sz("kl:UCHAR-CODE-LIMIT"),
                      __long_to_fixnum_term(UCHAR_CODE_LIMIT));

  // *load-pathname*
  g_load_pathname = make_global_var_from_sz("kl:*load-pathname*", nil);

  // REPL throw tag
  g_repl_throw_tag = make_symbol_from_sz("repl-throw-tag");

  // Mathematical Constants
  define_symbol_macro(resolve_symbol_from_sz("m:e"), double_to_term(M_E));
  define_symbol_macro(resolve_symbol_from_sz("m:log2e"), double_to_term(M_LOG2E));
  define_symbol_macro(resolve_symbol_from_sz("m:log10e"), double_to_term(M_LOG10E));
  define_symbol_macro(resolve_symbol_from_sz("m:ln2"), double_to_term(M_LN2));
  define_symbol_macro(resolve_symbol_from_sz("m:ln10"), double_to_term(M_LN10));
  define_symbol_macro(resolve_symbol_from_sz("m:pi"), double_to_term(M_PI));
  define_symbol_macro(resolve_symbol_from_sz("m:pi/2"), double_to_term(M_PI_2));
  define_symbol_macro(resolve_symbol_from_sz("m:pi/4"), double_to_term(M_PI_4));
  define_symbol_macro(resolve_symbol_from_sz("m:1/pi"), double_to_term(M_1_PI));
  define_symbol_macro(resolve_symbol_from_sz("m:2/pi"), double_to_term(M_2_PI));
  define_symbol_macro(resolve_symbol_from_sz("m:2/sqrt_pi"), double_to_term(M_2_SQRTPI));
  define_symbol_macro(resolve_symbol_from_sz("m:sqrt2"), double_to_term(M_SQRT2));
  define_symbol_macro(resolve_symbol_from_sz("m:sqrt1/2"), double_to_term(M_SQRT1_2));

  // PATH_MAX
  define_symbol_macro(resolve_symbol_from_sz("kl:PATH_MAX"), __long_to_fixnum_term(PATH_MAX));

  // Define file-open flags
  g_file_open_flags = nil;
  term * p = &g_file_open_flags;

#define DEF_OPEN_FLAG(c)                                                \
  do {                                                                  \
    *p = LIST_1(define_symbol_macro(resolve_symbol_from_sz("kl:" #c),   \
                                    __long_to_fixnum_term(c)));         \
    p = &__get_cons_for_write(*p)->second;                              \
  } while (0)

  DEF_OPEN_FLAG(O_RDONLY);
  DEF_OPEN_FLAG(O_WRONLY);
  DEF_OPEN_FLAG(O_RDWR);
  DEF_OPEN_FLAG(O_APPEND);
  DEF_OPEN_FLAG(O_CREAT);
  DEF_OPEN_FLAG(O_CLOEXEC);
  DEF_OPEN_FLAG(O_DIRECT);
  DEF_OPEN_FLAG(O_DIRECTORY);
  DEF_OPEN_FLAG(O_EXCL);
  DEF_OPEN_FLAG(O_NOATIME);
  DEF_OPEN_FLAG(O_NOCTTY);
  DEF_OPEN_FLAG(O_NOFOLLOW);
  DEF_OPEN_FLAG(O_NONBLOCK);
  DEF_OPEN_FLAG(O_SYNC);
  DEF_OPEN_FLAG(O_TRUNC);
  // Define file-open modes
  g_file_open_modes = nil;
  p = &g_file_open_modes;
  DEF_OPEN_FLAG(S_IRWXU);
  DEF_OPEN_FLAG(S_IRUSR);
  DEF_OPEN_FLAG(S_IWUSR);
  DEF_OPEN_FLAG(S_IXUSR);
  DEF_OPEN_FLAG(S_IRWXG);
  DEF_OPEN_FLAG(S_IRGRP);
  DEF_OPEN_FLAG(S_IWGRP);
  DEF_OPEN_FLAG(S_IXGRP);
  DEF_OPEN_FLAG(S_IRWXO);
  DEF_OPEN_FLAG(S_IROTH);
  DEF_OPEN_FLAG(S_IWOTH);
  DEF_OPEN_FLAG(S_IXOTH);
  // Define file-access modes
  g_file_access_modes = nil;
  p = &g_file_access_modes;
  DEF_OPEN_FLAG(F_OK);
  DEF_OPEN_FLAG(R_OK);
  DEF_OPEN_FLAG(W_OK);
  DEF_OPEN_FLAG(X_OK);

  // seek options
  define_symbol_macro(resolve_symbol_from_sz("kl:SEEK_SET"),
                      __long_to_fixnum_term(SEEK_SET));
  define_symbol_macro(resolve_symbol_from_sz("kl:SEEK_CUR"),
                      __long_to_fixnum_term(SEEK_CUR));
  define_symbol_macro(resolve_symbol_from_sz("kl:SEEK_END"),
                      __long_to_fixnum_term(SEEK_END));

  // OS clocks IDs
  define_symbol_macro(resolve_symbol_from_sz("kl:CLOCK_REALTIME"),
                      __long_to_fixnum_term(CLOCK_REALTIME));
  define_symbol_macro(resolve_symbol_from_sz("kl:CLOCK_MONOTONIC"),
                      __long_to_fixnum_term(CLOCK_MONOTONIC));
  define_symbol_macro(resolve_symbol_from_sz("kl:CLOCK_PROCESS_CPUTIME_ID"),
                      __long_to_fixnum_term(CLOCK_PROCESS_CPUTIME_ID));
  define_symbol_macro(resolve_symbol_from_sz("kl:CLOCK_THREAD_CPUTIME_ID"),
                      __long_to_fixnum_term(CLOCK_THREAD_CPUTIME_ID));

  // OS misc info
  define_symbol_macro(resolve_symbol_from_sz("os:PAGE-SIZE"),
                      __long_to_fixnum_term(getpagesize()));
  define_symbol_macro(resolve_symbol_from_sz("os:CPU-ONLINE-COUNT"),
                      __long_to_fixnum_term(sysconf(_SC_NPROCESSORS_ONLN)));
  define_symbol_macro(resolve_symbol_from_sz("os:CPU-CONFIGURED-COUNT"),
                      __long_to_fixnum_term(sysconf(_SC_NPROCESSORS_CONF)));
  define_symbol_macro(resolve_symbol_from_sz("os:PHYS-MEM-PAGES"),
                      __long_to_fixnum_term(sysconf(_SC_PHYS_PAGES)));
  define_symbol_macro(resolve_symbol_from_sz("os:PHYS-MEMORY"),
                      uint64_to_term((uint64_t)sysconf(_SC_PHYS_PAGES) *
                                     getpagesize()));

  // LEVEL-1 CPU cache - is distingued between instructions cache and data cache
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL1-ICACHE-SIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL1_ICACHE_SIZE)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL1-ICACHE-ASSOC"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL1_ICACHE_ASSOC)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL1-ICACHE-LINESIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL1_ICACHE_LINESIZE)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL1-DCACHE-SIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL1_DCACHE_SIZE)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL1-DCACHE-ASSOC"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL1_DCACHE_ASSOC)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL1-DCACHE-LINESIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL1_DCACHE_LINESIZE)));

  // LEVEL-2 CPU cache
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL2-CACHE-SIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL2_CACHE_SIZE)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL2-CACHE-ASSOC"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL2_CACHE_ASSOC)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL2-CACHE-LINESIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL2_CACHE_LINESIZE)));

  // LEVEL-3 CPU cache
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL3-CACHE-SIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL3_CACHE_SIZE)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL3-CACHE-ASSOC"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL3_CACHE_ASSOC)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL3-CACHE-LINESIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL3_CACHE_LINESIZE)));

  // LEVEL-4 CPU cache
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL4-CACHE-SIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL4_CACHE_SIZE)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL4-CACHE-ASSOC"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL4_CACHE_ASSOC)));
  define_symbol_macro(resolve_symbol_from_sz("os:LEVEL4-CACHE-LINESIZE"),
                      __long_to_fixnum_term(sysconf(_SC_LEVEL4_CACHE_LINESIZE)));

  // Misc sockets info
  define_symbol_macro(resolve_symbol_from_sz("kl:FD_SETSIZE"),
                      __long_to_fixnum_term(FD_SETSIZE));
}
