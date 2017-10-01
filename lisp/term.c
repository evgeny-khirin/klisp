///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : term.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp terms.
///-------------------------------------------------------------------

#include "klisp.h"

//-------------------------------------------------------------------
// is_numeric_type
//-------------------------------------------------------------------
#define is_numeric_type(tp)   ((tp) == fixnum_e || (tp) == bigint_e || (tp) == double_e)

//-------------------------------------------------------------------
// term_cmp
// returns negative if l < r,
// returns 0 if l = r,
// returns positive if l > r.
// Comparisons of all containers are done lexicographically. This means that two
// containers are equal if and only if their size is the same and each element
// is equal to the element in corresponding location in the other container.
//-------------------------------------------------------------------
long term_cmp(term l, term r) {
  term_type_e tp = get_term_type(l);
  term_type_e rtp = get_term_type(r);
  if (tp >= __first_non_comparable_type__) {
    lisp_signal(g_invalid_arg, l);
  }
  if (rtp >= __first_non_comparable_type__) {
    lisp_signal(g_invalid_arg, r);
  }
  if (tp != rtp) {
    if (!is_numeric_type(tp)) {
      return tp - rtp;
    }
    if (!is_numeric_type(rtp)) {
      return tp - rtp;
    }
  }
  // both terms are of same type
  switch (tp) {
  case null_e:
    return 0;
  case fixnum_e:
    switch (rtp) {
    case fixnum_e:
      return __fixnum_term_to_long(l) - __fixnum_term_to_long(r);
    case bigint_e:
      return -mpz_cmp_si(__term_to_bigint(r)->mpz, __fixnum_term_to_long(l));
    case double_e:
      {
        long lf = __fixnum_term_to_long(l);
        double rd = __term_to_double(r);
        if (lf < rd) {
          return -1;
        }
        if (lf > rd) {
          return 1;
        }
        return 0;
      }
    default:
      SIGNAL_INTERNAL_ERROR();
    }
  case bigint_e:
    switch (rtp) {
    case fixnum_e:
      return mpz_cmp_si(__term_to_bigint(l)->mpz, __fixnum_term_to_long(r));
    case bigint_e:
      return mpz_cmp(__term_to_bigint(l)->mpz, __term_to_bigint(r)->mpz);
    case double_e:
      return mpz_cmp_d(__term_to_bigint(l)->mpz, __term_to_double(r));
    default:
      SIGNAL_INTERNAL_ERROR();
    }
  case double_e:
    switch (rtp) {
    case fixnum_e:
      {
        double ld = __term_to_double(l);
        long rf = __fixnum_term_to_long(r);
        if (ld < rf) {
          return -1;
        }
        if (ld > rf) {
          return 1;
        }
        return 0;
      }
    case bigint_e:
      return -mpz_cmp_d(__term_to_bigint(r)->mpz, __term_to_double(l));
    case double_e:
      {
        double ld = __term_to_double(l);
        double rd = __term_to_double(r);
        if (ld < rd) {
          return -1;
        }
        if (ld > rd) {
          return 1;
        }
        return 0;
      }
    default:
      SIGNAL_INTERNAL_ERROR();
    }
  case symbol_e:
    {
      const symbol_t * ls = __term_to_symbol(l);
      const symbol_t * rs = __term_to_symbol(r);
      if (!eq(ls->package, rs->package)) {
        return
          term_cmp(is_null(ls->package) ? nil :
                                          package_get_name(ls->package),
                   is_null(rs->package) ? nil :
                                          package_get_name(rs->package));
      }
      return term_cmp(ls->name, rs->name);
    }
  case binary_e:
    return binary_cmp(__get_binary_for_read(l),
                      __get_binary_for_read(r));
  case ustring_e:
    return ustring_cmp(__get_ustring_for_read(l),
                       __get_ustring_for_read(r));
  case cons_e:
    return cons_cmp(__get_cons_for_read(l),
                    __get_cons_for_read(r));
  case vector_e:
    return vector_cmp(__get_vector_for_read(l),
                      __get_vector_for_read(r));
  default:
    SIGNAL_INTERNAL_ERROR();
  }
}

//-------------------------------------------------------------------
// hash_code
//-------------------------------------------------------------------
long term_hash_code(term t) {
  term_type_e tp = get_term_type(t);
  switch (tp) {
  case null_e:
    return 0;
  case symbol_e:
    return binary_hash_code(__get_binary_for_read(__term_to_symbol(t)->name));
  case bigint_e:
    return bigint_hash_code(__term_to_bigint(t));
  case fixnum_e:
    return __long_to_hash_code(__fixnum_term_to_long(t));
  case double_e:
    return __long_to_hash_code(__term_to_double(t));
  case binary_e:
    return binary_hash_code(__get_binary_for_read(t));
  case ustring_e:
    return ustring_hash_code(__get_ustring_for_read(t));
  case cons_e:
    return cons_hash_code(__get_cons_for_read(t));
  case vector_e:
    return vector_hash_code(__get_vector_for_read(t));
  case lambda_e:
  case macro_e:
  case custom_e:
    lisp_signal(g_invalid_arg, t);
    break;
  }
  SIGNAL_INTERNAL_ERROR();
}
