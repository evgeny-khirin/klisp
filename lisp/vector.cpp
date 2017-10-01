///-------------------------------------------------------------------
/// Copyright (c) 2009-2011 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : vector.cpp
/// Author  : Evgeny Khirin <>
/// Description : Erlios Lisp vector.
///-------------------------------------------------------------------
#include "klisp.h"

#include <algorithm>

//-------------------------------------------------------------------
// comparison
//-------------------------------------------------------------------
namespace {
  struct term_less {
    term fn;

  private:
    term_less();

  public:
    term_less(term fn) {
      this->fn = fn;
    }

    bool operator() (term l, term r) const {
      return term_to_long(FUNCALL(fn, l, r)) < 0;
    }
  };
} // namespace


//-------------------------------------------------------------------
// Function: (vector-sort seq &optional (test 'compare) start count) ==> seq
// Test is (lambda (x y)) and must return negative fixnum if x < y, zero if x = y
// and positive fixnum if x > y.
//-------------------------------------------------------------------
term lisp_vector_sort(term seq, term test, term start, term count) {
  vector_t * s = get_vector_for_write(seq);
  long i;
  if (is_null(start)) {
    i = 0;
  } else {
    i = term_to_long(start);
    if (i < 0 || i > s->size) {
      lisp_signal(g_out_of_range, start);
    }
  }
  long size;
  if (is_null(count)) {
    size = s->size - i;
  } else {
    size = term_to_long(count);
    if (size > s->size - i) {
      lisp_signal(g_out_of_range, count);
    }
  }
  term_less less(test);
  std::sort(s->data + i, s->data + size, less);
  return seq;
}

//-------------------------------------------------------------------
// Function: (vector-stable-sort seq &optional (test 'compare) start count) ==> seq
// Test is (lambda (x y)) and must return negative fixnum if x < y, zero if x = y
// and positive fixnum if x > y.
//-------------------------------------------------------------------
term lisp_vector_stable_sort(term seq, term start, term count, term test) {
  vector_t * s = get_vector_for_write(seq);
  long i;
  if (is_null(start)) {
    i = 0;
  } else {
    i = term_to_long(start);
    if (i < 0 || i > s->size) {
      lisp_signal(g_out_of_range, start);
    }
  }
  long size;
  if (is_null(count)) {
    size = s->size - i;
  } else {
    size = term_to_long(count);
    if (size > s->size - i) {
      lisp_signal(g_out_of_range, count);
    }
  }
  term_less less(test);
  std::stable_sort(s->data + i, s->data + size, less);
  return seq;
}

