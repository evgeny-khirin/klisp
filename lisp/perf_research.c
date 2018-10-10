///-------------------------------------------------------------------
/// Copyright (c) 2009-2011 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : perf_research.c
/// Author  : Evgeny Khirin <>
/// Description : Tests for performance research.
///-------------------------------------------------------------------

#include "perf_research.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>

//--------------------------------------------------------------------------
// GLOBAL MACROS
//--------------------------------------------------------------------------
#define BIG_LOOPS     (100 * 1000 * 1000)
#define SMALL_LOOPS   (1000 * 1000)

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
void timer_value(struct timespec * ts) {
  clock_gettime(CLOCK_MONOTONIC, ts);
}

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
double timer_diff(struct timespec * start_ts, struct timespec * end_ts) {
  uint64_t start = (uint64_t)start_ts->tv_sec * 1000 * 1000 * 1000 + start_ts->tv_nsec;
  uint64_t end = (uint64_t)end_ts->tv_sec * 1000 * 1000 * 1000 + end_ts->tv_nsec;
  return ((double)end - start) / (1000 * 1000 * 1000);
}

//-------------------------------------------------------------------
// Global variables
//-------------------------------------------------------------------
volatile pr_void_fn_ptr g_pr_f;
jmp_buf g_pr_jmp_buf;
int (* volatile psetjmp)(jmp_buf) = setjmp;
int (* volatile psigsetjmp)(sigjmp_buf, int) = __sigsetjmp;

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
void test_funcall() {
  struct timespec start, end;
  long i = 0;

  g_pr_f = pr_get_void_fn();

  // direct call of CDECL function
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    pr_void_fn(i);
  }
  timer_value(&end);
  printf("direct CDECL function call time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // direct call of STDCALL function
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    pr_void_stdcall_fn(i);
  }
  timer_value(&end);
  printf("direct STDCALL function call time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // direct vargs call
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    pr_void_varg_fn(3, i, i, i);
  }
  timer_value(&end);
  printf("direct vargs with 3 args function call time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // indirect call
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    g_pr_f(i);
  }
  timer_value(&end);
  printf("indirect function call time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // sum of two long
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    sum_fn(i, i);
  }
  timer_value(&end);
  printf("Sum of two integers time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // sum of two long with new Lisp function call protocol
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    vm_t vm;
    long args[2];
    args[0] = i;
    args[1] = i;
    lisp_fun_proto_1(&vm, 2, args);
  }
  timer_value(&end);
  printf("New Lisp protocol 1, sum of two integers time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // sum of two long with new Lisp function call protocol
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    long args[2];
    args[0] = i;
    args[1] = i;
    lisp_fun_proto_1(vm_get_current(), 2, args);
  }
  timer_value(&end);
  printf("New Lisp protocol 1 with vm_get_current(), sum of two integers time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // sum of two long with new Lisp function call protocol
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    long args[2];
    args[0] = i;
    args[1] = i;
    lisp_fun_proto_2(2, args);
  }
  timer_value(&end);
  printf("New Lisp protocol 2, sum of two integers time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // setjmp
  timer_value(&start);
  for (i = 0; i < SMALL_LOOPS; ++i) {
    if (psetjmp(g_pr_jmp_buf) == 0) {
      pr_void_fn(i);
    }
  }
  timer_value(&end);
  printf("setjmp time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), SMALL_LOOPS / timer_diff(&start, &end) / 1000000);

  // sigsetjmp with signal mask
  timer_value(&start);
  for (i = 0; i < SMALL_LOOPS; ++i) {
    if (psigsetjmp(g_pr_jmp_buf, 1) == 0) {
      pr_void_fn(i);
    }
  }
  timer_value(&end);
  printf("sigsetjmp with signal mask time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), SMALL_LOOPS / timer_diff(&start, &end) / 1000000);

  // sigsetjmp without signal mask
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    if (psigsetjmp(g_pr_jmp_buf, 0) == 0) {
      pr_void_fn(i);
    }
  }
  timer_value(&end);
  printf("sigsetjmp without signal mask time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // setjmp/longjmp
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    if (setjmp(g_pr_jmp_buf) == 0) {
      pr_throw_longjmp();
    }
  }
  timer_value(&end);
  printf("setjmp/longjmp time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // retcode
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    // simulate establish code
    pr_void_fn(i);
    // check retcode
    if (pr_throw_retcode() == -2) {
      // simulate stack unwinding
      pr_void_fn(i);
    }
  }
  timer_value(&end);
  printf("retcode establish time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // retcode + unwind
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    // simulate establish code
    pr_void_fn(i);
    // check retcode
    if (pr_throw_retcode() == -1) {
      // simulate stack unwinding
      pr_void_fn(i);
    }
  }
  timer_value(&end);
  printf("retcode establish + unwinding time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);
}

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
void test_gc() {
  struct timespec start, end;
  long i;

  // lisp_alloc
  timer_value(&start);
  for (i = 0; i < SMALL_LOOPS; ++i) {
    lisp_alloc(sizeof(cons_t), NULL);
  }
  timer_value(&end);
  printf("GC alloc time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), SMALL_LOOPS / timer_diff(&start, &end) / 1000000);

  // lisp_alloc_atomic
  timer_value(&start);
  for (i = 0; i < SMALL_LOOPS; ++i) {
    lisp_alloc_atomic(sizeof(cons_t), NULL);
  }
  timer_value(&end);
  printf("GC alloc atomic time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), SMALL_LOOPS / timer_diff(&start, &end) / 1000000);

  // malloc
  timer_value(&start);
  for (i = 0; i < SMALL_LOOPS; ++i) {
    // Pointer p is declared and used here in order to fix error:
    // ignoring return value of malloc, declared with attribute
    // warn_unused_result [-Werror=unused-result]
    void * p = malloc(sizeof(cons_t));
    (void)p;
  }
  timer_value(&end);
  printf("malloc time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), SMALL_LOOPS / timer_diff(&start, &end) / 1000000);
}

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
void test_gmp_mpz() {
  struct timespec start, end;
  long i;
  mpz_t m;

  mpz_init(m);

  printf("mpz_t size %d\n", (int)sizeof(mpz_t));

  // increment mpz_t
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    mpz_add_ui(m, m, 1);
  }
  timer_value(&end);
  printf("mpz_t inc result %s, time %g sec, throughput %g million ops/sec\n",
          mpz_get_str(NULL, 10, m),
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);
}

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
void test_make_binary() {
  struct timespec start, end;
  long i;

  timer_value(&start);
  for (i = 0; i < SMALL_LOOPS; ++i) {
    make_binary_from_sz("abcdefghtklk");
  }
  timer_value(&end);
  printf("make_binary_from_sz time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), SMALL_LOOPS / timer_diff(&start, &end) / 1000000);
}

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
void test_eval() {
  struct timespec start, end;
  long i;

  // evaluate nil
  term exp = nil;
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    eval(exp);
  }
  timer_value(&end);
  printf("(eval nil) time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // evaluate (quote a)
  exp = LIST_2(resolve_symbol_from_sz("quote"), resolve_symbol_from_sz("a"));
  timer_value(&start);
  for (i = 0; i < BIG_LOOPS; ++i) {
    eval(exp);
  }
  timer_value(&end);
  printf("(eval ''a) time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), BIG_LOOPS / timer_diff(&start, &end) / 1000000);

  // evaluate (if 1 2 3)
  exp = LIST_4(resolve_symbol_from_sz("if"),
               long_to_term(1), long_to_term(2), long_to_term(3));
  timer_value(&start);
  for (i = 0; i < SMALL_LOOPS; ++i) {
    eval(exp);
  }
  timer_value(&end);
  printf("(eval '(if 1 2 3)) time %g sec, throughput %g million ops/sec\n",
          timer_diff(&start, &end), SMALL_LOOPS / timer_diff(&start, &end) / 1000000);
}

//--------------------------------------------------------------------------
// BINARY_LOOPS
//--------------------------------------------------------------------------
#define BINARY_LOOPS    (1024 * 1024)

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
void test_binary() {
  struct timespec start, end;
  long i;

  // test binary append byte throughput
  timer_value(&start);
  binary_t b;
  binary_init(&b);
  binary_ensure_capacity(&b, BINARY_LOOPS);
  for (i = 0; i < BINARY_LOOPS; ++i) {
    binary_append_int8(&b, 1);
  }
  timer_value(&end);
  printf("binary_append_int8 time %g sec, throughput %g MB/sec\n",
          timer_diff(&start, &end), 1.0 / timer_diff(&start, &end));
}

//--------------------------------------------------------------------------
// Checks that va_list is passed by reference
//--------------------------------------------------------------------------
int print_arg(int i, va_list ap) {
  int x = va_arg(ap, int);
  printf("va_arg %d: %d\n", i, x);
  return x;
}

void do_print(int n, ...) {
  int i;
  va_list ap;
  va_start(ap, n);
  for (i = 0; i < n; ++i) {
    int x = print_arg(i, ap);
    if (x != i + 1) {
      printf("Seems that va_list is not passed by reference: expected %d, actual %d\n",
             i + 1, x);
      abort();
    }
  }
  va_end(ap);
}

void test_vargs() {
  do_print(3, 1, 2, 3);
}

//--------------------------------------------------------------------------
// internal function
//--------------------------------------------------------------------------
static term test_main(void * p) {
  test_vargs();
  test_funcall();
  test_gc();
  test_gmp_mpz();
  test_make_binary();
  test_eval();
  test_binary();
  return nil;
}

//--------------------------------------------------------------------------
// Public function
//--------------------------------------------------------------------------
int main(int argc, const char * * argv) {
  vm_t vm;
  vm_init(&vm);
  g_vm = &vm;
  // lisp_global_init MUST be called before any other function in program
  lisp_global_init(argc, argv);
  exec_in_lisp_context(test_main, NULL, 1);
  return 0;
}
