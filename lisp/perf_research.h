///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : perf_research.h
/// Author  : Evgeny Khirin <>
/// Description : Helper functions for performance research. Put in separate file
/// in order to prevent loop optimizations and inlining.
///-------------------------------------------------------------------
#ifndef __perf_research_h__
#define __perf_research_h__

#include "klisp.h"

#include <setjmp.h>

//--------------------------------------------------------------------------
// STDCALL
//--------------------------------------------------------------------------
#if defined(__x86_64__)
#define STDCALL
#elif defined(__i386__)
#define STDCALL __attribute__((stdcall))
#else
#error Unknown CPU architecture
#endif

//-------------------------------------------------------------------
// Global variables
//-------------------------------------------------------------------
extern jmp_buf g_pr_jmp_buf;

//-------------------------------------------------------------------
// Global functions
//-------------------------------------------------------------------
long sum_fn(long x, long y);

long lisp_fun_proto_1(vm_t * vm, long nargs, long * args);
long lisp_fun_proto_2(long nargs, long * args);

long pr_void_fn(long x);

long STDCALL pr_void_stdcall_fn(long x);

void pr_void_varg_fn(size_t n, ...);

typedef long (* pr_void_fn_ptr)(long x);

pr_void_fn_ptr pr_get_void_fn();

void pr_throw_longjmp();

long pr_throw_retcode();

#endif // __perf_research_h__
