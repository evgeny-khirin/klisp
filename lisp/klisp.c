///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : klisp.c
/// Author  : Evgeny Khirin <>
/// Description : Khirin Lisp (KL) entry point.
///-----------------------------------------------------------------------------

#include "klisp.h"

#include <stdio.h>

//------------------------------------------------------------------------------
// exec_repl
//------------------------------------------------------------------------------
static term exec_repl(void * p) {
  return repl(vm_get_dynamic(g_package_var));
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
int main(int argc, const char * * argv) {
  vm_t vm;
  vm_init(&vm);
  g_vm = &vm;
  // lisp_global_init_1 MUST be called before any other function in program
  lisp_global_init(argc, argv);
  exec_in_lisp_context(exec_repl, NULL, 1);
  return 0;
}
