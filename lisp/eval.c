///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2011 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : eval.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp evaluation functions.
///-----------------------------------------------------------------------------

#include "klisp.h"

#include <stddef.h>
#include <stdio.h>
#include <alloca.h>

//==============================================================================
// Special forms
//==============================================================================
static term g_declare = nil;
static term g_progn = nil;
static term g_if = nil;
static term g_lambda = nil;
static term g_macro = nil;
static term g_let = nil;
static term g_let_star = nil;
static term g_setq = nil;
static term g_tagbody = nil;
static term g_go = nil;
static term g_catch = nil;
static term g_handler_bind = nil;
static term g_unwind_protect = nil;
static term g_block = nil;
static term g_return_from = nil;
static term g_symbol_macrolet = nil;
static term g_dynamic_let = nil;
static term g_flet = nil;
static term g_labels = nil;
static term g_macrolet = nil;
static term g_throw = nil;
static term g_multiple_value_call = nil;
static term g_multiple_value_bind = nil;

static int is_special_form(term x) {
  return eq(x, g_declare) || eq(x, g_progn) || eq(x, g_if) || eq(x, g_lambda) ||
    eq(x, g_macro) || eq(x, g_let) || eq(x, g_let_star) || eq(x, g_setq) || eq(x, g_tagbody) ||
    eq(x, g_go) || eq(x, g_catch) || eq(x, g_handler_bind) || eq(x, g_unwind_protect) ||
    eq(x, g_block) || eq(x, g_return_from) || eq(x, g_symbol_macrolet) ||
    eq(x, g_dynamic_let) || eq(x, g_flet) || eq(x, g_labels) ||
    eq(x, g_macrolet) || eq(x, g_throw) || eq(x, g_multiple_value_call) ||
    eq(x, g_multiple_value_bind);
}

//==============================================================================
// Global variables
//==============================================================================
// lambda list keywords
static term g_optional;
static term g_rest;
static term g_key;

// embedded compiler macros
static term g_eq = nil;
static term g_zerop = nil;
static term g_null = nil;
static term g_not = nil;
static term g_one_minus = nil;            // 1-
static term g_one_plus = nil;             // 1=
static term g_num_equal = nil;            // %=
static term g_num_not_equal = nil;        // %/=
static term g_num_less = nil;             // %<
static term g_num_less_equal = nil;       // %<=
static term g_num_greater = nil;          // %>
static term g_num_greater_equal = nil;    // %>=
static term g_plus = nil;                 // %+
static term g_mul = nil;                  // %*
static term g_neg = nil;                  // %neg
static term g_minus = nil;                // %-
static term g_div = nil;                  // %div

// other public functions and macros, used by compiler
static term g_symbol_function = nil;
static term g_list = nil;
static term g_ignore = nil;

// private Lisp package symbols
static term g_global_function_name = nil;

// compiler internal symbols
static term g_let_not_tail = nil;
static term g_progn_tail = nil;
static term g_param_var = nil;   // Has form: (param-var name usage-count)
static term g_local_var = nil;   // Has form: (local-var name usage-count)
static term g_local_fun = nil;   // Has form: (local-fun name fn usage-count)
static term g_literal_closure = nil; // Has form: (literal-closure name fn usage-count)
static term g_local_var_ref = nil;
static term g_param_var_ref = nil;
static term g_deep_ref = nil;
static term g_dynamic_ref = nil;
static term g_local_var_set = nil;
static term g_param_var_set = nil;
static term g_deep_set = nil;
static term g_dynamic_set = nil;
static term g_local_tag = nil;
static term g_non_local_tag = nil;
static term g_unbound_special_form_marker = nil;
static term g_make_closure = nil;
static term g_call = nil;
static term g_call_c_fun = nil;
static term g_alloca = nil;
static term g_vm_catch_frame_install = nil;
static term g_vm_catch_frame_uninstall = nil;
static term g_vm_get_catch_value = nil;
static term g_vm_unwind_frame_install = nil;
static term g_vm_unwind_frame_uninstall = nil;
static term g_unwind_callback = nil;
static term g_vm_handler_frame_install = nil;
static term g_vm_handler_frame_uninstall = nil;
static term g_sigsetjmp = nil;
static term g_vm_set_dynamic_binding = nil;
static term g_unbound_dynamic_marker = nil;
static term g_clear_values = nil;
static term g_vm_set_new_values = nil;
static term g_vm_advance_values = nil;
static term g_vm_call_with_values = nil;
static term g_vm_restore_values = nil;
static term g_vm_set_new_values_bind = nil;

// macro environment class name
static term g_macro_env_class_name = nil;

// -FIXNUM_MIN
static term g_positive_fixnum_min = nil;

//==============================================================================
// Machine code generator
//==============================================================================
//==============================================================================
// Function frame, used by compiler
//==============================================================================
//------------------------------------------------------------------------------
// struct frame_t
//------------------------------------------------------------------------------
typedef struct frame_t  frame_t;

struct frame_t {
  frame_t *       next;
  frame_t *       peer;         // Parallel frame for lambda list. Used to
                                // correctly setup parameters usage count, when
                                // compiling lambda for optional and keyword arguments.
  vector_t        param_vars;
  vector_t        local_vars;
  term            ignores;      // ignores declared with (declare (ignore ...))
  long            local_bind_size;
  long            call_frame_size;
  long            max_call_frame_size;
  long            temp_var_id;     // Used by code generator to store temporary
                                   // values
  long            temp_vars_count; // Used by code generator to produce correct
                                   // stack frame size
  function_t *    lfn;             // Used by code generator
  int             create_environment;
  int             is_closure;
  unsigned int    dl_id;        // Used by code generator to generate
  unsigned int    dl_capacity;  // dynamic labels
};

//------------------------------------------------------------------------------
// frame_init
//------------------------------------------------------------------------------
static void frame_init(frame_t * f, frame_t * next, frame_t * peer) {
  f->next = next;
  f->peer = peer;
  vector_init(&f->param_vars);
  vector_init(&f->local_vars);
  f->ignores = nil;
  f->local_bind_size = 0;
  f->call_frame_size = 0;
  f->max_call_frame_size = 0;
  f->temp_var_id = 0;
  f->temp_vars_count = 0;
  f->lfn = NULL;
  f->create_environment = 0;
  f->is_closure = 0;
  f->dl_id = 0;
  f->dl_capacity = 0;
}

//------------------------------------------------------------------------------
// frame_add_local_binding
//------------------------------------------------------------------------------
static void frame_add_local_binding(frame_t * f, term t, int incr_bind_size) {
  vector_append_term(&f->local_vars, t);
  if (incr_bind_size) {
    f->local_bind_size += 1;
  }
}

//==============================================================================
// Compiler error handling
//==============================================================================
//------------------------------------------------------------------------------
// struct cc_state_t.
// Compiler state.
//------------------------------------------------------------------------------
typedef struct cc_state_t cc_state_t;

struct cc_state_t {
  function_t *  curr_lfn;          // currently compiling function
  long          symbol_id;         // used to generate symbols by compiler
  long          depth_of_deep_ops; // helps to detect closures during compilation
};

static void cc_state_t_init(cc_state_t * s) {
  s->curr_lfn = NULL;
  s->symbol_id = 0;
  s->depth_of_deep_ops = 0;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void compiler_error(cc_state_t * state, const char * msg, term t) {
  lisp_fprintf_sz(vm_get_dynamic(g_stderr_var),
                  "\nError: %s ~s, in function ~s", msg, t, state->curr_lfn->name);
  lisp_signal(g_syntax_error, t);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void compiler_warning(cc_state_t * state, const char * msg, term t) {
  lisp_fprintf_sz(vm_get_dynamic(g_stderr_var),
                  "\nWarning: %s ~s, in function ~s", msg, t, state->curr_lfn->name);
}

//===================================================================
// Native code generator
//===================================================================
//------------------------------------------------------------------------------
// internal function
// returns frame, where arguments are stored
//------------------------------------------------------------------------------
static term * check_args_rt(long nargs, const term * args, const function_t * lfn,
                            term * frame, long check_args) {
  const long origin_nargs = nargs;
  term * const origin_frame = frame;
  // parse required arguments
  if (nargs < lfn->nreq_args) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  // copy required arguments to frame
  long i;
  for (i = 0; i < lfn->nreq_args; ++i) {
    frame[i] = args[i];
  }
  if (!check_args) {
    if (lfn->nopt_args != 0 || lfn->has_rest_arg || lfn->nkey_args != 0) {
      // Only lamba lists functions for calculating optional and key arguments
      // do not require arguments parser. Compiler generates lambdas with required
      // arguments only and runtime ensures that number of actual arguments is
      // equal or more than required.
      SIGNAL_INTERNAL_ERROR();
    }
    return frame;
  }
  nargs -= lfn->nreq_args;
  args += lfn->nreq_args;
  frame += lfn->nreq_args;
  // parse &optional arguments
  if (lfn->nopt_args != 0) {
    if (nargs >= lfn->nopt_args) {
      // there is enough actual arguments, so just copy them
      for (i = 0; i < lfn->nopt_args; ++i) {
        frame[i] = args[i];
      }
      nargs -= lfn->nopt_args;
      args += lfn->nopt_args;
      frame += lfn->nopt_args;
    } else {
      for (i = 0; i < nargs; ++i) {
        frame[i] = args[i];
      }
      nargs = 0;
      args = NULL;
      // calculate missed optional arguments
      for (; i < lfn->nopt_args; ++i) {
        function_t * fn = term_to_lambda(lfn->opt_arg_funs[i]);
        frame[i] = fn->bcode(lfn->nreq_args + i, origin_frame);
      }
      frame += lfn->nopt_args;
    }
  }
  // parse &rest argument
  if (lfn->has_rest_arg) {
    *frame++ = lisp_list(nargs, args);
    if (lfn->nkey_args == 0) {
      nargs = 0;
      args = NULL;
    }
  }
  if (lfn->nkey_args == 0) {
    // there is no &key arguments, ensure that everything is checked
    if (nargs != 0) {
      lisp_signal(g_invalid_args_count, long_to_term(origin_nargs));
    }
    return origin_frame;
  }
  // parse &key arguments
  if ((nargs & 1) != 0) {
    // ensure that number of arguments is even
    lisp_signal(g_invalid_args_count, long_to_term(origin_nargs));
  }
  long found = 0;
  term key_arg_names = lfn->key_arg_names;
  long args_till_keys = frame - origin_frame;
  for (i = 0; i < lfn->nkey_args; ++i) {
    term name = car(key_arg_names);
    key_arg_names = cdr(key_arg_names);
    long j;
    for (j = 0; j < nargs; j += 2) {
      if (eq(args[j], name)) {
        frame[i] = args[j + 1];
        found += 2;
        break;
      }
    }
    if (j == nargs) {
      // argument is not supplied
      function_t * fn = term_to_lambda(lfn->key_arg_funs[i]);
      frame[i] = fn->bcode(args_till_keys + i, origin_frame);
    }
  }
  if (found != nargs) {
    lisp_signal(g_invalid_key_args, lisp_list(nargs, args));
  }
  return origin_frame;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static closure_env_t * create_environment_rt(long nargs, const term * args,
                                             const function_t * lfn, long check_args) {
  closure_env_t * f = lisp_alloc(sizeof(closure_env_t) + lfn->frame_size * sizeof(term), NULL);
  f->next = lfn->enclosed_env;
  f->size = lfn->frame_size;
  check_args_rt(nargs, args, lfn, f->data, check_args);
  return f;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void vm_catch_frame_install(catch_frame_t * f, term tag) {
  catch_frame_install(f, vm_get_current(), tag);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term vm_catch_frame_uninstall(catch_frame_t * f, term value) {
  catch_frame_uninstall(f, vm_get_current());
  return value;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void vm_unwind_frame_install(lisp_unwind_frame_t * f, lisp_unwind_callback callback,
                                    void * env, void * params, void * local_vars) {
  lisp_unwind_frame_install(f, vm_get_current(), callback, env, params, local_vars);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term vm_unwind_frame_uninstall(term value) {
  vm_t * vm = vm_get_current();
  lisp_unwind_frame_t * f = (lisp_unwind_frame_t *)vm->sig_frame;
  vm->sig_frame = f->header.next;
  if (!f->done) {
    f->done = 1;
    values_immune(vm->values);
    UNWIND_PROTECT_BEGIN((c_unwind_callback)values_unimmune, vm->values) {
      f->callback(f->env, f->params, f->local_vars);
    } UNWIND_PROTECT_END;
  }
  return value;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void vm_handler_frame_install(sig_lisp_handler_frame_t * f, term handler) {
  sig_lisp_handler_frame_install(f, vm_get_current(), handler);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term vm_handler_frame_uninstall(term value) {
  vm_frame_uninstall(vm_get_current());
  return value;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static values_t * vm_set_new_values(values_t * new_values, term * data, long capacity) {
  vm_t * vm = vm_get_current();
  values_t * old_values = vm->values;
  values_init(new_values, data, capacity);
  vm->values = new_values;
  return old_values;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void vm_advance_values(term value) {
  vm_t * vm = vm_get_current();
  if (vm->values->capacity == 0) {
    return;
  }
  if (vm->values->size == 0) {
    vm->values->data[0] = value;
    vm->values->data += 1;
    vm->values->capacity -= 1;
    return;
  }
  vm->values->data += vm->values->size;
  vm->values->capacity -= vm->values->size;
  vm->values->size = 0;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term vm_call_with_values(term fn, term * new_values_data, values_t * old_values) {
  vm_t * vm = vm_get_current();
  values_t * new_values = vm->values;
  // restore old values
  vm->values = old_values;
  long nargs = new_values->data - new_values_data + new_values->size;
  function_t * lfn = term_to_function(fn);
  // clear values because fn may be C routine
  __vm_clear_values();
  return lfn->bcode(nargs, new_values_data);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void vm_restore_values(values_t * old_values) {
  vm_t * vm = vm_get_current();
  vm->values = old_values;
}

//------------------------------------------------------------------------------
// enum op_dest_e - operation destination
//------------------------------------------------------------------------------
typedef enum op_dest_e op_dest_e;

enum op_dest_e {
  none_e,
  val_reg_e,
  local_var_e,
  param_var_e,
  call_param_e
};

//------------------------------------------------------------------------------
// include real machine code generator
//------------------------------------------------------------------------------
#if defined(__x86_64__)
#include "eval_x86_64_linux.c.h"
#elif defined(__i386__)
#include "eval_x86_32_linux.c.h"
#else
#error Unknown CPU architecture
#endif

//==============================================================================
// Misc functions
//==============================================================================
//-------------------------------------------------------------------
// Internal function
//-------------------------------------------------------------------
term __deep_immune_literals(term x) {
  switch (get_term_type(x)) {
  case null_e:
  case fixnum_e:
  case symbol_e:             // do not immute symbols
  case bigint_e:             // there is no way to change numeric object in Lisp
  case double_e:             // there is no way to change numeric object in Lisp
  case lambda_e:             // there is no way to change lambda object in Lisp
  case macro_e:              // there is no way to change macro object in Lisp
    return x;
  case binary_e:
    ((binary_t *)__get_binary_for_read(x))->immutable = 1;
    return x;
  case ustring_e:
    ((ustring_t *) __get_ustring_for_read(x))->immutable = 1;
    return x;
  case cons_e:
    {
      term save_x = x;
      do {
        const cons_t * p = __get_cons_for_read(x);
        if (p->immutable) {
          // already immutable
          return save_x;
        }
        ((cons_t *)p)->immutable = 1;
        __deep_immune_literals(p->first);
        x = p->second;
      } while (is_cons(x));
      if (!is_null(x)) {
        __deep_immune_literals(x);
      }
      return save_x;
    }
  case vector_e:
    {
      const vector_t * v = __get_vector_for_read(x);
      if (v->immutable) {
        // already immutable
        return x;
      }
      ((vector_t *)v)->immutable = 1;
      long i;
      for (i = 0; i < v->size; ++i) {
        __deep_immune_literals(v->data[i]);
      }
    }
    return x;
  case custom_e:                // there is no way to write literal custom object
    SIGNAL_INTERNAL_ERROR();
  }
  SIGNAL_INTERNAL_ERROR();
}

//==============================================================================
// Compiler
//==============================================================================
//------------------------------------------------------------------------------
// Forward declarations
//------------------------------------------------------------------------------
static term compile_exp(cc_state_t * state, term exp, frame_t * env, int is_tail);
static term apply_with_macro_env(term fn, term args, frame_t * macro_env);
static term compile_lambda(cc_state_t * state, term lambda_list,
                           term body, frame_t * env, int fn_is_lambda,
                           term fn, int immute_literals, int check_args,
                           frame_t * peer_frame, int is_unwind_callback);
static term compile_invocation(cc_state_t * state, term exp, term fname, term args,
                               frame_t * env, int is_tail);

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term resolve_var_ref(cc_state_t * state, term sym, frame_t * env,
                            int for_macroexpand) {
  if (symbol_is_keyword(term_to_symbol(sym))) {
    if (for_macroexpand) {
      // keywords are evaluated to themself and therefore are not expanded
      return LIST_2(g_local_var, sym);
    }
    return sym;
  }
  frame_t * saved_env = env;
  long frame = 0;
  while (env != NULL) {
    long offset = env->local_bind_size - 1;
    long i;
    // resolve local variables
    for (i = env->local_vars.size - 1; i >= 0; --i) {
      term var_descr = env->local_vars.data[i];
      if (eq(var_descr, g_unbound_special_form_marker)) {
        // variable is marker of one of specail forms above, which is out of
        // scope. Offset is not decremented.
        continue;
      }
      term var_kind = car(var_descr);
      if (eq(var_kind, g_local_var)) {
        // variable is real variable
        if (!eq(sym, cadr(var_descr))) {
          --offset;
          continue;
        }
        // found matching
        if (for_macroexpand) {
          return var_descr;
        }
        // increase usage count
        cons_t * usage = get_cons_for_write(cddr(var_descr));
        usage->first = long_to_term(term_to_long(usage->first) + 1);
        if (frame == 0) {
          // shallow reference
          return LIST_2(g_local_var_ref, long_to_term(offset));
        }
        // deep reference
        if (frame > state->depth_of_deep_ops) {
          state->depth_of_deep_ops = frame;
        }
        return LIST_3(g_deep_ref, long_to_term(frame),
                      long_to_term(env->param_vars.size + offset));
      }
      if (eq(var_kind, g_symbol_macrolet)) {
        if (!eq(cadr(var_descr), sym)) {
          // variable is symbol-macrolet. offset is not decremented
          continue;
        }
        if (for_macroexpand) {
          return var_descr;
        }
        // increase usage count
        cons_t * usage = get_cons_for_write(cdddr(var_descr));
        usage->first = long_to_term(term_to_long(usage->first) + 1);
        // expand symbol macro
        return compile_exp(state, caddr(var_descr), saved_env, 0);
      }
      if (eq(var_kind, g_local_tag) || eq(var_kind, g_non_local_tag)) {
        // variable is tag. offset is not decremented
        continue;
      }
      if (eq(var_kind, g_catch) || eq(var_kind, g_handler_bind) ||
          eq(var_kind, g_unwind_protect) || eq(var_kind, g_block)) {
        // variable is marker of one of special forms, which catches non-local
        // goto. Offset is not decremented
        continue;
      }
      if (eq(var_kind, g_local_fun) || eq(var_kind, g_literal_closure)) {
        term var = caddr(var_descr);
        term_type_e tp = get_term_type(var);
        if (tp == lambda_e || tp == macro_e) {
          // variable names literal function. offset is not decremented
          continue;
        }
        if (eq(car(var), g_make_closure)) {
          // variable is closure, which stored in local variable
          --offset;
          continue;
        }
        pprint(var_descr, vm_get_dynamic(g_stderr_var));
        SIGNAL_INTERNAL_ERROR();
      }
      if (eq(var_kind, g_global_function_name)) {
        // variable names currently compiling global function, and used by
        // compiler to verify recursive calls. offset is not decremented
        continue;
      }
      pprint(var_descr, vm_get_dynamic(g_stderr_var));
      SIGNAL_INTERNAL_ERROR();
    }
    // resolve parameters
    for (i = 0; i < env->param_vars.size; ++i) {
      term var_descr = env->param_vars.data[i];
      term var_kind = car(var_descr);
      if (eq(var_kind, g_param_var)) {
        // variable is real variable
        if (!eq(sym, cadr(var_descr))) {
          continue;
        }
        // found matching
        if (for_macroexpand) {
          return var_descr;
        }
        // increase usage count
        cons_t * usage = get_cons_for_write(cddr(var_descr));
        usage->first = long_to_term(term_to_long(usage->first) + 1);
        if (env->peer != NULL) {
          // increase usage count in peer frame
          term var_descr = env->peer->param_vars.data[i];
          cons_t * usage = get_cons_for_write(cddr(var_descr));
          usage->first = long_to_term(term_to_long(usage->first) + 1);
        }
        if (frame == 0) {
          // shallow reference
          return LIST_2(g_param_var_ref, long_to_term(i));
        }
        // deep reference
        if (frame > state->depth_of_deep_ops) {
          state->depth_of_deep_ops = frame;
        }
        return LIST_3(g_deep_ref, long_to_term(frame), long_to_term(i));
      }
      SIGNAL_INTERNAL_ERROR();
    }
    ++frame;
    env = env->next;
  }
  if (for_macroexpand) {
    return sym;
  }
  term macro = symbol_macro(sym, g_unbound_marker);
  if (eq(macro, g_unbound_marker)) {
    if (!symbol_is_value_bound(term_to_symbol(sym))) {
      compiler_warning(state, "undefined variable", sym);
    }
    // dynamic reference
    return LIST_2(g_dynamic_ref, sym);
  }
  // expand global symbol macro
  return compile_exp(state, macro, saved_env, 0);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term resolve_var_set(cc_state_t * state, term sym, frame_t * env, term value) {
  if (symbol_is_keyword(term_to_symbol(sym))) {
    lisp_signal(g_keyword_symbol, sym);
  }
  long frame = 0;
  while (env != NULL) {
    long offset = env->local_bind_size - 1;
    long i;
    for (i = env->local_vars.size - 1; i >= 0; --i) {
      term var_descr = env->local_vars.data[i];
      if (eq(var_descr, g_unbound_special_form_marker)) {
        // variable is marker of one of specail forms above, which is out of
        // scope. Offset is not decremented.
        continue;
      }
      term var_kind = car(var_descr);
      if (eq(var_kind, g_local_var)) {
        // variable is real variable
        if (!eq(sym, cadr(var_descr))) {
          --offset;
          continue;
        }
        // found matching
        // increase usage count
        cons_t * usage = get_cons_for_write(cddr(var_descr));
        usage->first = long_to_term(term_to_long(usage->first) + 1);
        if (frame == 0) {
          // shallow set
          return LIST_3(g_local_var_set, long_to_term(offset), value);
        }
        // deep set
        if (frame > state->depth_of_deep_ops) {
          state->depth_of_deep_ops = frame;
        }
        return LIST_4(g_deep_set, long_to_term(frame),
                      long_to_term(env->param_vars.size + offset),
                      value);
      }
      if (eq(var_kind, g_symbol_macrolet)) {
        // variable is symbol-macrolet. offset is not decremented
        continue;
      }
      if (eq(var_kind, g_local_tag) || eq(var_kind, g_non_local_tag)) {
        // variable is tag. offset is not decremented
        continue;
      }
      if (eq(var_kind, g_catch) || eq(var_kind, g_handler_bind) ||
          eq(var_kind, g_unwind_protect) || eq(var_kind, g_block)) {
        // variable is marker of one of special forms, which catches non-local
        // goto. Offset is not decremented
        continue;
      }
      if (eq(var_kind, g_local_fun) || eq(var_kind, g_literal_closure)) {
        term var = caddr(var_descr);
        term_type_e tp = get_term_type(var);
        if (tp == lambda_e || tp == macro_e) {
          // variable names literal function. offset is not decremented
          continue;
        }
        if (eq(car(var), g_make_closure)) {
          // variable is closure, which stored in local variable
          --offset;
          continue;
        }
        pprint(var_descr, vm_get_dynamic(g_stderr_var));
        SIGNAL_INTERNAL_ERROR();
      }
      if (eq(var_kind, g_global_function_name)) {
        // variable names currently compiling global function, and used by
        // compiler to verify recursive calls. offset is not decremented
        continue;
      }
      pprint(var_descr, vm_get_dynamic(g_stderr_var));
      SIGNAL_INTERNAL_ERROR();
    }
    // resolve parameters
    for (i = 0; i < env->param_vars.size; ++i) {
      term var_descr = env->param_vars.data[i];
      term var_kind = car(var_descr);
      if (eq(var_kind, g_param_var)) {
        // variable is real variable
        if (!eq(sym, cadr(var_descr))) {
          continue;
        }
        // found matching
        // increase usage count
        cons_t * usage = get_cons_for_write(cddr(var_descr));
        usage->first = long_to_term(term_to_long(usage->first) + 1);
        if (env->peer != NULL) {
          // increase usage count in peer frame
          term var_descr = env->peer->param_vars.data[i];
          cons_t * usage = get_cons_for_write(cddr(var_descr));
          usage->first = long_to_term(term_to_long(usage->first) + 1);
        }
        if (frame == 0) {
          // shallow set
          return LIST_3(g_param_var_set, long_to_term(i), value);
        }
        // deep set
        if (frame > state->depth_of_deep_ops) {
          state->depth_of_deep_ops = frame;
        }
        return LIST_4(g_deep_set, long_to_term(frame), long_to_term(i), value);
      }
      pprint(var_descr, vm_get_dynamic(g_stderr_var));
      SIGNAL_INTERNAL_ERROR();
    }
    ++frame;
    env = env->next;
  }
  if (!symbol_is_value_bound(term_to_symbol(sym))) {
    compiler_warning(state, "undefined variable", sym);
  }
  return LIST_3(g_dynamic_set, sym, value);
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term resolve_fun(cc_state_t * state, term sym, frame_t * env,
                        int get_literal_object_for_validation) {
  if (is_cons(sym)) {
    term op = car(sym);
    if (eq(op, g_lambda) || eq(op, g_macro)) {
      term res = compile_exp(state, sym, env, 0);
      if (is_function(res)) {
        return res;
      }
      if (!is_cons(res) || !eq(car(res), g_make_closure)) {
        SIGNAL_INTERNAL_ERROR();
      }
      if (get_literal_object_for_validation) {
        return cadr(res);
      }
      return res;
    }
    if (eq(op, g_make_closure)) {
      if (get_literal_object_for_validation) {
        return cadr(sym);
      }
      return sym;
    }
    lisp_signal(g_not_function, sym);
  }
  if (is_function(sym)) {
    // literal function
    return sym;
  }
  if (!is_symbol(sym)) {
    lisp_signal(g_not_function, sym);
  }
  long frame = 0;
  while (env != NULL) {
    long offset = env->local_bind_size - 1;
    long i;
    for (i = env->local_vars.size - 1; i >= 0; --i) {
      term var_descr = env->local_vars.data[i];
      if (eq(var_descr, g_unbound_special_form_marker)) {
        // variable is marker of one of specail forms above, which is out of
        // scope. Offset is not decremented.
        continue;
      }
      term var_kind = car(var_descr);
      if (eq(var_kind, g_local_var)) {
        // it's regular variable
        --offset;
        continue;
      }
      if (eq(var_kind, g_symbol_macrolet)) {
        // variable is symbol-macrolet. offset is not decremented
        continue;
      }
      if (eq(var_kind, g_local_tag) || eq(var_kind, g_non_local_tag)) {
        // variable is tag. offset is not decremented
        continue;
      }
      if (eq(var_kind, g_catch) || eq(var_kind, g_handler_bind) ||
          eq(var_kind, g_unwind_protect) || eq(var_kind, g_block)) {
        // variable is marker of one of special forms, which catches non-local
        // goto. Offset is not decremented
        continue;
      }
      if (eq(var_kind, g_global_function_name)) {
        // variable names currently compiling global function, and used by
        // compiler to verify recursive calls. offset is not decremented
        if (!eq(cadr(var_descr), sym)) {
          continue;
        }
        return var_descr;
      }
      if (eq(var_kind, g_local_fun) || eq(var_kind, g_literal_closure)) {
        term name = cadr(var_descr);
        term fn = caddr(var_descr);
        term_type_e tp = get_term_type(fn);
        if (!eq(name, sym)) {
          // name do not match
          if (tp == lambda_e || tp == macro_e) {
            // variable names literal function. offset is not decremented
            continue;
          }
          if (eq(car(fn), g_make_closure)) {
            // variable is closure, which stored in local variable
            --offset;
            continue;
          }
          pprint(var_descr, vm_get_dynamic(g_stderr_var));
          SIGNAL_INTERNAL_ERROR();
        }
        // name is match
        // increase usage count
        cons_t * usage = get_cons_for_write(cdddr(var_descr));
        usage->first = __long_to_fixnum_term(term_to_long(usage->first) + 1);
        if (tp == lambda_e || tp == macro_e) {
          // variable names literal function.
          return fn;
        }
        if (!eq(car(fn), g_make_closure)) {
          // variable is not closure
          SIGNAL_INTERNAL_ERROR();
        }
        // variable is closure
        if (get_literal_object_for_validation) {
          return cadr(fn);
        }
        if (frame == 0) {
          // shallow reference
          return LIST_2(g_local_var_ref, long_to_term(offset));
        }
        // deep reference
        if (frame > state->depth_of_deep_ops) {
          state->depth_of_deep_ops = frame;
        }
        return LIST_3(g_deep_ref, long_to_term(frame),
                      long_to_term(env->param_vars.size + offset));
      }
      pprint(var_descr, vm_get_dynamic(g_stderr_var));
      SIGNAL_INTERNAL_ERROR();
    }
    ++frame;
    env = env->next;
  }
  return sym;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term gen_symbol(cc_state_t * state, const char * prefix) {
  char buf[128];
  long len = snprintf(buf, sizeof(buf), "%s%ld", prefix, ++(state->symbol_id));
  return make_symbol_from_binary((uint8_t *)buf, len);
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term resolve_tag(cc_state_t * state, term tag, frame_t * env) {
  long frame = 0;
  int force_non_local = 0;
  while (env != NULL) {
    long i;
    for (i = env->local_vars.size - 1; i >= 0; --i) {
      term var_descr = env->local_vars.data[i];
      if (eq(var_descr, g_unbound_special_form_marker)) {
        // out of scope special form
        continue;
      }
      term var_kind = car(var_descr);
      if (eq(var_kind, g_catch) || eq(var_kind, g_handler_bind) ||
          eq(var_kind, g_unwind_protect) ||
          (eq(var_kind, g_block) && (eq(caadr(var_descr), g_non_local_tag)))) {
        // variable is marker of one of special forms, which catches non-local
        // goto.
        force_non_local = 1;
        continue;
      }
      if (!eq(var_kind, g_local_tag) && !eq(var_kind, g_non_local_tag)) {
        // it is not tag
        continue;
      }
      if (!eq(cadr(var_descr), tag)) {
        continue;
      }
      // tag is found
      if (frame == 0 && !force_non_local) {
        // local tag
        if (eq(var_kind, g_local_tag)) {
          return var_descr;
        }
        return cons(g_local_tag, cdr(var_descr));
      }
      // handle non-local tag
      get_cons_for_write(var_descr)->first = g_non_local_tag;
      if (is_null(cddr(var_descr))) {
        // generate tag label
        get_cons_for_write(get_cons_for_read(var_descr)->second)->second =
          LIST_1(gen_symbol(state, "tag"));
      }
      return var_descr;
    }
    env = env->next;
    ++frame;
  }
  lisp_signal(g_invalid_tag, tag);
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term resolve_block_tag(cc_state_t * state, term tag, frame_t * env) {
  long frame = 0;
  int force_non_local = 0;
  while (env != NULL) {
    long i;
    for (i = env->local_vars.size - 1; i >= 0; --i) {
      term var_descr = env->local_vars.data[i];
      if (eq(var_descr, g_unbound_special_form_marker)) {
        // variable is marker of one of specail forms above, which is out of
        // scope. Offset is not decremented.
        continue;
      }
      term var_kind = car(var_descr);
      if (eq(var_kind, g_catch) || eq(var_kind, g_handler_bind) ||
          eq(var_kind, g_unwind_protect)) {
        // variable is marker of one of special forms, which catches non-local
        // goto.
        force_non_local = 1;
        continue;
      }
      if (!eq(var_kind, g_block)) {
        continue;
      }
      term tag_spec = cadr(var_descr);
      if (eq(car(tag_spec), g_non_local_tag)) {
        force_non_local = 1;
      }
      if (!eq(cadr(tag_spec), tag)) {
        continue;
      }
      // tag is found
      // increase block's usage count
      cons_t * usage = get_cons_for_write(cddr(var_descr));
      usage->first = __long_to_fixnum_term(term_to_long(usage->first) + 1);
      if (frame == 0 && !force_non_local) {
        // local tag
        return tag_spec;
      }
      // handle non-local tag
      get_cons_for_write(tag_spec)->first = g_non_local_tag;
      if (is_null(cdddr(tag_spec))) {
        // generate tag label
        get_cons_for_write(get_cons_for_read(get_cons_for_read(tag_spec)->second)->second)->second =
          LIST_1(gen_symbol(state, "tag"));
      }
      return tag_spec;
    }
    env = env->next;
    ++frame;
  }
  lisp_signal(g_invalid_block, tag);
}

//------------------------------------------------------------------------------
// lambda list compilation state
//------------------------------------------------------------------------------
typedef enum ll_state_e   ll_state_e;

enum ll_state_e {
  required_e,
  optional_e,
  rest_e,
  key_e,
};

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void compile_lambda_list(cc_state_t * cc_state, function_t * lfn,
                                frame_t * frame, frame_t * env,
                                int just_validate) {
  if (is_null(lfn->origin_ll)) {
    return;
  }
  term ll = lfn->origin_ll;
  ll_state_e ll_state = required_e;
  term compiled_opt_args = nil;
  term compiled_key_args = nil;
  term * p_opt_args = &compiled_opt_args;
  term * p_key_args = &compiled_key_args;
  term arg_funs_ll = nil;
  term * p_arg_funs_ll = &arg_funs_ll;
  term * p_key_arg_names = &lfn->key_arg_names;
  while (!is_null(ll)) {
    term arg = car(ll);
    if (eq(arg, g_optional)) {
      if (ll_state >= optional_e) {
        compiler_error(cc_state, "invalid lambda list", lfn->origin_ll);
        return;
      }
      ll_state = optional_e;
    } else if (eq(arg, g_rest)) {
      if (ll_state >= rest_e) {
        compiler_error(cc_state, "invalid lambda list", lfn->origin_ll);
        return;
      }
      ll_state = rest_e;
    } else if (eq(arg, g_key)) {
      if (ll_state >= key_e) {
        compiler_error(cc_state, "invalid lambda list", lfn->origin_ll);
        return;
      }
      ll_state = key_e;
    } else {
      switch (ll_state) {
      case required_e:
        if (!just_validate) {
          if (!eq(symbol_macro(arg, g_unbound_marker), g_unbound_marker)) {
            compiler_warning(cc_state,
                             "parameter conflicts with symbol macro",
                             arg);
          }
          vector_append_term(&frame->param_vars, LIST_3(g_param_var, arg, __long_to_fixnum_term(0)));
          *p_arg_funs_ll = LIST_1(arg);
          p_arg_funs_ll = &__get_cons_for_write(*p_arg_funs_ll)->second;
        }
        lfn->nreq_args += 1;
        break;
      case optional_e:
        lfn->nopt_args += 1;
        if (!just_validate) {
          if (!is_cons(arg)) {
            if (!eq(symbol_macro(arg, g_unbound_marker), g_unbound_marker)) {
              compiler_warning(cc_state,
                             "parameter conflicts with symbol macro",
                             arg);
          }
            vector_append_term(&frame->param_vars, LIST_3(g_param_var, arg, __long_to_fixnum_term(0)));
            *p_opt_args = LIST_1(compile_lambda(cc_state, nil, LIST_1(nil), env, 1, nil, 1, 0, frame, 0));
            *p_arg_funs_ll = LIST_1(arg);
          } else {
            if (!eq(symbol_macro(car(arg), g_unbound_marker), g_unbound_marker)) {
              compiler_warning(cc_state,
                             "parameter conflicts with symbol macro",
                             arg);
          }
            vector_append_term(&frame->param_vars, LIST_3(g_param_var, car(arg), __long_to_fixnum_term(0)));
            term arg_fn = compile_lambda(cc_state, copy_list(arg_funs_ll), LIST_1(cadr(arg)), env,
                                         1, nil, 1, 0, frame, 0);
            *p_opt_args = LIST_1(arg_fn);
            term arg_name = car(arg);
            *p_arg_funs_ll = LIST_1(arg_name);
            if (is_lambda(arg_fn)) {
              term_to_lambda(arg_fn)->name = arg_name;
            } else if (is_cons(arg_fn)) {
              assert(eq(car(arg_fn), g_make_closure));
              term_to_lambda(cadr(arg_fn))->name = arg_name;
            } else {
              SIGNAL_INTERNAL_ERROR();
            }
          }
          p_arg_funs_ll = &__get_cons_for_write(*p_arg_funs_ll)->second;
          p_opt_args = &__get_cons_for_write(*p_opt_args)->second;
        }
        break;
      case rest_e:
        if (lfn->has_rest_arg) {
          compiler_error(cc_state, "invalid lambda list", lfn->origin_ll);
          return;
        }
        if (!just_validate) {
            if (!eq(symbol_macro(arg, g_unbound_marker), g_unbound_marker)) {
              compiler_warning(cc_state,
                             "parameter conflicts with symbol macro",
                             arg);
          }
          vector_append_term(&frame->param_vars, LIST_3(g_param_var, arg, __long_to_fixnum_term(0)));
          *p_arg_funs_ll = LIST_1(arg);
          p_arg_funs_ll = &__get_cons_for_write(*p_arg_funs_ll)->second;
        }
        lfn->has_rest_arg = 1;
        break;
      case key_e:
        lfn->nkey_args += 1;
        if (!just_validate) {
          if (!is_cons(arg)) {
            if (!eq(symbol_macro(arg, g_unbound_marker), g_unbound_marker)) {
              compiler_warning(cc_state,
                             "parameter conflicts with symbol macro",
                             arg);
          }
            vector_append_term(&frame->param_vars, LIST_3(g_param_var, arg, __long_to_fixnum_term(0)));
            *p_key_args = LIST_1(compile_lambda(cc_state, nil, LIST_1(nil), env, 1, nil, 1, 0, frame, 0));
            *p_key_arg_names = LIST_1(intern(term_to_symbol(arg)->name, g_kw_package, g_true));
            *p_arg_funs_ll = LIST_1(arg);
          } else {
            if (!eq(symbol_macro(car(arg), g_unbound_marker), g_unbound_marker)) {
              compiler_warning(cc_state,
                             "parameter conflicts with symbol macro",
                             arg);
          }
            vector_append_term(&frame->param_vars, LIST_3(g_param_var, car(arg), __long_to_fixnum_term(0)));
            term arg_fn = compile_lambda(cc_state, copy_list(arg_funs_ll), LIST_1(cadr(arg)), env,
                                         1, nil, 1, 0, frame, 0);
            *p_key_args = LIST_1(arg_fn);
            *p_key_arg_names = LIST_1(intern(term_to_symbol(car(arg))->name, g_kw_package, g_true));
            term arg_name = car(arg);
            *p_arg_funs_ll = LIST_1(arg_name);
            if (is_lambda(arg_fn)) {
              term_to_lambda(arg_fn)->name = arg_name;
            } else if (is_cons(arg_fn)) {
              assert(eq(car(arg_fn), g_make_closure));
              term_to_lambda(cadr(arg_fn))->name = arg_name;
            } else {
              SIGNAL_INTERNAL_ERROR();
            }
          }
          p_key_args = &__get_cons_for_write(*p_key_args)->second;
          p_key_arg_names = &__get_cons_for_write(*p_key_arg_names)->second;
          p_arg_funs_ll = &__get_cons_for_write(*p_arg_funs_ll)->second;
        }
        break;
      }
    }
    ll = cdr(ll);
  }
  switch (ll_state) {
  case required_e:
    break;
  case optional_e:
    if (lfn->nopt_args == 0) {
      compiler_error(cc_state, "invalid lambda list", lfn->origin_ll);
      return;
    }
    break;
  case rest_e:
    if (!lfn->has_rest_arg) {
      compiler_error(cc_state, "invalid lambda list", lfn->origin_ll);
      return;
    }
    break;
  case key_e:
    if (lfn->nkey_args == 0) {
      compiler_error(cc_state, "invalid lambda list", lfn->origin_ll);
      return;
    }
    break;
  }
  if (lfn->nopt_args != 0 && !just_validate) {
    long i;
    lfn->opt_arg_funs = (term *)lisp_alloc(sizeof(term) * lfn->nopt_args, NULL);
    for (i = 0; i < lfn->nopt_args; ++i) {
      lfn->opt_arg_funs[i] = car(compiled_opt_args);
      compiled_opt_args = cdr(compiled_opt_args);
    }
  }
  if (lfn->nkey_args != 0 && !just_validate) {
    long i;
    lfn->key_arg_funs = (term *)lisp_alloc(sizeof(term) * lfn->nkey_args, NULL);
    for (i = 0; i < lfn->nkey_args; ++i) {
      lfn->key_arg_funs[i] = car(compiled_key_args);
      compiled_key_args = cdr(compiled_key_args);
    }
  }
}

//------------------------------------------------------------------------------
// initializes local closures used in 'lambda' invocation, see is_literal_closure
// in compile_invocation function
//------------------------------------------------------------------------------
static term add_literal_closures_inits(term compiled_body, frame_t * env) {
  long offset = env->local_bind_size - 1;
  long i;
  for (i = env->local_vars.size - 1; i >= 0; --i) {
    term var_descr = env->local_vars.data[i];
    if (eq(var_descr, g_unbound_special_form_marker)) {
      // variable is marker of one of specail forms above, which is out of
      // scope. Offset is not decremented.
      continue;
    }
    term var_kind = car(var_descr);
    if (eq(var_kind, g_local_var)) {
      // it's regular variable
      --offset;
      continue;
    }
    if (eq(var_kind, g_symbol_macrolet)) {
      // variable is symbol-macrolet. offset is not decremented
      continue;
    }
    if (eq(var_kind, g_local_tag) || eq(var_kind, g_non_local_tag)) {
      // variable is tag. offset is not decremented
      continue;
    }
    if (eq(var_kind, g_catch) || eq(var_kind, g_handler_bind) ||
        eq(var_kind, g_unwind_protect) || eq(var_kind, g_block)) {
      // variable is marker of one of special forms, which catches non-local
      // goto. Offset is not decremented
      continue;
    }
    if (eq(var_kind, g_global_function_name)) {
      // variable names currently compiling global function, and used by
      // compiler to verify recursive calls. offset is not decremented
      continue;
    }
    if (eq(var_kind, g_local_fun)) {
      term fn = caddr(var_descr);
      term_type_e tp = get_term_type(fn);
      if (tp == lambda_e || tp == macro_e) {
        // variable names literal function. offset is not decremented
        continue;
      }
      if (eq(car(fn), g_make_closure)) {
        // variable is closure, which stored in local variable
        --offset;
        continue;
      }
      pprint(var_descr, vm_get_dynamic(g_stderr_var));
      SIGNAL_INTERNAL_ERROR();
    }
    if (eq(var_kind, g_literal_closure)) {
      term fn = caddr(var_descr);
      // it is literal closure, should be initialized
      compiled_body = cons(LIST_3(g_local_var_set, long_to_term(offset), fn), compiled_body);
      --offset;
      continue;
    }
    pprint(var_descr, vm_get_dynamic(g_stderr_var));
    SIGNAL_INTERNAL_ERROR();
  }
  return compiled_body;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int list_find(term lst, term what) {
  while (!is_null(lst)) {
    if (eq(car(lst), what)) {
      return 1;
    }
    lst = cdr(lst);
  }
  return 0;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int should_ignore_usage(term ignores, term var) {
  while (!is_null(ignores)) {
    if (list_find(car(ignores), var)) {
      return 1;
    }
    ignores = cdr(ignores);
  }
  return 0;
}

//------------------------------------------------------------------------------
// internal function
// if check_args is false, then lambda is created by compile_lambda_list
// function for optional or key argument
//------------------------------------------------------------------------------
static term compile_lambda(cc_state_t * state, term lambda_list,
                           term body, frame_t * env, int fn_is_lambda,
                           term fn, int immute_literals, int check_args,
                           frame_t * peer_frame, int is_unwind_callback) {
  if (immute_literals) {
    __deep_immune_literals(lambda_list);
    __deep_immune_literals(body);
  }
  frame_t frame_1;
  frame_t * pframe;
  long max_call_frame_size = 0;
  long call_frame_size = 0;
  if (is_unwind_callback) {
    // setup correct environment for unwind callback
    max_call_frame_size = env->max_call_frame_size;
    call_frame_size = env->call_frame_size;
    env->max_call_frame_size = 0;
    env->call_frame_size = 0;
    pframe = env;
    env = env->next;
  } else {
    frame_init(&frame_1, env, peer_frame);
    pframe = &frame_1;
  }
  // create functional object
  function_t * lfn;
  if (is_null(fn)) {
    fn = fn_is_lambda ? make_lambda() : make_macro();
    lfn = term_to_function(fn);
  } else {
    lfn = term_to_function(fn);
    function_reset(lfn);
  }
  // save depth_of_deep_ops
  long depth_of_deep_ops = state->depth_of_deep_ops;
  // save curr_lfn
  function_t * prev_lfn = state->curr_lfn;
  state->curr_lfn = lfn;
  // compile lambda list
  lfn->origin_ll = lambda_list;
  if (!is_unwind_callback) {
    // unwind callback does not need that and compilation will just destroy
    // enclosing function parameters.
    compile_lambda_list(state, lfn, pframe, env, 0);
  }
  // proceed declarations
  term ignores = nil;
  while (is_cons(car(body)) && eq(caar(body), g_declare)) {
    term decl_list = cdar(body);
    while (!is_null(decl_list)) {
      term declaration = car(decl_list);
      decl_list = cdr(decl_list);
      if (eq(car(declaration), g_global_function_name)) {
        // bind function to prevent warnings on recursive call
        lfn->name = cadr(declaration);
        symbol_t * fn_sym = term_to_symbol(lfn->name);
        if (!symbol_is_function_bound(fn_sym)) {
          fn_sym->fun = fn;
        }
        frame_add_local_binding(pframe, LIST_3(g_global_function_name, lfn->name, fn), 0);
        continue;
      }
      if (eq(car(declaration), g_ignore)) {
        ignores = cons(cdr(declaration), ignores);
        continue;
      }
      lisp_signal(g_syntax_error, car(body));
    }
    body = cdr(body);
  }
  term compiled_body = nil;
  term * p = &compiled_body;
  // compile body
  term rest;
  while (is_cons(rest = cdr(body))) {
    *p = LIST_1(compile_exp(state, car(body), pframe, 0));
    p = &__get_cons_for_write(*p)->second;
    body = rest;
  }
  *p = LIST_1(compile_exp(state, car(body), pframe, 1));
  if (!is_unwind_callback) {
    // initialize local closures used in 'lambda' invocation, see is_literal_closure
    // in compile_invocation function
    compiled_body = add_literal_closures_inits(compiled_body, pframe);
  }
  // set implicit progn
  compiled_body = cons(g_progn, compiled_body);
  lfn->compiled_body = compiled_body;
  // pprint(compiled_body, vm_get_dynamic(g_stdout_var));
  // check for closure
  if (!is_unwind_callback) {
    // unwind callback will be always called in enclosed function environment, so there
    // is no necessary to check here for closures, will be checked when enclosed
    // function will finnish compilation
    if (state->depth_of_deep_ops != 0) {
      pframe->is_closure = 1;
      long i;
      frame_t * p = env;
      long last_closure = state->depth_of_deep_ops - 1;
      for (i = 0; i < state->depth_of_deep_ops; ++i, p = p->next) {
        p->create_environment = 1;
        if (i < last_closure) {
          p->is_closure = 1;
        }
      }
    }
    if (pframe->is_closure) {
      fn = LIST_2(g_make_closure, fn);
    }
  }
  // pprint(fn, vm_get_dynamic(g_stdout_var));
  // generate machine code
  generate_function_bytecode(state, lfn, compiled_body, pframe, check_args, is_unwind_callback);
  long i;
  // warn unused parameters
  if (check_args && !is_unwind_callback) {
    for (i = 0; i < pframe->param_vars.size; ++ i) {
      term var_descr = pframe->param_vars.data[i];
      if (eq(car(var_descr), g_param_var)) {
        // check usage count
        if (term_to_long(caddr(var_descr)) == 0 &&
            !should_ignore_usage(ignores, cadr(var_descr))) {
          compiler_warning(state, "unused parameter", cadr(var_descr));
        }
        continue;
      }
      SIGNAL_INTERNAL_ERROR();
    }
  }
  // if check_args is false, then lambda is created by compile_lambda_list
  // function for optional or key argument. In that case depth_of_deep_ops
  // should not be restored.
  // Also in case of unwind_callback parameters also not necessary to restore
  // depth_of_deep_ops.
  if (!is_unwind_callback && check_args) {
    // restore depth_of_deep_ops
    state->depth_of_deep_ops = depth_of_deep_ops;
  }
  // restore max_call_frame_size
  if (is_unwind_callback) {
    pframe->max_call_frame_size = max_call_frame_size;
    pframe->call_frame_size = call_frame_size;
  }
  // restore curr_lfn
  state->curr_lfn = prev_lfn;
  return fn;
}

//------------------------------------------------------------------------------
// C function compiler - creates C and Lisp interfaces for C function
//------------------------------------------------------------------------------
void compile_c_function(function_t * lfn, term lambda_list, const void * cfn,
                        int check_args) {
  __deep_immune_literals(lambda_list);
  lfn->origin_ll = lambda_list;
  if (!check_args) {
    // just validate lambda list and set args count
    frame_t frame;
    frame_init(&frame, NULL, NULL);
    cc_state_t  state;
    cc_state_t_init(&state);
    state.curr_lfn = lfn;
    compile_lambda_list(&state, lfn, &frame, NULL, 1);
    // set function body
    lfn->bcode = (lisp_fun_t)cfn;
    return;
  }
  // generate code for parsing and validation of function arguments
  frame_t frame;
  frame_init(&frame, NULL, NULL);
  cc_state_t  state;
  cc_state_t_init(&state);
  state.curr_lfn = lfn;
  // compile lambda list
  compile_lambda_list(&state, lfn, &frame, NULL, 0);
  // generate machine code
  generate_c_function_bytecode(&state, lfn, cfn, &frame);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
static term c_function_to_term_proto(term proto, const void * cfn, int check_args,
                                     int is_macro, int global) {
  term sym = car(proto);
  term lambda_list = cdr(proto);
  function_t * lfn = (function_t *)lisp_alloc(sizeof(function_t), NULL);
  function_init(lfn, is_macro ? macro_e : lambda_e);
  lfn->name = sym;
  compile_c_function(lfn, lambda_list, cfn, check_args);
  term fn = __pointer_to_term(lfn);
  if (global) {
    return def_global_fun(sym, fn);
  }
  return fn;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
static term c_function_to_term_sz(const char * lisp_proto, const void * cfn, int check_args,
                                  int is_macro, int global) {
  return c_function_to_term_proto(lisp_read(make_binary_stream_from_sz(lisp_proto), nil),
                                  cfn, check_args, is_macro, global);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term make_c_lambda(const char * lisp_proto, const void * cfn, int check_args) {
  return c_function_to_term_sz(lisp_proto, cfn, check_args, 0, 0);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term make_c_lambda_proto(term lisp_proto, const void * cfn, int check_args) {
  return c_function_to_term_proto(lisp_proto, cfn, check_args, 0, 0);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term make_global_c_lambda(const char * lisp_proto, const void * cfn, int check_args) {
  return c_function_to_term_sz(lisp_proto, cfn, check_args, 0, 1);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term make_c_macro(const char * lisp_proto, const void * cfn, int check_args) {
  return c_function_to_term_sz(lisp_proto, cfn, check_args, 1, 0);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term make_global_c_macro(const char * lisp_proto, const void * cfn, int check_args) {
  return c_function_to_term_sz(lisp_proto, cfn, check_args, 1, 1);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_let(cc_state_t * state, term bindings, term body,
                        frame_t * env, int is_tail) {
  term vars = nil;
  term inits = nil;
  // compile bindins
  long nbindings = 0;
  while (!is_null(bindings)) {
    term b = car(bindings);
    if (is_symbol(b)) {
      vars = cons(b, vars);
      inits = cons(nil, inits);
    } else {
      vars = cons(car(b), vars);
      inits = cons(compile_exp(state, cadr(b), env, 0), inits);
    }
    ++nbindings;
    bindings = cdr(bindings);
  }
  vars = nreverse(vars);
  inits = nreverse(inits);
  // make new LET form
  term res = LIST_1(g_let);
  term * p = &__get_cons_for_write(res)->second;
  long clean_idx = env->local_vars.size;
  // offset of LET variables
  *p = LIST_1(long_to_term(env->local_bind_size));
  p = &__get_cons_for_write(*p)->second;
  // bind LET variables and generate inits
  term bres = nil;
  term *bp = &bres;
  long i;
  for (i = 0; i < nbindings; ++i) {
    if (!eq(symbol_macro(car(vars), g_unbound_marker), g_unbound_marker)) {
      compiler_warning(state,
                       "local variable conflicts with symbol macro",
                       car(vars));
    }
    frame_add_local_binding(env, LIST_3(g_local_var, car(vars),
                                        __long_to_fixnum_term(0)),
                      1);
    *bp = LIST_1(car(inits));
    bp = &__get_cons_for_write(*bp)->second;
    vars = cdr(vars);
    inits = cdr(inits);
  }
  *p = LIST_1(bres);
  p = &__get_cons_for_write(*p)->second;
  // compile body
  term rest;
  while (is_cons(rest = cdr(body))) {
    *p = LIST_1(compile_exp(state, car(body), env, 0));
    p = &__get_cons_for_write(*p)->second;
    body = rest;
  }
  *p = LIST_1(compile_exp(state, car(body), env, is_tail));
  // clean up bindings
  for (i = 0; i < nbindings; ++i) {
    term var_descr = env->local_vars.data[clean_idx + i];
    // check usage count
    if (term_to_long(caddr(var_descr)) == 0 &&
        !should_ignore_usage(env->ignores, cadr(var_descr))) {
      compiler_warning(state, "unused variable", cadr(var_descr));
    }
    get_cons_for_write(cdr(var_descr))->first = g_unbound_marker;
  }
  return res;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_let_star(cc_state_t * state, term bindings, term body,
                             frame_t * env, int is_tail) {
  term res;
  if (is_null(bindings)) {
    res = cons(g_progn, body);
  } else {
    res = LIST_3(g_let, LIST_1(car(bindings)),
                 cons(g_let_star, cons(cdr(bindings), body)));
  }
  // pprint(res, vm_get_dynamic(g_stdout_var));
  return compile_exp(state, res, env, is_tail);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_tagbody(cc_state_t * state, term body, frame_t * env) {
  long ntags = 0;
  long tags_start = env->local_vars.size;
  // append tags
  term b = body;
  while (!is_null(b)) {
    term tag = car(b);
    if (!is_cons(tag)) {
      long i;
      for (i = 0; i < ntags; ++i) {
        if (eq(cdr(env->local_vars.data[tags_start + i]), tag)) {
          lisp_signal(g_duplicate_tag, tag);
        }
      }
      ++ntags;
      frame_add_local_binding(env, LIST_2(g_local_tag, tag), 0);
    }
    b = cdr(b);
  }
  // compile body
  term res = LIST_1(g_tagbody);
  term * p = &__get_cons_for_write(res)->second;
  b = body;
  while (!is_null(b)) {
    term exp = car(b);
    if (!is_cons(exp)) {
      *p = LIST_1(exp);
    } else {
      // clean tags
      long i;
      for (i = 0; i < ntags; ++i) {
        get_cons_for_write(env->local_vars.data[tags_start + i])->first = g_local_tag;
      }
      // save enviroment for recompilation
      long env_size = env->local_vars.size;
      long env_bind_size = env->local_bind_size;
      // compile expression
      term comp_exp = compile_exp(state, exp, env, 0);
      // check if expression should be re-compiled
      int recompile = 0;
      for (i = 0; i < ntags; ++i) {
        if (eq(car(env->local_vars.data[tags_start + i]), g_non_local_tag)) {
          term tag_label = caddr(env->local_vars.data[tags_start + i]);
          term tag = cadr(env->local_vars.data[tags_start + i]);
          exp = LIST_3(g_if, LIST_3(g_eq, LIST_3(g_catch, LIST_2(g_quote, tag_label), exp),
                                    LIST_2(g_quote, tag_label)),
                       LIST_2(g_go, tag));
          recompile = 1;
        }
      }
      if (recompile) {
        env->local_vars.size = env_size;
        env->local_bind_size = env_bind_size;
        *p = LIST_1(compile_exp(state, exp, env, 0));
      } else {
        *p = LIST_1(comp_exp);
      }
    }
    p = &get_cons_for_write(*p)->second;
    b = cdr(b);
  }
  // clean tags
  long i;
  for (i = 0; i < ntags; ++i) {
    env->local_vars.data[tags_start + i] = LIST_2(g_local_tag, g_unbound_marker);
  }
  return res;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_go(cc_state_t * state, term exp, term tag, frame_t * env,
                       int is_tail) {
  term t = resolve_tag(state, tag, env);
  term kind = car(t);
  if (eq(kind, g_local_tag)) {
    return exp;
  }
  if (eq(kind, g_non_local_tag)) {
    term label = caddr(t);
    return compile_exp(state,
                       LIST_3(g_throw,
                              LIST_2(g_quote, label),
                              LIST_2(g_quote, label)),
                       env, is_tail);
  }
  SIGNAL_INTERNAL_ERROR();
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void validate_key_arguments(cc_state_t * state, term exp, term args, const function_t * lfn) {
  term arg_keys = nil;
  while (!is_null(args)) {
    term k = car(args);
    if (list_find(arg_keys, k)) {
      compiler_warning(state, "duplicate key argument", exp);
    }
    arg_keys = cons(k, arg_keys);
    if (!list_find(lfn->key_arg_names, k) && is_symbol(k)) {
      compiler_warning(state, "invalid argument's key", cons(k, exp));
    }
    if (is_null(cdr(args))) {
      compiler_warning(state, "missed last key argument", exp);
      break;
    }
    args = cddr(args);
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term map_find(term map, term key) {
  while (!is_null(map)) {
    term kv = car(map);
    if (eq(car(kv), key)) {
      return kv;
    }
    map = cdr(map);
  }
  return nil;
}

//------------------------------------------------------------------------------
// internal function
// Translates expressions with literal lambda invocations, like
//   ((lambda (x) (+ x x)) 2)
// into LET special form
//   (let ((x 2)) (+ x x))
// and then compiles them.
// In simple case, like example above, that will not improve performance, but if
// literal lambda is closure referencing environment of covering function, then
// performance improvement is significant.
//------------------------------------------------------------------------------
static term compile_literal_lambda_invocation(cc_state_t * state, term exp, term lambda,
                                              term args, frame_t * env, int is_tail) {
  term origin_args = args;
  // construct LET bindings
  term ll = cadr(lambda);
  term body = cddr(lambda);
  term bindings = nil;
  term *p = &bindings;
  ll_state_e ll_state = required_e;
  int has_rest_arg = 0;
  while (!is_null(ll)) {
    term var;
  again:
    var = car(ll);
    ll = cdr(ll);
    if (eq(var, g_optional)) {
      if (ll_state >= optional_e) {
        compiler_error(state, "invalid lambda list", lambda);
        return nil;
      }
      ll_state = optional_e;
      goto again;
    }
    if (eq(var, g_rest)) {
      if (ll_state >= rest_e) {
        compiler_error(state, "invalid lambda list", lambda);
        return nil;
      }
      ll_state = rest_e;
      goto again;
    }
    if (eq(var, g_key)) {
      if (ll_state >= key_e) {
        compiler_error(state, "invalid lambda list", lambda);
        return nil;
      }
      ll_state = key_e;
      break;
    }
    term var_init = nil;
    term var_name = nil;
    switch (ll_state) {
    case required_e:
      if (is_null(args)) {
        compiler_error(state, "too few arguments", exp);
        return nil;
      }
      var_name = var;
      var_init = car(args);
      args = cdr(args);
      break;
    case optional_e:
      if (is_null(args)) {
        if (is_cons(var)) {
          var_name = car(var);
          var_init = cadr(var);
        } else {
          var_name = var;
        }
      } else {
        var_init = car(args);
        args = cdr(args);
        if (is_cons(var)) {
          var_name = car(var);
        } else {
          var_name = var;
        }
      }
      break;
    case rest_e:
      if (has_rest_arg == 0) {
        has_rest_arg = 1;
      } else {
        compiler_error(state, "invalid lambda list", lambda);
        return nil;
      }
      var_name = var;
      if (!is_null(args)) {
        var_init = cons(g_list, args);
      }
      break;
    case key_e:
      SIGNAL_INTERNAL_ERROR();
      break;
    }
    *p = LIST_1(LIST_2(var_name, var_init));
    p = &__get_cons_for_write(*p)->second;
  }
  if (ll_state != key_e) {
    if (!is_null(args) && ll_state != rest_e) {
      compiler_error(state, "too much arguments", exp);
      return nil;
    }
  } else {
    // parse &key arguments
    // construct key parameter names list
    term keys_vars_map = nil;
    term * pkv = &keys_vars_map;
    do {
      term v = car(ll);
      ll = cdr(ll);
      term n = v;
      if (is_cons(n)) {
        n = car(n);
      }
      term k = intern(term_to_symbol(n)->name, g_kw_package, g_true);
      *pkv = LIST_1(cons(k, v));
      pkv = &__get_cons_for_write(*pkv)->second;
    } while (!is_null(ll));
    // bind supplied arguments in order
    term arg_keys = nil;
    while (!is_null(args)) {
      term k = car(args);
      if (!is_symbol(k)) {
        if (!is_cons(k)) {
          compiler_error(state, "invalid argument's key", cons(k, exp));
          return nil;
        }
        // key is evaluated, and we can't to find corresponding parameter name,
        // expanding of LAMBDA to LET is not possible, compile function and
        // invoke it in usual way
        term fn = compile_exp(state, lambda, env, 0);
        return compile_invocation(state, exp, fn, origin_args, env, is_tail);
      }
      if (list_find(arg_keys, k)) {
        compiler_error(state, "duplicate key argument", cons(k, exp));
      }
      arg_keys = cons(k, arg_keys);
      term kv = map_find(keys_vars_map, k);
      if (is_null(kv)) {
        compiler_error(state, "invalid argument's key", cons(k, exp));
        return nil;
      }
      if (is_null(cdr(args))) {
        compiler_error(state, "missed last key argument", exp);
        return nil;
      }
      // add binding
      term var_init = cadr(args);
      args = cddr(args);
      if (has_rest_arg) {
        term env_term = make_custom(g_macro_env_class_name, env);
        term expanded_var_init = macroexpand(var_init, env_term);
        if (is_cons(expanded_var_init) && !eq(g_quote, car(expanded_var_init))) {
          // variable is initialized by function call and there is &rest
          // argument, which also invokes the function. So we have two calls
          // instead of single call, and that may cause unwanted side
          // effects. Generally, there is way to correctly handle that and
          // remain within expansion into LET, but I decided to do not expand
          // and just compile lambda and invoke it in usual way.
          term fn = compile_exp(state, lambda, env, 0);
          return compile_invocation(state, exp, fn, origin_args, env, is_tail);
        }
      }
      term var_name = cdr(kv);
      if (is_cons(var_name)) {
        var_name = car(var_name);
      }
      *p = LIST_1(LIST_2(var_name, var_init));
      p = &__get_cons_for_write(*p)->second;

    }
    // add to binding missed key arguments with default initializations
    while (!is_null(keys_vars_map)) {
      term kv = car(keys_vars_map);
      keys_vars_map = cdr(keys_vars_map);
      term k = car(kv);
      if (list_find(arg_keys, k)) {
        // parameter is already bound
        continue;
      }
      term var = cdr(kv);
      term var_name = is_cons(var) ? car(var) : var;
      term var_init = is_cons(var) ? cadr(var) : nil;
      *p = LIST_1(LIST_2(var_name, var_init));
      p = &__get_cons_for_write(*p)->second;
    }
  }
  // proceed declarations
  term ignores = env->ignores;
  env->ignores = nil;
  while (is_cons(car(body)) && eq(caar(body), g_declare)) {
    term decl_list = cdar(body);
    while (!is_null(decl_list)) {
      term declaration = car(decl_list);
      decl_list = cdr(decl_list);
      if (eq(car(declaration), g_ignore)) {
        env->ignores = cons(cdr(declaration), env->ignores);
        continue;
      }
      lisp_signal(g_syntax_error, car(body));
    }
    body = cdr(body);
  }
  // construct transalation
  term res = cons(g_let_star, cons(bindings, body));
  // pprint(res, vm_get_dynamic(g_stdout_var));
  // compile result
  res = compile_exp(state, res, env, is_tail);
  env->ignores = ignores;
  return res;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_invocation(cc_state_t * state, term exp, term fname, term args,
                               frame_t * env, int is_tail) {
  if (is_cons(fname)) {
    term op = car(fname);
    if (eq(op, g_lambda)) {
      return compile_literal_lambda_invocation(state, exp, fname, args, env, is_tail);
    }
  }
  term fn = resolve_fun(state, fname, env, 0);
  if (is_macro(fn)) {
    return compile_exp(state, apply_with_macro_env(fn, args, env), env, is_tail);
  }
  int is_literal_closure = 0;
  const function_t * lfn = NULL;
  if (!is_symbol(fn)) {
    term tmp_fn = fn;
    if (is_cons(fn)) {
      term car_fn = car(fn);
      if (eq(car_fn, g_global_function_name)) {
        tmp_fn = caddr(fn);
        fn = fname;
      } else if (eq(car_fn, g_make_closure)) {
        // invocation of literal closure in expression like:
        //   (let (i) ((lambda () i)))
        // if thar happens in loop, then closure will be created on each iteration,
        // but it may be created once only and reused on each iteration.
        is_literal_closure = is_cons(fname) && eq(car(fname), g_lambda);
        tmp_fn = cadr(fn);
      } else {
        // function is flet/labels closure
        tmp_fn = resolve_fun(state, fname, env, 1);
      }
    }
    if (is_macro(tmp_fn)) {
      compiler_error(state, "invalid macro closure invocation", exp);
      return nil;
    }
    lfn = term_to_lambda(tmp_fn);
  } else {
    term macro = compiler_macro(fn, g_unbound_marker);
    if (!eq(macro, g_unbound_marker)) {
      return compile_exp(state, apply_with_macro_env(macro, args, env),
                         env, is_tail);
    }
    // continue regular compilation
    const symbol_t * s = term_to_symbol(fn);
    if (!symbol_is_function_bound(s)) {
      compiler_warning(state, "undefined function", fn);
    } else {
      term sfn = symbol_get_function(s);
      if (is_macro(sfn)) {
        return compile_exp(state, apply_with_macro_env(symbol_get_function(s), args, env),
                           env, is_tail);
      }
      lfn = term_to_lambda(sfn);
    }
  }
  // proceed literal closure
  if (is_literal_closure) {
    term f_var = gen_symbol(state, "f");
    frame_add_local_binding(env, LIST_4(g_literal_closure, f_var, fn, __long_to_fixnum_term(0)), 1);
    fn = resolve_fun(state, f_var, env, 0);
  }
  // both compile and validate function invocation
  term res;
  long call_frame_size = env->call_frame_size;
  if (!is_tail) {
    res = LIST_4(g_call, __long_to_fixnum_term(call_frame_size), nil, fn);
  } else {
    res = LIST_4(g_call, __long_to_fixnum_term(call_frame_size), g_true, fn);
  }
  term * p = &__get_cons_for_write(cdddr(res))->second;
  term lambda_list;
  ll_state_e ll_state = required_e;
  int validate;
  if (lfn == NULL) {
    validate = 0;
    lambda_list = nil;
  } else {
    validate = 1;
    lambda_list = lfn->origin_ll;
  }
  long nargs = 0;
  while (!is_null(args)) {
    if (validate) {
    again:
      if (is_null(lambda_list)) {
        if (!lfn->has_rest_arg) {
          compiler_warning(state, "too much arguments", exp);
        }
        validate = 0;
      } else {
        term ll_arg = car(lambda_list);
        lambda_list = cdr(lambda_list);
        if (eq(ll_arg, g_optional)) {
          ll_state = optional_e;
          lambda_list = cdr(lambda_list);
        } else if (eq(ll_arg, g_rest)) {
          ll_state = rest_e;
          lambda_list = cdr(lambda_list);
          goto again;
        } else if (eq(ll_arg, g_key)) {
          ll_state = key_e;
          validate_key_arguments(state, exp, args, lfn);
          validate = 0;
        }
      }
    }
    *p = LIST_1(compile_exp(state, car(args), env, 0));
    p = &__get_cons_for_write(*p)->second;
    ++nargs;
    env->call_frame_size += 1;
    args = cdr(args);
  }
  env->call_frame_size = call_frame_size;
  call_frame_size += nargs;
  if (call_frame_size > env->max_call_frame_size) {
    env->max_call_frame_size = call_frame_size;
  }
  if (validate && ll_state == required_e && !is_null(lambda_list)) {
    // not enough required arguments, previos loop is terminated because args
    // are null, but lambda list is not null
    term ll_arg = car(lambda_list);
    if (!eq(ll_arg, g_optional) && !eq(ll_arg, g_rest) && !eq(ll_arg, g_key)) {
      compiler_warning(state, "too few arguments", exp);
    }
  }
  return res;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_function(cc_state_t * state, term sym, frame_t * env, int is_tail) {
  term fn = resolve_fun(state, sym, env, 0);
  if (eq(fn, sym) || (is_cons(fn) && eq(car(fn), g_global_function_name))) {
    return compile_exp(state, LIST_2(g_symbol_function, LIST_2(g_quote, sym)), env, is_tail);
  }
  return fn;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_dynamic_let(cc_state_t * state, term bindings, term body,
                                frame_t * env, int is_tail) {
  term syms = nil;
  term vars = nil;
  term inits = nil;
  term init_bindings = nil;
  term done_bindings = nil;
  while (!is_null(bindings)) {
    term b = car(bindings);
    term s;
    if (is_symbol(b)) {
      s = b;
      inits = cons(nil, inits);
    } else {
      s = car(b);
      inits = cons(cadr(b), inits);
    }
    term v = gen_symbol(state, "save");
    syms = cons(s, syms);
    vars = cons(v, vars);
    init_bindings = cons(LIST_2(v, g_unbound_dynamic_marker), init_bindings);
    done_bindings = cons(LIST_3(g_if, LIST_2(g_null, LIST_3(g_eq, v, g_unbound_dynamic_marker)),
                                LIST_3(g_vm_set_dynamic_binding, s, v)),
                         done_bindings);
    bindings = cdr(bindings);
  }
  init_bindings = nreverse(init_bindings);
  done_bindings = nreverse(done_bindings);
  while (!is_null(inits)) {
    body = cons(LIST_3(g_setq, car(vars), LIST_3(g_vm_set_dynamic_binding, car(syms), car(inits))),
                body);
    inits = cdr(inits);
    vars = cdr(vars);
    syms = cdr(syms);
  }
  term res = LIST_3(g_let, init_bindings,
                    LIST_3(g_unwind_protect, cons(g_progn, body),
                           cons(g_progn, done_bindings)));
  //  pprint(res, vm_get_dynamic(g_stdout_var));
  return compile_exp(state, res, env, is_tail);
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_catch(cc_state_t * state, term tag, term body,
                          frame_t * env, int is_tail) {
  term res = LIST_1(g_let_not_tail);
  term * p = &__get_cons_for_write(res)->second;

  long form_clean_idx = env->local_vars.size;
  frame_add_local_binding(env, cons(g_catch, res), 0);

  // binding initialization
  // ((#:frame (alloca (sizeof catch_frame_t))))
  term frame_var = gen_symbol(state, "frame");
  *p = LIST_1(LIST_1(LIST_2(frame_var,
                            LIST_2(g_alloca, __long_to_fixnum_term(sizeof(catch_frame_t))))));
  p = &__get_cons_for_write(*p)->second;

  // install catch frame
  *p = LIST_1(LIST_3(g_vm_catch_frame_install, frame_var, tag));
  p = &__get_cons_for_write(*p)->second;

  // execute body
  // (vm-catch-frame-uninstall (if (zerop (sigsetjmp new-handler))
  //                               (progn ,@body)
  //                               (vm-get-catch-value #:frame)))
  *p = LIST_1(LIST_3(g_vm_catch_frame_uninstall, frame_var,
                     LIST_4(g_if, LIST_2(g_zerop, LIST_2(g_sigsetjmp, frame_var)),
                            is_tail ? cons(g_progn_tail, body) : cons(g_progn, body),
                            LIST_2(g_vm_get_catch_value, frame_var))));
  p = &__get_cons_for_write(*p)->second;

  //pprint(res, vm_get_dynamic(g_stdout_var));
  res = compile_exp(state, res, env, is_tail);
  // clean special form
  env->local_vars.data[form_clean_idx] = g_unbound_special_form_marker;
  return res;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_handler_bind(cc_state_t * state, term handler, term body,
                                 frame_t * env, int is_tail) {
  term res = LIST_1(g_let);
  term * p = &__get_cons_for_write(res)->second;

  long form_clean_idx = env->local_vars.size;
  frame_add_local_binding(env, cons(g_handler_bind, res), 0);

  // binding initialization
  // ((#:frame (alloca (sizeof sig_lisp_handler_frame_t))))
  term frame_var = gen_symbol(state, "frame");
  *p = LIST_1(LIST_1(LIST_2(frame_var, LIST_2(g_alloca,
                                              __long_to_fixnum_term(sizeof(sig_lisp_handler_frame_t))))));
  p = &__get_cons_for_write(*p)->second;

  // set signal handler
  // (vm-handler-frame_install frame handler)
  *p = LIST_1(LIST_3(g_vm_handler_frame_install, frame_var, handler));
  p = &__get_cons_for_write(*p)->second;

  // evaluate body and uninstall signal handler
  // (vm-handler-frame-uninstall (progn ,@body))
  *p = LIST_1(LIST_2(g_vm_handler_frame_uninstall, cons(g_progn, body)));
  p = &__get_cons_for_write(*p)->second;

  //pprint(res, vm_get_dynamic(g_stdout_var));
  res = compile_exp(state, res, env, is_tail);
  // clean special form
  env->local_vars.data[form_clean_idx] = g_unbound_special_form_marker;
  return res;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_unwind_protect(cc_state_t * state, term protected_form,
                                   term cleanup_forms, frame_t * env, int is_tail) {
  term res = LIST_1(g_let);
  term * p = &__get_cons_for_write(res)->second;

  long form_clean_idx = env->local_vars.size;
  frame_add_local_binding(env, cons(g_unwind_protect, res), 0);

  // binding initialization
  // ((#:frame (alloca (sizeof lisp_unwind_frame_t))))
  term frame_var = gen_symbol(state, "frame");
  *p = LIST_1(LIST_1(LIST_2(frame_var, LIST_2(g_alloca,
                                              __long_to_fixnum_term(sizeof(lisp_unwind_frame_t))))));
  p = &__get_cons_for_write(*p)->second;

  // (vm-unwind-frame-install frame callback)
  *p = LIST_1(LIST_3(g_vm_unwind_frame_install, frame_var, cons(g_unwind_callback, cleanup_forms)));
  p = &__get_cons_for_write(*p)->second;

  // execute protected form and unistall unwind frame
  // (vm-unwind-frame-uninstall protected_form)
  *p = LIST_1(LIST_2(g_vm_unwind_frame_uninstall, protected_form));
  p = &__get_cons_for_write(*p)->second;

  // pprint(res, vm_get_dynamic(g_stdout_var));
  res = compile_exp(state, res, env, is_tail);
  // clean special form
  env->local_vars.data[form_clean_idx] = g_unbound_special_form_marker;
  return res;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_flet(cc_state_t * state, term bindings, term body,
                         frame_t * env, int is_tail, int fn_is_lambda) {
  term res = LIST_2(g_let, __long_to_fixnum_term(env->local_bind_size));
  term * p = &__get_cons_for_write(res)->second;
  p = &__get_cons_for_write(*p)->second;
  long let_vars_start = env->local_vars.size;
  // compile bindings
  long nbindings = 0;
  term vars = nil;
  term bres = nil;
  term *bp = &bres;
  while (!is_null(bindings)) {
    ++nbindings;
    term b = car(bindings);
    term name = car(b);
    term lambda_list = cadr(b);
    term fn_body = cddr(b);
    term fn =  fn_is_lambda ? make_lambda() : make_macro();
    function_t * lfn = __term_to_function(fn);
    // set function name
    lfn->name = name;
    fn = compile_lambda(state, lambda_list, LIST_1(cons(g_block, cons(name, fn_body))),
                        env, fn_is_lambda, fn, 1, 1, NULL, 0);
    if (is_cons(fn)) {
      // closure. closure must be bound to local variables
      assert(eq(car(fn), g_make_closure));
      if (!fn_is_lambda) {
        // macro can not be closure in macrolet, because closure is created
        // during run time. But macro must be expanded during compilation time.
        lisp_signal(g_closure_in_macrolet, b);
      }
      // generate code
      *bp = LIST_1(fn);
      bp = &__get_cons_for_write(*bp)->second;
    }
    frame_add_local_binding(env, LIST_4(g_local_fun, g_unbound_marker, fn, __long_to_fixnum_term(0)),
                            is_cons(fn));
    vars = cons(name, vars);
    bindings = cdr(bindings);
  }
  vars = nreverse(vars);
  // store inits
  *p = LIST_1(bres);
  p = &__get_cons_for_write(*p)->second;
  // enable LET vars
  long i;
  for (i = 0; i < nbindings; ++i) {
    __get_cons_for_write(cdr(env->local_vars.data[let_vars_start + i]))->first = car(vars);
    vars = cdr(vars);
  }
  // compile body
  term rest;
  while (is_cons(rest = cdr(body))) {
    *p = LIST_1(compile_exp(state, car(body), env, 0));
    p = &__get_cons_for_write(*p)->second;
    body = rest;
  }
  *p = LIST_1(compile_exp(state, car(body), env, is_tail));
  // clean up bindings
  for (i = 0; i < nbindings; ++i) {
    term var_descr = env->local_vars.data[let_vars_start + i];
    // check usage count
    if (term_to_long(cadddr(var_descr)) == 0) {
      if (fn_is_lambda) {
        compiler_warning(state, "unused local function", cadr(var_descr));
      } else {
        compiler_warning(state, "unused local macro", cadr(var_descr));
      }
    }
    get_cons_for_write(cdr(var_descr))->first = g_unbound_marker;
  }
  return res;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_labels(cc_state_t * state, term bindings, term body,
                           frame_t * env, int is_tail) {
  term res = LIST_2(g_let, __long_to_fixnum_term(env->local_bind_size));
  term * p = &__get_cons_for_write(res)->second;
  p = &__get_cons_for_write(*p)->second;
  long let_vars_start = env->local_vars.size;
  // bind all functions, in order they will be correctly resolved
  long nbindings = 0;
  term save_bindings = bindings;
  while (!is_null(bindings)) {
    ++nbindings;
    term b = car(bindings);
    term name = car(b);
    term fn = make_lambda();
    function_t * lfn = __term_to_function(fn);
    // set name
    lfn->name = name;
    lfn->origin_ll = cadr(b);
    frame_add_local_binding(env, LIST_4(g_local_fun, name, fn, __long_to_fixnum_term(0)), 0);
    bindings = cdr(bindings);
  }
  // precompile all functions, in order to ensure that mutually recursive functions
  // will be correctly resolved. For example:
  //   (lables ((f1 () (f2))
  //            (f2 () 1))
  //     (f1))
  // When f1 is compiled, it will refer literal f2, whose body may be relocated,
  // when f2 itself is compiled.
  bindings = save_bindings;
  long i;
  for (i = 0; i < nbindings; ++i) {
    term b = car(bindings);
    term lambda_list = cadr(b);
    term body = cddr(b);
    term name = cadr(env->local_vars.data[let_vars_start + i]);
    term fn = caddr(env->local_vars.data[let_vars_start + i]);
    compile_lambda(state, lambda_list, LIST_1(cons(g_block, cons(name, body))),
                   env, 1, fn, 1, 1, NULL, 0);
    bindings = cdr(bindings);
  }
  // first time compilation
  int has_closures;
  long attempts = 0;
 again:
  bindings = save_bindings;
  has_closures = 0;
  for (i = 0; i < nbindings; ++i) {
    term b = car(bindings);
    term lambda_list = cadr(b);
    term body = cddr(b);
    term fn = caddr(env->local_vars.data[let_vars_start + i]);
    const function_t * lfn = __term_to_function(fn);
    lisp_fun_t bcode = lfn->bcode;
    fn = compile_lambda(state, lambda_list, LIST_1(cons(g_block, cons(lfn->name, body))),
                        env, 1, fn, 1, 1, NULL, 0);
    if (is_cons(fn)) {
      // closure. closure must be bound to local variables
      assert(eq(car(fn), g_make_closure));
      has_closures = 1;
      env->local_bind_size += 1;
      // update binding with closure
      __get_cons_for_write(cddr(env->local_vars.data[let_vars_start + i]))->first = fn;
    }
    if (!has_closures && bcode != lfn->bcode) {
      // unexpected function bytecode relocation. It will happen in mutually
      // recursive invocations:
      //   (lables ((f1 () (f2))
      //            (f2 () 1))
      //     (f1))
      // because, bcode of f2 is NULL, when precompiling f1. Thus code generator
      // decides that it is recursive call of function itself and generates
      // different machine instructions. Or some other pointers may be reallocated.
      if (attempts < nbindings * 2) {
        ++attempts;
        goto again;
      }
      SIGNAL_INTERNAL_ERROR();
    }
    bindings = cdr(bindings);
  }
  if (!has_closures) {
    *p = LIST_1(nil);
    p = &__get_cons_for_write(*p)->second;
  } else {
    // precompile functions before second time compilation, in order to
    // correctly resolve references between mutually recursive functions.
    bindings = save_bindings;
    while (!is_null(bindings)) {
      term b = car(bindings);
      term name = car(b);
      term lambda_list = cadr(b);
      term body = cddr(b);
      term fn = resolve_fun(state, name, env, 1);
      compile_lambda(state, lambda_list, LIST_1(cons(g_block, cons(name, body))),
                     env, 1, fn, 1, 1, NULL, 0);
      bindings = cdr(bindings);
    }
    // compile bindings second time because of closures
    term bres = nil;
    term *bp = &bres;
    bindings = save_bindings;
    term * env_ptr = env->local_vars.data + let_vars_start;
    while (!is_null(bindings)) {
      term b = car(bindings);
      term name = car(b);
      term lambda_list = cadr(b);
      term body = cddr(b);
      term fn = resolve_fun(state, name, env, 1);
      const function_t * lfn = __term_to_function(fn);
      lisp_fun_t bcode = lfn->bcode;
      fn = compile_lambda(state, lambda_list, LIST_1(cons(g_block, cons(name, body))),
                          env, 1, fn, 1, 1, NULL, 0);
      if (bcode != lfn->bcode) {
        // unexpected function bytecode relocation
        SIGNAL_INTERNAL_ERROR();
      }
      if (is_cons(fn)) {
        // closure. closure must be bound to local variables
        assert(eq(car(fn), g_make_closure));
        // update binding with closure
        __get_cons_for_write(cddr(*env_ptr++))->first = fn;
        // generate binding initialization
        *bp = LIST_1(fn);
        bp = &__get_cons_for_write(*bp)->second;
      }
      bindings = cdr(bindings);
    }
    *p = LIST_1(bres);
    p = &__get_cons_for_write(*p)->second;
  }
  // compile body
  term rest;
  while (is_cons(rest = cdr(body))) {
    *p = LIST_1(compile_exp(state, car(body), env, 0));
    p = &__get_cons_for_write(*p)->second;
    body = rest;
  }
  *p = LIST_1(compile_exp(state, car(body), env, is_tail));
  for (i = 0; i < nbindings; ++i) {
    term var_descr = env->local_vars.data[let_vars_start + i];
    // check usage count
    if (term_to_long(cadddr(var_descr)) == 0) {
      compiler_warning(state, "unused local function", cadr(var_descr));
    }
    get_cons_for_write(cdr(var_descr))->first = g_unbound_marker;
  }
  return res;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_block(cc_state_t * state, term name, term body,
                          frame_t * env, int is_tail) {
  long block_start = env->local_vars.size;
  term tag_spec = LIST_3(g_local_tag, name, is_tail ? g_true : nil);
  term res = LIST_3(g_block, tag_spec, __long_to_fixnum_term(0));
  term * p = &__get_cons_for_write(res)->second;
  p = &__get_cons_for_write(*p)->second;
  p = &__get_cons_for_write(*p)->second;
  frame_add_local_binding(env, res, 0);
  // compile body
  term saved_body = body;
  term rest;
  while (is_cons(rest = cdr(body))) {
    *p = LIST_1(compile_exp(state, car(body), env, 0));
    p = &__get_cons_for_write(*p)->second;
    body = rest;
  }
  *p = LIST_1(compile_exp(state, car(body), env, is_tail));
  if (eq(car(tag_spec), g_non_local_tag)) {
    // block should be recompiled with 'catch
    term label = cadddr(tag_spec);
    res = compile_exp(state, cons(g_catch, cons(LIST_2(g_quote, label), saved_body)),
                      env, is_tail);
  }
  // clean block
  env->local_vars.data[block_start] = g_unbound_special_form_marker;
  return res;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_return_from(cc_state_t * state, term tag, term value,
                                frame_t * env, int is_tail) {
  term t = resolve_block_tag(state, tag, env);
  term kind = car(t);
  if (eq(kind, g_local_tag)) {
    int is_tail_block = !is_null(caddr(t));
    term res = LIST_1(g_return_from);
    term * p = &__get_cons_for_write(res)->second;
    *p = LIST_2(tag, compile_exp(state, value, env, is_tail_block));
    return res;
  }
  if (eq(kind, g_non_local_tag)) {
    term label = cadddr(t);
    return compile_exp(state, LIST_3(g_throw, LIST_2(g_quote, label), value), env, is_tail);
  }
  SIGNAL_INTERNAL_ERROR();
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term compile_symbol_macrolet(cc_state_t * state, term bindings, term body,
                                    frame_t * env, int is_tail) {
  term res = LIST_1(g_progn);
  term * p = &__get_cons_for_write(res)->second;
  long clean_idx = env->local_vars.size;
  // append bindings
  long nbindings = 0;
  while (!is_null(bindings)) {
    ++nbindings;
    term b = car(bindings);
    term name = car(b);
    term expansion = cadr(b);
    frame_add_local_binding(env, LIST_4(g_symbol_macrolet, name,
                                        expansion, __long_to_fixnum_term(0)),
                            0);
    bindings = cdr(bindings);
  }
  // compile body
  term rest;
  while (is_cons(rest = cdr(body))) {
    *p = LIST_1(compile_exp(state, car(body), env, 0));
    p = &__get_cons_for_write(*p)->second;
    body = rest;
  }
  *p = LIST_1(compile_exp(state, car(body), env, is_tail));
  // clean up bindings
  long i;
  for (i = 0; i < nbindings; ++i) {
    term var_descr = env->local_vars.data[clean_idx + i];
    // check usage count
    if (term_to_long(cadddr(var_descr)) == 0) {
      compiler_warning(state, "unused local symbol macro", cadr(var_descr));
    }
    get_cons_for_write(cdr(var_descr))->first = g_unbound_marker;
  }
  return res;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_alloca(cc_state_t * state, term size_term, frame_t * env, int is_tail) {
  // recalculate size in bytes to size in terms
  long size = term_to_long(size_term);
  size = ALIGNUP(size, sizeof(term)) / sizeof(term);
  long env_offset = env->local_bind_size;
  long i;
  for (i = 0; i < size; ++i) {
    frame_add_local_binding(env, LIST_3(g_local_var, g_unbound_marker, __long_to_fixnum_term(0)), 1);
  }
  return LIST_3(g_alloca, __long_to_fixnum_term(size), __long_to_fixnum_term(env_offset));
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_progn(cc_state_t * state, term body, frame_t * env, int is_tail) {
  term res = LIST_1(g_progn);
  term * p = &__get_cons_for_write(res)->second;
  term rest;
  while (is_cons(rest = cdr(body))) {
    *p = LIST_1(compile_exp(state, car(body), env, 0));
    p = &__get_cons_for_write(*p)->second;
    body = rest;
  };
  *p = LIST_1(compile_exp(state, car(body), env, is_tail));
  return res;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_throw(cc_state_t * state, term tag, term value, frame_t * env) {
  // ensure that things are compiled in supplied order
  term tag_1 = compile_exp(state, tag, env, 0);
  term value_1 = compile_exp(state, value, env, 1);
  return LIST_3(g_throw, tag_1, value_1);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_multiple_value_call(cc_state_t * state, term fn, term val_forms,
                                        frame_t * env, int is_tail) {
  // expanded into following code:
  // (let* ((new-values-data (alloca (* MULTIPLE-VALUES-LIMIT sizeof(term))))
  //        (new-values (alloca sizeof(values_t)))
  //        (old-values (vm-set-new-values new-values
  //                                       new-values-data
  //                                       MULTIPLE-VALUES-LIMIT)))
  //   (unwind-protect
  //       (progn
  //         (vm-advance-values form1)
  //         (vm-advance-values form2)
  //         (vm-call-with-values fn new-values-data old-values))
  //     (vm-restore-values old-values)))

  term res = LIST_1(g_let_star);
  term * p = &__get_cons_for_write(res)->second;

  // binding initialization
  term new_values_data_var = gen_symbol(state, "new-values-data");
  term new_values_var = gen_symbol(state, "new-values");
  term old_values_var = gen_symbol(state, "old-values");
  *p = LIST_1(LIST_3(LIST_2(new_values_data_var,
                            LIST_2(g_alloca,
                                   __long_to_fixnum_term(MULTIPLE_VALUES_LIMIT * sizeof(term)))),
                     LIST_2(new_values_var,
                            LIST_2(g_alloca,
                                   __long_to_fixnum_term(sizeof(values_t)))),
                     LIST_2(old_values_var,
                            LIST_4(g_vm_set_new_values,
                                   new_values_var,
                                   new_values_data_var,
                                   __long_to_fixnum_term(MULTIPLE_VALUES_LIMIT)))));
  p = &__get_cons_for_write(*p)->second;

  // generate forms execution sequence
  term body = nil;
  term * pb = &body;
  do {
    term form = car(val_forms);
    val_forms = cdr(val_forms);
    *pb = LIST_1(LIST_2(g_vm_advance_values, form));
    pb = &__get_cons_for_write(*pb)->second;
  } while (!is_null(val_forms));
  *pb = LIST_1(LIST_4(g_vm_call_with_values, fn, new_values_data_var, old_values_var));

  // construct unwind-protect
  *p = LIST_1(LIST_3(g_unwind_protect, cons(g_progn, body),
                     LIST_2(g_vm_restore_values, old_values_var)));

  // compile translated program
  // pprint(res, vm_get_dynamic(g_stdout_var));
  return compile_exp(state, res, env, is_tail);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_multiple_value_bind(cc_state_t * state, term bindings, term val_form,
                                        term body, frame_t * env, int is_tail) {
  // expanded into following code:
  // (let (bindings)
  //   (let* ((new-values (alloca sizeof(values_t)))
  //          (old-values (vm-set-new-values-bind new-values
  //                                              bindings-start
  //                                              bindings-len)))
  //      (unwind-protect
  //          (vm-advance-values val-form)
  //        (vm-restore-values old-values))
  //      (progn ,@body)))

  long bindings_len = list_length(bindings);
  long bindings_start = env->local_bind_size;
  term new_values_var = gen_symbol(state, "new-values");
  term old_values_var = gen_symbol(state, "old-values");

  term res = LIST_3(g_let, bindings,
                    LIST_4(g_let_star,
                           LIST_2(LIST_2(new_values_var,
                                         LIST_2(g_alloca,
                                                __long_to_fixnum_term(sizeof(values_t)))),
                                  LIST_2(old_values_var,
                                         LIST_4(g_vm_set_new_values_bind,
                                                new_values_var,
                                                __long_to_fixnum_term(bindings_start),
                                                __long_to_fixnum_term(bindings_len)))),
                           LIST_3(g_unwind_protect,
                                  LIST_2(g_vm_advance_values, val_form),
                                  LIST_2(g_vm_restore_values, old_values_var)),
                           cons(g_progn, body)));

  // compile translated program
  // pprint(res, vm_get_dynamic(g_stdout_var));
  return compile_exp(state, res, env, is_tail);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term compile_exp(cc_state_t * state, term x, frame_t * env, int is_tail) {
  term_type_e tp = get_term_type(x);

  if (tp == symbol_e) {
    if (is_tail) {
      return LIST_2(g_clear_values, resolve_var_ref(state, x, env, 0));
    }
    return resolve_var_ref(state, x, env, 0);
  }

  if (tp != cons_e) {
    if (is_tail) {
      return LIST_2(g_clear_values, x);
    }
    return x;
  }

  term op = car(x);
  term args = cdr(x);

  if (eq(op, g_quote)) {        // QUOTE
    term res = car(args);
    if (is_cons(res)) {
      return x;
    }
    return res;
  }

  if (eq(op, g_progn)) {        // PROGN
    return compile_progn(state, args, env, is_tail);
  }

  if (eq(op, g_progn_tail)) {   // PROGN-TAIL
    return compile_progn(state, args, env, 1);
  }

  if (eq(op, g_if)) {           // IF
    term cond = compile_exp(state, car(args), env, 0);
    term true_clause = compile_exp(state, cadr(args), env, is_tail);
    term false_clause;
    if (!is_null(cddr(args))) {
      false_clause = compile_exp(state, caddr(args), env, is_tail);
    } else {
      false_clause = nil;
    }
    return LIST_4(g_if, cond, true_clause, false_clause);
  }

  if (eq(op, g_lambda)) {       // LAMBDA
    term lambda_list = car(args);
    term body = cdr(args);
    term fn = compile_lambda(state, lambda_list, body, env, 1, nil, 1, 1, NULL, 0);
    return fn;
  }

  if (eq(op, g_macro)) {        // MACRO
    term lambda_list = car(args);
    term body = cdr(args);
    term fn = compile_lambda(state, lambda_list, body, env, 0, nil, 1, 1, NULL, 0);
    return fn;
  }

  if (eq(op, g_unwind_callback)) { // UNWIND-CALLBACK, internal special form
    term fn = compile_lambda(state, nil, args, env, 1, nil, 1, 0, NULL, 1);
    return fn;
  }

  if (eq(op, g_let)) {          // LET
    term bindings = car(args);
    term body = cdr(args);
    return compile_let(state, bindings, body, env, is_tail);
  }

  if (eq(op, g_let_not_tail)) { // LET-NOT-TAIL
    term bindings = car(args);
    term body = cdr(args);
    return compile_let(state, bindings, body, env, 0);
  }

  if (eq(op, g_let_star)) {     // LET*
    term bindings = car(args);
    term body = cdr(args);
    return compile_let_star(state, bindings, body, env, is_tail);
  }

  if (eq(op, g_setq)) {         // SETQ
    term res = LIST_1(g_progn);
    term * p = &__get_cons_for_write(res)->second;
    do {
      term sym = car(args);
      term val = cadr(args);
      *p = LIST_1(resolve_var_set(state, sym, env, compile_exp(state, val, env, 0)));
      p = &__get_cons_for_write(*p)->second;
      args = cddr(args);
    } while (!is_null(args));
    return res;
  }

  if (eq(op, g_tagbody)) {      // TAGBODY
    return compile_tagbody(state, args, env);
  }

  if (eq(op, g_go)) {           // GO
    return compile_go(state, x, car(args), env, is_tail);
  }

  if (eq(op, g_declare)) {      // DECLARE
    lisp_signal(g_syntax_error, x);
  }

  if (eq(op, g_function)) {     // FUNCTION
    term sym = car(args);
    return compile_function(state, sym, env, is_tail);
  }

  if (eq(op, g_dynamic_let)) {  // DYNAMIC-LET
    term bindings = car(args);
    term body = cdr(args);
    return compile_dynamic_let(state, bindings, body, env, is_tail);
  }

  if (eq(op, g_catch)) {        // CATCH
    term tag = car(args);
    term body = cdr(args);
    return compile_catch(state, tag, body, env, is_tail);
  }

  if (eq(op, g_handler_bind)) { // HANDLER-BIND
    term handler = car(args);
    term body = cdr(args);
    return compile_handler_bind(state, handler, body, env, is_tail);
  }

  if (eq(op, g_flet)) {         // FLET
    term bindings = car(args);
    term body = cdr(args);
    return compile_flet(state, bindings, body, env, is_tail, 1);
  }

  if (eq(op, g_labels)) {       // LABELS
    term bindings = car(args);
    term body = cdr(args);
    return compile_labels(state, bindings, body, env, is_tail);
  }

  if (eq(op, g_macrolet)) {     // MACROLET
    term bindings = car(args);
    term body = cdr(args);
    return compile_flet(state, bindings, body, env, is_tail, 0);
  }

  if (eq(op, g_block)) {        // BLOCK
    term name = car(args);
    term body = cdr(args);
    return compile_block(state, name, body, env, is_tail);
  }

  if (eq(op, g_return_from)) {  // RETURN-FROM
    term tag = car(args);
    term value = nil;
    if (!is_null(cdr(args))) {
      value = cadr(args);
    }
    return compile_return_from(state, tag, value, env, is_tail);
  }

  if (eq(op, g_unwind_protect)) {  // UNWIND-PROTECT
    term protected_form = car(args);
    term cleanup_forms = cdr(args);
    return compile_unwind_protect(state, protected_form, cleanup_forms, env, is_tail);
  }

  if (eq(op, g_symbol_macrolet)) { // SYMBOL-MACROLET
    term bindings = car(args);
    term body = cdr(args);
    return compile_symbol_macrolet(state, bindings, body, env, is_tail);
  }

  if (eq(op, g_throw)) {        // THROW
    term tag = car(args);
    term value = is_null(cdr(args)) ? nil : cadr(args);
    return compile_throw(state, tag, value, env);
  }

  if (eq(op, g_multiple_value_call)) { // MULTIPLE-VALUE-CALL
    term fn = car(args);
    term val_forms = cdr(args);
    return compile_multiple_value_call(state, fn, val_forms, env, is_tail);
  }

  if (eq(op, g_multiple_value_bind)) { // MULTIPLE-VALUE-BIND
    term bindings = car(args);
    term val_form = cadr(args);
    term body = cddr(args);
    return compile_multiple_value_bind(state, bindings, val_form, body, env, is_tail);
  }

  // INTERNAL SPECIAL FORMS
  if (eq(op, g_alloca)) {  // ALLOCA - internal special form
    return compile_alloca(state, car(args), env, is_tail);
  }

  if (eq(op, g_vm_catch_frame_install)) {  // VM-CATCH-FRAME-INSTALL - internal special form
    term frame = compile_exp(state, car(args), env, 0);
    term tag = compile_exp(state, cadr(args), env, is_tail);
    return LIST_3(g_vm_catch_frame_install, frame, tag);
  }

  if (eq(op, g_vm_catch_frame_uninstall)) {  // VM-CATCH-FRAME-UNINSTALL - internal special form
    term frame = compile_exp(state, car(args), env, 0);
    term value = compile_exp(state, cadr(args), env, is_tail);
    return LIST_3(g_vm_catch_frame_uninstall, frame, value);
  }

  if (eq(op, g_vm_get_catch_value)) {    // VM-GET-CATCH-VALUE - internal special form
    return LIST_2(g_vm_get_catch_value, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_vm_unwind_frame_install)) {  // VM-UNWIND-FRAME-INSTALL - internal special form
    term frame = compile_exp(state, car(args), env, 0);
    term callback = compile_exp(state, cadr(args), env, 0);
    return LIST_3(g_vm_unwind_frame_install, frame, callback);
  }

  if (eq(op, g_vm_unwind_frame_uninstall)) { // VM-UNWIND-FRAME-UNINSTALL - internal special form
    return LIST_2(g_vm_unwind_frame_uninstall, compile_exp(state, car(args), env, is_tail));
  }

  if (eq(op, g_vm_handler_frame_install)) {  // VM-HANDLER-FRAME-INSTALL - internal special form
    term frame = compile_exp(state, car(args), env, 0);
    term handler = compile_exp(state, cadr(args), env, 0);
    return LIST_3(g_vm_handler_frame_install, frame, handler);
  }

  if (eq(op, g_vm_handler_frame_uninstall)) { // VM-HANDLER-FRAME-UNINSTALL - internal special form
    return LIST_2(g_vm_handler_frame_uninstall, compile_exp(state, car(args), env, is_tail));
  }

  if (eq(op, g_sigsetjmp)) {    // SIGSETJMP - internal special form
    return LIST_2(g_sigsetjmp, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_vm_set_dynamic_binding)) { // VM-SET-DYNAMIC-BINDING - internal special form
    return LIST_3(g_vm_set_dynamic_binding, car(args),
                  compile_exp(state, cadr(args), env, 0));
  }

  if (eq(op, g_clear_values)) { // CLEAR-VALUES - internal special form
    return x;
  }

  if (eq(op, g_vm_set_new_values)) { // VM-SET-NEW-VALUES - internal special form
    term new_values = compile_exp(state, car(args), env, 0);
    term data = compile_exp(state, cadr(args), env, 0);
    term capacity = caddr(args);
    return LIST_4(g_vm_set_new_values, new_values, data, capacity);
  }

  if (eq(op, g_vm_set_new_values_bind)) { // VM-SET-NEW-VALUES-BIND - internal special form
    term new_values = compile_exp(state, car(args), env, 0);
    term bindings_start = cadr(args);
    term bindings_len = caddr(args);
    return LIST_4(g_vm_set_new_values_bind, new_values, bindings_start, bindings_len);
  }

  if (eq(op, g_vm_advance_values)) { // VM-ADVANCE-VALUES - internal special form
    return LIST_2(g_vm_advance_values, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_vm_call_with_values)) { // VM-CALL-WITH-VALUES - internal special form
    term fn = compile_exp(state, car(args), env, 0);
    term new_values_data = compile_exp(state, cadr(args), env, 0);
    term old_values = compile_exp(state, caddr(args), env, 0);
    return LIST_4(g_vm_call_with_values, fn, new_values_data, old_values);
  }

  if (eq(op, g_vm_restore_values)) { // VM-RESTORE-VALUES - internal special form
    return LIST_2(g_vm_restore_values, compile_exp(state, car(args), env, 0));
  }

  // PROCEED EMBEDDED MACROS
  term is_tail_term = is_tail ? g_true : nil;
  if (eq(op, g_eq)) {           // EQ - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_eq, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_zerop)) {        // ZEROP - embedded compiler macro
    return LIST_3(g_zerop, is_tail_term, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_null)) {        // NULL - embedded compiler macro
    return LIST_3(g_null, is_tail_term, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_not)) {          // NOT - embedded compiler macro
    return LIST_3(g_not, is_tail_term, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_one_minus)) {    // 1- - embedded compiler macro
    return LIST_3(g_one_minus, is_tail_term, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_one_plus)) {    // 1+ - embedded compiler macro
    return LIST_3(g_one_plus, is_tail_term, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_num_equal)) {    // %= - embedded compiler macro
    return LIST_4(g_num_equal, is_tail_term, compile_exp(state, car(args), env, 0),
                  compile_exp(state, cadr(args), env, 0));
  }

  if (eq(op, g_num_not_equal)) { // %/= - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_num_not_equal, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_num_less)) {     // %< - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_num_less, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_num_less_equal)) { // %<= - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_num_less_equal, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_num_greater)) {  // %> - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_num_greater, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_num_greater_equal)) { // %> - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_num_greater_equal, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_plus)) {         // %+ - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_plus, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_mul)) {         // %* - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_mul, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_neg)) {        // %neg - embedded compiler macro
    return LIST_3(g_neg, is_tail_term, compile_exp(state, car(args), env, 0));
  }

  if (eq(op, g_minus)) {        // %- - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_minus, is_tail_term, arg_1, arg_2);
  }

  if (eq(op, g_div)) {          // %div - embedded compiler macro
    // ensure that things are compiled in supplied order
    term arg_1 = compile_exp(state, car(args), env, 0);
    term arg_2 = compile_exp(state, cadr(args), env, 0);
    return LIST_4(g_div, is_tail_term, arg_1, arg_2);
  }

  // NAMED FUNCTION INVOCATION
  return compile_invocation(state, x, op, args, env, is_tail);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term eval(term x) {
  // direct evaluation of simple expressions
  term_type_e tp = get_term_type(x);
  if (tp != cons_e) {
    if (tp == symbol_e) {
      if (symbol_is_keyword(__term_to_symbol(x))) {
        __vm_clear_values();
        return x;
      }
    } else {
      __vm_clear_values();
      return x;
    }
  } else if (eq(car(x), g_quote)) {        // QUOTE
    __vm_clear_values();
    return cadr(x);
  }
  // expression must be compiled before evaluation
  cc_state_t  state;
  cc_state_t_init(&state);
  uint8_t __storage_for_lfn[sizeof(function_t) + 8];
  function_t * lfn = (function_t *)ALIGNUP((uintptr_t)__storage_for_lfn, 8);
  function_init(lfn, lambda_e);
  compile_lambda(&state, nil, LIST_1(x), NULL, 1, __pointer_to_term(lfn), 0, 1, NULL, 0);
  return lfn->bcode(0, NULL);
}

//-------------------------------------------------------------------
// Function: (apply fn args)
//-------------------------------------------------------------------
term apply(term fn, term args) {
  const function_t * lfn;
  if (is_symbol(fn)) {
    lfn = term_to_function(symbol_get_function(term_to_symbol(fn)));
  } else {
    lfn = term_to_function(fn);
  }
  long nargs = list_length(args);
  term * fn_args = alloca(nargs * sizeof(term));
  long i;
  for (i = 0; i < nargs; ++i) {
    const cons_t * p = __get_cons_for_read(args);
    fn_args[i] = p->first;
    args = p->second;
  }
  __vm_clear_values();
  return lfn->bcode(nargs, fn_args);
}

//-------------------------------------------------------------------
// Function: (apply-vector fn args)
//-------------------------------------------------------------------
term apply_vector(term fn, term args) {
  const function_t * lfn;
  if (is_symbol(fn)) {
    lfn = term_to_function(symbol_get_function(term_to_symbol(fn)));
  } else {
    lfn = term_to_function(fn);
  }
  const vector_t * v = get_vector_for_read(args);
  __vm_clear_values();
  return lfn->bcode(v->size, v->data);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void restore_macro_env(void * p) {
  vm_t * vm = vm_get_current();
  vm->macro_env = (term)p;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static term apply_with_macro_env(term fn, term args, frame_t * macro_env) {
  term env = make_custom(g_macro_env_class_name, macro_env);
  vm_t * vm = vm_get_current();
  term res = nil;
  UNWIND_PROTECT_BEGIN(restore_macro_env, (void *)vm->macro_env) {
    vm->macro_env = env;
    res = apply(fn, args);
  } UNWIND_PROTECT_END;
  return res;
}

//-------------------------------------------------------------------
// Internal function.
// Expands macro once.
//-------------------------------------------------------------------
static term macroexpand_one(cc_state_t * state, term exp, frame_t * env) {
  if (is_symbol(exp)) {
    term macro = resolve_var_ref(state, exp, env, 1);
    if (!eq(macro, exp)) {
      term var_kind = car(macro);
      if (eq(var_kind, g_local_var) || eq(var_kind, g_param_var)) {
        return exp;
      }
      if (eq(var_kind, g_symbol_macrolet)) {
        return caddr(macro);
      }
      print(macro, vm_get_dynamic(g_stderr_var));
      SIGNAL_INTERNAL_ERROR();
    }
    macro = symbol_macro(exp, g_unbound_marker);
    if (eq(macro, g_unbound_marker)) {
      return exp;
    }
    return macroexpand_one(state, macro, env);
  }
  if (!is_cons(exp)) {
    return exp;
  }
  term fname = car(exp);
  if (is_special_form(fname)) {
    return exp;
  }
  term args = cdr(exp);
  term fn = resolve_fun(state, fname, env, 0);
  if (is_macro(fn)) {
    return apply_with_macro_env(fn, args, env);
  }
  if (!is_symbol(fn)) {
    return exp;
  }
  // continue regular expansion
  const symbol_t * s = term_to_symbol(fn);
  if (!symbol_is_function_bound(s)) {
    return exp;
  }
  term sfn = symbol_get_function(s);
  if (is_macro(sfn)) {
    return apply_with_macro_env(symbol_get_function(s), args, env);
  }
  return exp;
}

//-------------------------------------------------------------------
// Function: (macroexpand-1 exp &optional env) ==> object
//-------------------------------------------------------------------
term macroexpand_1(term exp, term env) {
  cc_state_t state;
  cc_state_t_init(&state);
  frame_t * env_ptr;
  if (is_null(env)) {
    env_ptr = NULL;
  } else {
    env_ptr = (frame_t *)term_custom_value(env, g_macro_env_class_name);
  }
  return macroexpand_one(&state, exp, env_ptr);
}

//-------------------------------------------------------------------
// Function: (macroexpand exp &optional env) ==> object
//-------------------------------------------------------------------
term macroexpand(term exp, term env) {
  cc_state_t state;
  cc_state_t_init(&state);
  frame_t * env_ptr;
  if (is_null(env)) {
    env_ptr = NULL;
  } else {
    env_ptr = (frame_t *)term_custom_value(env, g_macro_env_class_name);
  }
  term exp1;
  while (!eq(exp, (exp1 = macroexpand_one(&state, exp, env_ptr)))) {
    exp = exp1;
  }
  return exp1;
}

//-------------------------------------------------------------------
// Internal function.
// Expands compiler macro once.
//-------------------------------------------------------------------
static term compiler_macroexpand_one(cc_state_t * state, term exp, frame_t * env) {
  if (!is_cons(exp)) {
    return exp;
  }
  term fname = car(exp);
  if (is_special_form(fname)) {
    return exp;
  }
  term fn = car(exp);
  term macro = compiler_macro(fn, g_unbound_marker);
  if (!eq(macro, g_unbound_marker)) {
    term args = cdr(exp);
    return apply_with_macro_env(macro, args, env);
  }
  return exp;
}

//-------------------------------------------------------------------
// Function: (compiler-macroexpand-1 exp &optional env) ==> object
//-------------------------------------------------------------------
term compiler_macroexpand_1(term exp, term env) {
  cc_state_t state;
  cc_state_t_init(&state);
  frame_t * env_ptr;
  if (is_null(env)) {
    env_ptr = NULL;
  } else {
    env_ptr = (frame_t *)term_custom_value(env, g_macro_env_class_name);
  }
  return compiler_macroexpand_one(&state, exp, env_ptr);
}

//-------------------------------------------------------------------
// Function: (compiler-macroexpand exp &optional env) ==> object
//-------------------------------------------------------------------
term compiler_macroexpand(term exp, term env) {
  cc_state_t state;
  cc_state_t_init(&state);
  frame_t * env_ptr;
  if (is_null(env)) {
    env_ptr = NULL;
  } else {
    env_ptr = (frame_t *)term_custom_value(env, g_macro_env_class_name);
  }
  term exp1;
  while (!eq(exp, (exp1 = compiler_macroexpand_one(&state, exp, env_ptr)))) {
    exp = exp1;
  }
  return exp1;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
void __eval_init() {
  // special forms
  g_declare = resolve_symbol_from_sz("kl:declare");
  g_progn = resolve_symbol_from_sz("kl:progn");
  g_if = resolve_symbol_from_sz("kl:if");
  g_lambda = resolve_symbol_from_sz("kl:lambda");
  g_macro = resolve_symbol_from_sz("kl:macro");
  g_let = resolve_symbol_from_sz("kl:let");
  g_let_star = resolve_symbol_from_sz("kl:let*");
  g_setq = resolve_symbol_from_sz("kl:setq");
  g_tagbody = resolve_symbol_from_sz("kl:tagbody");
  g_go = resolve_symbol_from_sz("kl:go");
  g_catch = resolve_symbol_from_sz("kl:catch");
  g_handler_bind = resolve_symbol_from_sz("kl:handler-bind");
  g_unwind_protect = resolve_symbol_from_sz("kl:unwind-protect");
  g_block = resolve_symbol_from_sz("kl:block");
  g_return_from = resolve_symbol_from_sz("kl:return-from");
  g_symbol_macrolet = resolve_symbol_from_sz("kl:symbol-macrolet");
  g_dynamic_let = resolve_symbol_from_sz("kl:dynamic-let");
  g_flet = resolve_symbol_from_sz("kl:flet");
  g_labels = resolve_symbol_from_sz("kl:labels");
  g_macrolet = resolve_symbol_from_sz("kl:macrolet");
  g_throw = resolve_symbol_from_sz("kl:throw");
  g_multiple_value_call = resolve_symbol_from_sz("kl:multiple-value-call");
  g_multiple_value_bind = resolve_symbol_from_sz("kl:multiple-value-bind");

  // lambda list keywords
  g_optional = resolve_symbol_from_sz("kl:&optional");
  g_rest = resolve_symbol_from_sz("kl:&rest");
  g_key = resolve_symbol_from_sz("kl:&key");

  // embedded compiler macros
  g_eq = resolve_symbol_from_sz("kl:eq");
  g_zerop = resolve_symbol_from_sz("kl:zerop");
  g_null = resolve_symbol_from_sz("kl:null");
  g_not = resolve_symbol_from_sz("kl:not");
  g_one_minus = resolve_symbol_from_sz("kl:1-");
  g_one_plus = resolve_symbol_from_sz("kl:1+");
  g_num_equal = resolve_symbol_from_sz("kl::%=");
  g_num_not_equal = resolve_symbol_from_sz("kl::%/=");
  g_num_less = resolve_symbol_from_sz("kl::%<");
  g_num_less_equal = resolve_symbol_from_sz("kl::%<=");
  g_num_greater = resolve_symbol_from_sz("kl::%>");
  g_num_greater_equal = resolve_symbol_from_sz("kl::%>=");
  g_plus = resolve_symbol_from_sz("kl::%+");
  g_mul = resolve_symbol_from_sz("kl::%*");
  g_neg = resolve_symbol_from_sz("kl::neg");
  g_minus = resolve_symbol_from_sz("kl::%-");
  g_div = resolve_symbol_from_sz("kl::%div");

  // other public functions, used by compiler
  g_symbol_function = resolve_symbol_from_sz("kl:symbol-function");
  g_list = resolve_symbol_from_sz("kl:list");
  g_ignore = resolve_symbol_from_sz("kl:ignore");

  // private Lisp package symbols
  g_global_function_name = resolve_symbol_from_sz("kl::global-function-name");

  // compiler internal symbols
  g_let_not_tail = resolve_symbol_from_sz("#:let-not-tail");
  g_progn_tail = resolve_symbol_from_sz("#:progn-tail");
  g_param_var = resolve_symbol_from_sz("#:param-var");
  g_local_var = resolve_symbol_from_sz("#:local-var");
  g_local_fun = resolve_symbol_from_sz("#:local-fun");
  g_literal_closure = resolve_symbol_from_sz("#:literal-closure");
  g_local_var_ref = resolve_symbol_from_sz("#:local-var-ref");
  g_param_var_ref = resolve_symbol_from_sz("#:param-var-ref");
  g_deep_ref = resolve_symbol_from_sz("#:deep-ref");
  g_dynamic_ref = resolve_symbol_from_sz("#:dynamic-ref");
  g_local_var_set = resolve_symbol_from_sz("#:local-var-set");
  g_param_var_set = resolve_symbol_from_sz("#:param-var-set");
  g_deep_set = resolve_symbol_from_sz("#:deep-set");
  g_dynamic_set = resolve_symbol_from_sz("#:dynamic-set");
  g_local_tag = resolve_symbol_from_sz("#:local-tag");
  g_non_local_tag = resolve_symbol_from_sz("#:non-local-tag");
  g_unbound_special_form_marker = resolve_symbol_from_sz("#:unbound-special-form-marker");
  g_make_closure = resolve_symbol_from_sz("#:make-closure");
  g_call = resolve_symbol_from_sz("#:call");
  g_call_c_fun = resolve_symbol_from_sz("#:call-c-fun");
  g_alloca = resolve_symbol_from_sz("#:alloca");
  g_vm_catch_frame_install = resolve_symbol_from_sz("#:vm-catch-frame-install");
  g_vm_catch_frame_uninstall = resolve_symbol_from_sz("#:vm-catch-frame-uninstall");
  g_vm_get_catch_value = resolve_symbol_from_sz("#:vm-get-catch-value");
  g_vm_unwind_frame_install = resolve_symbol_from_sz("#:vm-unwind-frame-install");
  g_vm_unwind_frame_uninstall = resolve_symbol_from_sz("#:vm-unwind-frame-uninstall");
  g_unwind_callback = resolve_symbol_from_sz("#:unwind-callback");
  g_vm_handler_frame_install = resolve_symbol_from_sz("#:vm-handler-frame-install");
  g_vm_handler_frame_uninstall = resolve_symbol_from_sz("#:vm-handler-frame-uninstall");
  g_sigsetjmp = resolve_symbol_from_sz("#:sigsetjmp");
  g_vm_set_dynamic_binding = resolve_symbol_from_sz("#:vm-set-dynamic-binding");
  g_unbound_dynamic_marker = LIST_2(g_quote, resolve_symbol_from_sz("#:unbound-dynamic-marker"));
  g_clear_values = resolve_symbol_from_sz("#:clear-values");
  g_vm_set_new_values = resolve_symbol_from_sz("#:vm-set-new-values");
  g_vm_advance_values = resolve_symbol_from_sz("#:vm-advance-values");
  g_vm_call_with_values = resolve_symbol_from_sz("#:vm-call-with-values");
  g_vm_restore_values = resolve_symbol_from_sz("#:vm-restore-values old-values");
  g_vm_set_new_values_bind = resolve_symbol_from_sz("#:vm-set-new-values-bind");

  // environment class name
  g_macro_env_class_name = resolve_symbol_from_sz("#:macro-env");

  g_positive_fixnum_min = __long_to_bigint_term(-FIXNUM_MIN);
}
