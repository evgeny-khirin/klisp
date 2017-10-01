///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : thread.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp threads.
///-------------------------------------------------------------------

#include "klisp.h"

#include <semaphore.h>

//==============================================================================
// THREADS
//==============================================================================
//------------------------------------------------------------------------------
// thread_t
//------------------------------------------------------------------------------
typedef struct thread_t thread_t;

struct thread_t {
  term        fn;
  term        args;
  term        name;
  pthread_t   pthread;
  sem_t       ack_sem;
};

//------------------------------------------------------------------------------
// term_to_thread
//------------------------------------------------------------------------------
static thread_t * term_to_thread(term t) {
  return (thread_t *)term_custom_value(t, g_thread_class_name);
}

//------------------------------------------------------------------------------
// global variables
//------------------------------------------------------------------------------
static term g_thread_throw_tag;

//------------------------------------------------------------------------------
// thread unhandled signal handler
//------------------------------------------------------------------------------
static void thread_unhandled_signal_handler(term label, term value) {
  lisp_throw(g_thread_throw_tag, VALUES(g_thread_throw_tag, label, value));
}

//-------------------------------------------------------------------
// exec_in_lisp_context
//-------------------------------------------------------------------
term exec_in_lisp_context(term (*fn)(void *), void * p, int is_main_thread) {
  vm_t __vm;
  int reset_vm_ptr = (g_vm == NULL);
  if (reset_vm_ptr) {
    vm_init(&__vm);
    g_vm = &__vm;
  }
  if (reset_vm_ptr || is_main_thread) {
    gc_add_current_thread();
  }
  values_t * old_values = g_vm->values;
  term my_values_data[MULTIPLE_VALUES_LIMIT];
  values_t my_values;
  if (old_values == NULL) {
    values_init(&my_values, my_values_data, MULTIPLE_VALUES_LIMIT);
    g_vm->values = &my_values;
  }
  term res = nil;
  term catched_val = g_unbound_marker;
  CATCH_BEGIN(g_thread_throw_tag) {
    HANDLER_BIND_BEGIN(thread_unhandled_signal_handler) {
      res = fn(p);
    } HANDLER_BIND_END;
  } CATCH_END_EX(&catched_val);
  if (eq(catched_val, g_thread_throw_tag)) {
    lisp_fprintf_sz(vm_get_dynamic(g_stderr_var),
                    "\nexec_in_lisp_context: unhandled signal: (~s ~s)",
                    g_vm->values->data[1], g_vm->values->data[2]);
    __vm_clear_values();
  }
  if (reset_vm_ptr || is_main_thread) {
    gc_remove_current_thread();
  }
  if (reset_vm_ptr) {
    g_vm = NULL;
  } else {
    g_vm->values = old_values;
  }
  return res;
}

//-------------------------------------------------------------------
// thread function
//-------------------------------------------------------------------
static term thread_fn(void * arg) {
  thread_t * t = (thread_t *)arg;
  sem_post(&t->ack_sem);
  return apply(t->fn, t->args);
}

//-------------------------------------------------------------------
// thread start function
//-------------------------------------------------------------------
static void * thread_start_fn(void * arg) {
  return (void *) exec_in_lisp_context(thread_fn, arg, 0);
}

//-------------------------------------------------------------------
// Function: (thread-create fn args &key name stack-size (detached t)
//                          suppress-closure-warning) ==> thread
//-------------------------------------------------------------------
term lisp_thread_create(term fn, term args, term name, term stack_size,
                        term detached, term suppress_closure_warning) {
  if (is_null(suppress_closure_warning) && is_closure(term_to_function(fn))) {
    lisp_fputsz("\nWARNING: closure is passed to thread-create, make sure that you understand all risks.",
                vm_get_dynamic(g_stderr_var));
  }
  thread_t * t = (thread_t *)lisp_alloc(sizeof(thread_t), NULL);
  t->fn = fn;
  t->args = args;
  t->name = name;
  if (sem_init(&t->ack_sem, 0, 0) != 0) {
    lisp_signal_system_error();
  }
  pthread_attr_t attr;
  int rc;
  if ((rc = pthread_attr_init(&attr)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  if ((rc = pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  if (!is_null(stack_size) &&
      (rc = pthread_attr_setstacksize(&attr, term_to_long(stack_size))) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  if (!is_null(detached)) {
    if ((rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED)) != 0) {
      errno = rc;
      lisp_signal_system_error();
    }
  } else if ((rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  if ((rc = pthread_create(&t->pthread, &attr, thread_start_fn, t)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  if ((rc = pthread_attr_destroy(&attr)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
 again:
  if (sem_wait(&t->ack_sem) != 0 && errno == EINTR) {
    goto again;
  }
  sem_destroy(&t->ack_sem);
  return make_custom(g_thread_class_name, t);
}

//-------------------------------------------------------------------
// __thread_init
//-------------------------------------------------------------------
void __thread_init() {
  g_thread_throw_tag = make_symbol_from_sz("thread-throw-tag");
}

//-------------------------------------------------------------------
// Function: (thread-join thread) ==> object
//-------------------------------------------------------------------
term lisp_thread_join(term thread) {
  thread_t * t = term_to_thread(thread);
  void * ret_val;
  int rc = pthread_join(t->pthread, &ret_val);
  if (rc != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  if (ret_val == NULL) {
    return nil;
  }
  return (term)ret_val;
}

//-------------------------------------------------------------------
// Function: (thread-name mutex) ==> object
//-------------------------------------------------------------------
term lisp_thread_name(term thread) {
  return term_to_thread(thread)->name;
}

//==============================================================================
// MUTEXES
//==============================================================================
//------------------------------------------------------------------------------
// lisp_mutex_t
//------------------------------------------------------------------------------
typedef struct lisp_mutex_t lisp_mutex_t;

struct lisp_mutex_t {
  term        name;
  mutex_t     mutex;
};

//------------------------------------------------------------------------------
// term_to_mutex
//------------------------------------------------------------------------------
static lisp_mutex_t * term_to_mutex(term t) {
  return (lisp_mutex_t *)term_custom_value(t, g_mutex_class_name);
}

//-------------------------------------------------------------------
// Function: (mutex-create &optional name) ==> mutex
//-------------------------------------------------------------------
term lisp_mutex_create(term name) {
  static mutex_t template = MUTEX_INITIALIZER;
  lisp_mutex_t * m = (lisp_mutex_t *)lisp_alloc(sizeof(lisp_mutex_t), NULL);
  m->name = name;
  m->mutex = template;
  return make_custom(g_mutex_class_name, m);
}

//-------------------------------------------------------------------
// Function: (recursive-mutex-create &optional name) ==> mutex
//-------------------------------------------------------------------
term lisp_recursive_mutex_create(term name) {
  static mutex_t template = RECURSIVE_MUTEX_INITIALIZER;
  lisp_mutex_t * m = (lisp_mutex_t *)lisp_alloc(sizeof(lisp_mutex_t), NULL);
  m->name = name;
  m->mutex = template;
  return make_custom(g_mutex_class_name, m);
}

//-------------------------------------------------------------------
// Function: (mutex-destroy mutex) ==> nil
//-------------------------------------------------------------------
term lisp_mutex_destroy(term mutex) {
  lisp_mutex_t * m = term_to_mutex(mutex);
  int rc = pthread_mutex_destroy(&m->mutex);
  if (rc != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return nil;
}

//-------------------------------------------------------------------
// Function: (mutex-lock mutex) ==> mutex
//-------------------------------------------------------------------
term lisp_mutex_lock(term mutex) {
  lisp_mutex_t * m = term_to_mutex(mutex);
  mutex_lock(&m->mutex);
  return mutex;
}

//-------------------------------------------------------------------
// Function: (mutex-trylock mutex) ==> bool
//-------------------------------------------------------------------
term lisp_mutex_trylock(term mutex) {
  lisp_mutex_t * m = term_to_mutex(mutex);
  if (mutex_trylock(&m->mutex)) {
    return g_true;
  }
  return nil;
}

//-------------------------------------------------------------------
// Function: (mutex-unlock mutex) ==> mutex
//-------------------------------------------------------------------
term lisp_mutex_unlock(term mutex) {
  lisp_mutex_t * m = term_to_mutex(mutex);
  mutex_unlock(&m->mutex);
  return mutex;
}

//-------------------------------------------------------------------
// Function: (mutex-name mutex) ==> object
//-------------------------------------------------------------------
term lisp_mutex_name(term mutex) {
  return term_to_mutex(mutex)->name;
}

//==============================================================================
// CONDITIONS
//==============================================================================
#define COND_INITIALIZER             PTHREAD_COND_INITIALIZER

//------------------------------------------------------------------------------
// lisp_cond_t
//------------------------------------------------------------------------------
typedef pthread_cond_t     cond_t;
typedef struct lisp_cond_t lisp_cond_t;

struct lisp_cond_t {
  term        name;
  cond_t      cond;
};

//------------------------------------------------------------------------------
// term_to_mutex
//------------------------------------------------------------------------------
static lisp_cond_t * term_to_cond(term t) {
  return (lisp_cond_t *)term_custom_value(t, g_condition_class_name);
}

//-------------------------------------------------------------------
// Function: (condition-create &optional name) ==> condition
//-------------------------------------------------------------------
term lisp_condition_create(term name) {
  static cond_t template = COND_INITIALIZER;
  lisp_cond_t * c = (lisp_cond_t *)lisp_alloc(sizeof(lisp_cond_t), NULL);
  c->name = name;
  c->cond = template;
  return make_custom(g_condition_class_name, c);
}

//-------------------------------------------------------------------
// Function: (condition-destroy condition) ==> nil
//-------------------------------------------------------------------
term lisp_condition_destroy(term condition) {
  lisp_cond_t * c = term_to_cond(condition);
  int rc = pthread_cond_destroy(&c->cond);
  if (rc != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return nil;
}

//-------------------------------------------------------------------
// Function: (condition-signal condition) ==> condition
//-------------------------------------------------------------------
term lisp_condition_signal(term condition) {
  lisp_cond_t * c = term_to_cond(condition);
  int rc;
  if ((rc = pthread_cond_signal(&c->cond)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return condition;
}

//-------------------------------------------------------------------
// Function: (condition-wait condition mutex) ==> condition
//-------------------------------------------------------------------
term lisp_condition_wait(term condition, term mutex) {
  lisp_cond_t * c = term_to_cond(condition);
  lisp_mutex_t * m = term_to_mutex(mutex);
  int rc;
  if ((rc = pthread_cond_wait(&c->cond, &m->mutex)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return condition;
}

//-------------------------------------------------------------------
// Function: (condition-name condition) ==> object
//-------------------------------------------------------------------
term lisp_condition_name(term condition) {
  return term_to_cond(condition)->name;
}

//==============================================================================
// RWLOCKS
//==============================================================================
//------------------------------------------------------------------------------
// lisp_rwlock_t
//------------------------------------------------------------------------------
typedef pthread_rwlock_t     rwlock_t;
typedef struct lisp_rwlock_t lisp_rwlock_t;

struct lisp_rwlock_t {
  term         name;
  rwlock_t     rwlock;
};

//------------------------------------------------------------------------------
// term_to_rwlock
//------------------------------------------------------------------------------
static lisp_rwlock_t * term_to_rwlock(term t) {
  return (lisp_rwlock_t *)term_custom_value(t, g_rwlock_class_name);
}

//-------------------------------------------------------------------
// Function: (rwlock-create &optional name) ==> rwlock
//-------------------------------------------------------------------
term lisp_rwlock_create(term name) {
  lisp_rwlock_t * m = (lisp_rwlock_t *)lisp_alloc(sizeof(lisp_rwlock_t), NULL);
  m->name = name;
  int rc = pthread_rwlock_init(&m->rwlock, NULL);
  if (rc != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return make_custom(g_rwlock_class_name, m);
}

//-------------------------------------------------------------------
// Function: (rwlock-destroy rwlock) ==> nil
//-------------------------------------------------------------------
term lisp_rwlock_destroy(term rwlock) {
  lisp_rwlock_t * m = term_to_rwlock(rwlock);
  int rc = pthread_rwlock_destroy(&m->rwlock);
  if (rc != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return nil;
}

//-------------------------------------------------------------------
// Function: (rwlock-rdlock rwlock) ==> rwlock
//-------------------------------------------------------------------
term lisp_rwlock_rdlock(term rwlock) {
  lisp_rwlock_t * m = term_to_rwlock(rwlock);
  int rc = pthread_rwlock_rdlock(&m->rwlock);
  if (rc != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return rwlock;
}

//-------------------------------------------------------------------
// Function: (rwlock-wrlock rwlock) ==> rwlock
//-------------------------------------------------------------------
term lisp_rwlock_wrlock(term rwlock) {
  lisp_rwlock_t * m = term_to_rwlock(rwlock);
  int rc = pthread_rwlock_wrlock(&m->rwlock);
  if (rc != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return rwlock;
}

//-------------------------------------------------------------------
// Function: (rwlock-try-rdlock rwlock) ==> bool
//-------------------------------------------------------------------
term lisp_rwlock_try_rdlock(term rwlock) {
  lisp_rwlock_t * m = term_to_rwlock(rwlock);
  int rc = pthread_rwlock_tryrdlock(&m->rwlock);
  if (rc == 0) {
    return g_true;
  }
  if (rc == EBUSY) {
    return nil;
  }
  errno = rc;
  lisp_signal_system_error();
}

//-------------------------------------------------------------------
// Function: (rwlock-try-wrlock rwlock) ==> bool
//-------------------------------------------------------------------
term lisp_rwlock_try_wrlock(term rwlock) {
  lisp_rwlock_t * m = term_to_rwlock(rwlock);
  int rc = pthread_rwlock_trywrlock(&m->rwlock);
  if (rc == 0) {
    return g_true;
  }
  if (rc == EBUSY) {
    return nil;
  }
  errno = rc;
  lisp_signal_system_error();
}

//-------------------------------------------------------------------
// Function: (rwlock-unlock rwlock) ==> rwlock
//-------------------------------------------------------------------
term lisp_rwlock_unlock(term rwlock) {
  lisp_rwlock_t * m = term_to_rwlock(rwlock);
  int rc;
  if ((rc = pthread_rwlock_unlock(&m->rwlock)) != 0) {
    errno = rc;
    lisp_signal_system_error();
  }
  return rwlock;
}

//-------------------------------------------------------------------
// Function: (rwlock-name rwlock) ==> object
//-------------------------------------------------------------------
term lisp_rwlock_name(term rwlock) {
  return term_to_rwlock(rwlock)->name;
}

//==============================================================================
// SEMAPHORES
//==============================================================================
//------------------------------------------------------------------------------
// lisp_sem_t
//------------------------------------------------------------------------------
typedef struct lisp_sem_t lisp_sem_t;

struct lisp_sem_t {
  term        name;
  sem_t       sem;
};

//------------------------------------------------------------------------------
// term_to_mutex
//------------------------------------------------------------------------------
static lisp_sem_t * term_to_sem(term t) {
  return (lisp_sem_t *)term_custom_value(t, g_semaphore_class_name);
}

//-------------------------------------------------------------------
// Function: (semaphore-create &optional name value) ==> semaphore
//-------------------------------------------------------------------
term lisp_semaphore_create(term name, term value) {
  lisp_sem_t * c = (lisp_sem_t *)lisp_alloc(sizeof(lisp_sem_t), NULL);
  c->name = name;
  if (sem_init(&c->sem, 0, is_null(value) ? 0 : term_to_uint(value)) != 0) {
    lisp_signal_system_error();
  }
  return make_custom(g_semaphore_class_name, c);
}

//-------------------------------------------------------------------
// Function: (semaphore-destroy semaphore) ==> nil
//-------------------------------------------------------------------
term lisp_semaphore_destroy(term semaphore) {
  lisp_sem_t * c = term_to_sem(semaphore);
  if (sem_destroy(&c->sem) != 0) {
    lisp_signal_system_error();
  }
  return nil;
}

//-------------------------------------------------------------------
// Function: (semaphore-post semaphore) ==> semaphore
//-------------------------------------------------------------------
term lisp_semaphore_post(term semaphore) {
  lisp_sem_t * c = term_to_sem(semaphore);
  if (sem_post(&c->sem) != 0) {
    lisp_signal_system_error();
  }
  return semaphore;
}

//-------------------------------------------------------------------
// Function: (semaphore-wait semaphore) ==> semaphore
//-------------------------------------------------------------------
term lisp_semaphore_wait(term semaphore) {
  lisp_sem_t * c = term_to_sem(semaphore);
 again:
  if (sem_wait(&c->sem) != 0) {
    if (errno == EINTR) {
      goto again;
    }
    lisp_signal_system_error();
  }
  return semaphore;
}

//-------------------------------------------------------------------
// Function: (semaphore-name semaphore) ==> object
//-------------------------------------------------------------------
term lisp_semaphore_name(term semaphore) {
  return term_to_sem(semaphore)->name;
}

//-------------------------------------------------------------------
// Function: (semaphore-value semaphore) ==> integer
//-------------------------------------------------------------------
term lisp_semaphore_value(term semaphore) {
  lisp_sem_t * c = term_to_sem(semaphore);
  int value;
  if (sem_getvalue(&c->sem, &value) != 0) {
    lisp_signal_system_error();
  }
  return long_to_term(value);
}

