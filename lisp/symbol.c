///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : symbol.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp symbols.
///-------------------------------------------------------------------

#include "klisp.h"

//--------------------------------------------------------------------------
// public function
//--------------------------------------------------------------------------
void __signal_unbound_function() {
  lisp_signal(g_unbound_function, nil);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void mutex_unlock_on_unwind(void * m) {
  mutex_unlock((mutex_t *)m);
}

//--------------------------------------------------------------------------
// public function
//--------------------------------------------------------------------------
void symbol_set_prop(symbol_t * v, term key, term val) {
  mutex_lock(&g_symbols_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_symbols_mutex) {
    term plist = v->plist;
    while (!is_null(plist)) {
      const cons_t * p = get_cons_for_read(plist);
      cons_t * kv = (cons_t *)get_cons_for_write(p->first);
      if (eq(kv->first, key)) {
        kv->second = val;
        goto __unwind_protect_end__;
      }
      plist = p->second;
    }
    v->plist = cons(cons(key, val), v->plist);
  } UNWIND_PROTECT_END;
}

//--------------------------------------------------------------------------
// public function
//--------------------------------------------------------------------------
term symbol_get_prop(const symbol_t * v, term key, term dflt) {
  mutex_lock(&g_symbols_mutex);
  term res = dflt;
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_symbols_mutex) {
    term plist = v->plist;
    while (!is_null(plist)) {
      const cons_t * p = get_cons_for_read(plist);
      const cons_t * kv = get_cons_for_read(p->first);
      if (eq(kv->first, key)) {
        res = kv->second;
        break;
      }
      plist = p->second;
    }
  } UNWIND_PROTECT_END;
  return res;
}

//--------------------------------------------------------------------------
// public function
//--------------------------------------------------------------------------
term symbol_rm_prop(symbol_t * v, term key) {
  term res = nil;
  mutex_lock(&g_symbols_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_symbols_mutex) {
    term plist = v->plist;
    cons_t * prev_pair = NULL;
    while (!is_null(plist)) {
      cons_t * p = get_cons_for_write(plist);
      const cons_t * kv = get_cons_for_read(p->first);
      if (eq(kv->first, key)) {
        res = kv->second;
        if (prev_pair == NULL) {
          v->plist = p->second;
        } else {
          prev_pair->second = p->second;
        }
        break;
      }
      prev_pair = p;
      plist = p->second;
    }
  } UNWIND_PROTECT_END;
  return res;
}

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
static mutex_t        g_dyn_idx_mutex = MUTEX_INITIALIZER;
static long *         g_dyn_idx_array = NULL;
static long           g_dyn_idx_size = 0;
static long           g_dyn_idx_free_list = -1;

//------------------------------------------------------------------------------
// internal public function
//------------------------------------------------------------------------------
long __symbol_alloc_dyn_idx(symbol_t * s) {
  mutex_lock(&g_dyn_idx_mutex);
  UNWIND_PROTECT_BEGIN(mutex_unlock_on_unwind, &g_dyn_idx_mutex) {
    if (s->dyn_idx != -1) {
      goto __unwind_protect_end__;
    }
    if (g_dyn_idx_free_list == -1) {
      long new_size;
      if (g_dyn_idx_array == NULL) {
        new_size = 8;
        g_dyn_idx_array = (long *)lisp_alloc_atomic(sizeof(long) * new_size, NULL);
      } else {
        new_size = g_dyn_idx_size * 2;
        g_dyn_idx_array = (long *)lisp_realloc_atomic(g_dyn_idx_array,
                                                      sizeof(long) * g_dyn_idx_size,
                                                      sizeof(long) * new_size);
      }
      long end = new_size - 1;
      long i;
      for (i = g_dyn_idx_size; i < end;) {
        long j = i + 1;
        g_dyn_idx_array[i] = j;
        i = j;
      }
      g_dyn_idx_array[end] = -1;
      g_dyn_idx_free_list = g_dyn_idx_size;
      g_dyn_idx_size = new_size;
    }
    s->dyn_idx = g_dyn_idx_free_list;
    g_dyn_idx_free_list = g_dyn_idx_array[g_dyn_idx_free_list];
  } UNWIND_PROTECT_END;
  return s->dyn_idx;
}

//------------------------------------------------------------------------------
// internal public function
//------------------------------------------------------------------------------
void __symbol_free_dyn_idx(long dyn_idx) {
  mutex_lock(&g_dyn_idx_mutex);
  g_dyn_idx_array[dyn_idx] = g_dyn_idx_free_list;
  g_dyn_idx_free_list = dyn_idx;
  mutex_unlock(&g_dyn_idx_mutex);
}

//------------------------------------------------------------------------------
// symbol GC finalizer
//------------------------------------------------------------------------------
void symbol_finalizer(void * p) {
  symbol_t * s = (symbol_t *)p;
  if (s->dyn_idx != -1) {
    __symbol_free_dyn_idx(s->dyn_idx);
  }
  if (!is_null(s->package)) {
    __package_remove_symbol(s->package, get_binary_for_read(s->name));
  }
}
