///-------------------------------------------------------------------
/// Copyright (c) 2002-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : tqueue.c
/// Author  : Evgeny Khirin <>
/// Description : Non blocking FIFO queue. It is thread safe with single writer
/// and single reader.
///-------------------------------------------------------------------

#include "klisp.h"

#include <semaphore.h>

//-----------------------------------------------------------------
// struct tqueue_node
//-----------------------------------------------------------------
typedef struct tnode tnode;
struct tnode {
  tnode *   next;
  term      data;
};

//-----------------------------------------------------------------
// struct tqueue_t
//-----------------------------------------------------------------
typedef struct tqueue_t tqueue_t;
struct tqueue_t {
  char            res_1[DATACACHE_LINESIZE];
  tnode *         head;
  tnode *         free_tail;
  unsigned long   free_added;
  char            res_2[DATACACHE_LINESIZE - 2 * sizeof(void *) - sizeof(long)];
  tnode *         tail;
  tnode *         free_head;
  unsigned long   free_removed;
  char            res_3[DATACACHE_LINESIZE - 2 * sizeof(void *) - sizeof(long)];
  term            name;
  unsigned long   free_capacity;
  sem_t           sem;
  int             waiting;
};

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void tqueue_finalizer(void * p) {
  tqueue_t * q = (tqueue_t *)p;
  sem_destroy(&q->sem);
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void tqueue_init(tqueue_t * q, term name, unsigned long free_capacity) {
  tnode * n = (tnode *)lisp_alloc(sizeof(tnode), NULL);
  q->head = q->tail = n;
  n = (tnode *)lisp_alloc(sizeof(tnode), NULL);
  q->free_head = q->free_tail = n;
  q->name = name;
  if (sem_init(&q->sem, 0, 0) != 0) {
    lisp_signal_system_error();
  }
  q->waiting = 0;
  q->free_capacity = free_capacity;
  q->free_removed = 0;
  q->free_added = 0;
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static int tqueue_is_empty(const tqueue_t * q) {
  return (q->head == q->tail);
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static tnode * tqueue_alloc_node(tqueue_t * q) {
  if (q->free_head == q->free_tail) {
    return (tnode *)lisp_alloc(sizeof(tnode), NULL);
  }
  tnode * res = q->free_head;
  q->free_head = res->next;
  q->free_removed += 1;
  return res;
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void tqueue_free_node(tqueue_t * q, tnode * n) {
  if (q->free_added - q->free_removed < q->free_capacity) {
    q->free_added += 1;
    q->free_tail->next = n;
    q->free_tail = n;
  }
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void tqueue_push(tqueue_t * q, term data) {
  tnode * n = tqueue_alloc_node(q);
  n->data = data;
  q->tail->next = n;
  q->tail = n;
  if (q->waiting) {
    if (sem_post(&q->sem) != 0) {
      lisp_signal_system_error();
    }
  }
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static term tqueue_pop(tqueue_t * q) {
  if (tqueue_is_empty(q)) {
    int i = 0;
    q->waiting = 1;
    while (tqueue_is_empty(q) && i < 100) {
      ++i;
      pthread_yield();
    }
    if (tqueue_is_empty(q)) {
    again:
      if (sem_wait(&q->sem) != 0) {
        if (errno == EINTR) {
          goto again;
        }
        lisp_signal_system_error();
      }
      if (tqueue_is_empty(q)) {
        goto again;
      }
    }
    q->waiting = 0;
    int count;
    if (sem_getvalue(&q->sem, &count) == 0) {
      for (i = 0; i < count; ++i) {
        sem_wait(&q->sem);
      }
    }
  }
  tnode * res = q->head->next;
  tqueue_free_node(q, q->head);
  q->head = res;
  return res->data;
}

//------------------------------------------------------------------------------
// term_to_tqueue
//------------------------------------------------------------------------------
static tqueue_t * term_to_tqueue(term t) {
  return (tqueue_t *)term_custom_value(t, g_tqueue_class_name);
}

//-------------------------------------------------------------------
// Function: (tqueue-create &optional name (alloc-pool-size 256)) ==> tqueue
//-------------------------------------------------------------------
term lisp_tqueue_create(term name, term alloc_pool_size) {
  tqueue_t * q = (tqueue_t *)lisp_alloc(sizeof(tqueue_t), tqueue_finalizer);
  tqueue_init(q, name, term_to_ulong(alloc_pool_size));
  return make_custom(g_tqueue_class_name, q);
}

//-------------------------------------------------------------------
// Function: (tqueue-push tqueue object) ==> tqueue
//-------------------------------------------------------------------
term lisp_tqueue_push(term tqueue, term object) {
  tqueue_t * q = term_to_tqueue(tqueue);
  tqueue_push(q, object);
  return tqueue;
}

//-------------------------------------------------------------------
// Function: (tqueue-pop tqueue) ==> object
//-------------------------------------------------------------------
term lisp_tqueue_pop(term tqueue) {
  tqueue_t * q = term_to_tqueue(tqueue);
  return tqueue_pop(q);
}

//-------------------------------------------------------------------
// Function: (tqueue-name tqueue) ==> object
//-------------------------------------------------------------------
term lisp_tqueue_name(term tqueue) {
  return term_to_tqueue(tqueue)->name;
}

//-------------------------------------------------------------------
// Function: (tqueue-emptyp tqueue) ==> bool
//-------------------------------------------------------------------
term lisp_tqueue_emptyp(term tqueue) {
  if (tqueue_is_empty(term_to_tqueue(tqueue))) {
    return g_true;
  }
  return nil;
}
