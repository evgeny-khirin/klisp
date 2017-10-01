///----------------------------------------------------------------
/// Copyright (c) 2002-2010 by Evgeny Khirin.
///----------------------------------------------------------------
///----------------------------------------------------------------
/// File    : dlist.h
/// Author  : Evgeny Khirin <>
/// Description : Double linked list.
///----------------------------------------------------------------
#ifndef __dlist_h__
#define __dlist_h__

#include "klisp.h"

#include <stdlib.h>

//-----------------------------------------------------------------
// struct dlist_node_t
//-----------------------------------------------------------------
typedef struct dlist_node_t dlist_node_t;
struct dlist_node_t {
  dlist_node_t * next;
  dlist_node_t * prev;
};

//-----------------------------------------------------------------
// struct dlist_t
//-----------------------------------------------------------------
typedef struct dlist_t dlist_t;
struct dlist_t {
  dlist_node_t * head;
  dlist_node_t * tail;
};

//-------------------------------------------------------------------
// C++
//-------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

  //-----------------------------------------------------------------
  // dlist_init: initializes list
  //-----------------------------------------------------------------
  INLINE void dlist_init(dlist_t * list) {
    list->head = list->tail = NULL;
  }

  //-----------------------------------------------------------------
  // dlist_push_front: adds new element to begining of list
  //-----------------------------------------------------------------
  INLINE void dlist_push_front(dlist_t * l, dlist_node_t * n) {
    n->next = l->head;
    n->prev = NULL;
    if (l->head != NULL) {
      l->head->prev = n;
    } else {
      l->tail = n;
    }
    l->head = n;
  }

  //-----------------------------------------------------------------
  // dlist_push_back: adds new element to end of list
  //-----------------------------------------------------------------
  INLINE void dlist_push_back(dlist_t * l, dlist_node_t * n) {
    n->next = NULL;
    n->prev = l->tail;
    if (l->tail != NULL) {
      l->tail->next = n;
    } else {
      l->head = n;
    }
    l->tail = n;
  }

  //-----------------------------------------------------------------
  // dlist_is_empty
  //-----------------------------------------------------------------
  INLINE int dlist_is_empty(dlist_t * l) {
    return l->head == NULL;
  }

  //-----------------------------------------------------------------
  // dlist_pop_front
  //-----------------------------------------------------------------
  INLINE dlist_node_t * dlist_pop_front(dlist_t * l) {
    dlist_node_t * n = l->head;
    l->head = n->next;
    if (l->head != NULL) {
      l->head->prev = NULL;
    } else {
      l->tail = NULL;
    }
    return n;
  }

  //-----------------------------------------------------------------
  // dlist_pop_back
  //-----------------------------------------------------------------
  INLINE dlist_node_t * dlist_pop_back(dlist_t * l) {
    dlist_node_t * n = l->tail;
    l->tail = n->prev;
    if (l->tail != NULL) {
      l->tail->next = NULL;
    } else {
      l->head = NULL;
    }
    return n;
  }

  //-----------------------------------------------------------------
  // dlist_remove
  //-----------------------------------------------------------------
  INLINE void dlist_remove(dlist_t * l, dlist_node_t * n) {
    if (n == l->head) {
      dlist_pop_front(l);
      return;
    }
    if (n == l->tail) {
      dlist_pop_back(l);
      return;
    }
    n->prev->next = n->next;
    n->next->prev = n->prev;
  }

  //-------------------------------------------------------------------
  // C++
  //-------------------------------------------------------------------
#ifdef __cplusplus
}
#endif

#endif // __dlist_h__
