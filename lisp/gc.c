///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2011 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : gc.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp garbage collector.
///-----------------------------------------------------------------------------

#include "klisp.h"

#include <sys/mman.h>
#include <unistd.h>
#include <semaphore.h>
#include <signal.h>

//------------------------------------------------------------------------------
// GC signals
//------------------------------------------------------------------------------
#define GC_SIG_SUSPEND    SIGUSR1
#define GC_SIG_RESUME     SIGUSR2

//-------------------------------------------------------------------
// struct free_block_t
//-------------------------------------------------------------------
typedef struct free_block_t free_block_t;

struct free_block_t {
  long            size;
  free_block_t *  next;
};

//------------------------------------------------------------------------------
// struct heap_descr_t is allocated from C heap in order to prevent false
// pointers keeping.
//------------------------------------------------------------------------------
typedef struct heap_descr_t heap_descr_t;

struct heap_descr_t {
  uint8_t *          heap_start;
  uint8_t *          heap_end;
  free_block_t *     free_blocks;
};

//------------------------------------------------------------------------------
// Initial GC ptrs capacity
//------------------------------------------------------------------------------
#define GC_INITIAL_PTRS_CAPACITY    (128 * 1024)

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
static heap_descr_t *     g_heap_descr = NULL;
static gc_descr_t *       g_ptrs = NULL;
static long               g_nptrs = 0;
static long               g_next_gc_ptrs_count = GC_INITIAL_PTRS_CAPACITY / 2;
static long               g_ptrs_capacity = GC_INITIAL_PTRS_CAPACITY;
static int                g_curr_color = 0;
static mutex_t            g_heap_lock = MUTEX_INITIALIZER;
static int                g_min_alloc = -1;
static dlist_t            g_live_threads;
static long               g_nthreads = 0;
static sem_t              g_ack_sem;
static sigset_t           g_suspend_handler_mask;
static term               g_gc_throw_tag;
static double             g_gc_time = 0;
static long               g_heap_size = 0;

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
static void timer_value(struct timespec * ts) {
  clock_gettime(CLOCK_MONOTONIC, ts);
}

//--------------------------------------------------------------------------
// Internal function
//--------------------------------------------------------------------------
static double timer_diff(struct timespec * start_ts, struct timespec * end_ts) {
  uint64_t start = (uint64_t)start_ts->tv_sec * 1000 * 1000 * 1000 + start_ts->tv_nsec;
  uint64_t end = (uint64_t)end_ts->tv_sec * 1000 * 1000 * 1000 + end_ts->tv_nsec;
  return ((double)end - start) / (1000 * 1000 * 1000);
}

//------------------------------------------------------------------------------
// GC unhandled signal handler
//------------------------------------------------------------------------------
static void gc_unhandled_signal_handler(term label, term value) {
  lisp_throw(g_gc_throw_tag, VALUES(g_gc_throw_tag, label, value));
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void invoke_finalizer(finalizer_t finalizer, void * ptr) {
  vm_t * vm = vm_get_current();
  values_t * old_values = vm->values;
  term my_values_data[MULTIPLE_VALUES_LIMIT];
  values_t my_values;
  values_init(&my_values, my_values_data, MULTIPLE_VALUES_LIMIT);
  vm->values = &my_values;
  term catched_val = g_unbound_marker;
  CATCH_BEGIN(g_gc_throw_tag) {
    HANDLER_BIND_BEGIN(gc_unhandled_signal_handler) {
      finalizer(ptr);
    } HANDLER_BIND_END;
  } CATCH_END_EX(&catched_val);
  if (eq(catched_val, g_gc_throw_tag)) {
    fputs("\nUnhandled signal in GC finalizer\n", stderr);
    // lisp_fprintf_sz(vm_get_dynamic(g_stderr_var),
    //                 "\nGC_finalizer: unhandled signal: (~s ~s)",
    //                 vm->values->data[1], vm->values->data[2]);
  }
  vm->values = old_values;
}

//------------------------------------------------------------------------------
// GC suspend signal handler
//------------------------------------------------------------------------------
static void suspend_handler(int signum, siginfo_t * info, void * context) {
  vm_t * vm = vm_get_current();
  vm->context = (ucontext_t *)context;
  vm->stack_top = &vm;
  // wait for resume signal
  sem_post(&g_ack_sem);
  sigsuspend(&g_suspend_handler_mask);
  vm->context = NULL;
  vm->stack_top = NULL;
  sem_post(&g_ack_sem);
}

//------------------------------------------------------------------------------
// GC resume signal handler
//------------------------------------------------------------------------------
static void resume_handler(int signum, siginfo_t * info, void * context) {
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
void gc_init(long heap_size) {
  g_heap_descr = (heap_descr_t *)malloc(sizeof(heap_descr_t));
  if (g_heap_descr == NULL) {
    fprintf(stderr, "Failed to allocate heap descriptor\n");
    abort();
  }
  long page_size = getpagesize();
  if (!IS_POW2(page_size)) {
    fprintf(stderr, "Unexpected page size %ld\n", page_size);
    abort();
  }
  g_heap_size = ALIGNUP(heap_size, page_size);
  g_ptrs = (gc_descr_t *)malloc(g_ptrs_capacity * sizeof(gc_descr_t));
  if (g_ptrs == NULL) {
    fprintf(stderr, "Failed to allocate initial pointers array\n");
    abort();
  }
  g_heap_descr->heap_start = mmap(NULL, g_heap_size, PROT_EXEC | PROT_READ | PROT_WRITE,
                                  MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (g_heap_descr->heap_start == MAP_FAILED) {
    fprintf(stderr, "Failed to allocate heap, size %ld\n", g_heap_size);
    abort();
  }
  g_heap_descr->heap_end = g_heap_descr->heap_start + g_heap_size;
  g_heap_descr->free_blocks = (free_block_t *)g_heap_descr->heap_start;
  g_heap_descr->free_blocks->size = g_heap_size;
  g_heap_descr->free_blocks->next = NULL;
  dlist_init(&g_live_threads);
  g_min_alloc = ALIGNUP(sizeof(free_block_t), 8);
  if (sem_init(&g_ack_sem, 0, 0) != 0) {
    perror("Failed to init GC ack semaphore");
    abort();
  }
  // init suspend handler mask
  sigfillset(&g_suspend_handler_mask);
  sigdelset(&g_suspend_handler_mask, GC_SIG_RESUME);
  // set suspend signal handler
  struct sigaction act;
  memset(&act, 0, sizeof(act));
  act.sa_sigaction = suspend_handler;
  sigfillset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  if (sigaction(GC_SIG_SUSPEND, &act, NULL) != 0) {
    perror("Failed to install GC suspend signal handler");
    abort();
  }
  // set resume signal handler
  act.sa_sigaction = resume_handler;
  if (sigaction(GC_SIG_RESUME, &act, NULL) != 0) {
    perror("Failed to install GC resume signal handler");
    abort();
  }
  // GC throw tag
  g_gc_throw_tag = make_symbol_from_sz("gc-throw-tag");
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
void gc_lock() {
  mutex_lock(&g_heap_lock);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
void gc_unlock() {
  mutex_unlock(&g_heap_lock);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int expand_pdescr_array() {
  long new_capacity = g_ptrs_capacity * 2;
  gc_descr_t * new_ptrs = (gc_descr_t *)realloc(g_ptrs, new_capacity * sizeof(gc_descr_t));
  if (new_ptrs == NULL) {
    return 0;
  } else {
    g_ptrs = new_ptrs;
    g_ptrs_capacity = new_capacity;
  }
  return 1;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
void gc_add_current_thread() {
  vm_t * vm = vm_get_current();
  gc_lock();
  g_nthreads += 1;
  dlist_push_back(&g_live_threads, &vm->root_node);
  if (g_ptrs_capacity / 4 <= 2 * GC_NDESCR_IN_VM * g_nthreads) {
    expand_pdescr_array();
  }
  gc_unlock();
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
void gc_remove_current_thread() {
  vm_t * vm = vm_get_current();
  gc_lock();
  g_nthreads -= 1;
  dlist_remove(&g_live_threads, &vm->root_node);
  if (g_ptrs_capacity - g_nptrs < vm->nptrs && !expand_pdescr_array()) {
    fprintf(stderr, "Failed to copy pointers from dead thread\n");
    abort();
  }
  int i;
  int nptrs = vm->nptrs;
  for (i = 0; i < nptrs; ++i) {
    gc_descr_t * pdescr = g_ptrs + g_nptrs;
    ++g_nptrs;
    *pdescr = vm->ptrs[i];
    pdescr->color = g_curr_color;
  }
  vm->nptrs = 0;
  vm->avail_mem = NULL;
  vm->avail_size = 0;
  gc_unlock();
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static vm_t * dlist_node_to_vm(dlist_node_t * n) {
  if (n == NULL) {
    return NULL;
  }
  return (vm_t *)((uint8_t *)n - offsetof(vm_t, root_node));
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void wait_ack() {
  long nthreads = g_nthreads - 1;
  long acked = 0;
  while (acked != nthreads) {
    int sval = 0;
    sem_getvalue(&g_ack_sem, &sval);
    acked += sval;
    int i;
    for (i = 0; i < sval; ++i) {
      sem_wait(&g_ack_sem);
    }
    if (acked != nthreads) {
      pthread_yield();
    }
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void suspend_all_threads() {
  if (g_nthreads == 1) {
    return;
  }
  vm_t * current = vm_get_current();
  vm_t * vm;
  vm = dlist_node_to_vm(g_live_threads.head);
  while (vm != NULL) {
    if (vm != current) {
      pthread_spin_lock(&vm->alloc_spin);
      if (pthread_kill(vm->thread, GC_SIG_SUSPEND) != 0) {
        perror("\nFailed to send suspend signal");
        abort();
      }
    }
    vm = dlist_node_to_vm(vm->root_node.next);
  }
  wait_ack();
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void resume_all_threads() {
  if (g_nthreads == 1) {
    return;
  }
  vm_t * current = vm_get_current();
  vm_t * vm;
  vm = dlist_node_to_vm(g_live_threads.head);
  while (vm != NULL) {
    if (vm != current) {
      pthread_spin_unlock(&vm->alloc_spin);
      if (pthread_kill(vm->thread, GC_SIG_RESUME) != 0) {
        perror("\nFailed to send resume signal");
        abort();
      }
    }
    vm = dlist_node_to_vm(vm->root_node.next);
  }
  wait_ack();
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int pdescr_bsearch_compare(const void * p, const void * elem) {
  const gc_descr_t * pdescr = (gc_descr_t *)elem;
  if (p < (void *)pdescr->start) {
    return -1;
  }
  if (p >= (void *)pdescr->end) {
    return 1;
  }
  return 0;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static gc_descr_t * ptr_to_descr(const void * p) {
  if (p < (void *)g_heap_descr->heap_start || p >= (void *)g_heap_descr->heap_end) {
    return NULL;
  }
  return (gc_descr_t *)bsearch(p, g_ptrs, g_nptrs, sizeof(gc_descr_t),
                               pdescr_bsearch_compare);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static gc_descr_t * mark_and_link(const void * start, const void * end,
                                  gc_descr_t * to_scan_list) {
  while (start < end) {
    gc_descr_t * pdescr = ptr_to_descr(*(const void * *)start);
    if (pdescr != NULL) {
      if (pdescr->color != g_curr_color) {
        pdescr->color = g_curr_color;
        pdescr->next = to_scan_list;
        to_scan_list = pdescr;
      }
    }
    start += sizeof(void *);
  }
  return to_scan_list;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void mark_root(const void * start, const void * end) {
  gc_descr_t * to_scan_list = mark_and_link(start, end, NULL);
  while (to_scan_list != NULL) {
    gc_descr_t * pdescr = to_scan_list;
    to_scan_list = to_scan_list->next;
    if (!pdescr->atomic) {
      to_scan_list = mark_and_link(pdescr->start, pdescr->end, to_scan_list);
    }
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int pdescr_sort_compare(const void * left, const void * right) {
  const gc_descr_t * l = (gc_descr_t *)left;
  const gc_descr_t * r = (gc_descr_t *)right;
  assert(l->start != r->start);
  if (l->start < r->start) {
    return -1;
  }
  return 1;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void mark_stacks() {
  vm_t * vm = dlist_node_to_vm(g_live_threads.head);
  while (vm != NULL) {
    // mark stack
    mark_root(vm->stack_top, vm + 1);
    // mark CPU context
    if (vm->context != NULL) {
      mark_root(&vm->context->uc_mcontext, &vm->context->uc_mcontext + 1);
    }
    vm = dlist_node_to_vm(vm->root_node.next);
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int copy_all_vm_ptrs() {
  vm_t * vm = dlist_node_to_vm(g_live_threads.head);
  while (vm != NULL) {
    if (g_ptrs_capacity - g_nptrs < vm->nptrs && !expand_pdescr_array()) {
      return 0;
    }
    int i;
    int nptrs = vm->nptrs;
    for (i = 0; i < nptrs; ++i) {
      gc_descr_t * pdescr = g_ptrs + g_nptrs;
      ++g_nptrs;
      *pdescr = vm->ptrs[i];
      pdescr->color = g_curr_color;
    }
    memset(vm->ptrs, 0, sizeof(gc_descr_t) * nptrs);
    vm->nptrs = 0;
    vm->avail_mem = NULL;
    vm->avail_size = 0;
    vm = dlist_node_to_vm(vm->root_node.next);
  }
  return 1;
}

//------------------------------------------------------------------------------
// The addresses of these symbols indicate the end of various program segments:
//   __data_start This is first address of data segment, GCC specific.
//   _etext       This is the first address past the end of the text segment
//                (the program code).
//   _edata       This is the first address past the end of the initialized
//                data segment.
//   _end         This is the first address past the end of the uninitialized
//                data segment (also known as the BSS segment).
//------------------------------------------------------------------------------
extern char __data_start, _end;

//------------------------------------------------------------------------------
// internal function
// all local variables, that may contain heap pointers are declared static in
// order to avoid occasional pointers remaining.
//------------------------------------------------------------------------------
static void gc_collect_internal() {
  struct timespec start, end;
  timer_value(&start);
  // set stack top of current thread
  sigjmp_buf jbuf;
  sigsetjmp(jbuf, 0);
  g_vm->stack_top = jbuf->__jmpbuf;
  suspend_all_threads();
  if (!copy_all_vm_ptrs()) {
      g_vm->stack_top = NULL;
      resume_all_threads();
      timer_value(&end);
      g_gc_time += timer_diff(&start, &end);
      return;
  }
  g_curr_color = (g_curr_color + 1) & 1; // update color
  qsort(g_ptrs, g_nptrs, sizeof(gc_descr_t), pdescr_sort_compare);
  mark_root(&__data_start, &_end); // mark global variables
  mark_stacks();                   // mark all threads stacks
  g_vm->stack_top = NULL;
  // invoke finalizers
  long i;
  for (i = 0; i < g_nptrs; ++i) {
    gc_descr_t * pdescr = g_ptrs + i;
    if (pdescr->color != g_curr_color && pdescr->finalizer != NULL) {
      invoke_finalizer(pdescr->finalizer, pdescr->start);
    }
  }
  // build new list of free blocks
  const uint8_t *    free_block_start;
  const uint8_t *    free_block_end;
  g_heap_descr->free_blocks = NULL;
  free_block_start = free_block_end = g_heap_descr->heap_start;
  long nptrs = 0;
  free_block_t * * ptail = &g_heap_descr->free_blocks;
  for (i = 0; i < g_nptrs; ++i) {
    gc_descr_t * pdescr = g_ptrs + i;
    if (pdescr->color != g_curr_color) {
      free_block_end = pdescr->end;
      continue;
    }
    if (i != nptrs) {
      g_ptrs[nptrs] = *pdescr;
    }
    ++nptrs;
    long free_block_size = free_block_end - free_block_start;
    if (free_block_size >= g_min_alloc) {
      free_block_t *     free_block;
      free_block = (free_block_t *)free_block_start;
      free_block->size = free_block_size;
      free_block->next = NULL;
      *ptail = free_block;
      ptail = &free_block->next;
    }
    free_block_start = free_block_end = pdescr->end;
  }
  g_nptrs = nptrs;
  g_next_gc_ptrs_count = (g_nptrs + 2 * GC_NDESCR_IN_VM * g_nthreads) * 2;
  long free_block_size = g_heap_descr->heap_end - free_block_start;
  if (free_block_size >= g_min_alloc) {
    free_block_t *     free_block;
    free_block = (free_block_t *)free_block_start;
    free_block->size = free_block_size;
    free_block->next = NULL;
    *ptail = free_block;
  }
  resume_all_threads();
  timer_value(&end);
  g_gc_time += timer_diff(&start, &end);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
void * gc_alloc(long size, finalizer_t finalizer, int atomic) {
  if (size < g_min_alloc) {
    size = g_min_alloc;
  } else {
    size = ALIGNUP(size, 8);
  }
  vm_t * vm = vm_get_current();
 again:
  if (vm->nptrs == GC_NDESCR_IN_VM || vm->avail_size < size ||
      g_nptrs >= g_next_gc_ptrs_count ||
      g_ptrs_capacity - g_nptrs <= 2 * GC_NDESCR_IN_VM * g_nthreads) {
    gc_lock();
    if (g_nptrs >= g_next_gc_ptrs_count ||
        g_ptrs_capacity - g_nptrs <= 2 * GC_NDESCR_IN_VM * g_nthreads) {
      gc_collect_internal();
      if (g_ptrs_capacity - g_nptrs < g_ptrs_capacity / 2 &&
          !expand_pdescr_array() && g_ptrs_capacity - g_nptrs < GC_NDESCR_IN_VM) {
        gc_unlock();
        return NULL;
      }
    } else if (vm->nptrs == GC_NDESCR_IN_VM) {
      int i;
      for (i = 0; i < GC_NDESCR_IN_VM; ++i) {
        gc_descr_t * pdescr = g_ptrs + g_nptrs;
        ++g_nptrs;
        *pdescr = vm->ptrs[i];
        pdescr->color = g_curr_color;
      }
      memset(vm->ptrs, 0, sizeof(vm->ptrs));
      vm->nptrs = 0;
    }
    if (vm->avail_size < size) {
      // find free block
      free_block_t * free_block = g_heap_descr->free_blocks;
      while (free_block != NULL && free_block->size < size) {
        free_block = free_block->next;
      }
      if (free_block == NULL) {
        gc_collect_internal();
        if (g_ptrs_capacity - g_nptrs < g_ptrs_capacity / 2) {
          expand_pdescr_array();
        }
        free_block = g_heap_descr->free_blocks;
        while (free_block != NULL && free_block->size < size) {
          free_block = free_block->next;
        }
        if (free_block == NULL) {
          gc_unlock();
          return NULL;
        }
      }
      long block_size = size < (128 * 1024) ? (128 * 1024) : size;
      if (free_block->size <= block_size * 2) {
        vm->avail_mem = (uint8_t *)free_block;
        vm->avail_size = free_block->size;
        g_heap_descr->free_blocks = free_block->next;
      } else {
        vm->avail_mem = (uint8_t *)free_block;
        vm->avail_size = block_size;
        free_block_t * next = free_block->next;
        long new_block_size = free_block->size - block_size;
        g_heap_descr->free_blocks = (free_block_t *)((uint8_t *)free_block + block_size);
        g_heap_descr->free_blocks->size = new_block_size;
        g_heap_descr->free_blocks->next = next;
      }
    }
    gc_unlock();
  }
  pthread_spin_lock(&vm->alloc_spin);
  if (vm->avail_size < size) {
    pthread_spin_unlock(&vm->alloc_spin);
    goto again;
  }
  uint8_t * res = vm->avail_mem;
  gc_descr_t * pdescr = vm->ptrs + vm->nptrs;
  vm->nptrs += 1;
  pdescr->start = res;
  pdescr->end = res + size;
  pdescr->finalizer = finalizer;
  pdescr->atomic = atomic;
  vm->avail_mem = pdescr->end;
  vm->avail_size -= size;
  pthread_spin_unlock(&vm->alloc_spin);
  if (!atomic) {
    bzero(res, size);
  }
  return res;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
void gc_collect() {
  gc_lock();
  gc_collect_internal();
  if (g_ptrs_capacity - g_nptrs < g_ptrs_capacity / 2) {
    expand_pdescr_array();
  }
  gc_unlock();
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
double gc_time() {
  return g_gc_time;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
long gc_threads() {
  return g_nthreads;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
long gc_live_ptrs() {
  return g_nptrs;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
long gc_ptrs_capacity() {
  return g_ptrs_capacity;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
long gc_heap_size() {
  return g_heap_size;
}

//------------------------------------------------------------------------------
// Allocates memory from heap and checks parameters and return value.
// Allocated memory may contain pointers. Block of memory may be
// reallocated.
//------------------------------------------------------------------------------
void * lisp_alloc(unsigned long size, finalizer_t fn) {
  if (size > MAX_ALLOC) {
    lisp_signal(g_max_alloc_size_exceeded, __ulong_to_bigint_term(size));
  }
  void * r = gc_alloc(size, fn, 0);
  if (r == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(size));
  }
  assert(((long)r & 7) == 0);
  return r;
}

//------------------------------------------------------------------------------
// Allocates memory from heap and checks parameters and return value
// Allocated memory can not contain pointers. Block of memory may be
// reallocated.
//------------------------------------------------------------------------------
void * lisp_alloc_atomic(unsigned long size, finalizer_t fn) {
  if (size > MAX_ALLOC) {
    lisp_signal(g_max_alloc_size_exceeded, __ulong_to_bigint_term(size));
  }
  void * r = gc_alloc(size, fn, 1);
  if (r == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(size));
  }
  assert(((long)r & 7) == 0);
  return r;
}

//------------------------------------------------------------------------------
// Reallocates pointer, previosly allocated with lisp_alloc
//------------------------------------------------------------------------------
void * lisp_realloc(void * p, unsigned long old_size, unsigned long new_size) {
  if (new_size == 0) {
    return NULL;
  }
  if (new_size > MAX_ALLOC) {
    lisp_signal(g_max_alloc_size_exceeded, __ulong_to_bigint_term(new_size));
  }
  void * r = gc_alloc(new_size, NULL, 0);
  if (r == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(new_size));
  }
  assert(((long)r & 7) == 0);
  memcpy(r, p, old_size);
  return r;
}

//------------------------------------------------------------------------------
// Reallocates pointer, previosly allocated with lisp_alloc_atomic
//------------------------------------------------------------------------------
void * lisp_realloc_atomic(void * p, unsigned long old_size, unsigned long new_size) {
  if (new_size == 0) {
    return NULL;
  }
  if (new_size > MAX_ALLOC) {
    lisp_signal(g_max_alloc_size_exceeded, __ulong_to_bigint_term(new_size));
  }
  void * r = gc_alloc(new_size, NULL, 1);
  if (r == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(new_size));
  }
  assert(((long)r & 7) == 0);
  memcpy(r, p, old_size);
  return r;
}

//------------------------------------------------------------------------------
// Allocates executable memory from heap and checks parameters and return value.
//------------------------------------------------------------------------------
void * lisp_alloc_executable(unsigned long size) {
  return lisp_alloc_atomic(size, NULL);
}

//------------------------------------------------------------------------------
// Reallocates pointer, previosly allocated with lisp_alloc_executable
//------------------------------------------------------------------------------
void * lisp_realloc_executable(void * p, unsigned long old_size, unsigned long new_size) {
  return lisp_realloc_atomic(p, old_size, new_size);
}
