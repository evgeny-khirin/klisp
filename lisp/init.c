///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : init.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp initialization.
///-----------------------------------------------------------------------------

#include "klisp.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <locale.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

//==============================================================================
// Global terms
//==============================================================================
term g_package_class_name = nil;
term g_lisp_package = nil;
term g_kw_package = nil;       // keyword package
term g_package_var = nil;

term g_quote = nil;
term g_quasiquote = nil;
term g_unquote = nil;
term g_unquote_splicing = nil;
term g_function = nil;

term g_unbound_marker = nil;
term g_unbound_fun_marker = nil;

term g_true = nil;

// stream classes
term g_stream_class_name = nil;
term g_binary_stream_class_name = nil;
term g_file_stream_class_name = nil;
term g_custom_stream_class_name = nil;

// standard streams variables
term g_stdin_var = nil;
term g_stdout_var = nil;
term g_stderr_var = nil;

// hashmap class name
term g_hashmap_class_name = nil;

// treemap class name
term g_treemap_class_name = nil;

// treemap class name
term g_avlmap_class_name = nil;

// threads classes
term g_thread_class_name = nil;
term g_mutex_class_name = nil;
term g_condition_class_name = nil;
term g_rwlock_class_name = nil;
term g_semaphore_class_name = nil;
term g_tqueue_class_name = nil;

// sockets
term g_socket_class_name = nil;
term g_socket_stream_class_name = nil;

// regular expressions
term g_regex_class_name = nil;

// random state
term g_random_state_var = nil;

//==============================================================================
// Global varaibles
//==============================================================================
mutex_t           g_symbols_mutex = MUTEX_INITIALIZER;
dlist_t           g_global_symbols = {0};
__thread vm_t *   g_vm = NULL;

//--------------------------------------------------------------------
// Declare exceptions
//--------------------------------------------------------------------
#define DEF_EXCEPTION(e)                     \
  term g_ ## e = nil;

EXCEPTIONS_LIST

#undef DEF_EXCEPTION

//--------------------------------------------------------------------
// Custom allocators for GMP library.
//--------------------------------------------------------------------
static void * my_gmp_alloc(size_t size) {
  return lisp_alloc_atomic(size, NULL);
}

static void * my_gmp_realloc(void *ptr, size_t old_size, size_t new_size) {
  return lisp_realloc_atomic(ptr, old_size, new_size);
}

static void my_gmp_free(void *ptr, size_t size) {
}

//------------------------------------------------------------------------------
// garbage collector init
//------------------------------------------------------------------------------
static void __gc_init(int argc, const char * * argv) {
  dlist_init(&g_global_symbols);
  int i;
  for (i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--heap-size") == 0) {
      break;
    }
  }
  unsigned long heap_size;
  if (i >= argc) {
    heap_size = sysconf(_SC_PHYS_PAGES) * getpagesize();
    if (heap_size > MAX_ALLOC) {
      heap_size = MAX_ALLOC;
    }
    heap_size = heap_size / 4;
  } else {
    ++i;
    char * end;
    heap_size = strtoul(argv[i], &end, 10);
    if (heap_size <= 0 || heap_size > MAX_ALLOC) {
      fprintf(stderr, "Invalid value of \"--heap-size\" argument: %s\n", argv[i]);
      abort();
    }
    if (*end != 0) {
      if (strlen(end) != 2) {
        fprintf(stderr, "Invalid value of \"--heap-size\" argument: %s\n", argv[i]);
        abort();
      }
      if (strcasecmp(end, "KB") == 0) {
        if (heap_size > MAX_ALLOC / 1024) {
          fprintf(stderr, "Invalid value of \"--heap-size\" argument: %s\n", argv[i]);
          abort();
        }
        heap_size *= 1024;
      } else if (strcasecmp(end, "MB") == 0) {
        if (heap_size > MAX_ALLOC / 1024 / 1024) {
          fprintf(stderr, "Invalid value of \"--heap-size\" argument: %s\n", argv[i]);
          abort();
        }
        heap_size *= 1024 * 1024;
      } else  if (strcasecmp(end, "GB") == 0) {
        if (heap_size > MAX_ALLOC / 1024 / 1024 / 1024) {
          fprintf(stderr, "Invalid value of \"--heap-size\" argument: %s\n", argv[i]);
          abort();
        }
        heap_size *= 1024 * 1024 * 1024;
      } else {
        fprintf(stderr, "Invalid value of \"--heap-size\" argument: %s\n", argv[i]);
        abort();
      }
    }
  }
  gc_init(heap_size);
  // init GMP library
  mp_set_memory_functions(my_gmp_alloc,
                          my_gmp_realloc,
                          my_gmp_free);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void init_global_terms() {
  // init unbound markers
  g_unbound_marker = make_symbol_from_sz("unbound-marker");
  g_unbound_fun_marker = make_symbol_from_sz("unbound-fun-marker");

  // init package management
  __packages_init();
  g_package_class_name = make_symbol_from_sz("package");
  g_lisp_package = package_find_create(make_binary_from_sz("kl"));
  g_kw_package = package_find_create(make_binary_from_sz("keyword"));
  g_package_var = make_global_var_from_sz("kl:*package*", g_lisp_package);

  // exceptions
#define DEF_EXCEPTION(e)                          \
  g_ ## e = resolve_symbol_from_sz("kl:" #e);
  EXCEPTIONS_LIST
#undef DEF_EXCEPTION

  // init rest of symbols and variables
  g_quote = resolve_symbol_from_sz("kl:quote");
  g_quasiquote = resolve_symbol_from_sz("kl:quasiquote");
  g_unquote = resolve_symbol_from_sz("kl:unquote");
  g_unquote_splicing = resolve_symbol_from_sz("kl:unquote-splicing");
  g_function = resolve_symbol_from_sz("kl:function");

  g_true = resolve_symbol_from_sz("kl:t");

  // stream classes
  g_stream_class_name = make_symbol_from_sz("stream");
  g_binary_stream_class_name = make_symbol_from_sz("binary-stream");
  g_file_stream_class_name = make_symbol_from_sz("file-stream");
  g_custom_stream_class_name = make_symbol_from_sz("custom-stream");

  // Init evaluator
  __eval_init();

  // standard streams variables
  g_stdin_var = make_global_var_from_sz("kl:*stdin*",
                                        make_file_stream_from_fd(STDIN_FILENO,
                                                                 resolve_symbol_from_sz("kl:*stdin*")));
  g_stdout_var = make_global_var_from_sz("kl:*stdout*",
                                         make_file_stream_from_fd(STDOUT_FILENO,
                                                                  resolve_symbol_from_sz("kl:*stdout*")));
  g_stderr_var = make_global_var_from_sz("kl:*stderr*",
                                         make_file_stream_from_fd(STDERR_FILENO,
                                                                  resolve_symbol_from_sz("kl:*stderr*")));

  // hashmap class name
  g_hashmap_class_name = make_symbol_from_sz("hashmap");

  // treemap class name
  g_treemap_class_name = make_symbol_from_sz("treemap");

  // avlmap class name
  g_avlmap_class_name = make_symbol_from_sz("avlmap");

  // threads classes
  g_thread_class_name = make_symbol_from_sz("thread");
  g_mutex_class_name = make_symbol_from_sz("mutex");
  g_condition_class_name = make_symbol_from_sz("condition");
  g_rwlock_class_name = make_symbol_from_sz("rwlock");
  g_semaphore_class_name = make_symbol_from_sz("semaphore");
  g_tqueue_class_name = make_symbol_from_sz("tqueue");

  // sockets
  g_socket_class_name = make_symbol_from_sz("socket");
  g_socket_stream_class_name = make_symbol_from_sz("socket-stream");

  // regular expressions
  g_regex_class_name = make_symbol_from_sz("regex");

  // *features*
  term features = LIST_1(resolve_symbol_from_sz(":klisp"));
  // set Lisp version
  features =  cons(resolve_symbol_from_sz(":klisp-2014.0002"), features);
  // set OS
#if defined(linux)
  features =  cons(resolve_symbol_from_sz(":linux"), features);
#else
#error Unknown OS
#endif
  // set cpu architecture
#if defined(__x86_64__)
  features = cons(resolve_symbol_from_sz(":x86-64"), features);
  features = cons(resolve_symbol_from_sz(":64-bit"), features);
  features = cons(resolve_symbol_from_sz(":little-endian"), features);
#elif defined(__i386__)
  features =  cons(resolve_symbol_from_sz(":x86-32"), features);
  features = cons(resolve_symbol_from_sz(":32-bit"), features);
  features = cons(resolve_symbol_from_sz(":little-endian"), features);
#else
#error Unknown CPU architecture
#endif
  // set debug mode
#ifdef DEBUG
  features = cons(resolve_symbol_from_sz(":debug"), features);
#endif
  // define *features* variable
  make_global_var_from_sz("kl:*features*", nreverse(features));

  // *random-state* variable
  g_random_state_var = make_global_var_from_sz("kl:*random-state*",
                                               make_random_state(__long_to_fixnum_term(0)));

}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void init_program_args(int argc, const char * * argv) {
  term args = make_vector();
  vector_t * a = get_vector_for_write(args);
  vector_ensure_capacity(a, argc);
  int i;
  for (i = 0; i < argc; ++i) {
    vector_append_term(a, make_binary_from_sz(argv[i]));
  }
  make_global_var_from_sz("kl:*argv*", args);
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
void lisp_global_init(int argc, const char * * argv) {
  static int init_done = 0;
  if (init_done) {
    return;
  }
  init_done = 1;

  // Set default global locale from environment
  setlocale (LC_ALL, "");
  // Init GC
  __gc_init(argc, argv);

  // Ensure that term size at least 4 bytes - Lisp encoding of Unicode character
  // requires at least 4 bytes
  if (sizeof(term) < 4) {
    SIGNAL_INTERNAL_ERROR();
  }

  // Ensure that line size of data cache is OK
  if (sysconf(_SC_LEVEL1_DCACHE_LINESIZE) != DATACACHE_LINESIZE) {
    SIGNAL_INTERNAL_ERROR();
  }

  // Init global variables and terms
  init_global_terms();
  init_program_args(argc, argv);
  __native_init();
  __thread_init();
  __http_init();
}
