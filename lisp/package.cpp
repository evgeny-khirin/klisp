///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : package.cpp
/// Author  : Evgeny Khirin <>
/// Description : Erlios Lisp package management. Package management uses
/// uncollectable STL objects in order to avoid dead locks of garbage
/// collector.
///-------------------------------------------------------------------
#include "klisp.h"

#include <unistd.h>
#include <tr1/unordered_map>

//-------------------------------------------------------------------
// Maps binary to symbol
//-------------------------------------------------------------------
namespace {
  //-------------------------------------------------------------------
  // binary hash code and comparison
  //-------------------------------------------------------------------
  struct binary_hash {
    long operator() (const binary_t * s) const {
      return binary_hash_code((binary_t *)s);
    }
  };

  struct binary_equal {
    bool operator() (const binary_t * l, const binary_t * r) const {
      return binary_cmp(l, r) == 0;
    }
  };

  //-------------------------------------------------------------------
  // binary-to-term map
  //-------------------------------------------------------------------
  typedef std::tr1::unordered_map<const binary_t *, term,
                                  binary_hash, binary_equal> map_t;

  //-------------------------------------------------------------------
  // Insert result
  //-------------------------------------------------------------------
  typedef std::pair<map_t::iterator, bool>  insert_res_t;

  //-------------------------------------------------------------------
  // struct package_t
  //-------------------------------------------------------------------
  struct package_t {
    term            name;
    map_t           map;
    bool            mapped;

    package_t() {
      name = nil;
      mapped = false;
    }
  };
} // namespace

//--------------------------------------------------------------------------
// Global variables
//--------------------------------------------------------------------------
static map_t *      g_pmap = NULL;

//--------------------------------------------------------------------------
// __packages_map_init
//--------------------------------------------------------------------------
void __packages_init() {
  g_pmap = new map_t;
  if (g_pmap == NULL) {
    lisp_signal(g_out_of_memory, __long_to_fixnum_term(sizeof(map_t)));
  }
}

//--------------------------------------------------------------------------
// GC finalizer of Lisp object with type g_package. When package object is
// collected, it may be safely removed from packages map, because it is empty
// and is not referenced.
//--------------------------------------------------------------------------
static void package_finalizer(void * p) {
  package_t * pckg = (package_t *)p;
  if (pckg->mapped) {
    pckg->mapped = false;
    const binary_t * s = get_binary_for_read(pckg->name);
    g_pmap->erase(s);
  }
  // call destructor explicitly
  pckg->~package_t();
}

//--------------------------------------------------------------------------
// Public function
//--------------------------------------------------------------------------
term package_find_create(term name) {
  const binary_t * n = get_binary_for_read(name);
  term res;
  gc_lock();
  map_t::const_iterator it = g_pmap->find(n);
  if (it != g_pmap->end()) {
    res = it->second;
  } else {
    // create package term without lock, in order to avoid dead lock of GC
    gc_unlock();
    package_t * p = (package_t *)lisp_alloc(sizeof(package_t), package_finalizer);
    p = new(p)package_t;
    p->name = __deep_immune_literals(make_binary_from_binary(n->data, n->size));
    n = get_binary_for_read(p->name);
    res = make_custom(g_package_class_name, p);
    gc_lock();
    insert_res_t ins_res = g_pmap->insert(map_t::value_type(n, res));
    if (!ins_res.second) {
      res = ins_res.first->second;
    } else {
      p->mapped = true;
    }
  }
  gc_unlock();
  return res;
}

//--------------------------------------------------------------------------
// Public function
//--------------------------------------------------------------------------
term package_find(const binary_t * name) {
  term res = nil;
  gc_lock();
  map_t::const_iterator it = g_pmap->find(name);
  if (it != g_pmap->end()) {
    res = it->second;
  }
  gc_unlock();
  return res;
}

//------------------------------------------------------------------------------
// Returns list, containing all packages objects.
// Function first copies all packages to array allocated with alloca, and then
// creates list. Otherwise, GC deadlock may occur.
//------------------------------------------------------------------------------
term get_all_packages() {
  term * lst;
  size_t len;
 again:
  len = g_pmap->size();
  lst = (term *)lisp_alloc(len * sizeof(term), NULL);
  gc_lock();
  if (g_pmap->size() < len) {
    len = g_pmap->size();
  } else if (g_pmap->size() > len) {
    gc_unlock();
    goto again;
  }
  size_t i = 0;
  for (map_t::const_iterator it = g_pmap->begin(); it != g_pmap->end(); ++it, ++i) {
    lst[i] = it->second;
  }
  gc_unlock();
  return lisp_list(len, lst);
}

//--------------------------------------------------------------------------
// Public function
//--------------------------------------------------------------------------
term package_get_name(term package) {
  package_t * p = (package_t *)term_custom_value(package, g_package_class_name);
  return p->name;
}

//--------------------------------------------------------------------------
// Public function
//--------------------------------------------------------------------------
term package_find_symbol(term package, const binary_t * name) {
  package_t * p = (package_t *)term_custom_value(package, g_package_class_name);
  term res = nil;
  gc_lock();
  map_t::const_iterator it = p->map.find(name);
  if (it != p->map.end()) {
    res = it->second;
  }
  gc_unlock();
  return res;
}

//--------------------------------------------------------------------------
// Public function
//--------------------------------------------------------------------------
term package_find_create_symbol(term package, term name) {
  const binary_t * n = get_binary_for_read(name);
  binary_get_c_str(n);
  package_t * p = (package_t *)term_custom_value(package, g_package_class_name);
  term res;
  gc_lock();
  map_t::const_iterator it = p->map.find(n);
  if (it != p->map.end()) {
    res = it->second;
  } else {
    // create symbol term without lock, in order to avoid dead lock of GC
    gc_unlock();
    res = make_symbol(name);
    gc_lock();
    n = get_binary_for_read(__term_to_symbol(res)->name);
    insert_res_t ins_res = p->map.insert(map_t::value_type(n, res));
    if (ins_res.second) {
      __term_to_symbol(res)->package = package;
    } else {
      res = ins_res.first->second;
    }
  }
  gc_unlock();
  return res;
}

//--------------------------------------------------------------------------
// Internal public function
//--------------------------------------------------------------------------
void __package_remove_symbol(term package, const binary_t * name) {
  package_t * p = (package_t *)term_custom_value(package, g_package_class_name);
  if (p->mapped) {
    p->map.erase(name);
  }
}

//------------------------------------------------------------------------------
// Returns vector, containing all package symbols.
// Function ensures that vector has enough capacity before map
// iteration. Otherwise, GC deadlock may occur.
//------------------------------------------------------------------------------
term package_symbols(term package) {
  package_t * p = (package_t *)term_custom_value(package, g_package_class_name);
  term res = make_vector();
  vector_t * v = __get_vector_for_write(res);
  size_t size;
 again:
  size = p->map.size();
  vector_ensure_capacity(v, size);
  gc_lock();
  if (p->map.size() > size) {
    gc_unlock();
    goto again;
  }
  for (map_t::const_iterator it = p->map.begin(); it != p->map.end(); ++it) {
    vector_append_term(v, it->second);
  }
  gc_unlock();
  return res;
}
