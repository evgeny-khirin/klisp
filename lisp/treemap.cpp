///-------------------------------------------------------------------
/// Copyright (c) 2009-2011 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : treemap.cpp
/// Author  : Evgeny Khirin <>
/// Description : Erlios Lisp treemap, based on STL map.
///-------------------------------------------------------------------
#include "klisp.h"

#include <map>

//-------------------------------------------------------------------
// Maps term to term
//-------------------------------------------------------------------
namespace {
  //-------------------------------------------------------------------
  // comparison
  //-------------------------------------------------------------------
  struct term_less {
    term fn;

  private:
    term_less();

  public:
    term_less(term fn) {
      this->fn = fn;
    }

    bool operator() (term l, term r) const {
      return term_to_long(FUNCALL(fn, l, r)) < 0;
    }
  };

  typedef std::map<term,
                   term,
                   term_less,
                   gc_allocator<std::pair<const term, term> > > stlmap_t;

  struct map_t {
    stlmap_t  map;
    int       immutable;

    map_t(term test) : map(term_less(test)) {
      immutable = 0;
    }

  private:
    map_t();
  };
} // namespace

//------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------
static const map_t * get_map_for_read(term tbl) {
  return (const map_t *)term_custom_value(tbl, g_treemap_class_name);
}

//------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------
static map_t * get_map_for_write(term tbl) {
  map_t * m = (map_t *)term_custom_value(tbl, g_treemap_class_name);
  if (m->immutable) {
    lisp_signal(g_immutable_object, tbl);
  }
  return m;
}

//------------------------------------------------------------------------
// Function: (treemap-create &optional (test 'compare)) ==> treemap
// Test is (lambda (x y)) and must return negative fixnum if x < y, zero if x = y
// and positive fixnum if x > y.
//------------------------------------------------------------------------
term treemap_create(term test) {
  map_t * m = new(GC) map_t(test);
  term t = make_custom(g_treemap_class_name, m);
  return t;
}

//------------------------------------------------------------------------
// Function: (treemap-insert tbl key value &optional no-old-value) => old-value
//------------------------------------------------------------------------
typedef std::pair<stlmap_t::iterator, bool>  insert_res_t;

term treemap_insert(term tbl, term key, term value, term no_old_value) {
  map_t * m = get_map_for_write(tbl);
  insert_res_t res = m->map.insert(stlmap_t::value_type(key, value));
  if (res.second) {
    __deep_immune_literals(key);
    return no_old_value;
  }
  term old_value = res.first->second;
  res.first->second = value;
  return old_value;
}

//------------------------------------------------------------------------
// Function: (treemap-lookup tbl key (not-found nil)) =>  object
//------------------------------------------------------------------------
term treemap_lookup(term tbl, term key, term not_found) {
  const map_t * m = get_map_for_read(tbl);
  stlmap_t::const_iterator i = m->map.find(key);
  if (i == m->map.end()) {
    return not_found;
  }
  return i->second;
}

//------------------------------------------------------------------------
// Function: (treemap-remove tbl key &optional no-old-value) => old-value
//------------------------------------------------------------------------
term treemap_remove(term tbl, term key, term no_old_value) {
  map_t * m = get_map_for_write(tbl);
  stlmap_t::iterator i = m->map.find(key);
  if (i == m->map.end()) {
    return no_old_value;
  }
  term old_value = i->second;
  m->map.erase(i);
  return old_value;
}

//------------------------------------------------------------------------
// Function: (treemap-size tbl) => integer
//------------------------------------------------------------------------
term treemap_size(term tbl) {
  const map_t * m = get_map_for_read(tbl);
  return long_to_term(m->map.size());
}

//------------------------------------------------------------------------
// Function: (treemap-clear tbl) => tbl
//------------------------------------------------------------------------
term treemap_clear(term tbl) {
  map_t * m = get_map_for_write(tbl);
  m->map.clear();
  return tbl;
}

//------------------------------------------------------------------------
// Function: (treemap-to-vector tbl &optional vec) ==> vector
// Each element of vector is cons, where car is key and cdr is value.
//------------------------------------------------------------------------
term treemap_to_vector(term tbl, term vec) {
  const map_t * m = get_map_for_read(tbl);
  term res = is_null(vec) ? make_vector() : vec;
  vector_t * v = __get_vector_for_write(res);
  vector_ensure_capacity(v, m->map.size());
  long i = 0;
  for (stlmap_t::const_iterator it = m->map.begin(); it != m->map.end(); ++it, ++i) {
    v->data[i] = cons(it->first, it->second);
  }
  v->size += m->map.size();
  return res;
}

//------------------------------------------------------------------------
// Function: (treemap-to-list tbl) ==> list
//------------------------------------------------------------------------
term treemap_to_list(term tbl) {
  const map_t * m = get_map_for_read(tbl);
  term res = nil;
  term * p = &res;
  for (stlmap_t::const_iterator it = m->map.begin(); it != m->map.end(); ++it) {
    *p = LIST_1(cons(it->first, it->second));
    p = &__get_cons_for_write(*p)->second;
  }
  return res;
}

//------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------
static void unimmune_map(void * m) {
  ((map_t *)m)->immutable -= 1;
}

//------------------------------------------------------------------------
// Function: (treemap-do tbl fn) ==> nil
//
// Parameters: tbl - treemap
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in treemap, value returned by 'fn' is
// ignored.
//------------------------------------------------------------------------
term treemap_do(term tbl, term fn) {
  map_t * m = (map_t *)term_custom_value(tbl, g_treemap_class_name);
  UNWIND_PROTECT_BEGIN(unimmune_map, (void *)m) {
    for (stlmap_t::const_iterator it = m->map.begin(); it != m->map.end(); ++it) {
      FUNCALL(fn, it->first, it->second);
    }
  } UNWIND_PROTECT_END;
  return nil;
}

//------------------------------------------------------------------------
// Function: (treemap-do-lower-bound tbl key fn) ==> nil
//
// Parameters: tbl - treemap
//             key - object.
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in treemap, value returned by 'fn' is
// ignored.
// Traversing is started from the first element in the treemap whose key is not
// considered to go before given key (i.e., either it is equivalent or goes
// after). Returns last value returned by fn.
// ------------------------------------------------------------------------
term treemap_do_lower_bound(term tbl, term key, term fn) {
  map_t * m = (map_t *)term_custom_value(tbl, g_treemap_class_name);
  UNWIND_PROTECT_BEGIN(unimmune_map, (void *)m) {
    for (stlmap_t::const_iterator it = m->map.lower_bound(key); it != m->map.end(); ++it) {
      FUNCALL(fn, it->first, it->second);
    }
  } UNWIND_PROTECT_END;
  return nil;
}

//------------------------------------------------------------------------
// Function: (treemap-do-upper-bound tbl key fn) ==> nil
//
// Parameters: tbl - treemap
//             key - object.
//             fn - (lambda (key val))
//
// Invokes 'fn' for each key-value in treemap, value returned by 'fn' is
// ignored.
// Traversing is started from the first element in the treemap whose key is
// considered to go after given key.
// ------------------------------------------------------------------------
term treemap_do_upper_bound(term tbl, term key, term fn) {
  map_t * m = (map_t *)term_custom_value(tbl, g_treemap_class_name);
  UNWIND_PROTECT_BEGIN(unimmune_map, (void *)m) {
    for (stlmap_t::const_iterator it = m->map.upper_bound(key); it != m->map.end(); ++it) {
      FUNCALL(fn, it->first, it->second);
    }
  } UNWIND_PROTECT_END;
  return nil;
}
