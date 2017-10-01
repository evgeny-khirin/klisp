///-------------------------------------------------------------------
/// Copyright (c) 2009-2011 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : hashmap.cpp
/// Author  : Evgeny Khirin <>
/// Description : Erlios Lisp hashmap, based on STL unordered_map.
///-------------------------------------------------------------------
#include "klisp.h"

#include <tr1/unordered_map>

//-------------------------------------------------------------------
// Maps term to term
//-------------------------------------------------------------------
namespace {
  //-------------------------------------------------------------------
  // hash code
  //-------------------------------------------------------------------
  struct term_hash {
    term fn;

  private:
    term_hash();

  public:
    term_hash(term fn) {
      this->fn = fn;
    }

    long operator() (term x) const {
      return term_to_long(FUNCALL(fn, x));
    }
  };

  //-------------------------------------------------------------------
  // comparison
  //-------------------------------------------------------------------
  struct term_equal {
    term fn;

  private:
    term_equal();

  public:
    term_equal(term fn) {
      this->fn = fn;
    }

    bool operator() (term l, term r) const {
      return !is_null(FUNCALL(fn, l, r));
    }
  };

  //-------------------------------------------------------------------
  // hash map defenition
  //-------------------------------------------------------------------
  typedef std::tr1::unordered_map<term,
                                  term,
                                  term_hash,
                                  term_equal,
                                  gc_allocator<std::pair<const term, term> > > stlmap_t;

  struct map_t {
    stlmap_t  map;
    int       immutable;

    map_t(term test, term hash_code) : map(0, term_hash(hash_code), term_equal(test)) {
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
  return (const map_t *)term_custom_value(tbl, g_hashmap_class_name);
}

//------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------
static map_t * get_map_for_write(term tbl) {
  map_t * m = (map_t *)term_custom_value(tbl, g_hashmap_class_name);
  if (m->immutable) {
    lisp_signal(g_immutable_object, tbl);
  }
  return m;
}

//------------------------------------------------------------------------
// Function: (hashmap-create &optional (test #'equal) (hash-code #'hash-code)) ==> hashmap
//------------------------------------------------------------------------
term hashmap_create(term test, term hash_code) {
  map_t * m = new(GC) map_t(test, hash_code);
  term t = make_custom(g_hashmap_class_name, m);
  return t;
}

//------------------------------------------------------------------------
// Function: (hashmap-insert tbl key value &optional no-old-value) => old-value
//------------------------------------------------------------------------
typedef std::pair<stlmap_t::iterator, bool>  insert_res_t;

term hashmap_insert(term tbl, term key, term value, term no_old_value) {
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
// Function: (hashmap-lookup tbl key (not-found nil)) =>  object
//------------------------------------------------------------------------
term hashmap_lookup(term tbl, term key, term not_found) {
  const map_t * m = get_map_for_read(tbl);
  stlmap_t::const_iterator i = m->map.find(key);
  if (i == m->map.end()) {
    return not_found;
  }
  return i->second;
}

//------------------------------------------------------------------------
// Function: (hashmap-remove tbl key &optional no-old-value) => old-value
//------------------------------------------------------------------------
term hashmap_remove(term tbl, term key, term no_old_value) {
  map_t * m = get_map_for_write(tbl);
  stlmap_t::const_iterator i = m->map.find(key);
  if (i == m->map.end()) {
    return no_old_value;
  }
  term old_value = i->second;
  m->map.erase(key);
  return old_value;
}

//------------------------------------------------------------------------
// Function: (hashmap-size tbl) => integer
//------------------------------------------------------------------------
term hashmap_size(term tbl) {
  const map_t * m = get_map_for_read(tbl);
  return long_to_term(m->map.size());
}

//------------------------------------------------------------------------
// Function: (hashmap-clear tbl) => tbl
//------------------------------------------------------------------------
term hashmap_clear(term tbl) {
  map_t * m = get_map_for_write(tbl);
  m->map.clear();
  return tbl;
}

//------------------------------------------------------------------------
// Function: (hashmap-to-vector tbl &optional vec) ==> vector
// Each element of vector is cons, where car is key and cdr is value.
//------------------------------------------------------------------------
term hashmap_to_vector(term tbl, term vec) {
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
// Function: (hashmap-to-list tbl) ==> list
//------------------------------------------------------------------------
term hashmap_to_list(term tbl) {
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
// Function: (hashmap-do tbl fn) ==> nil
//
// Parameters: tbl - hashmap
//             fn - (lambda (key val)) ==> bool
//
// Invokes 'fn' for each key-value in treemap, value returned by 'fn' is
// ignored.
//------------------------------------------------------------------------
term hashmap_do(term tbl, term fn) {
  const map_t * m = get_map_for_read(tbl);
  ((map_t *)m)->immutable += 1;
  UNWIND_PROTECT_BEGIN(unimmune_map, (void *)m) {
    for (stlmap_t::const_iterator it = m->map.begin(); it != m->map.end(); ++it) {
      FUNCALL(fn, it->first, it->second);
    }
  } UNWIND_PROTECT_END;
  return nil;
}
