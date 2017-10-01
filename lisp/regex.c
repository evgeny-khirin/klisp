///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : regex.c
/// Author  : Evgeny Khirin <>
/// Description : Posix regular expressions.
///-------------------------------------------------------------------

#include "klisp.h"

#include <sys/types.h>
#include <regex.h>

//------------------------------------------------------------------------------
// regex_finalizer
//------------------------------------------------------------------------------
static void regex_finalizer(void * p) {
  regex_t * r = (regex_t *)p;
  regfree(r);
}

//------------------------------------------------------------------------------
// Function: (regcomp pattern &key extented icase nosub newline) => regex
//------------------------------------------------------------------------------
term lisp_regcomp(term pattern, term extended, term icase, term nosub, term newline) {
  regex_t * r = lisp_alloc_atomic(sizeof(regex_t), regex_finalizer);
  int flags = 0;
  if (!is_null(extended)) {
    flags |= REG_EXTENDED;
  }
  if (!is_null(icase)) {
    flags |= REG_ICASE;
  }
  if (!is_null(nosub)) {
    flags |= REG_NOSUB;
  }
  if (!is_null(newline)) {
    flags |= REG_NEWLINE;
  }
  int rc = regcomp(r, binary_get_c_str(get_binary_for_read(pattern)), flags);
  if (rc != 0) {
    char buf[128];
    regerror(rc, r, buf, sizeof(buf));
    lisp_error_sz(g_regex_error, "%s", buf);
  }
  return make_custom(g_regex_class_name, r);
}

//------------------------------------------------------------------------------
// Function: (regexec regex str &key (str-offset 0) nmatch not-bol not-eol) ==> bool
// If nmatch is not null, then list of matches with maximal length 'nmatch' is
// returned. Each match is described as pair (start-offset . end-offset).
//------------------------------------------------------------------------------
term lisp_regexec(term regex, term str, term str_offset, term nmatch, term not_bol,
                  term not_eol) {
  int flags = 0;
  if (!is_null(not_bol)) {
    flags |= REG_NOTBOL;
  }
  if (!is_null(not_eol)) {
    flags |= REG_NOTEOL;
  }
  const regex_t * r = (regex_t *)term_custom_value(regex, g_regex_class_name);
  size_t nm = (is_null(nmatch) ? 0 : term_to_long(nmatch));
  if (nm < 0 || nm > MAX_ALLOC / sizeof(regmatch_t)) {
    lisp_signal(g_invalid_arg, nmatch);
  }
  const binary_t * s = get_binary_for_read(str);
  long offset = term_to_long(str_offset);
  if ((unsigned long)offset > (unsigned long)s->size) {
    lisp_signal(g_invalid_arg, str_offset);
  }
  regmatch_t * pm = (nm == 0 ? NULL : (regmatch_t *)lisp_alloc_atomic(sizeof(regmatch_t) * nm, NULL));
  int rc = regexec(r, binary_get_c_str(s) + offset, nm, pm, flags);
  if (rc == REG_NOMATCH) {
    return nil;
  }
  if (nm == 0) {
    return g_true;
  }
  term res = nil;
  term * p = &res;
  long i;
  for (i = 0; i < nm; ++i) {
    if (pm[i].rm_so == -1) {
      break;
    }
    *p = LIST_1(cons(__long_to_fixnum_term(pm[i].rm_so), __long_to_fixnum_term(pm[i].rm_eo)));
    p = &get_cons_for_write(*p)->second;
  }
  return res;
}
