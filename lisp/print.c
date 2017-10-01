///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : print.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp print functions.
///-------------------------------------------------------------------

#include "klisp.h"
#include <math.h>
#include <ctype.h>
#include <wchar.h>

//-------------------------------------------------------------------
// NFPRINTF
//-------------------------------------------------------------------
#define NFPRINTF(stream, buf_size, fmt, ...)                             \
  do {                                                                   \
    char __buf__[buf_size];                                              \
    long __len__ = snprintf(__buf__, sizeof(__buf__), fmt, __VA_ARGS__); \
    if (__len__ >= sizeof(__buf__)) {                                    \
      SIGNAL_INTERNAL_ERROR();                                           \
    }                                                                    \
    lisp_fputs((const uint8_t *)__buf__, __len__, stream);               \
  } while (0)

//-------------------------------------------------------------------
// WRITE_BYTE
//-------------------------------------------------------------------
#define WRITE_BYTE(b, fd)   write_byte(uint8_to_term(b), fd)

//-------------------------------------------------------------------
// character printer with escaping
//-------------------------------------------------------------------
static void print_char(term fd, uint8_t c, int escape) {
  if (!escape || (c != '\\' && c != '"' && isprint(c))) {
    WRITE_BYTE(c, fd);
    return;
  }
  switch (c) {
  case '\\':
    PUT_LITERAL_STR("\\\\", fd);
    return;
  case '"':
    PUT_LITERAL_STR("\\\"", fd);
    return;
  case '\a':
    PUT_LITERAL_STR("\\a", fd);
    return;
  case '\b':
    PUT_LITERAL_STR("\\b", fd);
    return;
  case '\f':
    PUT_LITERAL_STR("\\f", fd);
    return;
  case '\n':
    PUT_LITERAL_STR("\\n", fd);
    return;
  case '\r':
    PUT_LITERAL_STR("\\r", fd);
    return;
  case '\t':
    PUT_LITERAL_STR("\\t", fd);
    return;
  case '\v':
    PUT_LITERAL_STR("\\v", fd);
    return;
  case 27:
    PUT_LITERAL_STR("\\e", fd);
    return;
  case 127:
    PUT_LITERAL_STR("\\d", fd);
    return;
  default:
    NFPRINTF(fd, 8, "\\x%02X", c);
  }
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static void print_term(term t, term fd, int escape) {
  switch (get_term_type(t)) {
  case null_e:
    PUT_LITERAL_STR("nil", fd);
    return;
  case fixnum_e:
    NFPRINTF(fd, 32, "%ld", __fixnum_term_to_long(t));
    return;
  case bigint_e:
    lisp_fputsz(mpz_get_str(NULL, 10, __term_to_bigint(t)->mpz), fd);
    return;
  case double_e:
    NFPRINTF(fd, 24, "%.15G", __term_to_double(t));
    return;
  case symbol_e:
    {
      const symbol_t * sym = __term_to_symbol(t);
      if (is_null(sym->package)) {
        PUT_LITERAL_STR("#:", fd);
      } else {
        if (symbol_is_keyword(sym)) {
          WRITE_BYTE(':', fd);
          if (!sym->exported) {
            WRITE_BYTE(':', fd);
          }
        } else if (!sym->exported || !eq(sym->package, g_lisp_package)) {
          write_whole_binary(package_get_name(sym->package), fd);
          WRITE_BYTE(':', fd);
          if (!sym->exported) {
            WRITE_BYTE(':', fd);
          }
        }
      }
      write_whole_binary(sym->name, fd);
    }
    return;
  case binary_e:
    {
      if (escape) {
        WRITE_BYTE('"', fd);
      }
      const binary_t * s = __get_binary_for_read(t);
      long i;
      for (i = 0; i < s->size; ++i) {
        print_char(fd, s->data[i], escape);
      }
      if (escape) {
        WRITE_BYTE('"', fd);
      }
    }
    return;
  case ustring_e:
    SIGNAL_NOT_IMPLEMENTED();
    break;
  case cons_e:
    {
      WRITE_BYTE('(', fd);
      int first_loop = 1;
      do {
        if (first_loop) {
          first_loop = 0;
        } else {
          WRITE_BYTE(' ', fd);
        }
        const cons_t * p = __get_cons_for_read(t);
        print_term(p->first, fd, escape);
        t = p->second;
      } while (is_cons(t));
      if (!is_null(t)) {
        PUT_LITERAL_STR(" . ", fd);
        print_term(t, fd, escape);
      }
      WRITE_BYTE(')', fd);
    }
    return;
  case vector_e:
    {
      PUT_LITERAL_STR("#(", fd);
      const vector_t * v = __get_vector_for_read(t);
      long i;
      for (i = 0; i < v->size; ++i) {
        if (i != 0) {
          WRITE_BYTE(' ', fd);
        }
        print_term(v->data[i], fd, escape);
      }
      WRITE_BYTE(')', fd);
    }
    return;
  case lambda_e:
    {
      const function_t * f = __term_to_function(t);
      if (f->enclosed == NULL) {
        PUT_LITERAL_STR("#<lambda ", fd);
      } else {
        PUT_LITERAL_STR("#<lambda-closure ", fd);
      }
      NFPRINTF(fd, 32, "%p ", f);
      print_term(f->name, fd, escape);
      WRITE_BYTE(' ', fd);
      print_term(f->origin_ll, fd, escape);
      WRITE_BYTE('>', fd);
    }
    return;
  case macro_e:
    {
      const function_t * f = __term_to_function(t);
      if (f->enclosed == NULL) {
        PUT_LITERAL_STR("#<macro ", fd);
      } else {
        PUT_LITERAL_STR("#<macro-closure ", fd);
      }
      NFPRINTF(fd, 32, "%p ", f);
      print_term(f->name, fd, escape);
      WRITE_BYTE(' ', fd);
      print_term(f->origin_ll, fd, escape);
      WRITE_BYTE('>', fd);
    }
    return;
  case custom_e:
    {
      const custom_t * c = (custom_t *)__term_to_pointer(t);
      PUT_LITERAL_STR("#<", fd);
      print_term(c->cl_name, fd, escape);
      NFPRINTF(fd, 32, " %p>", c);
    }
    return;
  }
}

//-------------------------------------------------------------------
// pretty prints term in form suitable for latter parsing
//-------------------------------------------------------------------
static void pprint_term(term t, term fd, int escape) {
  switch (get_term_type(t)) {
  case null_e:
  case fixnum_e:
  case bigint_e:
  case double_e:
  case binary_e:
  case ustring_e:
  case lambda_e:
  case macro_e:
  case custom_e:
    print_term(t, fd, escape);
    return;
  case symbol_e:
    {
      const symbol_t * sym = __term_to_symbol(t);
      if (is_null(sym->package)) {
        PUT_LITERAL_STR("#:", fd);
      } else {
        if (symbol_is_keyword(sym)) {
          WRITE_BYTE(':', fd);
          if (!sym->exported) {
            WRITE_BYTE(':', fd);
          }
        } else if (!eq(sym->package, vm_get_dynamic(g_package_var)) &&
                   !eq(sym->package, g_lisp_package)) {
          write_whole_binary(package_get_name(sym->package), fd);
          WRITE_BYTE(':', fd);
          if (!sym->exported) {
            WRITE_BYTE(':', fd);
          }
        }
      }
      write_whole_binary(sym->name, fd);
    }
    return;
  case cons_e:
    {
      if (!is_cons(cdr(t)) || !is_null(cddr(t))) {
        // list is not proper list of two elements
      print_usual:
        WRITE_BYTE('(', fd);
        int first_loop = 1;
        do {
          if (first_loop) {
            first_loop = 0;
          } else {
            WRITE_BYTE(' ', fd);
          }
          const cons_t * p = __get_cons_for_read(t);
          pprint_term(p->first, fd, escape);
          t = p->second;
        } while (is_cons(t));
        if (!is_null(t)) {
          PUT_LITERAL_STR(" . ", fd);
          pprint_term(t, fd, escape);
        }
        WRITE_BYTE(')', fd);
      } else if (eq(car(t), g_quote)) {
        WRITE_BYTE('\'', fd);
        pprint_term(cadr(t), fd, escape);
      } else if (eq(car(t), g_quasiquote)) {
        WRITE_BYTE('`', fd);
        pprint_term(cadr(t), fd, escape);
      } else if (eq(car(t), g_unquote)) {
        WRITE_BYTE(',', fd);
        pprint_term(cadr(t), fd, escape);
      } else if (eq(car(t), g_unquote_splicing)) {
        PUT_LITERAL_STR(",@", fd);
        pprint_term(cadr(t), fd, escape);
      } else {
        goto print_usual;
      }
    }
    return;
  case vector_e:
    {
      PUT_LITERAL_STR("#(", fd);
      const vector_t * v = __get_vector_for_read(t);
      long i;
      for (i = 0; i < v->size; ++i) {
        if (i != 0) {
          WRITE_BYTE(' ', fd);
        }
        pprint_term(v->data[i], fd, escape);
      }
      WRITE_BYTE(')', fd);
    }
    return;
  }
}

//-------------------------------------------------------------------
// public function
//-------------------------------------------------------------------
term prin1(term x, term stream) {
  print_term(x, stream, 1);
  return x;
}

//-------------------------------------------------------------------
// public function
//-------------------------------------------------------------------
term pprin1(term x, term stream) {
  pprint_term(x, stream, 1);
  return x;
}

//-------------------------------------------------------------------
// public function
//-------------------------------------------------------------------
term princ(term x, term stream) {
  pprint_term(x, stream, 0);
  return x;
}

//-------------------------------------------------------------------
// public function
//-------------------------------------------------------------------
void lisp_fputs(const uint8_t * data, long size, term stream) {
  uint8_t __storage_for_b[sizeof(binary_t) + 8];
  binary_t * b = (binary_t *)ALIGNUP((uintptr_t)__storage_for_b, 8);
  binary_init(b);
  b->data = (uint8_t *)data;
  b->size = size;
  b->immutable = 1;
  write_whole_binary(__pointer_to_term(b), stream);
}

//------------------------------------------------------------------------------
// pstream - parser stream
//------------------------------------------------------------------------------
typedef struct pstream pstream;
struct pstream {
  binary_t    bin;
  long        offset;
  int         ungot_char;
};

static void pstream_init(pstream * s, const char * data, long size) {
  binary_init(&s->bin);
  s->bin.data = (uint8_t *)data;
  s->bin.size = size;
  s->offset = 0;
  s->ungot_char = EOF - 1;
}

static int my_getc(pstream * s) {
  if (s->ungot_char != EOF - 1) {
    int res = s->ungot_char;
    s->ungot_char = EOF - 1;
    return res;
  }
  if (s->offset == s->bin.size) {
    return EOF;
  }
  int res = s->bin.data[s->offset];
  s->offset += 1;
  return res;
}

static void my_ungetc(pstream * s, int c) {
  if (c == EOF - 1 || s->ungot_char != EOF - 1) {
    SIGNAL_INTERNAL_ERROR();
  }
  s->ungot_char = c;
}

static int peek_char(pstream * s) {
  int c = my_getc(s);
  my_ungetc(s, c);
  return c;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int is_flag(int c) {
  return (c == '#' || c == '0' || c == '-' || c == ' ' || c == '+');
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term get_printf_stream(vm_t * vm) {
  if (is_null(vm->printf_stream)) {
    vm->printf_stream = make_binary_stream(__pointer_to_term(vm->printf_res));
  } else {
    binary_stream_rewind(vm->printf_stream, g_true, nil);
  }
  return vm->printf_stream;
};

//------------------------------------------------------------------------------
// internal function
// Positional parameters '%m$' and '*m$' are not supported at this stage.
//------------------------------------------------------------------------------
static void handle_lisp_format(pstream * s, term fmt_term, term arg) {
  vm_t * vm = vm_get_current();
  binary_t * cfmt = vm->printf_cfmt;
  binary_t * res = vm->printf_res;
  binary_truncate(res, 0);
  binary_truncate(cfmt, 0);
  binary_append_uint8(cfmt, '%');
  int c = my_getc(s);
  // copy flags
  while (is_flag(c)) {
    binary_append_uint8(cfmt, c);
    c = my_getc(s);
  }
  // copy field width
  while (isdigit(c)) {
    binary_append_uint8(cfmt, c);
    c = my_getc(s);
  }
  // copy precision
  if (c == '.') {
    binary_append_uint8(cfmt, c);
    while (isdigit(c = my_getc(s))) {
      binary_append_uint8(cfmt, c);
    }
  }
  switch (c) {
  case 'd':
  case 'i':
  case 'o':
  case 'x':
  case 'X':
    {
      switch (get_term_type(arg)) {
      case fixnum_e:
        {
          binary_append_uint8(cfmt, 'l');
          binary_append_uint8(cfmt, c);
          long x = __fixnum_term_to_long(arg);
          long len = snprintf((char *)res->data, res->size, binary_get_c_str(cfmt), x);
          if (len >= res->size) {
            binary_ensure_capacity(res, len + 1);
            snprintf((char *)res->data, res->capacity, binary_get_c_str(cfmt), x);
          }
          res->size = len;
        }
        break;
      case bigint_e:
        {
          binary_append_uint8(cfmt, 'Z');
          binary_append_uint8(cfmt, c);
          const bigint_t * x = __term_to_bigint(arg);
          long len = gmp_snprintf((char *)res->data, res->size, binary_get_c_str(cfmt), x->mpz);
          if (len >= res->size) {
            binary_ensure_capacity(res, len + 1);
            gmp_snprintf((char *)res->data, res->capacity, binary_get_c_str(cfmt), x->mpz);
          }
          res->size = len;
        }
        break;
      default:
        lisp_signal(g_invalid_arg, arg);
      }
    }
    break;
  case 'e':
  case 'E':
  case 'f':
  case 'F':
  case 'g':
  case 'G':
    {
      binary_append_uint8(cfmt, c);
      double x = term_to_double(arg);
      long len = snprintf((char *)res->data, res->size, binary_get_c_str(cfmt), x);
      if (len >= res->size) {
        binary_ensure_capacity(res, len + 1);
        snprintf((char *)res->data, res->capacity, binary_get_c_str(cfmt), x);
      }
      res->size = len;
    }
    break;
  case 'c':
    binary_append_uint8(cfmt, c);
    binary_ensure_capacity(res, 2);
    snprintf((char *)res->data, 2, binary_get_c_str(cfmt), (int)term_to_uint8(arg));
    res->size = 1;
    break;
  case 's':
    prin1(arg, get_printf_stream(vm));
    break;
  case 'S':
    pprin1(arg, get_printf_stream(vm));
    break;
  case 'a':
    princ(arg, get_printf_stream(vm));
    break;
  default:
    lisp_signal(g_invalid_printf_format, fmt_term);
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
term __lisp_fprintf(term stream, term fmt_term, long nargs, const term * args) {
  vm_t * vm = vm_get_current();
  long count = 0;
  int c;
  pstream s;
  const binary_t * fmt = get_binary_for_read(fmt_term);
  pstream_init(&s, (char *)fmt->data, fmt->size);
  long arg_idx = 0;
  stream_lock(stream);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)stream_unlock, (void *) stream) {
    while ((c = my_getc(&s)) != EOF) {
      if (c != '~') {
        ++count;
        WRITE_BYTE(c, stream);
        continue;
      }
      if (peek_char(&s) == '~') {
        ++count;
        my_getc(&s);
        WRITE_BYTE('~', stream);
        continue;
      }
      if (arg_idx == nargs) {
        lisp_signal(g_too_few_printf_args, fmt_term);
      }
      handle_lisp_format(&s, fmt_term, args[arg_idx]);
      ++arg_idx;
      write_whole_binary(__pointer_to_term(vm->printf_res), stream);
      count += vm->printf_res->size;
    }
    if (arg_idx != nargs) {
      lisp_signal(g_too_much_printf_args, fmt_term);
    }
  } UNWIND_PROTECT_END;
  return __long_to_fixnum_term(count);
}

//------------------------------------------------------------------------------
// lisp_printf
//------------------------------------------------------------------------------
term lisp_printf(long nargs, const term * args) {
  if (nargs < 1) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  return __lisp_fprintf(vm_get_dynamic(g_stdout_var), args[0], nargs - 1, args + 1);
}

//------------------------------------------------------------------------------
// lisp_fprintf
//------------------------------------------------------------------------------
term lisp_fprintf(long nargs, const term * args) {
  if (nargs < 2) {
    lisp_signal(g_invalid_args_count, long_to_term(nargs));
  }
  return __lisp_fprintf(args[0], args[1], nargs - 2, args + 2);
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void ensure_int_format_char(binary_t * cfmt, int c) {
  if (c == 'd' || c == 'i' || c == 'o' ||  c == 'u' || c == 'x' || c == 'X') {
    return;
  }
  lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void ensure_format_after_h(binary_t * cfmt, int c) {
  ensure_int_format_char(cfmt, c);
  if (c == 'h' || c == 'n') {
    return;
  }
  lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void ensure_format_after_hh(binary_t * cfmt, int c) {
  ensure_int_format_char(cfmt, c);
  if (c == 'n') {
    return;
  }
  lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void ensure_format_after_l(binary_t * cfmt, int c) {
  ensure_int_format_char(cfmt, c);
  if (c == 'l' || c == 'n' || c == 'c' || c == 's') {
    return;
  }
  lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void ensure_format_after_ll(binary_t * cfmt, int c) {
  ensure_format_after_hh(cfmt, c);
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static void ensure_format_after_L(binary_t * cfmt, int c) {
  if (c == 'a' || c == 'A' || c == 'e' || c == 'E' || c == 'f' ||
      c == 'F' || c == 'g' || c == 'G') {
    return;
  }
  lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
}

//------------------------------------------------------------------------------
// len_modifier_e
//------------------------------------------------------------------------------
typedef enum len_modifier_e {
  lm_none_e,
  lm_short_e,
  lm_char_e,
  lm_long_e,
  lm_long_long_e,
  lm_long_double_e,
  lm_intmax_e,
  lm_size_e,
  lm_ptrdiff_e,
} len_modifier_e;

//------------------------------------------------------------------------------
// PRINTF_ARG
//------------------------------------------------------------------------------
#define PRINTF_ARG(type)                                                \
  do {                                                                  \
    type a = va_arg(args, type);                                         \
    len = snprintf((char *)res->data, res->size, binary_get_c_str(cfmt), a); \
    if (len >= res->size) {                                             \
      binary_ensure_capacity(res, len + 1);                             \
      snprintf((char *)res->data, res->capacity, binary_get_c_str(cfmt), a); \
    }                                                                   \
  } while (0)

//------------------------------------------------------------------------------
// STORE_ARG
//------------------------------------------------------------------------------
#define STORE_ARG(type)                         \
  do {                                          \
    type * p = va_arg(args, type *);            \
    *p = (type)count;                           \
  } while (0)

//------------------------------------------------------------------------------
// lisp_vfprintf_sz
//------------------------------------------------------------------------------
long lisp_vfprintf_sz(term stream, const char * fmt, va_list args) {
  vm_t * vm = vm_get_current();
  long count = 0;
  int c;
  uint8_t __storage_for_fmt_bin[sizeof(binary_t) + 8];
  binary_t * fmt_bin = (binary_t *)ALIGNUP((uintptr_t)__storage_for_fmt_bin, 8);
  binary_init(fmt_bin);
  fmt_bin->data = (uint8_t *)fmt;
  fmt_bin->size = strlen(fmt);
  fmt_bin->immutable = 1;
  term fmt_term = __pointer_to_term(fmt_bin);
  pstream s;
  pstream_init(&s, fmt, fmt_bin->size);
  stream_lock(stream);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)stream_unlock, (void *) stream) {
    while ((c = my_getc(&s)) != EOF) {
      if (c != '~' && c != '%') {
        ++count;
        WRITE_BYTE(c, stream);
        continue;
      }
      if (peek_char(&s) == c) {
        // '~~' or '%%' escape sequence
        ++count;
        my_getc(&s);
        WRITE_BYTE(c, stream);
        continue;
      }
      if (c == '~') {
        // lisp format
        handle_lisp_format(&s, fmt_term, va_arg(args, term));
      } else {
        // C format
        // DON'T PLACE that handling into seperate function, which accepts
        // va_list argument. It will not updated properly on some platforms, for
        // example, linux GCC X86_32.
        binary_t * cfmt = vm->printf_cfmt;
        binary_t * res = vm->printf_res;
        binary_truncate(res, 0);
        binary_truncate(cfmt, 0);
        binary_append_uint8(cfmt, '%');
        int stop = 0;
        long len = 0;
        len_modifier_e len_modifier = lm_none_e;
        while (!stop && (c = my_getc(&s)) != EOF) {
          binary_append_uint8(cfmt, c);
          switch (c) {
          case 'h':
            // h or hh length modifier
            switch (len_modifier) {
            case lm_none_e:
              len_modifier = lm_short_e;
              ensure_format_after_h(cfmt, peek_char(&s));
              break;
            case lm_short_e:
              len_modifier = lm_char_e;
              ensure_format_after_hh(cfmt, peek_char(&s));
              break;
            default:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            break;
          case 'l':
            // l or ll length modifier
            switch (len_modifier) {
            case lm_none_e:
              len_modifier = lm_long_e;
              ensure_format_after_l(cfmt, peek_char(&s));
              break;
            case lm_long_e:
              len_modifier = lm_long_long_e;
              ensure_format_after_ll(cfmt, peek_char(&s));
              break;
            default:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            break;
          case 'L':
            // L length modifier
            if (len_modifier != lm_none_e) {
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            len_modifier = lm_long_double_e;
            ensure_format_after_L(cfmt, peek_char(&s));
            break;
          case 'q':
            // q length modifier
            if (len_modifier != lm_none_e) {
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            len_modifier = lm_long_long_e;
            ensure_format_after_ll(cfmt, peek_char(&s));
            break;
          case 'j':
            // j length modifier
            if (len_modifier != lm_none_e) {
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            len_modifier = lm_intmax_e;
            ensure_int_format_char(cfmt, peek_char(&s));
            break;
          case 'z':
            // z length modifier
            if (len_modifier != lm_none_e) {
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            len_modifier = lm_size_e;
            ensure_int_format_char(cfmt, peek_char(&s));
            break;
          case 't':
            // t length modifier
            if (len_modifier != lm_none_e) {
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            len_modifier = lm_ptrdiff_e;
            ensure_int_format_char(cfmt, peek_char(&s));
            break;
          case 'd':
          case 'i':
          case 'o':
          case 'u':
          case 'x':
          case 'X':
            switch (len_modifier) {
            case lm_none_e:
              PRINTF_ARG(int);
              break;
            case lm_short_e:
              PRINTF_ARG(int);  // short int is promoted to int when passed through
              break;
            case lm_char_e:
              PRINTF_ARG(int);  // char is promoted to int when passed through
              break;
            case lm_long_e:
              PRINTF_ARG(long);
              break;
            case lm_long_long_e:
              PRINTF_ARG(long long);
              break;
            case lm_intmax_e:
              PRINTF_ARG(intmax_t);
              break;
            case lm_size_e:
              PRINTF_ARG(size_t);
              break;
            case lm_ptrdiff_e:
              PRINTF_ARG(ptrdiff_t);
              break;
            case lm_long_double_e:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
              break;
            }
            stop = 1;
            break;
          case 'e':
          case 'E':
          case 'f':
          case 'F':
          case 'g':
          case 'G':
          case 'a':
          case 'A':
            switch (len_modifier) {
            case lm_none_e:
              PRINTF_ARG(double);
              break;
            case lm_long_double_e:
              PRINTF_ARG(long double);
              break;
            default:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            stop = 1;
            break;
          case 'c':
            switch (len_modifier) {
            case lm_none_e:
              PRINTF_ARG(int);
              break;
            case lm_long_e:
              PRINTF_ARG(wint_t);
              break;
            default:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            stop = 1;
            break;
          case 'C':
            switch (len_modifier) {
            case lm_none_e:
              PRINTF_ARG(wint_t);
              break;
            default:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            stop = 1;
            break;
          case 's':
            switch (len_modifier) {
            case lm_none_e:
              PRINTF_ARG(const char *);
              break;
            case lm_long_e:
              PRINTF_ARG(const wchar_t *);
              break;
            default:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            stop = 1;
            break;
          case 'S':
            switch (len_modifier) {
            case lm_none_e:
              PRINTF_ARG(const wchar_t *);
              break;
            default:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            stop = 1;
            break;
          case 'p':
            switch (len_modifier) {
            case lm_none_e:
              PRINTF_ARG(const void *);
              break;
            default:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
            }
            stop = 1;
            break;
          case 'n':
            switch (len_modifier) {
            case lm_none_e:
              STORE_ARG(int);
              break;
            case lm_short_e:
              STORE_ARG(short);
              break;
            case lm_char_e:
              STORE_ARG(char);
              break;
            case lm_long_e:
              STORE_ARG(long);
              break;
            case lm_long_long_e:
              STORE_ARG(long long);
              break;
            case lm_intmax_e:
              STORE_ARG(intmax_t);
              break;
            case lm_size_e:
              STORE_ARG(size_t);
              break;
            case lm_ptrdiff_e:
              STORE_ARG(ptrdiff_t);
              break;
            case lm_long_double_e:
              lisp_signal(g_invalid_printf_format, __pointer_to_term(cfmt));
              break;
            }
            len = 0;
            stop = 1;
            break;
          case 'm':
            len = 0;
            stop = 1;
            break;
          }
        }
        res->size = len;
      }
      write_whole_binary(__pointer_to_term(vm->printf_res), stream);
      count += vm->printf_res->size;
    }
  } UNWIND_PROTECT_END;
  return count;
}
