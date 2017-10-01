///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : read.c
/// Author  : Evgeny Khirin <>
/// Description : Khirin Lisp reader.
///-----------------------------------------------------------------------------

#include "klisp.h"

#include <ctype.h>
#include <math.h>
#include <stdio.h>

//-------------------------------------------------------------------
// tokens returned by lexer
//-------------------------------------------------------------------
typedef enum token {
  invalid_tok = -1,
  int_tok,
  double_tok,
  char_tok,
  symbol_tok,
  quote_tok,
  quasiquote_tok,
  unquote_tok,
  unquote_splicing_tok,
  eof_tok,
  open_paren_tok,
  close_paren_tok,
  dot_tok,
  binary_tok,
  multibinary_start_tok,
  vector_start_tok,
  function_tok,
} token;

//-------------------------------------------------------------------
// scanner state
//-------------------------------------------------------------------
typedef struct lexer_state lexer_state;

struct lexer_state {
  term        m_raw_stream;
  binary_t *  m_raw_binary;     // make it pointer because it is passed to
                                // __pointer_to_term and must be properly aligned
  binary_t    m_term_binary;
  double      m_double;
  uint8_t     m_char;
  bigint_t    m_bigint;
  token       m_peeked_token;
  long        m_no_eof;
  uint8_t     __storage_for_raw_binary[sizeof(binary_t) + 8];
};

static void lexer_state_init(lexer_state * s) {
  s->m_raw_stream = nil;
  s->m_raw_binary = (binary_t *)ALIGNUP((uintptr_t)s->__storage_for_raw_binary, 8);
  binary_init(s->m_raw_binary);
  binary_init(&s->m_term_binary);
  s->m_peeked_token = invalid_tok;
  bigint_init(&s->m_bigint);
  s->m_no_eof = 0;
}

//-------------------------------------------------------------------
// my_getc
//-------------------------------------------------------------------
static int my_getc(term stream) {
  term b = read_byte(stream);
  if (is_null(b)) {
    return EOF;
  }
  return term_to_uint8(b);
}

//-------------------------------------------------------------------
// my_ungetc
//-------------------------------------------------------------------
static void my_ungetc(term stream, int b) {
  if (b == EOF) {
    unread_byte(nil, stream);
  } else {
    unread_byte(uint8_to_term(b), stream);
  }
}

//-------------------------------------------------------------------
// Peeks character from stream
//-------------------------------------------------------------------
static int peekc(term in) {
  int c = my_getc(in);
  my_ungetc(in, c);
  return c;
}

//-------------------------------------------------------------------
// tries to convert character to digit
//-------------------------------------------------------------------
static int char_to_digit(int c, int radix, int * digit) {
  // there are 26 letters in english
  if (radix < 2 || radix > 36) {
    pprint(long_to_term(radix), vm_get_dynamic(g_stderr_var));
    SIGNAL_INTERNAL_ERROR();
  }
  if (radix <= 10 || c <= '9') {
    *digit = c - '0';
  } else {
    *digit = 10 + toupper(c) - 'A';
  }
  if (*digit < 0 || *digit >= radix) {
    return 0;
  }
  return 1;
}

//-------------------------------------------------------------------
// tries to decode integer constant.
//-------------------------------------------------------------------
static int decode_int(term in, int radix, mpz_t * res) {
  // there are 26 letters in english
  if (radix < 2 || radix > 36) {
    pprint(long_to_term(radix), vm_get_dynamic(g_stderr_var));
    SIGNAL_INTERNAL_ERROR();
  }
  int found_digits = 0;
  int sign = 1;
  switch (peekc(in)) {
  case '+':
    my_getc(in);
    break;
  case '-':
    sign = -1;
    my_getc(in);
    break;
  }
  int c;
  mpz_set_ui(*res, 0);
  while (isdigit(c = my_getc(in)) || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
    int digit;
    if (!char_to_digit(c, radix, &digit)) {
      break;
    }
    found_digits = 1;
    mpz_mul_si(*res, *res, radix);
    mpz_add_ui(*res, *res, digit);
  }
  my_ungetc(in, c);
  if (!found_digits) {
    return 0;
  }
  if (sign < 0) {
    mpz_neg(*res, *res);
  }
  return 1;
}

//-------------------------------------------------------------------
// reads decimal digit and returns its code
//-------------------------------------------------------------------
static int read_digit(lexer_state * state, term in, int radix) {
  int c = my_getc(in);
  if (c == EOF) {
    lisp_signal(g_unexpected_eof, nil);
  }
  int res;
  if (!char_to_digit(c, radix, &res)) {
    lisp_signal(g_syntax_error, make_binary_from_binary(state->m_raw_binary->data,
                                                        state->m_raw_binary->size));
  }
  return res;
}

//-------------------------------------------------------------------
// reads single char, possible escaped
//-------------------------------------------------------------------
static uint8_t read_char(lexer_state * state, term in) {
  int c = my_getc(in);
  if (c == EOF) {
    lisp_signal(g_unexpected_eof, nil);
  }
  if (c != '\\') {
    return c;
  }
  c = my_getc(in);
  switch (c) {
  case 'a':
    return '\a';
  case 'b':
    return '\b';
  case 'f':
    return '\f';
  case 'n':
    return '\n';
  case 'r':
    return '\r';
  case 't':
    return '\t';
  case 'v':
    return '\v';
  case '"':
    return '"';
  case '\\':
    return '\\';
  case 'e':
    return 27;
  case 'd':
    return 127;
  case 'x':
  case 'X':
    // \xhh - two hexadecimal digits followed by x or X
    {
      int h1 = read_digit(state, in, 16);
      int h2 = read_digit(state, in, 16);
      return h1 * 16 + h2;
    }
  case 'o':
  case 'O':
    // \oOOO - three octal digits followed by o or O
    {
      int o1 = read_digit(state, in, 8);
      int o2 = read_digit(state, in, 8);
      int o3 = read_digit(state, in, 8);
      return o1 * 8 * 8 + o2 * 8 + o3;
    }
  default:
    lisp_signal(g_syntax_error, make_binary_from_binary(state->m_raw_binary->data,
                                                        state->m_raw_binary->size));
  }
}

//-------------------------------------------------------------------
// scans binary literal
//-------------------------------------------------------------------
static token read_binary_literal(lexer_state * state, term in) {
  int c;
  while ((c = my_getc(in)) != '"' && c != EOF) {
    my_ungetc(in, c);
    binary_append_uint8(&state->m_term_binary, read_char(state, in));
  }
  if (c != '"') {
    lisp_signal(g_syntax_error, make_binary_from_binary(state->m_raw_binary->data,
                                                        state->m_raw_binary->size));
  }
  return binary_tok;
}

//-------------------------------------------------------------------
// tries to parse unsigned double constant, assumes that stream is null
// terminated.
//-------------------------------------------------------------------
static int parse_double(lexer_state * state) {
  binary_t * s = state->m_raw_binary;
  int n;
  int rc = sscanf(binary_get_c_str(s), "%lf%n", &state->m_double, &n);
  if (rc != 1) {
    return 0;
  }
  // ensure that whole string is scanned
  if (s->size != (long)n) {
    return 0;
  }
  return 1;
}

//-------------------------------------------------------------------
// tries to parse integer constant
//-------------------------------------------------------------------
static int parse_int(term in, mpz_t * res) {
  int radix = 10;
  if (peekc(in) == '#') {
    my_getc(in);
    int c = my_getc(in);
    switch (c) {
    case 'b':
    case 'B':
      radix = 2;
      break;
    case 'o':
    case 'O':
      radix = 8;
      break;
    case 'x':
    case 'X':
      radix = 16;
      break;
    default:
      if (!isdigit(c)) {
        return 0;
      }
      radix = c - '0';
      c = my_getc(in);
      if (isdigit(c)) {
        radix = 10 * radix + (c - '0');
        c = my_getc(in);
      }
      if (c != 'r' && c != 'R') {
        return 0;
      }
    }
  }
  if (!decode_int(in, radix, res)) {
    return 0;
  }
  if (my_getc(in) != EOF) {
    return 0;
  }
  return 1;
}

//-------------------------------------------------------------------
// tries to parse numeric constant
//-------------------------------------------------------------------
static int parse_number(lexer_state * state, token * tok_res) {
  if (is_null(state->m_raw_stream)) {
    state->m_raw_stream = make_binary_stream(__pointer_to_term(state->m_raw_binary));
  } else {
    binary_stream_rewind(state->m_raw_stream, nil, nil);
  }
  if (parse_int(state->m_raw_stream, &state->m_bigint.mpz)) {
    *tok_res = int_tok;
    return 1;
  }
  binary_stream_rewind(state->m_raw_stream, nil, nil);
  if (parse_double(state)) {
    *tok_res = double_tok;
    return 1;
  }
  return 0;
}

//-------------------------------------------------------------------
// tries to parse hash notation
//-------------------------------------------------------------------
static void check_hash_notation(lexer_state * state) {
  if (state->m_raw_binary->size < 2 ||
      state->m_raw_binary->data[0] != '#') {
    return;
  }
  if (state->m_raw_binary->data[1] == '<') {
    // #< hash notation
    lisp_signal(g_syntax_error, make_binary_from_binary(state->m_raw_binary->data,
                                                        state->m_raw_binary->size));
  }
  return;
}

//-------------------------------------------------------------------
// Checks if character is delimeter
//-------------------------------------------------------------------
static int is_delimiter(int c) {
  return isspace(c) || c == EOF || c == '('   || c == ')' || c == ';' ||
    c == '\'' || c == '`' || c == '"';
}

//-------------------------------------------------------------------
// skips white space
//-------------------------------------------------------------------
static void skip_ws(term in) {
  int c;
  while ((c = my_getc(in)) != EOF) {
    if (isspace(c)) {
      continue;
    }
    if (c == ';') {
      // comments
      while (((c = my_getc(in)) != EOF) && (c != '\n')) {
      }
      continue;
    }
    break;
  }
  my_ungetc(in, c);
}

//-------------------------------------------------------------------
// scans input stream for tokens
//-------------------------------------------------------------------
static token scan_tok(lexer_state * state, term in) {
  if (state->m_peeked_token != invalid_tok) {
    token res = state->m_peeked_token;
    state->m_peeked_token = invalid_tok;
    return res;
  }
  skip_ws(in);
  state->m_raw_binary->size = 0;
  int c;
  // read token
  while (!is_delimiter(c = my_getc(in))) {
    if (state->m_raw_binary->size == 0 && c == ',') {
      if (peekc(in) == '@') {
        my_getc(in);
        return unquote_splicing_tok;
      }
      return unquote_tok;
    }
    binary_append_uint8(state->m_raw_binary, c);
  }
  // there is no token, just delimeter
  if (state->m_raw_binary->size == 0) {
    switch (c) {
    case '(':
      return open_paren_tok;
    case ')':
      return close_paren_tok;
    case '\'':
      return quote_tok;
    case '`':
      return quasiquote_tok;
    case '"':
      state->m_term_binary.size = 0;;
      return read_binary_literal(state, in);
    case EOF:
      return eof_tok;
    default:
      lisp_fprintf_sz(vm_get_dynamic(g_stderr_var),
                      "\nUnexpected delimeter '%c'", c);
      SIGNAL_INTERNAL_ERROR();
    }
  }
  if (c == '"' && state->m_raw_binary->size == 1 && state->m_raw_binary->data[0] == '#') {
    // found "#"" - read char const
    state->m_char = read_char(state, in);
    if (my_getc(in) != '"') {
      lisp_signal(g_syntax_error, make_binary_from_binary(state->m_raw_binary->data,
                                                          state->m_raw_binary->size));
    }
    return char_tok;
  }
  if (c == '\'' && state->m_raw_binary->size == 1 && state->m_raw_binary->data[0] == '#') {
    // found "#'" - function token
    return function_tok;
  }
  if (c == '(' && state->m_raw_binary->size == 2 && state->m_raw_binary->data[0] == '#' &&
      state->m_raw_binary->data[1] == 's') {
    // found "#s(" - multibinary
    return multibinary_start_tok;
  }
  if (c == '(' && state->m_raw_binary->size == 1 && state->m_raw_binary->data[0] == '#') {
    // found "#(" - vector
    return vector_start_tok;
  }
  // unget delimeter. Delimiter will be returned by next call, if not space.
  my_ungetc(in, c);
  token res = invalid_tok;
  // try to parse number
  if (parse_number(state, &res)) {
    return res;
  }
  // check for unreadable object ('#<') hash notation
  check_hash_notation(state);
  // may be '.'
  if (state->m_raw_binary->size == 1 && state->m_raw_binary->data[0] == '.') {
    return dot_tok;
  }
  // it is symbol
  return symbol_tok;
}

//-------------------------------------------------------------------
// scans input stream for tokens
//-------------------------------------------------------------------
static token peek_tok(term in, lexer_state * state) {
  if (state->m_peeked_token != invalid_tok) {
    return state->m_peeked_token;
  }
  state->m_peeked_token = scan_tok(state, in);
  return state->m_peeked_token;
}

//-------------------------------------------------------------------
// returns true if binary is equal to "nil" string
//-------------------------------------------------------------------
static int is_nil_literal(const binary_t * s) {
  return s->size == 3 && s->data[0] == 'n' && s->data[1] == 'i' && s->data[2] == 'l';
}

//-------------------------------------------------------------------
// reads term from stream.
//-------------------------------------------------------------------
static term read_term(lexer_state * state, term in, term eof) {
  token tok = scan_tok(state, in);
  switch (tok) {
  case int_tok:
    {
      if (mpz_cmp_si(state->m_bigint.mpz, FIXNUM_MIN) >= 0 &&
          mpz_cmp_si(state->m_bigint.mpz, FIXNUM_MAX) <= 0) {
        return __long_to_fixnum_term(mpz_get_si(state->m_bigint.mpz));
      }
      return __bigint_to_bigint_term(&state->m_bigint);
    }
  case double_tok:
    return double_to_term(state->m_double);
  case binary_tok:
    return make_binary_from_binary(state->m_term_binary.data, state->m_term_binary.size);
  case char_tok:
    return uint8_to_term(state->m_char);
  case symbol_tok:
    if (is_nil_literal(state->m_raw_binary)) {
      return nil;
    }
    return resolve_symbol(make_binary_from_binary(state->m_raw_binary->data,
                                                  state->m_raw_binary->size));
  case quote_tok:
    {
      state->m_no_eof++;
      term t = read_term(state, in, eof);
      state->m_no_eof--;
      return LIST_2(g_quote, t);
    }
  case function_tok:
    {
      state->m_no_eof++;
      term t = read_term(state, in, eof);
      state->m_no_eof--;
      return LIST_2(g_function, t);
    }
  case quasiquote_tok:
    {
      state->m_no_eof++;
      term t = read_term(state, in, eof);
      state->m_no_eof--;
      return LIST_2(g_quasiquote, t);
    }
  case unquote_tok:
    {
      state->m_no_eof++;
      term t = read_term(state, in, eof);
      state->m_no_eof--;
      return LIST_2(g_unquote, t);
    }
  case unquote_splicing_tok:
    {
      state->m_no_eof++;
      term t = read_term(state, in, eof);
      state->m_no_eof--;
      return LIST_2(g_unquote_splicing, t);
    }
  case eof_tok:
    if (state->m_no_eof) {
      lisp_signal(g_unexpected_eof, nil);
    }
    return eof;
  case open_paren_tok:
    {
      if (peek_tok(in, state) == close_paren_tok) {
        // next token is ')'
        scan_tok(state, in);
        return nil;
      }
      if (peek_tok(in, state) == dot_tok) {
        lisp_signal(g_syntax_error, make_binary_from_binary(state->m_raw_binary->data,
                                                            state->m_raw_binary->size));
      }
      state->m_no_eof++;
      term head = LIST_1(read_term(state, in, eof));
      term * cur_node = &__get_cons_for_write(head)->second;
      while (1) {
        if (peek_tok(in, state) == close_paren_tok) {
          // next token is ')'
          scan_tok(state, in);
          state->m_no_eof--;
          return head;
        }
        if (peek_tok(in, state) == dot_tok) {
          // next token is '.'
          scan_tok(state, in);
          term t = read_term(state, in, eof);
          if (scan_tok(state, in) != close_paren_tok) {
            lisp_signal(g_syntax_error, make_binary_from_binary(state->m_raw_binary->data,
                                                                state->m_raw_binary->size));
          }
          *cur_node = t;
          state->m_no_eof--;
          return head;
        }
        term t = read_term(state, in, eof);
        *cur_node = LIST_1(t);
        cur_node = &__get_cons_for_write(*cur_node)->second;
      }
      SIGNAL_INTERNAL_ERROR();
    }
  case close_paren_tok:
  case dot_tok:
    lisp_signal(g_syntax_error, make_binary_from_binary(state->m_raw_binary->data,
                                                        state->m_raw_binary->size));
  case multibinary_start_tok:
    {
      term t = make_binary();
      binary_t * b = __get_binary_for_write(t);
      state->m_no_eof++;
      while (1) {
        if (peek_tok(in, state) == close_paren_tok) {
          // next token is ')'
          scan_tok(state, in);
          state->m_no_eof--;
          return t;
        }
        term obj = read_term(state, in, eof);
        switch (get_term_type(obj)) {
        case fixnum_e:
          binary_append_uint8(b, term_to_uint8(obj));
          break;
        case binary_e:
          {
            const binary_t * s = __get_binary_for_read(obj);
            binary_append_binary(b, s->data, s->size);
          }
          break;
        default:
          lisp_signal(g_invalid_arg, obj);
        }
      }
      SIGNAL_INTERNAL_ERROR();
    }
  case vector_start_tok:
    {
      term t = make_vector();
      vector_t * v = __get_vector_for_write(t);
      state->m_no_eof++;
      while (1) {
        if (peek_tok(in, state) == close_paren_tok) {
          // next token is ')'
          scan_tok(state, in);
          state->m_no_eof--;
          return t;
        }
        vector_append_term(v, read_term(state, in, eof));
      }
      SIGNAL_INTERNAL_ERROR();
    }
  default:
    pprint(long_to_term(tok), vm_get_dynamic(g_stderr_var));
    SIGNAL_INTERNAL_ERROR();
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void read_finished(void * p) {
  mutex_unlock((mutex_t *)p);
}

//------------------------------------------------------------------------------
// Function: (read &optional (stream *stdin*) (eof nil)) ==> object
// Parameters: stream - stream object
//             eof - value returned in case of end of file.
// Returns object parsed from input stream or value of eof parameter in case of
// end of file.
//------------------------------------------------------------------------------
term lisp_read(term stream, term eof) {
  lexer_state state;
  lexer_state_init(&state);
  term res;
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN(read_finished, &s->mutex) {
    res = read_term(&state, stream, eof);
  } UNWIND_PROTECT_END;
  return res;
}
