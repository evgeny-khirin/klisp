///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : binary.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp binaries.
///-------------------------------------------------------------------

#include "klisp.h"

//-------------------------------------------------------------------
// Serialization signatures. Max number of signatures is 256.
//-------------------------------------------------------------------
#define NULL_S                0

#define SYMBOL_LEN8_S         1
#define SYMBOL_LEN32_S        2
#define SYMBOL_LEN64_S        3

#define INT8_S                4
#define UINT8_S               5

#define INT16_S               6
#define UINT16_S              7

#define INT32_S               8
#define UINT32_S              9

#define INT64_S               10
#define UINT64_S              11

#define POS_BIGINT_LEN8_S     12
#define POS_BIGINT_LEN32_S    13
#define POS_BIGINT_LEN64_S    14

#define NEG_BIGINT_LEN8_S     15
#define NEG_BIGINT_LEN32_S    16
#define NEG_BIGINT_LEN64_S    17

#define DOUBLE_S              18

#define EMPTY_USTRING_S       19
#define USTRING_LEN8_S        20
#define USTRING_LEN32_S       21
#define USTRING_LEN64_S       22

#define EMPTY_BINARY_S        23
#define BINARY_LEN8_S         24
#define BINARY_LEN32_S        25
#define BINARY_LEN64_S        26

#define EMPTY_VECTOR_S        27
#define VECTOR_LEN8_S         28
#define VECTOR_LEN32_S        29
#define VECTOR_LEN64_S        30

#define LIST_START_S          31    // Proper list is terminated by NULL_S element
#define LIST_LAST_S           32    // Stored before last element in dotted list

//-------------------------------------------------------------------
// Public function
//-------------------------------------------------------------------
void binary_append_object(binary_t * buf, term t) {
  buf->hash_valid = 0;
  switch (get_term_type(t)) {
  case null_e:
    binary_append_uint8(buf, NULL_S);
    break;
  case symbol_e:
    {
      const symbol_t * s = __term_to_symbol(t);
      long len = 0;
      const binary_t * pname;
      if (is_null(s->package)) {
        pname = NULL;
        len = 2;
      } else {
        pname = get_binary_for_read(package_get_name(s->package));
        len = pname->size;
        if (s->exported) {
          len += 1;
        } else {
          len += 2;
        }
      }
      const binary_t * sname = get_binary_for_read(s->name);
      len += sname->size;
      if (len <= UINT8_MAX) {
        binary_append_uint8(buf, SYMBOL_LEN8_S);
        binary_append_uint8(buf, len);
      } else if (len <= UINT32_MAX) {
        binary_append_uint8(buf, SYMBOL_LEN32_S);
        binary_append_uint32_be(buf, len);
      } else if (len <= UINT64_MAX) {
        binary_append_uint8(buf, SYMBOL_LEN64_S);
        binary_append_uint64_be(buf, len);
      } else {
        SIGNAL_INTERNAL_ERROR();
      }
      if (pname == NULL) {
        binary_append_uint8(buf, '#');
        binary_append_uint8(buf, ':');
      } else {
        binary_append_binary(buf, pname->data, pname->size);
        binary_append_uint8(buf, ':');
        if (!s->exported) {
          binary_append_uint8(buf, ':');
        }
      }
      binary_append_binary(buf, sname->data, sname->size);
    }
    break;
  case fixnum_e:
    {
      long v = __fixnum_term_to_long(t);
      if (v >= INT8_MIN && v <= INT8_MAX) {
        binary_append_uint8(buf, INT8_S);
        binary_append_int8(buf, v);
      } else if (v >= 0 && v <= UINT8_MAX) {
        binary_append_uint8(buf, UINT8_S);
        binary_append_uint8(buf, v);
      } else if (v >= INT16_MIN && v <= INT16_MAX) {
        binary_append_uint8(buf, INT16_S);
        binary_append_int16_be(buf, v);
      } else if (v >= 0 && v <= UINT16_MAX) {
        binary_append_uint8(buf, UINT16_S);
        binary_append_uint16_be(buf, v);
      } else if (v >= INT32_MIN && v <= INT32_MAX) {
        binary_append_uint8(buf, INT32_S);
        binary_append_int32_be(buf, v);
      } else if (v >= 0 && v <= UINT32_MAX) {
        binary_append_uint8(buf, UINT32_S);
        binary_append_uint32_be(buf, v);
      } else if (v >= INT64_MIN && v <= INT64_MAX) {
        binary_append_uint8(buf, INT64_S);
        binary_append_int64_be(buf, v);
      } else if (v >= 0 && v <= UINT64_MAX) {
        binary_append_uint8(buf, UINT64_S);
        binary_append_uint64_be(buf, v);
      } else {
        SIGNAL_INTERNAL_ERROR();
      }
    }
    break;
  case bigint_e:
    {
      const mpz_t * v = &__term_to_bigint(t)->mpz;
      long len = (mpz_sizeinbase(*v, 2) + 7) / 8;
      if (len < 0) {
        SIGNAL_INTERNAL_ERROR();
      }
      if (len <= UINT8_MAX) {
        if (mpz_sgn(*v) >= 0) {
          binary_append_uint8(buf, POS_BIGINT_LEN8_S);
        } else {
          binary_append_uint8(buf, NEG_BIGINT_LEN8_S);
        }
        binary_append_uint8(buf, len);
      } else if (len <= UINT32_MAX) {
        if (mpz_sgn(*v) >= 0) {
          binary_append_uint8(buf, POS_BIGINT_LEN32_S);
        } else {
          binary_append_uint8(buf, NEG_BIGINT_LEN32_S);
        }
        binary_append_uint32_be(buf, len);
      } else if (len <= UINT64_MAX) {
        if (mpz_sgn(*v) >= 0) {
          binary_append_uint8(buf, POS_BIGINT_LEN64_S);
        } else {
          binary_append_uint8(buf, NEG_BIGINT_LEN64_S);
        }
        binary_append_uint64_be(buf, len);
      } else {
        SIGNAL_INTERNAL_ERROR();
      }
      binary_ensure_capacity(buf, len);
      size_t rlen = 0;
      mpz_export(buf->data + buf->size, &rlen, -1, 1, -1, 0, *v);
      if ((long)rlen != len) {
        SIGNAL_INTERNAL_ERROR();
      }
      buf->size += len;
    }
    break;
  case double_e:
    {
      double d = __term_to_double(t);
      binary_ensure_capacity(buf, sizeof(double) + 1);
      binary_append_uint8(buf, DOUBLE_S);
      *(double *)(buf->data + buf->size) = d;
      buf->size += sizeof(double);
    }
    break;
  case ustring_e:
    {
      const ustring_t * s = __get_ustring_for_read(t);
      if (s->size == 0) {
        binary_append_uint8(buf, EMPTY_USTRING_S);
        break;
      }
      if (s->size <= UINT8_MAX) {
        binary_append_uint8(buf, USTRING_LEN8_S);
        binary_append_uint8(buf, s->size);
      } else if (s->size <= UINT32_MAX) {
        binary_append_uint8(buf, USTRING_LEN32_S);
        binary_append_uint32_be(buf, s->size);
      } else if (s->size <= UINT64_MAX) {
        binary_append_uint8(buf, USTRING_LEN64_S);
        binary_append_uint64_be(buf, s->size);
      } else {
        SIGNAL_INTERNAL_ERROR();
      }
      binary_append_ustring_utf8(buf, s->data, s->size);
    }
    break;
  case binary_e:
    {
      const binary_t * s = __get_binary_for_read(t);
      if (s->size == 0) {
        binary_append_uint8(buf, EMPTY_BINARY_S);
        break;
      }
      if (s->size <= UINT8_MAX) {
        binary_append_uint8(buf, BINARY_LEN8_S);
        binary_append_uint8(buf, s->size);
      } else if (s->size <= UINT32_MAX) {
        binary_append_uint8(buf, BINARY_LEN32_S);
        binary_append_uint32_be(buf, s->size);
      } else if (s->size <= UINT64_MAX) {
        binary_append_uint8(buf, BINARY_LEN64_S);
        binary_append_uint64_be(buf, s->size);
      } else {
        SIGNAL_INTERNAL_ERROR();
      }
      binary_append_binary(buf, s->data, s->size);
    }
    break;
  case vector_e:
    {
      const vector_t * v = __get_vector_for_read(t);
      if (v->size == 0) {
        binary_append_uint8(buf, EMPTY_VECTOR_S);
        break;
      }
      if (v->size <= UINT8_MAX) {
        binary_append_uint8(buf, VECTOR_LEN8_S);
        binary_append_uint8(buf, v->size);
      } else if (v->size <= UINT32_MAX) {
        binary_append_uint8(buf, VECTOR_LEN32_S);
        binary_append_uint32_be(buf, v->size);
      } else if (v->size <= UINT64_MAX) {
        binary_append_uint8(buf, VECTOR_LEN64_S);
        binary_append_uint64_be(buf, v->size);
      } else {
        SIGNAL_INTERNAL_ERROR();
      }
      long i;
      for (i = 0; i < v->size; ++i) {
        binary_append_object(buf, v->data[i]);
      }
    }
    break;
  case cons_e:
    {
      binary_append_uint8(buf, LIST_START_S);
      do {
        const cons_t * p = __get_cons_for_read(t);
        binary_append_object(buf, p->first);
        t = p->second;
      } while (is_cons(t));
      if (is_null(t)) {
        binary_append_uint8(buf, NULL_S);
      } else {
        binary_append_uint8(buf, LIST_LAST_S);
        binary_append_object(buf, t);
      }
    }
    break;
  default:
    lisp_signal(g_invalid_arg, t);
  }
}

//--------------------------------------------------------------------
// Serial buffer on top of binary term.
//--------------------------------------------------------------------
typedef struct ser_buf ser_buf;

struct ser_buf {
  const binary_t *  buffer;
  long              get_offset;
};

static void ser_buf_init(ser_buf * s, const binary_t * buffer, long offset) {
  if ((unsigned long)offset >= (unsigned long)buffer->size) {
    lisp_signal(g_out_of_range, long_to_term(offset));
  }
  s->buffer = buffer;
  s->get_offset = offset;
}

static const void * ser_buf_get_ptr(ser_buf * v, long size) {
  if ((unsigned long)(v->buffer->size - v->get_offset) < (unsigned long)size) {
    lisp_signal(g_out_of_range, long_to_term(size));
  }
  const void * data = &v->buffer->data[v->get_offset];
  v->get_offset += size;
  return data;
}

static uint8_t ser_buf_peek_uint8(ser_buf * v) {
  if (v->get_offset == v->buffer->size) {
    lisp_signal(g_out_of_range, __long_to_fixnum_term(1));
  }
  return v->buffer->data[v->get_offset];
}

static int8_t ser_buf_get_int8(ser_buf * v) {
  int8_t res = binary_get_int8(v->buffer, v->get_offset);
  v->get_offset += sizeof(res);
  return res;
}

static uint8_t ser_buf_get_uint8(ser_buf * v) {
  uint8_t res = binary_get_uint8(v->buffer, v->get_offset);
  v->get_offset += sizeof(res);
  return res;
}

static int16_t ser_buf_get_int16(ser_buf * v) {
  int16_t res = binary_get_int16_be(v->buffer, v->get_offset);
  v->get_offset += sizeof(res);
  return res;
}

static uint16_t ser_buf_get_uint16(ser_buf * v) {
  uint16_t res = binary_get_uint16_be(v->buffer, v->get_offset);
  v->get_offset += sizeof(res);
  return res;
}

static int32_t ser_buf_get_int32(ser_buf * v) {
  int32_t res = binary_get_int32_be(v->buffer, v->get_offset);
  v->get_offset += sizeof(res);
  return res;
}

static uint32_t ser_buf_get_uint32(ser_buf * v) {
  uint32_t res = binary_get_uint32_be(v->buffer, v->get_offset);
  v->get_offset += sizeof(res);
  return res;
}

static int64_t ser_buf_get_int64(ser_buf * v) {
  int64_t res = binary_get_int64_be(v->buffer, v->get_offset);
  v->get_offset += sizeof(res);
  return res;
}

static uint64_t ser_buf_get_uint64(ser_buf * v) {
  uint64_t res = binary_get_uint64_be(v->buffer, v->get_offset);
  v->get_offset += sizeof(res);
  return res;
}

static uint32_t ser_buf_get_uchar(ser_buf * v) {
  if (v->get_offset >= v->buffer->size) {
    lisp_signal(g_out_of_range, __long_to_fixnum_term(v->get_offset));
  }
  uint32_t c;
  int len = u8_mbtouc(&c, v->buffer->data + v->get_offset,
                      v->buffer->size - v->get_offset);
  if (len <= 0) {
    lisp_signal_system_error();
  }
  v->get_offset += len;
  return c;
}

//-------------------------------------------------------------------
// Internal function
//-------------------------------------------------------------------
static term deserialize(ser_buf * buf) {
  uint8_t sign;
  switch ((sign = ser_buf_get_uint8(buf))) {
  case NULL_S:
    return nil;
  case SYMBOL_LEN8_S:
    {
      uint8_t len = ser_buf_get_uint8(buf);
      term name = make_binary();
      binary_t * s = __get_binary_for_write(name);
      binary_ensure_capacity(s, len);
      const uint8_t * p = ser_buf_get_ptr(buf, len);
      binary_append_binary(s, p, len);
      return resolve_symbol(name);
    }
  case SYMBOL_LEN32_S:
    {
      uint32_t len = ser_buf_get_uint32(buf);
      if (len > BINARY_MAX_CAPACITY) {
        lisp_signal(g_out_of_range, uint64_to_term(len));
      }
      term name = make_binary();
      binary_t * s = __get_binary_for_write(name);
      binary_ensure_capacity(s, len);
      const uint8_t * p = ser_buf_get_ptr(buf, len);
      binary_append_binary(s, p, len);
      return resolve_symbol(name);
    }
  case SYMBOL_LEN64_S:
    {
      uint64_t len = ser_buf_get_uint64(buf);
      if (len > BINARY_MAX_CAPACITY) {
        lisp_signal(g_out_of_range, uint64_to_term(len));
      }
      term name = make_binary();
      binary_t * s = __get_binary_for_write(name);
      binary_ensure_capacity(s, len);
      const uint8_t * p = ser_buf_get_ptr(buf, len);
      binary_append_binary(s, p, len);
      return resolve_symbol(name);
    }
  case INT8_S:
    return int8_to_term(ser_buf_get_int8(buf));
  case UINT8_S:
    return uint8_to_term(ser_buf_get_uint8(buf));
  case INT16_S:
    return int16_to_term(ser_buf_get_int16(buf));
  case UINT16_S:
    return uint16_to_term(ser_buf_get_uint16(buf));
  case INT32_S:
    return int32_to_term(ser_buf_get_int32(buf));
  case UINT32_S:
    return uint32_to_term(ser_buf_get_uint32(buf));
  case INT64_S:
    return int64_to_term(ser_buf_get_int64(buf));
  case UINT64_S:
    return uint64_to_term(ser_buf_get_uint64(buf));
  case POS_BIGINT_LEN8_S:
    {
      term res = __make_bigint();
      bigint_t * v = (bigint_t *)__term_to_bigint(res);
      uint8_t len = ser_buf_get_uint8(buf);
      const void * p = ser_buf_get_ptr(buf, len);
      mpz_import(v->mpz, len, -1, 1, -1, 0, p);
      if (mpz_cmp_si(v->mpz, FIXNUM_MAX) <= 0) {
        return __long_to_fixnum_term(mpz_get_si(v->mpz));
      }
      return res;
    }
  case POS_BIGINT_LEN32_S:
    {
      term res = __make_bigint();
      bigint_t * v = (bigint_t *)__term_to_bigint(res);
      uint32_t len = ser_buf_get_uint32(buf);
      if (len > SIZE_MAX) {
        lisp_signal(g_out_of_range, uint32_to_term(len));
      }
      const void * p = ser_buf_get_ptr(buf, len);
      mpz_import(v->mpz, len, -1, 1, -1, 0, p);
      return res;
    }
  case POS_BIGINT_LEN64_S:
    {
      term res = __make_bigint();
      bigint_t * v = (bigint_t *)__term_to_bigint(res);
      uint64_t len = ser_buf_get_uint64(buf);
      if (len > SIZE_MAX) {
        lisp_signal(g_out_of_range, uint64_to_term(len));
      }
      const void * p = ser_buf_get_ptr(buf, len);
      mpz_import(v->mpz, len, -1, 1, -1, 0, p);
      return res;
    }
  case NEG_BIGINT_LEN8_S:
    {
      term res = __make_bigint();
      bigint_t * v = (bigint_t *)__term_to_bigint(res);
      uint8_t len = ser_buf_get_uint8(buf);
      const void * p = ser_buf_get_ptr(buf, len);
      mpz_import(v->mpz, len, -1, 1, -1, 0, p);
      mpz_neg(v->mpz, v->mpz);
      if (mpz_cmp_si(v->mpz, FIXNUM_MIN) >= 0) {
        return __long_to_fixnum_term(mpz_get_si(v->mpz));
      }
      return res;
    }
  case NEG_BIGINT_LEN32_S:
    {
      term res = __make_bigint();
      bigint_t * v = (bigint_t *)__term_to_bigint(res);
      uint32_t len = ser_buf_get_uint32(buf);
      if (len > SIZE_MAX) {
        lisp_signal(g_out_of_range, uint32_to_term(len));
      }
      const void * p = ser_buf_get_ptr(buf, len);
      mpz_import(v->mpz, len, -1, 1, -1, 0, p);
      mpz_neg(v->mpz, v->mpz);
      return res;
    }
  case NEG_BIGINT_LEN64_S:
    {
      term res = __make_bigint();
      bigint_t * v = (bigint_t *)__term_to_bigint(res);
      uint64_t len = ser_buf_get_uint64(buf);
      if (len > SIZE_MAX) {
        lisp_signal(g_out_of_range, uint64_to_term(len));
      }
      const void * p = ser_buf_get_ptr(buf, len);
      mpz_import(v->mpz, len, -1, 1, -1, 0, p);
      mpz_neg(v->mpz, v->mpz);
      return res;
    }
  case DOUBLE_S:
    {
      const double * d = ser_buf_get_ptr(buf, sizeof(double));
      return double_to_term(*d);
    }
  case EMPTY_USTRING_S:
    return make_ustring();
  case USTRING_LEN8_S:
    {
      uint8_t len = ser_buf_get_uint8(buf);
      term res = make_ustring();
      ustring_t * s = __get_ustring_for_write(res);
      ustring_ensure_capacity(s, len);
      long i;
      for (i = 0; i < len; ++i) {
        ustring_append_uchar(s, ser_buf_get_uchar(buf));
      }
      return res;
    }
  case USTRING_LEN32_S:
    {
      uint32_t len = ser_buf_get_uint32(buf);
      if (len > USTRING_MAX_CAPACITY) {
        lisp_signal(g_out_of_range, uint32_to_term(len));
      }
      term res = make_ustring();
      ustring_t * s = __get_ustring_for_write(res);
      ustring_ensure_capacity(s, len);
      uint32_t i;
      for (i = 0; i < len; ++i) {
        ustring_append_uchar(s, ser_buf_get_uchar(buf));
      }
      return res;
    }
  case USTRING_LEN64_S:
    {
      uint64_t len = ser_buf_get_uint64(buf);
      if (len > USTRING_MAX_CAPACITY) {
        lisp_signal(g_out_of_range, uint64_to_term(len));
      }
      term res = make_ustring();
      ustring_t * s = __get_ustring_for_write(res);
      ustring_ensure_capacity(s, len);
      uint64_t i;
      for (i = 0; i < len; ++i) {
        ustring_append_uchar(s, ser_buf_get_uchar(buf));
      }
      return res;
    }
  case EMPTY_BINARY_S:
    return make_binary();
  case BINARY_LEN8_S:
    {
      uint8_t len = ser_buf_get_uint8(buf);
      term res = make_binary();
      binary_t * s = __get_binary_for_write(res);
      binary_ensure_capacity(s, len);
      const uint8_t * p = ser_buf_get_ptr(buf, len);
      binary_append_binary(s, p, len);
      return res;
    }
  case BINARY_LEN32_S:
    {
      uint32_t len = ser_buf_get_uint32(buf);
      if (len > BINARY_MAX_CAPACITY) {
        lisp_signal(g_out_of_range, uint32_to_term(len));
      }
      term res = make_binary();
      binary_t * s = __get_binary_for_write(res);
      binary_ensure_capacity(s, len);
      const uint8_t * p = ser_buf_get_ptr(buf, len);
      binary_append_binary(s, p, len);
      return res;
    }
  case BINARY_LEN64_S:
    {
      uint64_t len = ser_buf_get_uint64(buf);
      if (len > BINARY_MAX_CAPACITY) {
        lisp_signal(g_out_of_range, uint64_to_term(len));
      }
      term res = make_binary();
      binary_t * s = __get_binary_for_write(res);
      binary_ensure_capacity(s, len);
      const uint8_t * p = ser_buf_get_ptr(buf, len);
      binary_append_binary(s, p, len);
      return res;
    }
  case EMPTY_VECTOR_S:
    return make_vector();
  case VECTOR_LEN8_S:
    {
      uint8_t len = ser_buf_get_uint8(buf);
      term res = make_vector();
      vector_t * v = __get_vector_for_write(res);
      vector_ensure_capacity(v, len);
      long i;
      for (i = 0; i < len; ++i) {
        vector_append_term(v, deserialize(buf));
      }
      return res;
    }
  case VECTOR_LEN32_S:
    {
      uint32_t len = ser_buf_get_uint32(buf);
      if (len > VECTOR_MAX_CAPACITY) {
        lisp_signal(g_out_of_range, uint32_to_term(len));
      }
      term res = make_vector();
      vector_t * v = __get_vector_for_write(res);
      vector_ensure_capacity(v, len);
      uint32_t i;
      for (i = 0; i < len; ++i) {
        vector_append_term(v, deserialize(buf));
      }
      return res;
    }
  case VECTOR_LEN64_S:
    {
      uint64_t len = ser_buf_get_uint64(buf);
      if (len > VECTOR_MAX_CAPACITY) {
        lisp_signal(g_out_of_range, uint64_to_term(len));
      }
      term res = make_vector();
      vector_t * v = __get_vector_for_write(res);
      vector_ensure_capacity(v, len);
      uint64_t i;
      for (i = 0; i < len; ++i) {
        vector_append_term(v, deserialize(buf));
      }
      return res;
    }
  case LIST_START_S:
    {
      term t = nil;
      term * cur_node = &t;
      while (1) {
        switch (ser_buf_peek_uint8(buf)) {
        case NULL_S:
          buf->get_offset += 1;
          return t;
        case LIST_LAST_S:
          buf->get_offset += 1;
          *cur_node = deserialize(buf);
          return t;
        default:
          *cur_node = cons(deserialize(buf), nil);
          cur_node = &__get_cons_for_write(*cur_node)->second;
        }
      }
      SIGNAL_INTERNAL_ERROR();
    }
  default:
    lisp_signal(g_bad_signature, __long_to_fixnum_term(sign));
  }
}

//-------------------------------------------------------------------
// Public function
//-------------------------------------------------------------------
term binary_decode_object(const binary_t * buf, long offset) {
  ser_buf b;
  ser_buf_init(&b, buf, offset);
  term obj = deserialize(&b);
  return VALUES(obj, __long_to_fixnum_term(b.get_offset - offset));
}
