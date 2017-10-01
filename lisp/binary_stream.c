///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : binary_stream.c
/// Author  : Evgeny Khirin <>
/// Description : Binary stream implementations.
///-----------------------------------------------------------------------------

#include "klisp.h"

//------------------------------------------------------------------------------
// binary_stream_t
//------------------------------------------------------------------------------
typedef struct binary_stream_t binary_stream_t;

struct binary_stream_t {
  stream_t    stream;
  term        content;
  long        offset;
};

//------------------------------------------------------------------------------
// stream_to_binary_stream
//------------------------------------------------------------------------------
static binary_stream_t * stream_to_binary_stream(stream_t * s) {
  if (s->class_name != g_binary_stream_class_name) {
    SIGNAL_INTERNAL_ERROR("Invalid stream class name ~s", s->class_name);
  }
  return (binary_stream_t *)s;
}

//------------------------------------------------------------------------------
// Reads single byte from stream and returns it. Returns nil in case of end-of-file
//------------------------------------------------------------------------------
static term bs_read_byte(stream_t * stream) {
  binary_stream_t * s = stream_to_binary_stream(stream);
  const binary_t * b = get_binary_for_read(s->content);
  if (s->offset == b->size) {
    return nil;
  }
  return __long_to_fixnum_term(b->data[s->offset++]);
}

//------------------------------------------------------------------------------
// Writes byte to stream. Error is signaled, if byte cann't be written.
//------------------------------------------------------------------------------
static void bs_write_byte(stream_t * stream, term byte) {
  binary_stream_t * s = stream_to_binary_stream(stream);
  binary_t * b = get_binary_for_write(s->content);
  binary_append_uint8(b, term_to_uint8(byte));
}

//------------------------------------------------------------------------------
// Tries to read maximum 'count' bytes from stream into buffer. Nil is
// returned in case of end of file.
//------------------------------------------------------------------------------
static term bs_read_binary(stream_t * stream, term count, term buf) {
  binary_stream_t * s = stream_to_binary_stream(stream);
  const binary_t * str = get_binary_for_read(s->content);
  if (s->offset == str->size) {
    return nil;
  }
  binary_t * b = get_binary_for_write(buf);
  long c = term_to_ulong(count);
  long avail = str->size - s->offset;
  if (c > avail) {
    c = avail;
  }
  binary_append_binary(b, str->data + s->offset, c);
  s->offset += c;
  return buf;
}

//------------------------------------------------------------------------------
// Tries to write at least one byte of 'buf' to stream. Error is signaled, if
// nothing is written to stream. Returns number of bytes written to stream.
//------------------------------------------------------------------------------
static term bs_write_binary(stream_t * stream, term buf, term buf_offset, term count) {
  binary_stream_t * s = stream_to_binary_stream(stream);
  binary_t * str = get_binary_for_write(s->content);
  const binary_t * b = get_binary_for_read(buf);
  long offset = term_to_long(buf_offset);
  long len = term_to_long(count);
  binary_append_binary(str, b->data + offset, len);
  return __long_to_fixnum_term(len);
}

//------------------------------------------------------------------------------
// Flush ensures that, if stream performs output buffering, then all modified
// data from buffer are transmitted to operating system.
//------------------------------------------------------------------------------
static void bs_flush(stream_t * stream) {
}

//------------------------------------------------------------------------------
// Sync ensures that all modified data from both user-space and operating
// system buffers are transferred to physical media. The call blocks until the
// device reports that the transfer has completed. It also flushes metadata
// information associated with the stream.
//------------------------------------------------------------------------------
static void bs_sync(stream_t * stream) {
}

//------------------------------------------------------------------------------
// datasync is similar to sync, but does not flush modified metadata unless
// that metadata is needed in order to allow a subsequent data retrieval to be
// correctly handled.
//------------------------------------------------------------------------------
static void bs_datasync(stream_t * stream) {
}

//------------------------------------------------------------------------------
// Flushes all modified data and closes stream. Stream becomes unusable.
//------------------------------------------------------------------------------
static void bs_close(stream_t * stream) {
  binary_stream_t * s = stream_to_binary_stream(stream);
  s->content = nil;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term make_binary_stream(term content) {
  term res = make_stream(sizeof(binary_stream_t), g_binary_stream_class_name,
                         nil, NULL);
  binary_stream_t * bs = stream_to_binary_stream(term_to_stream(res));
  bs->content = content;
  stream_t * s = &bs->stream;
  s->read_byte = bs_read_byte;
  s->write_byte = bs_write_byte;
  s->read_binary = bs_read_binary;
  s->write_binary = bs_write_binary;
  s->flush = bs_flush;
  s->sync = bs_sync;
  s->datasync = bs_datasync;
  s->close = bs_close;
  return res;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term binary_stream_rewind(term stream, term reset_content, term new_content) {
  stream_t * ss = term_to_stream(stream);
  mutex_lock(&ss->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &ss->mutex) {
    __reset_stream(ss);
    binary_stream_t * s = stream_to_binary_stream(ss);
    if (!is_null(reset_content)) {
      if (is_null(new_content)) {
        binary_truncate(get_binary_for_write(s->content), 0);
      } else {
        s->content = new_content;
      }
    }
    s->offset = 0;
  } UNWIND_PROTECT_END;
  return stream;
}

//------------------------------------------------------------------------------
// Public function
//------------------------------------------------------------------------------
term binary_stream_content(term stream) {
  return stream_to_binary_stream(term_to_stream(stream))->content;
}
