///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : stream.c
/// Author  : Evgeny Khirin <>
/// Description : stream implementation.
///-----------------------------------------------------------------------------

#include "klisp.h"

//------------------------------------------------------------------------------
// creates empty stream term
//------------------------------------------------------------------------------
term make_stream(long alloc_size, term class_name, term name, finalizer_t finalizer) {
  static mutex_t mutex_template = RECURSIVE_MUTEX_INITIALIZER;
  if (alloc_size < sizeof(stream_t)) {
    SIGNAL_INTERNAL_ERROR("alloc_size %ld", alloc_size);
  }
  stream_t * s = (stream_t *)lisp_alloc(alloc_size, finalizer);
  memset(s, 0, alloc_size);
  s->mutex = mutex_template;
  s->class_name = class_name;
  s->name = name;
  s->ungot_char = g_unbound_marker;
  return make_custom(g_stream_class_name, s);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
void __reset_stream(stream_t * stream) {
  stream->ungot_char = g_unbound_marker;
}

//------------------------------------------------------------------------------
// Function: (unread-byte byte &optional (stream *stdin*)) ==> nil
//
// Parameters: stream - stream
//             byte - integer in range 0 - 255 or nil
//
// Pushes back byte to stream, where it is available for subsequent read
// operations. Pushed back characters will be returned in reverse order; only
// one pushback is guaranteed.
//------------------------------------------------------------------------------
term unread_byte(term byte, term stream) {
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  s->ungot_char = byte;
  mutex_unlock(&s->mutex);
  return nil;
}

//------------------------------------------------------------------------------
// Function: (read-byte &optional (stream *stdin*)) ==> integer in range 0-255 or nil
//
// Parameters: stream - stream
//
// Reads single byte from stream and returns it. Returns nil in case of end-of-file
//------------------------------------------------------------------------------
term read_byte(term stream) {
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  term res;
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    if (!eq(s->ungot_char, g_unbound_marker)) {
      res = s->ungot_char;
      s->ungot_char = g_unbound_marker;
    } else {
      res = s->read_byte(s);
    }
  } UNWIND_PROTECT_END;
  return res;
}

//------------------------------------------------------------------------------
// Function: (write-byte byte &optional (stream *stdout*)) ==> nil
//
// Parameters: stream - stream
//             byte - integer in range 0 - 255
//
// Writes byte to stream. Error is signaled, if byte cann't be written.
//------------------------------------------------------------------------------
term write_byte(term byte, term stream) {
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    s->write_byte(s, byte);
  } UNWIND_PROTECT_END;
  return nil;
}

//------------------------------------------------------------------------------
// Function: (read-binary count &optional (stream *stdin*)
//                        (buf (binary))) ==> buf or nil
//
// Parameters: stream - stream
//             count - max number of bytes to read
//             buf - binary, read data are added to end of buffer.
//
// Tries to read maximum 'count' bytes from stream. Returns buffer. nil is
// returned in case of end-of-file.
//------------------------------------------------------------------------------
term read_binary(term count, term stream, term buf) {
  long c = term_to_long(count);
  if (c <= 0 || c > BINARY_MAX_CAPACITY) {
    lisp_signal(g_invalid_arg, count);
  }
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  term res;
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    if (!eq(s->ungot_char, g_unbound_marker)) {
      count = __long_to_fixnum_term(c - 1);
      binary_append_uint8(get_binary_for_write(buf), term_to_uint8(s->ungot_char));
      s->ungot_char = g_unbound_marker;
      if (c == 1) {
        return buf;
      }
    }
    res = s->read_binary(s, count, buf);
  } UNWIND_PROTECT_END;
  return res;
}

//------------------------------------------------------------------------------
// Function: (read-exact count &optional (stream *stdin*) (buf (binary))) ==> buf
//
// Parameters: stream - stream
//             count - max number of bytes to read
//             buf - binary, read data are added to end of buffer.
//
// Reads exact 'count' bytes from stream. Error is signaled if less bytes are read.
//------------------------------------------------------------------------------
term read_exact(term count, term stream, term buf) {
  stream_t * s = term_to_stream(stream);
  const binary_t * b = get_binary_for_read(buf);
  long c = term_to_long(count);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    while (c != 0) {
      long old_size = b->size;
      term rd = s->read_binary(s, long_to_term(c), buf);
      if (is_null(rd)) {
        lisp_signal(g_unexpected_eof, stream);
      }
      long delta = b->size - old_size;
      if ((unsigned long)delta > (unsigned long)c) {
        SIGNAL_INTERNAL_ERROR();
      }
      c -= delta;
    }
  } UNWIND_PROTECT_END;
  return buf;
}

//------------------------------------------------------------------------------
// Function: (write-binary buf &optional (stream *stdout*) (buf-offset 0)
//                         (count (- (binary-length buf) buf-offset))) ==> integer
//
// Parameters: buf - binary.
//             stream - stream
//             buf-offset - integer. Offset within buffer, from which data are
//               written to stream.
//             count - maximal number of bytes, that should be written to stream.
//
// Tries to write at least one byte of 'buf' to stream. Error is signaled, if
// nothing is written to stream. Returns number of bytes written to stream.
//------------------------------------------------------------------------------
term write_binary(term buf, term stream, term buf_offset, term count) {
  long offset = term_to_long(buf_offset);
  long nbytes = term_to_long(count);
  const binary_t * b = get_binary_for_read(buf);
  if ((unsigned long)offset >= (unsigned long)b->size) {
    lisp_signal(g_out_of_range, buf_offset);
  }
  if (nbytes <= 0 || nbytes > b->size - offset) {
    lisp_signal(g_out_of_range, count);
  }
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  term res;
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    res = s->write_binary(s, buf, buf_offset, count);
  } UNWIND_PROTECT_END;
  return res;
}

//------------------------------------------------------------------------------
// Function: (write-exact buf &optional (stream *stdout*)
//                        buf-offset count) ==> nil
//
// Parameters: buf - binary.
//             stream - stream
//             buf-offset - integer. Offset within buffer, from which data are
//               written to stream. Default is 0.
//             count - exact number of bytes, that should be written to
//               stream. Default is write all bytes starting from offset.
//
// Writes exact number of buffer bytes, starting from given offset, to stream.
//------------------------------------------------------------------------------
term write_exact(term buf, term stream, term buf_offset, term count) {
  const binary_t * b = get_binary_for_read(buf);
  long offset = 0;
  if (!is_null(buf_offset)) {
    offset = term_to_long(buf_offset);
    if ((unsigned long)offset > (unsigned long)b->size) {
      lisp_signal(g_out_of_range, buf_offset);
    }
  }
  long nbytes = b->size - offset;
  if (!is_null(count)) {
    long new_nbytes = term_to_long(count);
    if ((unsigned long)new_nbytes > (unsigned long)nbytes) {
      lisp_signal(g_out_of_range, count);
    }
    nbytes = new_nbytes;
  }
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    while (nbytes != 0) {
      long delta = term_to_long(s->write_binary(s, buf, long_to_term(offset),
                                                long_to_term(nbytes)));
      if ((unsigned long)delta > (unsigned long)nbytes) {
        SIGNAL_INTERNAL_ERROR();
      }
      offset += delta;
      nbytes -= delta;
    }
  } UNWIND_PROTECT_END;
  return nil;
}

//------------------------------------------------------------------------------
// Function: (flush &optional (stream *stdout*)) ==> nil
//
// Parameters: stream - stream
//
// Flush ensures that, if stream performs output buffering, then all modified
// data from buffer are transmitted to operating system.
//------------------------------------------------------------------------------
term stream_flush(term stream) {
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    s->flush(s);
  } UNWIND_PROTECT_END;
  return nil;
}

//------------------------------------------------------------------------------
// Function: (sync &optional (stream *stdout*)) ==> nil
//
// Parameters: stream - stream
//
// Sync ensures that all modified data from both user-space and operating
// system buffers are transferred to physical media. The call blocks until the
// device reports that the transfer has completed. It also flushes metadata
// information associated with the stream.
//------------------------------------------------------------------------------
term stream_sync(term stream) {
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    s->sync(s);
  } UNWIND_PROTECT_END;
  return nil;
}

//------------------------------------------------------------------------------
// Function: (datasync &optional (stream *stdout*)) ==> nil
//
// Parameters: stream - stream
//
// datasync is similar to sync, but does not flush modified metadata unless
// that metadata is needed in order to allow a subsequent data retrieval to be
// correctly handled.
//------------------------------------------------------------------------------
term stream_datasync(term stream) {
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    s->datasync(s);
  } UNWIND_PROTECT_END;
  return nil;
}

//------------------------------------------------------------------------------
// Function: (close stream) ==> nil
//
// Parameters: stream - stream
//
// Flushes all modified data and closes stream. Stream becomes unusable.
//------------------------------------------------------------------------------
term stream_close(term stream) {
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    s->close(s);
  } UNWIND_PROTECT_END;
  return nil;
}

//------------------------------------------------------------------------------
// Function: (read-line &optional (stream *stdin*) max-len buffer) ==> buffer or nil
//
// Parameters: stream - stream.
//             max-len - integer. Maximal length of read line.
//             buffer - binary. If supplied then line is added to buffer.
//
// Reads characters from stream until new line character #\"\\n\" or end of file is
// encountered. Returns read line or nil in case of end of file. If no new line is
// found and number of characters exceeds max-len, then error is signaled"
//------------------------------------------------------------------------------
term read_line(term stream, term max_len, term buffer) {
  term res;
  stream_t * s = term_to_stream(stream);
  mutex_lock(&s->mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &s->mutex) {
    term c = read_byte(stream);
    if (is_null(c)) {
      res = nil;
      goto __unwind_protect_end__;
    }
    long len = 1;
    long len_limit = (is_null(max_len) ? BINARY_MAX_CAPACITY : term_to_long(max_len));
    if (len_limit <= 0 ) {
      lisp_signal(g_invalid_arg, max_len);
    }
    if (is_null(buffer)) {
      buffer = make_binary();
    }
    res = buffer;
    binary_t * line = get_binary_for_write(buffer);
    binary_append_uint8(line, __fixnum_term_to_long(c));
    while (len < len_limit && !is_null(c = read_byte(stream))) {
      if (len == len_limit) {
        unread_byte(stream, c);
        lisp_signal(g_out_of_range, max_len);
      }
      uint8_t byte = __fixnum_term_to_long(c);
      binary_append_uint8(line, byte);
      if (byte == '\n') {
        break;
      }
      ++len;
    }
  } UNWIND_PROTECT_END;
  return res;
}
