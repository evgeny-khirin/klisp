///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : custom_stream.c
/// Author  : Evgeny Khirin <>
/// Description : Custom Lisp stream implementations.
///-----------------------------------------------------------------------------

#include "klisp.h"

//------------------------------------------------------------------------------
// binary_stream_t
//------------------------------------------------------------------------------
typedef struct custom_stream_t custom_stream_t;

struct custom_stream_t {
  stream_t    stream;
  term        read_byte;
  term        write_byte;
  term        read_binary;
  term        write_binary;
  term        flush;
  term        sync;
  term        datasync;
  term        close;
};

//------------------------------------------------------------------------------
// stream_to_custom_stream
//------------------------------------------------------------------------------
static custom_stream_t * stream_to_custom_stream(stream_t * s) {
  if (s->class_name != g_custom_stream_class_name) {
    SIGNAL_INTERNAL_ERROR("Invalid stream class name ~s", s->class_name);
  }
  return (custom_stream_t *)s;
}

//------------------------------------------------------------------------------
// Reads single byte from stream and returns it. Returns nil in case of end-of-file
//------------------------------------------------------------------------------
static term cs_read_byte(stream_t * stream) {
  const custom_stream_t * s = stream_to_custom_stream(stream);
  return FUNCALL(s->read_byte);
}

//------------------------------------------------------------------------------
// Writes byte to stream. Error is signaled, if byte cann't be written.
//------------------------------------------------------------------------------
static void cs_write_byte(stream_t * stream, term byte) {
  const custom_stream_t * s = stream_to_custom_stream(stream);
  FUNCALL(s->write_byte, byte);
}

//------------------------------------------------------------------------------
// Tries to read maximum 'count' bytes from stream into buffer. Nil is
// returned in case of end of file.
//------------------------------------------------------------------------------
static term cs_read_binary(stream_t * stream, term count, term buf) {
  const custom_stream_t * s = stream_to_custom_stream(stream);
  return FUNCALL(s->read_binary, count, buf);
}

//------------------------------------------------------------------------------
// Tries to write at least one byte of 'buf' to stream. Error is signaled, if
// nothing is written to stream. Returns number of bytes written to stream.
//------------------------------------------------------------------------------
static term cs_write_binary(stream_t * stream, term buf, term buf_offset, term count) {
  const custom_stream_t * s = stream_to_custom_stream(stream);
  return FUNCALL(s->write_binary, buf, buf_offset, count);
}

//------------------------------------------------------------------------------
// Flush ensures that, if stream performs output buffering, then all modified
// data from buffer are transmitted to operating system.
//------------------------------------------------------------------------------
static void cs_flush(stream_t * stream) {
  const custom_stream_t * s = stream_to_custom_stream(stream);
  FUNCALL(s->flush);
}

//------------------------------------------------------------------------------
// Sync ensures that all modified data from both user-space and operating
// system buffers are transferred to physical media. The call blocks until the
// device reports that the transfer has completed. It also flushes metadata
// information associated with the stream.
//------------------------------------------------------------------------------
static void cs_sync(stream_t * stream) {
  const custom_stream_t * s = stream_to_custom_stream(stream);
  FUNCALL(s->sync);
}

//------------------------------------------------------------------------------
// datasync is similar to sync, but does not flush modified metadata unless
// that metadata is needed in order to allow a subsequent data retrieval to be
// correctly handled.
//------------------------------------------------------------------------------
static void cs_datasync(stream_t * stream) {
  const custom_stream_t * s = stream_to_custom_stream(stream);
  FUNCALL(s->datasync);
}

//------------------------------------------------------------------------------
// Flushes all modified data and closes stream. Stream becomes unusable.
//------------------------------------------------------------------------------
static void cs_close(stream_t * stream) {
  custom_stream_t * s = stream_to_custom_stream(stream);
  FUNCALL(s->close);
  s->read_byte = nil;
  s->write_byte = nil;
  s->read_binary = nil;
  s->write_binary = nil;
  s->flush = nil;
  s->sync = nil;
  s->datasync = nil;
  s->close = nil;
}

//------------------------------------------------------------------------------
// Function:
// (kl:stream-create &key
//  name
//  (read-byte (lambda ()
//               (signal 'unsupported-operation 'read-byte)))
//  (write-byte (lambda (byte)
//                (declare (ignore byte))
//                (signal 'unsupported-operation 'write-byte)))
//  (read-binary (lambda (count buf)
//                 (declare (ignore count buf))
//                 (signal 'unsupported-operation 'read-binary)))
//  (write-binary (lambda (buf buf-offset count)
//                  (declare (ignore buf buf-offset count))
//                  (signal 'unsupported-operation 'write-binary)))
//  (flush (lambda ()
//          (signal 'unsupported-operation 'flush)))
//  (sync (lambda ()
//          (signal 'unsupported-operation 'sync)))
//  (datasync (lambda ()
//              (signal 'unsupported-operation 'datasync)))
//  (close (lambda ()
//           (signal 'unsupported-operation 'close))))
//------------------------------------------------------------------------------
term make_custom_stream(term name, term read_byte, term write_byte,
                        term read_binary, term write_binary, term flush,
                        term sync, term datasync, term close) {
  term res = make_stream(sizeof(custom_stream_t), g_custom_stream_class_name,
                         name, NULL);
  custom_stream_t * cs = stream_to_custom_stream(term_to_stream(res));
  // set custom Lisp implementation functions
  cs->read_byte = read_byte;
  cs->write_byte = write_byte;
  cs->read_binary = read_binary;
  cs->write_binary = write_binary;
  cs->flush = flush;
  cs->sync = sync;
  cs->datasync = datasync;
  cs->close = close;
  // set stream standard C methods
  stream_t * s = &cs->stream;
  s->read_byte = cs_read_byte;
  s->write_byte = cs_write_byte;
  s->read_binary = cs_read_binary;
  s->write_binary = cs_write_binary;
  s->flush = cs_flush;
  s->sync = cs_sync;
  s->datasync = cs_datasync;
  s->close = cs_close;
  return res;
}
