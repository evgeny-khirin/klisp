///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : file_stream.c
/// Author  : Evgeny Khirin <>
/// Description : File stream implementations.
///-----------------------------------------------------------------------------

#include "klisp.h"

#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


//------------------------------------------------------------------------------
// file_t
//------------------------------------------------------------------------------
typedef struct file_t file_t;

struct file_t {
  stream_t    stream;
  int         fd;
};

//------------------------------------------------------------------------------
// term_to_file
//------------------------------------------------------------------------------
static file_t * stream_to_file(stream_t * s) {
  if (s->class_name != g_file_stream_class_name) {
    SIGNAL_INTERNAL_ERROR("Invalid stream class name ~s", s->class_name);
  }
  return (file_t *)s;
}

//------------------------------------------------------------------------------
// file_finalizer
//------------------------------------------------------------------------------
static void file_finalizer(void * p) {
  file_t * s = (file_t *)p;
  if (s->fd != -1) {
    if (close(s->fd) != 0) {
      lisp_signal_system_error();
    }
    s->fd = -1;
  }
}

//------------------------------------------------------------------------------
// Reads single byte from stream and returns it. Returns nil in case of end-of-file
//------------------------------------------------------------------------------
static term fs_read_byte(stream_t * stream) {
  file_t * s = stream_to_file(stream);
  uint8_t b;
  ssize_t c;
 again:
  c = read(s->fd, &b, 1);
  switch (c) {
  case 0:
    return nil;
  case 1:
    return __long_to_fixnum_term(b);
  case -1:
    if (errno == EINTR) {
      goto again;
    }
    lisp_signal_system_error();
    break;
  }
  SIGNAL_INTERNAL_ERROR();
}

//------------------------------------------------------------------------------
// Writes byte to stream. Error is signaled, if byte cann't be written.
//------------------------------------------------------------------------------
static void fs_write_byte(stream_t * stream, term byte) {
  file_t * s = stream_to_file(stream);
  uint8_t b = term_to_uint8(byte);
  ssize_t c;
 again:
  c = write(s->fd, &b, 1);
  if (c <= 0) {
    if (errno == EINTR) {
      goto again;
    }
    lisp_signal_system_error();
  }
}

//------------------------------------------------------------------------------
// Tries to read maximum 'count' bytes from stream into buffer. Nil is
// returned in case of end of file.
//------------------------------------------------------------------------------
static term fs_read_binary(stream_t * stream, term count, term buf) {
  file_t * s = stream_to_file(stream);
  binary_t * b = get_binary_for_write(buf);
  long c = term_to_long(count);
  if (c > SSIZE_MAX) {
    SIGNAL_INTERNAL_ERROR();
  }
  binary_ensure_capacity(b, c);
 again:
  c = read(s->fd, b->data, c);
  if (c == 0) {
    return nil;
  }
  if (c < 0) {
    if (errno == EINTR) {
      goto again;
    }
    lisp_signal_system_error();
  }
  b->size += c;
  return buf;
}

//------------------------------------------------------------------------------
// Tries to write at least one byte of 'buf' to stream. Error is signaled, if
// nothing is written to stream. Returns number of bytes written to stream.
//------------------------------------------------------------------------------
static term fs_write_binary(stream_t * stream, term buf, term buf_offset, term count) {
  file_t * s = stream_to_file(stream);
  const binary_t * b = get_binary_for_read(buf);
  long offset = term_to_long(buf_offset);
  long len = term_to_long(count);
  if (len > SSIZE_MAX) {
    SIGNAL_INTERNAL_ERROR();
  }
  ssize_t c;
 again:
  c = write(s->fd, b->data + offset, len);
  if (c <= 0) {
    if (errno == EINTR) {
      goto again;
    }
    lisp_signal_system_error();
  }
  return __long_to_fixnum_term(c);
}

//------------------------------------------------------------------------------
// Flush ensures that, if stream performs output buffering, then all modified
// data from buffer are transmitted to operating system.
//------------------------------------------------------------------------------
static void fs_flush(stream_t * stream) {
}

//------------------------------------------------------------------------------
// Sync ensures that all modified data from both user-space and operating
// system buffers are transferred to physical media. The call blocks until the
// device reports that the transfer has completed. It also flushes metadata
// information associated with the stream.
//------------------------------------------------------------------------------
static void fs_sync(stream_t * stream) {
  file_t * s = stream_to_file(stream);
  if (fsync(s->fd) != 0) {
    lisp_signal_system_error();
  }
}

//------------------------------------------------------------------------------
// datasync is similar to sync, but does not flush modified metadata unless
// that metadata is needed in order to allow a subsequent data retrieval to be
// correctly handled.
//------------------------------------------------------------------------------
static void fs_datasync(stream_t * stream) {
  file_t * s = stream_to_file(stream);
  if (fdatasync(s->fd) != 0) {
    lisp_signal_system_error();
  }
}

//------------------------------------------------------------------------------
// Flushes all modified data and closes stream. Stream becomes unusable.
//------------------------------------------------------------------------------
static void fs_close(stream_t * stream) {
  file_t * s = stream_to_file(stream);
  if (close(s->fd) != 0) {
    lisp_signal_system_error();
  }
  s->fd = -1;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void file_init(file_t * f, int fd) {
  stream_t * s = &f->stream;
  s->read_byte = fs_read_byte;
  s->write_byte = fs_write_byte;
  s->read_binary = fs_read_binary;
  s->write_binary = fs_write_binary;
  s->flush = fs_flush;
  s->sync = fs_sync;
  s->datasync = fs_datasync;
  s->close = fs_close;
  f->fd = fd;
}

//------------------------------------------------------------------------------
// Function: (file-stream-create pathname flags &optional mode) ==> file-stream
//------------------------------------------------------------------------------
term make_file_stream(term pathname, term flags, term mode) {
  term res = make_stream(sizeof(file_t), g_file_stream_class_name,
                         pathname, file_finalizer);
  file_t * f = stream_to_file(term_to_stream(res));
  file_init(f, -1);
  const binary_t * p = get_binary_for_read(pathname);
  if ((f->fd = open(binary_get_c_str(p), term_to_long(flags) | O_LARGEFILE,
                    term_to_long(mode))) == -1) {
    lisp_signal_system_error();
  }
  return res;
}

//------------------------------------------------------------------------------
// make_file_stream_from_fd
//------------------------------------------------------------------------------
term make_file_stream_from_fd(int fd, term name) {
  term res = make_stream(sizeof(file_t), g_file_stream_class_name,
                         name, file_finalizer);
  file_t * f = stream_to_file(term_to_stream(res));
  file_init(f, fd);
  return res;
}

//------------------------------------------------------------------------------
// Function: (file-pwrite file file-offset buf &optional (buf-offset 0)
//                        (count (- (binary-length buf) buf-offset))) => integer
//------------------------------------------------------------------------------
term file_pwrite(term file, term file_offset, term buf, term buf_offset, term count) {
  long b_offs = term_to_long(buf_offset);
  long nbytes = term_to_long(count);
  if (nbytes > SSIZE_MAX) {
    SIGNAL_INTERNAL_ERROR();
  }
  const binary_t * b = get_binary_for_read(buf);
  if ((unsigned long)b_offs >= (unsigned long)b->size) {
    lisp_signal(g_out_of_range, buf_offset);
  }
  if (nbytes <= 0 || nbytes > b->size - b_offs) {
    lisp_signal(g_out_of_range, count);
  }
  ssize_t wr;
  file_t * f = stream_to_file(term_to_stream(file));
  mutex_lock(&f->stream.mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &f->stream.mutex) {
  again:
    wr = pwrite64(f->fd, b->data + b_offs, nbytes, term_to_uint64(file_offset));
    if (wr <= 0) {
      if (errno == EINTR) {
        goto again;
      }
      lisp_signal_system_error();
    }
  } UNWIND_PROTECT_END;
  return __long_to_fixnum_term(wr);
}

//------------------------------------------------------------------------------
// Function: (file-pread file offset count &optional (buf (binary))) => integer or nil
//------------------------------------------------------------------------------
term file_pread(term file, term offset, term count, term buf) {
  ssize_t rd;
  binary_t * b = get_binary_for_write(buf);
  long b_count = term_to_long(count);
  if (b_count > SSIZE_MAX) {
    SIGNAL_INTERNAL_ERROR();
  }
  binary_ensure_capacity(b, b_count);
  file_t * f = stream_to_file(term_to_stream(file));
  mutex_lock(&f->stream.mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &f->stream.mutex) {
  again:
    rd = pread64(f->fd, b->data + b->size, b_count, term_to_uint64(offset));
    if (rd < 0) {
      if (errno == EINTR) {
        goto again;
      }
      lisp_signal_system_error();
    }
  } UNWIND_PROTECT_END;
  if (rd == 0) {
    return nil;
  }
  b->size += rd;
  return buf;
}

//------------------------------------------------------------------------------
// Function: (file-seek file offset whence) => integer
//------------------------------------------------------------------------------
term file_seek(term file, term offset, term whence) {
  off_t res;
  file_t * f = stream_to_file(term_to_stream(file));
  mutex_lock(&f->stream.mutex);
  UNWIND_PROTECT_BEGIN((c_unwind_callback)mutex_unlock, &f->stream.mutex) {
    res = lseek(f->fd, term_to_int64(offset), term_to_int(whence));
  } UNWIND_PROTECT_END;
  if (res < 0) {
    lisp_signal_system_error();
  }
  return uint64_to_term(res);
}
