///-------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : sockets.c
/// Author  : Evgeny Khirin <>
/// Description : Lisp sockets.
///-------------------------------------------------------------------

#include "klisp.h"

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <poll.h>

//==============================================================================
// RESOLVER
//==============================================================================
//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void * my_gethostbyname(const char * name, int af, struct sockaddr_storage * addr) {
  struct hostent ret = {0};
  size_t buf_size = 128;
  struct hostent *result;
  char * buf = alloca(buf_size);
  int herr = 0;
  while (gethostbyname2_r(name, af, &ret, buf, buf_size, &result, &herr) == ERANGE) {
    buf_size *= 2;
    buf = alloca(buf_size);
  }
  if (result == NULL) {
    if (herr == HOST_NOT_FOUND) {
      return NULL;
    }
    lisp_signal(g_networking, make_binary_from_sz(hstrerror(herr)));
  }
  switch (af) {
  case AF_INET:
    *((struct in_addr *)addr) = *((struct in_addr *)ret.h_addr);
    break;
  case AF_INET6:
    *((struct in6_addr *)addr) = *((struct in6_addr *)ret.h_addr);
    break;
  default:
    SIGNAL_INTERNAL_ERROR("unknown socket address family %d", af);
  }
  return addr;
}

//------------------------------------------------------------------------------
// Function: (gethostbyname name &optional (address-family AF_INET)) ==> string
//------------------------------------------------------------------------------
term lisp_gethostbyname(term name, term addr_family) {
  struct sockaddr_storage addr_storage;
  int af = term_to_long(addr_family);
  if (my_gethostbyname(binary_get_c_str(get_binary_for_read(name)),
                       af, &addr_storage) == NULL) {
    return nil;
  }
  char buf[128];
  if (inet_ntop(af, &addr_storage, buf, sizeof(buf)) == NULL) {
    lisp_signal_system_error();
  }
  return make_binary_from_sz(buf);
}

//------------------------------------------------------------------------------
// Function: (gethostname) ==> string
//------------------------------------------------------------------------------
term lisp_gethostname() {
  char name[HOST_NAME_MAX + 1];
  if (gethostname(name, sizeof(name)) != 0) {
    lisp_signal_system_error();
  }
  return make_binary_from_sz(name);
}

//==============================================================================
// SOCKETS
//==============================================================================
//------------------------------------------------------------------------------
// socket_t
//------------------------------------------------------------------------------
typedef struct socket_t socket_t;

struct socket_t {
  term        timeout;
  int         sd;               // socket descriptor
};

//------------------------------------------------------------------------------
// term_to_socket
//------------------------------------------------------------------------------
static socket_t * term_to_socket(term t) {
  return (socket_t *)term_custom_value(t, g_socket_class_name);
}

//------------------------------------------------------------------------------
// socket_finalizer
//------------------------------------------------------------------------------
static void socket_finalizer(void * p) {
  socket_t * s = (socket_t *)p;
  if (s->sd != -1) {
    if (close(s->sd) != 0) {
      lisp_signal_system_error();
    }
    s->sd = -1;
  }
}

//------------------------------------------------------------------------------
// make_socket
//------------------------------------------------------------------------------
static term make_socket(int domain, int type, int protocol, int create) {
  socket_t * s = (socket_t *)lisp_alloc(sizeof(socket_t), socket_finalizer);
  s->timeout = nil;
  if (!create) {
    s->sd = -1;
  } else {
    s->sd = socket(domain, type, protocol);
    if (s->sd == -1) {
      lisp_signal_system_error();
    }
  }
  return make_custom(g_socket_class_name, s);
}

//------------------------------------------------------------------------------
// Function: (socket-close socket) ==> nil
//------------------------------------------------------------------------------
term socket_close(term sock) {
  socket_t * s = term_to_socket(sock);
  if (close(s->sd) != 0) {
    lisp_signal_system_error();
  }
  s->sd = -1;
  return nil;
}

//------------------------------------------------------------------------------
// Function: (socket-set-timeout socket timeout) ==> socket
//------------------------------------------------------------------------------
term socket_set_timeout(term sock, term timeout) {
  term_to_socket(sock)->timeout = timeout;
  return sock;
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
static term socket_poll(term sock, term timeout, int events) {
  long time = term_to_long(timeout);
  if (time < 0 || time > INT_MAX / 1000) {
    time = -1;
  } else {
    time = time * 1000;
  }
  const socket_t * s = term_to_socket(sock);
  struct pollfd rfds = {s->sd, events};
  int rc;
 again:
  rc = poll(&rfds, 1, time);
  if(rc == -1) {
    if (errno == EINTR) {
      goto again;
    }
    lisp_signal_system_error();
  }
  if (rc == 0) {
    return nil;
  }
  if (rc != 1) {
    SIGNAL_INTERNAL_ERROR("rc = %d, sd = %d", rc, s->sd);
  }
  if ((rfds.revents & POLLERR) == POLLERR) {
    socklen_t optlen = sizeof(rc);
    if (getsockopt(s->sd, SOL_SOCKET, SO_ERROR, &rc, &optlen) != 0) {
      lisp_signal_system_error();
    }
    if (rc != 0) {
      errno = rc;
      lisp_signal_system_error();
    }
  }
  return g_true;
}

//------------------------------------------------------------------------------
// Function: (socket-select-read socket timeout) ==> bool
//------------------------------------------------------------------------------
term socket_select_read(term sock, term timeout) {
  return socket_poll(sock, timeout, POLLIN);
}

//------------------------------------------------------------------------------
// Function: (socket-select-write socket timeout) ==> bool
//------------------------------------------------------------------------------
term socket_select_write(term sock, term timeout) {
  return socket_poll(sock, timeout, POLLOUT);
}

//------------------------------------------------------------------------------
// Function: (socket-accept socket) ==> socket or nil
// Returns nil if socket timeout is expired. Accepted socket does not have time.
//------------------------------------------------------------------------------
term socket_accept(term sock) {
  const socket_t * s = term_to_socket(sock);
  if (!is_null(s->timeout)) {
    if (is_null(socket_select_read(sock, s->timeout))) {
      return nil;
    }
  }
  term asock = make_socket(0, 0, 0, 0);
  socket_t * as = term_to_socket(asock);
 again:
  as->sd = accept(s->sd, NULL, NULL);
  if (as->sd == -1) {
    if (errno == EINTR) {
      goto again;
    }
    lisp_signal_system_error();
  }
  return asock;
}

//------------------------------------------------------------------------------
// Function: (socket-peer-address socket) ==> (values ip-string port-num)
//------------------------------------------------------------------------------
term socket_peer_address(term sock) {
  struct sockaddr peer_addr;
  socklen_t addr_len = sizeof(peer_addr);
  if (getpeername(term_to_socket(sock)->sd, &peer_addr, &addr_len) != 0) {
    lisp_signal_system_error();
  }
  if (addr_len != sizeof(struct sockaddr_in)) {
    SIGNAL_INTERNAL_ERROR();
  }
#if (INET_ADDRSTRLEN >= 128 || INET6_ADDRSTRLEN >= 128)
#error Increase buffer size
#endif
  char buf[128];
  if (inet_ntop(peer_addr.sa_family, &((struct sockaddr_in *)&peer_addr)->sin_addr,
                buf, sizeof(buf)) == NULL) {
    lisp_signal_system_error();
  }
  return VALUES(make_binary_from_sz(buf),
                uint16_to_term(ntohs(((struct sockaddr_in *)&peer_addr)->sin_port)));
}

//==============================================================================
// TCP specific functions
//==============================================================================
//------------------------------------------------------------------------------
// Function: (tcp-connect host port &key timeout nodelay
//                        bind-addr bind-port) ==> socket
//------------------------------------------------------------------------------
term tcp_connect(term host, term port, term timeout, term nodelay,
                 term bind_addr, term bind_port) {
  struct sockaddr_storage addr_storage;
  if (my_gethostbyname(binary_get_c_str(get_binary_for_read(host)),
                       AF_INET, &addr_storage) == NULL) {
    lisp_signal(g_networking, make_binary_from_sz(hstrerror(HOST_NOT_FOUND)));
  }
  term sock = make_socket(AF_INET, SOCK_STREAM, 0, 1);
  socket_t * s = term_to_socket(sock);
  s->timeout = timeout;
  if (!is_null(nodelay)) {
    int flag = 1;
    int rc = setsockopt(s->sd,          /* socket affected */
                        IPPROTO_TCP,    /* set option at TCP level */
                        TCP_NODELAY,    /* name of option */
                        (char *) &flag, /* the cast is historical
                                           cruft */ //
                        sizeof(int)); /* length of option value */
    if (rc != 0) {
      lisp_signal_system_error();
    }
  }
  if (!is_null(bind_addr) || !is_null(bind_port)) {
    struct sockaddr_in local_addr = {AF_INET};
    if (!is_null(bind_port)) {
      local_addr.sin_port = htons(term_to_uint16(bind_port));
    }
    if (!is_null(bind_addr)) {
      struct sockaddr_storage addr_storage_2;
      if (my_gethostbyname(binary_get_c_str(get_binary_for_read(bind_addr)),
                           AF_INET, &addr_storage_2) == NULL) {
        lisp_error_sz(g_networking, "could not resolve local binding address ~s",
                      bind_addr);
      }
      local_addr.sin_addr = *((struct in_addr *)&addr_storage_2);
    }
    if (bind(s->sd, &local_addr, sizeof(local_addr)) != 0) {
      lisp_signal_system_error();
    }
  }
  if (!is_null(timeout)) {
    fcntl(s->sd, F_SETFL, fcntl(s->sd, F_GETFL, 0) | O_NONBLOCK);
  }
  struct sockaddr_in host_addr = {AF_INET};
  host_addr.sin_port = htons(term_to_uint16(port));
  host_addr.sin_addr = *((struct in_addr *)&addr_storage);
 again:
  if (connect(s->sd, &host_addr, sizeof(host_addr)) != 0) {
    if (errno != EINPROGRESS) {
      if (errno == EINTR) {
        goto again;
      }
      lisp_signal_system_error();
    }
    if (is_null(socket_select_write(sock, timeout))) {
      lisp_signal(g_timeout, nil);
    }
    // switch back to synchronous mode
    fcntl(s->sd, F_SETFL, fcntl(s->sd, F_GETFL, 0) & ~O_NONBLOCK);
  }
  return sock;
}

//------------------------------------------------------------------------------
// Function: (tcp-listen port &key bind_addr (reuse-address t) (backlog 128) timeout) ==> socket
//------------------------------------------------------------------------------
term tcp_listen(term port, term bind_addr, term reuse_address, term backlog,
                term timeout) {
  term sock = make_socket(AF_INET, SOCK_STREAM, 0, 1);
  socket_t * s = term_to_socket(sock);
  s->timeout = timeout;
  struct sockaddr_in local_addr = {AF_INET};
  local_addr.sin_port = htons(term_to_uint16(port));
  if (!is_null(bind_addr)) {
    struct sockaddr_storage addr_storage;
    if (my_gethostbyname(binary_get_c_str(get_binary_for_read(bind_addr)),
                         AF_INET, &addr_storage) == NULL) {
      lisp_error_sz(g_networking, "could not resolve local binding address ~s",
                    bind_addr);
    }
    local_addr.sin_addr = *((struct in_addr *)&addr_storage);
  }
  if (!is_null(reuse_address)) {
    int flag = 1;
    int rc = setsockopt(s->sd,          /* socket affected */
                        SOL_SOCKET,    /* set option at socket level */
                        SO_REUSEADDR,    /* name of option */
                        (char *) &flag, /* the cast is historical
                                           cruft */ //
                        sizeof(int)); /* length of option value */
    if (rc != 0) {
      lisp_signal_system_error();
    }
  }
  if (bind(s->sd, &local_addr, sizeof(local_addr)) != 0) {
    lisp_signal_system_error();
  }
  if (listen(s->sd, term_to_long(backlog)) != 0) {
    lisp_signal_system_error();
  }
  return sock;
}

//==============================================================================
// SOCKET STREAM
//==============================================================================
//------------------------------------------------------------------------------
// socket_stream_t
//------------------------------------------------------------------------------
typedef struct socket_stream_t socket_stream_t;

struct socket_stream_t {
  stream_t    stream;
  term        sock;
};

//------------------------------------------------------------------------------
// stream_to_socket_stream
//------------------------------------------------------------------------------
static socket_stream_t * stream_to_socket_stream(stream_t * s) {
  if (s->class_name != g_socket_stream_class_name) {
    SIGNAL_INTERNAL_ERROR("Invalid stream class name ~s", s->class_name);
  }
  return (socket_stream_t *)s;
}

//------------------------------------------------------------------------------
// Reads single byte from stream and returns it. Returns nil in case of end-of-file
//------------------------------------------------------------------------------
static term ss_read_byte(stream_t * stream) {
  const socket_stream_t * ss = stream_to_socket_stream(stream);
  const socket_t * s = term_to_socket(ss->sock);
  if (!is_null(s->timeout) &&
      is_null(socket_select_read(ss->sock, s->timeout))) {
    lisp_signal(g_timeout, nil);
  }
  uint8_t b;
  ssize_t c;
 again:
  c = recv(s->sd, &b, 1, 0);
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
static void ss_write_byte(stream_t * stream, term byte) {
  const socket_stream_t * ss = stream_to_socket_stream(stream);
  const socket_t * s = term_to_socket(ss->sock);
  uint8_t b = term_to_uint8(byte);
  if (!is_null(s->timeout) &&
      is_null(socket_select_write(ss->sock, s->timeout))) {
    lisp_signal(g_timeout, nil);
  }
  ssize_t c;
 again:
  c = send(s->sd, &b, 1, MSG_NOSIGNAL);
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
static term ss_read_binary(stream_t * stream, term count, term buf) {
  const socket_stream_t * ss = stream_to_socket_stream(stream);
  const socket_t * s = term_to_socket(ss->sock);
  binary_t * b = get_binary_for_write(buf);
  long c = term_to_long(count);
  if (c > SSIZE_MAX) {
    SIGNAL_INTERNAL_ERROR();
  }
  binary_ensure_capacity(b, c);
  if (!is_null(s->timeout) &&
      is_null(socket_select_read(ss->sock, s->timeout))) {
    lisp_signal(g_timeout, nil);
  }
 again:
  c = recv(s->sd, b->data, c, 0);
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
static term ss_write_binary(stream_t * stream, term buf, term buf_offset, term count) {
  const socket_stream_t * ss = stream_to_socket_stream(stream);
  const socket_t * s = term_to_socket(ss->sock);
  const binary_t * b = get_binary_for_read(buf);
  long offset = term_to_long(buf_offset);
  long len = term_to_long(count);
  if (len > SSIZE_MAX) {
    SIGNAL_INTERNAL_ERROR();
  }
  if (!is_null(s->timeout) &&
      is_null(socket_select_write(ss->sock, s->timeout))) {
    lisp_signal(g_timeout, nil);
  }
  ssize_t c;
 again:
  c = send(s->sd, b->data + offset, len, MSG_NOSIGNAL);
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
static void ss_flush(stream_t * stream) {
}

//------------------------------------------------------------------------------
// Sync ensures that all modified data from both user-space and operating
// system buffers are transferred to physical media. The call blocks until the
// device reports that the transfer has completed. It also flushes metadata
// information associated with the stream.
//------------------------------------------------------------------------------
static void ss_sync(stream_t * stream) {
  lisp_signal(g_unsupported_stream_operation, stream_to_socket_stream(stream)->sock);
}

//------------------------------------------------------------------------------
// datasync is similar to sync, but does not flush modified metadata unless
// that metadata is needed in order to allow a subsequent data retrieval to be
// correctly handled.
//------------------------------------------------------------------------------
static void ss_datasync(stream_t * stream) {
  lisp_signal(g_unsupported_stream_operation, stream_to_socket_stream(stream)->sock);
}

//------------------------------------------------------------------------------
// Flushes all modified data and closes stream. Stream becomes unusable.
//------------------------------------------------------------------------------
static void ss_close(stream_t * stream) {
  const socket_stream_t * ss = stream_to_socket_stream(stream);
  socket_close(ss->sock);
}

//------------------------------------------------------------------------------
// Function: (socket-stream-create socket &optional timeout name) ==> stream
//------------------------------------------------------------------------------
term make_socket_stream(term sock, term timeout, term name) {
  term res = make_stream(sizeof(socket_stream_t), g_socket_stream_class_name,
                         name, NULL);
  socket_set_timeout(sock, timeout);
  socket_stream_t * ss = stream_to_socket_stream(term_to_stream(res));
  ss->sock = sock;
  stream_t * s = &ss->stream;
  s->read_byte = ss_read_byte;
  s->write_byte = ss_write_byte;
  s->read_binary = ss_read_binary;
  s->write_binary = ss_write_binary;
  s->flush = ss_flush;
  s->sync = ss_sync;
  s->datasync = ss_datasync;
  s->close = ss_close;
  return res;
}
