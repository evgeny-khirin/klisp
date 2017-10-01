///-----------------------------------------------------------------------------
/// Copyright (c) 2009-2010 by Evgeny Khirin.
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
/// File    : httpd.c
/// Author  : Evgeny Khirin <>
/// Description : HTTP support functions.
///-----------------------------------------------------------------------------

#include "klisp.h"

//------------------------------------------------------------------------------
// http status reasons
//------------------------------------------------------------------------------
static term g_reason_100 = nil;
static term g_reason_101 = nil;
static term g_reason_200 = nil;
static term g_reason_201 = nil;
static term g_reason_202 = nil;
static term g_reason_203 = nil;
static term g_reason_204 = nil;
static term g_reason_205 = nil;
static term g_reason_206 = nil;
static term g_reason_300 = nil;
static term g_reason_301 = nil;
static term g_reason_302 = nil;
static term g_reason_303 = nil;
static term g_reason_304 = nil;
static term g_reason_305 = nil;
static term g_reason_307 = nil;
static term g_reason_400 = nil;
static term g_reason_401 = nil;
static term g_reason_402 = nil;
static term g_reason_403 = nil;
static term g_reason_404 = nil;
static term g_reason_405 = nil;
static term g_reason_406 = nil;
static term g_reason_407 = nil;
static term g_reason_408 = nil;
static term g_reason_409 = nil;
static term g_reason_410 = nil;
static term g_reason_411 = nil;
static term g_reason_412 = nil;
static term g_reason_413 = nil;
static term g_reason_414 = nil;
static term g_reason_415 = nil;
static term g_reason_416 = nil;
static term g_reason_417 = nil;
static term g_reason_500 = nil;
static term g_reason_501 = nil;
static term g_reason_502 = nil;
static term g_reason_503 = nil;
static term g_reason_504 = nil;
static term g_reason_505 = nil;
static term g_reason_unknown = nil;

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term get_status_reason(term status) {
  switch (term_to_long(status)) {
  case 100: return g_reason_100;
  case 101: return g_reason_101;
  case 200: return g_reason_200;
  case 201: return g_reason_201;
  case 202: return g_reason_202;
  case 203: return g_reason_203;
  case 204: return g_reason_204;
  case 205: return g_reason_205;
  case 206: return g_reason_206;
  case 300: return g_reason_300;
  case 301: return g_reason_301;
  case 302: return g_reason_302;
  case 303: return g_reason_303;
  case 304: return g_reason_304;
  case 305: return g_reason_305;
  case 307: return g_reason_307;
  case 400: return g_reason_400;
  case 401: return g_reason_401;
  case 402: return g_reason_402;
  case 403: return g_reason_403;
  case 404: return g_reason_404;
  case 405: return g_reason_405;
  case 406: return g_reason_406;
  case 407: return g_reason_407;
  case 408: return g_reason_408;
  case 409: return g_reason_409;
  case 410: return g_reason_410;
  case 411: return g_reason_411;
  case 412: return g_reason_412;
  case 413: return g_reason_413;
  case 414: return g_reason_414;
  case 415: return g_reason_415;
  case 416: return g_reason_416;
  case 417: return g_reason_417;
  case 500: return g_reason_500;
  case 501: return g_reason_501;
  case 502: return g_reason_502;
  case 503: return g_reason_503;
  case 504: return g_reason_504;
  case 505: return g_reason_505;
  default: return g_reason_unknown;
  }
}

//------------------------------------------------------------------------------
// Short days of weeks
//------------------------------------------------------------------------------
static const char * g_short_days_of_week[7] =
  {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"};

//------------------------------------------------------------------------------
// Long days of weeks
//------------------------------------------------------------------------------
static const char * g_long_days_of_week[7] =
  {"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"};

static int g_days_of_week_len[7] = {6, 7, 9, 8, 6, 8, 6};

//------------------------------------------------------------------------------
// Months
//------------------------------------------------------------------------------
static const char * g_months[12] =
  {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
   "Nov", "Dec"};

//------------------------------------------------------------------------------
// skips white space
//------------------------------------------------------------------------------
static const char * skip_ws(const char * s) {
  while (isspace(*s)) {
    ++s;
  }
  return s;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static void signal_invalid_http_date(term date) {
  lisp_error_sz(g_invalid_arg, "invalid HTTP date: ~s", date);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static const char * parse_uint(const char * s, int * res) {
  s = skip_ws(s);
  int has_digits = 0;
  while (*s == '0') {
    has_digits = 1;
    s++;
  }
  int r = 0;
  while (isdigit(*s)) {
    has_digits = 1;
    r = r * 10 + (*s++) - '0';
  }
  if (!has_digits) {
    return NULL;
  }
  *res = r;
  return s;
}

//------------------------------------------------------------------------------
// parse HTTP time
// Format: " 08:49:37 "
//------------------------------------------------------------------------------
static const char * parse_http_time(term date, const char * s, struct tm * tm) {
  // parse hour
  s = parse_uint(s, &tm->tm_hour);
  if (s == NULL || *s != ':') {
    signal_invalid_http_date(date);
  }
  // parse minute
  s = parse_uint(s + 1, &tm->tm_min);
  if (s == NULL || *s != ':') {
    signal_invalid_http_date(date);
  }
  // parse second
  s = parse_uint(s + 1, &tm->tm_sec);
  if (s == NULL || !isspace(*s)) {
    signal_invalid_http_date(date);
  }
  return s;
}

//------------------------------------------------------------------------------
// parse rfc-822 date
// Format: "Thu, 06 Nov 2014 08:49:37 GMT"
//------------------------------------------------------------------------------
static void parse_rfc_822_date(term date, const char * s, struct tm * tm) {
  // parse name of day of week - value is ignored
  int day_of_week;
  for (day_of_week = 0; day_of_week < 7; day_of_week++) {
    if (strncasecmp(s, g_short_days_of_week[day_of_week], 3) == 0) {
      break;
    }
  }
  if (day_of_week == 7) {
    signal_invalid_http_date(date);
  }
  // parse day of month
  s = parse_uint(s + 4, &tm->tm_mday);
  if (s == NULL || !isspace(*s)) {
    signal_invalid_http_date(date);
  }
  // parse month
  s = skip_ws(s);
  for (tm->tm_mon = 0; tm->tm_mon < 12; tm->tm_mon++) {
    if (strncasecmp(s, g_months[tm->tm_mon], 3) == 0) {
      break;
    }
  }
  if (tm->tm_mon == 12) {
    signal_invalid_http_date(date);
  }
  s += 3;
  if (!isspace(*s)) {
    signal_invalid_http_date(date);
  }
  // parse year
  s = parse_uint(s, &tm->tm_year);
  if (s == NULL || !isspace(*s)) {
    signal_invalid_http_date(date);
  }
  tm->tm_year -= 1900;
  // parse time
  s = parse_http_time(date, s, tm);
  // check timezone and check end of line
  s = skip_ws(s);
  if (toupper(s[0]) != 'G' || toupper(s[1]) != 'M' || toupper(s[2]) != 'T') {
    signal_invalid_http_date(date);
  }
  s = skip_ws(s + 3);
  if (*s != 0) {
    signal_invalid_http_date(date);
  }
}

//------------------------------------------------------------------------------
// parse ANSI date
// Format: "Thu Nov  6 08:49:37 2014"
//------------------------------------------------------------------------------
static void parse_ansi_date(term date, const char * s, struct tm * tm) {
  // parse name of day of week - value is ignored
  int day_of_week;
  for (day_of_week = 0; day_of_week < 7; day_of_week++) {
    if (strncasecmp(s, g_short_days_of_week[day_of_week], 3) == 0) {
      break;
    }
  }
  if (day_of_week == 7) {
    signal_invalid_http_date(date);
  }
  // parse month
  s = skip_ws(s + 3);
  for (tm->tm_mon = 0; tm->tm_mon < 12; tm->tm_mon++) {
    if (strncasecmp(s, g_months[tm->tm_mon], 3) == 0) {
      break;
    }
  }
  if (tm->tm_mon == 12) {
    signal_invalid_http_date(date);
  }
  s += 3;
  if (!isspace(*s)) {
    signal_invalid_http_date(date);
  }
  // parse day of month
  s = parse_uint(s, &tm->tm_mday);
  if (s == NULL || !isspace(*s)) {
    signal_invalid_http_date(date);
  }
  // parse time
  s = parse_http_time(date, s, tm);
  // parse year
  s = parse_uint(s, &tm->tm_year);
  if (s == NULL) {
    signal_invalid_http_date(date);
  }
  tm->tm_year -= 1900;
  // check end of line
  s = skip_ws(s);
  if (*s != 0) {
    signal_invalid_http_date(date);
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static int current_centure() {
  time_t t = time(NULL);
  struct tm tm;
  gmtime_r(&t, &tm);
  int year = 1900 + tm.tm_year;
  return year - year % 100;
}

//------------------------------------------------------------------------------
// parse rfc-850 date
// Format: "Thursday, 06-Nov-14 08:49:37 GMT"
//------------------------------------------------------------------------------
static void parse_rfc_850_date(term date, const char * s, struct tm * tm) {
  // parse name of day of week - value is ignored
  int day_of_week;
  for (day_of_week = 0; day_of_week < 7; day_of_week++) {
    if (strncasecmp(s, g_short_days_of_week[day_of_week], 3) == 0) {
      break;
    }
  }
  if (day_of_week == 7) {
    signal_invalid_http_date(date);
  }
  if (strncasecmp(s, g_long_days_of_week[day_of_week], g_days_of_week_len[day_of_week]) != 0) {
    signal_invalid_http_date(date);
  }
  s += g_days_of_week_len[day_of_week];
  if (*s != ',') {
    signal_invalid_http_date(date);
  }
  // parse day of month
  s = parse_uint(s + 1, &tm->tm_mday);
  if (s == NULL || *s != '-') {
    signal_invalid_http_date(date);
  }
  // parse month
  s = skip_ws(s + 1);
  for (tm->tm_mon = 0; tm->tm_mon < 12; tm->tm_mon++) {
    if (strncasecmp(s, g_months[tm->tm_mon], 3) == 0) {
      break;
    }
  }
  if (tm->tm_mon == 12) {
    signal_invalid_http_date(date);
  }
  s += 3;
  if (*s != '-') {
    signal_invalid_http_date(date);
  }
  // parse year
  s = parse_uint(s + 1, &tm->tm_year);
  if (s == NULL) {
    signal_invalid_http_date(date);
  }
  tm->tm_year = current_centure() + tm->tm_year - 1900;
  // parse time
  s = parse_http_time(date, s, tm);
  // check timezone and check end of line
  s = skip_ws(s);
  if (toupper(s[0]) != 'G' || toupper(s[1]) != 'M' || toupper(s[2]) != 'T') {
    signal_invalid_http_date(date);
  }
  s = skip_ws(s + 3);
  if (*s != 0) {
    signal_invalid_http_date(date);
  }
}

//------------------------------------------------------------------------------
// (parse-date date) ==> universal-time
//------------------------------------------------------------------------------
static term http_parse_date(term date) {
  struct tm tm = {0};
  const char * s = skip_ws(binary_get_c_str(get_binary_for_read(date)));
  switch (s[3]) {
  case ',':
    parse_rfc_822_date(date, s, &tm);
    break;
  case ' ':
  case '\t':
    parse_ansi_date(date, s, &tm);
    break;
  default:
    parse_rfc_850_date(date, s, &tm);
  }
  time_t res = mktime(&tm);
  if (res == (time_t)-1) {
    lisp_signal_system_error();
  }
  return long_to_term(res - timezone);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static const char * read_word(const char * s, const char * * pstart) {
  *pstart = s = skip_ws(s);
  while (!isspace(*s) && *s != 0) {
    ++s;
  }
  return s;
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static uint8_t xchar_to_digit(uint8_t c) {
  if (c <= '9') {
    return c - '0';
  } else {
    return 10 + toupper(c) - 'A';
  }
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
static term decode_url(const char * url_start, const char * url_end) {
  long src_len = url_end - url_start;
  term url = make_binary();
  binary_t * u = get_binary_for_write(url);
  binary_ensure_capacity(u, src_len);
  uint8_t * s = (uint8_t *)url_start;
  uint8_t * d = u->data;
  long i;
  for (i = 0; i < src_len; ++i) {
    if (*s == '%') {
      ++s;
      src_len -= 2;
      if (i >= src_len || !isxdigit(s[0]) || !isxdigit(s[1])) {
        lisp_signal(g_invalid_url_encoding,
                    make_binary_from_binary((uint8_t *)url_start, url_end - url_start));
      }
      *d++ = 16 * xchar_to_digit(s[0]) + xchar_to_digit(s[1]);
      s += 2;
    } else {
      *d++ = *s++;
    }
    u->size += 1;
  }
  return url;
}

//------------------------------------------------------------------------------
// (http::parse-request-line line) ==> (values method url version)
//------------------------------------------------------------------------------
static term             g_get = nil;
static const binary_t * g_get_binary = NULL;
static term             g_head = nil;
static const binary_t * g_head_binary = NULL;
static term             g_post = nil;
static const binary_t * g_post_binary = NULL;
static term             g_connect = nil;
static const binary_t * g_connect_binary = NULL;
static term             g_http_0_9 = nil;
static term             g_http_1_0 = nil;
static term             g_http_1_1 = nil;

static term http_parse_request_line(term line) {
  const char * method_start, * method_end;
  method_end = read_word(binary_get_c_str(get_binary_for_read(line)), &method_start);
  if (method_start == method_end) {
    lisp_signal(g_invalid_request_line, line);
  }
  term method;
  binary_t m;
  binary_init(&m);
  m.data = (uint8_t *) method_start;
  m.size = method_end - method_start;
  if (binary_icmp(&m, g_get_binary) == 0) {
    method = g_get;
  } else if (binary_icmp(&m, g_post_binary) == 0) {
    method = g_post;
  } else if (binary_icmp(&m, g_head_binary) == 0) {
    method = g_head;
  } else if (binary_icmp(&m, g_connect_binary) == 0) {
    method = g_connect;
  } else {
    method = make_binary_from_binary((uint8_t *)method_start, method_end - method_start);
  }
  const char * url_start, * url_end;
  url_end = read_word(method_end, &url_start);
  if (url_start == url_end) {
    lisp_signal(g_invalid_request_line, line);
  }
  term url = decode_url(url_start, url_end);
  const char * version_start, * version_end;
  version_end = read_word(url_end, &version_start);
  term version;
  if (version_start == version_end) {
    if (binary_icmp(get_binary_for_read(method), g_get_binary) != 0) {
      lisp_signal(g_invalid_request_line, line);
    }
    version = g_http_0_9;
  } else if (version_end - version_start != 8 ||
             strncasecmp(version_start, "HTTP/1.", 7) != 0) {
    lisp_signal(g_invalid_request_line, line);
  } else {
    switch (*(version_end - 1)) {
    case '0':
      version = g_http_1_0;
      break;
    case '1':
      version = g_http_1_1;
      break;
    default:
      lisp_signal(g_invalid_request_line, line);
    }
  }
  if (*skip_ws(version_end) != 0) {
    lisp_signal(g_invalid_request_line, line);
  }
  return VALUES(method, url, version);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
const char * extract_header_name(const char * s, binary_t * name) {
  s = skip_ws(s);
  char c;
  while ((c = *s++)) {
    if (c == ':') {
      return s;
    }
    if (isspace(c)) {
      s = skip_ws(s);
      if (*s != ':') {
        lisp_signal(g_invalid_http_headers, nil);
      }
      return s + 1;
    }
    binary_append_uint8(name, c);
  }
  lisp_signal(g_invalid_http_headers, nil);
}

//------------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------------
void extract_header_value(const char * s, binary_t * value, long len) {
  const char * s1 = skip_ws(s);
  binary_append_binary(value, (uint8_t *)s1, len - (s1 - s));
  long i;
  for (i = value->size - 1; i >= 0; --i) {
    if (!isspace(value->data[i])) {
      break;
    }
  }
  binary_truncate(value, i + 1);
}

//------------------------------------------------------------------------------
// (http::parse-headers in max-line-length max-lines) ==> hashmap
//------------------------------------------------------------------------------
static term g_binary_iequal = nil;
static term g_binary_ihash_code = nil;

static term http_parse_headers(term in, term  max_line_length, term max_lines) {
  term headers = hashmap_create(g_binary_iequal, g_binary_ihash_code);
  uint8_t __storage_for_lb[sizeof(binary_t) + 8];
  binary_t * lb = (binary_t *)ALIGNUP((uintptr_t)__storage_for_lb, 8);
  binary_init(lb);
  term line = __pointer_to_term(lb);
  long count = 0;
  long max_count = term_to_long(max_lines);
  while (!is_null(read_line(in, max_line_length, line)) && lb->size != 0) {
    const char * s = binary_get_c_str(lb);
    if ((lb->size == 1 && *s == '\n') ||
        (lb->size == 2 && s[0] == '\r' && s[1] == '\n')) {
      break;
    }
    if (++count > max_count) {
      lisp_signal(g_http_headers_too_large, nil);
    }
    term name = make_binary();
    s = extract_header_name(s, get_binary_for_write(name));
    term value = make_binary();
    binary_t * v = get_binary_for_write(value);
    extract_header_value(s, v, lb->size - (s - (char *)lb->data));
    term c = read_byte(in);
    while (c == __long_to_fixnum_term(' ') || c == __long_to_fixnum_term('\t')) {
      binary_truncate(lb, 0);
      if (is_null(read_line(in, max_line_length, line))) {
        return headers;
      }
      if (++count > max_count) {
        lisp_signal(g_http_headers_too_large, nil);
      }
      const char * s = binary_get_c_str(lb);
      binary_append_uint8(v, ' ');
      extract_header_value(s, v, lb->size);
      c = read_byte(in);
    }
    unread_byte(c, in);
    binary_truncate(lb, 0);
    term old_value = hashmap_lookup(headers, name, nil);
    hashmap_insert(headers, name, cons(value, old_value), nil);
  }
  return headers;
}

//------------------------------------------------------------------------------
// Internal function
//------------------------------------------------------------------------------
void __http_init() {
  make_global_c_lambda("(http:parse-date date)", http_parse_date, 1);
  make_global_c_lambda("(http::status-code-to-reason status-code)",
                       get_status_reason, 1);
  make_global_c_lambda("(http::parse-request-line line)", http_parse_request_line, 1);
  make_global_c_lambda("(http::parse-headers in max-line-length max-lines)",
                       http_parse_headers, 1);

  // create reason phrases
  g_reason_100 = __deep_immune_literals(make_binary_from_sz("Continue"));
  g_reason_101 = __deep_immune_literals(make_binary_from_sz("Switching Protocols"));
  g_reason_200 = __deep_immune_literals(make_binary_from_sz("OK"));
  g_reason_201 = __deep_immune_literals(make_binary_from_sz("Created"));
  g_reason_202 = __deep_immune_literals(make_binary_from_sz("Accepted"));
  g_reason_203 = __deep_immune_literals(make_binary_from_sz("Non-Authoritative Information"));
  g_reason_204 = __deep_immune_literals(make_binary_from_sz("No Content"));
  g_reason_205 = __deep_immune_literals(make_binary_from_sz("Reset Content"));
  g_reason_206 = __deep_immune_literals(make_binary_from_sz("Partial Content"));
  g_reason_300 = __deep_immune_literals(make_binary_from_sz("Multiple Choices"));
  g_reason_301 = __deep_immune_literals(make_binary_from_sz("Moved Permanently"));
  g_reason_302 = __deep_immune_literals(make_binary_from_sz("Found"));
  g_reason_303 = __deep_immune_literals(make_binary_from_sz("See Other"));
  g_reason_304 = __deep_immune_literals(make_binary_from_sz("Not Modified"));
  g_reason_305 = __deep_immune_literals(make_binary_from_sz("Use Proxy"));
  g_reason_307 = __deep_immune_literals(make_binary_from_sz("Temporary Redirect"));
  g_reason_400 = __deep_immune_literals(make_binary_from_sz("Bad Request"));
  g_reason_401 = __deep_immune_literals(make_binary_from_sz("Unauthorized"));
  g_reason_402 = __deep_immune_literals(make_binary_from_sz("Payment Required"));
  g_reason_403 = __deep_immune_literals(make_binary_from_sz("Forbidden"));
  g_reason_404 = __deep_immune_literals(make_binary_from_sz("Not Found"));
  g_reason_405 = __deep_immune_literals(make_binary_from_sz("Method Not Allowed"));
  g_reason_406 = __deep_immune_literals(make_binary_from_sz("Not Acceptable"));
  g_reason_407 = __deep_immune_literals(make_binary_from_sz("Proxy Authentication Required"));
  g_reason_408 = __deep_immune_literals(make_binary_from_sz("Request Time-out"));
  g_reason_409 = __deep_immune_literals(make_binary_from_sz("Conflict"));
  g_reason_410 = __deep_immune_literals(make_binary_from_sz("Gone"));
  g_reason_411 = __deep_immune_literals(make_binary_from_sz("Length Required"));
  g_reason_412 = __deep_immune_literals(make_binary_from_sz("Precondition Failed"));
  g_reason_413 = __deep_immune_literals(make_binary_from_sz("Request Entity Too Large"));
  g_reason_414 = __deep_immune_literals(make_binary_from_sz("Request-URI Too Large"));
  g_reason_415 = __deep_immune_literals(make_binary_from_sz("Unsupported Media Type"));
  g_reason_416 = __deep_immune_literals(make_binary_from_sz("Requested range not satisfiable"));
  g_reason_417 = __deep_immune_literals(make_binary_from_sz("Expectation Failed"));
  g_reason_500 = __deep_immune_literals(make_binary_from_sz("Internal Server Error"));
  g_reason_501 = __deep_immune_literals(make_binary_from_sz("Not Implemented"));
  g_reason_502 = __deep_immune_literals(make_binary_from_sz("Bad Gateway"));
  g_reason_503 = __deep_immune_literals(make_binary_from_sz("Service Unavailable"));
  g_reason_504 = __deep_immune_literals(make_binary_from_sz("Gateway Time-out"));
  g_reason_505 = __deep_immune_literals(make_binary_from_sz("HTTP Version not supported"));
  g_reason_unknown = __deep_immune_literals(make_binary_from_sz("Unknown status"));

  // init data for http_parse_request_line
  g_get = __deep_immune_literals(make_binary_from_sz("GET"));
  g_get_binary = get_binary_for_read(g_get);
  g_head = __deep_immune_literals(make_binary_from_sz("HEAD"));
  g_head_binary = get_binary_for_read(g_head);
  g_post = __deep_immune_literals(make_binary_from_sz("POST"));
  g_post_binary = get_binary_for_read(g_post);
  g_connect = __deep_immune_literals(make_binary_from_sz("CONNECT"));
  g_connect_binary = get_binary_for_read(g_connect);
  g_http_0_9 = double_to_term(0.9);
  g_http_1_0 = double_to_term(1.0);
  g_http_1_1 = double_to_term(1.1);

  // init data for http_parse_headers
  g_binary_iequal = resolve_symbol_from_sz("kl:binary-iequal");
  g_binary_ihash_code = resolve_symbol_from_sz("kl:binary-ihash-code");
}

