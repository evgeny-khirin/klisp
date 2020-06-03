#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([klisp], [0.1])
AC_CONFIG_SRCDIR([lisp/init.c])
AC_CONFIG_HEADERS([include/config.h])

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([no-define subdir-objects foreign])

# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB

# Checks LUA, ensure that under Ubuntu "autoconf-archive" package is installed
AX_PROG_LUA([5.1], [], [], [AC_MSG_ERROR([LUA 5.1+ not found])])

# Checks for libraries.
AC_CHECK_LIB([unistring], [u32_check], [],  [AC_MSG_ERROR([unistring library not found])])
AC_CHECK_LIB([pthread], [pthread_mutex_lock], [],  [AC_MSG_ERROR([pthread library not found])])
AC_CHECK_LIB([gmp], [__gmpz_init], [],  [AC_MSG_ERROR([gmp library not found])])
AC_CHECK_LIB([capstone], [cs_disasm], [],  [AC_MSG_ERROR([capstone3 library not found])])

# Checks for header files.
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([arpa/inet.h fcntl.h float.h inttypes.h limits.h locale.h netdb.h netinet/in.h stddef.h stdint.h stdlib.h string.h strings.h sys/socket.h sys/time.h unistd.h wchar.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_FSEEKO
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_MMAP
AC_FUNC_REALLOC
AC_FUNC_STRCOLL
AC_FUNC_STRERROR_R
AC_FUNC_STRTOD
AC_CHECK_FUNCS([bzero clock_gettime fdatasync floor getcwd getpagesize localtime_r memchr memmove memset munmap pow regcomp setenv setlocale socket sqrt strcasecmp strchr strcspn strerror strncasecmp strpbrk strrchr strstr strtoul tzset uname])

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
