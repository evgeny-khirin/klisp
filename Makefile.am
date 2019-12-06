CFLAGS=-D_LARGEFILE64_SOURCE -D_GNU_SOURCE -D __STDC_LIMIT_MACROS -D __STDC_CONSTANT_MACROS \
	-Wall -Werror

CXXFLAGS=-D_LARGEFILE64_SOURCE -D_GNU_SOURCE -D __STDC_LIMIT_MACROS -D __STDC_CONSTANT_MACROS \
	-Wall -Werror

lisp/eval.c: lisp/eval_x86_32_linux.c.h lisp/eval_x86_64_linux.c.h

lisp/eval_x86_32_linux.c.h: lisp/eval_x86_32_linux.c.h.dasc
	lua ./include/dynasm/dynasm.lua -o $@ $<

lisp/eval_x86_64_linux.c.h: lisp/eval_x86_64_linux.c.h.dasc
	lua ./include/dynasm/dynasm.lua -o $@ $<

bin_PROGRAMS = klisp.release klisp.debug

klisp_release_CFLAGS=$(CFLAGS) -O3 -fno-strict-aliasing
klisp_release_CXXFLAGS=$(CXXFLAGS) -O3 -fno-strict-aliasing

klisp_release_SOURCES = \
	lisp/avlmap.c \
	lisp/binary.c \
	lisp/binary_stream.c \
	lisp/custom_stream.c \
	lisp/eval.c \
	lisp/file_stream.c \
	lisp/gc.c \
	lisp/hashmap.cpp \
	lisp/http.c \
	lisp/init.c \
	lisp/klisp.c \
	lisp/native.c\
	lisp/package.cpp \
	lisp/print.c \
	lisp/read.c \
	lisp/regex.c \
	lisp/sockets.c \
	lisp/stream.c \
	lisp/symbol.c \
	lisp/term.c \
	lisp/thread.c \
	lisp/tqueue.c \
	lisp/treemap.cpp \
	lisp/vector.cpp

klisp_debug_CFLAGS=$(CFLAGS) -O0 -g
klisp_debug_CXXFLAGS=$(CXXFLAGS) -O0 -g

klisp_debug_SOURCES = $(klisp_release_SOURCES)
