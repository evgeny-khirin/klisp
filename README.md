# K-Lisp

* K-Lisp is compiler and not interpreter. Programs in K-Lisp are dynamically
  compiled directly into native machine instructions. Like other Lisps, it
  supports dynamic re-definition and re-compilation of any system parts at
  runtime without stopping the system. That gives incredible possibilities for
  support and upgrade of non-stop systems.

* K-Lisp has tight integration with C. K-Lisp provides infrastructure to extend
  Lisp with C functions with minimum overflow for performance reasons or
  integration with third party C libraries. K-Lisp also provides both funcall
  function and FUNCALL macro for easy call of Lisp functions from C. Generally
  speaking, K-Lisp may be considered as metaprogramming extension of C language
  with dynamic typing and compilation. And C may be considered as low level
  extension of K-Lisp for performance purposes. K-Lisp supports loading of
  dynamically linked libraries with C-extensions at runtime.

* K-Lisp exports garbage collector (GC) API to C programs. And such significatly
  simplifies memory management in complex programs. GC is conservative.

* K-Lisp has separate namespaces for functions and variables.

* K-Lisp supports both lexical and dynamic variables binding. It has separate
  syntax for lexical and dynamic binding. All global variables are dynamically
  bound, while function parameters and local variables are lexically bound.
  
* More dateiled specs are in docs/specs.txt.

# Building

* OS supported: Linux 32 and 64 bits.

* Prerequirements. You need Lua with bitops library. You need Capstone dissassembling
  library, resides under 3rd_party/ durectory.

* Since it does not have make file, you'll need Erlang to build the compiler. There is
  build.erl file in root directory. It is my replacement of make, written in Erlang.
  You need to start Erlang in the root directory:
  
      evgeny@wheezy:~/work/klisp$ erl
      Erlang/OTP 17 [erts-6.2] [source] [async-threads:10] [kernel-poll:false]

      Eshell V6.2  (abort with ^G)
      1> c("build.erl").
      {ok,build}
      2> build:compile().
      
After that you'll have K-Lisp build in lisp/tests directory:

      evgeny@wheezy:~/work/klisp$ ./lisp/tests/klisp.release
      kl> (load "lisp/boot.lisp")

      :ok
      kl> "Hello, world!"

      "Hello, world!"
      kl>
