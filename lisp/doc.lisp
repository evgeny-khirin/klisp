;;-------------------------------------------------------------------
;; Copyright (c) 2009-2012 by Evgeny Khirin.
;;-------------------------------------------------------------------
;;-------------------------------------------------------------------
;; File    : doc.lisp
;; Author  : Evgeny Khirin <>
;; Description : Initializes Lisp documentation for native functions and native
;; global variables.
;;-------------------------------------------------------------------

;;--------------------------------------------------------------------------
;; in-package
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'in-package
  "Macro: (in-package name) ==> package

Parameters: name - symbol

Causes the the package named by 'name' to become the current package - that is,
the value of '*package*' global variable. If no such package already exists,
then package is created automatically.")

;;--------------------------------------------------------------------------
;; *package*
;;--------------------------------------------------------------------------
(set-symbol-value-doc '*package*
  "Variable: *package*
Current package.")

;;--------------------------------------------------------------------------
;; Document global standard I/O streams
;;--------------------------------------------------------------------------
(set-symbol-value-doc '*stdin*
  "Variable: *stdin*
Defines standard input stream.")

(set-symbol-value-doc '*stdout*
  "Variable: *stdout*
Defines standard output stream.")

(set-symbol-value-doc '*stderr*
  "Variable: *stderr*
Defines standard error output stream.")

;;--------------------------------------------------------------------------
;; Function: (not x) ==> bool
;; Function is native, just document it.
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'not
  "Function: (not x) ==> bool

Parameters: x - object

Performs boolen not operation for object x: if x is nil, then t is returned,
otherwise nil is returned.")

;;--------------------------------------------------------------------------
;; quote
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'quote
  "The special form quote returns its single argument, as written, without
evaluating it. This provides a way to include constant symbols and lists, which
are not self-evaluating objects, in a program. (It is not necessary to quote
self-evaluating objects such as numbers, strings, and vectors.)

- Special Form: quote object

This special form returns object, without evaluating it.

Because quote is used so often in programs, K-Lisp provides a convenient read
syntax for it. An apostrophe character (') followed by a K-Lisp object (in read
syntax) expands to a list whose first element is quote, and whose second element
is the object. Thus, the read syntax 'x is an abbreviation for (quote x).")

;;--------------------------------------------------------------------------
;; if
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'if
  "Special Form: if condition then-form [else-form]

Returns result of 'then-form' or 'else-form', depending on value of
'condition'.

If value of 'condition' is non-nil, then 'then-form' is evaluated and the result
is returned. Otherwise, the 'else-form' is evaluated and the result returned. if
'else-form' is omitted and value of 'condition' is false, then nil is returned.")

;;--------------------------------------------------------------------------
;; lambda
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'lambda
  "Special Form: lambda lambda-list expression ...

Returns new functional object. Lambda list and body automatically marked as
immutable, preventing some weird bugs and code injection.

The environment in effect when the lambda form is evaluated is remembered as
part of the procedure; it is called the closing environment. When the procedure
is later called with some arguments, the closing environment is extended by
binding the variables in the formal parameter list to fresh locations, and the
locations are filled with the arguments according to rules about to be
given. The new environment created by this process is referred to as the
invocation environment.

Once the invocation environment has been constructed, the expressions in the
body of the lambda expression are evaluated sequentially in it. This means that
the region of the variables bound by the lambda expression is all of the
expressions in the body. The result of evaluating the last expression in the
body is returned as the result of the procedure call.

Lambda special form must contain at least one expression.")

;;--------------------------------------------------------------------------
;; macro
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'macro
  "Special Form: macro lambda-list expression ...

Returns new macro object. Lambda list and body automatically marked as
immutable, preventing some weird bugs and code injection.

Syntax of macro form is same as lambda form, with first symbol 'macro'.")

;;--------------------------------------------------------------------------
;; setq
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'setq
  "Special form: (setq {var form}+) ==> object

Parameters: var - symbol
            form - form.

Returns value of last evaluated form.

Binds variable 'var' to value of 'expression'. Variable is looked starting from
current lexical environment. If variable is not found in lexical environment,
then dynamic environment of current thread is searched. And then global
environment is searched. If symbol is not found in any environment, then error
is signaled.  Assigment is done sequentially, ie. if variable mentioned in
'setq' is used in latter forms, then new value of the variable will be used.")

;;--------------------------------------------------------------------------
;; progn
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'progn
  "Special form: (progn exp1 exp2 ... expN) ==> object
  expressions is sequence of any K-Lisp expressions.

Returns result of last expression.

Sequencing special form: evaluates expressions in sequence as they appear and
returns result of last expression.")

;;--------------------------------------------------------------------------
;; dynamic-let
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'dynamic-let
  "Special form: (dynamic-let ({(var value)}*) body) ==> object

Syntax: var - symbol.
        value - form
        body - form+.

Returns result of last expression.

Performs dynamic binding of global variables. Binding performed in parallel: all
value forms are evaluated in order before any variable is bound. Body forms are
evaluated in implicit 'progn' and modified global environment. Original values
of global variables are automatically restored upon return from dynamic-let form
or stack unwinding. Child thread does not share dynamic bindings of it's parent.")

;;--------------------------------------------------------------------------
;; let
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'let
  "Special form: (let (init*) body) ==> object

Syntax: init - var or (var value)
        var - symbol.
        value - form
        body - form+.

Returns result of last expression.

Performs lexical binding of variables. Binding performed in parallel: all value
forms are evaluated in order before any variable is bound. Body forms are
evaluated in implicit 'progn' and modified lexical environment.")

;;--------------------------------------------------------------------------
;; catch
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'catch
  "Special form: (catch tag exp*)

Syntax: exp - form
        tag - form

'Catch' is used as the destination of a non-local control transfer by 'throw'
function. Tags are used to find the 'catch', to which 'throw' is transferring
control. (catch 'foo form) catches a (throw 'foo form) but not a (throw 'bar
form).

The order of execution of 'catch' follows:
  1. Tag is evaluated. It serves as the name of the 'catch'.
  2. Forms are then evaluated as an implicit 'progn', and the results of the last
     form are returned unless a 'throw' occurs.
  3. If 'throw' occurs during the execution of one of the forms, control is
     transferred to the 'catch' form whose tag is 'eq' to the tag argument of the
     'throw' and which is the most recently established 'catch' with that tag. No
     further evaluation of forms occurs.
  4. The tag established by 'catch' is disestablished just before the results are
     returned.

If during the execution of one of the forms, 'throw' is executed whose tag is 'eq'
to the 'catch' tag, then the values specified by the 'throw' are returned as the
result of the dynamically most recently established 'catch' form with that tag.
The mechanism for 'catch' and 'throw' works even if 'throw' is not within the lexical
scope of 'catch'. 'throw' must occur within the dynamic extent of the evaluation of
the body of a 'catch' with a corresponding tag.")

;;--------------------------------------------------------------------------
;; throw
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'throw
  "Special form: (throw tag &optional value)

Parameters: tag - form.
            value - form.

The purpose of 'throw' is to return to a return point previously established
with 'catch'. The argument tag is evaluated and used to choose among the various
existing return points; it must be 'eq' to the value specified in the
'catch'. If multiple return points match tag, the innermost one is used. If
value form is evaluated to multiple values, then all values are passed to return
point.

If no return point is in effect with tag tag, then a no_catch error is signaled
with data (tag . value).")

;;--------------------------------------------------------------------------
;; handler-bind
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'handler-bind
  "Special form: (handler-bind handler body+)

Parameters: handler - function of two arguments (lambda (label value)). The
              handler will be invoked by signal function, in case of error
              signaling. Return value of handler is ignored by signal function.
            body - forms

Evaluates body in implicit progn with installed signal handler. Returns value of
last evaluated form.")

;;--------------------------------------------------------------------------
;; signal
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'signal
  "Function: (signal label &optional value)

Parameters: label - symbol.
            value - object.

The function signals an error named by label. The value is additional data
relevant to the circumstances of the error. The function calls signal handlers
installed with handler-bind function. The handlers are called in reverse order -
most recently installed handler is called first etc. Since handlers are called
before stack unwinding, they may take actions to fix the error and pass control
back with 'throw' special form. In that case signal hander function does not
return and signal is handled. If signal is not handled, then function from
'*unhandled-signal-hook*' is called. If hook is not installed or is returned,
then 'signal' function terminates current thread.")

;;--------------------------------------------------------------------------
;; flet
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'flet
  "Special form: (flet ({(function-name lambda-list function-body)}*) flet-body) ==> object

Syntax: function-name - symbol.
        lambda-list - lambda list.
        function-body - form+.
        flet-body - form+.

Returns result of last expression in flet-body.

Performs lexical binding of functions. Binding performed in parallel: all
definitions are evaluated in order before any function is bound. Flet body forms
are evaluated in implicit 'progn' and modified lexical environment.")

;;--------------------------------------------------------------------------
;; labels
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'labels
  "Special form: (labels ({(function-name lambda-list function-body)}*) labels-body) ==> object

Syntax: function-name - symbol.
        lambda-list - lambda list.
        function-body - form+.
        labels-body - form+.

Returns result of last expression in labels-body.

Labels is equivalent to flet except that the scope of the defined function names
for labels encompasses the function definitions themselves as well as the
body. Definitions are evaluated in order.")

;;--------------------------------------------------------------------------
;; macrolet
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'macrolet
  "Special form: (macrolet ({(macro-name lambda-list macro-body)}*) macrolet-body) ==> object

Syntax: macro-name - symbol.
        lambda-list - lambda list.
        macro-body - form+.
        macrolet-body - form+.

Returns result of last expression in macrolet-body.

Macrolet is equivalent to flet except that it defines local macros. Definitions
are evaluated in order. Local macros can not be closures, because closure is
created during runtime, but macro must be expanded during compilation time.")

;;--------------------------------------------------------------------------
;; function
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'function
  "Special form: (function name) ==> lambda or macro

The value of function is the functional value of name in the current lexical
environment.

If name is a function name, the functional definition of that name is that
established by the innermost lexically enclosing flet, labels, or macrolet form,
if there is one. Otherwise the global functional definition of the function name
is returned. Lexically defined functional objectecs are evaluated in compile
time, while global function definitions are evaluated in runtime using
'symbol-function' function.")

;;--------------------------------------------------------------------------
;; tagbody
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'tagbody
  "Special form: (tagbody body) => nil
              (go tag)

Executes one or more statements in a lexical environment that provides for
control transfers to labels indicated by the tags. Tags are atoms on top level
of 'tagbody' form, and they are matched with 'eq' function.

The statements in a 'tagbody' are evaluated in order from left to right, and their
values are discarded. If at any time there are no remaining statements, 'tagbody'
returns nil. However, if '(go tag)' is evaluated, control jumps to the part of
the body labeled with the tag. Tags are not evaluated in 'go' and matched with
'eq' function.

A tag established by 'tagbody' has lexical scope and has dynamic extent. Once
'tagbody' has been exited, it is no longer valid to 'go' to a tag in its body. It is
permissible for 'go' to jump to a 'tagbody' that is not the innermost 'tagbody'
containing that 'go'; the tags established by a 'tagbody' only shadow other tags of
like name.

The determination of which elements of the body are tags and which are
statements is made prior to any macro expansion of that element. If a statement
is a macro form and its macro expansion is an atom, that atom is treated as a
statement, not a tag.")

;;--------------------------------------------------------------------------
;; go
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'go
  "Special form 'go' is used in 'tagbody' special form")

;;--------------------------------------------------------------------------
;; unwind-protect
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'unwind-protect
  "Special form: (unwind-protect protected-form cleanup-form+)

'unwind-protect' evaluates protected-form and guarantees that cleanup-forms are
executed before 'unwind-protect' exits, whether it terminates normally or is
aborted by a control transfer of some kind. 'unwind-protect' is intended to be
used to make sure that certain side effects take place after the evaluation of
protected-form.

If a non-local exit occurs during execution of cleanup-forms, no special action
is taken. The cleanup-forms of 'unwind-protect' are not protected by that
'unwind-protect'.

'unwind-protect' protects against all attempts to exit from protected-form,
including 'go', 'throw', 'signal' and 'return-from'.")

;;--------------------------------------------------------------------------
;; symbol-macrolet
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-macrolet
  "Special form: (symbol-macrolet ((var value)*) body) ==> object

Syntax: var - symbol.
        value - form
        body - form+.

Returns result of last expression.

'symbol-macrolet' provides a mechanism for affecting the macro expansion
environment for symbols.

'symbol-macrolet' lexically establishes expansion functions for each of the symbol
macros named by symbols. The only guaranteed property of an expansion function
for a symbol macro is that when it is applied to the form and the environment it
returns the correct expansion.

Each reference to symbol as a variable within the lexical scope of
'symbol-macrolet' is expanded by the normal macro expansion process.  The
expansion of a symbol macro is subject to further macro expansion in the same
lexical environment as the symbol macro invocation, exactly analogous to normal
macros.

The use of 'symbol-macrolet' can be shadowed by let. In other words,
'symbol-macrolet' only substitutes for occurrences of symbol that would be in the
scope of a lexical binding of symbol surrounding the forms.")

;;--------------------------------------------------------------------------
;; block
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'block
  "Special form: (block name body) ==> object

Syntax: name - symbol or nil
        body - form+

'Block' establishes a block named name and then evaluates forms as an implicit
'progn'.

The special operators 'block' and 'return-from' work together to provide a
structured, lexical, non-local exit facility. At any point lexically contained
within forms, 'return-from' can be used with the given name to return control and
values from the 'block' form, except when an intervening block with the same name
has been established, in which case the outer block is shadowed by the inner
one.

The 'block' named name has lexical scope and dynamic extent.

Once established, a 'block' may only be exited once, whether by normal return or
explicit return.")

;;--------------------------------------------------------------------------
;; return-from
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'return-from
  "Special form: (return-from name [value]) ==> none

Syntax: name - symbol or nil
        value - form. Default is nil.

Returns control and value from a lexically enclosing block with given name.

A 'block' form named name must lexically enclose the occurrence of
'return-from'.")

;;--------------------------------------------------------------------------
;; symbolp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbolp
  "Function: (symbolp x) ==> bool

Parameters: x - object

Returns true if x is symbol; otherwise, returns false.")

;;--------------------------------------------------------------------------
;; keywordp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'keywordp
  "Function: (keywordp x) ==> bool

Parameters: x - object

Returns true if x is keyword symbol; otherwise, returns false.")

;;--------------------------------------------------------------------------
;; MOST-POSITIVE-FIXNUM
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'MOST-POSITIVE-FIXNUM
 "Maximal value of fixnum numbers.")

;;--------------------------------------------------------------------------
;; MOST-NEGATIVE-FIXNUM
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'MOST-NEGATIVE-FIXNUM
 "Minimal value of fixnum numbers.")

;;--------------------------------------------------------------------------
;; USTRING-MAX-CAPACITY
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'USTRING-MAX-CAPACITY
  "Maximal number of characters in Unicode strings")

;;--------------------------------------------------------------------------
;; BINARY-MAX-CAPACITY
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'BINARY-MAX-CAPACITY
  "Maximal number of bytes in binaries.")

;;--------------------------------------------------------------------------
;; VECTOR-MAX-CAPACITY
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'VECTOR-MAX-CAPACITY
  "Maximal number of objects in vectors.")

;;--------------------------------------------------------------------------
;; MOST-POSITIVE-DOUBLE-FLOAT
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'MOST-POSITIVE-DOUBLE-FLOAT
  "Maximal value of positive double numbers.")

;;--------------------------------------------------------------------------
;; LEAST-POSITIVE-DOUBLE-FLOAT
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'LEAST-POSITIVE-DOUBLE-FLOAT
  "Minimal value of positive double numbers.")

;;--------------------------------------------------------------------------
;; MOST-NEGATIVE-DOUBLE-FLOAT
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'MOST-NEGATIVE-DOUBLE-FLOAT
  "Maximal value of negative double numbers.")

;;--------------------------------------------------------------------------
;; LEAST-NEGATIVE-DOUBLE-FLOAT
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'LEAST-NEGATIVE-DOUBLE-FLOAT
  "Minimal value of negative double numbers.")

;;--------------------------------------------------------------------------
;; *load-pathname*
;;--------------------------------------------------------------------------
(set-symbol-value-doc '*load-pathname*
  "Full path of currently loading file, as set by 'load' function.")

;;--------------------------------------------------------------------------
;; +
;;--------------------------------------------------------------------------
(set-symbol-function-doc '+
  "Function: (+ &rest args) ==> number

Parameters: args - numbers

Returns sum of numbers. If non arguments are specified, then 0 is returned.")

;;--------------------------------------------------------------------------
;; -
;;--------------------------------------------------------------------------
(set-symbol-function-doc '-
  "Function: (- minuend &rest subtrahends) ==> number

Parameters: minuend - number
            subtrahends - numbers

Performs arithmetic subtraction and negation. If only one number is supplied,
the negation of that number is returned. If more than one argument is given, it
subtracts all of the subtrahends from the minuend and returns the result.")

;;--------------------------------------------------------------------------
;; *
;;--------------------------------------------------------------------------
(set-symbol-function-doc '*
  "Function: (* &rest args) ==> number

Parameters: args - numbers

Returns the product of numbers. If no numbers are supplied, 1 is returned.")

;;--------------------------------------------------------------------------
;; /
;;--------------------------------------------------------------------------
(set-symbol-function-doc '/
  "Function: (/ numerator &rest denominators) ==> float

Parameters: numerator - number
            denominators - numbers

Performs floating point division of numerator by denominators or reciprocation
of numerator, if no denominators are supplied. Always returns floating point
number.")

;;--------------------------------------------------------------------------
;; div
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'div
  "Function: (div numerator &rest denominators) ==> integer

Parameters: numerator - integer
            denominators - integers

Performs integer division of numerator by denominators. Each division is
truncated, if numerator is not divisible by current denominator.")

;;--------------------------------------------------------------------------
;; rem
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'rem
  "Function: (rem number divisor) ==> integer

Parameters: number - integer
            divisor - integer

Calculates remainder of integer division of number by divisor.")

;;--------------------------------------------------------------------------
;; zerop
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'zerop
  "Function: (zerop x) ==> bool

Parameters: x - number

Returns true, if x is equal to 0, otherwise, nil.")

;;--------------------------------------------------------------------------
;; abs
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'abs
  "Function: (abs x) ==> number

Parameters: x - number

Returns the absolute value of the number x.")

;;--------------------------------------------------------------------------
;; band
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'band
  "Function: (band &rest args) ==> integer

Parameters: args - integers.

Performs bitwise 'and' operation of integers.")

;;--------------------------------------------------------------------------
;; bor
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'bor
  "Function: (bor &rest args) ==> integer

Parameters: args - integers.

Performs bitwise 'or' operation of integers.")

;;--------------------------------------------------------------------------
;; bxor
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'bxor
  "Function: (bxor &rest args) ==> integer

Parameters: args - integers.

Performs bitwise 'xor' operation of integers.")

;;--------------------------------------------------------------------------
;; bnot
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'bnot
  "Function: (bnot x) ==> integer

Parameters: x - integer.

Performs bitwise 'not' operation of integer.")

;;--------------------------------------------------------------------------
;; bshl
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'bshl
  "Function: (bshl x bits) ==> integer

Parameters: x - integer.
            bits - fixnum

Shifts integer left on given number of 'bits'.")

;;--------------------------------------------------------------------------
;; bshr
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'bshr
  "Function: (bshr x bits) ==> integer

Parameters: x - integer.
            bits - fixnum

Shifts integer right on given number of 'bits'.")

;;--------------------------------------------------------------------------
;; minusp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'minusp
  "Function: (minusp x) ==> bool

Parameters: x - number.

Returns true if number is negative; otherwise returns false.")

;;--------------------------------------------------------------------------
;; plusp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'plusp
  "Function: (plusp x) ==> bool

Parameters: x - number.

Returns true if number is positive; otherwise returns false.")

;;--------------------------------------------------------------------------
;; fixnump
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'fixnump
  "Function: (fixnump x) ==> bool

Parameters: x - object.

Returns true if object is fixnum integer; otherwise returns false.")

;;--------------------------------------------------------------------------
;; bigintp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'bigintp
  "Function: (bigintp x) ==> bool

Parameters: x - object.

Returns true if object is big integer; otherwise returns false.")

;;--------------------------------------------------------------------------
;; integerp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'integerp
  "Function: (integerp x) ==> bool

Parameters: x - object.

Returns true if object is integer; otherwise returns false.")

;;--------------------------------------------------------------------------
;; doublep
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'doublep
  "Function: (doublep x) ==> bool

Parameters: x - object.

Returns true if object is double floating point number; otherwise returns false.")

;;--------------------------------------------------------------------------
;; numberp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'numberp
  "Function: (numberp x) ==> bool

Parameters: x - object.

Returns true if object is number; otherwise returns false.")

;;--------------------------------------------------------------------------
;; finitep
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'finitep
  "Function: (finitep x) ==> bool

Parameters: x - number

Returns true, if number is finite.")

;;--------------------------------------------------------------------------
;; infinitep
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'infinitep
  "Function: (infinitep x) ==> bool

Parameters: x - number

Returns true, if number is infinite.")

;;--------------------------------------------------------------------------
;; nanp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'nanp
  "Function: (nanp x) ==> bool

Parameters: x - number

Returns true, if number is not a number.")

;;--------------------------------------------------------------------------
;; <
;;--------------------------------------------------------------------------
(set-symbol-function-doc '<
  "Function: (< num &rest other) ==> bool

Parameters: num - number.
            other - numbers

Returns true if each following number is less then previos one; otherwise
returns false. Also returns true, if only single number is given.")

;;--------------------------------------------------------------------------
;; <=
;;--------------------------------------------------------------------------
(set-symbol-function-doc '<=
  "Function: (<= num &rest other) ==> bool

Parameters: num - number.
            other - numbers

Returns true if each following number is less or equal to previos one; otherwise
returns false. Also returns true, if only single number is given.")

;;--------------------------------------------------------------------------
;; >
;;--------------------------------------------------------------------------
(set-symbol-function-doc '>
  "Function: (> num &rest other) ==> bool

Parameters: num - number.
            other - numbers

Returns true if each following number is greater then previos one; otherwise
returns false. Also returns true, if only single number is given.")

;;--------------------------------------------------------------------------
;; >=
;;--------------------------------------------------------------------------
(set-symbol-function-doc '>=
  "Function: (>= num &rest other) ==> bool

Parameters: num - number.
            other - numbers

Returns true if each following number is greater or equal to previos one;
otherwise returns false. Also returns true, if only single number is given.")

;;--------------------------------------------------------------------------
;; =
;;--------------------------------------------------------------------------
(set-symbol-function-doc '=
  "Function: (= num &rest other) ==> bool

Parameters: num - number.
            other - numbers

Returns true if all numbers are equal; otherwise returns false. Also returns
true, if only single number is given.")

;;--------------------------------------------------------------------------
;; /=
;;--------------------------------------------------------------------------
(set-symbol-function-doc '/=
  "Function: (/= num &rest other) ==> bool

Parameters: num - number.
            other - numbers

Returns true if all numbers are not equal; otherwise returns false. Also returns
true, if only single number is given.")

;;--------------------------------------------------------------------------
;; 1+
;;--------------------------------------------------------------------------
(set-symbol-function-doc '1+
  "Function: (1+ num) ==> number

Parameters: num - number.

Returns number, which is one more than argument.")

;;--------------------------------------------------------------------------
;; 1-
;;--------------------------------------------------------------------------
(set-symbol-function-doc '1-
  "Function: (1- num) ==> number

Parameters: num - number.

Returns number, which is one less than argument.")

;;--------------------------------------------------------------------------
;; m:exp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:exp
  "Function: (m:exp x) ==> double

Parameters: x - number

Returns e raised to the power 'x', where e is the base of the natural logarithms.")

;;--------------------------------------------------------------------------
;; m:pow
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:pow
  "Function: (m:pow base power) ==> double

Parameters: base - number
            power - number

Raises 'base' into 'power' power.")

;;--------------------------------------------------------------------------
;; m:acos
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:acos
  "Function: (m:acos x) ==> double

Parameters: x - number

Returns the arc cosine of 'x' in radians; the return value is in the range [0, pi].")

;;--------------------------------------------------------------------------
;; m:acosh
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:acosh
  "Function: (m:acosh x) ==> double

Parameters: x - number

Returns the inverse hyperbolic cosine of x.")

;;--------------------------------------------------------------------------
;; m:asin
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:asin
  "Function: (m:asin x) ==> double

Parameters: x - number

Returns the principal value of the arc sine of 'x' in radians; the return value is
in the range [-pi/2, pi/2].")

;;--------------------------------------------------------------------------
;; m:asinh
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:asinh
  "Function: (m:asinh x) ==> double

Parameters: x - number

Returns the inverse hyperbolic sine of 'x'.")

;;--------------------------------------------------------------------------
;; m:atan
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:atan
  "Function: (m:atan x) ==> double

Parameters: x - number

Returns the principal value of the arc tangent of 'x' in radians; the return value is
in the range [-pi/2, pi/2].")

;;--------------------------------------------------------------------------
;; m:atan2
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:atan2
  "Function: (m:atan2 y x) ==> double

Parameters: y - number
            x - number

Returns the principal value of the arc tangent of y/x in radians; the return
value is in the range [-pi, pi].")

;;--------------------------------------------------------------------------
;; m:atanh
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:atanh
  "Function: (m:atanh x) ==> double

Parameters: x - number

Return the inverse hyperbolic tangent of 'x'.")

;;--------------------------------------------------------------------------
;; m:cbrt
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:cbrt
  "Function: (m:cbrt x) ==> double

Parameters: x - number

Returns the cube root of x.")

;;--------------------------------------------------------------------------
;; m:ceil
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:ceil
  "Function: (m:ceil x) ==> number

Parameters: x - number

Returns the smallest integral value that is not less than x. For example,
(m:ceil 0.5) is 1.0, and (m:ceil -0.5) is 0.0. If x is integral, +0, -0, NaN, or
infinite, x itself is returned.")

;;--------------------------------------------------------------------------
;; m:cos
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:cos
  "Function: (m:cos x) ==> double

Parameters: x - number

Returns the cosine of x, where x is given in radians.")

;;--------------------------------------------------------------------------
;; m:cosh
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:cosh
  "Function: (m:cosh x) ==> double

Parameters: x - number

Returns the hyperbolic cosine of x, which is defined mathematically as:
        cosh(x) = (exp(x) + exp(-x)) / 2")

;;--------------------------------------------------------------------------
;; m:erf
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:erf
  "Function: (m:erf x) ==> double

Parameters: x - number

Returns the error function of x, defined as:
        erf(x) = 2/sqrt(pi)* integral from 0 to x of exp(-t*t) dt")

;;--------------------------------------------------------------------------
;; m:erfc
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:erfc
  "Function: (m:erfc x) ==> double

Parameters: x - number

Returns the complementary error function of x, that is, 1.0 - erf(x). Returned
value is in range [0, 2].")

;;--------------------------------------------------------------------------
;; m:exp2
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:exp2
  "Function: (m:exp2 x) ==> double

Parameters: x - number

Returns the value of 2 raised to the power of 'x'.")

;;--------------------------------------------------------------------------
;; m:expm1
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'expm1
  "Function: (m:expm1 x) ==> double

Parameters: x - number

Returns a value equivalent to exp(x) - 1. It is computed in a way that is
accurate even if the value of x is near zero - a case where exp(x) - 1 would be
inaccurate due to subtraction of two numbers that are nearly equal.")

;;--------------------------------------------------------------------------
;; m:e
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:e
  "Constant: m:e

The base of natural logarithms.")

;;--------------------------------------------------------------------------
;; m:log2e
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:log2e
  "Constant: m:log2e

The logarithm to base 2 of e.")

;;--------------------------------------------------------------------------
;; m:log10e
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:log10e
  "Constant: m:log10e

The logarithm to base 10 of e.")

;;--------------------------------------------------------------------------
;; m:ln2
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:ln2
  "Constant m:ln2

The natural logarithm of 2.")

;;--------------------------------------------------------------------------
;; m:ln10
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:ln10
  "Constant ln10

The natural logarithm of 10.")


;;--------------------------------------------------------------------------
;; m:pi
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:pi
  "Constatnt: m:pi

Pi, the ratio of a circle's circumference to its diameter.")

;;--------------------------------------------------------------------------
;; m:pi/2
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:pi/2
  "Constatnt: m:pi/2

Pi divided by two.")

;;--------------------------------------------------------------------------
;; m:pi/4
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:pi/4
  "Constatnt: m:pi/4

Pi divided by four.")

;;--------------------------------------------------------------------------
;; m:1/pi
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:1/pi
  "Constatnt: m:1/pi

The reciprocal of pi (1/pi).")

;;--------------------------------------------------------------------------
;; m:2/pi
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:2/pi
  "Constatnt: m:2/pi

Two times the reciprocal of pi (2/pi).")

;;--------------------------------------------------------------------------
;; m:2/sqrt_pi
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:2/sqrt_pi
  "Constatnt: m:2/sqrt_pi

Two times the reciprocal of the square root of pi (2/sqrt(pi)).")

;;--------------------------------------------------------------------------
;; m:sqrt2
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:sqrt2
  "Constatnt: m:sqrt2

The square root of two.")

;;--------------------------------------------------------------------------
;; m:sqrt1/2
;;--------------------------------------------------------------------------
(set-symbol-value-doc 'm:sqrt1/2
  "Constatnt: sqrt1/2

The reciprocal of the square root of two (also the square root of 1/2).")

;;--------------------------------------------------------------------------
;; m:floor
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:floor
  "Function: (m:floor x) ==> number

Parameters: x - number

Returns the largest integral value that is not greater than x. For example,
floor(0.5) is 0.0, and floor(-0.5) is -1.0. If x is integral, +0, -0, NaN, or an
infinity, x itself is returned.")

;;--------------------------------------------------------------------------
;; m:log
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:log
  "Function: (m:log x) ==> double

Parameters: x - number

Returns the natural logarithm of x.")

;;--------------------------------------------------------------------------
;; m:log10
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:log10
  "Function: (m:log10 x) ==> double

Parameters: x - number

Returns the base 10 logarithm of x.")

;;--------------------------------------------------------------------------
;; m:log2
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:log2
  "Function: (m:log2 x) ==> double

Parameters: x - number

Returns the base 2 logarithm of x.")

;;--------------------------------------------------------------------------
;; m:round
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:round
  "Function: (m:round x) ==> number

Parameters: x - number

Round x to the nearest integer. For example, round(0.5) is 1.0, and round(-0.5)
is -1.0. If x is integral, +0, -0, NaN,  or infinite, x itself is returned.")

;;--------------------------------------------------------------------------
;; m:sin
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:sin
  "Function: (m:sin x) ==> double

Parameters: x - number

Returns the sine of x, where x is given in radians.")

;;--------------------------------------------------------------------------
;; m:sinh
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:sinh
  "Function: (m:sinh x) ==> double

Parameters: x - number

Returns the hyperbolic sine of x, which is defined mathematically as:
        sinh(x) = (exp(x) - exp(-x)) / 2")

;;--------------------------------------------------------------------------
;; m:sqrt
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:sqrt
  "Function: (m:sqrt x) ==> double

Parameters: x - number

Returns the nonnegative square root of x.")

;;--------------------------------------------------------------------------
;; m:tan
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:tan
  "Function: (m:tan x) ==> double

Parameters: x - number

Returns the tangent of x, where x is given in radians.")

;;--------------------------------------------------------------------------
;; m:tanh
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:tanh
  "Function: (m:tanh x) ==> double

Parameters: x - number

Returns the hyperbolic tangent of x, which is defined mathematically as:
        tanh(x) = sinh(x) / cosh(x)")

;;--------------------------------------------------------------------------
;; m:trunc
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'm:trunc
  "Function: (m:trunc x) ==> number

Parameters: x - number

Round x to the nearest integer not larger in absolute value. If x is integral,
infinite, or NaN, x itself is returned.")

;;--------------------------------------------------------------------------
;; symbolp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbolp
  "Function: (symbolp x) ==> bool

Parameters: x - object

Returns true if x is symbol; otherwise, returns false.")

;;--------------------------------------------------------------------------
;; keywordp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'keywordp
  "Function: (keywordp x) ==> bool

Parameters: x - object

Returns true if x is keyword symbol; otherwise, returns false.")

;;--------------------------------------------------------------------------
;; symbol-create
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-create
  "Function: (symbol-create name) ==> symbol

Parameters: name - binary. Name of symbol. Name-slot of symbol is set to copy of
            name.

Creates and returns a fresh, uninterned symbol.")

;;--------------------------------------------------------------------------
;; symbol-name
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-name
  "Function: (symbol-name x) ==> binary

Parameters: x - symbol

Returns name slot of symbol object.")

;;--------------------------------------------------------------------------
;; symbol-bound-p
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-bound-p
  "Function: (symbol-bound-p x) ==> bool

Parameters: x - symbol

Returns true, if dynamic variable x is bound.")

;;--------------------------------------------------------------------------
;; symbol-fbound-p
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-fbound-p
  "Function: (symbol-fbound-p x) ==> bool

Parameters: x - symbol

Returns true, if symbol function slot is bound.")

;;--------------------------------------------------------------------------
;; dynamic
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'dynamic
  "Function: (dynamic sym) ==> object

Parameters: sym - symbol

Returns value of global dynamic variable. Variable must be defined with 'defvar'
or 'defparameter' macros.")

;;--------------------------------------------------------------------------
;; set-dynamic
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'set-dynamic
  "Function: (set-dynamic sym val) ==> val

Parameters: sym - symbol
            val - object

Sets value of global dynamic variable. Variable must be defined with 'defvar' or
'defparameter' macros.")

;;--------------------------------------------------------------------------
;; symbol-function
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-function
  "Function: (symbol-function x) ==> lambda or macro

Parameters: x - symbol

Returns function slot of symbol. Function slot must be bound.")

;;--------------------------------------------------------------------------
;; symbol-set-prop
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-set-prop
  "Function: (symbol-set-prop sym key val) ==> sym

Parameters: sym - symbol
            key - object
            val - object

Updates symbol's property in plist slot of symbol object. If key is not exists
new key-value pair is added to properties list. Keys are matched using 'eq'
function.")

;;--------------------------------------------------------------------------
;; symbol-get-prop
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-get-prop
  "Function: (symbol-get-prop sym key &optional default-val) ==> object

Parameters: sym - symbol
            key - object
            default-val - object

Searches symbol's plist slot for given key. If key is not found, then supplied
default-val is returned. Keys are matched using 'eq' function.")

;;--------------------------------------------------------------------------
;; symbol-rm-prop
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-rm-prop
  "Function: (symbol-rm-prop sym key) ==> sym

Parameters: sym - symbol
            key - object

Removes key, if presented, from symbol's plist slot. Keys are matched using
'eq' function.")

;;--------------------------------------------------------------------------
;; symbol-package
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-package
  "Function: (symbol-package sym) ==> binary or nil

Parameters: sym - symbol

Returns name of package, in which symbol is interned. If symbol is uninterned
then nil is returned.")

;;--------------------------------------------------------------------------
;; symbol-macro
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'symbol-macro
  "Macro: (symbol-macro sym &optional not-found) ==> form

Parameters: sym - symbol
            not-found - form. Value returned in case when symbol does not have
              macro bound with define-symbol-macro macro.

Returns macro bound to symbol with define-symbol-macro macro.")

;;--------------------------------------------------------------------------
;; undefine-symbol-macro
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'undefine-symbol-macro
  "Macro: (undefine-symbol-macro sym) ==> bool

Parameters: sym - symbol

Unbinds macro bound to symbol with define-symbol-macro macro. Returns true if
macro was really undefined. Otherwise, returns nil.")

;;--------------------------------------------------------------------------
;; gensym
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'gensym
  "Function: (gensym &optional (prefix \"g\")) ==> symbol

Parameters: prefix - binary

Creates fresh uninterned symbol like 'symbol-create' function. The difference is
how symbol name is produced: gensym generates symbol name in form '#:prefixN',
where 'prefix' is parameter and N is current value of gensym's internal
counter. Value of counter is incremented on each call of gensym.")

;;--------------------------------------------------------------------------
;; intern
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'intern
  "Function: (intern name &optional (package *package*) exported) ==> symbol

Parameters: name - binary
            package - symbol or binary or package
            exported - bool

Function intern enters a symbol named 'name' into package. If package is
ommited, then symbol interned into current package. If symbol with such name is
already exist in package, the this symbol is returned. If 'exported' is true,
then new symbol is exported. 'Exported' does not affect symbols, already
presented in package.")

;;--------------------------------------------------------------------------
;; unintern
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'unintern
  "Function: (unintern sym) ==> sym

Parameters: sym - symbol

Removes symbol from package, to which symbol relates. Returns same, but
uninterned symbol.")

;;--------------------------------------------------------------------------
;; interned-p
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'interned-p
  "Function: (interned-p x) ==> bool

Parameters: x - symbol.

Returns t if symbol is interned.")

;;--------------------------------------------------------------------------
;; export
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'export
  "Function: (export sym) ==> sym

Parameters: sym - symbol

Exports symbol from it's package.")

;;--------------------------------------------------------------------------
;; unexport
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'unexport
  "Function: (unexport sym) ==> sym

Parameters: sym - symbol

Removes symbol from exported list of package.")

;;--------------------------------------------------------------------------
;; exported-p
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'exported-p
  "Function: (exported-p sym) ==> bool

Parameters: sym - symbol

Returns true, if symbol is exported from package.")

;;--------------------------------------------------------------------------
;; find-create-package
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'find-create-package
  "Function: (find-create-package name) ==> package

Parameters: name - binary or symbol.

Searches for package with given name. If package does not exit, it is created.")

;;--------------------------------------------------------------------------
;; package-name
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'package-name
  "Function: (package-name package) ==> binary

Parameters: package - package

Returns name of package.")

;;--------------------------------------------------------------------------
;; ucharp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ucharp
  "Function: (ucharp x) ==> bool

Parameters: x - object

Returns true, if x is valid Unicode character.")

;;--------------------------------------------------------------------------
;; char-alphap
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-alphap
  "Function: (char-alphap x) ==> bool

Parameters: x - ASCII char

Returns true if x is an alphabetic ASCII character.")

;;--------------------------------------------------------------------------
;; uchar-alphap
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-alphap
  "Function: (uchar-alphap x) ==> bool

Parameters: x - Unicode char

Returns true if x is an alphabetic Unicode character.")

;;--------------------------------------------------------------------------
;; char-alphanumericp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-alphanumericp
  "Function: (char-alphanumericp x) ==> bool

Parameters: x - ASCII char

Returns true if x is an alphanumeric ASCII character.")

;;--------------------------------------------------------------------------
;; uchar-alphanumericp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-alphanumericp
  "Function: (uchar-alphanumericp x) ==> bool

Parameters: x - Unicode char

Returns true if x is an alphanumeric Unicode character.")

;;--------------------------------------------------------------------------
;; char-controlp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-controlp
  "Function: (char-controlp x) ==> bool

Parameters: x - ASCII char

Returns true if x is a control ASCII character.")

;;--------------------------------------------------------------------------
;; uchar-controlp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-controlp
  "Function: (uchar-controlp x) ==> bool

Parameters: x - Unicode char

Returns true if x is a control ASCII character.")

;;--------------------------------------------------------------------------
;; char-digitp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-digitp
  "Function: (char-digitp x) ==> bool

Parameters: x - ASCII char

Returns true if x is a digit ASCII character.")

;;--------------------------------------------------------------------------
;; uchar-digitp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-digitp
  "Function: (uchar-digitp x) ==> bool

Parameters: x - Unicode char

Returns true if x is a digit Unicode character.")

;;--------------------------------------------------------------------------
;; char-graphp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-graphp
  "Function: (char-graphp x) ==> bool

Parameters: x - ASCII char

Determines whether a character is printable and not a space (returns nil for
control characters, format characters, and spaces). char-printp is similar, but
returns true for spaces.")

;;--------------------------------------------------------------------------
;; uchar-graphp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-graphp
  "Function: (uchar-graphp x) ==> bool

Parameters: x - Unicode char

Determines whether a character is printable and not a space (returns nil for
control characters, format characters, and spaces). char-printp is similar, but
returns true for spaces.")

;;--------------------------------------------------------------------------
;; char-lowerp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-lowerp
  "Function: (char-lowerp x) ==> bool

Parameters: x - ASCII char

Determines whether ASCII character is a lowercase letter.")

;;--------------------------------------------------------------------------
;; uchar-lowerp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-lowerp
  "Function: (uchar-lowerp x) ==> bool

Parameters: x - Unicode char

Determines whether Unicode character is a lowercase letter.")

;;--------------------------------------------------------------------------
;; char-printp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-printp
  "Function: (char-printp x) ==> bool

Parameters: x - ASCII char

Determines whether ASCII character is printable. Unlike char-graphp, returns true
for spaces.")

;;--------------------------------------------------------------------------
;; uchar-printp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-printp
  "Function: (uchar-printp x) ==> bool

Parameters: x - Unicode char

Determines whether Unicode character is printable. Unlike char-graphp, returns true
for spaces.")

;;--------------------------------------------------------------------------
;; char-punctp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-punctp
  "Function: (char-punctp x) ==> bool

Parameters: x - ASCII char

Determines whether ASCII character is punctuation or a symbol.")

;;--------------------------------------------------------------------------
;; uchar-punctp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-punctp
  "Function: (uchar-punctp x) ==> bool

Parameters: x - Unicode char

Determines whether Unicode character is punctuation or a symbol.")

;;--------------------------------------------------------------------------
;; char-spacep
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-spacep
  "Function: (char-spacep x) ==> bool

Parameters: x - ASCII char

Determines whether ASCII character is a space, tab, or line separator (newline,
carriage return, etc.).")

;;--------------------------------------------------------------------------
;; uchar-spacep
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-spacep
  "Function: (uchar-spacep x) ==> bool

Parameters: x - Unicode char

Determines whether Unicode character is a space, tab, or line separator (newline,
carriage return, etc.).")

;;--------------------------------------------------------------------------
;; char-upperp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-upperp
  "Function: (char-upperp x) ==> bool

Parameters: x - ASCII char

Determines if ASCII character is uppercase.")

;;--------------------------------------------------------------------------
;; uchar-upperp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-upperp
  "Function: (uchar-upperp x) ==> bool

Parameters: x - Unicode char

Determines if Unicode character is uppercase.")

;;--------------------------------------------------------------------------
;; char-xdigitp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-xdigitp
  "Function: (char-xdigitp x) ==> bool

Parameters: x - ASCII char

Determines if ASCII character is a hexidecimal digit.")

;;--------------------------------------------------------------------------
;; uchar-xdigitp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'uchar-xdigitp
  "Function: (uchar-xdigitp x) ==> bool

Parameters: x - Unicode char

Determines if Unicode character is a hexidecimal digit.")

;;--------------------------------------------------------------------------
;; char-to-lower
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-to-lower
  "Function: (char-to-lower x) ==> char

Parameters: x - ASCII char

Converts ASCII character to lower case.")

;;--------------------------------------------------------------------------
;; char-to-upper
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'char-to-upper
  "Function: (char-to-upper x) ==> char

Parameters: x - ASCII char

Converts ASCII character to uppercase.")

;;--------------------------------------------------------------------------
;; ustringp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustringp
  "Function: (ustringp x) ==> bool

Parameters: x - object

Returns true if object is Unicode string.")

;;--------------------------------------------------------------------------
;; ustring
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring
  "Function: (ustring &rest args) ==> ustring

Parameters: args - Unicode characters and/or strings.

Creates new Unicode string object and populates it with given arguments.")

;;--------------------------------------------------------------------------
;; ustring-create
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-create
  "Function: (ustring-create size &optional (init-element #\" \") max-capacity) ==> ustring

Parameters: size - non negative fixnum
            init-element - Unicode character
            max-capacity - non negative fixnum

Creates new Unicode string of given initial size and maximal capacity and fills
it with given initial element.")

;;--------------------------------------------------------------------------
;; ustring-length
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-length
  "Function: (ustring-length seq) ==> fixnum

Parameters: seq - Unicode string

Returns length of Unicode string.")

;;--------------------------------------------------------------------------
;; ustring-ref
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-ref
  "Function: (ustring-ref seq pos) ==> char

Parameters: seq - Unicode string
            pos - non negative fixnum

Returns element at 'pos' position of Unicode string.")

;;--------------------------------------------------------------------------
;; ustring-set
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-set
  "Function: (ustring-set seq pos val) ==> val

Parameters: seq - Unicode string
            pos - non negative fixnum
            val - Unicode char

Sets value of character at 'pos' position of Unicode string.")

;;--------------------------------------------------------------------------
;; ustring-append-uchar
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-append-uchar
  "Function: (ustring-append-uchar seq x) ==> seq

Parameters: seq - string
            x - Unicode character

Appends character to end of string.")

;;--------------------------------------------------------------------------
;; ustring-append-binary
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-append-binary
  "Function: (ustring-append-binary dest src &optional encoding) ==> dest

Parameters: dest - destination Unicode string
            src - source binary.
            encoding - binary. Source binary is encoded with given encoding. If
              encoding is ommited then character set of current thread's locale
              is used.

Converts multibyte binary to Unicode and appends it to end of ustring.")

;;--------------------------------------------------------------------------
;; ustring-append-utf8-binary
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-append-utf8-binary
  "Function: (ustring-append-binary-utf8 dest src) ==> dest

Parameters: dest - destination Unicode string
            src - source binary. Binary must be multibyte string, encoded in UTF-8.

Converts UTF-8 binary to UTF-32 and appends it to end of string.")

;;--------------------------------------------------------------------------
;; ustring-append-ustring
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-append-ustring
  "Function: (ustring-append-ustring dest src) ==> dest

Parameters: dest - destination string
            src - source string

Appends Unicode string to end of another Unicode string.")

;;--------------------------------------------------------------------------
;; ustring-ensure-capacity
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-ensure-capacity
  "Function: (ustring-ensure-capacity seq delta) ==> seq

Parameters: seq - string
            delta - capacity delta, non negative fixnum

Ensures that string has enough capacity to append 'delta' characters without
storage reallocations.")

;;--------------------------------------------------------------------------
;; ustring-clear
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-clear
  "Function: (ustring-clear seq) ==> seq

Parameters: seq - Unicode string

Removes all characters from string.")

;;--------------------------------------------------------------------------
;; ustring-remove
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-remove
  "Function: (ustring-remove seq start &optional count) ==> seq

Parameters: seq - Unicde string
            start - start position, non negative fixnum.
            count - number of elements to remove, non negative fixnum. If count
            is nil sequence is truncated from 'start' position.

Removes 'count' characters in string starting from 'start' position.")

;;--------------------------------------------------------------------------
;; ustring-insert-uchar
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-insert-uchar
  "Function: (ustring-insert-uchar seq pos x) ==> seq

Parameters: seq - Unocde string
            pos - position to insert, non negative fixnum.
            x - Unicode character

Inserts character into string at given position. String is modified.")

;;--------------------------------------------------------------------------
;; ustring-insert-binary
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-insert-binary
  "Function: (string-insert-binary dest dest-pos src &optional encoding) ==> dest

Parameters: dest - destination Unicde string
            dest-pos - position to insert, non negative fixnum.
            src - source binary.
            encoding - binary. Source binary is encoded with given encoding. If
              encoding is ommited then character set of current thread's locale
              is used.

Converts multibyte string to Unicode and inserts it into string at given
position. String is modified.")

;;--------------------------------------------------------------------------
;; ustring-insert-utf8-binary
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-insert-utf8-binary
  "Function: (string-insert-utf8-binary dest dest-pos src) ==> dest

Parameters: dest - destination string
            dest-pos - position to insert, non negative fixnum.
            src - source binary. Binary must be multibyte string, encoded in UTF-8.

Converts multibyte string to Unicode and inserts it into string at given
position. String is modified.")

;;--------------------------------------------------------------------------
;; ustring-insert-ustring
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-insert-ustring
  "Function: (ustring-insert-ustring dest dest-pos src) ==> dest

Parameters: dest - destination Unicode string
            dest-pos - position to insert, non negative fixnum.
            src - source Unicode string.

Inserts string into another string at given position. String is modified.")

;;--------------------------------------------------------------------------
;; ustring-reverse
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-reverse
  "Function: (ustring-reverse seq) ==> seq

Parameters: seq - Unicode string

Reverses string in place.")

;;--------------------------------------------------------------------------
;; ustring-set-max-capacity
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-set-max-capacity
  "Function: (ustring-set-max-capacity seq capacity) ==> seq

Parameters: seq - Unicode string
            capacity - non negative fixnum. Must be greater or equal current
              string length.

Limits maximal capacity of string.")

;;--------------------------------------------------------------------------
;; ustring-concat
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-concat
  "Function: (ustring-concat seq &rest args) ==> seq

Parameters: seq - Unicode string
            args - one of: valid Unicode character or Unicode string.

Appends all arguments to string.")

;;--------------------------------------------------------------------------
;; ustring-compare
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-compare
  "Function: (ustring-compare s1 s2) ==> fixnum

Parameters: s1 - Unicode string
            s2 - Unicode string

Compares two strings. It returns an fixnum less than, equal to, or greater than
zero if s1 is found, respectively, to be less than, to match, or be greater than
s2.")

;;--------------------------------------------------------------------------
;; ustring-find-uchar
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-find-uchar
  "Function: (ustring-find-uchar seq chr &optional (start 0)) ==> fixnum or nil

Parameters: seq - Unicode string
            chr - Unicode character
            start - fixnum

Searches string for character from 'start' position. Returns character position
or nil, if character is not found.")

;;--------------------------------------------------------------------------
;; ustring-find-ustring
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-find-ustring
  "Function: (ustring-find-ustring seq str &optional (start 0)) ==> fixnum or nil

Parameters: seq - Unicode string
            str - Unicode string to find
            start - fixnum

Searches string for substring from 'start' position. Returns substring position or
nil, if substring is not found.")

;;--------------------------------------------------------------------------
;; ustring-starts
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'ustring-starts
  "Function: (ustring-starts str prefix) ==> boolean

Parameters: str - Unicode string
            prefix - Unicode string

Checks if string 'str' starts with given prefix.")

;;--------------------------------------------------------------------------
;; binaryp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binaryp
  "Function: (binaryp x) ==> bool

Parameters: x - object

Returns true if object is binary.")

;;--------------------------------------------------------------------------
;; binary
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary
  "Function: (binary &rest args) ==> binary

Parameters: args - one of: integer in range 0-255 or binary.

Creates new binary object and populates it with given arguments.")

;;--------------------------------------------------------------------------
;; binary-create
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-create
  "Function: (binary-create size &optional (init-element 0) max-capacity) ==> binary

Parameters: size - non negative fixnum
            init-element - integer in range 0-255
            max-capacity - non negative fixnum

Creates new binary of given initial size and maximal capacity and fills it with
given initial element.")

;;--------------------------------------------------------------------------
;; binary-length
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-length
  "Function: (binary-length seq) ==> fixnum

Parameters: seq - binary

Returns length of binary.")

;;--------------------------------------------------------------------------
;; binary-ref
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-ref
  "Function: (binary-ref seq pos) ==> byte

Parameters: seq - binary
            pos - non negative fixnum

Returns integer in range 0-255 at 'pos' position of binary.")

;;--------------------------------------------------------------------------
;; binary-set
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-set
  "Function: (binary-set seq pos val) ==> val

Parameters: seq - binary
            pos - non negative fixnum
            val - integer in range 0-255

Sets value of element at position 'pos' of binary.")

;;--------------------------------------------------------------------------
;; binary-append-char
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-append-char
  "Function: (binary-append-char seq x) ==> seq

Parameters: seq - binary
            x - integer in range 0-255.

Appends character to end of binary.")

;;--------------------------------------------------------------------------
;; binary-append-binary
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-append-binary
  "Function: (binary-append-binary dest src) ==> dest

Parameters: dest - destinationbinary
            src - source binary.

Appends binary object to end of binary.")

;;--------------------------------------------------------------------------
;; binary-append-uchar
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-append-uchar
  "Function: (binary-append-uchar seq x &optional encoding
                                 (error-mode iconveh_error)) ==> seq

Parameters: seq - binary
            x - Unicode character.
            encoding - binary. Unicode character 'x' is encoded with given
              encoding. If encoding is ommited then character set of current
              thread's locale is used.
            error-mode - one of predefined constants: iconveh_error,
              iconveh_question_mark or iconveh_escape_sequence. Defines how
              function behaves if Unicode character cannot be represented in
              selected encoding:
                iconveh_error - system error is signaled.
                iconveh_question_mark - question mark is put.
                iconveh_escape_sequence - escape sequence \\Uxxxxxxxx is put.

Converts UTF-32 character to given encoding and appends it to end of binary.")

;;--------------------------------------------------------------------------
;; binary-append-uchar-utf8
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-append-uchar-utf8
  "Function: (binary-append-uchar-utf8 seq x) ==> seq

Parameters: seq - binary
            x - UTF-32 character. Character is converted to UTF-8 encoded character.

Converts UTF-32 character to UTF-8 and appends it to end of binary.")

;;--------------------------------------------------------------------------
;; binary-append-ustring
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-append-ustring
  "Function: (binary-append-ustring dest src &optional encoding
                                    (error-mode iconveh_error)) ==> dest

Parameters: dest - destination binary
            src - Unicode string.
            encoding - binary. Unicode string 'src' is encoded with given
              encoding. If encoding is ommited then character set of current
              thread's locale is used.
            error-mode - one of predefined constants: iconveh_error,
              iconveh_question_mark or iconveh_escape_sequence. Defines how
              function behaves if Unicode character cannot be represented in
              selected encoding:
                iconveh_error - system error is signaled.
                iconveh_question_mark - question mark is put.
                iconveh_escape_sequence - escape sequence \\Uxxxxxxxx is put.

Converts UTF-32 string to given encoding and appends string to end of binary.")

;;--------------------------------------------------------------------------
;; binary-append-ustring-utf8
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-append-ustring-utf8
  "Function: (binary-append-ustring-utf8 dest src) ==> dest

Parameters: dest - destination binary
            src - Unicode string.

Converts UTF-32 string to UTF-8 and appends string to end of binary.")

;;--------------------------------------------------------------------------
;; binary-ensure-capacity
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-ensure-capacity
  "Function: (binary-ensure-capacity seq delta) ==> seq

Parameters: seq - binary
            delta - capacity delta, non negative fixnum

Ensures that 'delta' bytes may be added to binary without storage reallocations.")

;;--------------------------------------------------------------------------
;; binary-clear
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-clear
  "Function: (binary-clear seq) ==> seq

Parameters: seq - binary

Erases all elements in binary.")

;;--------------------------------------------------------------------------
;; binary-remove
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-remove
  "Function: (binary-remove seq start &optional count) ==> seq

Parameters: seq - binary
            start - start position, non negative fixnum.
            count - number of elements to remove, non negative fixnum. If count
              is nil sequence is truncated from 'start' position.

Removes 'count' elements in binary starting from 'start' position.")

;;--------------------------------------------------------------------------
;; binary-insert-char
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-insert-char
  "Function: (binary-insert-char seq pos x) ==> seq

Parameters: seq - binary
            pos - position to insert, non negative fixnum.
            x - integer in range 0-255.

Inserts character into binary at given position.")

;;--------------------------------------------------------------------------
;; binary-insert-binary
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-insert-binary
  "Function: (binary-insert-binary dest dest-pos src) ==> dest

Parameters: dest - destination binary
            dest-pos - position to insert, non negative fixnum.
            src - source binary.

Inserts binary object into another binary at given position.")

;;--------------------------------------------------------------------------
;; binary-insert-uchar
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-insert-uchar
  "Function: (binary-insert-uchar seq pos x &optional encoding
                                  (error-mode iconveh_error)) ==> seq

Parameters: seq - binary
            pos - position to insert, non negative fixnum.
            x - Unicode character.
            encoding - binary. Unicode character 'x' is encoded with given
              encoding. If encoding is ommited then character set of current
              thread's locale is used.
            error-mode - one of predefined constants: iconveh_error,
              iconveh_question_mark or iconveh_escape_sequence. Defines how
              function behaves if Unicode character cannot be represented in
              selected encoding:
                iconveh_error - system error is signaled.
                iconveh_question_mark - question mark is put.
                iconveh_escape_sequence - escape sequence \\Uxxxxxxxx is put.

Converts Unicode character to given encoding and inserts it into binary at given
position.")

;;--------------------------------------------------------------------------
;; binary-insert-uchar-utf8
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-insert-uchar-utf8
  "Function: (binary-insert-uchar-utf8 seq pos x) ==> seq

Parameters: seq - binary
            pos - position to insert, non negative fixnum.
            x - Unicode character.

Converts UTF-32 character to UTF-8 and inserts it into binary at given
position.")

;;--------------------------------------------------------------------------
;; binary-insert-ustring
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-insert-ustring
  "Function: (binary-insert-ustring dest dest-pos src &optional encoding
                                    (error-mode iconveh_error)) ==> dest

Parameters: dest - destination binary
            dest-pos - destination position to insert, non negative fixnum.
            src - source string. String is converted to multibyte string, using
              current thread's locale.
            encoding - binary. Unicode string 'src' is encoded with given
              encoding. If encoding is ommited then character set of current
              thread's locale is used.
            error-mode - one of predefined constants: iconveh_error,
              iconveh_question_mark or iconveh_escape_sequence. Defines how
              function behaves if Unicode character cannot be represented in
              selected encoding:
                iconveh_error - system error is signaled.
                iconveh_question_mark - question mark is put.
                iconveh_escape_sequence - escape sequence \\Uxxxxxxxx is put.

Converted UTF-32 string to given encoding: and inserts it into binary at given
position.")

;;--------------------------------------------------------------------------
;; binary-insert-ustring-utf8
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-insert-ustring-utf8
  "Function: (binary-insert-ustring-utf8 dest dest-pos src) ==> dest

Parameters: dest - destination binary
            dest-pos - destination position to insert, non negative fixnum.
            src - source string. String is converted to UTF-8.

Converts UTF-32 string to UTF-8 and inserts it into binary at given position.")

;;--------------------------------------------------------------------------
;; binary-reverse
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-reverse
  "Function: (binary-reverse seq) ==> seq

Parameters: seq - binary

Reverses binary in place.")

;;--------------------------------------------------------------------------
;; binary-set-max-capacity
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-set-max-capacity
  "Function: (binary-set-max-capacity seq capacity) ==> seq

Parameters: seq - binary
            capacity - non negative fixnum. Must be greater or equal current
            sequence length.

Limits maximal capacity of binary object to given value.")

;;--------------------------------------------------------------------------
;; binary-concat
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-concat
  "Function: (binary-concat seq &rest args) ==> seq

Parameters: seq - binary
            args - one of: integer in range 0-255, character, string or binary.

Appends all arguments to binary.")

;;--------------------------------------------------------------------------
;; binary-find-char
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-find-char
  "Function: (binary-find-char seq x &optional (start 0)) ==> fixnum or nil

Parameters: seq - binary
            x - integer in range 0-255
            start - fixnum

Searches binary for character from 'start' position. Return character's position
or nil, if character is not found.")

;;--------------------------------------------------------------------------
;; Binary integers encoding/decoding
;;--------------------------------------------------------------------------
(let ((d "Functions: (binary-append-int8 seq x) ==> seq
           (binary-ref-int8 seq offset) ==> integer
           (binary-append-uint8 seq x) ==> seq
           (binary-ref-uint8 seq offset) ==> integer
           (binary-set-int8 seq offset x) ==> seq
           (binary-set-uint8 seq offset x) ==> seq

           (binary-append-int16-le seq x) ==> seq
           (binary-append-int16-be seq x) ==> seq
           (binary-append-int16-me seq x) ==> seq
           (binary-ref-int16-le seq offset) ==> integer
           (binary-ref-int16-be seq offset) ==> integer
           (binary-ref-int16-me seq offset) ==> integer
           (binary-set-int16-le seq offset x) ==> seq
           (binary-set-int16-be seq offset x) ==> seq
           (binary-set-int16-me seq offset x) ==> seq

           (binary-append-uint16-le seq x) ==> seq
           (binary-append-uint16-be seq x) ==> seq
           (binary-append-uint16-me seq x) ==> seq
           (binary-ref-uint16-le seq offset) ==> integer
           (binary-ref-uint16-be seq offset) ==> integer
           (binary-ref-uint16-me seq offset) ==> integer
           (binary-set-uint16-le seq offset x) ==> seq
           (binary-set-uint16-be seq offset x) ==> seq
           (binary-set-uint16-me seq offset x) ==> seq

           (binary-append-int32-le seq x) ==> seq
           (binary-append-int32-be seq x) ==> seq
           (binary-append-int32-me seq x) ==> seq
           (binary-ref-int32-le seq offset) ==> integer
           (binary-ref-int32-be seq offset) ==> integer
           (binary-ref-int32-me seq offset) ==> integer
           (binary-set-int32-le seq offset x) ==> seq
           (binary-set-int32-be seq offset x) ==> seq
           (binary-set-int32-me seq offset x) ==> seq

           (binary-append-uint32-le seq x) ==> seq
           (binary-append-uint32-be seq x) ==> seq
           (binary-append-uint32-me seq x) ==> seq
           (binary-ref-uint32-le seq offset) ==> integer
           (binary-ref-uint32-be seq offset) ==> integer
           (binary-ref-uint32-me seq offset) ==> integer
           (binary-set-uint32-le seq offset x) ==> seq
           (binary-set-uint32-be seq offset x) ==> seq
           (binary-set-uint32-me seq offset x) ==> seq

           (binary-append-int64-le seq x) ==> seq
           (binary-append-int64-be seq x) ==> seq
           (binary-append-int64-me seq x) ==> seq
           (binary-ref-int64-le seq offset) ==> integer
           (binary-ref-int64-be seq offset) ==> integer
           (binary-ref-int64-me seq offset) ==> integer
           (binary-set-int64-le seq offset x) ==> seq
           (binary-set-int64-be seq offset x) ==> seq
           (binary-set-int64-me seq offset x) ==> seq

           (binary-append-uint64-le seq x) ==> seq
           (binary-append-uint64-be seq x) ==> seq
           (binary-append-uint64-me seq x) ==> seq
           (binary-ref-uint64-le seq offset) ==> integer
           (binary-ref-uint64-be seq offset) ==> integer
           (binary-ref-uint64-me seq offset) ==> integer
           (binary-set-uint64-le seq offset x) ==> seq
           (binary-set-uint64-be seq offset x) ==> seq
           (binary-set-uint64-me seq offset x) ==> seq

Parameters: seq - binary
            x - integer in range, according to function requirement
            offset - fixnum, offset in binary object

Functions encode/decode integers within binary objects, according to specified
endianess:
  le - little endian
  be - big endian
  me - machine native endian

Append functions add integer to end of binary. Refer and set functions
decode/encode integers at specified offset."))

  (set-symbol-function-doc 'binary-append-int8 d)
  (set-symbol-function-doc 'binary-ref-int8 d)
  (set-symbol-function-doc 'binary-append-uint8 d)
  (set-symbol-function-doc 'binary-ref-uint8 d)
  (set-symbol-function-doc 'binary-set-int8 d)
  (set-symbol-function-doc 'binary-set-uint8 d)
  (set-symbol-function-doc 'binary-append-int16-le d)
  (set-symbol-function-doc 'binary-append-int16-be d)
  (set-symbol-function-doc 'binary-append-int16-me d)
  (set-symbol-function-doc 'binary-ref-int16-le d)
  (set-symbol-function-doc 'binary-ref-int16-be d)
  (set-symbol-function-doc 'binary-ref-int16-me d)
  (set-symbol-function-doc 'binary-set-int16-le d)
  (set-symbol-function-doc 'binary-set-int16-be d)
  (set-symbol-function-doc 'binary-set-int16-me d)
  (set-symbol-function-doc 'binary-append-uint16-le d)
  (set-symbol-function-doc 'binary-append-uint16-be d)
  (set-symbol-function-doc 'binary-append-uint16-me d)
  (set-symbol-function-doc 'binary-ref-uint16-le d)
  (set-symbol-function-doc 'binary-ref-uint16-be d)
  (set-symbol-function-doc 'binary-ref-uint16-me d)
  (set-symbol-function-doc 'binary-set-uint16-le d)
  (set-symbol-function-doc 'binary-set-uint16-be d)
  (set-symbol-function-doc 'binary-set-uint16-me d)
  (set-symbol-function-doc 'binary-append-int32-le d)
  (set-symbol-function-doc 'binary-append-int32-be d)
  (set-symbol-function-doc 'binary-append-int32-me d)
  (set-symbol-function-doc 'binary-ref-int32-le d)
  (set-symbol-function-doc 'binary-ref-int32-be d)
  (set-symbol-function-doc 'binary-ref-int32-me d)
  (set-symbol-function-doc 'binary-set-int32-le d)
  (set-symbol-function-doc 'binary-set-int32-be d)
  (set-symbol-function-doc 'binary-set-int32-me d)
  (set-symbol-function-doc 'binary-append-uint32-le d)
  (set-symbol-function-doc 'binary-append-uint32-be d)
  (set-symbol-function-doc 'binary-append-uint32-me d)
  (set-symbol-function-doc 'binary-ref-uint32-le d)
  (set-symbol-function-doc 'binary-ref-uint32-be d)
  (set-symbol-function-doc 'binary-ref-uint32-me d)
  (set-symbol-function-doc 'binary-set-uint32-le d)
  (set-symbol-function-doc 'binary-set-uint32-be d)
  (set-symbol-function-doc 'binary-set-uint32-me d)
  (set-symbol-function-doc 'binary-append-int64-le d)
  (set-symbol-function-doc 'binary-append-int64-be d)
  (set-symbol-function-doc 'binary-append-int64-me d)
  (set-symbol-function-doc 'binary-ref-int64-le d)
  (set-symbol-function-doc 'binary-ref-int64-be d)
  (set-symbol-function-doc 'binary-ref-int64-me d)
  (set-symbol-function-doc 'binary-set-int64-le d)
  (set-symbol-function-doc 'binary-set-int64-be d)
  (set-symbol-function-doc 'binary-set-int64-me d)
  (set-symbol-function-doc 'binary-append-uint64-le d)
  (set-symbol-function-doc 'binary-append-uint64-be d)
  (set-symbol-function-doc 'binary-append-uint64-me d)
  (set-symbol-function-doc 'binary-ref-uint64-le d)
  (set-symbol-function-doc 'binary-ref-uint64-be d)
  (set-symbol-function-doc 'binary-ref-uint64-me d)
  (set-symbol-function-doc 'binary-set-uint64-le d)
  (set-symbol-function-doc 'binary-set-uint64-be d)
  (set-symbol-function-doc 'binary-set-uint64-me d))

;;--------------------------------------------------------------------------
;; binary-append-object
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-append-object
  "Function: (binary-append-object seq x) ==> seq

Parameters: seq - binary
            x - object: number, symbol, binary, Unicode string, vector or cons.

Converts object to binary machine-independed form and appends it to
binary. Object letter can be restored using 'binary-decode-object' function.")

;(******************************)
;(probably multiple value support is required)
;;--------------------------------------------------------------------------
;; binary-decode-object
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'binary-decode-object
  "Function: (binary-decode-object seq offset) ==> object

Parameters: seq - binary
            offset - fixnum, offset where object resides.

Restores object from binary machine-independed form, previosly encoded with
'binary-append-object' function.")

;;--------------------------------------------------------------------------
;; vectorp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vectorp
  "Function: (vectorp x) ==> bool

Parameters: x - object

Returns true if object is vector.")

;;--------------------------------------------------------------------------
;; vector
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector
  "Function: (vector &rest args) ==> vector

Parameters: args - objects.

Creates new vector object and populates it with given objects.")

;;--------------------------------------------------------------------------
;; vector-create
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-create
  "Function: (vector-create size &optional init-element max-capacity) ==> binary

Parameters: size - non negative fixnum
            init-element - object
            max-capacity - non negative fixnum

Creates new vector of given initial size and maximal capacity and fills it with
given initial element.")

;;--------------------------------------------------------------------------
;; vector-length
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-length
  "Function: (vector-length seq) ==> fixnum

Parameters: seq - vector

Returns length of vector.")

;;--------------------------------------------------------------------------
;; vector-ref
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-ref
  "Function: (vector-ref seq pos) ==> object

Parameters: seq - vector
            pos - non negative fixnum

Returns element at 'pos' position of vector.")

;;--------------------------------------------------------------------------
;; vector-set
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-set
  "Function: (vector-set! seq pos val) ==> val

Parameters: seq - vector
            pos - non negative fixnum
            val - object

Sets value of nth element of vector to specified object.")

;;--------------------------------------------------------------------------
;; vector-append
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-append
  "Function: (vector-append seq x) ==> seq

Parameters: seq - vector
            x - object

Appends object to end of vector.")

;;--------------------------------------------------------------------------
;; vector-append-list
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-append-list
  "Function: (vector-append-list dest src &optional count) ==> dest

Parameters: dest - destination vector
            src - source list
            count - non negative fixnum. Number of elements from source to add
              to destination. Whole list is added, if ommited.

Appends list to end of vector.")

;;--------------------------------------------------------------------------
;; vector-append-vector
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-append-vector
  "Function: (vector-append-vector dest src &optional (src-offset 0)
           (src-len (- (vector-length src) src-offset))) ==> dest

Parameters: dest - destination vector
            src - source vector
            src-offset - fixnum. Offset within source.
            src-len - number of elements in source to add to destination. If not
              specified, than whole source, starting from src-offset is added.

Appends vector to end of another vector.")

;;--------------------------------------------------------------------------
;; vector-ensure-capacity
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-ensure-capacity
  "Function: (vector-ensure-capacity seq delta) ==> seq

Parameters: seq - vector
            delta - capacity delta, non negative fixnum

Ensures that 'delta' objects may be added to vector without storage
rellocations.")

;;--------------------------------------------------------------------------
;; vector-clear
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-clear
  "Function: (vector-clear seq) ==> seq

Parameters: seq - vector

Erases all elements in vector.")

;;--------------------------------------------------------------------------
;; vector-remove
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-remove
  "Function: (vector-remove seq start &optional count) ==> seq

Parameters: seq - vector
            start - start position, non negative fixnum.
            count - number of elements to remove, non negative fixnum. If count
            is nil, then vector is truncated from 'start' position.

Removes 'count' elements in vector starting from 'start' position.")

;;--------------------------------------------------------------------------
;; vector-insert
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-insert
  "Function: (vector-insert seq pos x) ==> seq

Parameters: seq - vector
            pos - position to insert, non negative fixnum.
            x - object

Inserts object into vector at given position.")

;;--------------------------------------------------------------------------
;; vector-insert-list
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-insert-list
  "Function: (vector-insert-list dest dest-pos src &optional count) ==> dest

Parameters: dest - destination vector
            dest-pos - position to insert, non negative fixnum.
            src - source list
            count - non negative fixnum. Number of elements from source to add
              to destination.

Inserts 'count' elements from list into vector at given position.")

;;--------------------------------------------------------------------------
;; vector-insert-vector
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-insert-vector
  "Function: (vector-insert-vector dest dest-pos src &key (src-offset 0) src-len) ==> dest

Parameters: dest - destination vector
            dest-pos - position to insert, non negative fixnum.
            src - source vector
            src-offset - fixnum. Offset within source.
            src-len - number of elements in source to add to destination. If nil
              is passed, than whole source, starting from src-offset is added.

Inserts vector into another vector at given position.")

;;--------------------------------------------------------------------------
;; vector-reverse
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-reverse
  "Function: (vector-reverse seq &key (start 0) len) ==> seq

Parameters: seq - vector
            start - non negative fixnum. Start position to reverse
            len - non negative fixnum. Length of reversed sequence within
              original one.

Reverses vector in place.")

;;--------------------------------------------------------------------------
;; vector-set-max-capacity
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-set-max-capacity
  "Function: (vector-set-max-capacity seq capacity) ==> seq

Parameters: seq - vector
            capacity - non negative fixnum. Must be greater or equal current
              vector length.

Limits maximal capacity of vector to given value.")

;;--------------------------------------------------------------------------
;; vector-find
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-find
  "Function: (vector-find seq obj &key (start 0) (test #'eql)) ==> fixnum or nil

Parameters: seq - vector
            obj - object
            start - fixnum
            test - function: (lambda x y) ==> bool. Returns true, if x is equal
              to y.

Searches vector for object from 'start' position. Return position or nil, if
object is not found.")

;;--------------------------------------------------------------------------
;; vector-concat
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-concat
  "Function: (vector-concat seq &rest args) ==> seq

Parameters: seq - vector
            args - objects

Appends all arguments to vector.")

;;--------------------------------------------------------------------------
;; vector-to-list
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-to-list
  "Function: (vector-to-list seq &key (start 0) count) ==> list

Parameters: seq - vector
            start - fixnum. Start offset within object.
            count - number of vector to put in list.

Converts vector to list.")

;;--------------------------------------------------------------------------
;; vector-sort
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-sort
  "Function: (vector-sort seq &key (start 0) count
                       (test (lambda (x y) (minusp (compare x y))))) ==> seq

Parameters: seq - vector
            start - fixnum. Start offset within object.
            count - number of vector elements to sort.
            test - function: (lambda x y) ==> bool. Returns true if x should be
              placed before y.

Sorts vector in place.")

;;--------------------------------------------------------------------------
;; vector-stable-sort
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'vector-stable-sort
  "Function: (vector-stable-sort seq &key (start 0) count
                             (test (lambda (x y) (minusp (compare x y))))) ==> seq

Parameters: seq - vector
            start - fixnum. Start offset within object.
            count - number of vector elements to sort.
            test - function: (lambda x y) ==> bool. Returns true if x should be
              placed before y.

Sorts vector in place and preserves the relative order of the elements with
equivalent values.")

;;--------------------------------------------------------------------------
;; null
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'null
  "Function: (null x) ==> bool

Parameters: x - object

Returns true, if x is nil.")

;;--------------------------------------------------------------------------
;; consp
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'consp
  "Function: (consp x) ==> bool

Parameters: x - object

Returns true if object is cons.")

;;--------------------------------------------------------------------------
;; list
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'list
  "Function: (list &rest args) ==> list

Parameters: args - objects

Constructs fresh proper list from its arguments.")

;;--------------------------------------------------------------------------
;; cons
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'cons
  "Function: (cons x y) ==> pair

Parameters: x - object
            y - object

Constructs cons object, which consists of objects x and y.")

;;--------------------------------------------------------------------------
;; car
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'car
  "Function: (car x) ==> object

Parameters: x - pair

Returns car cell of cons.")

;;--------------------------------------------------------------------------
;; cdr
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'cdr
  "Function: (cdr x) ==> object

Parameters: x - pair

Returns cdr cell of cons.")

;;--------------------------------------------------------------------------
;; set-car
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'set-car
  "Function: (set-car x v) ==> v

Parameters: x - cons
            v - object

Sets car cell of cons.")

;;--------------------------------------------------------------------------
;; set-cdr
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'set-cdr
  "Function: (set-cdr x v) ==> v

Parameters: x - cons
            v - object

Sets cdr cell of cons.")

;;--------------------------------------------------------------------------
;; Compositions of car and cdr
;;--------------------------------------------------------------------------
(let ((s "K-Lisp provides implementation for following compositions of car and cdr
functions:
  (caar x)        (car (car x))
  (cadr x)        (car (cdr x))
  (cdar x)        (cdr (car x))
  (cddr x)        (cdr (cdr x))
  (caaar x)       (car (car (car x)))
  (caadr x)       (car (car (cdr x)))
  (cadar x)       (car (cdr (car x)))
  (caddr x)       (car (cdr (cdr x)))
  (cdaar x)       (cdr (car (car x)))
  (cdadr x)       (cdr (car (cdr x)))
  (cddar x)       (cdr (cdr (car x)))
  (cdddr x)       (cdr (cdr (cdr x)))
  (caaaar x)      (car (car (car (car x))))
  (caaadr x)      (car (car (car (cdr x))))
  (caadar x)      (car (car (cdr (car x))))
  (caaddr x)      (car (car (cdr (cdr x))))
  (cadaar x)      (car (cdr (car (car x))))
  (cadadr x)      (car (cdr (car (cdr x))))
  (caddar x)      (car (cdr (cdr (car x))))
  (cadddr x)      (car (cdr (cdr (cdr x))))
  (cdaaar x)      (cdr (car (car (car x))))
  (cdaadr x)      (cdr (car (car (cdr x))))
  (cdadar x)      (cdr (car (cdr (car x))))
  (cdaddr x)      (cdr (car (cdr (cdr x))))
  (cddaar x)      (cdr (cdr (car (car x))))
  (cddadr x)      (cdr (cdr (car (cdr x))))
  (cdddar x)      (cdr (cdr (cdr (car x))))
  (cddddr x)      (cdr (cdr (cdr (cdr x))))"))

  (set-symbol-function-doc 'caar s)
  (set-symbol-function-doc 'cadr s)
  (set-symbol-function-doc 'cdar s)
  (set-symbol-function-doc 'cddr s)
  (set-symbol-function-doc 'caaar s)
  (set-symbol-function-doc 'caadr s)
  (set-symbol-function-doc 'cadar s)
  (set-symbol-function-doc 'caddr s)
  (set-symbol-function-doc 'cdaar s)
  (set-symbol-function-doc 'cdadr s)
  (set-symbol-function-doc 'cddar s)
  (set-symbol-function-doc 'cdddr s)
  (set-symbol-function-doc 'caaaar s)
  (set-symbol-function-doc 'caaadr s)
  (set-symbol-function-doc 'caadar s)
  (set-symbol-function-doc 'caaddr s)
  (set-symbol-function-doc 'cadaar s)
  (set-symbol-function-doc 'cadadr s)
  (set-symbol-function-doc 'caddar s)
  (set-symbol-function-doc 'cadddr s)
  (set-symbol-function-doc 'cdaaar s)
  (set-symbol-function-doc 'cdaadr s)
  (set-symbol-function-doc 'cdadar s)
  (set-symbol-function-doc 'cdaddr s)
  (set-symbol-function-doc 'cddaar s)
  (set-symbol-function-doc 'cddadr s)
  (set-symbol-function-doc 'cdddar s)
  (set-symbol-function-doc 'cddddr s))

;;--------------------------------------------------------------------------
;; length
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'length
  "Function: (length seq) ==> integer

Parameters: seq - proper list

Returns length of proper list. If list contains loops, then error is signaled.")

;;--------------------------------------------------------------------------
;; copy-list
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'copy-list
  "Function: (copy-list x) ==> list

Parameters: x - list

Returns 'shallow' (non-recursive) copy of list.")

;;--------------------------------------------------------------------------
;; nreverse
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'nreverse
  "Function: (nreverse seq) ==> list

Parameters: seq - list

Destructivly reverses list in place.")

;;--------------------------------------------------------------------------
;; reverse
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'reverse
  "Function: (reverse seq) ==> list

Parameters: seq - list

Returns reverse non-recursive copy of list.")

;;--------------------------------------------------------------------------
;; append
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'append
  "Function: (append &rest args) ==> list

Parameters: args - lists, but last argument may be atom

Append returns a new list that is the concatenation of the copies. Original
lists are left unchanged; the list structure of each of lists except the last
is copied. The last argument is not copied; it becomes the cdr of the final
dotted pair of the concatenation of the preceding lists, or is returned
directly if there are no preceding non-empty lists.")

;;--------------------------------------------------------------------------
;; mapcar
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'mapcar
  "Function: (mapcar fn &rest args) ==> list

Parameters: fn - function taking as many arguments as there are 'lists'.
            args - lists. All lists must be of same length.

Mapcar applies 'fn' element-wise to the elements of the lists and returns a list
of the results, in order from left to right.")

;;--------------------------------------------------------------------------
;; *stdin-file*
;;--------------------------------------------------------------------------
(set-symbol-value-doc '*stdin-file*
  "Variable: *stdin-file*
Low level file object assotiated with OS standard input file descriptor.")

;;--------------------------------------------------------------------------
;; *stdout-file*
;;--------------------------------------------------------------------------
(set-symbol-value-doc '*stdout-file*
  "Variable: *stdout-file*
Low level file object assotiated with OS standard output file descriptor.")

;;--------------------------------------------------------------------------
;; *stderr-file*
;;--------------------------------------------------------------------------
(set-symbol-value-doc '*stderr-file*
  "Variable: *stderr-file*
Low level file object assotiated with OS standard error file descriptor.")

;;--------------------------------------------------------------------------
;; file-open
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-open
  "Function: (file-open pathname flags &optional mode) ==> file

Parameters: pathname - string
            flags - integer, bitwise composition of flags, same as in 'open(2)'
              system call. Function 'file-open-flags' returns list of
              predefined symbolic constants for flags.
            mode - integer, bitwise composition of flags, same as in 'open(2)'
              system call. Function 'file-open-modes' returns list of
              predefined symbolic constants for modes.

The function is wrapper for 'open(2)' system call. Returns file. O_LARGEFILE
flags is always turned on by 'file-open' function.")

;;--------------------------------------------------------------------------
;; file-open-flags
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-open-flags
  "Function: (file-open-flags) ==> list

Returns list of predefined symbolic constans for flags parameter of 'file-open'
function, like: O_RDONLY, O_WRONLY, O_RDWR, O_APPEND, O_CREAT, O_CLOEXEC,
O_DIRECT, O_DIRECTORY, O_EXCL, O_NOATIME, O_NOCTTY, O_NOFOLLOW, O_NONBLOCK,
O_SYNC, O_TRUNC etc.")

;;--------------------------------------------------------------------------
;; file-open-modes
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-open-modes
  "Function: (file-open-modes) ==> list

Returns list of predefined symbolic constans for mode parameter of 'file-open'
function, like: S_IRWXU, S_IRUSR, S_IWUSR, S_IXUSR, S_IRWXG, S_IRGRP, S_IWGRP,
S_IXGRP, S_IRWXO, S_IROTH, S_IWOTH, S_IXOTH etc.")

;;--------------------------------------------------------------------------
;; file-close
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-close
  "Function: (file-close file) ==> nil

Parameters: file - file

The function is wrapper for 'close(2)' system call.")

;;--------------------------------------------------------------------------
;; file-write
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-write
  "Function: (file-write file buffer &key (offset 0) count) => integer or 'EAGAIN

Parameters: file -file
            buffer - binary.
            offset - fixnum. Offset within buffer.
            count - fixnum. Number of bytes in buffer to write to file. If nil
              is passed, than whole buffer, starting from offset is written.

The function is wrapper for 'write(2)' system call. Returns number of bytes
written to file. If file is in non-nblocking mode and write failed with EAGAIN
or EWOULDBLOCK error, then EAGAIN symbol is returned. If number of bytes to
write to file is greater than zero and no bytes are written, then
('zero-bytes-written count) signal is raised.")

;;--------------------------------------------------------------------------
;; file-read
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-read
  "Function: (file-read file buffer count) => integer or 'EAGAIN or nil

Parameters: file -file
            buffer - binary. Where to store read bytes. Read bytes are added to
               end of buffer.
            count - fixnum. Number of bytes to read from file.

The function is wrapper for 'read(2)' system call. Returns number of bytes
read from file. If file is in non-nblocking mode and read failed with EAGAIN
or EWOULDBLOCK error, then EAGAIN symbol is returned. If number of bytes to
read to file is greater than zero and no bytes are read, then nil is returned.")

;;--------------------------------------------------------------------------
;; file-pwrite
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-pwrite
  "Function: (file-write file buffer f-offset &key (b-offset 0) count) => integer or 'EAGAIN

Parameters: file -file
            buffer - binary.
            f-offset - integer. File offset from the beginning of the file.
            b-offset - fixnum. Offset within buffer.
            count - fixnum. Number of bytes in buffer to write to file. If nil
              is passed, than whole buffer, starting from offset is written.

The function is wrapper for 'pwrite(2)' system call. Returns number of bytes
written to file. If file is in non-nblocking mode and write failed with EAGAIN
or EWOULDBLOCK error, then EAGAIN symbol is returned. If number of bytes to
write to file is greater than zero and no bytes are written, then
('zero-bytes-written count) signal is raised.")

;;--------------------------------------------------------------------------
;; file-pread
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-pread
  "Function: (file-pread file buffer count offset) => integer or 'EAGAIN or nil

Parameters: file -file
            buffer - binary. Where to store read bytes. Read bytes are added to
               end of buffer.
            count - fixnum. Number of bytes to read from file.
            offset - integer. File offset from the beginning of the file.

The function is wrapper for 'pread(2)' system call. Returns number of bytes
read from file. If file is in non-nblocking mode and read failed with EAGAIN
or EWOULDBLOCK error, then EAGAIN symbol is returned. If number of bytes to
read to file is greater than zero and no bytes are read, then nil is returned.")

;;--------------------------------------------------------------------------
;; file-seek
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-seek
  "Function: (file-seek file offset whence) ==> integer

Parameters: file - file
            offset - integer. File offset according to directive whence as
              following:
                SEEK_SET - the offset is set to offset bytes.
                SEEK_CUR - the offset is set to its current location plus offset
                  bytes.
                SEEK_END - the offset is set to the size of the file plus offset bytes.
            whence - symbol.

The function is wrapper for 'lseek(2)' system call. Sets file read/write
position. Returns the resulting offset location as measured in bytes from the
beginning of the file.")

;;--------------------------------------------------------------------------
;; file-sync
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-sync
  "Function: (file-sync file) ==> file

Parameters: file - file

The function is wrapper for 'fsync(2)' system call.")

;;--------------------------------------------------------------------------
;; file-datasync
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-datasync
  "Function: (file-datasync file) ==> file

Parameters: file - file

The function is wrapper for 'fdatasync(2)' system call.")

;;--------------------------------------------------------------------------
;; file-access
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-access
  "Function: (file-access pathname mode) ==> bool

Parameters: pathname - string

            mode - integer, either F_OK or bitwise composition of flags (W_OK,
              R_OK, X_OK), same as in 'access(2)' system call. Function
              'file-access-modes' returns list of predefined symbolic constants
              for modes.

The function is wrapper for 'access(2)' system call.")

;;--------------------------------------------------------------------------
;; file-access-modes
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'file-access-modes
  "Function: (file-access-modes) ==> list

Returns list of predefined symbolic constans for flags parameter of 'file-access'
function, like: F_OK, W_OK, R_OK, X_OK.")

;;--------------------------------------------------------------------------
;; *argv*
;;--------------------------------------------------------------------------
(set-symbol-value-doc '*argv*
  "Variable: *argv*
Stores program arguments.")

;;--------------------------------------------------------------------------
;; getenv
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'getenv
  "Function: (getenv name &optional default) ==> string or default

Parameters: name - string
            default - object

The function is wrapper for C 'getenv' function. If environment variable is not
present, then 'default' parameter is returned.")

;;--------------------------------------------------------------------------
;; setenv
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'setenv
  "Function: (setenv name value overwrite) ==> name

Parameters: name - string
            value - string
            overwrite - boolean

The function is wrapper for C 'setenv' function.")

;;--------------------------------------------------------------------------
;; unsetenv
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'unsetenv
  "Function: (unsetenv name) ==> name

Parameters: name - string

The function is wrapper for C 'unsetenv' function.")

;;--------------------------------------------------------------------------
;; clearenv
;;--------------------------------------------------------------------------
(set-symbol-function-doc ' clearenv
  "Function: (clearenv) ==> nil

Parameters: none

The function is wrapper for C 'clearenv' function.")
