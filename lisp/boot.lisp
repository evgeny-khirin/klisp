;;-------------------------------------------------------------------
;; Copyright (c) 2009-2012 by Evgeny Khirin.
;;-------------------------------------------------------------------
;;-------------------------------------------------------------------
;; File    : boot.lisp
;; Author  : Evgeny Khirin <>
;; Description : Initializes Lisp
;;-------------------------------------------------------------------

;;--------------------------------------------------------------------------
;; set package
;;--------------------------------------------------------------------------
(export 'in-package)
(def-global-fun 'in-package
    (macro (name)
      (list 'setq '*package* (list 'kl::find-create-package (list 'quote name)))))

;;--------------------------------------------------------------------------
;; set package
;;--------------------------------------------------------------------------
(in-package kl)

;;--------------------------------------------------------------------------
;; (%defvar sym val)
;; Bootstrap version of defvar macro
;;--------------------------------------------------------------------------
(def-global-fun '%defvar
    (macro (sym val)
      (setq sym (list 'quote sym))
      (list 'if (list 'symbol-boundp sym)
            sym
            (list 'def-global-var sym val))))

;;--------------------------------------------------------------------------
;; (set-symbol-function-doc sym doc) ==> sym
;;--------------------------------------------------------------------------
(%defvar *symbol-function-doc-marker* '#:symbol-function-doc-marker)

(def-global-fun 'set-symbol-function-doc
    (lambda (sym doc)
      (if (null doc)
          (symbol-rm-prop sym *symbol-function-doc-marker*)
          (symbol-set-prop sym *symbol-function-doc-marker* doc))
      sym))

;;--------------------------------------------------------------------------
;; (symbol-function-doc sym)
;;--------------------------------------------------------------------------
(def-global-fun 'symbol-function-doc
    (lambda (sym)
      (symbol-get-prop sym *symbol-function-doc-marker*)))

;;--------------------------------------------------------------------------
;; (set-symbol-value-doc sym doc)
;;--------------------------------------------------------------------------
(%defvar *symbol-value-doc-marker* '#:symbol-value-doc-marker)

(def-global-fun 'set-symbol-value-doc
    (lambda (sym doc)
      (if (null doc)
          (symbol-rm-prop sym *symbol-value-doc-marker*)
          (symbol-set-prop sym *symbol-value-doc-marker* doc))
      sym))

;;--------------------------------------------------------------------------
;; (symbol-value-doc sym)
;;--------------------------------------------------------------------------
(def-global-fun 'symbol-value-doc
    (lambda (sym)
      (symbol-get-prop sym *symbol-value-doc-marker*)))

;;--------------------------------------------------------------------------
;; (set-symbol-struct-doc sym doc)
;;--------------------------------------------------------------------------
(%defvar *symbol-struct-doc-marker* '#:symbol-struct-doc-marker)

(def-global-fun 'set-symbol-struct-doc
    (lambda (sym doc)
      (if (null doc)
          (symbol-rm-prop sym *symbol-struct-doc-marker*)
          (symbol-set-prop sym *symbol-struct-doc-marker* doc))
      sym))

;;--------------------------------------------------------------------------
;; (symbol-struct-doc sym)
;;--------------------------------------------------------------------------
(def-global-fun 'symbol-struct-doc
    (lambda (sym)
      (symbol-get-prop sym *symbol-struct-doc-marker*)))

;;--------------------------------------------------------------------------
;;
;;--------------------------------------------------------------------------
(load (binary (dirname *load-pathname*) "/doc.lisp"))

;;--------------------------------------------------------------------------
;; Macro: (and . args) ==> bool
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'and
  "Macro: (and &rest args) ==> bool

Parameters: args - objects

Returns value of its last argument, if all previous arguments are not
evaluated to nil. Otherwise, nil is returned. If no arguments are provided t
is returned.")

(export 'and)
(def-global-fun 'and
    (macro (&rest args)
      (if (null args)
          t
          (if (null (cdr args))
              (car args)
              (list 'if (car args) (cons 'and (cdr args)) nil)))))

;;--------------------------------------------------------------------------
;; Function: (atom x) ==> bool
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'atom
  "Function: (atom x) ==> bool

Parameters: x - object

Returns true if x is not cons.")

(export 'atom)
(def-global-fun 'atom
    (lambda (x) (not (consp x))))

;;--------------------------------------------------------------------------
;; Macro: (cond . clauses) ==> object
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'cond
  "Macro: (cond &rest clauses) ==> object

Parameters: clauses - list of (test-form form+)
            test-form - form or t
            form+ - implicit progn

cond allows the execution of forms to be dependent on test-form.

Test-forms are evaluated one at a time in the order in which they are given in
the argument list until a test-form is found that evaluates to true.  Then the
forms associated with this test-form are evaluated in order, left to right, as
an implicit progn, and the value returned by the last form are returned by the
cond form.

Once one test-form has yielded true, no additional test-forms are
evaluated. If no test-form yields true, exception is raised.")

(export 'cond)
(def-global-fun 'cond
    ;; The macro loaded and executed on early boot stages, so it is written using
    ;; only primitive funactions and functions defined above.
    (macro (&rest clauses)
      (if (null clauses)
          '(signal 'no-clause-match)
          (if (eq t (caar clauses))
              (cons 'progn (cdar clauses))
              (list 'if (caar clauses)
                    (cons 'progn (cdar clauses))
                    (cons 'cond (cdr clauses)))))))

;;--------------------------------------------------------------------------
;; quasiquote
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'quasiquote
  "Quasiquote in Lisp is handled with macros as in Scheme, but beahaves same
as backquote in Common Lisp. For example, following will fail in Scheme:
  > (define x '(a b c))
  > ``(,,@x)
But in Lisp it will be expanded into (list a b c), that corresponds Common
Lisp semantics.

When reader encouners quasiquote \"`x\" (backquote), it expands it into
(quasiquote x).

When reader encouners comma-at \",@x\" as two first characters of symbol name,
it expands it into (unquote-splicing x).

When reader encouners comma \",x\" as first character of symbol name, it
expands it into (unquote x).")

(export 'quasiquote)
(def-global-fun 'quasiquote
    (macro (x)
      (labels ((qq-expand-list (x)
                 ;; builds arguments list for list function
                 (cond ((null x) nil)
                       ((atom x) nil)
                       ((eq (car x) 'unquote) nil) ; dotted list: will be
                                        ; handled by qq-expand-list-2 with append
                                        ; function
                       ((eq (car x) 'unquote-splicing) nil) ; dotted
                                        ; list: will be handled by qq-expand-list-2
                                        ; with append function
                       ((and (consp (car x))
                             (eq (caar x) 'unquote-splicing))
                        nil)            ; unquote-splicing will be handled
                                        ; be qq-expand-list-2 with append
                                        ; function
                       (t (cons (qq-expand (car x))
                                (qq-expand-list (cdr x))))))
               (qq-expand-list-2 (prev x)
                 ;; skips everything expanded with qq-expand-list and expands
                 ;; the rest wtih append function
                 (cond ((null x) prev)
                       ((atom x) (list 'append prev (qq-expand x)))
                       ((eq (car x) 'unquote) (list 'append prev (cadr x)))
                       ((eq (car x) 'unquote-splicing) (list 'append prev (cadr x)))
                       ((and (consp (car x))
                             (eq (caar x) 'unquote-splicing))
                        (if (null (cdr x))
                            (list 'append prev (cadar x))
                            (list 'append prev (cadar x) (qq-expand (cdr x)))))
                       (t (qq-expand-list-2 prev (cdr x)))))
               (qq-expand (x)
                 (cond ((vectorp x)
                        (cons 'vector (qq-expand (vector-to-list x))))
                       ((consp x)
                        (cond ((eq (car x) 'quasiquote) (qq-expand (qq-expand (cadr x))))
                              ((eq (car x) 'unquote) (cadr x))
                              ((eq (car x) 'unquote-splicing)
                               (signal 'comma-at-not-in-list x))
                              (t (qq-expand-list-2 (cons 'list (qq-expand-list x)) x))))
                       (t (list 'quote x)))))
        (qq-expand x))))

(export 'unquote)
(def-global-fun 'unquote
    (macro (x) (signal 'comma-outside-of-backquote x)))

(export 'unquote-splicing)
(def-global-fun 'unquote-splicing
    (macro (x) (signal 'comma-at-outside-of-backquote x)))

;;--------------------------------------------------------------------------
;; extract-declarations
;; splits function body into declarations and body
;;--------------------------------------------------------------------------
(def-global-fun 'extract-declarations
    (lambda (body)
      (block extract-declarations
        (let (decls)
          (tagbody
           again
             (if (and (consp (car body)) (eq 'declare (caar body)))
                 (progn
                   (setq decls (cons (car body) decls))
                   (setq body (cdr body))
                   (go again))
                 (return-from extract-declarations (values (nreverse decls) body))))))))

;;--------------------------------------------------------------------------
;; defmacro
;;--------------------------------------------------------------------------
(set-symbol-function-doc 'defmacro
  "Macro: (defmacro name args &rest body) ==> name

Parameters: name - symbol
            args - lambda-list
            body - list. Macro body.

Defines global, named macro. Returns name of macro. If first form of body is
binary and rest of body is not nil, then this binary is stored in doc slot of
name symbol.")

(export 'defmacro)
(def-global-fun 'defmacro
    (macro (name args &rest body)
      (if (and (binaryp (car body)) (not (null (cdr body))))
          ;; Macro with documentation string
          (multiple-value-bind
                (decls pure-body) (extract-declarations (cdr body))
            `(progn
               (undefine-compiler-macro ',name)
               (def-global-fun (set-symbol-function-doc ',name ,(car body))
                   (macro ,args
                     (declare (global-function-name ,name))
                     ,@decls
                     (block ,name ,@pure-body)))))
          ;; Macro without documentation string
          (multiple-value-bind
                (decls pure-body) (extract-declarations body)
            `(progn
               (undefine-compiler-macro ',name)
               (def-global-fun (set-symbol-function-doc ',name nil)
                   (macro ,args
                     (declare (global-function-name ,name))
                     ,@decls
                     (block ,name ,@pure-body))))))))

;;--------------------------------------------------------------------------
;; defun
;;--------------------------------------------------------------------------
(export 'defun)
(defmacro defun (name args &rest body)
  "Macro: (defun name args &rest body) ==> name

Parameters: name - symbol
            args - lambda-list
            body - list. Function body.

Defines global, named function. Returns name of function.  If first form of body
is binary and rest of body is not nil, then this binary is stored in doc slot of
name symbol."

  (if (and (binaryp (car body)) (not (null (cdr body))))
      ;; Function with documentation string
      (multiple-value-bind (decls pure-body) (extract-declarations (cdr body))
        `(progn
           (undefine-compiler-macro ',name)
           (def-global-fun (set-symbol-function-doc ',name ,(car body))
               (lambda ,args
                 (declare (global-function-name ,name))
                 ,@decls
                 (block ,name ,@pure-body)))))
      ;; Function without documentation string
      (multiple-value-bind (decls pure-body) (extract-declarations body)
        `(progn
           (undefine-compiler-macro ',name)
           (def-global-fun (set-symbol-function-doc ',name nil)
               (lambda ,args
                 (declare (global-function-name ,name))
                 ,@decls
                 (block ,name ,@pure-body)))))))

;;--------------------------------------------------------------------------
;; undefun
;;--------------------------------------------------------------------------
(export 'undefun)
(defun undefun (sym)
  "Function: (undefun sym) ==> bool

Parameters: sym - symbol

Undefines global function. Returns true, if function really
undefined. Otherwise, returns nil."

  (if (symbol-fboundp sym)
      (progn
        (undefine-compiler-macro sym)
        (set-symbol-function-doc sym nil)
        (undef-global-fun sym)
        t)))

;;--------------------------------------------------------------------------
;; defvar
;;--------------------------------------------------------------------------
(export 'defvar)
(defmacro defvar (name &rest val-doc)
  "Syntax: (defvar name [val [doc]]) ==> name

Parameters: name - symbol
            val - form
            doc - binary

Defines new global variable. If variable already defined, does nothing. Returns
name of variable."

  (let ((val (if val-doc (car val-doc)))
        (doc (if (and val-doc (cdr val-doc)) (cadr val-doc))))
    `(if (symbol-boundp ',name)
         ',name
         (progn
           (undefine-symbol-macro ',name)
           (set-symbol-value-doc (def-global-var ',name ,val) ,doc)))))

;; Remove bootstrap version of defvar
(undefun '%defvar)

;;--------------------------------------------------------------------------
;; undefvar
;;--------------------------------------------------------------------------
(export 'undefvar)
(defun undefvar (sym)
  "Function: (undefvar sym) ==> bool

Parameters: sym - symbol
            val - form
            doc - binary

Undefines global variable. Returns true, if variable really
undefined. Otherwise, returns nil."

  (if (symbol-boundp sym)
      (progn
        (set-symbol-value-doc sym nil)
        (undef-global-var sym)
        t)))

;;--------------------------------------------------------------------------
;; defparameter
;;--------------------------------------------------------------------------
(export 'defparameter)
(defmacro defparameter (name &rest val-doc)
  "Syntax: (defparameter name [val [doc]]) ==> name

Parameters: name - symbol
            val - form
            doc - binary

Defines global, named variable. If variable already defined, it is redefined.
Returns name of variable."

  (let ((val (if val-doc (car val-doc)))
        (doc (if (and val-doc (cdr val-doc)) (cadr val-doc))))
    `(progn
       (undefine-symbol-macro ',name)
       (def-global-var (set-symbol-value-doc ',name ,doc) ,val))))

;;--------------------------------------------------------------------------
;; defconstant
;;--------------------------------------------------------------------------
(export 'defconstant)
(defmacro defconstant (name val &optional doc)
  "Macro: (defconstant name val &optional doc) ==> name

Parameters: name - symbol
            val - form
            doc - binary

Evaluates val and creates symbol macro named name, which expands into result of
the evaluation. Constants are substituted by their values during compilation.
Returns name."

  `(set-symbol-value-doc (%define-symbol-macro ',name ,val) ,doc))

;;--------------------------------------------------------------------------
;; define-symbol-macro
;;--------------------------------------------------------------------------
(export 'define-symbol-macro)
(defmacro define-symbol-macro (sym expansion)
  "Macro: (define-symbol-macro sym expansion) ==> sym

Parameters: sym - symbol
            expansion - form

Defines global symbol macro - each reference to 'sym' as to dynamic variable is
expanded to 'expansion' form and then evaluated."

  `(%define-symbol-macro ',sym ',expansion))

;;--------------------------------------------------------------------------
;; binary-reverse
;;--------------------------------------------------------------------------
(export 'binary-reverse)
(defun binary-reverse (seq)
  (binary-nreverse (binary seq)))

;;--------------------------------------------------------------------------
;; ustring-reverse
;;--------------------------------------------------------------------------
(defun ustring-reverse (seq)
  (ustring-nreverse (ustring seq)))

;;--------------------------------------------------------------------------
;; vector-reverse
;;--------------------------------------------------------------------------
(defun vector-reverse (seq)
  (vector-nreverse (ustring seq)))

;;--------------------------------------------------------------------------
;; when
;;--------------------------------------------------------------------------
(export 'when)
(defmacro when (cond &rest body)
  "Macro: (when cond statement+)

Parameters: cond - form
            statement - form

Evaluates statements, when condion is evaluated to boolean true and returns
value of last form. Returns nil, if condition is evaluated to boolean false."

  `(if ,cond (progn ,@body)))

;;--------------------------------------------------------------------------
;; unless
;;--------------------------------------------------------------------------
(export 'unless)
(defmacro unless (cond &rest body)
  "Macro: (unless cond statement+)

Parameters: cond - form
            statement - form

Evaluates statements, when condion is evaluated to boolean false and returns
value of last form. Returns nil, if condition is evaluated to boolean true."

  `(if (null ,cond) (progn ,@body)))

;;--------------------------------------------------------------------------
;; push
;;--------------------------------------------------------------------------
(export 'push)
(defmacro push (value place)
  "Macro: (push value place) ==> value

Parameters: value - object
            place - symbol, variable name

Conses 'value' to the value stored in 'place' and makes it the new value of
'place'. Returns new value of 'place'."

  `(setq ,place (cons ,value ,place)))

;;--------------------------------------------------------------------------
;; incf
;;--------------------------------------------------------------------------
(export 'incf)
(defmacro incf (place &optional (delta 1))
  "Macro: (incf place &optional (delta 1)) ==> integer

Parameters: place - symbol, variable name
            delta - integer

Increments value stored in 'place' on given 'delta' and makes it the new value of
'place'. Returns new value of 'place'."
  (if (eq delta 1)
      `(setq ,place (1+ ,place))
      `(setq ,place (+ ,place ,delta))))

;;--------------------------------------------------------------------------
;; decf
;;--------------------------------------------------------------------------
(export 'decf)
(defmacro decf (place &optional (delta 1))
  "Macro: (decf place &optional (delta 1)) ==> integer

Parameters: place - symbol, variable name
            delta - integer

Decrements value stored in 'place' on given 'delta' and makes it the new value of
'place'. Returns new value of 'place'."
  (if (eq delta 1)
      `(setq ,place (1- ,place))
      `(setq ,place (- ,place ,delta))))

;;--------------------------------------------------------------------------
;; map
;;--------------------------------------------------------------------------
(export 'map)
(defun map (fn lst)
  "Function: (map fn lst) ==> list

Parameters: fn - function, taking one argument
            lst - proper list.

Map is lightweight version of 'mapcar', which accepts single list only. It
applies 'fn' to each element in 'lst' in order and returns, list freshly
constructed from results of the application in same order."

  (let (res)
    (tagbody
     again
       (if lst
           (progn (push (funcall fn (car lst)) res)
                  (setq lst (cdr lst))
                  (go again))
           (go end))
     end)
    (nreverse res)))

;;--------------------------------------------------------------------------
;; or
;;--------------------------------------------------------------------------
(export 'or)
(defmacro or (&rest args)
  "Macro: (or &rest args) ==> bool

Parameters: args - objects

Evaluates each form, one at a time, from left to right. The evaluation of all
forms terminates when a form evaluates to true (i.e., something other than nil).

If the evaluation of any form other than the last returns a value that is true,
'or' immediately returns that value without evaluating the remaining forms. If
every form but the last returns false, 'or' returns value returned by the last
form. If no forms are supplied, 'or' returns nil."

  (labels ((or-loop (var args)
             (if (null args)
                 'nil
                 `(if (setq ,var ,(car args))
                      ,var
                      ,(or-loop var (cdr args))))))
    (if (null args)
        nil
        (let ((var (gensym "res")))
          `(let ((,var ,(car args)))
             (if ,var
                 ,var
                 ,(or-loop var (cdr args))))))))

;;--------------------------------------------------------------------------
;; with-gensyms
;;--------------------------------------------------------------------------
(export 'with-gensyms)
(defmacro with-gensyms (names &rest body)
  "Macro: (with-gensyms names &rest body) ==> list

Parameters: names - list
            body - list of expressions

Binds each variable named by a symbol in 'names' to a unique symbol around
'body'."

  `(let ,(map (lambda (n) `(,n (gensym ,(symbol-name n))))
              names)
     . ,body))

;;--------------------------------------------------------------------------
;; psetq
;;--------------------------------------------------------------------------
(export 'psetq)
(defmacro psetq (&rest args)
  "Macro: (psetq &rest args) ==> object

Parameters: args - pair*
            pair - symbol form

Assigns values to variables. Returns value of last evaluated form.

This is just like setq, except that the assignments happen in parallel. That is,
first all of the forms are evaluated, and only then are the variables set to the
resulting values. In this way, the assignment to one variable does not affect
the value computation of another in the way that would occur with setq's
sequential assignment."

  (labels ((split-by-pairs (l f s)
             ;; Splits list in two parts by pairs. Returns cons of f and s
             ;; lists.
             (if (null l)
                 (cons (nreverse f) (nreverse s))
                 (split-by-pairs (cddr l) (cons (car l) f) (cons (cadr l) s)))))
    (let* ((splited (split-by-pairs args nil nil))
           (vars (car splited))
           (inits (cdr splited))
           (tmp-vars (map (lambda (x) (gensym (symbol-name x))) vars)))
      `(let ,(mapcar #'list tmp-vars inits)
         ,@(mapcar (lambda (v1 v2)
                     `(setq ,v1 ,v2))
                   vars tmp-vars)))))

;;--------------------------------------------------------------------------
;; gen-do-body
;;--------------------------------------------------------------------------
(defun gen-do-body (bindings test-and-result body let-name set-name)
  ;; generates body of do or do* loop.
  ;; for do: let-name is let and set-name is psetq
  ;; for do*: let-name is let* and set-name is setq
  (labels ((collect-do-steps (bindings res)
             ;; produces arguments list for setq or psetq
             (if (null bindings)
                 res
                 (let ((cur-bind (car bindings)))
                   (if (null (cddr cur-bind))
                       ;; there is no step in this binding
                       (collect-do-steps (cdr bindings) res)
                       ;; there is step in this binding
                       (collect-do-steps (cdr bindings)
                                         (append res
                                                 (list (car cur-bind)
                                                       (caddr cur-bind)))))))))
    (let ((variables (map #'car bindings))
          (inits (map #'cadr bindings))
          (steps (collect-do-steps bindings nil))
          (test (car test-and-result))
          (result (cdr test-and-result)))
      (if (null result)
          ;; there is no result form
          (with-gensyms (start end ret)
            `(,let-name ,(append (mapcar #'list variables inits)
                                 `((,ret nil)))
                        (tagbody
                           ,start
                           (if (setq ,ret ,test)
                               (go ,end)
                               (progn ,@body))
                           ,@(if steps
                                 (list (cons set-name steps))
                                 nil)
                           (go ,start)
                           ,end)
                        ,ret))
          ;; result form is supplied
          (if (null (cdr result))
              (let ((result (car result)))
                (with-gensyms (start end)
                  `(,let-name ,(mapcar #'list variables inits)
                              (tagbody
                                 ,start
                                 (if ,test
                                     (go ,end)
                                     (progn ,@body))
                                 ,@(if steps
                                       (list (cons set-name steps))
                                       nil)
                                 (go ,start)
                                 ,end)
                              ,result)))
              (signal 'too-much-result-forms result))))))

;;--------------------------------------------------------------------------
;; do
;;--------------------------------------------------------------------------
(export 'do)
(defmacro do (bindings test-and-result &rest body)
  "Macro: do

Syntax:
  do ({(var init-form [step-form])}*) (end-test-form result-form*) statement*

Arguments and Values:
  var - a symbol.
  init-form - a form.
  step-form - a form.
  end-test-form -a form.
  result-form - a form.
  statement - a compound form; evaluated as described below.

Returns: value of result-form

Description:
do iterates over a group of statements while a test condition holds. do accepts
an arbitrary number of iteration vars which are bound within the iteration and
stepped in parallel. An initial value must be supplied for each iteration
variable by use of an init-form. Step-forms may be used to specify how the vars
should be updated on succeeding iterations through the loop. Step-forms may be
used both to generate successive values or to accumulate results. If the
end-test-form condition is met prior to an execution of the body, the iteration
terminates.

Before the first iteration, all the init-forms are evaluated, and each var is
bound to the value of its respective init-form, if supplied. This is a binding,
not an assignment; when the loop terminates, the old values of those variables
will be restored. For do, all of the init-forms are evaluated before any var is
bound. The init-forms can refer to the bindings of the vars visible before
beginning execution of do.

At the beginning of each iteration, after processing the variables, the
end-test-form is evaluated. If the result is false, execution proceeds with the
body of the do form. If the result is true, the result-form is evaluated, and
then do returns. If result-form is not specified, then value of end-test-form
returned as result of do form.

At the beginning of each iteration other than the first, vars are updated as
follows. All the step-forms, if supplied, are evaluated, from left to right, and
the resulting values are assigned to the respective vars. Any var that has no
associated step-form is not assigned to. All the step-forms are evaluated before
any var is updated; the assignment of values to vars is done in
parallel. Because all of the step-forms are evaluated before any of the vars are
altered, a step-form when evaluated always has access to the old values of all
the vars, even if other step-forms precede it. After the vars have been updated,
the end-test-form is evaluated as described above, and the iteration continues.

Example:
  (do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1- temp-two)))
      ((> (- temp-one temp-two) 5) temp-one)
    (print (list temp-one temp-two))) ==>  4"

  (gen-do-body bindings test-and-result body 'let 'psetq))

;;--------------------------------------------------------------------------
;; do*
;;--------------------------------------------------------------------------
(export 'do*)
(defmacro do* (bindings test-and-result &rest body)
  "Macro: do*

Syntax:
  do* ({(var init-form [step-form])}*) (end-test-form result-form*) statement*

This macro is same as 'do', but bindings and stepping are done sequentially."

  (gen-do-body bindings test-and-result body 'let* 'setq))

;;--------------------------------------------------------------------------
;; dolist
;;--------------------------------------------------------------------------
(export 'dolist)
(defmacro dolist (head &rest body)
  "Macro: (dolist (var list-form [result-form]) body)

Parameters: var - symbol.
            list-form - form.
            result-form - form.
            body - form+.

Dolist iterates over the elements of a list.

Dolist evaluates list-form, which should produce a list. It then evaluates the
body in implicit progn once for each element in the list, with var bound to the
element. If result-form is presented, then dolist returns result of result form,
otherwise it returns nil."

  (let ((var (car head))
        (lst (cadr head))
        (res (cddr head)))
    (if (null res)
        nil
        (setq res (car res)))
    (with-gensyms (l)
      `(do ((,l ,lst (cdr ,l))
            (,var nil))
           ((null ,l) ,res)
         (setq ,var (car ,l))
        . ,body))))

;;--------------------------------------------------------------------------
;; while
;;--------------------------------------------------------------------------
(export 'while)
(defmacro while (cond &rest body)
  "Macro: (while cond statement+) ==> nil

Parameters: cond - form
            statement - form

Evaluates statements in loop while condion is evaluated to boolean
true. Condition is evaluated before each loop iteration."

  (with-gensyms (again)
    `(tagbody
        ,again
        (when ,cond
          ,@body
          (go ,again)))))

;;--------------------------------------------------------------------------
;; find
;;--------------------------------------------------------------------------
(export 'find)
(defun find (item seq &optional (test 'equal))
  "Function: (find item seq &optional (test 'equal)) ==> list

Parameters: item - object
            seq - proper list
            test - (lambda (x y)) ==> bool

Compares item with members of list seq and returns sublist starting with
element, for which test function first time returned true. Or nil if no such
elements."

  (do ((l seq (cdr l)))
      ((null l) nil)
    (if (funcall test item (car l))
        (return-from find l))))

;;--------------------------------------------------------------------------
;; find-if
;;--------------------------------------------------------------------------
(export 'find-if)
(defun find-if (predicate seq)
  "Function: (find-if predicate seq) ==> list

Parameters: predicate - (lambda (x)) ==> bool
            seq - proper list

Returns sublist of seq starting with element, for which predicate function first time
returned true. Or nil if no such elements."

  (do ((l seq (cdr l)))
      ((null l) nil)
    (if (funcall predicate (car l))
        (return-from find-if l))))

;;--------------------------------------------------------------------------
;; find-if-not
;;--------------------------------------------------------------------------
(export 'find-if-not)
(defun find-if-not (predicate seq)
  "Function: (find-if-not predicate seq) ==> list

Parameters: predicate - (lambda (x)) ==> bool
            seq - proper list

Returns sublist of seq starting with element, for which predicate function first time
returned nil. Or nil if no such elements."

  (do ((l seq (cdr l)))
      ((null l) nil)
    (if (not (funcall predicate (car l)))
        (return-from find-if-not l))))

;;--------------------------------------------------------------------------
;; remove-if
;;--------------------------------------------------------------------------
(export 'remove-if)
(defun remove-if (predicate seq)
  "Function: (remove-if predicate seq) ==> proper list

Parameters: predicate - (lambda (x)) ==> bool
            seq - proper list

Returns list, consisting of elements from seq for which predicate function returned false."

  (let (res)
    (dolist (x seq (nreverse res))
      (if (not (funcall predicate x))
          (push x res)))))

;;--------------------------------------------------------------------------
;; remove-if-not
;;--------------------------------------------------------------------------
(export 'remove-if-not)
(defun remove-if-not (predicate seq)
  "Function: (remove-if-not predicate seq) ==> proper list

Parameters: predicate - (lambda (x)) ==> bool
            seq - proper list

Returns list, consisting of elements from seq for which predicate function returned true."

  (let (res)
    (dolist (x seq (nreverse res))
      (if (funcall predicate x)
          (push x res)))))

;;--------------------------------------------------------------------------
;; const-literalp
;; x should be macroexpanded
;;--------------------------------------------------------------------------
(defun const-literalp (x)
  (cond ((atom x) (not (symbolp x)))
        (t (eq (car x) 'quote))))

;;--------------------------------------------------------------------------
;; function-callp
;; x should be macroexpanded
;;--------------------------------------------------------------------------
(defun function-callp (x)
  (and (consp x) (not (eq (car x) 'quote))))

;;--------------------------------------------------------------------------
;; compiler macro: =
;;--------------------------------------------------------------------------
(macrolet
  ((gen-macro (op)
     (let ((priv-op (intern (binary #"%" (symbol-name op)))))
       `(define-compiler-macro ',op
         (macro (num &rest more)
           (let ((env (macro-env)))
             (setq num (macroexpand num env))
             (setq more (map (lambda (x) (macroexpand x env)) more))
             (cond ((null more) t)             ; one number is given
                   ((null (cdr more))          ; two numbers are given
                    (if (and (const-literalp num) (const-literalp (car more)))
                        (funcall ',op num (car more))
                        `(,',priv-op ,num ,(car more))))
                   (t                          ; three and more numbers are given
                    (let ((args (cons num more)))
                      (cond ((not (find-if-not #'const-literalp args))
                             ;; there are literals only
                             (apply ',op args))
                            ((find-if #'function-callp args)
                             ;; There are function calls in arguments.
                             ;; Side effects are possible.
                             (let* ((num-init `(,(gensym) ,num))
                                    (num-var (car num-init))
                                    (more-inits (map (lambda (x) `(,(gensym) ,x)) more)))
                               `(let (,num-init ,@more-inits)
                                  (and ,@(map (lambda (x) `(,',priv-op ,num-var ,(car x)))
                                              more-inits)))))
                            (t
                             ;; arguments just list of variables and literals
                             `(and ,@(map (lambda (x) `(,',priv-op ,num ,x))
                                          more)))))))))))))
  (gen-macro =))

;;--------------------------------------------------------------------------
;; compiler macro: /=
;;--------------------------------------------------------------------------
(macrolet
  ((gen-macro (op)
     (let ((priv-op (intern (binary #"%" (symbol-name op))))
           (more-priv-op (intern (binary "%%" (symbol-name op)))))
       `(define-compiler-macro ',op
         (macro (num &rest more)
           (let ((env (macro-env)))
             (setq num (macroexpand num env))
             (setq more (map (lambda (x) (macroexpand x env)) more))
             (cond ((null more) t)             ; one number is given
                   ((null (cdr more))          ; two numbers are given
                    (if (and (const-literalp num) (const-literalp (car more)))
                        (funcall ',op num (car more))
                        `(,',priv-op ,num ,(car more))))
                   (t                     ; three and more numbers are given
                    (let ((args (cons num more)))
                      (if (not (find-if-not #'const-literalp args))
                          ;; there are literals only - can be evaluated during
                          ;; compilation
                          (apply ',op args)
                          ;; cannot inline, because numbers should be sorted before
                          ;; comparison
                          `(,',more-priv-op ,@args)))))))))))
  (gen-macro /=))

;;--------------------------------------------------------------------------
;; compiler macros: <, <=, >, >=
;;--------------------------------------------------------------------------
(macrolet
  ((gen-macro (op)
     (let ((priv-op (intern (binary #"%" (symbol-name op)))))
       `(define-compiler-macro ',op
         (macro (num &rest more)
           (setq num (macroexpand num))
           (setq more (map (lambda (x) (macroexpand x)) more))
           (cond ((null more) t)        ; one number is given
                 ((null (cdr more))     ; two numbers are given
                  (if (and (const-literalp num) (const-literalp (car more)))
                      (funcall ',op num (car more))
                      `(,',priv-op ,num ,(car more))))
                 (t                     ; three and more numbers are given
                  (let ((args (cons num more)))
                    (cond ((not (find-if-not #'const-literalp args))
                           ;; there are literals only
                           (apply ',op args))
                          ((find-if #'function-callp args)
                           ;; There are function calls in arguments.
                           ;; Side effects are possible.
                           (let* ((inits (map (lambda (x) `(,(gensym) ,x)) args))
                                  (rest (cdr inits)))
                             `(let (,inits)
                                (and ,@(map (lambda (x)
                                              (let ((exp `(,',priv-op ,(caar inits)
                                                                      ,(car x))))
                                                (setq inits (cdr inits))
                                                exp))
                                            rest)))))
                          (t
                           (let ((prev-exp num))
                             `(and ,@(map (lambda (x)
                                            (let ((exp `(,',priv-op ,prev-exp ,x)))
                                              (setq prev-exp x)
                                              exp))
                                          more)))))))))))))
  (gen-macro <)
  (gen-macro <=)
  (gen-macro >)
  (gen-macro >=))

;;--------------------------------------------------------------------------
;; compiler macros: +, *
;;--------------------------------------------------------------------------
(macrolet
  ((gen-macro (op initial-value)
     (let ((priv-op (intern (binary #"%" (symbol-name op)))))
       `(define-compiler-macro ',op
         (macro (&rest args)
           (let ((env (macro-env)))
             (setq args (map (lambda (x) (macroexpand x env)) args))
             (cond ((null args) ,initial-value)    ; there are no arguments
                   ((null (cdr args)) (car args)) ; one argument is given
                   (t
                    ;; two or more arguments are given
                    (let ((literals (remove-if-not #'const-literalp args))
                          (vars (remove-if #'const-literalp args)))
                      (cond ((null vars)
                             (apply ',op literals))
                            ((null literals)
                             `(,',priv-op ,(car vars) (,',op . ,(cdr vars))))
                            (t
                             `(,',priv-op ,(apply ',op literals)
                                          (,',op . ,vars)))))))))))))
  (gen-macro + 0)
  (gen-macro * 1))

;;--------------------------------------------------------------------------
;; compiler macro: -
;;--------------------------------------------------------------------------
(define-compiler-macro '-
  (macro (minuend &rest args)
    (let ((env (macro-env)))
      (setq minuend (macroexpand minuend env))
      (setq args (map (lambda (x) (macroexpand x env)) args))
      (cond ((null args)                  ; one number is given
             (if (const-literalp minuend)
                 (neg minuend)
                 `(neg ,minuend)))
            ((null (cdr args))            ; two numbers are given
             (if (and (const-literalp minuend) (const-literalp (car args)))
                 (%- minuend (car args))
                 `(%- ,minuend ,(car args))))
            (t
             ;; three and more numbers are given
             (let ((literals (remove-if-not #'const-literalp args))
                   (vars (remove-if #'const-literalp args)))
               (cond ((null vars)
                      (if (const-literalp minuend)
                          (apply '- (cons minuend literals))
                          `(%- ,minuend ,(apply '+ literals))))
                     (literals
                      (with-gensyms (res)
                        `(let ((,res ,minuend))
                           (setq ,res (%- ,res ,(apply '+ literals)))
                           ,@(map (lambda (x)
                                    (setq vars (cdr vars))
                                    (if vars
                                        `(setq ,res (%- ,res ,x))
                                        `(%- ,res ,x)))
                                  vars))))
                     (t
                      (with-gensyms (res)
                        `(let ((,res ,minuend))
                           ,@(map (lambda (x)
                                    (setq args (cdr args))
                                    (if args
                                        `(setq ,res (%- ,res ,x))
                                        `(%- ,res ,x)))
                                  args)))))))))))

;;--------------------------------------------------------------------------
;; compiler macro: div
;;--------------------------------------------------------------------------
(define-compiler-macro 'div
  (macro (numerator &rest args)
    (let ((env (macro-env)))
      (setq numerator (macroexpand numerator env))
      (setq args (map (lambda (x) (macroexpand x env)) args))
      (cond ((null args)                  ; one number is given
             (if (const-literalp numerator)
                 (%div 1 numerator)
                 `(%div 1 ,numerator)))
            ((null (cdr args))            ; two numbers are given
             (if (and (const-literalp numerator) (const-literalp (car args)))
                 (%div numerator (car args))
                 `(%div ,numerator ,(car args))))
            (t
             ;; three and more numbers are given
             (let ((literals (remove-if-not #'const-literalp args))
                   (vars (remove-if #'const-literalp args)))
               (cond ((null vars)
                      (if (const-literalp numerator)
                          (apply 'div (cons numerator literals))
                          `(%div ,numerator ,(apply '* literals))))
                     (literals
                      (with-gensyms (res)
                        `(let ((,res ,numerator))
                           (setq ,res (%div ,res ,(apply '* literals)))
                           ,@(map (lambda (x)
                                    (setq vars (cdr vars))
                                    (if vars
                                        `(setq ,res (%div ,res ,x))
                                        `(%div ,res ,x)))
                                  vars))))
                     (t
                      (with-gensyms (res)
                        `(let ((,res ,numerator))
                           ,@(map (lambda (x)
                                    (setq args (cdr args))
                                    (if args
                                        `(setq ,res (%div ,res ,x))
                                        `(%div ,res ,x)))
                                  args)))))))))))

;;--------------------------------------------------------------------------
;; count-if
;;--------------------------------------------------------------------------
(export 'count-if)
(defun count-if (predicate seq)
  "Function: (count-if predicate seq) ==> integer

Parameters: predicate - (lambda (x)) ==> bool
            seq - proper list

Returns number of elements, which predicate function returned true."

  (let ((res 0))
    (dolist (x seq)
      (if (funcall predicate x)
          (incf res)))
    res))

;;--------------------------------------------------------------------------
;; count-if-not
;;--------------------------------------------------------------------------
(export 'count-if-not)
(defun count-if-not (predicate seq)
  "Function: (count-if-not predicate seq) ==> integer

Parameters: predicate - (lambda (x)) ==> bool
            seq - proper list

Returns number of elements, which predicate function returned false."

  (let ((res 0))
    (dolist (x seq)
      (if (not (funcall predicate x))
          (incf res)))
    res))

;;--------------------------------------------------------------------------
;; once-only used to generate code that evaluates certain macro arguments once
;; only.
;;--------------------------------------------------------------------------
(export 'once-only)
(defmacro once-only (names &rest body)
  "Macro: (once-only names &rest body) ==> list

Parameters: names - list
            body - list of expressions

'once-only' macro used to generate code that evaluates certain macro arguments
once only."

  (let ((gensyms (map (lambda (n) (gensym (symbol-name n))) names)))
    (with-gensyms (env expanded-names)
      `(let* ((,env (macro-env))
              (,expanded-names (map (lambda (x) (macroexpand x ,env)) (list ,@names))))
         (if (not (find-if #'function-callp ,expanded-names))
             ;; there are variables and literals only
             `(progn ,,@body)
             ;; there are function calls in names
             (let (,@(map (lambda (g) `(,g (gensym))) gensyms))
               `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
                  ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
                        ,@body))))))))

;;--------------------------------------------------------------------------
;; case
;;--------------------------------------------------------------------------
(export 'case)
(defmacro case (exp &rest cases)
  "Macro: case

Syntax:
        case keyform {normal-clause}+ [t-clause] => object
        normal-clause::= (keys form+)
        t-clause::= (t form+)

Arguments and Values:
        keyform - a form; evaluated to produce a test-key.
        keys - a designator for single object or list of objects. The symbol t
          may not be used as the key designator. To refer to that symbol by
          itself as key, the designator (t) must be used instead.

Description:
Allows the conditional execution of a body of forms in a clause that is selected
by matching the test-key with 'eq' function.

Each of the normal-clauses is then considered in turn. If the test-key is the
same as any key for that clause, the forms in that clause are evaluated as an
implicit progn, and the value it returns is returned as the value of the case.
If non of normal clause is match and t-clause is presented, then it is evaluated
in implicit progn, and the value it returns is returned as the value of the
case. If non of normal clause is match and there is no t-clause, then exception
is raised.

Example:
  (case (f x y 1 2)
    ((1 2) 'clause1)
    (3 'clause2)
    (nil 'no-keys-so-never-seen)
    ((nil) 'nilslot)
    ((:four #\"v\") 'clause4)
    ((t) 'tslot)
    (t 'others))"

  (once-only (exp)
    `(cond . ,(map (lambda (clause)
                     (let ((test-key (car clause)))
                       (if (eq test-key t)
                           clause
                           (if (consp test-key)
                               `((or . ,(map (lambda (x) `(eq ,exp ',x))
                                             test-key)) . ,(cdr clause))
                               `((eq ,exp ',test-key) . ,(cdr clause))))))
                   cases))))

;;--------------------------------------------------------------------------
;; dotimes
;;--------------------------------------------------------------------------
(export 'dotimes)
(defmacro dotimes (head &rest body)
  "Macro: (dotimes (var count-form [result-form]) body)

Parameters: var - symbol.
            count-form - form.
            result-form - form.
            body - form+.

Dotimes iterates over a series of integers.

Dotimes evaluates count-form, which should produce an integer. If count-form is
zero or negative, the body is not executed. dotimes then executes the body once
for each integer from 0 up to but not including the value of count-form with var
bound to current integer value. If result-form is presented, then dotimes
returns result of result form, otherwise it returns nil."

  (let ((var (car head))
        (count (cadr head))
        (res (cddr head)))
    (if res (setq res (car res)))
    (with-gensyms (l)
      `(do ((,l ,count)
            (,var 0 (1+ ,var)))
           ((>= ,var ,l) ,res)
        . ,body))))

;;--------------------------------------------------------------------------
;; multiple-value-list
;;--------------------------------------------------------------------------
(defmacro multiple-value-list (exp)
  "Macro: (multiple-value-list exp) ==> list

Parameters: exp - expression

Evaluates expression and creates a list of the multiple values it returns."

  `(multiple-value-call #'list ,exp))

;;--------------------------------------------------------------------------
;; multiple-value-setq
;;--------------------------------------------------------------------------
(export 'multiple-value-setq)
(defmacro multiple-value-setq (vars exp)
  "Macro: (multiple-value-setq (vars) exp) ==> primary value of expression

Parameters: vars - symbols
            exp - expression

Evaluates expression and variables to the multiple values it returns."

  (let (gnames)
    (dolist (n vars)
      (push (gensym) gnames))
    `(multiple-value-bind ,gnames ,exp
       ,@(let (res)
           (do ((v vars (cdr v))
                (n gnames (cdr n)))
               ((null v) res)
             (push (list 'setq (car v) (car n)) res))))))

;;--------------------------------------------------------------------------
;; internal function
;;--------------------------------------------------------------------------
(defun %time-throughput (ops run-time)
  (if ops
      (let ((bs (binary-stream-create))
            (ops-res (/ ops run-time)))
        (write-binary ", throughput " bs)
        (cond ((>= ops-res 1000000000)
               (fprintf bs "~a Gops/sec" (/ ops-res 1000000000)))
              ((>= ops 1000000)
               (fprintf bs "~a Mops/sec" (/ ops-res 1000000)))
              ((>= ops 1000)
               (fprintf bs "~a Kops/sec" (/ ops-res 1000)))
              (t
               (fprintf bs "~a ops/sec" ops-res)))
        (binary-stream-content bs))
      ""))

;;--------------------------------------------------------------------------
;; time
;;--------------------------------------------------------------------------
(export 'time)
(defmacro time (exp &key (gc t) ops name)
  "Macro: (time exp &key (gc t) ops name) ==> object

Parameters: exp - expression
            gc - boolean - do include garbage collector measurments
            ops - integer. Number of operations performed during measurment. If
              specified, then throughput is calculated.
            name - test name, printed before time.

Returns: value of exp

Description:
Measures time of expression execution and prints it to *stdout*."

  (if gc
      (with-gensyms (start end res gc-start gc-end run-time gc-time)
        `(progn
           (gc)
           (let* ((,gc-start (gc-time))
                  (,start (clock-time-msec CLOCK_MONOTONIC))
                  (,res (multiple-value-list ,exp))
                  (,end (clock-time-msec CLOCK_MONOTONIC))
                  (,gc-end (gc-time))
                  (,run-time (/ (- ,end ,start) 1000))
                  (,gc-time (- ,gc-end ,gc-start)))
             (printf "\n~aun time ~f sec, GC time ~f sec (~f%)~a"
                     ,(if name (binary name ": r") "R")
                     ,run-time ,gc-time (* (/ ,gc-time ,run-time) 100)
                     (%time-throughput ,ops ,run-time))
             (apply 'values ,res))))
      (with-gensyms (start end res run-time)
        `(let* ((,start (clock-time-msec CLOCK_MONOTONIC))
                (,res (multiple-value-list ,exp))
                (,end (clock-time-msec CLOCK_MONOTONIC))
                (,run-time (/ (- ,end ,start) 1000)))
             (printf "\n~aun time ~f sec~a"
                     ,(if name (binary name ": r") "R")
                     ,run-time (%time-throughput ,ops ,run-time))
             (apply 'values ,res)))))

;;--------------------------------------------------------------------------
;; charp
;;--------------------------------------------------------------------------
(export 'charp)
(defun charp (x)
  "Function: (charp x) ==> boolean

Parameters: x - object

Returns true if x is fixnum in range 0-255 including."

  (<= 0 x 255))

;;--------------------------------------------------------------------------
;; symb
;;--------------------------------------------------------------------------
(defun symb (&rest parts)
  "Function: (symb &rest parts) ==> symbol

Parameters: parts - binary or symbol or char.

Interns symbol, whose name is built of supplied parts."

  (do ((name (binary))
       (parts parts (cdr parts)))
      ((null parts) (intern name))
    (let ((p (car parts)))
      (case (type-of p)
        (fixnum (if (charp p)
                    (binary-append-char name p)
                    (signal 'invalid-argument p)))
        (binary (binary-append-binary name p))
        (symbol (binary-append-binary name (symbol-name p)))))))

;;--------------------------------------------------------------------------
;; return
;;--------------------------------------------------------------------------
(export 'return)
(defmacro return (&optional val)
  "Macro (return (&optional val)) ==> val

Parameters: val - object

Returns, as if by return-from, from the block named nil."

  `(return-from nil ,val))

;;--------------------------------------------------------------------------
;; defstruct-marker
;;--------------------------------------------------------------------------
(defvar *defstruct-marker* '#:defstruct-marker
  "Name of defstruct properties, stored in structure symbol.")

;;--------------------------------------------------------------------------
;; struct-slots
;;--------------------------------------------------------------------------
(export 'struct-slots)
(defun struct-slots (sym)
  "Function: (struct-slots sym) ==> list

Parameters: sym - symbol

Returns list of structure slots, previosly defined with defstruct. If structure
is not defined, then returns nil."

  (symbol-get-prop sym *defstruct-marker*))

;;--------------------------------------------------------------------------
;; defstruct
;;--------------------------------------------------------------------------
(export 'defstruct)
(defmacro defstruct (struct-descr doc-first-slot &rest slots)
  "Macro 'defstruct' defines structure with named slots. Underlying type for
structure is vector.

Syntax:
  (defstruct struct-descr [doc] {slot-descr}+) ==> struct-name

  struct-descr ::= struct-name | (struct-name struct-options)
  struct-name ::= symbol, name of struct
  struct-options ::= export-option
  export-option ::= :export | (:export bool)
  doc ::= binary
  slot-descr ::= slot-name | (slot-name init-form)
  slot-name ::= symbol, name of slot

'Export-option' defines if structure functions and macros should be exported. By
default they are not exported.

'Defstruct' generates miscellaneous named functional objects for constructing
structure and operate with its slots. Functional object may be lambda or macro.

Constructor:
  (struct-name-create &key {slot-description}*) ==> struct-name
Constructs structure and initializes it. Returns freshly constructed object.

Structure predicate:
  (struct-namep x) ==> bool
Returns true if x is object of structure 'name'.

Slot readers:
  (struct-name-slot-name x) ==> object
Returns value of 'slot-name'. 'x' must be object of type 'struct-name'.

Slot writers:
  (struct-name-slot-name-set x val) ==> val
Assigns slot to new value. 'x' must be object of type 'struct-name'."

  (let* ((struct-name (if (consp struct-descr) (car struct-descr) struct-descr))
         (predicate-name (symb struct-name "p"))
         (validator-name (symb struct-name "-validate"))
         (struct-opts (if (consp struct-descr) (cdr struct-descr) nil))
         (doc (if (binaryp doc-first-slot) doc-first-slot nil))
         (slots (if (binaryp doc-first-slot) slots (cons doc-first-slot slots)))
         (slot-names (map (lambda (x) (if (consp x) (car x) x)) slots))
         (nslots (length slots))
         exported)
    ;; ensure that struct has slots, undefstruct and struct-slots functions
    ;; relay on that
    (if (not slots) (signal 'no-slots-defined struct-name))
    ;; set structure options
    (dolist (o struct-opts)
      (cond ((or (eq o :export)
                 (and (consp o) (eq (car o) :export)
                      (not (null (cadr o)))))
             (setq exported t))
            (t (signal 'unknow-struct-option o))))
    ;; generate structure macros
    `(progn
       ;; constructor
       ,(let ((definition
               `(defun ,(symb struct-name "-create") ,(cons '&key slots)
                  (vector ',struct-name ,@slot-names))))
         (if exported
             `(export ,definition)
             definition))
       ;; predicate
       ,(let ((definition
               `(defmacro ,predicate-name (x)
                  (once-only (x)
                    `(and (vectorp ,x)
                          (= (vector-length ,x) ,,(1+ nslots))
                          (eq (vector-ref ,x 0) ',',struct-name))))))
         (if exported
             `(export ,definition)
             definition))
       ;; validator
       ,(if (find-if (lambda (x) (eq x :debug)) *features*)
            `(defun ,validator-name (x)
               (if (,predicate-name x)
                   x
                   (signal ',(symb "invalid-structure-" struct-name) x)))
            `(defmacro ,validator-name (x) x))
       ;; readers and writers
       ,@(do ((i 1 (1+ i))
              (rest slot-names (cdr rest))
              (result nil))
             ((null rest) result)
             (let* ((f (car rest))
                    (reader `(defmacro ,(symb struct-name #"-" f)
                                 (s)
                               `(vector-ref (,',validator-name ,s) ,,i)))
                    (writer `(defmacro ,(symb struct-name #"-" f "-set")
                                 (s v)
                               `(vector-set (,',validator-name ,s) ,,i ,v))))
               (if exported
                   (progn
                     (push `(export ,reader) result)
                     (push `(export ,writer) result))
                   (progn
                     (push reader result)
                     (push writer result)))))
       ;; store structure slots in symbol. the list is returned by
       ;; 'structure-slots' function
       (symbol-set-prop ',struct-name *defstruct-marker* ',slot-names)
       ;; set struct documentation
       (set-symbol-struct-doc ',struct-name ,doc))))

;;--------------------------------------------------------------------------
;; undefstruct
;;--------------------------------------------------------------------------
(export 'undefstruct)
(defun undefstruct (sym)
  "Function: (undefstruct sym) ==> bool

Parameters: sym - symbol

Undefines structure, previosly defined with defstruct. Does nothing, if
structure is not defined. Returns true, if structure was undefined. Otherwise,
returns nil."

  (let ((slots (struct-slots sym)))
    (if slots
        (progn (set-symbol-struct-doc sym nil)
               (undefun (symb sym "-create"))
               (undefun (symb sym "p"))
               (undefun (symb sym "-validate"))
               ;; undefine slots macros
               (dolist (s slots)
                 (undefun (symb sym "-" s))
                 (undefun (symb sym "-" s "-set")))
               t)
        nil)))

;;--------------------------------------------------------------------------
;; evenp
;;--------------------------------------------------------------------------
(export 'evenp)
(defun evenp (x)
  "Function: (evenp x) ==> bool

Parameters: x - integer

Returns true, if number is even (divisible by two); otherwise, returns false."

  (zerop (band x 1)))

;;--------------------------------------------------------------------------
;; oddp
;;--------------------------------------------------------------------------
(export 'oddp)
(defun oddp (x)
  "Function: (oddp x) ==> bool

Parameters: x - integer

Returns true, if number is odd (not divisible by two); otherwise, returns false."

  (not (zerop (band x 1))))

;;--------------------------------------------------------------------------
;; do-packages
;;--------------------------------------------------------------------------
(export 'do-packages)
(defmacro do-packages (var-res &rest body)
  "Macro: (do-packages (var [result]) statement+)

Parameters: var - symbol
            result - form, default nil
            statement - form

Iterates over all packages. For each package the 'var' is bound to the package,
and the statements in the body are executed. When all the package have been
processed, 'result' form is evaluated and returned as the value of the macro."

  (let ((var (car var-res))
        (res (if (cdr var-res) (cadr var-res))))
    (with-gensyms (i)
      `(dolist (,i (all-packages) ,res)
         (let ((,var ,i))
           ,@body)))))

;;--------------------------------------------------------------------------
;; do-symbols
;;--------------------------------------------------------------------------
(export 'do-symbols)
(defmacro do-symbols (var-pack-res &rest body)
  "Macro: (do-symbols (var [package [result]]) statement+)

Parameters: var - symbol
            package - should evaluate to symbol or binary or package. Default is
              current package.
            result - form, default nil
            statement - form

Iterates over all symbols in package. For each symbol the 'var' is bound to the symbol,
and the statements in the body are executed. When all the symbols have been
processed, 'result' form is evaluated and returned as the value of the macro."

  (let ((var (car var-pack-res))
        (pack (if (cdr var-pack-res) (cadr var-pack-res) '*package*))
        (res (if (and (cdr var-pack-res) (cddr var-pack-res)) (caddr var-pack-res))))
    (with-gensyms (syms i)
      `(let ((,syms (package-symbols ,pack)))
         (dotimes (,i (vector-length ,syms) ,res)
           (let ((,var (vector-ref ,syms ,i)))
             ,@body))))))

;;--------------------------------------------------------------------------
;; do-all-symbols
;;--------------------------------------------------------------------------
(export 'do-all-symbols)
(defmacro do-all-symbols (var-res &rest body)
  "Macro: (do-all-symbols (var [result]) statement+)

Parameters: var - symbol
            result - form, default nil
            statement - form

Iterates over all symbols in all package. For each symbol the 'var' is bound to
the symbol, and the statements in the body are executed. When all the symbols
have been processed, 'result' form is evaluated and returned as the value of the
macro."

  (let ((var (car var-res))
        (res (if (cdr var-res) (cadr var-res))))
    (with-gensyms (p-var)
      `(do-packages (,p-var ,res)
         (do-symbols (,var ,p-var)
           ,@body)))))

;;--------------------------------------------------------------------------
;; prog1
;;--------------------------------------------------------------------------
(export 'prog1)
(defmacro prog1 (&rest body)
  "Macro: (prog1 statement+)

Parameters: statement - form

Evaluates the statements in order and returns value of first statement."

  (let ((first (car body))
        (rest (cdr body)))
    (if (null rest)
        first
        (with-gensyms (res)
          `(let ((,res ,first))
             ,@rest
             ,res)))))

;;--------------------------------------------------------------------------
;; with-stream-lock
;;--------------------------------------------------------------------------
(export 'with-stream-lock)
(defmacro with-stream-lock (stream &rest body)
  "Macro: (with-stream-lock (stream) statement+)

Parameters: stream - stream
            statement - form

Locks stream and evaluates forms, then unlocks stream."

  (let ((s (car stream)))
    (once-only (s)
      `(progn (stream-lock ,s)
              (unwind-protect (progn ,@body)
                (stream-unlock ,s))))))

;;--------------------------------------------------------------------------
;; file-pwrite-exact
;;--------------------------------------------------------------------------
(export 'file-pwrite-exact)
(defun file-pwrite-exact (file offset buf &optional (buf-offset 0)
                          (count (- (binary-length buf) buf-offset)))
  "Function: (file-write-exact file offset buf &optional (buf-offset 0)
                            (count (- (binary-length buf) buf-offset))) => nil

Parameters: file -file
            offset - integer. File offset from the beginning of the file.
            buf - binary.
            count - exact number of bytes, that should be written to file.

Calls 'file-pwrite' function in loop untill all required bytes are written in
file. Does not handle non-blocking files."

  (with-stream-lock (file)
    (tagbody
     again
       (let ((delta (file-pwrite file offset buf buf-offset count)))
         (unless (zerop (decf count delta))
           (incf offset delta)
           (incf buf-offset delta)
           (go again))))))

;;--------------------------------------------------------------------------
;; file-pread-exact
;;--------------------------------------------------------------------------
(export 'file-pread-exact)
(defun file-pread-exact (file offset count &optional (buf (binary)))
  "Function: (file-pread-exact file offset count &optional (buf (binary))) => buf

Parameters: file -file
            offset - integer. File offset from the beginning of the file.
            count - fixnum. Number of bytes to read from file.
            buf - binary. Where to store read bytes. Read bytes are added to
               end of buffer.

Calls 'file-pread' function in loop untill all required bytes are read from
file. Does not handle non-blocking files."

  (with-stream-lock (file)
    (tagbody
     again
       (let ((delta (file-pread file offset count buf)))
         (unless (zerop (decf count delta))
           (incf offset delta)
           (go again)))))
  buf)

;;--------------------------------------------------------------------------
;; file-tell
;;--------------------------------------------------------------------------
(export 'file-tell)
(defun file-tell (file)
  "Function: (file-tell file) ==> integer

Parameters: file - file

Returns current position of the file. Calls (file-seek file 0 'SEEK_CUR)."

  (file-seek file 0 'SEEK_CUR))

;;--------------------------------------------------------------------------
;; apropos
;;--------------------------------------------------------------------------
(export 'apropos)
(defun apropos (what &optional package)
  "Function: (apropos what &optional package) ==> bool

Parameters: what - regular expression - symbol or string
            package - should evaluate to symbol or binary or package. Default is
              all packages.

Searches all symbols in package for binary represented by symbol's name of 'sym'
argument. Search is case insensitive."

  (let ((re (regcomp (if (binaryp what)
                         what
                         (symbol-name what))
                     :icase t
                     :extented t
                     :nosub t)))
    (if package
        (do-symbols (s package)
          (if (regexec re (symbol-name s))
              (print s)))
        (do-all-symbols (s)
          (if (regexec re (symbol-name s))
              (print s))))))

;;--------------------------------------------------------------------------
;; describe
;;--------------------------------------------------------------------------
(defvar *describe-delimiter* #s("-----------------------------------------------"
                                "-----------------------------\n"))
(export 'describe)
(defun describe (sym)
  "Function: (describe sym) ==> nil

Parameters: sym - symbol

Prints symbol's documentation to standard output."

  (flet ((print-descr (d)
           (if d (printf "~a~a\n" *describe-delimiter* d))))
    (print-descr (binary "Symbol " (package-name (symbol-package sym))
                         (if (exportedp sym) #":" "::") (symbol-name sym)))
    (print-descr (symbol-function-doc sym))
    (print-descr (symbol-value-doc sym))
    (print-descr (symbol-struct-doc sym)))
  (write-exact *describe-delimiter*)
  nil)

;;--------------------------------------------------------------------------
;; subbinary
;;--------------------------------------------------------------------------
(export 'subbinary)
(defun subbinary (seq start &optional (length (- (binary-length seq) start))
                  (dest (binary)))
  "Function: (subbinary (seq start &optional (length (- (binary-length seq) start))
                     (dest (binary)))) ==> dest

Parameters: seq - binary
            start - fixnum
            length - fixnum. If not specified, then subbinary extracted upto end
              of binary.
            dest - binary. If specified, then subbinary is added to end of
              'dest' parameter.

Extracts subbinary from binary. Returns new binary, if 'dest' parameter is not
specified. Otherwise, 'dest' parameter is returned."

  (binary-ensure-capacity dest length)
  (dotimes (i length)
    (binary-append-char dest (binary-ref seq (+ start i))))
  dest)

;;--------------------------------------------------------------------------
;; which
;;--------------------------------------------------------------------------
(export 'which)
(defun which (name)
  "Function: (which name) ==> binary or nil

Parameters: name - binary

Locates executable, like 'which' command line utility"

  (cond ((or (absolute-pathp name)
             (binary-starts name "./"))
         (if (file-access name X_OK) name nil))
        ((not (equal (dirname name) "."))
         (let ((exe (binary (getcwd) #"/" name)))
           (if (file-access exe X_OK) exe nil)))
        (t
         (let* ((path (getenv "PATH" ""))
                (exe (binary))
                (start 0)
                (end (binary-find-char path #":" start)))
           (do ()
               ((not end) nil)
             (binary-append-binary
               (binary-append-char
                 (subbinary path start (- end start) exe)
                 #"/")
               name)
          (if (file-access exe X_OK) (return-from which exe))
          (binary-clear exe)
          (setq start (1+ end))
          (setq end (binary-find-char path #":" start)))))))

;;--------------------------------------------------------------------------
;; handler-case
;;--------------------------------------------------------------------------
(export 'handler-case)
(defmacro handler-case (exp &rest clauses)
  "Macro: (handler-case exp clauses+)

Syntax: exp - form
        clauses - label-clause | t-clause
        label-clause - (label var-binding exp+)
        label - object
        var-binding - nil | (value)
        var - symbol
        t-clause - (t t-var-binding exp+)
        t-var-binding - nil | (label value*)

'Handler-case' executes expression and catches signals generated by signal
special form. If no signals are generated by expression then value of that
expression is returned. If signals is generated by calling special form (signal
label &optional value), then signal's label is matched against labels specified in
label-clauses in turn, using 'eq' function. Labels are not evaluated. If label
is matched, then signal's value is bound to variable, specified in clause, and
clause's body is evaluated in implicit 'progn'. Rest of labels after matched one
is not matched. If var-binding is specified as nil, then clause's body is
evaluated without binding. If signal label is not matched any label-clause and
t-clause is presented, then signal is catched by t-clause and clause's body is
evaluated in implicit 'progn'. Vairables in t-clause is bound to arguments of
signal special form, generated signal.  If t-clause is not specified, then signal is
not catched by this handler and passed up to next handler. If signal is not
catched by all active handlers, then thread is terminated.

Clauses bodies in 'handler-case' are executed after stack is completely
unwinded. That is different from 'handler-bind', where handler is invoked before
stack unwinding.

Example:
  (handler-case
    (signal 'error 36)
    (error (n) (print (list 'catched 'error n)))
    (t (label value) (print (list 'unhandled 'signal label value))))"

  (labels ((gen-bind-clauses (catch-tag lablel-param value-param clauses first-time)
             (if (and (null first-time) (null clauses))
                 (return-from gen-bind-clauses '((t nil))))
             (let* ((c (car clauses))
                    (clause-label (car c)))
               (setq c `(,clause-label (throw ',catch-tag (values ',catch-tag
                                                                ,lablel-param ,value-param))))
               (if (eq clause-label t)
                   (cons c nil)
                   (cons c (gen-bind-clauses catch-tag lablel-param value-param
                                             (cdr clauses) nil)))))
           (gen-clauses (catch-tag lablel-param value-param clauses)
             (if (null clauses) (return-from gen-clauses '((t (signal 'handler-case-internal-error)))))
             (let* ((c (car clauses))
                    (clause-label (car c))
                    (var-bindings (cadr c))
                    (body (cddr c)))
               (if var-bindings
                   (if (eq clause-label t)
                       (setq var-bindings `((,(car var-bindings) ,lablel-param)
                                            (,(cadr var-bindings) ,value-param)))
                       (setq var-bindings `((,(car var-bindings) ,value-param)))))
               (setq c `(,clause-label (let ,var-bindings ,@body)))
               (if (eq clause-label t)
                   (cons c nil)
                   (cons c (gen-clauses catch-tag lablel-param value-param (cdr
             clauses)))))))
    (with-gensyms (tag label value res)
      `(let ((,res (multiple-value-list
                    (catch ',tag
                      (handler-bind
                          (lambda (,label ,value)
                            (case ,label ,@(gen-bind-clauses tag label value
                                                             clauses t)))
                        ,exp)))))
         (if (eq (car ,res) ',tag)
             ((lambda (,label ,value)
                (declare (ignore ,label ,value))
                (case ,label ,@(gen-clauses tag label value clauses)))
              (cadr ,res) (caddr ,res))
             (apply 'values ,res))))))

;;--------------------------------------------------------------------------
;; handler-case-single-value
;;--------------------------------------------------------------------------
(export 'handler-case-single-value)
(defmacro handler-case-single-value (exp &rest clauses)
  "Macro: (handler-case exp clauses+)

Syntax: exp - form
        clauses - label-clause | t-clause
        label-clause - (label var-binding exp+)
        label - object
        var-binding - nil | (value)
        var - symbol
        t-clause - (t t-var-binding exp+)
        t-var-binding - nil | (label value*)

'handler-case-single-value' is same as 'handler-case', but returns only main
value of 'exp', even if it returns multiple values, while 'handler-case' handles
multiple return values. This macro produces faster code because it uses
'multiple-value-bind' special form instead of 'multiple-value-list' macro."

  (labels ((gen-bind-clauses (catch-tag lablel-param value-param clauses first-time)
             (if (and (null first-time) (null clauses))
                 (return-from gen-bind-clauses '((t nil))))
             (let* ((c (car clauses))
                    (clause-label (car c)))
               (setq c `(,clause-label (throw ',catch-tag (values ',catch-tag
                                                                ,lablel-param ,value-param))))
               (if (eq clause-label t)
                   (cons c nil)
                   (cons c (gen-bind-clauses catch-tag lablel-param value-param
                                             (cdr clauses) nil)))))
           (gen-clauses (catch-tag lablel-param value-param clauses)
             (if (null clauses) (return-from gen-clauses '((t (signal 'handler-case-internal-error)))))
             (let* ((c (car clauses))
                    (clause-label (car c))
                    (var-bindings (cadr c))
                    (body (cddr c)))
               (if var-bindings
                   (if (eq clause-label t)
                       (setq var-bindings `((,(car var-bindings) ,lablel-param)
                                            (,(cadr var-bindings) ,value-param)))
                       (setq var-bindings `((,(car var-bindings) ,value-param)))))
               (setq c `(,clause-label (let ,var-bindings ,@body)))
               (if (eq clause-label t)
                   (cons c nil)
                   (cons c (gen-clauses catch-tag lablel-param value-param (cdr
             clauses)))))))
    (with-gensyms (tag label value res)
      `(multiple-value-bind (,res ,label ,value)
           (catch ',tag
             (handler-bind
                 (lambda (,label ,value)
                   (case ,label ,@(gen-bind-clauses tag label value clauses t)))
               ,exp))
         (if (eq ,res ',tag)
             ((lambda (,label ,value)
                (declare (ignore ,label ,value))
                (case ,label ,@(gen-clauses tag label value clauses)))
              ,label ,value)
             ,res)))))

;;--------------------------------------------------------------------------
;; with-mutex-lock
;;--------------------------------------------------------------------------
(export 'with-mutex-lock)
(defmacro with-mutex-lock (mutex &rest body)
  "Macro: (with-mutex-lock (mutex) statement+)

Parameters: mutex - mutex
            statement - form

Locks mutex and evaluates forms under 'unwind-protect, then unlocks mutex as
part of unwinding process."

  (let ((m (car mutex)))
    (once-only (m)
      `(progn (mutex-lock ,m)
              (unwind-protect (progn ,@body)
                (mutex-unlock ,m))))))

;;--------------------------------------------------------------------------
;; with-rwlock-rdlock
;;--------------------------------------------------------------------------
(export 'with-rwlock-rdlock)
(defmacro with-rwlock-rdlock (rwlock &rest body)
  "Macro: (with-rwlock-rdlock (rwlock) statement+)

Parameters: rwlock - rwlock
            statement - form

Locks rwlock for read and evaluates forms under 'unwind-protect, then unlocks
rwlock as part of unwinding process."

  (let ((m (car rwlock)))
    (once-only (m)
      `(progn (rwlock-rdlock ,m)
              (unwind-protect (progn ,@body)
                (rwlock-unlock ,m))))))

;;--------------------------------------------------------------------------
;; with-rwlock-wrlock
;;--------------------------------------------------------------------------
(export 'with-rwlock-wrlock)
(defmacro with-rwlock-wrlock (rwlock &rest body)
  "Macro: (with-rwlock-wrlock (rwlock) statement+)

Parameters: rwlock - rwlock
            statement - form

Locks rwlock for write and evaluates forms under 'unwind-protect, then unlocks
rwlock as part of unwinding process."

  (let ((m (car rwlock)))
    (once-only (m)
      `(progn (rwlock-wrlock ,m)
              (unwind-protect (progn ,@body)
                (rwlock-unlock ,m))))))

;;--------------------------------------------------------------------------
;; with-open-stream
;;--------------------------------------------------------------------------
(export 'with-open-stream)
(defmacro with-open-stream (init &rest body)
  "Macro: (with-open-stream (stream-var [stream-init]) statement+)

Parameters: stream-var - symbol
            stream-init - form
            statement - form

Initializes stream variable with specified init form and evaluates statements
under 'unwind-protect, then closes stream as part of unwinding process."

  (let ((stream (car init)))
    (if (cdr init)
        `(let (,init)
           (unwind-protect (progn ,@body)
             (close ,stream)))
        `(unwind-protect (progn ,@body)
           (close ,stream)))))

;;--------------------------------------------------------------------------
;; with-open-socket
;;--------------------------------------------------------------------------
(export 'with-open-socket)
(defmacro with-open-socket (init &rest body)
  "Macro: (with-open-socket (socket-var [socket-init]) statement+)

Parameters: socket-var - symbol
            socket-init - form
            statement - form

Initializes socket variable with specified init form and evaluates statements
under 'unwind-protect, then closes socket as part of unwinding process."

  (let ((socket (car init)))
    (if (cdr init)
        `(let (,init)
           (unwind-protect (progn ,@body)
             (socket-close ,socket)))
        `(unwind-protect (progn ,@body)
           (socket-close ,socket)))))

;;--------------------------------------------------------------------------
;; tcp-listener-start
;;--------------------------------------------------------------------------
(export 'tcp-listener-start)
(defun tcp-listener-start (port fn args &key bind-address (reuse-address t)
                           (backlog 128) timeout name stack-size
                           suppress-closure-warning)
  "Function: (tcp-listener-start (port fn args &key bind-address reuse-address
                                 backlog timeout name stack-size
                                 suppress-closure-warning) ==> tcp-listener

Parameters: port - integer. Listening port.
            fn - function or symbol. User supplied server function:
              (lambda (sock ...)). The function should close socket, when it
              finished processing. If socket is not closed by function, it will
              be closed by GC, but it is undefined, when it will be done.
              If symbol is given then function may be upgraded without listener
              restart.
            args - additional arguments passed to function.
            bind-address - string. Listening interface.
            reuse-address - bool. Reuse already bound address.
            backlog - integer. Size of backlog for incomming connections.
            timeout - integer. Read timeout of newly connected client.
            name - object. Name of listener's main thread.
            stack-size - integer. Stack size for handling function.
            suppress-closure-warning - boolean. Do not warn, if server function
              is closure. Since TCP listener is multithreading, ensure that you
              understand all risks of closure in threads.

Creates TCP listener. TCP listener provides infrastructure for fast accepting of
incoming TCP connections. Listener is created in new thread and also few working
threads are created - one per CPU. Main listener thread accepts incoming TCP
connections and dispatches them between working threads using fast
tqueues. Worker thread then invokes user supplied server function. If server
function is fast enough then it can handle connection by itself. Otherwise, it
can create additional threads for connections handling."

  (if (and (not suppress-closure-warning)
           (closurep (if (symbolp fn)
                         (symbol-function fn)
                         fn)))
      (princ #s("\nWARNING: closure is passed to tcp-listener-start, "
                "make sure that you understand all risks.")))
  (let (srv
        stopped
        (queues (vector-create os:CPU-ONLINE-COUNT)))
    ;; create worker threads pool
    (dotimes (i os:CPU-ONLINE-COUNT)
      (thread-create
       (lambda (q)
         (let (sock)
           (tagbody
            again
              (handler-case
                  (while (setq sock (tqueue-pop q))
                    (socket-set-timeout sock timeout)
                    (apply fn (cons sock args)))
                (t (label value)
                  (fprintf *stderr*
                           #s("\nUnhandled signal in request "
                              "processing (~s ~s). "
                              "TCP listener: ~a, port ~d")
                           label value name port)
                  (go again))))))
       (list (vector-set queues i (tqueue-create)))
       :stack-size stack-size
       :suppress-closure-warning t))
    ;; create accepting thread
    (setq srv (thread-create
               (lambda ()
                 (with-open-socket (srv-sock (tcp-listen port
                                                         :bind-address
                                                         bind-address
                                                         :reuse-address
                                                         reuse-address
                                                         :backlog backlog
                                                         :timeout 1))
                   (do ((n 0))
                       (stopped)
                     (handler-case
                         (let ((s (socket-accept srv-sock)))
                           (when s
                             (tqueue-push (vector-ref queues n) s)
                             (if (eq (incf n) os:CPU-ONLINE-COUNT) (setq n 0))))
                       (t (label value)
                         (fprintf *stderr*
                                  #s("\nUnhandled signal in new connection "
                                     "processing (~s ~s). "
                                     "TCP listener: ~a, port ~d")
                                  label value name port)))))
                 (dotimes (i os:CPU-ONLINE-COUNT)
                   (tqueue-push (vector-ref queues i) nil)))
                 nil
                 :name name
                 :stack-size (* 32 1024)
                 :detached nil
                 :suppress-closure-warning t))
    (lambda ()
      (setq stopped t)
      (thread-join srv))))

;;--------------------------------------------------------------------------
;; tcp-listener-stop
;;--------------------------------------------------------------------------
(export 'tcp-listener-stop)
(defun tcp-listener-stop (srv)
  "Function: (tcp-listener-stop srv) ==> nil

Parameters: srv - tcp-listener, returned by tcp-listener-start function.

Stops listener, created  by 'tcp-listener-start function."

  (funcall srv)
  nil)

;;--------------------------------------------------------------------------
;; null-stream-create
;;--------------------------------------------------------------------------
(export 'null-stream-create)
(defun null-stream-create (&optional name)
  (stream-create :name name
                 :read-byte (lambda () nil)
                 :write-byte (lambda (byte)
                               (declare (ignore byte))
                               nil)
                 :read-binary (lambda (count buf)
                              (declare (ignore count buf))
                              nil)
                 :write-binary (lambda (buf buf-offset count)
                                 (declare (ignore buf buf-offset))
                                 count)
                 :sync (lambda () nil)
                 :datasync (lambda () nil)
                 :close (lambda () nil)))

;;--------------------------------------------------------------------------
;; to-string
;;--------------------------------------------------------------------------
(export 'to-string)
(defun to-string (x &optional (str (binary)))
  "Function: (to-string x &optional (str (binary))) ==> string

Parameters: x - object
            str - binary to, which append the result.

Converts object to string reprsentation as output of prin1 function."

  (let ((s (binary-stream-create str)))
    (prin1 x s)
    (binary-stream-content s)))

;;--------------------------------------------------------------------------
;; from-string
;;--------------------------------------------------------------------------
(export 'from-string)
(defun from-string (str)
  "Function: (from-string str) ==> object

Parameters: str - string representation of object.

Converts string reprsentation to object as output of read function."

  (read (binary-stream-create str)))

;;--------------------------------------------------------------------------
;; hashmap-emptyp
;;--------------------------------------------------------------------------
(export 'hashmap-emptyp)
(defun hashmap-emptyp (map)
  (zerop (hashmap-size map)))

;;--------------------------------------------------------------------------
;; treemap-emptyp
;;--------------------------------------------------------------------------
(export 'treemap-emptyp)
(defun treemap-emptyp (map)
  (zerop (treemap-size map)))

;;--------------------------------------------------------------------------
;; binary-emptyp
;;--------------------------------------------------------------------------
(export 'binary-emptyp)
(defun binary-emptyp (seq)
  (zerop (binary-length seq)))

;;--------------------------------------------------------------------------
;; binary-equal
;;--------------------------------------------------------------------------
(export 'binary-equal)
(defun binary-equal (x y)
  (zerop (binary-compare x y)))

;;--------------------------------------------------------------------------
;; binary-iequal
;;--------------------------------------------------------------------------
(export 'binary-iequal)
(defun binary-iequal (x y)
  (zerop (binary-icompare x y)))

;;--------------------------------------------------------------------------
;; vector-emptyp
;;--------------------------------------------------------------------------
(export 'vector-emptyp)
(defun vector-emptyp (seq)
  (zerop (vector-length seq)))

;;--------------------------------------------------------------------------
;; ustring-emptyp
;;--------------------------------------------------------------------------
(export 'ustring-emptyp)
(defun ustring-emptyp (seq)
  (zerop (ustring-length seq)))

;;--------------------------------------------------------------------------
;; *unbound-marker*
;;--------------------------------------------------------------------------
(defvar *unbound-marker* '#unbound-marker)

;;--------------------------------------------------------------------------
;; bprefix-node
;;--------------------------------------------------------------------------
(defstruct bprefix-node
  char parent (value *unbound-marker*) (children (hashmap-create)))

;;--------------------------------------------------------------------------
;; bprefix-map
;;--------------------------------------------------------------------------
(defstruct bprefix-map
  icase                                 ; ignore case
  (size 0)
  (root (bprefix-node-create))
  (immutable 0))

;;--------------------------------------------------------------------------
;; binary-prefix-map-create
;;--------------------------------------------------------------------------
(export 'binary-prefix-map-create)
(defun binary-prefix-map-create (&key icase)
  (bprefix-map-create :icase icase))

;;--------------------------------------------------------------------------
;; binary-prefix-map-size
;;--------------------------------------------------------------------------
(export 'binary-prefix-map-size)
(defun binary-prefix-map-size (map)
  (bprefix-map-size map))

;;--------------------------------------------------------------------------
;; binary-prefix-map-emptyp
;;--------------------------------------------------------------------------
(export 'binary-prefix-map-emptyp)
(defun binary-prefix-map-emptyp (map)
  (zerop (bprefix-map-size map)))

;;--------------------------------------------------------------------------
;; bprefix-char
;;--------------------------------------------------------------------------
(defun bprefix-key-char (key idx icase)
  (if icase
      (char-to-upper (binary-ref key idx))
      (binary-ref key idx)))

;;--------------------------------------------------------------------------
;; binary-prefix-map-insert
;;--------------------------------------------------------------------------
(export 'binary-prefix-map-insert)
(defun binary-prefix-map-insert (map key value &optional no-old-value)
  (if (plusp (bprefix-map-immutable map))
      (signal 'immutable-object map))
  (let ((node (bprefix-map-root map)))
    (dotimes (i (binary-length key))
      (let ((children (bprefix-node-children node))
            (c (bprefix-key-char key i (bprefix-map-icase map)))
            (parent node))
        (unless (setq node (hashmap-lookup children c))
          (hashmap-insert children c
                          (setq node (bprefix-node-create :char c :parent parent))))))
    (let ((old-value (bprefix-node-value node)))
      (bprefix-node-value-set node value)
      (if (eq old-value *unbound-marker*)
          (progn
            (bprefix-map-size-set map (1+ (bprefix-map-size map)))
            no-old-value)
          old-value))))

;;--------------------------------------------------------------------------
;; binary-prefix-map-lookup
;;--------------------------------------------------------------------------
(export 'binary-prefix-map-lookup)
(defun binary-prefix-map-lookup (map key &optional not-found)
  (let ((node (bprefix-map-root map)))
    (dotimes (i (binary-length key))
      (unless (setq node (hashmap-lookup (bprefix-node-children node)
                                         (bprefix-key-char key i
                                                           (bprefix-map-icase map))))
        (return-from binary-prefix-map-lookup not-found)))
    (if (eq (bprefix-node-value node) *unbound-marker*)
        not-found
        (bprefix-node-value node))))

;;--------------------------------------------------------------------------
;; binary-prefix-map-remove
;;--------------------------------------------------------------------------
(export 'binary-prefix-map-remove)
(defun binary-prefix-map-remove (map key &optional no-old-value)
  (if (plusp (bprefix-map-immutable map))
      (signal 'immutable-object map))
  (let ((node (bprefix-map-root map)))
    (dotimes (i (binary-length key))
      (unless (setq node (hashmap-lookup (bprefix-node-children node)
                                         (bprefix-key-char key i
                                                           (bprefix-map-icase map))))
        (return-from binary-prefix-map-remove no-old-value)))
    (let ((value (bprefix-node-value node)))
      (if (eq value *unbound-marker*)
          (return-from binary-prefix-map-remove no-old-value))
      (bprefix-node-value-set node *unbound-marker*)
      (bprefix-map-size-set map (1- (bprefix-map-size map)))
      (if (not (hashmap-emptyp (bprefix-node-children node)))
          (return-from binary-prefix-map-remove value))
      (let (parent)
        (while (setq parent (bprefix-node-parent node))
          (let ((parent-children (bprefix-node-children parent)))
            (hashmap-remove parent-children (bprefix-node-char node))
            (if (or (not (hashmap-emptyp parent-children))
                    (not (eq (bprefix-node-value parent) *unbound-marker*)))
                (return-from binary-prefix-map-remove value))
            (setq node parent))))
      value)))

;;--------------------------------------------------------------------------
;; binary-prefix-map-scan-longest
;;--------------------------------------------------------------------------
(export 'binary-prefix-map-scan-longest)
(defun binary-prefix-map-scan-longest (map key fn)
  (bprefix-map-immutable-set map (1+ (bprefix-map-immutable map)))
  (unwind-protect
       (let ((prefix (binary))
             (node (bprefix-map-root map)))
         (block nil
           (dotimes (i (binary-length key))
             (let* ((c (bprefix-key-char key i (bprefix-map-icase map)))
                    (child (hashmap-lookup (bprefix-node-children node) c)))
               (if (not child) (return nil))
               (setq node child)
               (binary-append-char prefix c))))
         (tagbody
          again
            (let ((value (bprefix-node-value node)))
              (unless (or (eq value *unbound-marker*)
                          (funcall fn key prefix value))
                (return-from binary-prefix-map-scan-longest nil)))
            (when (setq node (bprefix-node-parent node))
              (binary-remove prefix (1- (binary-length prefix)))
              (go again))))
    (bprefix-map-immutable-set map (1- (bprefix-map-immutable map)))))

;;--------------------------------------------------------------------------
;; wc-map
;;--------------------------------------------------------------------------
(defstruct wc-map
  icase                                 ; ignore case when matching
  (size 0)
  (prefix-map (binary-prefix-map-create :icase icase))
  (reverse-prefix-map (binary-prefix-map-create :icase icase))
  (enum-map (hashmap-create)))

;;--------------------------------------------------------------------------
;; wildcard-map-create
;;--------------------------------------------------------------------------
(export 'wildcard-map-create)
(defun wildcard-map-create (&key icase)
  (wc-map-create :icase icase))

;;--------------------------------------------------------------------------
;; wildcard-map-size
;;--------------------------------------------------------------------------
(export 'wildcard-map-size)
(defun wildcard-map-size (map)
  (let ((prefix-size (binary-prefix-map-size (wc-map-prefix-map map)))
        (reverse-size (binary-prefix-map-size (wc-map-reverse-prefix-map map)))
        (enum-size (hashmap-size (wc-map-enum-map map))))
  (values (+ prefix-size reverse-size enum-size)
          prefix-size reverse-size enum-size)))

;;--------------------------------------------------------------------------
;; wildcard-map-emptyp
;;--------------------------------------------------------------------------
(export 'wildcard-map-emptyp)
(defun wildcard-map-emptyp (map)
  (zerop (wildcard-map-size map)))

;;--------------------------------------------------------------------------
;; wildcard-to-regexp
;;--------------------------------------------------------------------------
(export 'wildcard-to-regexp)
(defun wildcard-to-regexp (exp)
  (let (prefix regex)
    (dotimes (i (binary-length exp))
      (let ((c (binary-ref exp i)))
        (case c
          (#"*" (unless regex
                  (setq regex (binary #"^")))
                (binary-append-binary regex ".*"))
          (#"?" (unless regex
                  (setq regex (binary #"^")))
                (binary-append-char regex #"."))
          (t (if regex
                 (progn
                   ;; escape character if necessary
                   (if (binary-find-char ".$^{[(|)*+?\\" c)
                       (binary-append-char regex #"\\"))
                   (binary-append-char regex c))
                 (if prefix
                     (binary-append-char prefix c)
                     (setq prefix (binary c))))))))
    (if regex (binary-append-char regex #"$"))
    (values prefix regex)))

;;--------------------------------------------------------------------------
;; parse-wildcard
;;--------------------------------------------------------------------------
(defun parse-wildcard (exp)
  (multiple-value-bind (prefix regex)
      (wildcard-to-regexp exp)
    (if prefix
        (return-from parse-wildcard (values :prefix prefix regex)))
    (multiple-value-bind (reverse-prefix rev-regex)
        (wildcard-to-regexp (binary-reverse exp))
      (if reverse-prefix
          (values :reverse-prefix reverse-prefix rev-regex)
          (values :enum exp regex)))))

;;--------------------------------------------------------------------------
;; wildcard-matchp
;;--------------------------------------------------------------------------
(export 'wildcard-matchp)
(defun wildcard-matchp (wildcard str &optional icase)
    (multiple-value-bind (prefix regexp)
        (wildcard-to-regexp wildcard)
      (unless prefix (setq prefix ""))
      (unless (if icase
                  (binary-istarts str prefix)
                  (binary-starts str prefix))
        (return-from wildcard-matchp nil))
      (if regexp
          (regexec (regcomp regexp :extented t :nosub t :icase icase)
                   str :str-offset (binary-length prefix))
          (eq (binary-length prefix) (binary-length str)))))

;;--------------------------------------------------------------------------
;; wildcard-map-insert
;;--------------------------------------------------------------------------
(export 'wildcard-map-insert)
(defun wildcard-map-insert (map wildcard value &optional no-old-value)
  (flet ((add-to-map (map prefix regexp re-compiled no-old-value)
           (let ((regex-map (binary-prefix-map-lookup map prefix)))
             (unless regex-map
               (setq regex-map (hashmap-create))
               (binary-prefix-map-insert map prefix regex-map))
             (let ((old-value (hashmap-insert regex-map regexp
                                              (cons re-compiled value))))
               (if old-value
                   (cdr old-value)
                   no-old-value)))))
    (multiple-value-bind (search-kind prefix regexp)
        (parse-wildcard wildcard)
      (let ((re-compiled (if regexp (regcomp regexp :extented t :nosub t
                                             :icase (wc-map-icase map)))))
        (case search-kind
          (:prefix
           (add-to-map (wc-map-prefix-map map)
                       prefix regexp re-compiled no-old-value))
        (:reverse-prefix
         (add-to-map (wc-map-reverse-prefix-map map)
                     prefix regexp re-compiled no-old-value))
        (:enum
         (let ((old-value (hashmap-insert (wc-map-enum-map map)
                                          wildcard (cons re-compiled value))))
           (if old-value
               (cdr old-value)
               no-old-value))))))))

;;--------------------------------------------------------------------------
;; wildcard-map-remove
;;--------------------------------------------------------------------------
(export 'wildcard-map-remove)
(defun wildcard-map-remove (map wildcard &optional no-old-value)
  (flet ((remove-from-map (map prefix regexp no-old-value)
           (let ((regex-map (binary-prefix-map-lookup map prefix)))
             (if regex-map
                 (let ((old-value (hashmap-remove regex-map regexp)))
                   (if (hashmap-emptyp regex-map)
                       (binary-prefix-map-remove map prefix))
                   (if old-value
                       (cdr old-value)
                       no-old-value))
                 no-old-value))))
    (multiple-value-bind (search-kind prefix regexp)
        (parse-wildcard wildcard)
      (case search-kind
        (:prefix
         (remove-from-map (wc-map-prefix-map map)
                          prefix regexp no-old-value))
        (:reverse-prefix
         (remove-from-map (wc-map-reverse-prefix-map map)
                          prefix regexp no-old-value))
        (:enum
         (let ((old-value (hashmap-remove (wc-map-enum-map map)
                                          wildcard)))
           (if old-value
               (cdr old-value)
               no-old-value)))))))

;;--------------------------------------------------------------------------
;; wildcard-map-match
;;--------------------------------------------------------------------------
(export 'wildcard-map-match)
(defun wildcard-map-match (map str &optional not-found)
  (binary-prefix-map-scan-longest
   (wc-map-prefix-map map)
   str
   (lambda (k prefix regex-map)
     (declare (ignore k))
     (hashmap-do regex-map
                 (lambda (k value)
                   (declare (ignore k))
                   (if (null (car value))
                       (if (wc-map-icase map)
                           (if (binary-iequal prefix str)
                               (return-from wildcard-map-match (cdr value)))
                           (if (binary-equal prefix str)
                               (return-from wildcard-map-match (cdr value))))
                       (if (regexec (car value) str :str-offset (binary-length prefix))
                           (return-from wildcard-map-match (cdr value))))))))
  (binary-prefix-map-scan-longest
   (wc-map-reverse-prefix-map map)
   (binary-reverse str)
   (lambda (k prefix regex-map)
     (declare (ignore k))
     (hashmap-do regex-map
                 (lambda (k value)
                   (declare (ignore k))
                   (if (regexec (car value) str :str-offset (binary-length prefix))
                       (return-from wildcard-map-match (cdr value)))))))
  (hashmap-do (wc-map-enum-map map)
              (lambda (k value)
                (declare (ignore k))
                (if (regexec (car value) str)
                    (return-from wildcard-map-match (cdr value)))))
  not-found)

;;--------------------------------------------------------------------------
;; Buffered input stream
;;--------------------------------------------------------------------------
(export 'create-buffered-input-stream)
(defun create-buffered-input-stream (stream)
  "Function: (create-buffered-input-stream stream) ==> buffered-stream

Parameters: stream - stream

Creates buffered input stream, in order to improve performance of read-byte
function on block-oriented streams."

  (let ((buf (binary))
        (offs 0))
    (stream-create
     :read-byte (lambda ()
                  (block nil
                    (when (eq offs (binary-length buf))
                      (setq offs 0)
                      (unless (setq buf (read-binary 1024 stream (binary-clear buf)))
                        (return nil)))
                    (let ((res (binary-ref buf offs)))
                      (incf offs)
                      res)))
     :read-binary (lambda (count user-buf)
                    (block nil
                      (when (eq offs (binary-length buf))
                      (return (read-binary count stream user-buf)))
                    (let ((buf-count (- (binary-length buf) offs)))
                      (if (<= buf-count count)
                          (progn
                            (binary-append-binary user-buf buf offs buf-count)
                            (setq offs 0)
                            (binary-clear buf))
                          (progn
                            (binary-append-binary user-buf buf offs count)
                            (incf offs count)))) user-buf)))))

;;--------------------------------------------------------------------------
;; Buffered output stream
;;--------------------------------------------------------------------------
(export 'create-buffered-output-stream)
(defun create-buffered-output-stream (stream)
  "Function: (create-buffered-output-stream stream) ==> buffered-stream

Parameters: stream - stream

Creates buffered output stream, in order to improve performance of write-byte
function on block-oriented streams."

  (let ((buf (binary)))
    (macrolet ((flush ()
                 '(progn (write-exact buf stream)
                   (binary-clear buf))))
      (stream-create
       :write-byte
         (lambda (byte)
           (when (eq (binary-length (binary-append-uint8 buf byte))
                     1024)
             (flush))
           nil)
       :write-binary
         (lambda (user-buf buf-offset count)
           (if (>= (+ (binary-length buf) count) 1024)
               (progn
                 (flush)
                 (write-binary user-buf stream buf-offset count))
               (progn
                 (binary-append-binary buf user-buf buf-offset count)
                 count)))
       :flush (lambda ()
                (flush))
       :sync (lambda ()
               (flush)
               (sync stream))
       :datasync (lambda ()
                   (flush)
                   (datasync stream))
       :close (lambda ()
                (flush)
                (setq buf nil)
                (close stream))))))

;;---------------------------------------------------------------------------
;; (identity x) ==> x
;;---------------------------------------------------------------------------
(export 'identity)
(defun identity (x) x)

;;---------------------------------------------------------------------------
;; vector-create-ex
;;---------------------------------------------------------------------------
(export 'vector-create-ex)
(defun vector-create-ex (size &optional (init-fn 'identity) max-capacity)
  (let ((v (vector)))
    (if max-capacity (vector-set-max-capacity v max-capacity))
    (vector-ensure-capacity v size)
    (dotimes (i size)
      (vector-append v (funcall init-fn i)))
    v))

;;---------------------------------------------------------------------------
;; vector-shuffle
;;---------------------------------------------------------------------------
(export 'vector-shuffle)
(defun vector-shuffle (v)
  (let ((l (vector-length v))
        new-idx
        tmp)
  (dotimes (i (vector-length v))
    (setq tmp (vector-ref v i))
    (setq new-idx (random l))
    (vector-set v i (vector-ref v new-idx))
    (vector-set v new-idx tmp)))
  v)

;;---------------------------------------------------------------------------
;; to-base-36
;;---------------------------------------------------------------------------
(defun to-base-36 (x)
  (if (zerop x)
      (binary #"0")
      (let ((m "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            (r (binary)))
        (if (< x 0)
            (progn
              (binary-append-char r #"-")
              (setq x (neg x))))
        (while (not (zerop x))
          (binary-append-char r (binary-ref m (rem x 36)))
          (setq x (div x 36)))
        (binary-reverse r))))

;;--------------------------------------------------------------------------
;; end
;;--------------------------------------------------------------------------
:ok
