;;--------------------------------------------------------------------------
;; Requires boot.lisp to be loaded
;;--------------------------------------------------------------------------
(defmacro assert (exp)
  `(if ,exp
       :ok
       (signal 'assertion-failed ',exp)))

;;--------------------------------------------------------------------------
;; catch
;;--------------------------------------------------------------------------
(assert (eql 10 (catch 'ttt (throw 'ttt 10))))
(assert (eql 10 (let ((l 'ttt))
                  (catch l (throw l 10)))))
(assert (eql 10 (let ((l 'ttt))
                  (catch l (funcall (lambda () (throw l 10)))))))
(assert (eq 'handled (handler-case (assert (eql 1 (catch 'ttt (throw 'ttt 10))))
                       (assertion-failed () 'handled))))
(assert (eql 10 (catch 'ttt (funcall (lambda () (throw 'ttt 10))))))
(assert (equal '(ttt 10) (handler-case (throw 'ttt 10)
                           (no_catch (val) val))))
(assert (equal '(ttt 10) (handler-case (funcall (lambda () (throw 'ttt 10)))
                           (no_catch (val) val))))
(assert (equal '(a b c x y z)
               (let ((a 'a)
                     (b 'b)
                     (c 'c))
                 (catch 'ttt
                   (let ((x 'x)
                         (y 'y)
                         (z 'z))
                     (throw 'ttt (list a b c x y z)))))))
(assert (equal '(a b c x y z)
               (let ((a 'a) (b 'b) (c 'c))
                 (catch 'ttt
                   (let ((x 'x)
                         (y 'y)
                         (z 'z))
                     (funcall (lambda ()
                                (throw 'ttt (list a b c x y z)))))))))

;;--------------------------------------------------------------------------
;; tagbody
;;--------------------------------------------------------------------------
(assert (eql 10 (let (f)
                  (tagbody
                     (go start)
                   trap
                     (setq f 10)
                     (go end)
                   start
                     (setq f 1)
                     (go trap)
                   end)
                  f)))
(assert (eql 10 (let (f)
                  (tagbody
                     (go start)
                   trap
                     (setq f 10)
                     (go end)
                   start
                     (setq f 1)
                     (funcall (lambda () (go trap)))
                   end)
                  f)))
(assert (eq 'handled
            (handler-case (funcall (let (f)
                                     (tagbody
                                        (go start)
                                      trap
                                        (signal 'bug-tagbody)
                                        (go end)
                                      start
                                        (setq f (lambda () (go trap)))
                                      end)
                                     f))
              (no_catch () 'handled))))

;;--------------------------------------------------------------------------
;; test default arguments evaluation
;;--------------------------------------------------------------------------
(assert (eql 6 ((lambda (x &optional (y (1+ x)) (z (1+ y))) (+ x y z)) 1)))
(assert (eql 7 ((lambda (x &key (y (1+ x)) (z (1+ y))) (+ x y z)) 1 :z 4)))

;;---------------------------------------------------------------------------
;; test closure
;;---------------------------------------------------------------------------
(flet ((test-closure (x)
         (lambda (y)
           (let ((z (+ x y)))
             (lambda (w)
               (+ w z))))))
  (assert (eql 6 (funcall (funcall (test-closure 1) 2) 3))))

;;--------------------------------------------------------------------------
;; test closures in lambda list
;;--------------------------------------------------------------------------
(flet ((make-adder (delta)
         (lambda (x &optional (delta delta))
           (+ x delta))))
  (assert (eql 11 (funcall (make-adder 10) 1)))
  (assert (eql 3 (funcall (make-adder 10) 1 2))))

(flet ((make-adder (delta &optional (adder (lambda (x) (+ x delta))))
         adder))
  (assert (eql 11 (funcall (make-adder 10) 1))))

;;--------------------------------------------------------------------------
;; wildcard tests, also tests binary-prefix-map functions
;;--------------------------------------------------------------------------
(print '==================================================)
(print 'wildcard-tests)
(let ((m (wildcard-map-create :icase t))
      exact-keys exact-values
      prefix-keys prefix-values
      reverse-keys reverse-values
      enum-keys enum-values
      (N 10000)
      (enum-N 1000))
  (print `(generate ,N exact keys/values))
  (print `(generate ,N prefix keys/values))
  (print `(generate ,N reverse-prefix keys/values))
  (print `(generate ,enum-N enum keys/values))
  (time
   (progn
     (dotimes (i N)
       (push (binary "/abcdefgh/E/" (to-string i)) exact-keys)
       (push (binary "/ABCDEFGH/e/" (to-string i)) exact-values)
       (push (binary "/abcdefgh/P/" (to-string i) "/*") prefix-keys)
       (push (binary "/ABCDEFGH/p/" (to-string i) #"/" (to-string i)) prefix-values)
       (push (binary "*/R/" (to-string i)) reverse-keys)
       (push (binary "/ABCDEFGH/r/" (to-string i)) reverse-values))
     (dotimes (i enum-N)
       (push (binary "*/K/" (to-string i) "/K/*") enum-keys)
       (push (binary "/abcdefgh/k/" (to-string i) "/k/abcdefgh") enum-values))))

  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= 0 total-size) (= 0 prefix-size)
                 (= 0 reverse-size) (= 0 enum-size)
                 (wildcard-map-emptyp m))
      (signal 'bug 'wildcard-map-size-0)))
  (print 'insert-exact)
  (let ((vals exact-values))
    (time (dolist (s exact-keys)
            (wildcard-map-insert m s (car vals))
            (setq vals (cdr vals)))
          :ops N))
  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= N total-size) (= N prefix-size)
                 (= 0 reverse-size) (= 0 enum-size)
                 (not (wildcard-map-emptyp m)))
      (printf "\ntotal-size ~d, prefix-size ~d, reverse-size ~d, enum-size ~d"
              total-size prefix-size reverse-size enum-size)
      (signal 'bug 'wildcard-map-size-1)))
  (print 'insert-prefix)
  (let ((vals prefix-values))
    (time (dolist (s prefix-keys)
            (wildcard-map-insert m s (car vals))
            (setq vals (cdr vals)))
          :ops N))
  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= (* 2 N) total-size) (= (* 2 N) prefix-size)
                 (= 0 reverse-size) (= 0 enum-size)
                 (not (wildcard-map-emptyp m)))
      (printf "\ntotal-size ~d, prefix-size ~d, reverse-size ~d, enum-size ~d"
              total-size prefix-size reverse-size enum-size)
      (signal 'bug 'wildcard-map-size-2)))
  (print 'insert-reverse-prefix)
  (let ((vals reverse-values))
    (time (dolist (s reverse-keys)
            (wildcard-map-insert m s (car vals))
            (setq vals (cdr vals)))
          :ops N))
  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= (* 3 N) total-size) (= (* 2 N) prefix-size)
                 (= N reverse-size) (= 0 enum-size)
                 (not (wildcard-map-emptyp m)))
      (printf "\ntotal-size ~d, prefix-size ~d, reverse-size ~d, enum-size ~d"
              total-size prefix-size reverse-size enum-size)
      (signal 'bug 'wildcard-map-size-3)))
  (print 'insert-enums)
  (let ((vals enum-values))
    (time (dolist (s enum-keys)
            (wildcard-map-insert m s (car vals))
            (setq vals (cdr vals)))
          :ops enum-N))
  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= (+ enum-N (* 3 N)) total-size) (= (* 2 N) prefix-size)
                 (= N reverse-size) (= enum-N enum-size)
                 (not (wildcard-map-emptyp m)))
      (printf "\ntotal-size ~d, prefix-size ~d, reverse-size ~d, enum-size ~d"
              total-size prefix-size reverse-size enum-size)
      (signal 'bug 'wildcard-map-size-4)))

  (print 'search-exact)
  (time (dolist (s exact-values)
          (if (not (equal s (wildcard-map-match m s)))
              (signal 'bug 'wildcard-map-match-1)))
        :ops N)
  (print 'search-prefix)
  (time (dolist (s prefix-values)
          (unless (equal s (wildcard-map-match m s))
              (printf "\nValue ~s, match ~s"
                      s (wildcard-map-match m s))
              (signal 'bug 'wildcard-map-match-2)))
        :ops N)
  (print 'search-reverse-prefix)
  (time (dolist (s reverse-values)
          (if (not (equal s (wildcard-map-match m s)))
              (signal 'bug 'wildcard-map-match-3)))
        :ops N)
  (print 'search-enums)
  (time (dolist (s enum-values)
          (if (not (equal s (wildcard-map-match m s)))
              (signal 'bug 'wildcard-map-match-4)))
        :ops enum-N)

  (print 'remove-exact)
  (let ((vals exact-values))
    (time (dolist (s exact-keys)
            (if (not (equal (car vals) (wildcard-map-remove m s)))
                (signal 'bug 'wildcard-map-remove-1))
            (setq vals (cdr vals)))
          :ops N))
  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= (+ enum-N (* 2 N)) total-size) (= N prefix-size)
                 (= N reverse-size) (= enum-N enum-size)
                 (not (wildcard-map-emptyp m)))
      (signal 'bug 'wildcard-map-size-5)))
  (print 'remove-prefix)
  (let ((vals prefix-values))
    (time (dolist (s prefix-keys)
            (if (not (equal (car vals) (wildcard-map-remove m s)))
                (signal 'bug 'wildcard-map-remove-2))
            (setq vals (cdr vals)))
          :ops N))
  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= (+ enum-N N) total-size) (= 0 prefix-size)
                 (= N reverse-size) (= enum-N enum-size)
                 (not (wildcard-map-emptyp m)))
      (signal 'bug 'wildcard-map-size-6)))
  (print 'remove-reverse-prefix)
  (let ((vals reverse-values))
    (time (dolist (s reverse-keys)
            (if (not (equal (car vals) (wildcard-map-remove m s)))
                (signal 'bug 'wildcard-map-remove-2))
            (setq vals (cdr vals)))
          :ops N))
  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= enum-N total-size) (= 0 prefix-size)
                 (= 0 reverse-size) (= enum-N enum-size)
                 (not (wildcard-map-emptyp m)))
      (signal 'bug 'wildcard-map-size-7)))
  (print 'remove-enums)
  (let ((vals enum-values))
    (time (dolist (s enum-keys)
            (if (not (equal (car vals) (wildcard-map-remove m s)))
                (signal 'bug 'wildcard-map-remove-3))
            (setq vals (cdr vals)))
          :ops enum-N))
  (multiple-value-bind (total-size prefix-size reverse-size enum-size)
      (wildcard-map-size m)
    (unless (and (= 0 total-size) (= 0 prefix-size)
                 (= 0 reverse-size) (= 0 enum-size)
                 (wildcard-map-emptyp m))
      (signal 'bug 'wildcard-map-size-8))))

;;--------------------------------------------------------------------------
;; all tests finished
;;--------------------------------------------------------------------------
:ok
