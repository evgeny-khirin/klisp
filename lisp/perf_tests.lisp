;;---------------------------------------------------------------------------
;; test tagbody
;;---------------------------------------------------------------------------
(flet ((test-tagbody (n)
         (tagbody
          again
            (if (zerop n)
                (go end)
                (setq n (1- n)))
            (go again)
          end)))
  (time (test-tagbody 1000000) :ops 1000000 :name "tagbody-test"))

;;---------------------------------------------------------------------------
;; test function call by name
;;---------------------------------------------------------------------------
(flet ((test-call-by-name (n)
         (tagbody
          again
            (if (zerop n)
                (go end)
                (progn (identity n)
                       (setq n (1- n))))
            (go again)
          end)))
  (time (test-call-by-name 1000000) :ops 1000000 :name "call-by-name"))

;;---------------------------------------------------------------------------
;; test local function call
;;---------------------------------------------------------------------------
(flet ((test-call-local-fun (n)
         (flet ((f (x) x))
           (tagbody
            again
              (if (zerop n)
                  (go end)
                  (progn (f n)
                         (setq n (1- n))))
              (go again)
            end))))
  (time (test-call-local-fun 1000000) :ops 1000000 :name "call-local-fun"))

;;---------------------------------------------------------------------------
;; test funcall
;;---------------------------------------------------------------------------
(flet ((test-funcall (n)
         (tagbody
          again
            (if (zerop n)
                (go end)
                (progn (funcall 'identity n)
                       (setq n (1- n))))
            (go again)
          end)))
  (time (test-funcall 1000000) :ops 1000000 :name "test-funcall"))

;;---------------------------------------------------------------------------
;; test apply
;;---------------------------------------------------------------------------
(flet ((test-apply (n)
         (tagbody
          again
            (if (zerop n)
                (go end)
                (progn (apply 'identity '(1))
                (setq n (1- n))))
            (go again)
          end)))
  (time (test-apply 1000000) :ops 1000000 :name "test-apply"))

;;---------------------------------------------------------------------------
;; test apply-vector
;;---------------------------------------------------------------------------
(flet ((test-apply-vector (n)
         (tagbody
          again
            (if (zerop n)
                (go end)
                (progn (apply-vector 'identity #(1))
                (setq n (1- n))))
            (go again)
          end)))
  (time (test-apply-vector 1000000) :ops 1000000 :name "test-apply-vector"))

;;---------------------------------------------------------------------------
;; test function with optional arguments
;;---------------------------------------------------------------------------
(flet ((test-call-optional-args (n)
         (flet ((f (&optional x) x))
           (tagbody
            again
              (if (zerop n)
                  (go end)
                  (progn (f)
                         (setq n (1- n))))
              (go again)
            end))))
  (time (test-call-optional-args 1000000) :ops 1000000 :name "call-optional-args"))

;;---------------------------------------------------------------------------
;; test function with key arguments
;;---------------------------------------------------------------------------
(flet ((test-call-key-args (n)
         (flet ((f (&key x) x))
           (tagbody
            again
              (if (zerop n)
                  (go end)
                  (progn (f)
                         (setq n (1- n))))
              (go again)
            end))))
  (time (test-call-key-args 1000000) :ops 1000000 :name "call-key-args"))

;;---------------------------------------------------------------------------
;; test &rest arguments construction
;;---------------------------------------------------------------------------
(flet ((test-rest-args-construction (n)
         (flet ((f (&rest x) x))
           (tagbody
            again
              (if (zerop n)
                  (go end)
                  (progn (f n n n n n n n n n n)
                         (setq n (1- n))))
              (go again)
            end))))
  (time (test-rest-args-construction 1000000) :ops 1000000 :name "rest-args-construction"))

;;---------------------------------------------------------------------------
;; test binary-append-char for 1MB
;;---------------------------------------------------------------------------
(flet ((test-binary-append-char (n)
         (let ((b (binary-create n)))
           (tagbody
            again
              (if (zerop n)
                  (go end)
                  (progn (binary-append-char b 1)
                         (setq n (1- n))))
              (go again)
            end))))
  (time (test-binary-append-char 1000000) :ops 1000000 :name "binary-append-char"))

;;---------------------------------------------------------------------------
;; test unwind-protect
;;---------------------------------------------------------------------------
(flet ((test-unwind-protect (n)
         (tagbody
          again
            (if (zerop n)
                (go end)
                (unwind-protect (identity n)
                  (setq n (1- n))))
            (go again)
          end)))
  (time (test-unwind-protect 1000000) :ops 1000000 :name "unwind-protect"))

;;---------------------------------------------------------------------------
;; test catch/throw
;;---------------------------------------------------------------------------
(flet ((test-catch-throw (n)
         (tagbody
          again
            (if (zerop n)
                (go end)
                (catch 'ttt
                  (setq n (1- n))
                  (throw 'ttt)))
            (go again)
          end)))
  (time (test-catch-throw 1000000) :ops 1000000 :name "catch-throw"))

;;---------------------------------------------------------------------------
;; test GC performance
;;---------------------------------------------------------------------------
(flet ((test-gc (n)
         (tagbody
          again
            (if (zerop n)
                (go end)
                (progn (cons n n)
                       (setq n (1- n))))
            (go again)
          end)))
  (time (test-gc 1000000) :ops 1000000 :name "test-gc"))

;;---------------------------------------------------------------------------
;; test maps performance
;;---------------------------------------------------------------------------
(flet ((test-insert (map-name tbl v insert-fn size-fn)
         (dotimes (i (vector-length v))
           (let* ((n (vector-ref v i))
                  (old-val (funcall insert-fn tbl n (- n))))
             (if old-val
                 (signal `(insert-bug ,map-name)
                         `((i ,i) (n ,n) (old-val ,old-val))))))
         (unless (eql (funcall size-fn tbl) (vector-length v))
           (signal `(insert-size-bug ,map-name))))

       (test-lookup (map-name tbl v lookup-fn)
         (dotimes (i (vector-length v))
           (let* ((n (vector-ref v i))
                  (found-val (funcall lookup-fn tbl n)))
             (unless (eql found-val (- n))
               (signal `(lookup-bug ,map-name)
                       `((i ,i) (n ,n) (found-val ,found-val)))))))

       (test-remove (map-name tbl v remove-fn size-fn)
         (dotimes (i (vector-length v))
           (let* ((n (vector-ref v i))
                  (old-val (funcall remove-fn tbl n)))
             (unless (eql old-val (- n))
               (signal `(remove-bug ,map-name)
                       `((i ,i) (n ,n) (old-val ,old-val))))))
         (unless (zerop (funcall size-fn tbl))
           (signal `(remove-size-bug ,map-name)))))

  (let* ((hash (hashmap-create))
         (tree (treemap-create))
         (avl (avlmap-create))
         (seq-vec (vector-create-ex 1000000))
         (time (get-universal-time))
         (rand-vec (dynamic-let ((*random-state* (make-random-state time)))
                    (vector-shuffle (vector-create-ex 1000000)))))
    (printf "\nRandom seed ~d" time)

    ;; test hashmap sequentially
    (time (test-insert 'hash hash seq-vec 'hashmap-insert 'hashmap-size)
          :ops 1000000 :name "\nhashmap-insert")
    (time (test-lookup 'hash hash seq-vec 'hashmap-lookup)
          :ops 1000000 :name "hashmap-lookup")
    (time (test-remove 'hash hash seq-vec 'hashmap-remove 'hashmap-size)
          :ops 1000000 :name "hashmap-remove")

    ;; test hashmap randomly
    (time (test-insert 'hash hash rand-vec 'hashmap-insert 'hashmap-size)
          :ops 1000000 :name "\nhashmap-insert-random")
    (time (test-lookup 'hash hash rand-vec 'hashmap-lookup)
          :ops 1000000 :name "hashmap-lookup-random")
    (time (test-remove 'hash hash rand-vec 'hashmap-remove 'hashmap-size)
          :ops 1000000 :name "hashmap-remove-random")

    ;; test STL treemap sequentially
    (time (test-insert 'tree tree seq-vec 'treemap-insert 'treemap-size)
          :ops 1000000 :name "\ntreemap-insert")
    (time (test-lookup 'tree tree seq-vec 'treemap-lookup)
          :ops 1000000 :name "treemap-lookup")
    (time (test-remove 'tree tree seq-vec 'treemap-remove 'treemap-size)
          :ops 1000000 :name "treemap-remove")

    ;; test STL treemap randomly
    (time (test-insert 'tree tree rand-vec 'treemap-insert 'treemap-size)
          :ops 1000000 :name "\ntreemap-insert-random")
    (time (test-lookup 'tree tree rand-vec 'treemap-lookup)
          :ops 1000000 :name "treemap-lookup-random")
    (time (test-remove 'tree tree rand-vec 'treemap-remove 'treemap-size)
          :ops 1000000 :name "treemap-remove-random")

    ;; test AVL treemap sequentially
    (time (test-insert 'avl avl seq-vec 'avlmap-insert 'avlmap-size)
          :ops 1000000 :name "\navlmap-insert")
    (time (test-lookup 'avl avl seq-vec 'avlmap-lookup)
          :ops 1000000 :name "avlmap-lookup")
    (time (test-remove 'avl avl seq-vec 'avlmap-remove 'avlmap-size)
          :ops 1000000 :name "avlmap-remove")

    ;; test AVL treemap randomly
    (time (test-insert 'avl avl rand-vec 'avlmap-insert 'avlmap-size)
          :ops 1000000 :name "\navlmap-insert-random")
    (time (test-lookup 'avl avl rand-vec 'avlmap-lookup)
          :ops 1000000 :name "avlmap-lookup-random")
    (time (test-remove 'avl avl rand-vec 'avlmap-remove 'avlmap-size)
          :ops 1000000 :name "avlmap-remove-random")))

;;---------------------------------------------------------------------------
;; test maps conversion performance
;;---------------------------------------------------------------------------
(let ((m (treemap-create))
      (v (vector)))
  (vector-ensure-capacity v 1000000)
  (dotimes (i 1000000)
    (treemap-insert m i i))
  (time (treemap-to-vector m v) :ops 1 :name "treemap-to-vector")
  nil)

(let ((m (avlmap-create)))
  (dotimes (i 10)
    (avlmap-insert m i i))
  (avlmap-to-vector m))

;;---------------------------------------------------------------------------
;; test mutex performance
;;---------------------------------------------------------------------------
(let ((m (mutex-create))
      (threads (vector-create 4)))
  (dotimes (i 4)
    (printf "\nMutex ~d thread(s) 1000000 ops per thread" (1+ i))
    (time
     (progn (dotimes (j (1+ i))
              (vector-set threads j
                          (thread-create
                           (lambda ()
                             (time (dotimes (i 1000000)
                                     (mutex-unlock (mutex-lock m)))
                                   :gc nil
                                   :ops 1000000))
                           nil
                           :detached nil
                           :suppress-closure-warning t)))
          (dotimes (j (1+ i))
            (thread-join (vector-ref threads j))))
     :ops (* 1000000 (1+ i)))))

;;---------------------------------------------------------------------------
;; test rwlock read performance
;;---------------------------------------------------------------------------
(let ((m (rwlock-create))
      (threads (vector-create 4)))
  (dotimes (i 4)
    (printf "\nRWLOCK read lock ~d thread(s) 1000000 ops per thread" (1+ i))
    (time
     (progn (dotimes (j (1+ i))
              (vector-set threads j
                          (thread-create
                           (lambda ()
                             (time (dotimes (i 1000000)
                                     (rwlock-unlock (rwlock-rdlock m)))
                                   :gc nil
                                   :ops 1000000))
                           nil
                           :detached nil
                           :suppress-closure-warning t)))
          (dotimes (j (1+ i))
            (thread-join (vector-ref threads j))))
     :ops (* 1000000 (1+ i)))))

;;---------------------------------------------------------------------------
;; test rwlock write performance
;;---------------------------------------------------------------------------
(let ((m (rwlock-create))
      (threads (vector-create 4)))
  (dotimes (i 4)
    (printf "\nRWLOCK write lock ~d thread(s) 1000000 ops per thread" (1+ i))
    (time
     (progn (dotimes (j (1+ i))
              (vector-set threads j
                          (thread-create
                           (lambda ()
                             (time (dotimes (i 1000000)
                                     (rwlock-unlock (rwlock-wrlock m)))
                                   :gc nil
                                   :ops 1000000))
                           nil
                           :detached nil
                           :suppress-closure-warning t)))
          (dotimes (j (1+ i))
            (thread-join (vector-ref threads j))))
     :ops (* 1000000 (1+ i)))))

;;---------------------------------------------------------------------------
;; test semaphore synchronous post
;;---------------------------------------------------------------------------
(let ((s (semaphore-create)))
  (printf "\nSynchronous semaphore post-wait in single thread 1000000 ops")
  (time
   (dotimes (i 1000000)
     (semaphore-post s)
     (semaphore-wait s))
   :ops 1000000))

;;---------------------------------------------------------------------------
;; test semaphore post performance
;;---------------------------------------------------------------------------
(let ((s (semaphore-create))
      producer consumer)
  (printf "\nSemaphore post-wait between 2 thread(s) 1000000 ops")
  (time
   (progn
     (setq consumer
           (thread-create
            (lambda ()
              (time (dotimes (i 1000000) (semaphore-wait s))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (setq producer
           (thread-create
            (lambda ()
              (time (dotimes (i 1000000) (semaphore-post s))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (thread-join producer)
     (thread-join consumer))
   :ops 1000000))

;;---------------------------------------------------------------------------
;; test semaphore RPC performance
;;---------------------------------------------------------------------------
(let ((s1 (semaphore-create))
      (s2 (semaphore-create))
      (OPS 10000)
      producer consumer)
  (printf "\nDual semaphore RPC between 2 thread(s) ~d ops" OPS)
  (time
   (progn
     (setq consumer
           (thread-create
            (lambda ()
              (time (dotimes (i OPS) (semaphore-wait s1) (semaphore-post s2))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (setq producer
           (thread-create
            (lambda ()
              (time (dotimes (i OPS) (semaphore-post s1) (semaphore-wait s2))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (thread-join producer)
     (thread-join consumer))
   :ops OPS))

;;---------------------------------------------------------------------------
;; test dual condition RPC performance
;;---------------------------------------------------------------------------
(let ((c1 (condition-create))
      (m1 (mutex-create))
      (c2 (condition-create))
      (m2 (mutex-create))
      (OPS 10000)
      producer consumer)
  (printf "\nDual condition RPC between 2 thread(s) ~d ops" OPS)
  (time
   (progn
     (setq consumer
           (thread-create
            (lambda ()
              (time (with-mutex-lock (m1)
                      (dotimes (i OPS)
                        (condition-wait c1 m1)
                        (with-mutex-lock (m2)
                          (condition-signal c2))))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (setq producer
           (thread-create
            (lambda ()
              (time (with-mutex-lock (m2)
                      (dotimes (i OPS)
                        (with-mutex-lock (m1)
                          (condition-signal c1))
                        (condition-wait c2 m2)))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (thread-join producer)
     (thread-join consumer))
   :ops OPS))

;;---------------------------------------------------------------------------
;; test tqueue overall performance
;;---------------------------------------------------------------------------
(let ((q (tqueue-create)))
  (printf "\nTQUEUE synchronous rpc within single thread 1000000 ops")
  (time
   (dotimes (i 1000000)
     (tqueue-push q i)
     (unless (eq (tqueue-pop q) i)
       (signal 'invalid-object-from-tqueue)))
   :ops 1000000))

;;---------------------------------------------------------------------------
;; test tqueue post performance
;;---------------------------------------------------------------------------
(let ((q (tqueue-create))
      producer consumer)
  (printf "\nTQUEUE post between 2 thread(s) 1000000 ops")
  (time
   (progn
     (setq consumer
           (thread-create
            (lambda ()
              (time (dotimes (i 1000000) (unless (eq (tqueue-pop q) i)
                                           (signal
                                            'invalid-object-from-tqueue)))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (setq producer
           (thread-create
            (lambda ()
              (time (dotimes (i 1000000) (tqueue-push q i))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (thread-join producer)
     (thread-join consumer))
   :ops 1000000))

;;---------------------------------------------------------------------------
;; test tqueue rpc performance
;;---------------------------------------------------------------------------
(let ((q1 (tqueue-create))
      (q2 (tqueue-create))
      (OPS 1000000)
      producer consumer)
  (printf "\nTQUEUE rpc between 2 thread(s) ~d ops" OPS)
  (time
   (progn
     (setq consumer
           (thread-create
            (lambda ()
              (time (dotimes (i OPS)
                      (unless (eq (tqueue-pop q1) i)
                        (signal 'invalid-object-from-tqueue-1))
                      (tqueue-push q2 i))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (setq producer
           (thread-create
            (lambda ()
              (time (dotimes (i OPS)
                      (tqueue-push q1 i)
                      (unless (eq (tqueue-pop q2) i)
                        (signal 'invalid-object-from-tqueue-2)))
                    :gc nil))
            nil
            :detached nil
            :suppress-closure-warning t))
     (thread-join producer)
     (thread-join consumer))
   :ops OPS))

;;--------------------------------------------------------------------------
;; all tests finished
;;--------------------------------------------------------------------------
:ok

