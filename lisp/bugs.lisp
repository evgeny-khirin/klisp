;;---------------------------------------------------------------------------
;; test maps performance
;;---------------------------------------------------------------------------
(defun bug-1 ()
  (let ((mem-usage (progn (gc) (gc-heap-usage))))
    (handler-case
        (dotimes (i 10)
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

            (let* ((seq-vec (vector-create-ex 1000000))
                   (time (get-universal-time))
                   (rand-vec (dynamic-let ((*random-state* (make-random-state time)))
                                          (vector-shuffle (vector-create-ex 1000000)))))
              (printf "\nRandom seed ~d" time)

              ;; test hashmap sequentially
              (let ((hash (hashmap-create)))
                (time (test-insert 'hash hash seq-vec 'hashmap-insert 'hashmap-size)
                      :ops 1000000 :name "\nhashmap-insert")
                (time (test-lookup 'hash hash seq-vec 'hashmap-lookup)
                      :ops 1000000 :name "hashmap-lookup")
                (time (test-remove 'hash hash seq-vec 'hashmap-remove 'hashmap-size)
                      :ops 1000000 :name "hashmap-remove"))

              ;; test hashmap randomly
              (let ((hash (hashmap-create)))
                (time (test-insert 'hash hash rand-vec 'hashmap-insert 'hashmap-size)
                      :ops 1000000 :name "\nhashmap-insert-random")
                (time (test-lookup 'hash hash rand-vec 'hashmap-lookup)
                      :ops 1000000 :name "hashmap-lookup-random")
                (time (test-remove 'hash hash rand-vec 'hashmap-remove 'hashmap-size)
                      :ops 1000000 :name "hashmap-remove-random"))

              ;; test STL treemap sequentially
              (let ((tree (treemap-create)))
                (time (test-insert 'tree tree seq-vec 'treemap-insert 'treemap-size)
                      :ops 1000000 :name "\ntreemap-insert")
                (time (test-lookup 'tree tree seq-vec 'treemap-lookup)
                      :ops 1000000 :name "treemap-lookup")
                (time (test-remove 'tree tree seq-vec 'treemap-remove 'treemap-size)
                      :ops 1000000 :name "treemap-remove"))

              ;; test STL treemap randomly
              (let ((tree (treemap-create)))
                (time (test-insert 'tree tree rand-vec 'treemap-insert 'treemap-size)
                      :ops 1000000 :name "\ntreemap-insert-random")
                (time (test-lookup 'tree tree rand-vec 'treemap-lookup)
                      :ops 1000000 :name "treemap-lookup-random")
                (time (test-remove 'tree tree rand-vec 'treemap-remove 'treemap-size)
                      :ops 1000000 :name "treemap-remove-random"))

              ;; test AVL treemap sequentially
              (let* ((avl (avlmap-create)))
                (time (test-insert 'avl avl seq-vec 'avlmap-insert 'avlmap-size)
                      :ops 1000000 :name "\navlmap-insert")
                (time (test-lookup 'avl avl seq-vec 'avlmap-lookup)
                      :ops 1000000 :name "avlmap-lookup")
                (time (test-remove 'avl avl seq-vec 'avlmap-remove 'avlmap-size)
                      :ops 1000000 :name "avlmap-remove"))

              ;; test AVL treemap randomly
              (let* ((avl (avlmap-create)))
                (time (test-insert 'avl avl rand-vec 'avlmap-insert 'avlmap-size)
                      :ops 1000000 :name "\navlmap-insert-random")
                (time (test-lookup 'avl avl rand-vec 'avlmap-lookup)
                      :ops 1000000 :name "avlmap-lookup-random")
                (time (test-remove 'avl avl rand-vec 'avlmap-remove 'avlmap-size)
                      :ops 1000000 :name "avlmap-remove-random"))

              (print "No bug uccured")
              nil)))
      (out_of_memory (size)
        (printf #s("Failed to allocate ~d bytes\n"
                   "Heap free before gc ~d bytes\n"
                   "Heap free after gc ~d bytes\n"
                   "Heap leak ~d bytes\n")
                size (- (gc-heap-size) (gc-heap-usage))
                (progn (gc) (- (gc-heap-size) (gc-heap-usage)))
                (- (gc-heap-usage) mem-usage))))))
