(time (let ((mutex (mutex-create)))
        (dotimes (i 1000000)
          (with-mutex-lock (mutex) nil)))
      :ops 1000000
      :name "mutex lock/unlock single thread")

(let (threads
      (mutex (mutex-create)))
  (time (progn
          (dotimes (i os:CPU-ONLINE-COUNT)
            (push (thread-create (lambda ()
                                   (dotimes (i 1000000)
                                     (with-mutex-lock (mutex) nil)))
                                 nil
                                 :detached nil
                                 :suppress-closure-warning t)
                  threads))
          (dolist (thr threads)
            (thread-join thr)))
        :ops (* os:CPU-ONLINE-COUNT 1000000)
        :name "mutex lock/unlock in threads"))

(time (dotimes (i 1000000)
        (symbol-create "abc"))
      :ops 1000000
      :name "symbol-create single thread")

(let (threads)
  (time (progn
          (dotimes (i os:CPU-ONLINE-COUNT)
            (push (thread-create (lambda () (dotimes (i 1000000)
                                              (symbol-create "abc")))
                                 nil
                                 :detached nil)
                  threads))
          (dolist (thr threads)
            (thread-join thr)))
        :ops (* os:CPU-ONLINE-COUNT 1000000)
        :name "symbol-create in threads"))

(time (dotimes (i 1000000) (gensym))
      :ops 1000000
      :name "gensym single thread")

(let (threads)
  (time (progn
          (dotimes (i os:CPU-ONLINE-COUNT)
            (push (thread-create (lambda () (dotimes (i 1000000) (gensym)))
                                 nil
                                 :detached nil)
                  threads))
          (dolist (thr threads)
            (thread-join thr)))
        :ops (* os:CPU-ONLINE-COUNT 1000000)
        :name "gensym in threads"))

(time (let ((stream (binary-stream-create)))
        (dotimes (i 10000)
          (fprintf (binary-stream-rewind stream) "sym-~s" i)
          (intern (binary-stream-content stream))))
      :ops 10000
      :name "intern single thread")

(let (threads)
  (time (progn
          (dotimes (i os:CPU-ONLINE-COUNT)
            (push (thread-create (lambda ()
                                   (let ((stream (binary-stream-create)))
                                     (dotimes (i 10000)
                                       (fprintf (binary-stream-rewind stream) "sym-~s" i)
                                       (intern (binary-stream-content stream)))))
                                 nil
                                 :detached nil)
                  threads))
          (dolist (thr threads)
            (thread-join thr)))
        :ops (* os:CPU-ONLINE-COUNT 10000)
        :name "intern in threads"))
