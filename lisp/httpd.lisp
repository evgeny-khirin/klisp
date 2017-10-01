;;-------------------------------------------------------------------
;; Copyright (c) 2009-2012 by Evgeny Khirin.
;;-------------------------------------------------------------------
;;-------------------------------------------------------------------
;; File    : httpd.lisp
;; Author  : Evgeny Khirin <>
;; Description : Minimal HTTP server.
;;-------------------------------------------------------------------

;;--------------------------------------------------------------------------
;; set package
;;--------------------------------------------------------------------------
(in-package httpd)

;;--------------------------------------------------------------------------
;; HTTP transaction
;;--------------------------------------------------------------------------
(defstruct transaction
  server in-stream out-stream method url http-version req-headers resp-headers)

;;--------------------------------------------------------------------------
;; add-response-header
;;--------------------------------------------------------------------------
(defun add-response-header (transaction header value)
  (transaction-resp-headers-set transaction
                                (cons (cons header value)
                                      (transaction-resp-headers transaction))))

;;--------------------------------------------------------------------------
;; send-status-line
;;--------------------------------------------------------------------------
(defun send-status-line (transaction status-code)
  (fprintf (transaction-out-stream transaction) "HTTP/~2.1f ~d ~a\r\n"
           (transaction-http-version transaction))
           status-code (status-code-to-reason status-code))

***********************************
perforamnce tests
***********************************
(defun read-request (stream)
  (let ((line (binary)))
    (tagbody
     again
     (setq line (read-line stream 1024 (binary-clear line)))
     (unless line
       (return-from read-request nil))
     (if (binary-equal line "\r\n")
         (return-from read-request t))
     (go again))))

(defun server-fun (in-stream out-stream)
  (if (read-request in-stream)
      (write-exact #s("HTTP/1.1 200 OK\r\n"
                      "Connection: Close\r\n"
                      "Content-Length: 90\r\n"
                      "Content-Type: text/html\r\n"
                      "Date: Tue, 19 Aug 2014 10:05:24 GMT\r\n"
                      "\r\n"
                      "<html><head><title>libmicrohttpd demo</title></head>"
                      "<body>libmicrohttpd demo</body></html>")
                   out-stream)))

(defparameter *srv-11000*
  (tcp-listener-start 11000
                      (lambda (sock)
                        (thread-create
                         (lambda ()
                           (with-open-stream (stream (socket-stream-create sock))
                             (server-fun (create-buffered-input-stream stream)
                                         (create-buffered-output-stream stream))))
                         nil
                         :stack-size (* 32 1024)
                         :suppress-closure-warning t))
                      nil
                      :timeout 10
                      :stack-size (* 32 1024)))

(defparameter *srv-11001*
  (tcp-listener-start 11001
                      (lambda (sock)
                        (with-open-stream (stream (socket-stream-create sock))
                          (server-fun (create-buffered-input-stream stream)
                                      (create-buffered-output-stream stream))))
                      nil
                      :timeout 10
                      :stack-size (* 32 1024)))

(let* ((N 100000)
       (C 10)
       (REQ-PER-THREAD (div N C))
       (threads (vector-create C)))
  (printf "\nExecuting ~d requests in ~d threads (~d requests per thread)\n"
          (* REQ-PER-THREAD C) C REQ-PER-THREAD)
  (time
   (progn
     (dotimes (i C)
       (vector-set threads i
                   (thread-create
                    (lambda ()
                      (time
                       (let ((buf (binary)))
                         (dotimes (i REQ-PER-THREAD)
                           (handler-case
                               (with-open-stream (s (socket-stream-create
                                                     (tcp-connect "localhost" 11001 :timeout 10)
                                                     10))
                                 (write-exact #s("GET / HTTP/1.1\r\n"
                                                 "User-Agent: curl/7.21.0 (x86_64-pc-linux-gnu) "
                                                 "libcurl/7.21.0 OpenSSL/0.9.8o "
                                                 "zlib/1.2.3.4 libidn/1.15 libssh2/1.2.6\r\n"
                                                 "Host: localhost:11001"
                                                 "Accept: */*\r\n"
                                                 "\r\n")
                                              s)
                                 (while (read-binary 1024 s (binary-clear buf))
                                   nil))
                             (t (label value) (printf "\nClient unhandled signal (~S ~S)"
                                                      label value)))))
                       :gc nil
                       :ops REQ-PER-THREAD))
                    nil
                    :stack-size (* 32 1024)
                    :detached nil
                    :suppress-closure-warning t)))
     (dotimes (i C)
       (thread-join (vector-ref threads i))))
   :ops N))
