;;-------------------------------------------------------------------
;; Copyright (c) 2009-2012 by Evgeny Khirin.
;;-------------------------------------------------------------------
;;-------------------------------------------------------------------
;; File    : http.lisp
;; Author  : Evgeny Khirin <>
;; Description : HTTP protocol support.
;;-------------------------------------------------------------------

;;--------------------------------------------------------------------------
;; set package
;;--------------------------------------------------------------------------
(in-package http)

;;--------------------------------------------------------------------------
;; ihashmap-create
;;--------------------------------------------------------------------------
(defun ihashmap-create ()
  (hashmap-create 'binary-iequal 'binary-ihash-code))

;;--------------------------------------------------------------------------
;; HTTP transaction
;;--------------------------------------------------------------------------
(defstruct transaction
  server in-stream out-stream method url http-version req-headers resp-headers)

;;--------------------------------------------------------------------------
;; request-host
;;--------------------------------------------------------------------------
(defun request-host (transaction)
  (let ((host (hashmap-lookup (transaction-req-headers transaction) "HOST")))
    (if host (car host))))

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

;;--------------------------------------------------------------------------
;; send-response-headers
;;--------------------------------------------------------------------------
(defun send-response-headers (transaction)
  (let ((send-server-header t)
        (out (transaction-out-stream transaction)))
    (dolist (h (transaction-resp-headers transaction))
      (if (and send-server-header (binary-iequal (car h) "SERVER"))
        (setq send-server-header nil))
      (write-exact (car h) out)
      (write-exact ": " out)
      (write-exact (cdr h) out)
      (write-exact "\r\n" out))
    (if send-server-header (write-exact "Server: replware.com\r\n" out))
    (write-exact "\r\n" out)))

;;--------------------------------------------------------------------------
;; transaction-write
;;--------------------------------------------------------------------------
(defun transaction-write (transaction body)
  (write-exact body (transaction-out-stream transaction)))

;;--------------------------------------------------------------------------
;; get-request-header
;;--------------------------------------------------------------------------
(defun get-request-header (transaction header)
  (signal 'not-implemented 'get-request-header))

;;--------------------------------------------------------------------------
;; client-accepts
;;--------------------------------------------------------------------------
(defun client-accepts (transaction content-type)
  (if content-type
      (let ((accept (get-request-header transaction "ACCEPT")))
        (if accept
            (dolist (a accept)
              (if (or (binary-equal a "*/*")
                      (wildcard-matchp a content-type t))
                  (return-from client-accepts t)))
            t))
      t))

;;--------------------------------------------------------------------------
;; server-send-response
;;--------------------------------------------------------------------------
(export 'server-send-response)
(defun server-send-response (transaction status-code content-type body)
  (unless (client-accepts transaction content-type)
      (return-from server-send-response
        (server-send-response transaction 406 nil nil)))
  (if content-type
      (add-response-header transaction "Content-Type" content-type))
  (if body
      (add-response-header transaction "Content-Length"
                           (to-string (binary-length body))))
  (let ((http-version (transaction-http-version transaction)))
    (cond ((eql http-version 1.1)
           (signal 'not-implemented '(server-send-response "HTTP/1.1")))
          ((eql http-version 1.0)
           (send-status-line transaction status-code)
           (send-response-headers transaction)
           (if body (transaction-write transaction body)))
          ((eql http-version 0.9)
           (if body
               (transaction-write transaction body)
               (progn
                 (transaction-write transaction "<HTML><BODY>")
                 (send-status-line transaction status-code)
                 (transaction-write transaction "</BODY></HTML>")))))))

;;--------------------------------------------------------------------------
;; HTTP virtual host
;;--------------------------------------------------------------------------
(defstruct %vhost
  icase                                 ; case is ignored when request URL is
                                        ; matched
  (handlers-by-method (ihashmap-create)))

;;--------------------------------------------------------------------------
;; HTTP server
;;--------------------------------------------------------------------------
(defstruct %server
  (hosts (ihashmap-create))             ; virtual hosts
  (default-vhost (%vhost-create))       ; default host, when host is not
                                        ; specified by client
  (max-header-line-length 1024)         ; max length of header line
  (max-header-lines 32)                 ; max lines in header section of request
  (not-found-handler                    ; handler for not found URL
   (lambda (transaction)
     (server-send-response transaction 404 nil nil)))
  http-listeners
)

;;--------------------------------------------------------------------------
;; server-create
;;--------------------------------------------------------------------------
(export 'server-create)
(defun server-create (&key (max-header-line-length 1024) (max-header-lines 32))
  (%server-create :max-header-line-length max-header-line-length
                  :max-header-lines max-header-lines))

;;--------------------------------------------------------------------------
;; server-add-vhost
;;--------------------------------------------------------------------------
(export 'server-add-vhost)
(defun server-add-vhost (srv host-names &optional icase)
  (let ((vhost (%vhost-create :icase icase))
        (hosts (%server-hosts srv)))
    (dolist (h host-names)
      (if (hashmap-lookup hosts h)
          (signal 'vhost-already-exists h))
      (hashmap-insert hosts h vhost)))
  nil)

;;--------------------------------------------------------------------------
;; server-add-vhost-aliases
;;--------------------------------------------------------------------------
(export 'server-add-vhost-aliases)
(defun server-add-vhost-aliases (srv host-name aliases)
  (let* ((hosts (%server-hosts srv))
         (vhost (hashmap-lookup hosts host-name)))
    (dolist (h aliases)
      (if (hashmap-lookup hosts h)
          (signal 'vhost-already-exists h))
      (hashmap-insert hosts h vhost)))
  nil)

;;--------------------------------------------------------------------------
;; host-name-to-vhost
;;--------------------------------------------------------------------------
(defun host-name-to-vhost (srv name)
  (if (null name)
      (%server-default-vhost srv)
      (or (hashmap-lookup (%server-hosts srv) name)
          (%server-default-vhost srv))))

;;--------------------------------------------------------------------------
;; host-name-to-vhost-strict
;;--------------------------------------------------------------------------
(defun host-name-to-vhost-strict (srv name)
  (let ((vhost (hashmap-lookup (%server-hosts srv) name)))
    (if vhost
        vhost
        (signal 'vhost-not-found name))))

;;--------------------------------------------------------------------------
;; server-set-default-vhost
;;--------------------------------------------------------------------------
(export 'server-set-default-vhost)
(defun server-set-default-vhost (srv vhost-alias)
  (%server-default-vhost-set srv (host-name-to-vhost-strict srv vhost-alias)))

;;--------------------------------------------------------------------------
;; vhost-set-url-handler
;;--------------------------------------------------------------------------
(defun vhost-set-url-handler (vhost method url-wildcard handler)
  (let* ((handlers-by-method (%vhost-handlers-by-method vhost))
         (handlers (hashmap-lookup handlers-by-method method)))
    (if (not handlers)
        (hashmap-insert handlers-by-method method
                        (setq handlers
                              (wildcard-map-create :icase (%vhost-icase vhost)))))
    (wildcard-map-insert handlers url-wildcard handler)))

;;--------------------------------------------------------------------------
;; vhost-remove-url-handler
;;--------------------------------------------------------------------------
(defun vhost-remove-url-handler (vhost method url-wildcard)
  (let ((handlers (hashmap-lookup (%vhost-handlers-by-method vhost)
                                  method)))
    (if handlers
        (wildcard-map-remove handlers url-wildcard))))

;;--------------------------------------------------------------------------
;; vhost-get-url-handler
;;--------------------------------------------------------------------------
(defun vhost-get-url-handler (vhost method url)
  (let ((handlers (hashmap-lookup (%vhost-handlers-by-method vhost)
                                  method)))
    (if handlers
        (wildcard-map-match handlers url))))

;;--------------------------------------------------------------------------
;; server-set-url-handler
;;--------------------------------------------------------------------------
(export 'server-set-url-handler)
(defun server-set-url-handler (srv host-name method url-wildcard handler)
  (vhost-set-url-handler (host-name-to-vhost srv host-name)
                         method url-wildcard handler))

;;--------------------------------------------------------------------------
;; server-remove-url-handler
;;--------------------------------------------------------------------------
(export 'server-remove-url-handler)
(defun server-remove-url-handler (srv host-name method url-wildcard)
  (vhost-remove-url-handler (host-name-to-vhost srv host-name)
                            method url-wildcard))

;;--------------------------------------------------------------------------
;; server-set-not-found-handler
;;--------------------------------------------------------------------------
(export 'server-set-not-found-handler)
(defun server-set-not-found-handler (srv handler)
  (let ((old-handler (%server-not-found-handler srv)))
    (%server-not-found-handler-set srv handler)
    old-handler))

;;--------------------------------------------------------------------------
;; server-get-url-handler
;;--------------------------------------------------------------------------
(defun server-get-url-handler (srv host-name method url)
  (let ((handler (vhost-get-url-handler (host-name-to-vhost srv host-name)
                                        method url)))
    (if handler
        handler
        (%server-not-found-handler srv))))

**************************

;;--------------------------------------------------------------------------
;; call-url-handler
;;--------------------------------------------------------------------------
(defun call-url-handler (handler request response)
  (if handler
      (funcall handler request response)
      (server-send-response response 404 nil nil)))

;;--------------------------------------------------------------------------
;; handle-http-0.9
;;--------------------------------------------------------------------------
(defun handle-http-0.9 (srv request response)
  (if (not (eq (%request-method request) :GET))
      (server-send-response response 404 nil nil)
      (call-url-handler (server-get-url-handler srv nil :GET (%request-url request))
                        request response)))

;;--------------------------------------------------------------------------
;; handle-http-1.0
;;--------------------------------------------------------------------------
(defun handle-http-1.0 (srv request response)
  (call-url-handler (server-get-url-handler srv (request-host request)
                                            (%request-method request)
                                            (%request-url request))
                    request response))

;;--------------------------------------------------------------------------
;; handle-http-1.1
;;--------------------------------------------------------------------------
(defun handle-http-1.1 (srv request response)
  (signal 'not-implemented 'handle-http-1.1))

;;--------------------------------------------------------------------------
;; request-handler
;;--------------------------------------------------------------------------
(defun request-handler (srv &optional (in *stdin*) (out *stdout*))
  (let ((request-line (read-line in (%server-max-header-line-length srv))))
    (unless request-line (return-from request-handler nil))
    (multiple-value-bind
          (method url version)
        (parse-request-line request-line)
      (let* ((request (%request-create :stream in :method method :url url :version version))
             (response (%response-create :request request :stream out :version version)))
        (if (eq version :HTTP/0.9)
            (return-from request-handler (handle-http-0.9 srv request response)))
        (%request-headers-set request
                              (parse-headers in (%server-max-header-line-length srv)
                                             (%server-max-header-lines srv)))
        (case version
          (:HTTP/1.1 (handle-http-1.1 srv request response))
          (:HTTP/1.0 (handle-http-1.0 srv request response))
          (t (error 'unsupported-http-version
                    "Unsupported HTTP version ~s, request line ~s"
                    version request-line)))))))

;;--------------------------------------------------------------------------
;; server-start-http
;;--------------------------------------------------------------------------
(export 'server-start-http)
(defun server-start-http (srv port &key bind-address
                          (reuse-address t) (backlog 128) (timeout 10)
                          name stack-size)
  (%server-http-listener-set
   srv
   (tcp-server-start port
                     (lambda (stream srv)
                       (request-handler srv stream stream))
                     (list srv)
                     :bind-address bind-address
                     :reuse-address reuse-address
                     :backlog backlog
                     :timeout timeout
                     :name name
                     :stack-size stack-size))
  srv)

;;--------------------------------------------------------------------------
;; server-stop-http
;;--------------------------------------------------------------------------
(export 'server-stop-http)
(defun server-stop-http (srv)
  (tcp-server-stop (%server-http-listener srv))
  (%server-http-listener-set srv nil)
  srv)

;;==========================================================================
;; TESTS
;;==========================================================================
;;--------------------------------------------------------------------------
;; server congig
;;--------------------------------------------------------------------------
(defparameter *srv* (server-create))

(server-add-vhost *srv* '("replware.com" "www.replware.com"))

(server-set-url-handler
 *srv* "replware.com" "GET" "/"
 (lambda (request response)
   (declare (ignore request))
   (server-send-response
    response 200 "text/plain"
    "Hello, world, from replware.com!\n")))

;; set default host
(server-set-url-handler
 *srv* nil "GET" "*"
 (lambda (request response)
   (declare (ignore request))
   (server-send-response
    response 200 "text/plain"
    "Hello, world, from default host!\n")))

;;--------------------------------------------------------------------------
;; simple sanity
;;--------------------------------------------------------------------------
(let ((in (binary-stream-create #s("GET / HTTP/1.0\r\n"
                                   "User-Agent: curl/7.18.0 (i486-pc-linux-gnu) "
                                   "libcurl/7.18.0 OpenSSL/0.9.8g zlib/1.2.3.3 libidn/1.1\r\n"
                                   "Host: www.replware.com\r\n"
                                   "Accept: */*\r\n"
                                   "\r\n"))))
  (request-handler *srv* in *stdout*))

;;--------------------------------------------------------------------------
;; request processing performance
;;--------------------------------------------------------------------------
(symbol-macrolet ((THREADS 4)
                  (REQUESTS 20000))
  (printf "\nTesting HTTP server performance: threads ~d, requests per thread ~d, total ~d"
          THREADS REQUESTS (* THREADS REQUESTS))
  (let ((threads (vector-create THREADS)))
    (time
     (progn
       (dotimes (i THREADS)
         (vector-set threads i
                     (thread-create
                      (lambda ()
                        (let ((in (binary-stream-create #s("GET / HTTP/1.0\r\n"
                                                           "User-Agent: curl/7.18.0 (i486-pc-linux-gnu) "
                                                           "libcurl/7.18.0 OpenSSL/0.9.8g zlib/1.2.3.3 libidn/1.1\r\n"
                                                           "Host: www.replware.com\r\n"
                                                           "Accept: */*\r\n"
                                                           "\r\n")))
                              (out (null-stream-create)))
                          (dotimes (i REQUESTS)
                            (request-handler *srv* (binary-stream-rewind in) out))))
                      nil
                      :detached nil)))
       (dotimes (i THREADS)
         (thread-join (vector-ref threads i)))))))

;;--------------------------------------------------------------------------
;; date parsing performance
;;--------------------------------------------------------------------------
(symbol-macrolet ((LOOPS 50000))
  (printf "\nTesting HTTP date parser: loops ~d" LOOPS)
  (printf "\n\tRFC-822 date")
  (time (dotimes (i LOOPS) (parse-date "Thu, 06 Nov 2014 08:49:37 GMT")))
  (printf "\n\tANSI date")
  (time (dotimes (i LOOPS) (parse-date "Thu Nov  6 08:49:37 2014")))
  (printf "\n\tRFC-850 date")
  (time (dotimes (i LOOPS) (parse-date "Thursday, 06-Nov-14 08:49:37 GMT"))))

;;--------------------------------------------------------------------------
;; start server
;;--------------------------------------------------------------------------
(server-start-http *srv* 11000)

;;--------------------------------------------------------------------------
;; end
;;--------------------------------------------------------------------------
:ok
