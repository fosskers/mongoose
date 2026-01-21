(in-package :mongoose)

;; --- STRINGS --- ;;

(define-alien-type str
    (struct str
            (buf (* char))
            (len size-t)))

;; --- DNS --- ;;

(define-alien-type dns
    (struct dns
            (url c-string)
            (connection (* (struct connection)))))

;; --- ADDRESSES --- ;;

(define-alien-type addr
    (struct addr
            (ip (union nil
                       (ip  (array (unsigned 8) 16))
                       (ip4 (unsigned 32))
                       (ip6 (array (unsigned 64) 2))))
            (port (unsigned 16))
            (scope-id (unsigned 8))
            (is-ip6 (boolean 8))))

;; --- EVENT MANAGER --- ;;

(define-alien-type mgr
    (struct mgr
            (conns (* (struct connection)))
            (dns4 dns)
            (dns6 dns)
            (dnstimeout (signed 32))
            (use-dns6 (boolean 8))
            (nextid (unsigned 64))
            (userdata (* t))
            (tls-ctx (* t))
            (mqtt-id (unsigned 16))
            (active-dns-requests (* t))
            ;; mg_timer
            ;;
            ;; The Lisp-level user is unlikely to ever need to manipulate this
            ;; directly.
            (timers (* t))
            (epoll-fd (signed 32))
            ;; mg_tcpip_if
            ;;
            ;; The Lisp-level user is unlikely to ever need to manipulate this
            ;; directly.
            (ifp (* t))
            (extraconnsize size-t)
            ;; NOTE: 2025-12-30 Assumed to have no support for `FREERTOS_TCP'.
            (pipe (signed 32))))

(define-alien-routine ("mg_mgr_init" mgr-init) void
  "Initialize an event manager."
  (mgr (* mgr)))

(define-alien-routine ("mg_mgr_free" mgr-free) void
  "Free the `mgr' memory."
  (mgr (* mgr)))

(define-alien-routine ("mg_mgr_poll" mgr-poll) void
  (mgr (* mgr))
  (ms int))

;; --- IO BUFFER --- ;;

(define-alien-type iobuf
    (struct iobuf
            (buf (* unsigned-char))
            (size size-t)
            (len size-t)
            (align size-t)))

;; --- CONNECTIONS --- ;;

(define-alien-type connection
    (struct connection
            (next (* (struct connection)))
            (mgr (* mgr))
            (loc addr)
            (rem addr)
            (fd (* t))
            (id (unsigned 64))
            (recv iobuf)
            (send iobuf)
            (prof iobuf)
            (rtls iobuf)
            ;; mg_event_handler_t
            (fn (* (function void (* (struct connection)) int (* t))))
            (fn-data (* t))
            ;; mg_event_handler_t
            (pn (* (function void (* (struct connection)) int (* t))))
            (pfn-data (* t))
            (data (array char 32))
            (tls (* t))
            (is-listening (unsigned 1))
            (is-client (unsigned 1))
            (is-accepted (unsigned 1))
            (is-resolving (unsigned 1))
            (is-arplooking (unsigned 1))
            (is-connecting (unsigned 1))
            (is-tls (unsigned 1))
            (is-tls-hs (unsigned 1))
            (is-udp (unsigned 1))
            (is-websocket (unsigned 1))
            (is-mqtt5 (unsigned 1))
            (is-hexdumping (unsigned 1))
            (is-draining (unsigned 1))
            (is-closing (unsigned 1))
            (is-full (unsigned 1))
            (is-tls-throttled (unsigned 1))
            (is-resp (unsigned 1))
            (is-readable (unsigned 1))
            (is-writable (unsigned 1))))

;; --- HTTP --- ;;

(define-alien-type http-header
    (struct http-header
            (name str)
            (value str)))

(define-alien-type http-message
    (struct http-message
            (method str)
            (uri str)
            (query str)
            (proto str)
            ;; MG_MAX_HTTP_HEADERS = 30
            (headers (array http-header 30))
            (body str)
            (head str)
            (message str)))

(define-alien-type http-serve-opts
    (struct http-serve-opts
            ;; NOTE: 2025-12-31 Avoid double dots within paths.
            ;; This is a warning from the Mongoose docs.
            (root-dir c-string)
            (ssi-pattern c-string)
            (extra-headers c-string)
            (mime-types c-string)
            (page404 c-string)
            ;; mg_fs
            ;;
            ;; Filesystem implementation - NULL for POSIX.
            (fs (* t))))

#+nil
(let ((opts (make-alien http-serve-opts)))
  (setf (slot opts 'root-dir) "/home/colin/code/common-lisp/mongoose/")
  (describe (slot opts 'root-dir))
  (free-alien opts))

(define-alien-routine ("mg_http_serve_dir" http-serve-dir) void
  (c    (* connection))
  (hm   (* http-message))
  (opts (* http-serve-opts)))

(define-alien-routine ("mg_http_listen" http-listen) (* connection)
  "Listen for incoming HTTP requests on the given IP/PORT."
  (mgr (* mgr))
  (url c-string)
  (fn (* (function void (* connection) int (* t)))) ;; mg_event_handler_t
  (fn-data (* t)))

(define-alien-routine ("mg_http_reply" http-reply) void
  "Form an HTTP response."
  (c (* connection))
  (status-code int)
  (headers c-string)
  (body c-string))

;; --- LOGGING --- ;;

(define-alien-variable ("mg_log_level" *log-level*) int)

;; --- UTILITIES --- ;;

(declaim (ftype (function ((alien str)) (simple-array character (*))) str->lisp))
(defun str->lisp (s)
  "Convert a Mongoose `mg_str' into a freshly allocated Lisp string."
  (declare (optimize (speed 3)))
  (let* ((len (slot s 'len))
         (buf (slot s 'buf))
         (stream (make-string-output-stream)))
    (loop :for i fixnum :from 0 :below len
          :do (write-char (code-char (deref buf i)) stream))
    (get-output-stream-string stream)))

(declaim (ftype (function (string) hash-table) string->params))
(defun string->params (s)
  "Split a Lisp string into a Hash Table of query params."
  (let* ((ht (make-hash-table :test #'equal))
         (pairs (string-split s :separator #\&)))
    (dolist (pair pairs)
      (let ((split (string-split pair :separator #\=)))
        (when (= 2 (length split))
          (setf (gethash (car split) ht) (cadr split)))))
    ht))

#+nil
(string->params "foo=bar&baz=zoo")

(declaim (ftype (function ((alien (* http-message))) (or null hash-table)) http-message->params))
(defun http-message->params (hm)
  "Extract all the query params as a Hash Table. NIL if none were present."
  (let* ((str (slot hm 'query))
         (len (slot str 'len)))
    (declare (type fixnum len))
    (when (> len 0)
      (string->params (str->lisp str)))))

(declaim (ftype (function ((or string pathname)) (alien (* http-serve-opts))) http-serve-opts))
(defun http-serve-opts (path)
  "Helper: Serve files from the given directory. Freeing the C-memory is on you -
you must call `free-alien' after passing the result of this to `http-serve-dir'."
  (let ((opts (make-alien http-serve-opts))
        (path (if (pathnamep path) (namestring path) path)))
    (setf (slot opts 'root-dir) path)
    ;; NOTE: 2026-01-22 Not setting these all explicitly to NULL can cause
    ;; segfaults.
    (setf (slot opts 'fs) nil)
    (setf (slot opts 'ssi-pattern) nil)
    (setf (slot opts 'extra-headers) nil)
    (setf (slot opts 'mime-types) nil)
    (setf (slot opts 'page404) nil)
    opts))
