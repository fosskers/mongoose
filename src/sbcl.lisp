(in-package :mongoose)

;; --- STRINGS --- ;;

(define-alien-type str
    (struct str
            (buf (* char))
            (len size-t)))

;; (define-alien-routine ("mg_str" str) str
;;   (s c-string))

(defun str->lisp (s)
  "Convert a Mongoose `mg_str' into a freshly allocated Lisp string."
  (let* ((len (slot s 'len))
         (buf (slot s 'buf))
         (stream (make-string-output-stream)))
    (loop :for i fixnum :from 0 :below len
          :do (write-char (code-char (deref buf i)) stream))
    (get-output-stream-string stream)))

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
