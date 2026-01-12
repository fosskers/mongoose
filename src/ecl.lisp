(in-package :mongoose)

;; NOTE: 2026-01-11 Necessary or else the `def-function' bindings below won't
;; work. Note also that if you want to define a new function binding and quickly
;; test it, just rerun the special `load-system' call found in `repl.lisp'. It
;; seems to do the linking correctly, whereas a normal C-c C-c will not. It will
;; also catch arity errors, etc.; it isn't just a blind "bind and pray" process.
(ffi:clines "#include <mongoose.h>")

;; --- STRINGS --- ;;

(ffi:def-struct str
    (buf (* :char))
  (len :uint64-t)) ;; size_t

;; --- DNS --- ;;

(ffi:def-struct dns
    (url :cstring)
  (connection (* connection)))

;; --- ADDRESSES --- ;;

(ffi:def-union ip
    (ip (:array :uint8-t 16))
  (ip4 :uint32-t)
  (ip6 (:array :uint64-t 2)))

(ffi:def-struct addr
    (ip ip)
  (port :uint16-t)
  (scope-id :uint8-t)
  ;; HACK: 2026-01-11 Bool.
  (is-ip6 :uint8-t))

;; --- EVENT MANAGER --- ;;

(ffi:def-struct mgr
    (conns (* connection))
  (dns4 dns)
  (dns6 dns)
  (dnstimeout :int32-t)
  ;; HACK: 2026-01-11 Bool.
  (use-dns6 :uint8-t)
  (nextid :uint64-t)
  (userdata :pointer-void)
  (tls-ctx :pointer-void)
  (mqtt-id :uint16-t)
  (active-dns-requests :pointer-void)
  (timers :pointer-void)
  (epoll-fd :int32-t)
  (ifp :pointer-void)
  (extraconnsize :uint64-t) ;; size_t
  (pipe :int32-t))

(ffi:def-function ("mg_mgr_init" mgr-init)
    ((mgr (* mgr)))
  :returning :void)

(ffi:def-function ("mg_mgr_free" mgr-free)
    ((mgr (* mgr)))
  :returning :void)

(ffi:def-function ("mg_mgr_poll" mgr-poll)
    ((mgr (* mgr))
     (ms :int32-t))
  :returning :void)

;; --- IO BUFFER --- ;;

(ffi:def-struct iobuf
    (buf (* :unsigned-char))
  (size  :uint64-t)
  (len   :uint64-t)
  (align :uint64-t))

;; --- CONNECTIONS --- ;;

(ffi:def-struct connection
    (next :pointer-self)
  (mgr (* mgr))
  (loc addr)
  (rem addr)
  (fd :pointer-void)
  (id :uint64-t)
  (recv iobuf)
  (send iobuf)
  (prof iobuf)
  (rtls iobuf)
  ;; HACK: It's actually a function pointer.
  (fn :pointer-void)
  (fn-data :pointer-void)
  ;; HACK: It's actually a function pointer.
  (pn :pointer-void)
  (pfn-data :pointer-void)
  (data (:array :char 32))
  (tls :pointer-void)
  ;; HACK: 2026-01-11 Since I can't seem to find examples of bit field syntax.
  (flags :uint32-t))

;; --- HTTP --- ;;

(ffi:def-struct http-header
    (name str)
  (value str))

(ffi:def-struct http-message
    (method str)
  (uri str)
  (query str)
  (proto str)
  (headers (:array http-header 30))
  (body str)
  (head str)
  (message str))

(ffi:def-struct http-serve-opts
    ;; NOTE: 2025-12-31 Avoid double dots within paths.
    ;; This is a warning from the Mongoose docs.
    (root-dir :cstring)
  (ssi-pattern :cstring)
  (extra-headers :cstring)
  (mime-types :cstring)
  (page404 :cstring)
  ;; mg_fs
  ;;
  ;; Filesystem implementation - NULL for POSIX.
  (fs :pointer-void))

#+nil
(ffi:with-foreign-object (opts 'http-serve-opts)
  (format t "~a~%" (ffi:get-slot-value opts 'http-serve-opts 'root-dir))
  (setf (ffi:get-slot-value opts 'http-serve-opts 'root-dir) "/home/colin/code/common-lisp/mongoose/")
  (format t "~a~%" (ffi:get-slot-value opts 'http-serve-opts 'root-dir)))

(ffi:def-function ("mg_http_serve_dir" http-serve-dir)
    ((c    (* connection))
     (hm   (* http-message))
     (opts (* http-serve-opts)))
  :returning :void)

(ffi:def-function ("mg_http_listen" http-listen)
    ((mgr (* mgr))
     (url :cstring)
     ;; HACK: It's actually a function pointer.
     (fn :pointer-void)
     (fn-data :pointer-void)))

(ffi:def-function ("mg_http_reply" http-reply)
    ((c (* connection))
     (status-code :int32-t)
     (headers :cstring)
     (body :cstring))
  :returning :void)
