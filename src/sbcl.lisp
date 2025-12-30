(in-package :mongoose)

(define-alien-type dns
    (struct dns
            (url c-string)
            (connection (* (struct connection)))))

(define-alien-type addr
    (struct addr
            (ip (union nil
                       (ip  (array (unsigned 8) 16))
                       (ip4 (unsigned 32))
                       (ip6 (array (unsigned 64) 2))))
            (port (unsigned 16))
            (scope-id (unsigned 8))
            (is-ip6 (boolean 8))))

;; (define-alien-type timer
;;     (struct timer
;;             (period-ms (unsigned 64))
;;             (expire (unsigned 64))
;;             (flags (unsigned 32))
;;             ;; A function pointer.
;;             (fn (* t))
;;             (arg (* t))
;;             (next (* (struct timer)))))

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

(define-alien-type iobuf
    (struct iobuf
            (buf (* unsigned-char))
            (size size-t)
            (len size-t)
            (align size-t)))

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
            ;; Function pointer.
            (fn (* t))
            (fn-data (* t))
            ;; Function pointer.
            (pfn (* t))
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
