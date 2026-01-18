;;; Match on a particular route.
;;;
;;; Note the usage of `str->lisp' to extract a Lisp string out of the underlying
;;; Mongoose `mg_str' struct underneath.

(in-package :mongoose)

;; NOTE Defining our header externally like this has two effects:
;;
;; 1. We only need to call `format' once, instead of per request.
;; 2. With SBCL this produces a `simple-base-string', which the FFI can send to
;;    C without any memory copying.
(defparameter *content-type-text* (format nil "Content-Type: text/plain~c~c" #\return #\linefeed))

(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (when (= ev +ev-http-msg+)
    (let* ((hm  (cast ev-data (* http-message)))
           ;; NOTE Notice that for now, we need to lift the C-string out of the
           ;; `http-message' in order to compare it with a Lisp one.
           (uri (str->lisp (slot hm 'uri))))
      (cond
        ((string= "/foo" uri)
         (http-reply c 200 *content-type-text* "Hello!"))
        (t (http-reply c 404 nil ""))))))

#+nil
(let ((mgr (make-alien mgr)))
  (mgr-init mgr)
  (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
    (http-listen mgr "http://localhost:8000" handler nil))
  (format t "Waiting for requests...~%")
  (loop (mgr-poll mgr 1000))
  (mgr-free mgr)
  (free-alien mgr))

