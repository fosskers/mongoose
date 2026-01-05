(in-package :mongoose)

(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (when (= ev +ev-http-msg+)
    (with-alien ((opts (struct http-serve-opts)))
      (setf (slot opts 'root-dir) "/home/colin/code/common-lisp/mongoose/")
      (http-serve-dir c (cast ev-data (* http-message)) (addr opts)))))

#+nil
(with-alien ((mgr (struct mgr)))
  (mgr-init (addr mgr))
  (format t "Establishing handler...~%")
  (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
    (http-listen (addr mgr) "http://localhost:8000" handler nil))
  (format t "Waiting for requests...~%")
  (loop (mgr-poll (addr mgr) 1000))
  (mgr-free (addr mgr)))

