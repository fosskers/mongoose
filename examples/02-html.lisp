;;; Yield HTML.
;;;
;;; See https://github.com/fosskers/html for a zero-dependency HTML expansion
;;; library.

(in-package :mongoose)

(defparameter *headers* (headers '("Content-Type: text/html")))

(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (when (= ev +ev-http-msg+)
    (let* ((hm  (cast ev-data (* http-message)))
           (uri (str->lisp (slot hm 'uri))))
      (cond
        ((string= "/foo" uri)
         (http-reply c 200 *headers*
                     (html:html (:raw "<!DOCTYPE html>")
                                (:html
                                 (:head (:meta :charset "utf-8"))
                                 (:body "hello")))))
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

