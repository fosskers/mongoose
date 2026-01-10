(in-package :mongoose)

(defparameter +content-type-html+ (format nil "Content-Type: text/html; charset=utf-8~c~%" #\return))

;; NOTE: 2026-01-06 `define-alien-callable' is very powerful! Even when its
;; associated `alien-callable-function' is actually being used as a callback in
;; a live loop (see below), I can still edit this Lisp, recompile it, and the
;; changes will be reflected immediately in subsequent requests.
(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (when (= ev +ev-http-msg+)
    (let* ((hm  (cast ev-data (* http-message)))
           (uri (str->lisp (slot hm 'uri))))
      ;; (format t "-------------------------------------------~%")
      ;; (format t "METHOD: '~a'~%" (str->lisp (slot hm 'method)))
      ;; (format t "URI: '~a'~%" uri)
      ;; (format t "QUERY: '~a'~%" (str->lisp (slot hm 'query)))
      (cond
        #+nil
        ((string= "/foo" uri)
         (format t "Route matching!~%")
         (http-reply c 200
                     (format nil "Content-Type: text/plain~c~%" #\return)
                     "Hello, Jack!"))
        ((string= "/html" uri)
         ;; (format t "Serving HTML.~%")
         (http-reply c 200
                     +content-type-html+
                     (html:html (:raw "<!DOCTYPE html>")
                                (:html
                                 (:head (:meta :charset "utf-8"))
                                 (:body (:div :id "foo"
                                              :class "bar"
                                              (:span "hello")))))))
        (t (format t "Serve the filesystem~%")
           (with-alien ((opts (struct http-serve-opts)))
             (setf (slot opts 'root-dir) "/home/colin/code/common-lisp/mongoose/")
             (http-serve-dir c hm (addr opts))))))))

#+nil
(with-alien ((mgr (struct mgr)))
  (mgr-init (addr mgr))
  (format t "Establishing handler...~%")
  (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
    (http-listen (addr mgr) "http://localhost:8000" handler nil))
  (format t "Waiting for requests...~%")
  (loop (mgr-poll (addr mgr) 1000))
  (format t "Exiting~%")
  (mgr-free (addr mgr)))

#+nil
(html:html (:raw "<!DOCTYPE html>")
           (:html
            (:head (:meta :charset "utf-8"))
            (:body (:div :id "foo"
                         :class "bar"
                         (:span "hello")))))
