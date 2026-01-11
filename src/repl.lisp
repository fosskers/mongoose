(in-package :mongoose)

(defparameter +content-type-html+ (format nil "Content-Type: text/html; charset=utf-8~c~%" #\return))
(defparameter *requests* 0)

;; NOTE: 2026-01-06 `define-alien-callable' is very powerful! Even when its
;; associated `alien-callable-function' is actually being used as a callback in
;; a live loop (see below), I can still edit this Lisp, recompile it, and the
;; changes will be reflected immediately in subsequent requests.
#+sbcl
(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (declare (optimize (speed 3)))
  (when (= ev +ev-http-msg+)
    (let* ((hm  (cast ev-data (* http-message)))
           (uri (str->lisp (slot hm 'uri))))
      ;; (format t "-------------------------------------------~%")
      ;; (format t "METHOD: '~a'~%" (str->lisp (slot hm 'method)))
      ;; (format t "URI: '~a'~%" uri)
      ;; (format t "QUERY: '~a'~%" (str->lisp (slot hm 'query)))
      (incf *requests*)
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

#+sbcl
(defun normal ()
  (with-alien ((mgr (struct mgr)))
    (mgr-init (addr mgr))
    (format t "Establishing handler...~%")
    (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
      (http-listen (addr mgr) "http://localhost:8000" handler nil))
    (format t "Waiting for requests...~%")
    (loop (mgr-poll (addr mgr) 1000))
    (format t "Exiting~%")
    (mgr-free (addr mgr))))

#+nil
(normal)

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

#+sbcl
(defun counted ()
  (with-alien ((mgr (struct mgr)))
    (setf *requests* 0)
    (mgr-init (addr mgr))
    (format t "Establishing handler...~%")
    (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
      (http-listen (addr mgr) "http://localhost:8000" handler nil))
    (format t "Waiting for requests...~%")
    (loop :while (< *requests* 80000)
          :do (mgr-poll (addr mgr) 1000))
    (format t "Exiting~%")
    (mgr-free (addr mgr))))

#+nil
(sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.0001 :report :graph)
  (counted))

#+nil
(sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.001 :report :graph)
  (with-alien ((mgr (struct mgr)))
    (setf *requests* 0)
    (mgr-init (addr mgr))
    (format t "Establishing handler...~%")
    (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
      (http-listen (addr mgr) "http://localhost:8000" handler nil))
    (format t "Waiting for requests...~%")
    (loop :while (< *requests* 100000)
          :do (mgr-poll (addr mgr) 1000))
    (format t "Exiting~%")
    (mgr-free (addr mgr))))

#+nil
(html:html (:raw "<!DOCTYPE html>")
           (:html
            (:head (:meta :charset "utf-8"))
            (:body (:div :id "foo"
                         :class "bar"
                         (:span "hello")))))

#+nil
(require :sb-sprof)

;; --- ECL --- ;;

(progn
  (setf c:*user-linker-libs*  "-lmongoose")
  (asdf:load-system :mongoose :force t))

#+nil
(ffi:with-foreign-object (mgr 'mgr)
  (mgr-init mgr)
  (describe mgr)
  (format t "~a~%" (ffi:get-slot-value mgr 'mgr 'dnstimeout))
  (mgr-free mgr))

