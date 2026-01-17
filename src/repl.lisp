(in-package :mongoose)

(defparameter +content-type-html+ (format nil "Content-Type: text/html; charset=utf-8~c~%" #\return))
(defparameter *requests* 0)

;; --- SBCL --- ;;

#+sbcl
(require :sb-sprof)

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
      (cond
        ((string= "/html" uri)
         (http-reply c 200
                     +content-type-html+
                     (html:html (:raw "<!DOCTYPE html>")
                                (:html
                                 (:head (:meta :charset "utf-8"))
                                 (:body "hello")))))
        (t (format t "Serve the filesystem~%")
           (let ((opts (make-alien http-serve-opts)))
             (setf (slot opts 'root-dir) "/home/colin/code/common-lisp/mongoose/")
             (http-serve-dir c hm opts)
             (free-alien opts)))))))

#+nil
(let ((mgr (make-alien mgr)))
  (mgr-init mgr)
  (format t "Establishing handler...~%")
  (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
    (http-listen mgr "http://localhost:8000" handler nil))
  (format t "Waiting for requests...~%")
  (loop (mgr-poll mgr 1000))
  (format t "Exiting~%")
  (mgr-free mgr)
  (free-alien mgr))

#+nil
(html:html (:raw "<!DOCTYPE html>")
           (:html
            (:head (:meta :charset "utf-8"))
            (:body (:div :id "foo"
                         :class "bar"
                         (:span "hello")))))

;; --- ECL --- ;;

;; NOTE: 2026-01-17 Paused as of this date.

#+ecl
(progn
  (setf c:*user-linker-libs* "-lmongoose")
  (asdf:load-system :mongoose :force t))

#+nil
(ffi:with-foreign-object (mgr 'mgr)
  (mgr-init mgr)
  (describe mgr)
  (format t "~a~%" (ffi:get-slot-value mgr 'mgr 'dnstimeout))
  (mgr-free mgr))

#+ecl
(ffi:defcallback ev-handler :void ((c :pointer-void) (ev :int32-t) (ev-data :pointer-void))
  (when (= ev +ev-http-msg+)
    (ffi:with-cast-pointer (hm ev-data '(* http-message))
      ;; (format t "Serve the filesystem!~%")
      (ffi:with-foreign-object (opts 'http-serve-opts)
        (let* ((octets (ext:string-to-octets "/home/colin/code/common-lisp/mongoose/" :null-terminate t))
               (ctext  (si:make-foreign-data-from-array octets)))
          (setf (ffi:get-slot-value opts 'http-serve-opts 'root-dir) ctext))
        (http-serve-dir c hm opts)))))

#+nil
(ffi:with-foreign-object (mgr 'mgr)
  (mgr-init mgr)
  (let ((handler (ffi:callback 'ev-handler)))
    (http-listen mgr "http://localhost:8000" handler (ffi:make-null-pointer :pointer-void)))
  (format t "Waiting for requests...~%")
  (loop (mgr-poll mgr 1000))
  (format t "Exiting~%")
  (mgr-free mgr))

;; TODO: 2026-01-11 If continuing with ECL, start here. It's quite slow, and the
;; wrong path is being set. It's only recognizing the first character, /. Wasn't
;; something weird like this happening with strings and ECL before? Something
;; about it only taking the first char?
;;
;; Okay I pulled some old string conversion code from Raylib, and that gets me
;; further, but now it's segfaulting.
