;;; Accessing query parameters.
;;;
;;; `http-message->params' normally yields a Hash Table, but will return NIL
;;; instead when no params were given. This is more memory efficient than
;;; returning an empty Hash Table, especially during heavy traffic.
;;;
;;; We can see the `html:expand' (instead of the static `html:html') function
;;; being used here to dynamically inject content into the quoted HTML form. Yes
;;; this could be refactored to be prettier, but this demonstrates how quoted
;;; sections and expanded sections can be nested within one another.
;;;
;;; Usage of `cond' shows that we can have conditional logic directly within our
;;; HTML "template".

(in-package :mongoose)

(defparameter *content-type-html* (format nil "Content-Type: text/html~c~c" #\return #\linefeed))

(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (when (= ev +ev-http-msg+)
    (let* ((hm  (cast ev-data (* http-message)))
           (uri (str->lisp (slot hm 'uri))))
      (cond
        ((string= "/foo" uri)
         (let* ((params (http-message->params hm))
                (html (with-output-to-string (stream)
                        (html:expand stream `(:raw "<!DOCTYPE html>"))
                        (html:expand stream
                                     `(:html
                                       (:head (:meta :charset "utf-8"))
                                       (:body ,(cond ((null params) `(:span "No params given!"))
                                                     (t `(:div
                                                          (:span "Params given:")
                                                          (:ul ,@(loop :for k :being :the :hash-key
                                                                         :using (hash-value v) :of params
                                                                       :collect `(:li ,(format nil "~a: ~a" k v)))))))))))))
           (http-reply c 200 *content-type-html* html)))
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

