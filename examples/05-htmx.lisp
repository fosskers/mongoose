(in-package :mongoose)

(defparameter *headers* (headers '("Content-Type: text/html")))

(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (when (= ev +ev-http-msg+)
    (let* ((hm  (cast ev-data (* http-message)))
           (uri (str->lisp (slot hm 'uri))))
      (cond
        ((string= "/" uri)
         (let ((html (html:html (:raw "<!DOCTYPE html>")
                                (:html
                                 (:head (:meta :charset "utf-8")
                                        (:title "HTMX Example")
                                        (:script :src "/examples/htmx.min.js"))
                                 (:body (:div :id "replace-me"
                                              (:button :hx-get "/load"
                                                       :hx-target "#replace-me"
                                                       :hx-swap "outerHTML"
                                                       "Replace Me!")))))))
           (http-reply c 200 *headers* html)))
        ((string= "/load" uri)
         (let ((html (html:html (:div "You did it!"))))
           (http-reply c 200 *headers* html)))
        (t (let ((opts (http-serve-opts ".")))
             (http-serve-dir c hm opts)
             (free-alien opts)))))))

#+nil
(let ((mgr (make-alien mgr)))
  (mgr-init mgr)
  (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
    (http-listen mgr "http://localhost:8000" handler nil))
  (format t "Waiting for requests...~%")
  (loop (mgr-poll mgr 1000))
  (mgr-free mgr)
  (free-alien mgr))

