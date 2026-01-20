;;; Error handling.
;;;
;;; This example showcases:
;;;
;;; (1) How errors can be detected.
;;; (2) How the event handling loop can be entirely exited.

(in-package :mongoose)

(defparameter *headers* (headers '("Content-Type: text/plain")))
(defparameter *done?* nil)

(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (cond ((= ev +ev-http-msg+)
         (let* ((hm  (cast ev-data (* http-message)))
                (uri (str->lisp (slot hm 'uri))))
           (when (string= "/exit" uri)
             (setf *done?* t))
           (http-reply c 200 *headers* "Hello!")))
        ;; In the case of an error, we cast the `ev-data' to a string instead of
        ;; a `http-message' like usual.
        ((= ev +ev-error+)
         (let ((err (cast ev-data c-string)))
           (format t "Calamity: ~a~%" err)))))

#+nil
(let ((mgr (make-alien mgr)))
  (mgr-init mgr)
  (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
    (http-listen mgr "http://localhost:8000" handler nil))
  (format t "Waiting for requests...~%")
  ;; Loop on some condition to determine exit timing.
  (loop :until *done?*
        :do (mgr-poll mgr 1000))
  (format t "Exiting!~%")
  (mgr-free mgr)
  (free-alien mgr))

