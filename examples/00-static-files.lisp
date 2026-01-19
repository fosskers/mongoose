;;; Serve static files from the filesystem.
;;;
;;; Simple enough: tell it what directory to serve, and call `http-serve-dir'.

(in-package :mongoose)

(define-alien-callable ev-handler void ((c (* connection)) (ev int) (ev-data (* t)))
  "Handle HTTP events."
  (when (= ev +ev-http-msg+)
    ;; NOTE: The `ev-data' comes in "naked", so we must do a pointer cast (this
    ;; is free) to the real type we require.
    (let ((hm   (cast ev-data (* http-message)))
          (opts (make-alien http-serve-opts)))
      (setf (slot opts 'root-dir) ".")
      ;; NOTE: Notice the need to explicitly set this to NULL. This is a
      ;; Mongoose setting that ensures Posix will be used as the filesystem.
      (setf (slot opts 'fs) nil)
      (http-serve-dir c hm opts)
      (free-alien opts))))

#+nil
(let ((mgr (make-alien mgr)))
  (mgr-init mgr)
  (let ((handler (alien-sap (alien-callable-function 'ev-handler))))
    (http-listen mgr "http://localhost:8000" handler nil))
  (format t "Waiting for requests...~%")
  (loop (mgr-poll mgr 1000))
  (mgr-free mgr)
  (free-alien mgr))
