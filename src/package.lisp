(defpackage mongoose
  (:use :cl #+sbcl :sb-alien)
  (:documentation "Bindings to the Mongoose webserver."))

(in-package :mongoose)

#+sbcl
(defun load-shared-objects ()
  "Dynamically load the necessary `.so' file."
  (load-shared-object #p"/usr/lib/libmongoose.so" :dont-save t))

#+sbcl
(load-shared-objects)

;; --- EVENTS --- ;;

(defconstant +ev-error+ 0)
(defconstant +ev-open+ 1)
(defconstant +ev-poll+ 2)
(defconstant +ev-resolve+ 3)
(defconstant +ev-connect+ 4)
(defconstant +ev-accept+ 5)
(defconstant +ev-tls-hs+ 6)
(defconstant +ev-read+ 7)
(defconstant +ev-write+ 8)
(defconstant +ev-close+ 9)
(defconstant +ev-http-hdrs+ 10)
(defconstant +ev-http-msg+ 11)
(defconstant +ev-wf-msg+ 12)
(defconstant +ev-wf-ctl+ 13)
(defconstant +ev-mqtt-cmd+ 14)
(defconstant +ev-mqtt-msg+ 15)
(defconstant +ev-mqtt-open+ 16)
(defconstant +ev-mqtt-time+ 17)
(defconstant +ev-wakeup+ 18)
(defconstant +ev-user+ 19)

;; --- LOGGING --- ;;

;; enum { MG_LL_NONE, MG_LL_ERROR, MG_LL_INFO, MG_LL_DEBUG, MG_LL_VERBOSE };
(defconstant +ll-none+ 0)
(defconstant +ll-error+ 1)
(defconstant +ll-info+ 2)
(defconstant +ll-debug+ 3)
(defconstant +ll-verbose+ 4)

;; --- UTILITIES --- ;;

;; NOTE: 2026-01-19 Faster than `uiop:split-string'.
(declaim (ftype (function (string &key (:separator character)) list) string-split))
(defun string-split (string &key (separator #\space))
  "Split a string into a list of substrings according to some configurable
separator character."
  (labels ((recurse (acc start end)
             (declare (type fixnum start end))
             (cond ((and (< start 0) (< end 0)) acc)
                   ;; The separator was found at the very start of the string.
                   ((and (zerop start) (char= separator (char string start)))
                    (cons "" (cons (subseq string (1+ start) (1+ end)) acc)))
                   ;; We got to the beginning without seeing another separator.
                   ((zerop start) (cons (subseq string start (1+ end)) acc))
                   ;; Normal separator detection: collect the piece we've built.
                   ((char= separator (char string start))
                    (recurse (cons (subseq string (1+ start) (1+ end)) acc)
                             (1- start)
                             (1- start)))
                   ;; Base case: just keep moving.
                   (t (recurse acc (1- start) end)))))
    ;; We start from the end of the string and go backwards, in order to neatly
    ;; build up the final list without needing to `reverse'.
    (let ((end (1- (length string))))
      (recurse '() end end))))

#+nil
(string-split "bar=baz&foo=aaa&shrek=fiona" :separator #\&)
