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
