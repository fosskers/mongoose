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
