(defsystem "mongoose"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/mongoose"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "sbcl" :if-feature :sbcl))))
  :description "Bindings to the Mongoose webserver.")
