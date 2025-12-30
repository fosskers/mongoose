(defsystem "mongoose"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "GPL-2.0"
  :homepage "https://github.com/fosskers/mongoose"
  :depends-on (:trivial-garbage)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "sbcl" :if-feature :sbcl))))
  :description "Bindings to the Mongoose webserver.")
