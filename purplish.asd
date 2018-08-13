#|
 This file is a part of Purplish
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:purplish
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :version "1.0.0"
  :description "Imageboard CMS for Radiance"
  :homepage "https://github.com/Shinmera/purplish"
  :components ((:file "module")
               (:file "db")
               (:file "toolkit")
               (:file "parse")
               (:file "file")
               (:file "cache")
               (:file "post")
               (:file "search")
               (:file "front")
               (:file "api")
               (:file "atom")
               (:file "admin")
               (:file "migrate"))
  :depends-on ((:interface :database)
               (:interface :auth)
               (:interface :cache)
               (:interface :profile)
               (:interface :rate)
               :r-data-model
               :cl-ppcre
               :lquery
               :i-json
               :cl-json
               :trivial-mimes
               :local-time
               :clip
               :3bmd
               :3bmd-ext-code-blocks
               :trivial-thumbnail
               :drakma))
