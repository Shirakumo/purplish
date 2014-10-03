#|
 This file is a part of Purplish
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:purplish
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "0.1.0"
  :description "Imageboard CMS"
  :components ((:file "module")
               (:file "db")
               (:file "toolkit")
               (:file "parse")
               (:file "file")
               (:file "cache")
               (:file "post")
               (:file "api")
               (:file "front"))
  :depends-on ((:interface :database)
               (:interface :data-model)
               (:interface :auth)
               (:interface :cache)
               (:interface :profile)
               :i-json
               :trivial-mimes
               :local-time
               :clip
               :3bmd
               :3bmd-ext-code-blocks))
