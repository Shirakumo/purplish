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
  :license "Artistic"
  :version "0.9.0"
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
               (:file "api")
               (:file "front"))
  :depends-on ((:interface :database)
               (:interface :data-model)
               (:interface :auth)
               (:interface :cache)
               (:interface :profile)
               (:interface :rate)
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
