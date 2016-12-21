#|
 This file is a part of Purplish
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:purplish
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
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
               (:file "front")
               (:file "api")
               (:file "atom")
               (:file "admin"))
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
