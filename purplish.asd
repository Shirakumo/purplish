(asdf:defsystem #:purplish
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :version "1.0.0"
  :description "Imageboard CMS for Radiance"
  :homepage "https://Shinmera.github.io/purplish/"
  :bug-tracker "https://github.com/Shinmera/purplish/issues"
  :source-control (:git "https://github.com/Shinmera/purplish.git")
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
               :cl-markless-plump
               :trivial-thumbnail
               :drakma))
