#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(define-version-migration purplish (NIL 1.0.0)
  (flet ((migrate-directory (from to)
           (when (uiop:directory-exists-p from)
             (uiop:delete-directory-tree to :validate (constantly T))
             (rename-file from to))))
    (migrate-directory (conf-dir "cache/") *cache*)
    (migrate-directory (conf-dir "theme/") *themes*)
    (migrate-directory (conf-dir "headers/") *headers*)
    (migrate-directory (conf-dir "file/") *files*)))
