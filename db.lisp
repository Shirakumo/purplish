#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(define-trigger db:connected ()
  (when (db:create 'boards '((name (:varchar 32))
                             (description :text)
                             (visible (:integer 1)))
                   :indices '(name))
    (create-board "Default Board" "This is a default board set up for new Purplish installations."))

  (db:create 'posts '((board :ID)
                      (parent :ID)
                      (revision :integer)
                      (author (:varchar 32))
                      (registered (:integer 1))
                      (deleted (:integer 1))
                      (title (:varchar 64))
                      (time (:integer 5))
                      (updated (:integer 5))
                      (text :text))
             :indices '((_id board) (_id revision)))

  (db:create 'files '((board :ID)
                      (parent :ID)
                      (type (:varchar 16))
                      (filename (:varchar 128)))
             :indices '(parent)))

(define-trigger radiance:startup ()
  (flet ((copy-dir-contents (from to)
           (dolist (file (uiop:directory-files from))
             (uiop:copy-file file (make-pathname :name (pathname-name file)
                                                 :type (pathname-type file)
                                                 :defaults to)))))
    (when (null (uiop:directory-files *themes*))
      (copy-dir-contents (@static "theme/") *themes*))
    (when (null (uiop:directory-files *headers*))
      (copy-dir-contents (@static "headers/") *headers*))))
