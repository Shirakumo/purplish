#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defaulted-config "Purplish" :title)
(defaulted-config "An unconfigured imageboard." :description)
(defaulted-config 128 :thumb :width)
(defaulted-config 128 :thumb :height)
(defaulted-config T :thumb :gif)
(defaulted-config 5000 :file :size-limit)

(defun ensure-file (file)
  (or
   (typecase file
     (dm:data-model file)
     (db:id (dm:get-one 'files (db:query (:= '_id file))))
     (T (ensure-file (db:ensure-id file))))
   (error "No such file found.")))

(defun ensure-post (post)
  (or
   (typecase post
     (dm:data-model post)
     (db:id (dm:get-one 'posts (db:query (:= '_id post))))
     (T (ensure-post (db:ensure-id post))))
   (error "No such post found.")))

(defun ensure-thread (post)
  (let ((thread (ensure-post post)))
    (unless (= (dm:field thread "parent") -1)
      (error "This post is not a thread."))
    thread))

(defun ensure-board (board)
  (or
   (typecase board
     (dm:data-model board)
     (db:id (dm:get-one 'boards (db:query (:= '_id board))))
     (T (dm:get-one 'boards (let ((id (ignore-errors (db:ensure-id board))))
                              (if id
                                  (db:query (:= '_id id))
                                  (db:query (:= 'name board)))))))
   (error "No such board found.")))

(defun last-revision (post)
  (let ((id (etypecase post
              (dm:data-model (dm:id post))
              (db:id post)
              (T (db:ensure-id post)))))
    (dm:get-one 'posts (db:query (:and (:= 'parent id)
                                       (:< 0 'revision)))
                :sort '((revision :DESC)))))

(defun conf-dir (dir)
  (make-pathname :name NIL
                 :type NIL
                 :defaults (merge-pathnames dir (mconfig-pathname #.*package*))))
