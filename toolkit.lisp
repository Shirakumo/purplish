#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun ensure-file (file)
  (or
   (etypecase file
     (dm:data-model file)
     (fixnum (dm:get-one 'purplish-files (db:query (:= '_id file))))
     (string (dm:get-one 'purplish-files (db:query (:= '_id (parse-integer file))))))
   (error "No such file found.")))

(defun ensure-post (post)
  (or
   (etypecase post
     (dm:data-model post)
     (fixnum (dm:get-one 'purplish-posts (db:query (:= '_id post))))
     (string (dm:get-one 'purplish-posts (db:query (:= '_id (or (parse-integer post :junk-allowed T)
                                                                (return-from ensure-post)))))))
   (error "No such post found.")))

(defun ensure-board (board)
  (or
   (etypecase board
     (dm:data-model board)
     (fixnum (dm:get-one 'purplish-boards (db:query (:= '_id board))))
     (string (dm:get-one 'purplish-boards (let ((id (parse-integer board :junk-allowed T)))
                                            (if id
                                                (db:query (:= '_id id))
                                                (db:query (:= 'name board)))))))
   (error "No such board found.")))

(defun last-revision (post)
  (let ((id (etypecase post
              (dm:data-model (dm:id post))
              (fixnum post)
              (string (parse-integer post)))))
    (dm:get-one 'purplish-posts (db:query (:and (:= 'parent id)
                                                (:< 0 'revision)))
                :sort '((revision :DESC)))))
