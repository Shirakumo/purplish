#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun ensure-post (post)
  (etypecase post
    (dm:data-model post)
    (fixnum (dm:get-one 'purplish-posts (db:query (:= '_id post))))
    (string (dm:get-one 'purplish-posts (db:query (:= '_id (parse-integer post)))))))

(defun ensure-board (board)
  (etypecase board
    (dm:data-model board)
    (fixnum (dm:get-one 'purplish-boards (db:query (:= '_id board))))
    (string (dm:get-one 'purplish-boards (let ((id (parse-integer board :junk-allowed T)))
                                           (if id
                                               (db:query (:= '_id id))
                                               (db:query (:= 'name board))))))))

(defun last-revision (post)
  (let ((id (etypecase post
              (dm:data-model (dm:field post "_id"))
              (fixnum post)
              (string (parse-integer post)))))
    (dm:get-one 'purplish-posts (db:query (:= 'parent id))
                :sort '((revision :DESC)))))