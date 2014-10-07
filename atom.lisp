#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun atom-post-link (post)
  (format NIL "http://chan.~a/thread/~a#post-~:*~a"
          (when (boundp '*request*) (domain *request*)) (dm:id post)))

(defun atom-cache (&optional board)
  (merge-pathnames (format NIL "atom/~:[general.xml~;~:*~a.xml~]" board) *cache*))

(defun recache-atom (&key post board)
  (v:debug :purplish-cache "Recaching Atom~@[ ~a~]" (or post board))
  (flet ((execute (posts title description file)
           (with-cache-file (stream path file)
             (plump:serialize
              (clip:process
               (plump:parse (template "atom.ctml"))
               :title title
               :description description
               :updated (when posts (dm:field (car (last posts)) "time"))
               :posts posts)
              stream))))
    (when (or post board)
      (let ((board (ensure-board (if board
                                     board
                                     (dm:field (ensure-post post) "board")))))
        (execute (dm:get 'purplish-posts (db:query (:and (:= 'board (dm:id board))
                                                         (:= 'parent -1)))
                         :amount 20 :sort '((updated :DESC)))
                 (dm:field board "name")
                 (dm:field board "description")
                 (atom-cache (dm:id board)))))
    
    (execute (dm:get 'purplish-posts (db:query (:= 'revision 0))
                     :amount 20 :sort '((time :DESC)))
             (config-tree :purplish :title)
             (config-tree :purplish :description)
             (atom-cache))))

(define-trigger purplish:board-created (id)
  (recache-atom :board id))

(define-trigger purplish:board-deleted (id)
  (delete-file (atom-cache id))
  (recache-atom))

(define-trigger purplish:post-created (id)
  (recache-atom :post id))

(define-trigger purplish:post-edited (id edit-id)
  (recache-atom :post id))

(define-trigger purplish:post-purged (id)
  (recache-atom :post id))

(define-api purplish/atom (&optional board) ()
  (serve-or-err (atom-cache board) "Feed not found." "application/atom+xml"))
