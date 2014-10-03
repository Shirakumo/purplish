#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(define-trigger db:connected ()
  (db:create 'purplish-boards '((name (:varchar 32))
                                (description :text)
                                (visible (:integer 1)))
             :indices '(name))

  (db:create 'purplish-posts '((board :ID)
                               (parent :ID)
                               (revision :integer)
                               (author (:varchar 32))
                               (registered (:integer 1))
                               (title (:varchar 32))
                               (time (:integer 5))
                               (text :text))
             :indices '((_id board) (_id revision)))

  (db:create 'purplish-files '((board :ID)
                               (parent :ID)
                               (type (:varchar 16))
                               (filename (:varchar 128)))
             :indices '(post)))

