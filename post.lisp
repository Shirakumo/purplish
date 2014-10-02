#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun recache-post (post)
  )

(defun recache-thread (thread &key cascade)
  )

(defun recache-board (board &key cascade)
  )

(defun create-post (board parent title text files &optional (author (auth:current)))
  )

(defun delete-post (post &key recache purge)
  )

(defun edit-post (post title text)
  )
