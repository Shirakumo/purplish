#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun escape-regex (string)
  (cl-ppcre:regex-replace-all "([\\.\\$\\^\\[\\]\\(\\)\\{\\}\\|\\!\\?\\*\\+\\\\])" string "\\\\\\1"))

(defun parse-search-query (string)
  (let ((string (escape-regex string)))
    (with-output-to-string (stream)
      (write-string "(?i).*" stream)
      (loop with in-quot = NIL
            for prev = #\Space then char
            for char across string
            do (if (and (not in-quot) (char= char #\Space))
                   (write-string ".*" stream)
                   (write-char char stream))
               (when (and (not (char= prev #\\)) (char= char #\"))
                 (setf in-quot (not in-quot))))
      (write-string ".*" stream))))

(defun search-posts (query &key board thread (amount 20) (func 'dm:get))
  (let* ((board (when board (dm:id (ensure-board board))))
         (thread (when thread (dm:id (ensure-post thread))))
         (query (parse-search-query query))
         (query (cond
                  (thread (db:query (:and (:= 'parent thread)
                                          (:or (:matches 'title query)
                                               (:matches 'author query)
                                               (:matches 'text query)))))
                  (board (db:query  (:and (:= 'board board)
                                          (:or (:matches 'title query)
                                               (:matches 'author query)
                                               (:matches 'text query)))))
                  (T (db:query (:or (:matches 'title query)
                                    (:matches 'author query)
                                    (:matches 'text query)))))))
    (funcall func 'purplish-posts query :amount amount :sort '((time :DESC)))))
