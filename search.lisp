#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
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
  (let* ((board (when board (ensure-board board)))
         (thread (when thread (ensure-post thread)))
         (board (when board (dm:id board)))
         (thread (when thread (dm:id thread)))
         (query (parse-search-query query))
         (query (cond
                  (thread (db:query (:and (:= 'parent thread)
                                          (:= 'revision 0)
                                          (:or (:matches 'title query)
                                               (:matches 'author query)
                                               (:matches 'text query)))))
                  (board (db:query  (:and (:= 'board board)
                                          (:= 'revision 0)
                                          (:or (:matches 'title query)
                                               (:matches 'author query)
                                               (:matches 'text query)))))
                  (T (db:query (:and (:= 'revision 0)
                                     (:or (:matches 'title query)
                                          (:matches 'author query)
                                          (:matches 'text query))))))))
    (funcall func 'posts query :amount amount :sort '((time :DESC)))))
