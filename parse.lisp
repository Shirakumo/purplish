#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun date-machine (stamp)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '((:year 4) "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2)))))

(defun date-human (stamp)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '((:year 4) "." (:month 2) "." (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))))

(defun date-fancy (stamp)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '(:long-weekday ", " :ordinal-day " of " :long-month " " :year ", " :hour ":" :min ":" :sec))))

(lquery:define-lquery-function purplish-time (node time)
  (let ((stamp (local-time:universal-to-timestamp time)))
    (setf (plump:attribute node "datetime")
          (date-machine stamp))
    (setf (plump:attribute node "title")
          (date-fancy stamp))
    (setf (plump:children node) (plump:make-child-array))
    (plump:make-text-node node (date-human stamp))))

(lquery:define-lquery-function purplish-cache (node type object)
  ;; We don't want to parse the cached file again just to serialise it anew
  ;; so we cheat by changing this to a fulltext element and abusing its direct
  ;; serialisation.
  (change-class node 'plump:fulltext-element
                :children (plump:make-child-array))
  (plump:make-text-node
   node (with-open-file (stream (merge-pathnames
                                 (ecase type
                                   (:board (board-cache object))
                                   (:thread (thread-cache object))
                                   (:thread-min (thread-min-cache object))
                                   (:post (post-cache object)))
                                 *cache*))
          (plump::slurp-stream stream))))

(lquery:define-lquery-function purplish-template (node object)
  (setf (plump:children node) (plump:make-child-array))
  (plump:parse (template (format NIL "~(~a~).ctml" object)) :root node))

(defun parse (text)
  (let ((3bmd:*smart-quotes* T)
        (3bmd-code-blocks:*code-blocks* T))
    (with-output-to-string (stream)
      (3bmd:parse-string-and-print-to-stream text stream))))
