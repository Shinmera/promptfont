#|
sbcl --noinform --load "$0" --eval '(fixup)' --quit; exit
|#

(ql:quickload :shasht)
(defvar *here* #. (make-pathname :name NIL :type NIL :defaults *load-pathname*))

(defun fixup (&optional (file (merge-pathnames "glyphs.json" *here*)))
  (let ((data (with-open-file (stream file)
                (shasht:read-json stream))))
    (loop for entry across data
          for cp = (or (gethash "codepoint" entry)
                       (parse-integer (gethash "code" entry) :start 2 :radix 16))
          do (setf (gethash "character" entry) (string (code-char cp)))
             (setf (gethash "codepoint" entry) cp)
             (setf (gethash "code" entry) (format NIL "U+~4,'0x" cp)))
    (sort data #'< :key (lambda (entry) (gethash "codepoint" entry)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (shasht:write-json data stream))))
