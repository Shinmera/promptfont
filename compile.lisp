#|
exec sbcl --noinform --disable-debugger --load "$0" --eval '(fixup)' --eval '(generate)' --quit
|#

(ql:quickload '(clip shasht) :silent T)

(defvar *here* #.(or *compile-file-pathname*
                     *load-pathname*
                     (error "LOAD this file.")))

(defun file (name type)
  (make-pathname :name name :type type :defaults *here*))

(defun parse-glyphs (&optional (file (file "glyphs" "json")))
  (let ((sections (make-hash-table :test 'equal)))
    (loop for glyph across (with-open-file (stream file)
                             (shasht:read-json stream))
          do (push (list :code (gethash "code" glyph)
                         :codepoint (gethash "codepoint" glyph)
                         :name (gethash "name" glyph))
                   (gethash (gethash "category" glyph) sections)))
    (loop for name being the hash-keys of sections
          for glyphs being the hash-values of sections
          collect (list :name name
                        :glyphs (sort glyphs #'< :key (lambda (a) (getf a :codepoint)))))))

(defun generate (&key (input (file "index" "ctml")) (output (file "index" "html")))
  (let ((sections (parse-glyphs)))
    (with-open-file (stream output :direction :output :if-exists :supersede)
      (plump:serialize (clip:process input :sections sections) stream))))

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
