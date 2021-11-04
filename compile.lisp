#|
sbcl --noinform --load "$0" --eval '(generate)' --quit; exit
|#

(ql:quickload '(clip yason) :silent T)

(defvar *here* #.(or *compile-file-pathname*
                     *load-pathname*
                     (error "LOAD this file.")))

(defun file (name type)
  (make-pathname :name name :type type :defaults *here*))

(defun parse-glyphs (&optional (file (file "glyphs" "json")))
  (let ((sections (make-hash-table :test 'equal)))
    (dolist (glyph (yason:parse file))
      (push (list :code (gethash "code" glyph)
                  :codepoint (gethash "codepoint" glyph)
                  :name (gethash "name" glyph))
            (gethash (gethash "category" glyph) sections)))
    (loop for name being the hash-keys of sections
          for glyphs being the hash-values of sections
          collect (list :name name
                        :glyphs glyphs))))

(defun generate (&key (input (file "index" "ctml")) (output (file "index" "html")))
  (let ((sections (parse-glyphs)))
    (with-open-file (stream output :direction :output :if-exists :supersede)
      (plump:serialize (clip:process input :sections sections) stream))))
