#|
exec sbcl \
  --noinform \
  --disable-debugger \
  --eval "(ql:quickload '(clip shasht) :silent T)" \
  --load "$0" \
  --eval '(main)' \
  --quit \
  --end-toplevel-options "${@:1}"
|#

(defvar *here* #.(or *compile-file-pathname*
                     *load-pathname*
                     (error "LOAD this file.")))

(defun file (name type)
  (make-pathname :name name :type type :defaults *here*))

(defun parse-glyphs (&optional (file (file "glyphs" "json")))
  (let ((sections (make-hash-table :test 'equal)))
    (loop for glyph across (with-open-file (stream file)
                             (shasht:read-json stream))
          do (push (loop for k being the hash-keys of glyph using (hash-value v)
                         collect (intern (string-upcase k) "KEYWORD")
                         collect v)
                   (gethash (gethash "category" glyph) sections)))
    (loop for name being the hash-keys of sections
          for glyphs being the hash-values of sections
          collect (list :name name
                        :glyphs (sort glyphs #'< :key (lambda (a) (getf a :codepoint)))))))

(defun web (&optional (input (file "index" "ctml")) (output (file "index" "html")))
  (let ((sections (parse-glyphs)))
    (with-open-file (stream output :direction :output :if-exists :supersede)
      (plump:serialize (clip:process input :sections sections) stream))))

(defun txt (&optional (file (file "glyphs" "json")) (output (file "chars" "txt")))
  (with-open-file (stream output :direction :output :if-exists :supersede)
    (loop for glyph across (with-open-file (stream file)
                             (shasht:read-json stream))
          do (write-string (gethash "character" glyph) stream))))

(defun css-safe-name (name)
  (with-output-to-string (out)
    (let ((was-dash T))
      (labels ((process (name)
                 (loop for char across name
                       do (cond ((find char " -_/")
                                 (unless was-dash
                                   (write-char #\- out)
                                   (setf was-dash T)))
                                ((find char "()"))
                                ((alphanumericp char)
                                 (setf was-dash NIL)
                                 (write-char (char-downcase char) out))
                                (T
                                 (process (char-name char)))))))
        (process name)))))

(defun css (&optional (file (file "glyphs" "json")) (output (file "promptfont" "css")))
  (with-open-file (stream output :direction :output :if-exists :supersede)
    (format stream "~&@font-face{font-family:'promptfont'; src:url('promptfont.ttf');}~%")
    (format stream "~&.pf{font-family:promptfont;}~%")
    (loop for entry across (with-open-file (stream file)
                             (shasht:read-json stream))
          unless (string= "alphabet" (gethash "category" entry))
          do (format stream "~&.pf-~a::after{content:'\\u~x';}~%"
                     (css-safe-name (gethash "name" entry))
                     (gethash "codepoint" entry)))))

(defun fixup (&optional (file (file "glyphs" "json")))
  (let ((data (with-open-file (stream file)
                (shasht:read-json stream)))
        (names (make-hash-table :test 'equalp)))
    (loop for entry across data
          for cp = (or (gethash "codepoint" entry)
                       (parse-integer (gethash "code" entry) :start 2 :radix 16))
          do (setf (gethash "character" entry) (string (code-char cp)))
             (setf (gethash "codepoint" entry) cp)
             (setf (gethash "code" entry) (format NIL "U+~4,'0x" cp))
             (if (gethash (gethash "name" entry) names)
                 (format T "~&Character ~a has name ~s, which is already taken by ~a~%"
                         (gethash "code" entry) (gethash "name" entry) (gethash (gethash "name" entry) names))
                 (setf (gethash (gethash "name" entry) names) (gethash "code" entry))))
    (sort data #'< :key (lambda (entry) (gethash "codepoint" entry)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (shasht:write-json data stream))))

(defun fonts (&optional (file (file "promptfont" "sfd")))
  (uiop:run-program (list "fontforge" "-c" "fnt = fontforge.open(argv[1])
for file in argv[2:]:
  fnt.generate(file)"
                          (uiop:native-namestring file)
                          (uiop:native-namestring (make-pathname :type "ttf" :defaults file))
                          (uiop:native-namestring (make-pathname :type "otf" :defaults file)))))

(defun all ()
  (fixup)
  (fonts)
  (txt)
  (css)
  (web))

(defun help ()
  (format T "PromptFont data management utilities

Commands:
  help   --- Show this help screen
  all    --- Performs all below commands. This is run by default
  fixup  --- Fixes up the glyphs.json file
  fonts  --- Generates the promptfont.ttf and .otf files
  txt    --- Generates the chars.txt file
  css    --- Generates the promptfont.css file
  web    --- Generates the index.html file

You typically do not need this utility as it is run automatically by
the GitHub CI when you create a PR.

https://shinmera.com/promptfont
"))

(defun main ()
  (destructuring-bind (argv0 &optional (command "all") &rest args) (uiop:raw-command-line-arguments)
    (declare (ignore argv0))
    (apply (intern (format NIL "~:@(~a~)" command) #.*package*) args)))
