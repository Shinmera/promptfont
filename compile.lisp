#|
exec sbcl \
  --noinform \
  --disable-debugger \
  --eval "(ql:quickload '(clip shasht pathname-utils) :silent T)" \
  --load "$0" \
  --eval '(main)' \
  --quit \
  --end-toplevel-options "${@:1}"
|#

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults 
                                (or *compile-file-pathname* *load-pathname* (error "LOAD this file."))))

(defun file (&optional name type)
  (make-pathname :name name :type type :defaults *here*))

(defun run (program &rest args)
  (let ((program-args (loop for arg = (car args)
                            until (or (null args) (keywordp arg))
                            collect arg
                            do (pop args))))
    (flet ((normalize (arg)
             (etypecase arg
               (string arg)
               (pathname (uiop:native-namestring arg))
               (real (princ-to-string arg)))))
      (apply #'uiop:run-program
             (list* program (loop for arg in program-args
                                  append (if (listp arg) 
                                             (mapcar #'normalize arg)
                                             (list (normalize arg)))))
             args))))

(defun outdated-p (out in)
  (or (not (probe-file out))
      (< (file-write-date out) (file-write-date in))))

(defun pathname< (a b)
  (string< (pathname-name a) (pathname-name b)))

(defun status (format &rest args)
  (format *error-output* "~&; ~?~%" format args))

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
                 (status "Character ~a has name ~s, which is already taken by ~a"
                         (gethash "code" entry) (gethash "name" entry) (gethash (gethash "name" entry) names))
                 (setf (gethash (gethash "name" entry) names) (gethash "code" entry))))
    (sort data #'< :key (lambda (entry) (gethash "codepoint" entry)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (shasht:write-json data stream))))

(defun fonts (&optional (file (file "promptfont" "sfd")))
  (run "fontforge" "-c" "fnt = fontforge.open(argv[1])
for file in argv[2:]:
  fnt.generate(file)"
       file
       (make-pathname :type "ttf" :defaults file)
       (make-pathname :type "otf" :defaults file)))

(defun atlas (&optional bank (s 64))
  (cond ((and bank (not (equal "all" bank)))
         (dolist (file (directory (merge-pathnames (format NIL "glyphs/~a/**/*.svg" bank) (file))))
           (let ((out (make-pathname :type "png" :defaults file)))
             (when (outdated-p out file)
               (status "Compiling ~a" out)
               (run "inkscape" "-w" s "-h" s file "-o" out))))
         (status "Compiling atlas-~a" bank)
         (run "montage"
              (sort (directory (merge-pathnames (format NIL "glyphs/~a/**/*.png" bank) (file))) #'pathname<)
              "-geometry" (format NIL "~ax~a+1+1" s s)
              "-background" "none"
              (file (format NIL "atlas-~a" bank) "png")))
        (T
         (dolist (dir (directory (merge-pathnames "glyphs/*/" (file))))
           (atlas (pathname-utils:directory-name dir))))))

(defun release (&optional (file (file "promptfont" "zip")))
  (run "zip" "-j" "-X" file
       (file "LICENSE" "txt")
       (file "README" "md")
       (file "index" "html")
       (file "glyphs" "json")
       (file "chars" "txt")
       (file "promptfont" "ttf")
       (file "promptfont" "otf")
       (file "promptfont" "css")
       (directory (file :wild "png"))))

(defun run-command (command &rest args)
  (apply (intern (format NIL "~:@(~a~)" command) #.*package*) args))

(defun all (&rest commands)
  (dolist (command (or commands '(fixup fonts txt css web atlas release)))
    (run-command command)))

(defun help ()
  (format T "PromptFont data management utilities

Commands:
  help    --- Show this help screen
  all [command...]
          --- Performs all below commands. This is run by default
  fixup   --- Fixes up the glyphs.json file
  fonts   --- Generates the promptfont.ttf and .otf files
  atlas [bank] [size] 
          --- Generates the glyph texture atlas files
              Defaults to all banks and size of 64
  txt     --- Generates the chars.txt file
  css     --- Generates the promptfont.css file
  web     --- Generates the index.html file
  release --- Generates a release zip

You typically do not need this utility as it is run automatically by
the GitHub CI when you create a PR.

https://shinmera.com/promptfont
"))

(defun main ()
  (destructuring-bind (argv0 &optional (command "all") &rest args) (uiop:raw-command-line-arguments)
    (declare (ignore argv0))
    (apply #'run-command command args)))
