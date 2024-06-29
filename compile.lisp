#|
exec sbcl \
  --noinform \
  --disable-debugger \
  --eval "(ql:quickload '(clip shasht pathname-utils) :silent T)" \
  --load "$0" \
  --eval '(promptfont-compiler::main)' \
  --quit \
  --end-toplevel-options "${@:1}"
|#

(defpackage #:promptfont-compiler
  (:use #:cl)
  (:export))

(in-package #:promptfont-compiler)

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

(defun join (sequence)
  (format NIL "~{~a~^ ~}" (coerce sequence 'list)))

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

(defmacro define-processor (type (stream &rest args) &body body)
  (let ((glyph (gensym "GLYPH")))
    `(defun ,type (&optional (file (file "glyphs" "json")) (output (file "promptfont" ,(string-downcase type))))
       (with-open-file (,stream output :direction :output :if-exists :supersede)
         ,(first body)
         (loop for ,glyph across (with-open-file (stream file)
                                   (shasht:read-json stream))
               do (let ,(loop for arg in args
                              collect `(,arg (gethash ,(string-downcase arg) ,glyph)))
                    ,@(butlast (rest body))))
         ,@(last body)))))

(define-processor txt (stream character)
  ()
  (write-string character stream)
  ())

(define-processor css (stream category code-name codepoint)
  (format stream "~
// PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont
@font-face{font-family:'promptfont'; src:url('promptfont.ttf');}
.pf{font-family:promptfont;font-style:normal;}")
  (unless (string= "alphabet" category)
    (format stream "~&.pf-~a:before{content:\"\\~x\"}~%" code-name codepoint))
  ())

(defun to-c-name (name &key (upcase T))
  (with-output-to-string (out)
    (loop for c across name
          do (case c
               (#\- (write-char #\_ out))
               (T (write-char (if upcase (char-upcase c) c) out))))))

(define-processor h (stream code-name character codepoint)
  (format stream "~
// PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont
#ifndef __PROMPTFONT_H__
#define __PROMPTFONT_H__~%")
  (format stream "~&#define PF_~a ~s~%" (to-c-name code-name) character)
  (format stream "~&#define PF_~a_INT 0x~5,'0x~%" (to-c-name code-name) codepoint)
  (format stream "~&#endif~%"))

(define-processor cs (stream code-name character codepoint)
  (format stream "~
// PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont
public static class PromptFont {~%")
  (format stream "~&    public const string ~a = ~s;~%" (to-c-name code-name) character)
  (format stream "~&    public const int ~a_INT = 0x~5,'0x;~%" (to-c-name code-name) codepoint)
  (format stream "~&
    static string Get(string name){
      return (string)(typeof(PromptFont).GetProperty(name).GetValue(null));
    }
    static int GetInt(string name){
      return (int)(typeof(PromptFont).GetProperty(name+\"_INT\").GetValue(null));
    }
}~%"))

(define-processor py (stream code-name character)
  (format stream "~
# PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont~%")
  (format stream "~&~a = ~s~%" (to-c-name code-name) character)
  ())

(define-processor lisp (stream code-name character)
  (format stream "~
;;;; PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont
\(defpackage #:org.shirakumo.fraf.promptfont (:use))
\(in-package #:org.shirakumo.fraf.promptfont)~%~%")
  (format stream "~&(cl:define-symbol-macro ~a ~s)~%" code-name character)
  (format stream "~&~%~
(cl:do-symbols (cl:symbol cl:*package*)
  (cl:export cl:symbol))~%"))

(define-processor lua (stream code-name character)
  (format stream "~
-- PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont
local GLYPHS = {}~%")
  (format stream "~&GLYPHS.~a = ~s~%" (to-c-name code-name) character)
  (format stream "~&return GLYPHS~%"))

(define-processor rs (stream code-name character codepoint)
  (format stream "~
// PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont~%")
  (format stream "~&pub const ~a: &'static str = ~s;~%" (to-c-name code-name) character)
  (format stream "~&pub const ~a_INT: u64 = ~a;~%" (to-c-name code-name) codepoint)
  ())

(define-processor gd (stream code-name character codepoint)
  (format stream "~
# PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont
class_name PromptFont
extends Resource~%")
  (format stream "~&const ~a: StringName = &~s;~%" (to-c-name code-name) character)
  (format stream "~&const ~a_INT: int = ~a;~%" (to-c-name code-name) codepoint)
  ())

(defun fixup (&optional (file (file "glyphs" "json")))
  (let ((data (with-open-file (stream file)
                (shasht:read-json stream)))
        (names (make-hash-table :test 'equalp)))
    (loop for entry across data
          for cp = (or (gethash "codepoint" entry)
                       (parse-integer (gethash "code" entry) :start 2 :radix 16)
                       (error "Bad entry: character is missing both codepoint and code attributes!"))
          do (setf (gethash "character" entry) (string (code-char cp)))
             (setf (gethash "codepoint" entry) cp)
             (setf (gethash "code" entry) (format NIL "U+~4,'0x" cp))
             (when (= 0 (length (gethash "tags" entry)))
               (status "Warning: character ~5,'0x is missing a tags array!" cp)
               (setf (gethash "tags" entry) #()))
             (cond ((null (gethash "code-name" entry))
                    (error "Character ~5,'0x is missing the code-name entry."
                           cp))
                   ((gethash (gethash "code-name" entry) names)
                    (error "Character ~5,'0x has code-name ~s, which is already taken by ~a"
                           cp (gethash "code-name" entry) (gethash (gethash "code-name" entry) names)))
                   (T
                    (setf (gethash (gethash "name" entry) names) (gethash "code-name" entry))))
             (when (null (gethash "name" entry))
               (error "Character ~5,'0x is missing a name." cp)))
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
       (file "index" "css")
       (file "glyphs" "json")
       (file "promptfont" "txt")
       (file "promptfont" "ttf")
       (file "promptfont" "otf")
       (file "promptfont" "css")
       (file "promptfont" "h")
       (file "promptfont" "cs")
       (file "promptfont" "py")
       (file "promptfont" "lisp")
       (file "promptfont" "lua")
       (file "promptfont" "rs")
       (file "promptfont" "gd")
       (directory (file :wild "png"))))

(defun run-command (command &rest args)
  (apply (intern (format NIL "~:@(~a~)" command) #.*package*) args))

(defun all (&rest commands)
  (dolist (command (or commands '(fixup fonts txt css h cs py lisp lua rs gd web atlas release)))
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
