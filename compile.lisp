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
  (:shadow #:search #:remove)
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
                                  append (typecase arg
                                           (null ())
                                           (list (mapcar #'normalize arg))
                                           (T (list (normalize arg))))))
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

(defun load-glyphs (&optional (file (file "glyphs" "json")))
  (with-open-file (stream file)
    (shasht:read-json stream)))

(defun write-glyphs (data &optional (file (file "glyphs" "json")))
  (sort data #'< :key (lambda (entry) (gethash "codepoint" entry)))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (shasht:write-json data stream)))

(defun parse-glyphs (&optional (file (file "glyphs" "json")))
  (let ((sections (make-hash-table :test 'equal)))
    (loop for glyph across (load-glyphs)
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
         (loop for ,glyph across (load-glyphs)
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
#define __PROMPTFONT_H__
#include <string.h>
#include <stdlib.h>

struct PF_icon{
  const char *name;
  const char *string;
  const int codepoint;
};

struct PF_icon PF_icons[] = {")
  (format stream "~&#define PF_~a ~s~%" (to-c-name code-name) character)
  (format stream "~&#define PF_~a_INT 0x~5,'0x~%" (to-c-name code-name) codepoint)
  (format stream "~&  {\"PF_~a\", ~s, 0x~5,'0x},~%" (to-c-name code-name) character codepoint)
  (format stream "~&};

static int PF_cmp(const void *s1, const void *s2){
  const struct PF_icon *e1 = s1;
  const struct PF_icon *e2 = s2;
  return strcmp(e1->name, e2->name);
}

static const char *PF_get(const char *name){
  struct PF_icon *result, key = {name};
  result = bsearch(&key, PF_icons, sizeof(PF_icons)/sizeof(struct PF_icon), sizeof(struct PF_icon), PF_cmp);
  if(result) return result->string;
  return 0;
}

static int PF_get_int(const char *name){
  struct PF_icon *result, key = {name};
  result = bsearch(&key, PF_icons, sizeof(PF_icons)/sizeof(struct PF_icon), sizeof(struct PF_icon), PF_cmp);
  if(result) return result->codepoint;
  return 0;
}
#endif~%"))

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

(define-processor py (stream code-name character codepoint)
  (format stream "~
# PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont~%")
  (format stream "~&~a = ~s~%" (to-c-name code-name) character)
  (format stream "~&~a_INT = ~a~%" (to-c-name code-name) codepoint)
  (format stream "~&
def get(name):
  return globals()[name]

def get_int(name):
  return globals()[name+\"_INT\"]
"))

(define-processor lisp (stream code-name character)
  (format stream "~
;;;; PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont
\(defpackage #:org.shirakumo.fraf.promptfont (:use))
\(in-package #:org.shirakumo.fraf.promptfont)~%~%")
  (format stream "~&(cl:define-symbol-macro ~a ~s)~%" code-name character)
  (format stream "~&(cl:setf (cl:symbol-value '~a) ~s)~%" code-name character)
  (format stream "~&
(cl:defun get (get)
  (cl:etypecase get
    (cl:symbol (get (cl:symbol-name get)))
    (cl:string (cl:symbol-value (cl:find-symbol get #.cl:*package*)))))
(cl:do-symbols (cl:symbol cl:*package*)
  (cl:export cl:symbol))~%"))

(define-processor lua (stream code-name character codepoint)
  (format stream "~
-- PromptFont by Yukari \"Shinmera\" Hafner, accessible at https://shinmera.com/promptfont
local GLYPHS = {}~%")
  (format stream "~&GLYPHS.~a = ~s~%" (to-c-name code-name) character)
  (format stream "~&GLYPHS.~a_INT = ~a~%" (to-c-name code-name) codepoint)
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
  (format stream "~&
static var promptfont: PromptFont
static func _static_init() -> void:
    promptfont = PromptFont.new()

static func get_str(name: StringName) -> StringName:
    return promptfont.get(name)
static func get_int(name: StringName) -> int:
    return promptfont.get(name+\"_INT\")~%"))

(defun remove (glyph)
  (let ((data (load-glyphs)))
    (setf data (remove-if (lambda (entry)
                            (flet ((p (property) (gethash property entry)))
                              (or (string-equal glyph (p "character"))
                                  (string-equal glyph (p "code"))
                                  (string-equal glyph (princ-to-string (p "codepoint")))
                                  (string-equal glyph (p "name"))
                                  (string-equal glyph (p "code-name")))))
                          data))
    (write-glyphs data)))

(defun normalize-glyph (entry &optional names)
  (let ((cp (or (gethash "codepoint" entry)
                (when (gethash "code" entry) (parse-integer (gethash "code" entry) :start 2 :radix 16))
                (when (gethash "character" entry) (char-code (char (gethash "character" entry) 0)))
                (error "Bad entry: character is missing both codepoint, code, and character attributes!"))))
    (setf (gethash "character" entry) (string (code-char cp)))
    (setf (gethash "codepoint" entry) cp)
    (setf (gethash "code" entry) (format NIL "U+~4,'0x" cp))
    (unless (gethash "category" entry)
      (status "Warning: character ~5,'0x is missing a category!" cp)
      (setf (gethash "category" entry) "misc"))
    (when (= 0 (length (gethash "tags" entry)))
      (status "Warning: character ~5,'0x is missing a tags array!" cp)
      (setf (gethash "tags" entry) #()))
    (cond ((null (gethash "code-name" entry))
           (error "Character ~5,'0x is missing the code-name entry."
                  cp))
          ((and names (gethash (gethash "code-name" entry) names))
           (error "Character ~5,'0x has code-name ~s, which is already taken by ~a"
                  cp (gethash "code-name" entry) (gethash (gethash "code-name" entry) names)))
          (names
           (setf (gethash (gethash "name" entry) names) (gethash "code-name" entry))))
    (when (null (gethash "name" entry))
      (error "Character ~5,'0x is missing a name." cp))))

(defun add (codepoint name code-name &optional category &rest tags)
  (let ((entry (make-hash-table :test 'equal)))
    (cond ((= 1 (length codepoint))
           (setf (gethash "character" entry) codepoint))
          ((string= "U+" codepoint :end2 2)
           (setf (gethash "code" entry) codepoint))
          ((every #'digit-char-p codepoint)
           (setf (gethash "codepoint" entry) (parse-integer codepoint)))
          (T
           (error "Unknown character format: ~s" codepoint)))
    (setf (gethash "name" entry) name)
    (setf (gethash "code-name" entry) code-name)
    (setf (gethash "category" entry) category)
    (setf (gethash "tags" entry) (coerce tags 'vector))
    (normalize-glyph entry)
    (let ((data (load-glyphs)))
      (loop for glyph across data
            do (flet ((check-match (property)
                        (when (equalp (gethash property entry) (gethash property glyph))
                          (error "A character with the same ~a already exists!" property))))
                 (check-match "name")
                 (check-match "code-name")
                 (check-match "character")
                 (check-match "code")
                 (check-match "codepoint")))
      (write-glyphs (concatenate 'vector glyphs (vector entry))))))

(defun fixup (&optional (file (file "glyphs" "json")))
  (let ((data (load-glyphs file))
        (names (make-hash-table :test 'equalp)))
    (loop for entry across data
          do (normalize-glyph entry names))
    (write-glyphs data file)))

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
       (probe-file (file "promptfont" "run"))
       (probe-file (file "promptfont" "exe"))
       (probe-file (file "promptfont" "o"))
       (directory (file :wild "png"))))

(defun query (&rest glyphs)
  (let ((data (load-glyphs)))
    (unless glyphs
      (error "Specify at least one glyph to query."))
    (dolist (glyph glyphs)
      (loop for entry across data
            do (flet ((p (property) (gethash property entry)))
                 (when (or (string-equal glyph (p "character"))
                           (string-equal glyph (p "code"))
                           (string-equal glyph (princ-to-string (p "codepoint")))
                           (string-equal glyph (p "name"))
                           (string-equal glyph (p "code-name")))
                   (format T "Character: ~12t~a
Code: ~12t~a
Codepoint: ~12t~d
Category: ~12t~a
Name: ~12t~a
Code-Name: ~12t~a
Tags: ~12t~{~a~^, ~}~%~%"
                           (p "character") (p "code") (p "codepoint") (p "category") (p "name") (p "code-name")
                           (coerce (p "tags") 'list))
                   (return)))))))

(defun search (&rest query)
  (let ((data (load-glyphs)))
    (loop for entry across data
          do (flet ((p (property) (gethash property entry))
                    (? (property) (loop for part in query
                                        always (cl:search part property :test #'char-equal))))
               (when (or (? (p "character"))
                         (? (p "code"))
                         (? (p "name"))
                         (? (p "code-name"))
                         (? (p "category"))
                         (loop for tag across (p "tags") thereis (? tag)))
                 (format T "Character: ~12t~a
Code: ~12t~a
Codepoint: ~12t~d
Category: ~12t~a
Name: ~12t~a
Code-Name: ~12t~a
Tags: ~12t~{~a~^, ~}~%~%"
                         (p "character") (p "code") (p "codepoint") (p "category") (p "name") (p "code-name")
                         (coerce (p "tags") 'list)))))))

(defun run-command (command &rest args)
  (apply (intern (format NIL "~:@(~a~)" command) #.*package*) args))

(defun all (&rest commands)
  (dolist (command (or commands '(fixup fonts txt css h cs py lisp lua rs gd web atlas release)))
    (run-command command)))

(defun help ()
  (format T "PromptFont data management utilities
by Yukari \"Shinmera\" Hafner

Usage: ~a [command] args...

Query Data:
  help    --- Show this help screen

  query   --- Show info for one or more glyphs

  search  --- Search for matching glyphs

Modify Data:
  remove [character/code/codepoint/name/code-name]
          --- Remove a glyph

  add codepoint name code-name [category] [tag...]
          --- Add a new glyph

  fixup   --- Fixes up the glyphs.json file

Compile Data:
  all [command...]
          --- Performs all below commands. This is run by default

  fonts   --- Generates the promptfont.ttf and .otf files

  atlas [bank] [size] 
          --- Generates the glyph texture atlas files
              Defaults to all banks and size of 64

  web     --- Generates the index.html file

  release --- Generates a release zip

  txt     --- Generate the respective glyph mapping source file
  css
  h
  cs
  py
  lisp
  lua
  rs
  gd

You typically do not need this utility as it is run automatically by
the GitHub CI when you create a PR.

https://shinmera.com/promptfont
" (uiop:argv0)))

(defun main ()
  (destructuring-bind (argv0 &optional (command "help") &rest args) (uiop:raw-command-line-arguments)
    (declare (ignore argv0))
    (apply #'run-command command args)))

(defun dump ()
  (sb-ext:save-lisp-and-die
   (file "promptfont" #+win32 "exe" #+linux "run" #-(or win32 linux) "o")
   :toplevel #'main
   :executable T
   :compression 22))
