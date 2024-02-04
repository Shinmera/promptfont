(asdf:defsystem promptfont-compiler
  :components ((:file "compile.lisp"))
  :build-operation "program-op"
  :build-pathname "promptfont-compiler"
  :entry-point "promptfont-compiler::main"
  :depends-on (:clip :shasht :pathname-utils))
