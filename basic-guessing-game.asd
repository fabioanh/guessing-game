(in-package #:asdf)

(defsystem :basic-guessing-game
  :name "basic-guessing-game"
  :author "Fabio Navarrete"
  :description "Guessing game system definition"
  :long-description "Lisp implementation of the guessing game"
  :depends-on (:experiment-framework 
               :utils
               :monitors
               :plot-raw-data
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "defpackage")
   (:file "utils")
   (:file "guessing-game")
   (:file "monitors")
   ))
