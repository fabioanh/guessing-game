;;(defpackage :guessing-game
;;  (:use :common-lisp :utils :monitors :web-interface :plot-raw-data
;;        :experiment-framework))

(in-package :guessing-game)

;; ----------------
;; Main definitions
;; ----------------

(defclass gg-experiment (experiment)
  ()
  (:documentation "The experiment class for a Guessing Game"))

(defclass gg-agent (agent)
  ((lexicon 
    :documentation "The word meaning mappings of the agent"
    :type list :initform nil :accessor lexicon)
   (context
    :documentation "The context is a subset of world which contains
    some objects."
    :type list :initform nil :accessor context)
   (topic
    :documentation "A random element selected from the context."
    :initform nil :accessor topic)
   (applied-lex 
    :documentation "The name that was used. This is reset at the
    beginning of each game."
    :type (or null ng-lex) :initform nil :accessor applied-lex))
  (:documentation "A gessing-game agent"))

(define-configuration-default-value :number-of-object-per-scene 5)
(define-configuration-default-value :population-size 30)
(define-configuration-default-value :context-scaling t)
(defparameter *scalable-features* '("x" "y" "z" "width" "height"))


(defmethod initialize-instance :after ((experiment gg-experiment) &key)
  "Method initialize-instance called by make-instance. Here, code executed after
method is called for an instance of the gg-experiment class"
  (setf (world experiment)
	(read-file "~/Documents/vub/programming_paradigms/dev/Babel2/examples/assignment_3/data/object-features.txt"))
  
  (setf (population experiment)
	(loop for i from 1 to (get-configuration experiment :population-size)
           for agent = (make-instance 'gg-agent :id i
                                      :experiment experiment
                                      :world (world experiment))
           collect agent)))

;; Definition of the binary-tree node data structure
(defclass tree-node ()
  ((score 
    :documentation "Score representing the number of times the category has been used"
    :initform 0.5 :type short-float :accessor score)
   (game-number
    :documentation "Number game identifier. Used to know the age of the node"
    :type integer :accessor game-number)
   (lower-bound
    :documentation "Lower bound for the used score"
    :initform 0.0 :type short-float :accessor lower-bound)
   (upper-bound
    :documentation "Upper bound for the used score"
    :initform 1.0 :type short-float :accessor upper-bound)
   (left-child
    :documentation "Left child of the node data structure"
    :initform nil :type tree-node :accessor left-child)
   (right-child
    :documentation "Right child of the node data structure"
    :initform nil :type tree-node :accessor right-child))
  (:documentation "Defines a tree node that has two children"))

;; -----------------------------------------------
;; production and parsing + invention and adoption
;; -----------------------------------------------

(defclass gg-lex ()
  ((meaning 
    :initform nil :initarg :meaning :accessor meaning :type symbol)
   (form 
    :initform "" :initarg :form :accessor form :type list)
   (score 
    :initform 0.5 :accessor score :initarg :score :type number)))


;; ---------------------------------------------------
;;                      ALIGNMENT
;; ---------------------------------------------------

;; convenience methods for alignment

(defun inc-score (lex &optional (delta 0.1) (upper-bound 1.0))
  (incf (score lex) delta)
  (when (> (score lex) upper-bound)
    (setf (score lex) upper-bound))
  lex)

(defun dec-score (lex agent &key (delta 0.1) (lower-bound 0.0) 
                              (remove-on-lower-bound t))
  (decf (score lex) delta)
  (when (<= (score lex) lower-bound)
    (if remove-on-lower-bound
        (setf (lexicon agent) (remove lex (lexicon agent)))
        (setf (score lex) lower-bound)))
  (lexicon agent))

;; ----------------------------------------
;;            TREE MANIPULATION
;; ----------------------------------------
;; Method in charge of the expansion (bifurcate) of a node. Creates new children for it
(defmethod expand-node ((node tree-node) game-number)
  "Expands a node's children dividing its interval in two"
  (setf (left-child node) (make-instance 'tree-node :game-number game-number 
                                         :lower-bound (lower-bound node) 
                                         :upper-bound (/ (+ (lower-bound node) (upper-bound node)) 2.0)))
  (setf (right-child node) (make-instance 'tree-node :game-number game-number 
                                          :lower-bound (/ (+ (lower-bound node) (upper-bound node)) 2.0) 
                                          :upper-bound (upper-bound node)))
  node)

;; Function to get a random child 
(defmethod random-child ((node tree-node))
  "Gets a random child of the given node"
  (if(eq (random 2 (make-random-state t)) 0)
     (left-child node)
     (right-child node)))

(defmethod random-expand ((node tree-node) game-number)
  "Expands a random leaf in a given tree"
  (if(and (left-child node) (right-child node) nil)
     (random-expand (random-child node) game-number)
     (expand-node node game-number)))


;; ----------------------------------------
;;              INTERACTION
;; ----------------------------------------

;; Function to scale a list of input objects based on the scalable features
(defmethod scale-context ((context list))
  "Scale the scalable features for the given list of objects"
  (loop for sf in *scalable-features* do
       (let ((max-feature-value (apply 'max (get-property-values context sf))))
         (loop for obj in context do
              (set-input-object-value obj (/ (get-input-object-value obj sf) max-feature-value) sf))))
  context)

(defmethod initialize-agent ((agent gg-agent) &key context (context-scaling t))
  (setf (applied-lex agent) nil)
  (setf (communicated-successfully agent) t)
  (if context-scaling
      (setf (context agent) (scale-context context))
      (setf (context agent) context))
  (setf (topic agent) (first context)))

(defmethod interact ((experiment gg-experiment) interaction &key)
  (declare (ignore interaction))
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment))
        (context (random-subset (world experiment) :include-empty-set? nil)))
    (initialize-agent speaker :context context)
    (initialize-agent hearer :context context)
    (unless (produce speaker)
      (invent-name speaker) 
      (produce speaker))

(defmethod interact ((experiment gg-experiment) interaction &key)
  (declare (ignore interaction))
    (setf (utterance hearer) (utterance speaker))
    (unless (parse hearer)
      (adopt-name hearer)
      (setf (communicated-successfully hearer) nil)
      (setf (communicated-successfully speaker) nil)))
  (finish-interaction experiment))
