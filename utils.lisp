(in-package :guessing-game)

;; Definition of structure to manage a file input object for the guessing game
(defstruct (input-object)
  (x)
  (y)
  (z)
  (width)
  (height)
  (avg-y)
  (stdv-y)
  (min-y)
  (max-y)
  (avg-u)
  (stdv-u)
  (min-u)
  (max-u)
  (avg-v)
  (stdv-v)
  (min-v)
  (max-v))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

;; Function used to convert a string line into a list of numbers
(defun line-to-numbers (input-line)
  "Given a line read from a file as a string, gives back a list with
the real numbers corresponding to the string values"
  (with-input-from-string (in (replace-all input-line "," ""))
    (loop for x = (read in nil nil) while x collect x)))

;; Gets a list with the contents of the object and returns an input-object based on it.
(defun input-object-from-list(list-args)
  (let ((input-obj (make-input-object  :x (nth 0 list-args) :y (nth 1 list-args) :z (nth 2 list-args) :width (nth 3 list-args) :height (nth 4 list-args) :avg-y (nth 5 list-args) :avg-u (nth 6 list-args) :avg-v (nth 7 list-args) :min-y (nth 8 list-args) :min-u (nth 9 list-args) :min-v (nth 10 list-args) :max-y (nth 11 list-args) :max-u (nth 12 list-args) :max-v (nth 13 list-args) :stdv-y (nth 14 list-args) :stdv-u (nth 15 list-args) :stdv-v (nth 16 list-args))))
    input-obj))

;; Definition of function to load situation data
(defun read-file (filename)
  "Function that reads a file line by line returning the 
list of lines as strings."
  (let ((result '()))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
         while line
         do(setq result (append result (list (input-object-from-list (line-to-numbers line)))))))
    result))

;; Get random object from situation data
(defun get-random-situation-object(input-objects)
  "Gets a random situation object from the ones loaded from the file"
  (nth (+ (random (- (length input-objects) 1) (make-random-state t)) 1) input-objects))

;; Function that allows to get a random set of objects to make a game
(defun get-random-scene-objects(input-objects number-of-objects-per-scene)
  "Gets a random set of situational objects to execute a game based on it"
  (let ((result '()))
    (loop for i from 1 to number-of-objects-per-scene do
         (let ((obj (get-random-situation-object input-objects)))
           (if(eq (member obj result) nil)
              (setq result (append result (list obj)))
              (setq i (- i 1))) 
           ))
    result))

;; Call the accessor function of the inpu-object structure based on a string
(defun get-input-object-value (input-object slot-name)
  "Given a string for the slot name gets the value using the accessor function 
of the structure for the given input object"
  (funcall (symbol-function (find-symbol (string-upcase (concatenate 'string "input-object-" slot-name)))) input-object))

;; Functional setter for a property of a given input object
(defun set-input-object-value (input-object value slot-name)
  "Given a string for the slot name sets the value using the slot-value function 
of the structure for the given input object"
  (setf (slot-value input-object (find-symbol (string-upcase slot-name))) value))

;; List the values for a specific property in a set of input objects
(defun get-property-values (context feature-name)
  "List the values for a specific property in a set of input objects"
  (loop for obj in context
     collect (get-input-object-value obj feature-name)))
