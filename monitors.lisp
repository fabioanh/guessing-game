;; ----------
;; Monitoring
;; ----------

(in-package :guessing-game)

(defun run-experiments (strategies 
                        &key
                          (nr-of-objects 5)
                          (population-size 10)
                          (who-aligns :both) ;; can also be :speaker or :hearer
                          (initial-score 1.0)
                          (number-of-interactions 2000) (number-of-series 4)
                          (monitors 
                           '("export-communicative-success" "export-alignment-success" "export-nr-of-competitors")))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
   :experiment-class 'ng-experiment
   :number-of-interactions number-of-interactions
   :number-of-series number-of-series
   :monitors monitors
   :shared-configuration `((:total-nr-of-objects . ,nr-of-objects)
                           (:population-size . ,population-size)
                           (:who-aligns? . ,who-aligns)
                           (:initial-score . ,initial-score))
   :configurations strategies
   :output-dir (babel-pathname :directory 
                               `("examples" "assignment_3" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))
