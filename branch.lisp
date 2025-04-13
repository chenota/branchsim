#! /usr/bin/env -S sbcl --script

;; ARGUMENT PARSER

;; String of taken not taken to list
(defun parse-t-nt-string (string)
    (mapcar (lambda (c) (char= c #\T)) 
            (remove-if-not (lambda (c) (or (char= c #\T) (char= c #\N))) 
                           (coerce string 'list))))

;; Options
(defparameter cli/args '(
    (:pattern :positional :handler parse-t-nt-string)
    (:bits :name "bits" :handler parse-integer :default 2)
    (:iters :name "iters" :handler parse-integer :default 10)
    (:alias :name "alias" :handler parse-t-nt-string)
    (:verbose :name "verbose")
))

;; Extract row from cli/options that matches
(defun get-row (test)
    (reduce (lambda (acc new)
                    (cond (acc acc)
                          ((funcall test new) new)
                          (t nil)))
            cli/args
            :initial-value nil))

;; Extract value of parameter in param list
(defun parameter-v (parameter row)
    ;; Position of item
    (let ((position (position parameter (cdr row))))
         (if position
             (nth (+ position 1) (cdr row))
             nil)))

;; Does parameter exist in parameter list
(defun parameter-p (parameter row)
    (member parameter (cdr row)))

;; Check prefix
(defun string-prefix-p (string prefix)
  (every #'char= string prefix))

;; Read command line arguments
(defun read-args ()
           ;; Read supplied arguments
    (let ((args (first (reduce (lambda (acc arg)
                    ;; Read current accumulator
                    (destructuring-bind 
                        (l read-to positionals)
                        acc
                              ;; Trying to read argument to field
                        (cond (read-to
                               (list (cons (list (first read-to) (funcall (parameter-v :handler read-to) arg)) l) nil positionals))
                              ;; Reading optional argument
                              ((string-prefix-p "--" arg) 
                               (let ((row (get-row (lambda (row) (string= (parameter-v :name row) (subseq arg 2))))))
                                    (if (parameter-p :handler row)
                                        (list l row positionals)
                                        (list (cons (list (first row) t) l) nil positionals))))
                              ;; Reading positional argument
                              (t
                               (list (cons (list (first (first positionals)) (funcall (parameter-v :handler (first positionals)) arg)) l) nil (cdr positionals))))))
            (cdr *posix-argv*)
            :initial-value (list nil 
                                 nil 
                                 (remove-if-not (lambda (row) (parameter-p :positional row)) cli/args))))))
        ;; Add default values for non-supplied arguments
        (reduce (lambda (acc row)
                        (if (member (first row) args :key #'first)
                            acc
                            (cons (list (first row) (parameter-v :default row)) acc)))
                cli/args
                :initial-value args)))

;; Get argument from args list
(defun get-argument (argument args)
    (second (first (remove-if-not (lambda (arg) (equal (first arg) argument)) args))))

;; BRANCH PREDICTOR CODE

;; Predictor type
(defstruct predictor size state)

;; Maximum state of a predictor
(defun max-state (predictor)
    (- (expt 2 (predictor-size predictor)) 1))

;; New predictor
(defun new-predictor (size) 
    (make-predictor :size size 
                    :state 0))

;; Update predictor based on ground truth
(defun update (predictor truth)
    ;; Return new predictor w/ same size
    (make-predictor :size (predictor-size predictor)
                    ;; Increment or decrement based on truth value, clamp
                    :state (if truth
                               (min (max-state predictor)
                                    (+ 1 (predictor-state predictor)))
                               (max 0
                                    (- (predictor-state predictor) 1)))))

;; Get t/f prediction from predictor
(defun predict (predictor)
    ;; Check if MSB of predictor is 1 (i.e., value >= 2^(size - 1))
    (>= (predictor-state predictor) 
        (expt 2 
              (- (predictor-size predictor) 1))))

;; Run main program
       ;; Parse command line arguments
(let* ((args (read-args))
       ;; Regular pattern
       (pattern-default (get-argument :pattern args))
       ;; Alias pattern
       (alias (get-argument :alias args))
       ;; Pattern to test
       (pattern (if alias
                    ;; Combine alias and original pattern
                    (loop for i from 0 below (max (length pattern-default) (length alias)) by 1 appending 
                          (list (nth (mod i (length pattern-default)) pattern-default) 
                                (nth (mod i (length alias)) alias)))
                    ;; Only use default pattern
                    pattern-default))
       ;; Number of iterations
       (iters (get-argument :iters args))
       ;; Verbose
       (verbose (get-argument :verbose args))
       ;; Loop variables
       (predictor (new-predictor (get-argument :bits args)))
       (tally 0))
      ;; Verbose print
      (if verbose (format t "Predictor~CPrediction~CGround Truth~CHit Rate~%" #\tab #\tab #\tab) nil)
      ;; Count correct predictions
      (loop for i from 0 below iters by 1 do 
                ;; Get ground truth and prediction
                (let ((ground-truth (nth (mod i (length pattern)) pattern))
                        (prediction (predict predictor)))
                    ;; If prediction was correct, update tally
                    (if (eq ground-truth prediction)
                        (setq tally (+ tally 1))
                        nil)
                    ;; Verbose print
                    (if verbose
                        (format t 
                                "~V,'0B~C~C~C~C~C~2F%~%" 
                                (get-argument :bits args) 
                                (predictor-state predictor) 
                                #\tab 
                                (if prediction #\T #\N) 
                                #\tab 
                                (if ground-truth #\T #\N) 
                                #\tab 
                                (* 100 (/ tally (+ i 1))))
                        nil)
                    ;; Update predictior w/ ground truth
                    (setq predictor (update predictor ground-truth))))
      ;; Print results
      (if verbose nil (format t "Hits~C~D~%Misses~C~D~%Rate~C~2F%~%" #\tab tally #\tab (- iters tally) #\tab (* 100 (/ tally iters)))))