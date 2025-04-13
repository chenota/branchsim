#! /usr/bin/env -S sbcl --script

;; ARGUMENT PARSER

;; Options
(defparameter cli/args '(
    (:pattern :positional :handler identity)
    (:bits :short #\b :long "bits" :handler parse-integer :default 2)
    (:iters :short #\i :long "iters" :handler parse-integer :default 1000)
    (:alias :short #\a :long "alias" :handler identity)
    (:verbose :short #\v :long "verbose")
))

;; Extract row from cli/options that matches
(defun get-row (test)
    (reduce (lambda (acc new)
                    (cond (acc acc)
                          ((funcall test new) new)
                          (t nil)))
            (remove-if (lambda (row) (parameter-p :positional row)) cli/args)
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
    (let ((args (first (reduce (lambda (acc arg)
                    ;; Read current accumulator
                    (destructuring-bind 
                        (l read-to positionals)
                        acc
                              ;; Trying to read argument to field
                        (cond (read-to
                               (list (cons (list (first read-to) (funcall (parameter-v :handler read-to) arg)) l) nil positionals))
                              ;; Reading long argument
                              ((string-prefix-p "--" arg) 
                               (let ((row (get-row (lambda (row) (string= (parameter-v :long row) (subseq arg 2))))))
                                    (if (parameter-p :handler row)
                                        (list l row positionals)
                                        (list (cons (list (first row) t) l) nil positionals))))
                              ;; Reading short argument
                              ((string-prefix-p "-" arg)
                               (let ((row (get-row (lambda (row) (char= (parameter-v :short row) (char arg 1))))))
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
        (reduce (lambda (acc row)
                        (if (member (first row) args :key #'first)
                            acc
                            (cons (list (first row) (parameter-v :default row)) acc)))
                cli/args
                :initial-value args)))

;; Get argument from args list
(defun get-argument (argument args)
    (second (first (remove-if-not (lambda (arg) (equal (first arg) argument)) args))))

;; CODE

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
(format t "~A~%" (get-argument :alias (read-args)))