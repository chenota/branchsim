#! /usr/bin/env -S sbcl --script

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