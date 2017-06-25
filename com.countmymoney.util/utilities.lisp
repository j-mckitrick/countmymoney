;;; -*- mode:lisp; -*-

(in-package #:com.countmymoney.util)

;;; For some reason, these next 3 functions are not bound
;;; correctly unless they are wrapped in EVAL-WHEN.
;;; UPDATE: Since moving these functions here to a separate
;;; module and requiring it before the others, the problem
;;; seems to be solved.  For now.
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun extract-params (params)
  (loop
     for param in params
     when (listp param) collect (car param)
     else collect param))

;; Functions lifted from Arnesi ------------------------------------------------

(defun radix-values (radix)
  (assert (<= 2 radix 35)
          (radix)
          "RADIX must be between 2 and 35 (inclusive), not ~D." radix)
  (make-array radix
              :displaced-to "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              :displaced-index-offset 0
              :element-type 'character))

(defun trim-string (string &optional (char '(#\Space #\Tab #\Newline
                                             #\Return #\Linefeed)))
  (let ((chars char))
    (subseq string 
			(loop for index upfrom 0 below (length string)
			   when (not (member (aref string index) chars)) 
			   do (return index)
			   ;; if we get here we're trimming the entire string
			   finally (return-from trim-string ""))
			(loop for index downfrom (length string)
			   when (not (member (aref string (1- index)) chars))
			   do (return index)))))

(defun parse-float (float-string
                    &key (start 0) (end nil) (radix 10)
					(junk-allowed t)
					(type 'single-float)
					(decimal-character #\.))
  (let ((radix-array (radix-values radix))
        (integer-part 0)
        (mantissa 0)
        (mantissa-size 1)
        (sign 1))
    (with-input-from-string
		(float-stream (string-upcase (trim-string float-string)) :start start :end end)
	  (labels ((peek () (peek-char nil float-stream nil nil nil))
			   (next () (read-char float-stream nil nil nil))
			   (sign () ;; reads the (optional) sign of the number
				 (cond
				   ((char= (peek) #\+) (next) (setf sign 1))
				   ((char= (peek) #\-) (next) (setf sign -1)))
				 (integer-part))
			   (integer-part ()
				 (cond
				   ((position (peek) radix-array)
					;; the next char is a valid char
					(setf integer-part (+ (* integer-part radix)
										  (position (next) radix-array)))
					;; again
					(return-from integer-part (integer-part)))
				   ((null (peek))
					;; end of string
					(done))
				   ((char= decimal-character (peek))
					;; the decimal seperator
					(next)
					(return-from integer-part (mantissa)))                   
				   ;; junk
				   (junk-allowed (done))
				   (t (bad-string))))
			   (mantissa ()                 
				 (cond
				   ((position (peek) radix-array)
					(setf mantissa (+ (* mantissa radix)
									  (position (next) radix-array))
						  mantissa-size (* mantissa-size radix))
					(return-from mantissa
					  (mantissa)))
				   ((or (null (peek)) junk-allowed)
					;; end of string
					(done))
				   (t (bad-string))))
			   (bad-string ()
				 (error "Unable to parse ~S." float-string))
			   (done ()
				 (return-from parse-float
				   (coerce (* sign (+ integer-part (/ mantissa mantissa-size))) type))))
		(sign)))))
