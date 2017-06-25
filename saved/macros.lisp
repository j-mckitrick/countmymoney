(in-package #:com.countmymoney.serve)

(declaim (optimize debug))
;;(declaim (optimize speed))

(defmacro with-pattern-tokens ((pattern tokens) &body body)
  "Bind the regs from PATTERN matches to TOKENS for BODY."
  `(multiple-value-bind (whole-match regs)
       (scan-to-strings ,pattern (script-name*))
     (when whole-match
       (destructuring-bind (,tokens) (coerce regs 'list)
		 ,@body))))

(defmacro wrap-as-xml-result (&body body)
  "Wrap all output from BODY as an xml document."
  `(with-output-to-string (*standard-output*)
     (with-xml-output (*standard-output*)
       ,@body)))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue nil)
     ,@body))

(defmacro object->xml (report-class report-object)
  "Expands a report of type REPORT-CLASS into an xml conversion.
Use gensym so we evaluate REPORT-OBJECT once-only rather than for each slot,
since REPORT-OBJECT is often a form that returns the report, not the report itself."
  (let* ((class (find-class report-class))
		 (slots (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
		 (object (gensym)))
    (flet ((symbol->tag (s) (string-downcase (symbol-name s))))
      `(let ((,object ,report-object))
		 (wrap-as-xml-result
		   (with-tag (,(symbol->tag (class-name class)))
			 ,@(mapcar
				(lambda (slot)
				  `(simple-tag ,(symbol->tag slot)
							   (format nil "~,2F" (slot-value ,object ',slot))))
				slots)))))))

(defmacro object->json (report-class report-object)
  "Expands a report of type REPORT-CLASS into an xml conversion.
Use gensym so we evaluate REPORT-OBJECT once-only rather than for each slot,
since REPORT-OBJECT is often a form that returns the report, not the report itself."
  (let* ((class (find-class report-class))
		 (slots (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
		 (object (gensym)))
    `(let ((,object ,report-object))
       (list
        ,@(mapcar
           (lambda (slot)
             `(cons ,(intern (symbol-name slot) :keyword)
                    (format nil "~,2F" (slot-value ,object ',slot))))
           slots)))))

;;; For some reason, these next 3 functions are not bound
;;; correctly unless they are wrapped in EVAL-WHEN.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun extract-params (params)
    (loop
	   for param in params
	   when (listp param) collect (car param)
	   else collect param)))

;;; These macros allows us to define other macros
;;; as abstractions for REST-style resources.

(defmacro define-resource-type (type verbs methods &key with-id)
  `(defmacro ,(symb 'define-resource- type) (resource-name functions &optional args)
     (let ((regex (format nil "^/~A~A$" resource-name ,(if with-id "/(\\d+)" "")))
		   (handlers (mapcar (lambda (verb)
							   (symb (string-upcase (format nil "handle-~A-" verb))
									 (string-upcase resource-name)))
							 ',verbs)))
       `(progn
		  ,@(mapcar (lambda (h m f)
					  `(define-easy-handler
						   (,h :uri (lambda (r) (rest-url-p r ,m ,regex))) ,args
                         ,,(if with-id
							   ``(with-pattern-tokens (,regex ,',with-id)
								   (funcall #',f ,',with-id ,@(extract-params args)))
							   ``(funcall #',f ,@(extract-params args)))))
					handlers ',methods
					,(if (and (> (length verbs) 1)
                              (> (length methods) 1))
                         'functions
                         '(list functions)))))))

;; Use MACROLET instead of wrapping single verb and method as lists.
(macrolet ((define-resource-type-lambda (type verb method &rest args)
			 `(define-resource-type ,type ,(list verb) ,(list method) ,@args)))
  (define-resource-type-lambda html "get-html" "GET")
  (define-resource-type-lambda gettable "get" "GET")
  (define-resource-type-lambda procedure "procedure" "POST"))

(define-resource-type get-one ("get-one") ("GET") :with-id id)
(define-resource-type puttable ("put") ("PUT") :with-id id)
(define-resource-type deletable ("delete") ("DELETE") :with-id id)

;; Convert the above 3 to use this.
(define-resource-type json-item ("get-item" "put-item" "delete-item") ("GET" "PUT" "DELETE") :with-id id)

(defmacro with-session-args (&body body)
  "Shortcut to pass session values as args.  USER, MONTH, and YEAR will be taken from session."
  `(symbol-macrolet ((user (session-value 'user-idx))
                     (month (session-value 'month))
                     (year (session-value 'year)))
     ,@body))
