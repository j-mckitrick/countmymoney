(in-package :com.countmymoney.serve)

(defparameter cashflow-slots '(begin cash-in cash-out change end))
;;#- (and)
(defparameter cashflow-slots
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots
					 (class-of (get-cashflow-report 1)))))

(defmacro with-pattern-tokens ((pattern tokens) &body body)
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

;;#-(and)
(defmacro cashflow->xml (o)
  "Expands into a cashflow-to-xml conversion."
  `(wrap-as-xml-result
     (with-tag ("cashflow")
       ,@(mapcar
	  (lambda (slot)
	    `(simple-tag ,(string-downcase (symbol-name slot))
			 (format nil "~,2F" (,slot ,o))))
	  cashflow-slots))))

(eval-when (:load-toplevel :compile-toplevel :execute)
(defmacro report->xml (report-class report-object)
  "Expands a report into an xml conversion."
  (let ((c (gensym))
	(o (gensym)))
    `(let ((,c (find-class ',report-class))
	   (,o ,report-object))
       (wrap-as-xml-result
	 (with-tag ((string-downcase (symbol-name (class-name ,c))))
	   ,@(mapcar
	      (lambda (slot)
		`(simple-tag
		  ,(string-downcase (symbol-name slot))
		  (format nil "~,2F" (slot-value ,o ',slot))))
	      (mapcar #'sb-mop:slot-definition-name
		      (sb-mop:class-slots (find-class report-class))
		      ;;(sb-mop:class-slots report-class)
		      )))))))

(defmacro report->xml%% (report)
  "Expands a report into an xml conversion."
  `(list
    ,@(mapcar
       (lambda (slot)
	 (string-downcase (symbol-name slot)))
       (mapcar #'sb-mop:slot-definition-name
	       (sb-mop:class-slots (class-of report))))))

(defmacro report->xml%%% (report)
  "Expands a report into an xml conversion."
  `(list
    ,@(mapcar #'sb-mop:slot-definition-name
	      (sb-mop:class-slots (class-of report)))))

(defmacro report->xml%%%% (report)
  "Expands a report into an xml conversion."
  `(mapcar #'sb-mop:slot-definition-name
	   (sb-mop:class-slots (class-of ,report))))

(defmacro report->xml%%%%5 (report)
  "Expands a report into an xml conversion."
  `(report->xml%%%% ,report))

)

Remove macros:
--------------

(defparameter cashflow-slots '(begin cash-in cash-out change end))

#- (and)
(eval-when (:load-toplevel :execute)
  (defmacro report->xml (report-class report-object)
    "Expands a report into an xml conversion."
    (let ((c (gensym))
	  (o (gensym)))
      `(let ((,c (find-class ',report-class))
	     (,o ,report-object))
	 (wrap-as-xml-result
	   (with-tag ((string-downcase (symbol-name (class-name ,c))))
	     ,@(mapcar
		(lambda (slot)
		  `(simple-tag
		    ,(string-downcase (symbol-name slot))
		    (format nil "~,2F" (slot-value ,o ',slot))))
		(mapcar #'sb-mop:slot-definition-name
			(sb-mop:class-slots (find-class report-class))))))))))

#- (and)
(defmacro report->xml (report-class report-object)
  "Expands a report into an xml conversion."
  (let ((c (gensym))
	(o (gensym)))
    `(let ((,c (find-class ',report-class))
	   (,o ,report-object))
       (wrap-as-xml-result
	 (with-tag ((string-downcase (symbol-name (class-name ,c))))
	   ,@(mapcar
	      (lambda (slot)
		`(simple-tag
		  ,(string-downcase (symbol-name slot))
		  (format nil "~,2F" (slot-value ,o ',slot))))
	      (mapcar #'sb-mop:slot-definition-name
		      (sb-mop:class-slots (find-class report-class)))))))))

(defmacro object->xml% (class object)
  "Expands an object into an xml conversion."
  `(wrap-as-xml-result
     (with-tag ((string-downcase (symbol-name (class-name (find-class ',class)))))
       ,@(mapcar
	  (lambda (slot)
	    `(simple-tag
	      ,(string-downcase (symbol-name slot))
	      (format nil "~,2F" (slot-value ,object ',slot))))
	  (mapcar #'sb-mop:slot-definition-name
		  (sb-mop:class-slots (find-class class)))))))

(defmacro object->xml%% (report-class report-object)
  "Expands a report into an xml conversion."
  (let ((o (gensym)))
    `(let ((,o ,report-object))
       (wrap-as-xml-result
	 (with-tag ((string-downcase
		     (symbol-name (class-name (find-class ,report-class)))))
	   ,@(mapcar
	      (lambda (slot)
		`(simple-tag
		  ,(string-downcase (symbol-name slot))
		  (format nil "~,2F" (slot-value ,o ',slot))))
	      (mapcar #'sb-mop:slot-definition-name
		      (sb-mop:class-slots (find-class report-class)))))))))

(defmacro object->xml%%% (report-class report-object)
  "Expands a report into an xml conversion."
  (let ((o (gensym)))
    `(let ((,o ,report-object))
       (wrap-as-xml-result
	 (with-tag (,(string-downcase
		      (symbol-name (class-name (find-class `,report-class)))))
	   ,@(mapcar
	      (lambda (slot)
		`(simple-tag
		  ,(string-downcase (symbol-name slot))
		  (format nil "~,2F" (slot-value ,o ',slot))))
	      (mapcar #'sb-mop:slot-definition-name
		      (sb-mop:class-slots (find-class report-class)))))))))

(defmacro object->xml%%%% (report-class report-object)
  "Expands a report into an xml conversion.
Use gensym O so we only evaluate REPORT-OBJECT once, rather than for each slot,
since it is usually a form that generates the report, not the report itself."
  (let ((o (gensym)))
    `(let ((,o ,report-object))
       (wrap-as-xml-result
	 (with-tag (,(string-downcase
		      (symbol-name (class-name (find-class `,report-class)))))
	   ,@(mapcar
	      (lambda (slot)
		`(simple-tag
		  ,(string-downcase (symbol-name slot))
		  (format nil "~,2F" (slot-value ,o ',slot))))
	      (mapcar #'sb-mop:slot-definition-name
		      (sb-mop:class-slots (find-class `,report-class)))))))))

(defmacro object->xml% (report-class report-object)
  "Expands a report into an xml conversion.
Use gensym so we only evaluate REPORT-OBJECT once rather than for each slot,
since it is usually a form that returns the report, not the report itself."
  (flet ((symbol->string (s)
	   (string-downcase (symbol-name s))))
    (let ((o (gensym)))
      `(let ((,o ,report-object))
	 (wrap-as-xml-result
	   (with-tag (,(symbol->string (class-name (find-class `,report-class))))
	     ,@(mapcar
		(lambda (slot)
		  `(simple-tag
		    ,(symbol->string slot)
		    (format nil "~,2F" (slot-value ,o ',slot))))
		(mapcar #'sb-mop:slot-definition-name
			(sb-mop:class-slots (find-class `,report-class))))))))))



(defmacro define-rest-handlers-definer (name verbs methods itemp)
  `(defmacro ,name (resource-name functions args)
     (let ((handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
						  (string-upcase resource-name))) ,verbs))
	   (methods ,methods)
	   (re ,(if itemp
		    `(format nil "^/~A/(\\d+)$" resource-name)
		    `(format nil "^/~A$" resource-name))))
       `(progn
	  ,@(mapcar (lambda (h m f)
		      `(define-easy-handler
			   (,h :uri (lambda (r) (rest-url-p r ,m ,re))) ,args
			 ,,(if itemp
			       ``(with-pattern-tokens (,re id)
				   (funcall #',f id ,@(extract-params args)))
			       ``(funcall #',f ,@(extract-params args)))))
		    handlers methods functions)))))

(define-rest-handlers-definer define-base-rest-handlers '("read" "create") '("GET" "POST") nil)
(define-rest-handlers-definer define-item-rest-handlers '("update" "delete") '("POST" "DELETE") t)

(defmacro define-base-rest-handlers% (resource-name functions args)
  (let ((handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
					       (string-upcase resource-name)))
			  '("read" "create")))
	(methods '("GET" "POST"))
	(re (format nil "^/~A$" resource-name)))
    `(progn
       ,@(mapcar (lambda (h m f)
		   `(define-easy-handler
			(,h :uri (lambda (r) (rest-url-p r ,m ,re))) ,args
		      (funcall #',f ,@(extract-params args))))
		 handlers methods functions))))

(defmacro define-item-rest-handlers% (resource-name functions args)
  (let ((handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
					       (string-upcase resource-name)))
			  '("update" "delete")))
	(methods '("POST" "DELETE"))
	(re (format nil "^/~A/(\\d+)$" resource-name)))
    `(progn
       ,@(mapcar (lambda (h m f)
		   `(define-easy-handler
			(,h :uri (lambda (r) (rest-url-p r ,m ,re))) ,args
		      (with-pattern-tokens (,re id)
			(funcall #',f id ,@(extract-params args)))))
		 handlers methods functions))))

(defmacro define-rest-handlers% (resource-name read-fn create-fn update-fn delete-fn resource-args)
  "Define four REST handlers for RESOURCE-NAME.
Each of the basic HTTP/REST functions \(GET, POST, UPDATE, and DELETE\) are mapped to READ-FN, CREATE-FN, UPDATE-FN, and DELETE-FN, respectively."
  (let ((read-handler (symb "HANDLE-READ-" (string-upcase resource-name)))
	(create-handler (symb "HANDLE-CREATE-" (string-upcase resource-name)))
	(update-handler (symb "HANDLE-UPDATE-" (string-upcase resource-name)))
	(delete-handler (symb "HANDLE-DELETE-" (string-upcase resource-name)))
	(base-re (format nil "^/~A$" resource-name))
	(item-re (format nil "^/~A/(\\d+)$" resource-name)))
    `(progn
       ,@(mapcar (lambda (h m f)
		   `(define-easy-handler
			(,h :uri (lambda (r) (rest-url-p r ,m ,base-re))) ,resource-args
		      (funcall #',f ,@(extract-params resource-args))))
		 `(,read-handler ,create-handler)
		 `("GET" "POST")
		 `(,read-fn ,create-fn))
       ,@(mapcar (lambda (h m f)
		   `(define-easy-handler
			(,h :uri (lambda (r) (rest-url-p r ,m ,item-re))) ,resource-args
		      (with-pattern-tokens (,item-re id)
			(funcall #',f id ,@(extract-params resource-args)))))
		 `(,update-handler ,delete-handler)
		 `("POST" "DELETE")
		 `(,update-fn ,delete-fn)))))


(defmacro define-rest-handlers-definer (definer-name verbs methods)
  `(defmacro ,definer-name (resource-name functions args itemp)
     (let* ((handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
						   (string-upcase resource-name))) ,verbs))
	    (regex `(format nil "^/~A~A$" resource-name ,(if itemp "/(\\d+)" ""))))
       `(progn
	  ,@(mapcar (lambda (h m f)
		      `(define-easy-handler
			   (,h :uri (lambda (r) (rest-url-p r ,m ,regex))) ,args
			 ,(if itemp
			     `(with-pattern-tokens (,regex id)
				(funcall #'f id (extract-params args)))
			     `(funcall #'f (extract-params args)))))
		    handlers ,methods functions)))))

(define-rest-handlers-definer)

(define-rest-handlers-definer define-base-rest-handlers '("read-all" "create") '("GET" "POST") nil)
;;(define-rest-handlers-definer define-item-rest-handlers '("update" "delete") '("POST" "DELETE") t)
(define-rest-handlers-definer define-item-rest-handlers '("read-1" "update" "delete")
  '("GET" "POST" "DELETE")
  t)

(defmacro define-rest-handlers (resource-name base-fns item-fns resource-args)
  `(progn
     (define-base-rest-handlers ,resource-name ,base-fns ,resource-args)
     (define-item-rest-handlers ,resource-name ,item-fns ,resource-args)))

(defmacro define-rest-handlers-definer (definer-name verbs methods itemp)
  (let* ((regex `(format nil "^/~A~A$" resource-name ,(if itemp "/(\\d+)" "")))
	 (handler-template (if itemp
			       ``(with-pattern-tokens (,,regex id)
				   (funcall #',f id ,@(extract-params args)))
			       ``(funcall #',f ,@(extract-params args)))))
    `(defmacro ,definer-name (resource-name functions args)
       (let ((handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
						    (string-upcase resource-name))) ,verbs))
	     (methods ,methods))
	 `(progn
	    ,@(mapcar (lambda (h m f)
			`(define-easy-handler
			     (,h :uri (lambda (r) (rest-url-p r ,m ,,regex))) ,args
			   ,,handler-template))
		      handlers methods functions))))))

(define-rest-handlers-definer define-base-rest-handlers '("read-all" "create") '("GET" "POST") nil)
(define-rest-handlers-definer define-item-rest-handlers '("read-1" "update" "delete")
  '("GET" "POST" "DELETE") t)

(defmacro define-rest-handlers (resource-name base-fns item-fns resource-args)
  `(progn
     (define-base-rest-handlers ,resource-name ,base-fns ,resource-args)
     (define-item-rest-handlers ,resource-name ,item-fns ,resource-args)))


#|
(define-rest-handlers "foo" (foo-read-all foo-create) (foo-read-1 foo-update foo-delete) (name))

(PROGN
  (DEFINE-BASE-REST-HANDLERS "foo" (FOO-READ-ALL FOO-CREATE) (NAME))
  (DEFINE-ITEM-REST-HANDLERS "foo" (FOO-READ-1 FOO-UPDATE FOO-DELETE) (NAME)))

(DEFMACRO DEFINE-BASE-REST-HANDLERS (RESOURCE-NAME FUNCTIONS ARGS)
  (LET ((HANDLERS
         (MAPCAR
          (LAMBDA (VERB)
            (SYMB (STRING-UPCASE (FORMAT NIL "handle-~A-" VERB))
                  (STRING-UPCASE RESOURCE-NAME)))
          '("read-all" "create")))
        (METHODS '("GET" "POST")))
    `(PROGN
       ,@(MAPCAR
	  (LAMBDA (H M F)
	    `(DEFINE-EASY-HANDLER
		 (,H :URI
		     (LAMBDA (R)
		       (REST-URL-P R ,M ,(FORMAT NIL "^/~A~A$" RESOURCE-NAME ""))))
		 ,ARGS (FUNCALL #',F ,@(EXTRACT-PARAMS ARGS))))
	  HANDLERS METHODS FUNCTIONS))))

(DEFMACRO DEFINE-ITEM-REST-HANDLERS (RESOURCE-NAME FUNCTIONS ARGS)
  (LET ((HANDLERS
         (MAPCAR
          (LAMBDA (VERB)
            (SYMB (STRING-UPCASE (FORMAT NIL "handle-~A-" VERB))
                  (STRING-UPCASE RESOURCE-NAME)))
          '("read-1" "update" "delete")))
        (METHODS '("GET" "POST" "DELETE")))
    `(PROGN
       ,@(MAPCAR
	  (LAMBDA (H M F)
	    `(DEFINE-EASY-HANDLER
		 (,H :URI
		     (LAMBDA (R)
		       (REST-URL-P R ,M
				   ,(FORMAT NIL "^/~A~A$" RESOURCE-NAME "/(\\d+)"))))
		 ,ARGS
	       (WITH-PATTERN-TOKENS
		   (,(FORMAT NIL "^/~A~A$" RESOURCE-NAME "/(\\d+)") ID)
		 (FUNCALL #',F ID ,@(EXTRACT-PARAMS ARGS)))))
	  HANDLERS METHODS FUNCTIONS))))

(PROGN
  (PROGN
    (DEFINE-EASY-HANDLER
	(HANDLE-READ-ALL-FOO :URI (LAMBDA (R) (REST-URL-P R "GET" "^/foo$"))) (NAME)
      (FUNCALL #'FOO-READ-ALL NAME))
    (DEFINE-EASY-HANDLER
	(HANDLE-CREATE-FOO :URI (LAMBDA (R) (REST-URL-P R "POST" "^/foo$"))) (NAME)
      (FUNCALL #'FOO-CREATE NAME)))
  (PROGN
    (DEFINE-EASY-HANDLER
	(HANDLE-READ-1-FOO :URI (LAMBDA (R) (REST-URL-P R "GET" "^/foo/(\\d+)$")))
	(NAME)
      (WITH-PATTERN-TOKENS ("^/foo/(\\d+)$" ID) (FUNCALL #'FOO-READ-1 ID NAME)))
    (DEFINE-EASY-HANDLER
	(HANDLE-UPDATE-FOO :URI (LAMBDA (R) (REST-URL-P R "POST" "^/foo/(\\d+)$")))
	(NAME)
      (WITH-PATTERN-TOKENS ("^/foo/(\\d+)$" ID) (FUNCALL #'FOO-UPDATE ID NAME)))
    (DEFINE-EASY-HANDLER
	(HANDLE-DELETE-FOO :URI (LAMBDA (R) (REST-URL-P R "DELETE" "^/foo/(\\d+)$")))
	(NAME)
      (WITH-PATTERN-TOKENS ("^/foo/(\\d+)$" ID) (FUNCALL #'FOO-DELETE ID NAME)))))
|#

(defmacro define-rest-handlers-definer-alt (definer-name with-id-p &rest method-verb-pairs)
  `(defmacro ,definer-name (resource-name functions args)
     (let ((regex (format nil "^/~A~A$" resource-name ,(if with-id-p "/(\\d+)" "")))
	   (handlers (mapcar (lambda (pair)
			       (symb (string-upcase (format nil "handle-~A-" (cdr pair)))
				     (string-upcase resource-name)))
			     ',method-verb-pairs)))
       `(progn
	  ,@(mapcar (lambda (h m f)
		      `(define-easy-handler
			   (,h :uri (lambda (r) (rest-url-p r ,m ,regex))) ,args
			 ,,(if with-id-p
			       ``(with-pattern-tokens (,regex id)
				   (funcall #',f id ,@(extract-params args)))
			       ``(funcall #',f ,@(extract-params args)))))
		    handlers (mapcar #'cadr ',method-verb-pairs) functions)))))

(define-rest-handlers-definer-alt define-base-handlers nil ("GET" "read-all"))
(define-rest-handlers-definer-alt define-item-handlers t ("GET" "read-1"))

(defmacro define-rest-handlers (resource-name base-fns item-fns resource-args)
  `(progn
     (define-base-rest-handlers ,resource-name ,base-fns ,resource-args)
     (define-item-rest-handlers ,resource-name ,item-fns ,resource-args)))

;; Move these to handlers.lisp when macro is done being tested.
(define-rest-handlers-definer define-base-rest-handlers ("read-all" "create") ("GET" "POST") nil)
(define-rest-handlers-definer define-item-rest-handlers ("read-1" "update" "delete")
  ("GET" "POST" "DELETE") t)

(defmacro define-rest-handlers (resource-name base-fns item-fns resource-args)
  `(progn
     (define-base-rest-handlers ,resource-name ,base-fns ,resource-args)
     (define-item-rest-handlers ,resource-name ,item-fns ,resource-args)))

(define-rest-handlers "foo" (foo-read-all foo-create) (foo-read-1 foo-update foo-delete) (name))

(defun foo-read-1 (id bar)
  (format t "Read a foo: ~A ~A~%" id bar))

(defun foo-read-all (bar)
  (format t "Read a foo: ~A~%" bar))

(defun foo-create (bar)
  (format t "Create a foo: ~A~%" bar))

(defun foo-update (id bar)
  (format t "Update a foo: ~A ~A~%" id bar))

(defun foo-delete (id bar)
  (format t "Delete a foo: ~A ~A~%" id bar))

