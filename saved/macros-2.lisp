(in-package :com.countmymoney.serve)

#- (and)
(defmacro define-rest-handlers-definer (definer-name verbs methods itemp)
  (let* ((handler-template (if itemp
			       ``(with-pattern-tokens (,regex id)
				   (funcall #',f id ,@(extract-params args)))
			       ``(funcall #',f ,@(extract-params args)))))
    `(defmacro ,definer-name (resource-name functions args)
       (let ((handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
						    (string-upcase resource-name))) ,verbs))
	     (regex `(format nil "^/~A~A$" ,resource-name ,,(if itemp "/(\\d+)" ""))))
	 `(progn
	    ,@(mapcar (lambda (h m f)
			`(define-easy-handler
			     (,h :uri (lambda (r) (rest-url-p r ,m ,regex))) ,args
			   ,,handler-template))
		      handlers ,methods functions))))))

#- (and)
(defmacro def-base-handler (resource-name functions args)
  (let* ((regex (format nil "^/~A$" resource-name))
	 (verbs '("read-all" "create"))
	 (methods '("GET" "POST"))
	 (handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
						(string-upcase resource-name))) verbs)))
	 `(progn
	    ,@(mapcar (lambda (h m f)
			`(define-easy-handler
			     (,h :uri (lambda (r) (rest-url-p r ,m ,regex))) ,args
			   (funcall #',f ,@(extract-params args))))
		      handlers methods functions))))

(defmacro define-rest-handlers-definer% (definer-name verbs methods itemp)
  `(defmacro ,definer-name (resource-name functions args)
     (let ((regex (format nil "^/~A~A$" resource-name ,(if itemp "/(\\d+)" "")))
	   (handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
						  (string-upcase resource-name))) ,verbs)))
       `(progn
	  ,@(mapcar (lambda (h m f)
		      `(define-easy-handler
			   (,h :uri (lambda (r) (rest-url-p r ,m ,regex))) ,args
			 ,,(if itemp
			       ``(with-pattern-tokens (,regex id)
				   (funcall #',f id ,@(extract-params args)))
			       ``(funcall #',f ,@(extract-params args)))))
		    handlers ,methods functions)))))

(defmacro define-rest-handlers-definer%% (definer-name verbs methods itemp)
  `(defmacro ,definer-name (resource-name functions args)
     (let ((regex (format nil "^/~A~A$" resource-name ,(if itemp "/(\\d+)" "")))
	   (handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
						  (string-upcase resource-name))) ,verbs)))
       (macrolet ((base-wrapper (f args)
		    `(funcall #',f ,@(extract-params args)))
		  (item-wrapper (f id args)
		    `(with-pattern-tokens (,regex ,id)
			(funcall #',f id ,@(extract-params args))))))
       `(progn
	  ,@(mapcar (lambda (h m f)
		      `(define-easy-handler
			   (,h :uri (lambda (r) (rest-url-p r ,m ,regex))) ,args
			 ,(if ,itemp
			      (base-wrapper f args)
			      (item-wrapper f id args))))
		    handlers ,methods functions)))))

(defmacro define-rest-handlers-definer%% (definer-name verbs methods itemp)
  `(defmacro ,definer-name (resource-name functions args)
     (let ((regex (format nil "^/~A~A$" resource-name ,(if itemp "/(\\d+)" "")))
	   (handlers (mapcar (lambda (verb) (symb (string-upcase (format nil "handle-~A-" verb))
						  (string-upcase resource-name))) ,verbs)))
       `(progn
	  (macrolet ((wrap-handler (f args)
		       (if itemp
			   `(with-pattern-tokens (,regex id)
			      (funcall #',f id ,@(extract-params args)))
			   `(funcall #',f ,@(extract-params args)))))
	    ,@(mapcar (lambda (h m f)
			`(define-easy-handler
			     (,h :uri (lambda (r) (rest-url-p r ,m ,regex))) ,args
			   ,,(if itemp
				 ``(with-pattern-tokens (,regex id)
				     (funcall #',f id ,@(extract-params args)))
				 ``(funcall #',f ,@(extract-params args)))
			   ;;(wrap-handler ,f ,args)
			   ))
		      handlers ,methods functions))))))

(defmacro define-rest-handlers-definer%% (definer-name verbs methods itemp)
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

(define-rest-handlers-definer%% define-base-handlers '("read-all" "create") '("GET" "POST") nil)
(define-rest-handlers-definer%% define-item-handlers '("read-1" "update" "delete") '("GET" "POST" "DELETE") t)

(define-base-handlers "quux" '(foo-read-all foo-create) '(name))

(defmacro outer (name vars flag)
  `(defmacro ,name (args)
     (macrolet ((make-wrapper (v a)
		  (if ,flag
		      `(format t "T - Got: ~A ~A~%" ,v ,a)
		      `(format t "F - Got: ~A ~A~%" ,v ,a))))
       `(progn
	  ,@(mapcar (lambda (v a) (make-wrapper v a))
		    ,vars args)))))

(defun inner-fn (args flag)
  (macrolet ((make-wrapper (a)
	       `(if flag
		    (format t "T - Got: ~A~%" ,a)
		    (format t "F - Got: ~A~%" ,a))))
    (progn
      (make-wrapper args))))

(defun inner (args flag)
  `(macrolet ((make-wrapper (a)
		`(if flag
		     (format t "T - Got: ~A~%" ,a)
		     (format t "F - Got: ~A~%" ,a))))
     (progn
       ,@(mapcar
	  (lambda (a) (make-wrapper a))
	  args))))

(inner-fn '(1 2 3))
