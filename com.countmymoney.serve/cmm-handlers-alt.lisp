(in-package #:com.countmymoney.serve)

(declaim (optimize debug))
;;(declaim (optimize speed))

(define-easy-handler (handle-home :uri "/") ()
  (no-cache)
  (try-persisted-login)
  (handle-static-file "pub/xhtml/j.xhtml")
  #+nil(render-page-with-widgets "pub/xhtml/j.xhtml" *widgets*))

(define-easy-handler (handle-logout :uri "/logout") ()
  (logout)
  (redirect "/"))

(defun rest-url-p (request method uri-pattern)
  "Test if REQUEST url matches 'REST' METHOD and URI-PATTERN."
  (declare (ignore request) (type string method uri-pattern))
  ;;(format t "Method: ~A URI: ~A: Pattern: ~A~%" method (request-uri) uri-pattern)
  (and (string= method (request-method*))
       (scan uri-pattern (request-uri*))))

(defmacro with-pattern-tokens ((pattern tokens) &body body)
  "Bind the regs from PATTERN matches to TOKENS for BODY."
  `(multiple-value-bind (whole-match regs)
       (scan-to-strings ,pattern (script-name*))
     (when whole-match
       (destructuring-bind (,tokens) (coerce regs 'list)
         ,@body))))

;;; This macro allows us to define other macros
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

(macrolet ((define-resource-type-lambda (type verb method)
             "Resource with one function."
             `(define-resource-type ,type ,(list verb) ,(list method)))
           (define-resource-type-lambda-with-id (type verb method)
             "Resource with one function requiring an item id."
             `(define-resource-type ,type ,(list verb) ,(list method) :with-id id)))
  (define-resource-type-lambda html "get-html" "GET")
  (define-resource-type-lambda gettable "get" "GET")
  (define-resource-type-lambda procedure "procedure" "POST")
  (define-resource-type-lambda-with-id get-one "get-one" "GET")
  (define-resource-type-lambda-with-id puttable "put" "PUT")
  (define-resource-type-lambda-with-id deletable "delete" "DELETE"))

(define-resource-procedure "login"
    (lambda (user pass persist)
      (format nil (encode-json-alist-to-string (try-login user pass persist))))
  (user pass persist))

(define-resource-gettable "user"
    (lambda () (format nil (encode-json-alist-to-string (get-user-info-alist)))))

(define-resource-gettable "categories"
    (lambda () (format nil (encode-json-alist-to-string *category->string*))))

(define-resource-gettable "months"
    (lambda () (format nil (encode-json-alist-to-string *month-strings*))))

(define-resource-gettable "years"
    (lambda () (format nil (encode-json-to-string *years*))))

(define-resource-gettable "clients"
    (lambda () (format nil (encode-json-to-string
							(mapcar #'.name (get-clients))))))

(define-resource-procedure "month"
    (lambda (month)
      (assert (and (>= month 1) (<= month 12)) () "Month must be between 1 and 12.")
      (format t "Set month: ~A~%" month)
      (format t "User: ~A~%" (session-value 'user-idx))
      (when (session-value 'user-idx)
        (setf (session-value 'month) month)
        (set-cookie *month-cookie* :value month :expires (+ (get-universal-time) (* 60 60 24 7 2))))
      (app::reset-reports)
      (format nil (encode-json-alist-to-string (get-user-info-alist))))
  ((month :parameter-type 'integer)))

(defmacro define-resource-type-crud (base-verbs item-verbs)
  `(progn
     (define-resource-type crud-base ,base-verbs ("GET" "POST"))
     (define-resource-type crud-item ,item-verbs ("PUT" "DELETE") :with-id id)))

(define-resource-type-crud ("get-all" "create") ("update" "delete"))

(defun get-entries (category desc amount client newclient day)
  "Get all entries, with some search params as well."
  (declare (ignorable category desc amount client newclient day))
  (format t "Get entries.~%")
  (let ((entries (get-entries-for-month
                      (session-value 'user-idx)
                      (or (session-value 'month) 1)
                      (or (session-value 'year) 2010))))
        (if entries
            (format nil (encode-json-to-string
                         (mapcar #'entry->alist entries)))
            "[]")))

(defun create-entry (category desc amount client newclient day)
  "Create a new entry with these params."
  (format t "Create entry.~%")
  ;; Stuff client name into description.
  (when (or (string= category "invoicebilled")
			(string= category "invoicepaid"))
    (if (> (length newclient) 0)
		(setf desc (concatenate 'string "client" newclient))
		(setf desc (concatenate 'string "client" client))))
  (progn
    (format t "Category: ~A~%" category)
    (format t "Description: ~A~%" desc)
    (format t "Client: ~A~%" client)
    (format t "New client: ~A~%" newclient)
    (format t "Amount: ~A~%" amount)
    (format t "User: ~A~%" (session-value 'user)))
  (when (session-value 'user-idx)
    (format t "Adding entry.~%")
    (add-transaction (session-value 'user-idx)
                         (string-upcase category)
                         desc
                         (parse-float amount)
                         (session-value 'month)
                         day
                         (session-value 'year))))

(define-resource-crud-base "entry" (get-entries create-entry)
  (category desc amount client newclient (day :parameter-type 'integer)))

(defun update-entry (entry-idx category desc amount &optional client newclient day)
  (declare (ignorable client newclient day))
  (format t "Update entry.~%")
  (progn
    (format t "Category: ~A~%" category)
    (format t "Description: ~A~%" desc)
    (format t "Got amount: ~A~%" amount)
    (format t "User: ~A~%" (session-value 'user)))
  (when (session-value 'user-idx)
    (format t "Updating entry.~%")
    (delete-child-transactions entry-idx)
    (update-transaction entry-idx category desc (parse-float amount))))

(defun delete-entry-fn (entry-idx category desc amount client newclient day)
  (declare (ignorable category desc amount client newclient day))
  (format t "Delete entry.~%")
  (when (session-value 'user-idx)
    (format t "DELETE for user: ~A~%" (session-value 'username))
    (let ((username (session-value 'username))
		  (entry (db::get-one-entry entry-idx)))
      (unless (string= username (.username (.user entry)))
		(format t "Not allowed.~%")
		(return-from delete-entry-fn (values nil +http-forbidden+)))
      (when (string= username (.username (.user entry)))
		(delete-transaction (.idx entry))))))

(define-resource-crud-item "entry" (update-entry delete-entry-fn)
  (category desc amount client newclient (day :parameter-type 'integer)))

(defmacro object->json (report-class report-object)
  "Convert report to a JSON object.
Generate JSON from REPORT-OBJECT, an instance of REPORT-CLASS."
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

;;; Yes, this could be done with a more idiomatic WITH- construct,
;;; but I wanted to use SYMBOL-MACROLET, and this actually lets us
;;; wrap them all in this form without a WITH- for each definition.
(defmacro with-session-args (&body body)
  `(symbol-macrolet ((user (session-value 'user-idx))
                     (month (session-value 'month))
                     (year (session-value 'year)))
     ,@body))

(with-session-args
  (define-resource-gettable "income"
      (lambda () (format nil (encode-json-to-string
                              (object->json income-statement
                                            (get-income-statement user month year))))))
  
  (define-resource-gettable "capital"
      (lambda () (format nil (encode-json-to-string
                              (object->json capital-report
                                            (get-capital-report user month year))))))
  
  (define-resource-gettable "cashflow"
      (lambda () (format nil (encode-json-to-string
                              (object->json cashflow-report
                                            (get-cashflow-report user month year))))))
  
  (define-resource-html "balance"
      (lambda () (balance-sheet->table (get-balance-sheet user month year))))
  
  (defun get-t-account-alist (t-account)
    "Get display-friendly alist for entries from T-ACCOUNT."
    (t-account->alist (get-t-account-for-month t-account user month year))))

(defmacro define-t-account-resource (t-account)
  "Create a JSON getter for a T-ACCOUNT."
  `(define-resource-gettable ,(string-downcase (symbol-name t-account))
       (lambda () (format nil (encode-json-to-string (get-t-account-alist ',t-account))))))

(define-t-account-resource asset)
(define-t-account-resource revenue)
(define-t-account-resource expense)
(define-t-account-resource equity)

;;; rafael javascript plots
(define-easy-handler (handle-v :uri "/v") ()
  (handle-static-file "pub/xhtml/v.xhtml"))

(defclass debuggable-acceptor (hunchentoot:acceptor)
  ())

(defmethod acceptor-request-dispatcher ((*acceptor* debuggable-acceptor))
  (let ((dispatcher (call-next-method)))
	(lambda (request)
	  (handler-bind ((error #'invoke-debugger))
		(funcall dispatcher request)))))

;;; Web server entry points ----------------------------------------------------

(defun web-start ()
  (when *app-log-enable*
    (setf *message-log-pathname* "log/messagelog")
    (setf *access-log-pathname* "log/accesslog"))
  (format t "; --> Starting...~%")
  (setf *session-secret* (reset-session-secret))
  (setf *cmy-acceptor* (make-instance 'acceptor :port +server-port+))
  (setf *cmy-acceptor* (make-instance 'debuggable-acceptor :port +server-port+))
  (start *cmy-acceptor*)
  (format t "; --> Ready!~%"))

(defun web-stop ()
  (format t "; --> Stopping...~%")
  (restart-case
      (stop *cmy-acceptor*)
    (ignore-simple-error (c)
      (format t "STOP-LISTENING error ignored: ~A~%" (type-of c)))
    (ignore-unbound-slot (c)
      (format t "STOP-LISTENING error ignored: ~A~%" (type-of c)))
    (ignore-shutdown-condition (c)
      (format t "STOP-LISTENING error ignored: ~A~%" (type-of c))))
  (format t "; --> Stopped.~%"))

#+testing
(defun web-show-handlers ()
  "Show all handler urls.
easy-handler-alist "
  (flet ((show-easy-handler-closure (closure)))
    ))

;;; Initialize dispatch table --------------------------------------------------

(setf *dispatch-table*
      (nconc				; Remember: like @ splice

       ;; simple handlers (mostly for forms)
       (list 'dispatch-easy-handlers)

       ;; folders - need trailing slash
       (mapcar (lambda (args)
				 (apply #'create-folder-dispatcher-and-handler args))
			   `( ;; folders for content and other web files
				 ("/images/" "pub/images/")
				 ("/style/" "pub/style/")
				 ("/js/" "pub/js/")))

       ;; everything else
       (list
		#'default-dispatcher)))
