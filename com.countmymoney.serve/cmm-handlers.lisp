;;; -*- mode:lisp; -*-

(in-package #:com.countmymoney.serve)

;;(declaim (optimize speed))
(declaim (optimize debug))

(define-easy-handler (handle-home :uri "/") ()
  (no-cache)
  (try-persisted-login)
  (handle-static-file "pub/xhtml/j.xhtml"))

(define-easy-handler (handle-logout :uri "/logout") ()
  (logout)
  (redirect "/"))

(defun rest-url-p (request method uri-pattern)
  (declare (ignore request) (type string method uri-pattern))
  (and (string= method (request-method*))
       (scan uri-pattern (request-uri*))))

(defmacro with-pattern-tokens ((pattern tokens) &body body)
  `(multiple-value-bind (whole-match regs)
       (scan-to-strings ,pattern (script-name*))
     (when whole-match
       (destructuring-bind (,tokens) (coerce regs 'list)
         ,@body))))

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
             `(define-resource-type ,type ,(list verb) ,(list method)))
           (define-resource-type-lambda-with-id (type verb method)
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
    (lambda () (format nil (encode-json-alist-to-string (get-session-info-alist)))))

(define-resource-gettable "categories"
    (lambda () (format nil (encode-json-alist-to-string *category->string*))))

(define-resource-gettable "months"
    (lambda () (format nil (encode-json-alist-to-string *month-strings*))))

(define-resource-gettable "years"
    (lambda () (format nil (encode-json-to-string *years*))))

(define-resource-gettable "clients"
    (lambda () (format nil (encode-json-to-string
							(mapcar #'.name (get-clients))))))

(define-resource-procedure "year"
    (lambda (year)
      (assert (and (>= year 2000) (<= year 2050)) ()
              "Year must be between 2000 and 2050.")
      (format t "Set year: ~A~%" year)
      (format t "User: ~A~%" (session-value 'user-idx))
      (when (session-value 'user-idx)
        (setf (session-value 'year) year)
        (set-cookie *year-cookie*
                    :value year
                    :expires (+ (get-universal-time) (* 60 60 24 7 2))))
      (app::reset-reports)
      (format nil (encode-json-alist-to-string (get-session-info-alist))))
  ((year :parameter-type 'integer)))

(define-resource-procedure "month"
    (lambda (month)
      (assert (and (>= month 1) (<= month 12)) () "Month must be between 1 and 12.")
      (format t "Set month: ~A~%" month)
      (format t "User: ~A~%" (session-value 'user-idx))
      (when (session-value 'user-idx)
        (setf (session-value 'month) month)
        (set-cookie *month-cookie*
                    :value month
                    :expires (+ (get-universal-time) (* 60 60 24 7 2))))
      (app::reset-reports)
      (format nil (encode-json-alist-to-string (get-session-info-alist))))
  ((month :parameter-type 'integer)))

(define-resource-gettable "entry"
    (lambda (category desc amount client newclient day)
      (declare (ignorable category desc amount client newclient day))
      (let ((entries (get-entries-for-month
                      (session-value 'user-idx)
                      (or (session-value 'month) 1)
                      (or (session-value 'year) 2010))))
        (if entries
            (format nil (encode-json-to-string
                         (mapcar #'entry->alist entries)))
            "[]")))
  (category desc amount client newclient day))

(define-resource-procedure "entry"
    (lambda (category desc amount client newclient day)
      (format t "Create entry.~%")
      ;; Tweak entry based on category if necessary.
      (cond
        ((or (string= category "invoicebilled")
             (string= category "invoicepaid"))
         (if (> (length newclient) 0)
             (setf desc (concatenate 'string "client:" newclient))
             (setf desc (concatenate 'string "client:" client))))
        ((string= category "ownerdraw")
         (setf desc (if (string= desc "") "Draw" desc)))
        ((string= category "capitalcontribution")
         (setf desc (if (string= desc "") "Contribution" desc))))
      (progn
        (format t "Category: ~A~%" category)
        (format t "Description: ~A~%" desc)
        (format t "Client: ~A~%" client)
        (format t "New client: ~A~%" newclient)
        (format t "Amount: ~A~%" amount)
        (format t "User: ~A~%" (session-value 'user-idx)))
      (when (session-value 'user-idx)
        (format t "Adding entry.~%")
        (add-transaction (session-value 'user-idx)
                         (string-upcase category)
                         desc
                         (parse-float amount)
                         (session-value 'month)
                         day
                         (session-value 'year))))
  (category desc amount client newclient (day :parameter-type 'integer)))

(define-resource-puttable "entry"
    (lambda (id category desc amount client newclient day)
      (declare (ignorable client newclient day))
      (let ((entry (db::get-one-entry id)))
        (setf category (.category entry)))
      ;; Tweak entry based on category if necessary.
      (cond
        ((or (string= category "invoicebilled")
             (string= category "invoicepaid"))
         (if (> (length newclient) 0)
             (setf desc (concatenate 'string "client:" newclient))
             (setf desc (concatenate 'string "client:" client))))
        ((string= category "ownerdraw")
         #+nil(setf desc "Draw"))
        ((string= category "capitalcontribution")
         #+nil(setf desc "Contribution")))
      (progn
        (format t "Category: ~A~%" category)
        (format t "Description: ~A~%" desc)
        (format t "Client: ~A~%" client)
        (format t "New client: ~A~%" newclient)
        (format t "Amount: ~A~%" amount)
        (format t "User: ~A~%" (session-value 'user-idx)))
      (when (session-value 'user-idx)
        (format t "Updating json entry.~%")
        (delete-child-transactions id)
        (update-transaction id (string-upcase category) desc (parse-float amount))))
  (category desc amount client newclient day))

(define-resource-deletable "entry"
    (lambda (id)
      ;;(declare (ignorable category desc amount client newclient day))
      (format t "Delete entry.~%")
      (when (session-value 'user-idx)
        (format t "DELETE for user: ~A~%" (session-value 'username))
        (let ((username (session-value 'username))
              (entry (db::get-one-entry id)))
          (when (string= username (.username (.user entry)))
            (delete-transaction (.idx entry))))))
  ())

(define-resource-get-one "entry"
    (lambda (id)
      (format nil (encode-json-to-string (entry->alist (db:get-one-entry id))))))

(defmacro object->json (report-class report-object)
  (sb-mop:finalize-inheritance (find-class report-class))
  (let* ((class (find-class report-class))
         (slots (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
         (object (gensym)))
    `(let ((,object ,report-object))    ; once only
       (list
        ,@(mapcar
           (lambda (slot)
             `(cons ,(intern (symbol-name slot) :keyword)
                    (format nil "~,2F" (slot-value ,object ',slot))))
           slots)))))

;;; Yes, this could be done with a more idiomatic WITH- construct,
;;; but I wanted to use SYMBOL-MACROLET, and this actually lets us
;;; wrap them all in one form without a WITH- for each definition.
(symbol-macrolet ((%user (session-value 'user-idx))
                  (%month (session-value 'month))
                  (%year (session-value 'year)))
  (define-resource-gettable "income"
      (lambda ()
        (format nil (encode-json-to-string
                     (object->json income-statement
                                   (get-income-statement %user %month %year))))))
  
  (define-resource-gettable "capital"
      (lambda ()
        (format nil (encode-json-to-string
                     (object->json capital-report
                                   (get-capital-report %user %month %year))))))
  
  (define-resource-gettable "cashflow"
      (lambda ()
        (format nil (encode-json-to-string
                     (object->json cashflow-report
                                   (get-cashflow-report %user %month %year))))))
  
  (define-resource-html "balance"
      (lambda ()
        (balance-sheet->table (get-balance-sheet %user %month %year))))

  (defun get-t-account-alist (t-account)
    (t-account->alist (get-t-account-for-month t-account %user %month %year))))

(defmacro define-t-account-json-resource (t-account)
  `(define-resource-gettable ,(string-downcase (symbol-name t-account))
       (lambda ()
         (format nil (encode-json-to-string (get-t-account-alist ',t-account))))))

(define-t-account-json-resource asset)
(define-t-account-json-resource revenue)
(define-t-account-json-resource expense)
(define-t-account-json-resource equity)

;;; rafael javascript plots
(define-easy-handler (handle-v :uri "/v") ()
  (handle-static-file "pub/xhtml/v.xhtml"))

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
				 ("/js/" "pub/js/")))))

#+nil
(defclass debuggable-acceptor (hunchentoot:acceptor)
  ())

#+nil
(defmethod acceptor-request-dispatcher ((*acceptor* debuggable-acceptor))
  (let ((dispatcher (call-next-method)))
	(lambda (request)
	  (handler-bind ((error #'invoke-debugger))
		(funcall dispatcher request)))))

;;; Entry points

(defun web-start ()
  #- (and)
  (when *app-log-enable*
    (setf *message-log-pathname* "log/messagelog")
    (setf *access-log-pathname* "log/accesslog"))
  (format t "; --> Starting...~%")
  (setf *session-secret* (reset-session-secret))
  (setf *my-acceptor* (make-instance 'easy-acceptor :port +server-port+))
  (start *my-acceptor*)
  (format t "; --> Ready!~%"))

(defun web-stop ()
  (format t "; --> Stopping...~%")
  (restart-case
      (stop *my-acceptor*)
    (ignore-simple-error (c)
      (format t "STOP-LISTENING error ignored: ~A~%" (type-of c)))
    (ignore-unbound-slot (c)
      (format t "STOP-LISTENING error ignored: ~A~%" (type-of c)))
    (ignore-shutdown-condition (c)
      (format t "STOP-LISTENING error ignored: ~A~%" (type-of c))))
  (format t "; --> Stopped.~%"))
