;;;; Hunchentoot handlers.

(in-package :com.countmymoney.serve)

(declaim (optimize debug))
;;(declaim (optimize speed))

;; Simple handlers -------------------------------------------------------------

(define-easy-handler (handle-home :uri "/") ()
  "Main home page handler.  Checks for persistent login."
  (no-cache)
  (try-persisted-login)
  (handle-static-file "pub/xhtml/x.xhtml"))

(define-easy-handler (handle-logout :uri "/logout") ()
  "Log this user out of the system completely."
  (logout)
  (redirect "/"))

;; REST url detection ----------------------------------------------------------

(defun rest-url-p (request method uri-pattern)
  "Test if REQUEST url matches 'REST' METHOD and URI-PATTERN."
  (declare (ignore request))
  ;;(format t "Method: ~A URI: ~A: Pattern: ~A~%" method (request-uri) uri-pattern)
  (and (string= method (request-method*))
       (scan uri-pattern (request-uri*))))

;; JSON handlers ---------------------------------------------------------------

(define-resource-gettable-json "user"
    (lambda () (format nil (encode-json-alist-to-string (get-user-info)))))

(define-resource-settable-json "login"
    (lambda (user pass persist) (format nil (encode-json-alist-to-string (try-login user pass persist))))
  (user pass persist))

(define-resource-gettable-json "categories"
    (lambda () (format nil (encode-json-alist-to-string *category->string*))))

(define-resource-gettable-json "months"
    (lambda () (format nil (encode-json-alist-to-string *months*))))

(define-resource-gettable-json "clients"
    (lambda () (format nil (encode-json-to-string
							(mapcar #'.name (get-clients))))))

(define-resource-settable-json-item "month"
    (lambda (month)
      (let ((month (or (parse-integer month) 1)))
		(assert (and (>= month 1) (<= month 12)) () "Month must be between 1 and 12.")
		(format t "Set month: ~A~%" month)
		(when (session-value 'user-idx)
		  (setf (session-value 'month) month)
		  (when (session-value 'persist)
			(save-session-to-cookies))))
      (format nil (encode-json-alist-to-string (get-user-info)))))

(define-resource-gettable-json "jentry"
    (lambda (category desc amount client newclient day)
      (declare (ignorable category desc amount client newclient day))
      (format nil (encode-json-to-string
                   (mapcar #'entry->alist (get-entries-for-month
                                           (session-value 'user-idx)
                                           (or (session-value 'month) 1)
                                           (or (session-value 'year) 2010))))))
   (category desc amount client newclient day))

;; XML handlers ---------------------------------------------------------------

(defun get-entries (category desc amount client newclient day)
  "Get all entries, with some search params as well."
  (declare (ignorable category desc amount client newclient day))
  (format t "Get entries.~%")
  (entry-objects->xml (get-entries-for-month
					   (session-value 'user-idx)
					   (or (session-value 'month) 1)
                       (or (session-value 'year) 2010))))

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
    (setf (content-type*) "application/xml")
    (entry-objects->xml (list (add-transaction (session-value 'user-idx)
											   (string-upcase category)
											   desc
											   (parse-float amount)
											   (session-value 'month)
											   day
                                               (session-value 'year))))))

(define-resource-crud-base "entry" (get-entries create-entry)
  (category desc amount client newclient (day :parameter-type 'integer)))

(defun get-entry (entry-idx category desc amount client newclient day)
  (declare (ignorable entry-idx category desc amount client newclient day))
  (format t "Get entries.~%")
  (entry-objects->xml (list (get-one-entry entry-idx))))

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
    (setf (content-type*) "application/xml")
    (delete-child-transactions entry-idx)
    (entry-objects->xml (list (update-transaction entry-idx
												  category
												  desc
												  (parse-float amount))))))

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
		(delete-transaction (.idx entry)))))
  (get-entries category desc amount client newclient day))

(define-resource-crud-item "entry" (get-entry update-entry delete-entry-fn)
  (category desc amount client newclient (day :parameter-type 'integer)))

(defun get-t-account (account)
  (t-account->xml (get-t-account-for-month account
										   (session-value 'user-idx)
										   (session-value 'month)
                                           (session-value 'year))))

(define-resource-report "assets" (lambda () (get-t-account 'asset)))
(define-resource-report "revenue" (lambda () (get-t-account 'revenue)))
(define-resource-report "expenses" (lambda () (get-t-account 'expense)))
(define-resource-report "draw" (lambda () (get-t-account 'draw)))

(define-resource-report "income"
    (lambda () (object->xml income-statement
							(get-income-statement (session-value 'user-idx)
												  (session-value 'month)
                                                  (session-value 'year)))))
(define-resource-report "capital"
    (lambda () (object->xml capital-report
							(get-capital-report (session-value 'user-idx)
												(session-value 'month)
                                                (session-value 'year)))))

(define-resource-report "cashflow"
    (lambda () (object->xml cashflow-report
							(get-cashflow-report (session-value 'user-idx)
												 (session-value 'month)
                                                 (session-value 'year)))))

(define-resource-html "balance"
    (lambda () (balance-sheet->table (get-balance-sheet (session-value 'user-idx)
														(session-value 'month)
                                                        (session-value 'year)))))

;;; Web server entry points ----------------------------------------------------

(defun web-start ()
  "Start the server."
  (when *app-log-enable*
    (setf *message-log-pathname* "log/messagelog")
    (setf *access-log-pathname* "log/accesslog"))
  (format t "; --> Starting...~%")
  (setf *session-secret* (reset-session-secret))
  (setf *cmy-acceptor* (make-instance 'acceptor :port +server-port+))
  (start *cmy-acceptor*)
  (format t "; --> Ready!~%"))

(defun web-stop ()
  "Stop mark-my-words web application.
Stop Hunchentoot and disconnect from the database."
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
