;;; -*- mode:lisp; -*-

(in-package #:com.countmymoney.db)

(declaim (optimize debug))
;;(declaim (optimize speed))

(defvar *db-lock* (sb-thread:make-mutex :name "db-lock"))

(defun connect-to-db ()
  (format t "Connecting to db~%")
  (connect '("localhost" "cmm" "jcm" "none")
           :database-type :postgresql :encoding :latin-1))

(defun disconnect-from-db ()
  (disconnect))

(clsql-sys:file-enable-sql-reader-syntax)

(defun get-cmm-user (username)
  (declare (type string username))
  (with-mutex (*db-lock*)
    (car (select 'cmm-user
                 :flatp t
                 :where
                 [= [slot-value 'cmm-user 'username] username]))))

(defun get-entries-for-month (user-idx month year)
  (format t "For month: ~A year: ~A~%" month year)
  (with-mutex (*db-lock*)
    (let ((start-date (make-date :year year :month month))
          (end-date (make-date :year year :month (1+ month))))
      (select 'entry
              :flatp t
              :refresh t
              :order-by '([date] [idx])
              :where
              [and
              [= [slot-value 'entry 'user-idx] user-idx]
              [>= [slot-value 'entry 'date] start-date]
              [< [slot-value 'entry 'date] end-date]
              ]))))

(defun add-entry (user-idx category description amount month day year)
  "Create an entry and add it to the general ledger."
  (assert (and user-idx month) () "Must have USER-IDX and MONTH.")
  (format t "Day: ~A~%" day)
  (let ((entry (make-instance 'entry
                              :idx (1+ (or (caar (query "SELECT MAX(IDX) FROM ENTRY"))
                                           1))
                              :date (make-date :year year :month month :day day)
                              :user-idx user-idx
                              :category (string-upcase category)
                              :description description
                              :amount amount)))
    (format t "New entry created: ~A ~A~%" (.idx entry) (.date entry))
    (with-mutex (*db-lock*)
      (update-record-from-slots entry '(idx date user-idx category description amount)))
    (format t "Entry idx: ~A~%" (.idx entry))
    entry))

(defun update-entry (entry category description amount)
  "Update only CATEGORY, DESCRIPTION, and AMOUNT in ENTRY."
  (setf (.category entry) (string-upcase category)
        (.description entry) description
        (.amount entry) amount)
  (with-mutex (*db-lock*)
    (update-record-from-slots entry '(category description amount)))
  entry)

(defun save-entry-description (entry)
  (with-mutex (db::*db-lock*)
    (update-record-from-slot entry 'description)))

(defun total-account (user-idx account &key category desc month year)
  (assert user-idx () "Must have valid USER-IDX.")
  (let ((query (format nil "SELECT SUM(amount) FROM ~A WHERE " account)))
    (when user-idx
      (setf query (concatenate 'string query (format nil "user_idx = ~A " user-idx))))
    (when category
      (setf query
            (concatenate 'string query (format nil "AND category = '~A' " category))))
    (when desc
      (setf query
            (concatenate 'string query (format nil "AND description = '~A' " desc))))
    (when (and month year)
      (let ((max-days (clsql-sys::days-in-month month year)))
        (setf query
              (concatenate 'string query
                           (format nil "AND date >= '~A-~A-01' " year month)
                           (format nil "AND date <= '~A-~A-~A'" year month max-days)))))
    (with-mutex (*db-lock*)
      (coerce (or (car (query query :flatp t)) 0.0) 'single-float))))

(defun delete-entry (entry)
  (when entry
    (format t "Deleting entry: ~A~%" entry)
    (with-mutex (*db-lock*)
      (delete-instance-records entry))))

(defun get-one-entry (idx)
  (with-mutex (*db-lock*)
    (car
     (select 'entry
             :flatp t
             :refresh t
             :where
             [= [slot-value 'entry 'idx] idx]))))

(defun get-clients ()
  (with-mutex (*db-lock*)
    (select 'cmm-client :flatp t :refresh t)))

(defun find-client (name)
  (with-mutex (*db-lock*)
    (car (select 'cmm-client
                 :flatp t
                 :where [= [slot-value 'cmm-client 'name] name]))))

(defun add-client (name)
  (let ((client (make-instance 'cmm-client :name name)))
    (with-mutex (*db-lock*)
      (update-record-from-slot client 'name))))

;; Balances:
;; Assets - debit
;; Liabilities - credit
;; Equity - credit
;; Revenue - credit
;; Expense - debit
;; Equity - debit

(defun add-t-account-entries (entry debit-account debit-name credit-account credit-name)
  (format t "Creating t-accounts for entry.~%")
  (let ((debit (make-instance debit-account
                              :user-idx (.user-idx entry)
                              :entry-idx (.idx entry)
                              :category :debit
                              :description debit-name
                              :amount (.amount entry)
                              :date (.date entry)))
        (credit (make-instance credit-account
                               :user-idx (.user-idx entry)
                               :entry-idx (.idx entry)
                               :category :credit
                               :description credit-name
                               :amount (.amount entry)
                               :date (.date entry)))
        (slots '(user-idx entry-idx category description amount date)))
    (format t "Created t-accounts, updating db.~%")
    (with-mutex (*db-lock*)
      (update-record-from-slots debit slots)
      (update-record-from-slots credit slots)))
  entry)

#+nil
(defmacro def-t-account-getter (name account idx)
  `(defun ,name (,idx)
     (with-mutex (*db-lock*)
       (select ',account
               :flatp t
               :refresh t
               :order-by [entry_idx]
               :where
               (sql-= (sql-slot-value ',account ',idx) ,idx)))))

#+nil(def-t-account-getter get-assets asset user-idx)
#+nil(def-t-account-getter get-expenses expense user-idx)
#+nil(def-t-account-getter get-revenue revenue user-idx)
#+nil(def-t-account-getter get-equity equity user-idx)

;;#+nil
(defun get-assets (user-idx &optional description)
  (with-mutex (*db-lock*)
    (if description
        (select 'asset
                :flatp t
                :refresh t
                :order-by [entry_idx]
                :where
                [and
                [= [slot-value 'asset 'user-idx] user-idx]
                [uplike [slot-value 'asset 'description] description]
                ])
        (select 'asset
                :flatp t
                :refresh t
                :order-by [entry_idx]
                :where
                [= [slot-value 'asset 'user-idx] user-idx]))))

;;#+nil
(defun get-expenses (user-idx)
  (with-mutex (*db-lock*)
    (select 'expense
            :flatp t
            :refresh t
            :order-by [entry_idx]
            :where
            [= [slot-value 'expense 'user-idx] user-idx])))

;;#+nil
(defun get-revenue (user-idx)
  (with-mutex (*db-lock*)
    (select 'revenue
            :flatp t
            :refresh t
            :order-by [entry_idx]
            :where
            [= [slot-value 'revenue 'user-idx] user-idx])))

;;#+nil
(defun get-equity (user-idx)
  (with-mutex (*db-lock*)
    (select 'equity
            :flatp t
            :refresh t
            :order-by [entry_idx]
            :where
            [= [slot-value 'equity 'user-idx] user-idx])))

(defun get-t-account-for-month (account user-idx month year)
  ;;(format t "Getting t-account for: ~A~%" account)
  (let ((start-date (make-date :year year :month month))
        (end-date (make-date :year year :month (1+ month)))
        (rows (ecase account
                (asset (get-assets user-idx))
                (expense (get-expenses user-idx))
                (revenue (get-revenue user-idx))
                (equity (get-equity user-idx)))))
    (return-from get-t-account-for-month
      (remove-if (lambda (row)
                   (or (date< (.date row) start-date)
                       (date> (.date row) end-date))) rows))))

;;; Under development.

#+nil(def-t-account-getter get-asset-entry asset entry-idx)
#+nil(def-t-account-getter get-expense-entry expense entry-idx)
#+nil(def-t-account-getter get-revenue-entry revenue entry-idx)
#+nil(def-t-account-getter get-equity-entry equity entry-idx)

;;#+nil
(defun get-asset-entry (entry-idx)
  (with-mutex (*db-lock*)
    (select 'asset
            :flatp t
            :refresh t
            :order-by [entry_idx]
            :where
            [= [slot-value 'asset 'entry-idx] entry-idx])))

;;#+nil
(defun get-expense-entry (entry-idx)
  (with-mutex (*db-lock*)
    (select 'expense
            :flatp t
            :refresh t
            :order-by [entry_idx]
            :where
            [= [slot-value 'expense 'entry-idx] entry-idx])))

;;#+nil
(defun get-revenue-entry (entry-idx)
  (with-mutex (*db-lock*)
    (select 'revenue
            :flatp t
            :refresh t
            :order-by [entry_idx]
            :where
            [= [slot-value 'revenue 'entry-idx] entry-idx])))

;;#+nil
(defun get-equity-entry (entry-idx)
  (with-mutex (*db-lock*)
    (select 'equity
            :flatp t
            :refresh t
            :order-by [entry_idx]
            :where
            [= [slot-value 'equity 'entry-idx] entry-idx])))

