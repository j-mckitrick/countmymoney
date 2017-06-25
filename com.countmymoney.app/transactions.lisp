;;; -*- mode:lisp; -*-

(in-package #:com.countmymoney.app)

;;(declaim (optimize debug))
(declaim (optimize speed))

(defparameter +asset-types+ (list "Accounts Receivable" "Cash" "Equipment"))

(defparameter *categories*
  (list :invoicebilled :invoicepaid :expense :purchase :ownerdraw :capitalcontribution))

(defparameter *category-strings*
  (list "Invoice Billed" "Invoice Paid" "Expense" "Equipment Purchase" "Owner Draw"
        "Capital Contribution"))

(defparameter *category->string*
  (mapcar #'cons *categories* *category-strings*))

(defparameter *years*
  (loop for i from 2010 to 2012
     collect i))

(defparameter *month-strings*
  (loop for i from 1 to 12
     collect (cons (format nil "~A" i) (clsql-sys:month-name i))))

(defparameter *months*
  (loop for i from 1 to 12
     collect (cons i (clsql-sys:month-name i))))

(defun add-transaction (user-idx category description amount month day year)
  (reset-reports)
  (add-child-transactions
   (add-entry user-idx category description amount month day year)))

(defun update-transaction (entry-idx category description amount)
  (reset-reports)
  (add-child-transactions
   (update-entry (get-one-entry entry-idx) category description amount)))

(defun add-child-transactions (entry)
  (ecase (intern (.category entry) :keyword)
    (:invoicebilled (record-sale entry))
    (:invoicepaid (record-receive-payment entry))
    (:expense (record-expense entry))
    (:purchase (record-purchase entry))
    (:ownerdraw (record-owner-draw entry))
    (:capitalcontribution (record-capital-contribution entry))))

(defmacro def-record-transaction (fn-name debit-account debit-name credit-account
                                  credit-name
                                  &key setup-client-p)
  "Record both sides of a general ledger entry.
FN-NAME will be the name of the function defined.
Add entries to DEBIT-ACCOUNT called DEBIT-NAME.
Add entries to CREDIT-ACCOUNT called CREDIT-NAME.
Transactions involving accounts receivable can set SETUP-CLIENT-P
if there could be a client name in the description."
  `(defun ,(symb 'record- fn-name) (entry)
     ,(when setup-client-p
            `(setup-entry-client entry))
     (add-t-account-entries
      entry ',debit-account ,debit-name ',credit-account ,credit-name)))

;;; Transaction types:
;;; Sales:
;;; - debit cash, credit revenue (cash sale)
;;; - debit accounts receivable, credit revenue (credit sale)
;;; Purchases:
;;; - debit expenses/inventory, credit cash (cash)
;;; - debit expenses/inventory, credit accounts payable (credit)
;;; Make payments:
;;; - debit accounts payable, credit cash
;;; Receive payments:
;;; - debit cash, credit accounts receivable
;;; Borrow money:
;;; - debit cash/equipment, credit note payable
;;; Repay loan:
;;; - debit note payable, credit cash
;;; Owner Draw:
;;; - debit draw, credit cash
;;; Capital Contribution:
;;; - debit cash, credit ???
;;; Payroll:
;;; - debit salary expense, credit cash

(def-record-transaction sale asset "Accounts Receivable" revenue "Sales"
  :setup-client-p t)
(def-record-transaction receive-payment asset "Cash" asset "Accounts Receivable"
  :setup-client-p t)
(def-record-transaction expense expense "Expenses" asset "Cash")
(def-record-transaction purchase asset "Equipment" asset "Cash")
(def-record-transaction owner-draw equity "Draw" asset "Cash")
(def-record-transaction capital-contribution asset "Cash" equity "Contribution")

(defparameter +client-regex+ "(?i)^client:(.*)$")

(defun setup-entry-client (entry)
  "Handle a client name in the description slot of ENTRY.
Remove name from the field and add new clients to client table."
  ;;(format t "Find ~A in ~A.~%" +client-regex+ (.description entry))
  (register-groups-bind (client)
      ((the string +client-regex+)
       (the string (.description entry)))
    (format t "Found client: ~A~%" client)
    ;; 'client:' marker needs to be stripped from DESCRIPTION slot.
    (setf (.description entry) client)
    (save-entry-description entry)
    ;;(format t "Saved entry description with client.~%")
    (unless (find-client client)
      (format t "Adding new client: ~A~%" client)
      (add-client client))))

(defun delete-transaction (entry-idx)
  "Delete a transaction and its children, and reset report data."
  (delete-entry (get-one-entry entry-idx))
  (delete-child-transactions entry-idx)
  (reset-reports))

(defun delete-child-transactions (entry-idx)
  "Delete child transactions for an entry."
  (dolist (fn '(get-asset-entry
                get-revenue-entry
                get-equity-entry
                get-expense-entry))
    (dolist (child-entry (funcall fn entry-idx))
      (delete-entry child-entry))))

(defun populate-entries ()
  (flet ((make-amount (ceiling)
           (float (random ceiling))))
    (dotimes (month 3)
      (dotimes (day 5)
        (dolist (client (mapcar #'.name (get-clients)))
          (let ((description (format nil "client:~A" client)))
            (add-transaction 1 :invoicebilled description (make-amount 1000)
                             (1+ month)
                             (1+ day) 2010)
            (add-transaction 1 :invoicepaid description (make-amount 1000)
                             (1+ month)
                             (1+ day) 2010))
          (add-transaction 1 :expense "My expense" (make-amount 100)
                           (1+ month)
                           (1+ day) 2010)
          (add-transaction 1 :purchase "My equipment" (make-amount 500)
                           (1+ month)
                           (1+ day) 2010)
          (add-transaction 1 :ownerdraw "My draw" (make-amount 100)
                           (1+ month)
                           (1+ day) 2010)
          (add-transaction 1 :capitalcontribution "My deposit" (make-amount 100)
                           (1+ month)
                           (1+ day) 2010)
          #+ignore(sleep 1))))))
