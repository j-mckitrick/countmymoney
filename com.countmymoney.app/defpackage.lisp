(in-package #:cl-user)

(defpackage #:com.countmymoney.app
  (:nicknames #:app)
  (:documentation "App functions for Count My Money.")
  (:use #:cl #:cl-user #:com.countmymoney.db #:cl-ppcre #:com.countmymoney.util)
  (:export

   ;; functions
   #:add-transaction
   #:update-transaction
   #:delete-transaction
   #:delete-child-transactions
   #:get-income-statement
   #:get-balance-sheet
   #:get-capital-report
   #:get-cashflow-report
   #:reset-reports

   #:populate-entries
   
   ;; specials
   #:*categories*
   #:*category-strings*
   #:*category->string*
   #:*years*
   #:*months*
   #:*month-strings*
   #:*app-log-enable*

   ;; classes
   #:cashflow-report
   #:capital-report
   #:income-statement

   ;; slots
   #:asset-list
   #:assets
   #:l&e
   #:liabilities
   #:capital
   ))
