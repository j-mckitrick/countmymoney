(in-package #:cl-user)

;(push :db-debug *features*)

(defpackage #:com.countmymoney.db
  (:nicknames #:db)
  (:documentation "Database interface functions.")
  (:use #:cl #:cl-user #:sb-thread #:clsql #:split-sequence)
  (:export

   ;; classes
   #:cmm-user
   #:entry
   #:asset
   #:expense
   #:equity
   #:liability
   #:equity
   #:revenue
   
   ;; accessors
   #:.idx
   #:.username
   #:.password
   #:.name
   #:.date
   #:.user
   #:.category
   #:.amount
   #:.description
   #:.entry-idx

   ;; functions
   #:connect-to-db
   #:disconnect-from-db

   #:get-cmm-user
   #:get-idx-for-cmm-user
   #:get-entries
   #:get-entries-for-month
   #:add-entry
   #:save-entry-description
   #:add-t-account-entries
   #:get-t-account-for-month
   #:get-one-entry
   #:delete-entry
   #:update-entry

   #:total-account
   
   #:get-asset-entry
   #:get-revenue-entry
   #:get-equity-entry
   #:get-expense-entry

   #:find-client
   #:add-client
   #:get-clients
   
   ;; variables
   ))
