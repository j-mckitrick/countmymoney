;;; -*- mode:lisp; -*-

(in-package #:cl-user)

(defun ignore-simple-error (c)
  (invoke-restart 'com.countmymoney.serve::ignore-simple-error c))

(defun ignore-unbound-slot (c)
  (invoke-restart 'com.countmymoney.serve::ignore-unbound-slot c))

(defun ignore-shutdown-condition (c)
  (invoke-restart 'com.countmymoney.serve::ignore-shutdown-condition c))

;;; Entry points

(defun app-stop ()
  "Stop cmm application (db and web server)."
  (handler-case (com.countmymoney.db:disconnect-from-db)
    (simple-error (e) (format t "DISCONNECT error ignored: ~A~%" e)))
  
  (handler-bind ((simple-error #'ignore-simple-error)
				 (unbound-slot #'ignore-unbound-slot)
				 (simple-error #'ignore-shutdown-condition))
    (com.countmymoney.serve:web-stop)))

(defun app-start ()
  "Main system startup."
  (when serve::*my-acceptor*
    (app-stop))
  (handler-case (com.countmymoney.db:connect-to-db)
    (clsql-sys:sql-database-error (e) (format t "CONNECT error ignored: ~A~%" e)))
  (sb-thread:make-thread 'com.countmymoney.serve:web-start))

(app-start)
