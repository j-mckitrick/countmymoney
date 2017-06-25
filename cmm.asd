;;; -*- mode:lisp; -*-

(defpackage #:countmymoney-system (:use :cl :asdf))
(in-package #:countmymoney-system)

(defsystem cmm
  :name "countmymoney"
  :version "2.0"
  :author "Jonathon McKitrick"
  :description "Count My Money web app"
  :components
  ((:file "start" :depends-on ("com.countmymoney.serve"))
   (:module "com.countmymoney.serve" :depends-on ("com.countmymoney.app") :serial t
			:components ((:file "defpackage")
						 (:file "cmm-conversion")
						 (:file "cmm-conf")
						 (:file "cmm-auth")
						 (:file "cmm-handlers")))
   (:module "com.countmymoney.app"
    :depends-on ("com.countmymoney.db" "com.countmymoney.util") :serial t
			:components ((:file "defpackage")
						 (:file "transactions")
						 (:file "reports")))
   (:module "com.countmymoney.db" :serial t
			:components ((:file "defpackage")
						 (:file "classes")
						 (:file "sql")))
   (:module "com.countmymoney.util" :serial t
			:components ((:file "defpackage")
						 (:file "utilities"))))
  :depends-on (:hunchentoot :net-telent-date :split-sequence :html-template :cl-who
							:clsql :cl-json :rfc2388 :xml-emitter))

