;; -*- mode: lisp; -*-

(defpackage #:countmymoney-system (:use :cl :asdf))
(in-package #:countmymoney-system)

(defsystem countmymoney
  :name "countmymoney"
  :version "1.0"
  :author "Jonathon McKitrick"
  :description "Count My Money web app"
  :components
  ((:file "start" :depends-on ("com.countmymoney.serve"))
   (:module "com.countmymoney.serve" :depends-on ("com.countmymoney.app") :serial t
			:components ((:file "defpackage")
						 (:file "macros-1.0")
						 (:file "util")
						 (:file "conf")
						 (:file "authentication")
						 (:file "handlers-1.0")))
   (:module "com.countmymoney.app" :depends-on ("com.countmymoney.db") :serial t
			:components ((:file "defpackage")
						 (:file "transactions")
						 (:file "reports")))
   (:module "com.countmymoney.db" :serial t
			:components ((:file "defpackage")
						 (:file "classes")
						 (:file "sql"))))
  :depends-on (:hunchentoot :net-telent-date :split-sequence :html-template :cl-who
							:clsql :cl-json :rfc2388 :xml-emitter))

