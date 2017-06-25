(in-package #:cl-user)

;(push :server-debug *features*)

(defpackage #:com.countmymoney.serve
  (:nicknames #:serve)
  (:documentation "Hunchentoot functionality for mark my words web app.")
  (:use #:cl #:cl-user #:hunchentoot #:xml-emitter #:split-sequence #:json
        #:cl-ppcre #:cl-ppcre
        #:com.countmymoney.app #:com.countmymoney.db #:com.countmymoney.util
        ;;#:html-template
        #:cl-who
        )
  (:export
   ;; functions
   #:web-start
   #:web-stop

   ;; variables

   ;; constants

   ))
