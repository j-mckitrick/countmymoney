;;; -*- mode:lisp; -*-

(in-package #:com.countmymoney.serve)

;;(declaim (optimize debug))
(declaim (optimize speed))

#+nil
(defmacro wrap-as-xml-result (&body body)
  `(with-output-to-string (*standard-output*)
     (with-xml-output (*standard-output*)
       ,@body)))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue nil)
     ,@body))

;; Data conversion functions ---------------------------------------------------

(defun entry->alist (entry)
  "Return an ALIST representing ENTRY."
  (declare (type entry entry))
  (let ((date (first (split-sequence:split-sequence
                      #\Space (clsql-sys:format-date nil (.date entry))))))
    (list (cons :idx (.idx entry))
          (cons :amount (format nil "~,2F" (.amount entry)))
          (cons :user (.username (.user entry)))
          (cons :date date)
          (cons :categoryval (string-downcase (.category entry)))
          (cons :category (cdr (assoc (intern (.category entry) :keyword)
                                      app::*category->string*)))
          (cons :description (.description entry)))))

(defun t-account->alist (objects)
  "Return an alist of a subset of slots from t-account OBJECTS."
  (if objects
      (mapcar (lambda (o)
                (list (cons :ref (.entry-idx o))
                      (cons :amount (format nil "~,2F" (abs (.amount o))))
                      (cons :desc (.description o))
                      (cons :category (.category o))))
              objects)
      ""))

(defun balance-sheet->table (balance-sheet)
  "Generate html representation of BALANCE-SHEET.
Other reports are returned as json, but we use html here for 2 reasons:
- Balance sheet report has a list of asset accounts,
which could be tricky to convert from objects to json.
- Wanted to try a direct html report to compare with json format."
  (with-html
    (:table
     (:caption "Balance Sheet")
     (:tr
      (:th :colspan "3" :style "text-align:left" "Assets"))
     (dolist (asset-account (asset-list balance-sheet))
       (htm
		(:tr
		 (:td (str (car asset-account)))
		 (:td :class "Currency" (str (format nil "~,2F" (cdr asset-account))))
		 (:td))))
     (:tr
      (:td "Total Assets")
      (:td)
      (:td :class "Currency" (str (format nil "~,2F" (assets balance-sheet)))))
     (:tr
      (:th :colspan "3" :style "text-align:left" "Liabilities"))
     (:tr
      (:td "A/P")
      (:td :class "Currency" "0.00")
      (:td))
     (:tr
      (:td "Total Liabilities")
      (:td)
      (:td :class "Currency" (str (format nil "~,2F" (liabilities balance-sheet)))))
     (:tr
      (:th :colspan "3" :style "text-align:left" "Owner's Equity"))
     (:tr
      (:td "Capital")
      (:td)
      (:td :class "Currency" (str (format nil "~,2F" (capital balance-sheet)))))
     (:tr
      (:td "Total Liabilities and Equity")
      (:td)
      (:td :class "Currency" (str (format nil "~,2F" (l&e balance-sheet))))))))
