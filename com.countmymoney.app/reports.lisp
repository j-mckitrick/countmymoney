;;; -*- mode:lisp; -*-

(in-package #:com.countmymoney.app)

;;(declaim (optimize debug))
(declaim (optimize speed))

(defclass income-statement ()
  ((revenue :accessor revenue :initarg :revenue :initform 0.0)
   (expenses :accessor expenses :initarg :expenses :initform 0.0)
   (net-income :accessor net :initarg :net :initform 0.0)))

(defmethod initialize-instance :after ((p income-statement) &key)
  (setf (net p) (- (the single-float (revenue p))
                   (the single-float (expenses p)))))

(defparameter *income-statement* nil)

(defun get-income-statement (user &optional month year)
  (declare (ignorable month year))
  (unless *income-statement*
    (let* ((revenue
             (total-account user 'revenue :category :credit :month month :year year))
           (expenses
             (total-account user 'expense :category :debit :month month :year year))
           (report
             (make-instance 'income-statement :revenue revenue :expenses expenses)))
      (setf *income-statement* report)))
  *income-statement*)

(defclass capital-report ()
  ((begin :accessor begin :initarg :begin :initform 0.0)
   (contrib :accessor contrib :initarg :contrib :initform 0.0)
   (net-income :accessor net-income :initarg :net-income :initform 0.0)
   (withdrawals :accessor withdrawals :initarg :withdrawals :initform 0.0)
   (change :accessor change :initform 0.0)
   (end :accessor end :initform 0.0)))

(defmethod initialize-instance :after ((p capital-report) &key)
  (setf (change p) (- (+ (the single-float (net-income p))
                         (the single-float (contrib p)))
                      (the single-float (withdrawals p)))
		(end p) (+ (the single-float (begin p))
                   (the single-float (change p)))))

(defparameter *capital-report* nil)

(defun get-capital-report (user &optional month year)
  (unless *capital-report*
    (let* ((p&l (get-income-statement user month year))
           (draw (total-account user 'equity :category :debit
                                             :month month
                                             :year year))
           (equity (total-account user 'equity :category :credit
                                               :month month
                                               :year year))
           (contrib (if (< equity 0.0) (- equity) 0.0))
           (report (make-instance 'capital-report
                                  :net-income (net p&l)
                                  :contrib contrib
                                  :withdrawals draw)))
      (setf *capital-report* report)))
  *capital-report*)

(defclass balance-sheet ()
  ((assets :accessor assets :initarg :assets :initform 0.0)
   (asset-list :accessor asset-list :initarg :asset-list :initform ())
   (liabilities :accessor liabilities :initarg :liabilities :initform 0.0)
   (capital :accessor capital :initarg :capital :initform 0.0)
   (l&e :accessor l&e :initarg :l&e :initform 0.0)))

(defmethod initialize-instance :after ((p balance-sheet) &key)
  (setf (l&e p) (+ (the single-float (liabilities p))
                   (the single-float (capital p)))))

(defparameter *balance-sheet* nil)

(defun get-balance-sheet (user &optional month year)
  (declare (ignorable month user))
  (unless *balance-sheet*
    (let* ((asset-debit
             (total-account user 'asset :category :debit :month month :year year))
           (asset-credit
             (total-account user 'asset :category :credit :month month :year year))
           (asset-list
             (mapcar
              (lambda (a)
                (cons a (total-account user 'asset :desc a :month month :year year)))
              +asset-types+))
           (liabilities
             (total-account user 'liability :category :credit :month month :year year))
           (capital (end (get-capital-report user month year)))
           (report (make-instance 'balance-sheet
                                  :assets (+ (the single-float asset-debit)
                                             (the single-float asset-credit))
                                  :asset-list asset-list
                                  :liabilities liabilities
                                  :capital capital)))
      (setf *balance-sheet* report)))
  *balance-sheet*)

(defclass cashflow-report ()
  ((begin :accessor begin :initarg :begin :initform 0.0)
   (cash-in :accessor cash-in :initarg :cash-in :initform 0.0)
   (cash-out :accessor cash-out :initarg :cash-out :initform 0.0)
   (change :accessor change :initform 0.0)
   (end :accessor end :initform 0.0)))

(defmethod initialize-instance :after ((p cashflow-report) &key)
  (setf (change p) (- (the single-float (cash-in p))
                      (the single-float (cash-out p)))
		(end p) (+ (the single-float (begin p))
                   (the single-float (change p)))))

(defparameter *cashflow-report* nil)

(defun get-cashflow-report (user &optional month year)
  (declare (ignorable user month))
  (unless *cashflow-report*
    (let* ((cash-in (total-account user 'asset :category :debit
                                               :desc "Cash"
                                               :month month
                                               :year year))
           (cash-out (total-account user 'asset :category :credit
                                                :desc "Cash"
                                                :month month
                                                :year year))
           (report (make-instance 'cashflow-report
                                  :cash-in cash-in
                                  :cash-out (if (minusp cash-out)
                                                (- cash-out) cash-out))))
      (setf *cashflow-report* report)))
  *cashflow-report*)

(defun reset-reports ()
  "Clear memoized reports."
  (setf *income-statement* nil
        *capital-report* nil
        *balance-sheet* nil
        *cashflow-report* nil))
