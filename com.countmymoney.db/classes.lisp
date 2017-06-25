;;; -*- mode:lisp; -*-

(in-package #:com.countmymoney.db)

;;(declaim (optimize debug))
(declaim (optimize speed))

(clsql:def-view-class cmm-user ()
  ((idx
    :db-kind :key
    :db-constraints :not-null
    :accessor .idx
    :type integer
    :initarg :idx)
   (username
    :accessor .username
    :type varchar
    :initarg :username)
   (password
    :accessor .password
    :type varchar
    :initarg :password))
  (:documentation "An application user."))

(clsql:def-view-class entry ()
  ((idx
    :db-kind :key
    :db-constraints :auto-increment
    :accessor .idx
    :type integer
    :initarg :idx)
   (user-idx
    :accessor .user-idx
    :type integer
    :initarg :user-idx)
   (user
    :accessor .user
    :db-kind :join
    :db-info (:join-class cmm-user
                          :home-key user-idx
                          :foreign-key idx
                          :set nil))
   (date
    :accessor .date
    :type date
    :initarg :date)
   (category
    :accessor .category
    :type (varchar 40)
    :initarg :category)
   (description
    :accessor .description
    :type (varchar 80)
    :initarg :description)
   (amount
    :accessor .amount
    :type float
    :initarg :amount))
  (:documentation "General ledger entry."))

;;; Each of these other accounts are very similar,
;;; and should be derived from a base class.
;;; Find out how to do this for db-backed classes.

(clsql:def-view-class asset ()
  ((idx
    :db-kind :key
    :db-constraints :not-null
    :accessor .idx
    :type integer
    :initarg :idx)
   (user-idx
    :accessor .user-idx
    :type integer
    :initarg :user-idx)
   (user
    :accessor .user
    :db-kind :join
    :db-info (:join-class cmm-user
                          :home-key user-idx
                          :foreign-key idx
                          :set nil))
   (entry-idx
    :accessor .entry-idx
    :type integer
    :initarg :entry-idx)
   (entry
    :accessor .entry
    :db-kind :join
    :db-info (:join-class entry
                          :home-key entry-idx
                          :foreign-key idx
                          :set nil))
   (date
    :accessor .date
    :type date
    :initarg :date)
   (category
    :accessor .category
    :type keyword
    :initarg :category)
   (description
    :accessor .description
    :type (varchar 80)
    :initarg :description)
   (amount
    :accessor .amount
    :type float
    :initarg :amount))
  (:documentation "Asset t-account entry."))

(defmethod initialize-instance :after ((o asset) &key)
  "Make credit entry negative."
  (when (and (slot-boundp o 'category)
             (eql (.category o) :credit))
    (setf (.amount o) (- (.amount o)))))

(clsql:def-view-class expense ()
  ((idx
    :db-kind :key
    :db-constraints :not-null
    :accessor .idx
    :type integer
    :initarg :idx)
   (user-idx
    :accessor .user-idx
    :type integer
    :initarg :user-idx)
   (user
    :accessor .user
    :db-kind :join
    :db-info (:join-class cmm-user
                          :home-key user-idx
                          :foreign-key idx
                          :set nil))
   (entry-idx
    :accessor .entry-idx
    :type integer
    :initarg :entry-idx)
   (entry
    :accessor .entry
    :db-kind :join
    :db-info (:join-class entry
                          :home-key entry-idx
                          :foreign-key idx
                          :set nil))
   (date
    :accessor .date
    :type date
    :initarg :date)
   (category
    :accessor .category
    :type keyword
    :initarg :category)
   (description
    :accessor .description
    :type (varchar 80)
    :initarg :description)
   (amount
    :accessor .amount
    :type float
    :initarg :amount))
  (:documentation "Expense t-account entry."))

(defmethod initialize-instance :after ((o expense) &key)
  "Make credit entry negative."
  (when (and (slot-boundp o 'category)
             (eql (.category o) :credit))
    (setf (.amount o) (- (.amount o)))))

(clsql:def-view-class equity ()
  ((idx
    :db-kind :key
    :db-constraints :not-null
    :accessor .idx
    :type integer
    :initarg :idx)
   (user-idx
    :accessor .user-idx
    :type integer
    :initarg :user-idx)
   (user
    :accessor .user
    :db-kind :join
    :db-info (:join-class cmm-user
                          :home-key user-idx
                          :foreign-key idx
                          :set nil))
   (entry-idx
    :accessor .entry-idx
    :type integer
    :initarg :entry-idx)
   (entry
    :accessor .entry
    :db-kind :join
    :db-info (:join-class entry
                          :home-key entry-idx
                          :foreign-key idx
                          :set nil))
   (date
    :accessor .date
    :type date
    :initarg :date)
   (category
    :accessor .category
    :type keyword
    :initarg :category)
   (description
    :accessor .description
    :type (varchar 80)
    :initarg :description)
   (amount
    :accessor .amount
    :type float
    :initarg :amount))
  (:documentation "Owner's Equity t-account entry."))

(defmethod initialize-instance :after ((o equity) &key)
  "Make credit entry negative."
  (when (and (slot-boundp o 'category)
             (eql (.category o) :credit))
    (setf (.amount o) (- (.amount o)))))

(clsql:def-view-class liability ()
  ((idx
    :db-kind :key
    :db-constraints :not-null
    :accessor .idx
    :type integer
    :initarg :idx)
   (user-idx
    :accessor .user-idx
    :type integer
    :initarg :user-idx)
   (user
    :accessor .user
    :db-kind :join
    :db-info (:join-class cmm-user
                          :home-key user-idx
                          :foreign-key idx
                          :set nil))
   (entry-idx
    :accessor .entry-idx
    :type integer
    :initarg :entry-idx)
   (entry
    :accessor .entry
    :db-kind :join
    :db-info (:join-class entry
                          :home-key entry-idx
                          :foreign-key idx
                          :set nil))
   (date
    :accessor .date
    :type date
    :initarg :date)
   (category
    :accessor .category
    :type keyword
    :initarg :category)
   (description
    :accessor .description
    :type (varchar 80)
    :initarg :description)
   (amount
    :accessor .amount
    :type float
    :initarg :amount))
  (:documentation "Liabilities t-account entry."))

(defmethod initialize-instance :after ((o liability) &key)
  "Make debit entry negative."
  (when (and (slot-boundp o 'category)
             (eql (.category o) :debit))
    (setf (.amount o) (- (.amount o)))))

(clsql:def-view-class revenue ()
  ((idx
    :db-kind :key
    :db-constraints :not-null
    :accessor .idx
    :type integer
    :initarg :idx)
   (user-idx
    :accessor .user-idx
    :type integer
    :initarg :user-idx)
   (user
    :accessor .user
    :db-kind :join
    :db-info (:join-class cmm-user
                          :home-key user-idx
                          :foreign-key idx
                          :set nil))
   (entry-idx
    :accessor .entry-idx
    :type integer
    :initarg :entry-idx)
   (entry
    :accessor .entry
    :db-kind :join
    :db-info (:join-class entry
                          :home-key entry-idx
                          :foreign-key idx
                          :set nil))
   (date
    :accessor .date
    :type date
    :initarg :date)
   (category
    :accessor .category
    :type keyword
    :initarg :category)
   (description
    :accessor .description
    :type (varchar 80)
    :initarg :description)
   (amount
    :accessor .amount
    :type float
    :initarg :amount))
  (:documentation "Revenue t-account entry."))

(defmethod initialize-instance :after ((o revenue) &key)
  "Make debit entry negative."
  (when (and (slot-boundp o 'category)
             (eql (.category o) :debit))
    (setf (.amount o) (- (.amount o)))))

(clsql:def-view-class cmm-client ()
  ((idx
    :db-kind :key
    :db-constraints :not-null
    :accessor .idx
    :type integer
    :initarg :idx)
   (name
    :accessor .name
    :type varchar
    :initarg :name))
  (:documentation "Clients for accounts receivable."))
