;;; -*- mode:lisp; -*-

(in-package #:com.countmymoney.serve)

;;(declaim (optimize debug))
(declaim (optimize speed))

(defparameter *user-cookie* (concatenate 'string *app-prefix* "-" "user"))
(defparameter *auth-cookie* (concatenate 'string *app-prefix* "-" "auth"))
(defparameter *flag-cookie* (concatenate 'string *app-prefix* "-" "flag"))

(defun try-persisted-login ()
  "Check for flags from a persisted login."
  (format t "Session: ~A ~A Cookie: ~A~%"
          (session-value 'user-idx)
          (session-value 'username)
          (cookie-in *flag-cookie*))
  (unless (and (session-value 'user-idx)
               (session-value 'username))
    (when (string= (cookie-in *flag-cookie*) "t")
      (format t "Found flag.~%")
      (let* ((persisted-user (cookie-in *user-cookie*))
             (persisted-auth (cookie-in *auth-cookie*))
             (generated-auth (make-digest persisted-user *app-prefix*)))
        (declare (type simple-string persisted-user persisted-auth generated-auth))
        (progn
          (format t "Found persisted user: ~A~%" persisted-user)
          (format t "Compare digests:~%~A~%~A~%" persisted-auth generated-auth))
        (when (string= persisted-auth generated-auth)
          (login (get-cmm-user persisted-user)))))))

(defun try-login (username password persist)
  "Attempt to login USERNAME with PASSWORD and optional PERSIST setting."
  (declare (type string username password persist))
  (let* ((user (get-cmm-user username))
         (result
          (cond
            ((= (length username) 0)
             '((:fail . "Missing username")))
            ((= (length password) 0)
             '((:fail . "Missing password")))
            ((or (null user)
                 (string-not-equal password (.password user)))
             '((:fail . "Incorrect login")))
            (t
             (login user persist)
             (get-session-info-alist)))))
    result))

(defun login (user &optional (persist ""))
  "Set up USER after a successful login."
  (declare (type cmm-user user)
           (type string persist))
  (when (and (boundp '*request*) user)
    (format t "Logging in user: ~A~%" (.username user))
    (when (string= persist "on")
      (persist-login user))
    (setup-session user)))

(defun persist-login (user)
  "Set cookies for USER to be recognized without login."
  (declare (type cmm-user user))
  (let ((username (.username user)))
    (declare (type string username))
    (multiple-value-bind (dig exp) (make-digest username *app-prefix*)
      (declare (type string dig) (type integer exp))
      (format t "Username: ~A ~A ~A~%" username exp dig)
      (set-cookie *user-cookie* :value username :expires exp)
      (set-cookie *auth-cookie* :value dig :expires exp)
      (set-cookie *flag-cookie* :value "t" :expires exp))))

(defun make-digest (username &optional (seed "seed"))
  (declare (type string username seed))
  (values (hunchentoot::md5-hex (format nil "~A:~A:2weeks" username seed))
          (+ (get-universal-time) (* 2 7 24 60 60))))

(defun logout ()
  (clear-session)
  (clear-cookies))

;;; app-specific session setup

(defparameter *month-cookie* (concatenate 'string *app-prefix* "month"))
(defparameter *year-cookie* (concatenate 'string *app-prefix* "year"))

(defun setup-session (user)
  (declare (type cmm-user user))
  (format t "Setup session for: ~A (~A)~%" (.username user) (.idx user))
  (when user
    (let ((month-cookie (cookie-in *month-cookie*)))
      (start-session)
      (setf (session-value 'user-idx) (.idx user)
            (session-value 'username) (.username user)
            (session-value 'month) (or (and month-cookie
                                            (string-not-equal month-cookie "")
                                            (parse-integer month-cookie))
                                       (session-value 'month)
                                       1)
            (session-value 'year) (or (session-value 'year) 2010)))
    (format t "Month will be: ~A~%" (session-value 'month))))

(defun get-session-info-alist ()
  "Get some session info as an ALIST."
  (if (boundp '*request*)
      (when (session-value 'user-idx)
        (format t "Month from user: ~A~%"
                (assoc (the fixnum (session-value 'month)) *months*))
        (list (cons :user (session-value 'username))
              (cons :month (cdr (assoc (the fixnum (session-value 'month)) *months*)))
              (cons :year (session-value 'year))))
      '((:user "foo") (:month "December") (:year 2009))))

(defun clear-session ()
  (delete-session-value 'user-idx)
  (delete-session-value 'username)
  (delete-session-value 'year)
  (delete-session-value 'month))

(defun clear-cookies ()
  (set-cookie *flag-cookie*)
  (set-cookie *user-cookie*)
  (set-cookie *auth-cookie*)
  (set-cookie *month-cookie*))
