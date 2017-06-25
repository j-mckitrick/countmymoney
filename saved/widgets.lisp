(defclass widget ()
  ((id :accessor .id :initarg :id :initform "")
   (name :accessor .name :initarg :name :initform :widget)
   (element :accessor .element :initarg :element :initform :div)
   (content :accessor .content :initarg :content :initform "")
   (resource :accessor .resource :initarg :resource :initform "")))

(defparameter *widget-1* (make-instance 'widget
                                        :content "Count My Money"
                                        :name :hello
                                        :id "div-hello"))

(defparameter *widget-2* (make-instance 'widget
                                        :content "Hello, world!"
                                        :name :hello2
                                        :id "span-hello"
                                        :element :span))

(defparameter *widgets* (list *widget-1* *widget-2*))

(defmethod to-html ((widget widget))
  (let (html)
    (case (.element widget)
      (:div (setf html (format nil "<div id=\"~A\">~A</div>"
                               (.id widget)
                               (.content widget))))
      (:span (setf html (format nil "<span id=\"~A\">~A</span>"
                                (.id widget)
                                (.content widget)))))
    html))

(defmethod to-template ((widget widget))
  (list (.name widget) (to-html widget)))

(defun render-widgets (widgets)
  (loop
     for widget in widgets
     nconc (to-template widget)))

(defun render-page-with-widgets (filename widgets)
  (let ((html-template:*string-modifier* #'identity))
    (with-output-to-string (html-template:*default-template-output*)
      (html-template:fill-and-print-template (pathname filename)
                                             (render-widgets widgets)
                                             #+nil(list :hello "Count My Money")
                                             ))))

