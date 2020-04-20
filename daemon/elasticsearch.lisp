(uiop:define-package :ballish/daemon/elasticsearch
    (:use :cl)
  (:import-from :drakma #:http-request)
  (:import-from :quri #:uri #:merge-uris #:render-uri #:url-encode)
  (:import-from :drakma #:http-request)
  (:import-from :usocket #:connection-refused-error)
  (:import-from :cl-json #:encode-json-alist-to-string #:encode-json #:decode-json-from-string)
  (:export #:with-elasticsearch
	   #:index-code))

(in-package :ballish/daemon/elasticsearch)

(defvar *es-uri* (uri "http://localhost:9200"))

(push (cons "application" "json") drakma:*text-content-types*)
(setf drakma:*drakma-default-external-format* :utf8)

(defmacro with-elasticsearch (var &body body)
  (declare (ignore var))
  (let ((process (gensym))
	(timeout-counter (gensym))
	(es-url (gensym))
	(kill-counter (gensym)))
    `(let ((,process
	    (uiop:launch-program
	     "~/Downloads/elasticsearch-7.6.2/bin/elasticsearch"
	     :output :interactive))
	   (,timeout-counter 60)
	   (,kill-counter 10))
       (unwind-protect
	    (loop
	       (handler-case
		   (progn
		     (request "/" :get)
		     (setup-mappings)
		     (unwind-protect
			  (progn ,@body)
		       (return)))
		 (connection-refused-error ()
		   (if (= ,timeout-counter 0)
		       (error "Unable to start ElasticSearch")
		       (progn
			 (decf ,timeout-counter)
			 (sleep 1))))))
	 (uiop:terminate-process ,process)
	 (loop
	    (if (uiop:process-alive-p ,process)
		(if (> ,kill-counter 0)
		    (progn
		      (decf ,kill-counter)
		      (sleep 1))
		    (uiop:terminate-process ,process :urgent t))
		(return)))
	 (uiop:wait-process ,process)))))

(defun request (path method &optional alist)
  (decode-json-from-string
   (http-request (render-uri (merge-uris path *es-uri*))
		 :method method
		 :content-type "application/json"
		 :content (and alist (encode-json-alist-to-string alist)))))

(defmethod encode-json ((_ (eql :false)) &optional stream)
  (princ "false" stream)
  nil)

(defun setup-mappings ()
  (request "/source"
	   :put
	   '((mappings .
	      ((properties .
			   ((code . ((type . "text")))
			    (mtime . ((enabled . :false))))))))))

(defun cdr-assoc (item alist)
  (cdr (assoc item alist)))

(defun index-code (path mtime code)
  (let* ((req-path (namestring
		    (merge-pathnames
		     (url-encode (namestring path)) "/source/_doc/")))
	 (doc (request req-path :get)))
    (when (or (not (cdr-assoc :found doc))
	      (> mtime (cdr-assoc :mtime (cdr-assoc :--source doc))))
        (request req-path
		 :post
		 `((code . ,code)
		   (mtime . ,mtime))))))
