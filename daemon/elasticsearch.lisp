(uiop:define-package :ballish/daemon/elasticsearch
    (:use :cl)
  (:import-from :drakma #:http-request)
  (:import-from :quri #:uri)
  (:import-from :usocket #:connection-refused-error)
  (:import-from :named-readtables #:in-readtable)
  (:import-from :cl-elastic #:hashtable-syntax #:<client> #:*enable-keywords* #:send-request)
  (:export #:with-elasticsearch
	   #:export-code))

(in-package :ballish/daemon/elasticsearch)

(in-readtable hashtable-syntax)

(defmacro with-elasticsearch ((client) &body body)
  (let ((process (gensym))
	(timeout-counter (gensym))
	(es-url (gensym))
	(kill-counter (gensym)))
    `(let ((,process
	    (uiop:launch-program
	     "~/Downloads/elasticsearch-7.6.2/bin/elasticsearch"
	     :output :interactive))
	   (,timeout-counter 60)
	   (,kill-counter 10)
	   (,client (make-instance '<client> :endpoint "http://localhost:9200")))
       (unwind-protect
	    (loop
	       (handler-case
		   (progn
		     (send-request ,client :dummy)
		     (setup-mappings ,client)
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

(defun setup-mappings (client)
  (send-request client :source :method :put
		:data
		#{:mappings
		 #{:properties
		  #{:code #{:type "text"}
		    :mtime #{:type "integer" :enabled nil}}}}))

(defun index-code (client path mtime code))
