(uiop:define-package :ballish/daemon/elasticsearch
    (:use :cl)
  (:import-from :drakma #:http-request)
  (:import-from :quri #:uri)
  (:import-from :usocket #:connection-refused-error)
  (:export #:with-elasticsearch))

(in-package :ballish/daemon/elasticsearch)

(defmacro with-elasticsearch ((uri) &body body)
  (let ((process (gensym))
	(timeout-counter (gensym))
	(es-url (gensym))
	(kill-counter (gensym)))
    `(let ((,process
	    (uiop:launch-program
	     "~/Downloads/elasticsearch-7.6.2/bin/elasticsearch"
	     :output :interactive))
	   (,timeout-counter 60)
	   (,es-url "http://localhost:9200")
	   (,kill-counter 10))
       (unwind-protect
	    (loop
	       (handler-case
		   (progn
		     (http-request ,es-url)
		     (unwind-protect
			  (let ((,uri (quri:uri ,es-url)))
			    ,@body)
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
