(in-package #:tpd2.ml)

(defmacro output-object-to-ml (object)
  `(output-raw-ml (object-to-ml ,object)))

(defgeneric object-to-ml (object))

(defmethod object-to-ml ((list list))
  (with-ml-output
    (loop for i in list do (output-object-to-ml i))))

(defmethod object-to-ml ((f function))
  (object-to-ml (funcall f)))

(defmethod object-to-ml (object)
  (with-ml-output object))

(defmethod object-to-ml ((sendbuf sendbuf))
  (output-raw-ml (sendbuf-to-byte-vector sendbuf)))
