;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Queues (FIFO)

;;;; The class QUEUE represents a simple, circular buffer based, FIFO
;;;; implementation. The two core operations are enqueue and dequeue,
;;;; the utility method queue-count is also provided.

(defclass queue ()
  ((head-index :accessor head-index)
   (tail-index :accessor tail-index)
   (buffer :accessor buffer)))

(defmethod initialize-instance :after
    ((queue queue)
     &key
     (size 20)
     (element-type t)
     &allow-other-keys)
  (assert (< 1 size)
          (size)
          "Initial size of a queue must be greater than 1.")
  (setf (head-index queue) 0
        (tail-index queue) 0
        (buffer queue) (make-array (1+ size) :element-type element-type)))

(defmethod enqueue ((queue queue) value)
  (when (queue-full-p queue)
    (grow-queue queue))
  (setf (aref (buffer queue) (head-index queue)) value)
  (move-head queue)
  queue)

(defmethod dequeue ((queue queue) &optional (default-value nil))
  (if (queue-empty-p queue)
      default-value
      (prog1
          (aref (buffer queue) (tail-index queue))
        (move-tail queue))))

(defmethod peek-queue ((queue queue))
  (aref (buffer queue) (tail-index queue)))

(defmethod queue-empty-p ((queue queue))
  (= (head-index queue) (tail-index queue)))

(defmethod queue-full-p ((queue queue))
  (= (mod (tail-index queue) (length (buffer queue)))
     (mod (1+ (head-index queue)) (length (buffer queue)))))

(defmethod queue-count ((queue queue))
  (let ((head-index (head-index queue))
        (tail-index (tail-index queue)))
    (cond
      ((= head-index tail-index)
       0)
      ((< tail-index head-index)
       (- head-index tail-index))
      ((> tail-index head-index)
       (- (+ (length (buffer queue)) head-index)
          tail-index)))))

(defmethod random-queue-element ((queue queue))
  (let ((tail-index (tail-index queue))
        (buffer (buffer queue))
        (count (queue-count queue)))
    (when (zerop count)
      (error "Queue ~A is empty" queue))
    (aref buffer (mod (+ tail-index (random count))
                      (length buffer)))))

(defmethod call-for-all-elements-with-index ((queue queue) callback)
  "Calls CALLBACK passing it each element in QUEUE. The elements
will be called in the same order thah DEQUEUE would return them."
  (flet ((callback (index)
           (funcall callback (aref (buffer queue) index) index)))
    (if (< (head-index queue) (tail-index queue))
        ;; growing from the bottom. conceptualy the new elements need
        ;; to go between tail and head. it's simpler to just move them
        ;; all
        (progn
          (loop
             for index upfrom (tail-index queue) below (length (buffer queue))
             do (callback index))
          (loop
             for index upfrom 0 below (head-index queue)
             do (callback index)))
        ;; growing from the top
        (loop
           for index from (tail-index queue) below (head-index queue)
           do (callback index)))))

(defmacro do-all-elements ((element queue &optional index) &body body)
  (if index
      `(call-for-all-elements-with-index ,queue
                                         (lambda (,element ,index)
                                           ,@body))
      (let ((index (gensym "do-all-elements-index-")))
        `(call-for-all-elements-with-index ,queue
                                           (lambda (,element ,index)
                                             (declare (ignore ,index))
                                             ,@body)))))

(defmethod grow-queue ((queue queue))
  (let ((new-buffer (make-array (* (length (buffer queue)) 2)
                                :element-type (array-element-type (buffer queue)))))
    (let ((index 0))
      (do-all-elements (element queue)
        (setf (aref new-buffer index) element)
        (incf index))
      (setf (head-index queue) index
            (tail-index queue) 0
            (buffer queue) new-buffer))
    queue))

(defmacro incf-mod (place divisor)
  `(setf ,place (mod (1+ ,place) ,divisor)))

(defmethod move-tail ((queue queue))
  (incf-mod (tail-index queue) (length (buffer queue))))

(defmethod move-head ((queue queue))
  (incf-mod (head-index queue) (length (buffer queue))))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t :identity t)
    (format stream "~D" (queue-count queue))))

(defmethod queue->list ((queue queue))
  (let ((res nil))
    (do-all-elements (element queue)
      (push element res))
    (nreverse res)))

;;;; ** LRU Queue

(defclass lru-queue (queue)
  ()
  (:documentation "A queue which never grows. When an element is
  enqueued and the buffer is full we simply drop the last
  element."))

(defmethod enqueue ((queue lru-queue) value)
  (when (queue-full-p queue)
    (dequeue queue))
  (call-next-method queue value))

(defmethod enqueue-or-move-to-front ((queue lru-queue) element &key (test #'eql) (key #'identity))
  "Enqueues ELEMENT, if ELEMENT is already in the queue it is
  moved to the head.

NB: this method needs a better name."
  (do-all-elements (e queue index)
    (when (funcall test element (funcall key e))
      ;; found the element
      (rotatef (aref (buffer queue) index)
               (aref (buffer queue) (1- (if (zerop (head-index queue))
                                            (length (buffer queue))
                                            (head-index queue)))))
      (return-from enqueue-or-move-to-front queue)))
  ;; if we get here the element wasn't found
  (enqueue queue element))
