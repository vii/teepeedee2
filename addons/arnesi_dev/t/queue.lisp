;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.queue :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.queue)

(test make-queue
  (is (queue-empty-p (make-instance 'queue)))
  (is (eql 'empty (dequeue (make-instance 'queue) 'empty))))

(test queue-not-full-no-wrapping
  (let ((q (make-instance 'queue)))
    (enqueue q 1)
    (is (= 1 (dequeue q)))
    (enqueue q 1)
    (enqueue q 2)
    (is (= 1 (dequeue q)))
    (is (= 2 (dequeue q)))))

(test queue-full-not-wrapping
  (let ((q (make-instance 'queue :size 2)))
    (enqueue q 1)
    (enqueue q 2) ;; this causes the size to grow to 2
    (enqueue q 3) ;; this causes the size to grow to 4
    (enqueue q 4) ;; this doesn't affect the size
    (enqueue q 5) ;; this couses the size to grow to 8
    (is (= 1 (dequeue q)))
    (is (= 2 (dequeue q)))
    (is (= 3 (dequeue q)))
    (is (= 4 (dequeue q)))
    (is (= 5 (dequeue q)))))

(test queue-not-full-wrapping
  (let ((q (make-instance 'queue :size 2)))
    (enqueue q 1)
    (is (= 1 (queue-count q)))
    (is (= 1 (dequeue q)))
    (enqueue q 1)
    (is (= 1 (queue-count q)))
    (is (= 1 (dequeue q)))))

(test queue-full-wrapping
  (let ((q (make-instance 'queue :size 2)))
    (setf (arnesi::head-index q) 2
          (arnesi::tail-index q) 1
          (arnesi::buffer q) #(0 1))
    q
    (enqueue q 2)
    (is (= 1 (dequeue q)))
    (is (= 2 (dequeue q)))))

(test queue
  (for-all ((item (gen-integer :min -10 :max 10)))
    (let ((q (make-instance 'queue)))
      (enqueue q item)
      (is (= item (dequeue q)))
      (is (= 0 (queue-count q)))))
  (for-all ((one (gen-list :length (gen-integer :min 2 :max 3)
                           :elements (gen-integer :min -10 :max 10)))
            (two (gen-list :length (gen-integer :min 2 :max 3)
                           :elements (gen-integer :min -10 :max 10)))
            (three (gen-list :length (gen-integer :min 2 :max 3)
                             :elements (gen-integer :min -10 :max 10))))
    (let ((q (make-instance 'queue :size (1- (+ (length one)
                                                (length two)
                                                (length three))))))
      (flet ((enqueue-all (list)
               (loop for e in list do (enqueue q e)))
             (dequeue-all (list)
               (loop for e in list do (is (= e (dequeue q))))))
        (enqueue-all one)
        (enqueue-all two)
        (dequeue-all one)
        (enqueue-all three)
        (dequeue-all two)
        (dequeue-all three))
      (is (queue-empty-p q)))))
