;; (wrap-index bufnum in)

;; index into a table with a signal.

;; The input signal value is truncated to an integer value and used as
;; an index into the table.  out of range index values are wrapped
;; cyclically to the valid range.

;; bufnum - index of the buffer
;; in     - the input signal.

(with-sc3 
 (lambda (fd)
   (async fd (b-alloc 0 6 1))
   (send fd (b-setn1 0 0 (list 200 300 400 500 600 800)))
   (let ((f (wrap-index 0 (mouse-x kr 0 18 0 0.1))))
     (play fd (out 0 (mul (sin-osc ar f 0) 0.5))))))
