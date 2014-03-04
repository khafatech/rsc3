#lang racket

(require rnrs
         rhs/rhs
         rnrs/bytevectors-6
         rnrs/io/ports-6)

(provide (all-defined-out)
         put-bytevector
         get-bytevector-n
         bytevector-length)

;; JBC-2014 -- looks like all of these have
;; equivalents in Racket...

;; bytevector -> (port -> any) -> any
(define with-input-from-bytevector
  (lambda (b f)
    (let* ((p (open-bytevector-input-port b))
	   (r (f p)))
      (close-port p)
      r)))

;; bytevector -> int -> int -> bytevector
(define bytevector-section
  (lambda (v l r)
    (let* ((n (- r l))
	   (w (make-bytevector n 0)))
      (bytevector-copy! v l w 0 n)
      w)))

;; bytevector -> byte -> int
(define bytevector-find-index
  (lambda (v x)
    (letrec ((f (lambda (i)
		  (if (= (bytevector-u8-ref v i) x)
		      i
		      (f (+ i 1))))))
      (f 0))))

;; Tree bytevector -> bytevector
(define flatten-bytevectors
  (lambda (t)
    (let* ((l (flatten t))
	   (n (map1 bytevector-length l))
	   (m (sum n))
	   (v (make-bytevector m)))
      (let loop ((i 0)
		 (l l)
		 (n n))
	(if (null? l)
	    v
	    (let ((l0 (car l))
		  (n0 (car n)))
	      (bytevector-copy! l0 0 v i n0)
	      (loop (+ i n0) (cdr l) (cdr n))))))))

;; number a => (bytevector -> int -> a -> ()) -> int -> a
(define bytevector-make-and-set1
  (lambda (f k n)
    (let ((v (make-bytevector k 0)))
      (f v 0 n)
      v)))

;; number a => (bytevector -> int -> a -> ()) -> int -> a
(define bytevector-make-and-set
  (lambda (f k n)
    (let ((v (make-bytevector k 0)))
      (f v 0 n (endianness big))
      v)))


(module+ test
  (require rackunit)
  
  ;; test bytevector-section, which is equivalent ot subbytes
  (test-begin
   (let [(long-vec (bytes 10 20 30 40 50))]
     (check-equal? (bytevector-section long-vec 0 2)
                   (bytes 10 20))
     (check-equal? (bytevector-section long-vec 0 (bytes-length long-vec))
                   long-vec)
     (check-equal? (bytevector-section long-vec 1 3)
                   (subbytes long-vec 1 3))
     
     ; check if exceding limits raises exception
     (check-exn exn:fail? (λ () (bytevector-section long-vec 0 30)))
     (check-exn exn:fail? (λ () (bytevector-section long-vec -1 3)))))
  
  (check-equal? (flatten-bytevectors (list (bytes 10 20) (bytes 30 40) (list (bytes 50 60))))
                (bytes 10 20 30 40 50 60)))