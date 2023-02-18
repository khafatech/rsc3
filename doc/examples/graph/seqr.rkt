#lang racket
;; status - working as of 20220820

(require rsc3)

;; seqr (rd)

(define nfreq
  (lambda (n l r)
    (map midi-cps (map floor (replicate-m n (rand-float l r))))))

(define seqr-f
  (lambda (f e)
    (let ((n (/ (length e) 2)))
      (select (mul-add (lf-saw kr f 0) n n) (make-mce e)))))

(define seqr
  (let* ((n (rand-int 6 18))
         (f (/ (rand-int 9 18) n)))
    (mul (blip ar
               (mce2 (seqr-f f (nfreq n 72 96))
                     (seqr-f f (nfreq n 72 84)))
               (mce2 (seqr-f f (replicate-m n (rand-float 1 3)))
                     (seqr-f f (replicate-m n (rand-float 3 6)))))
         (mce2 (seqr-f f (replicate-m n (rand-float 0.05 0.10)))
               (seqr-f f (replicate-m n (rand-float 0.05 0.15)))))))

(audition (out 0 seqr))
