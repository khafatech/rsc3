#lang scribble/manual
@(require (for-label racket))

@title{pack-fft
(let* ((n 100)
       (n* (enumFromTo 1 n))
       (m1 (map (lambda (_) (range (f-sin-osc (exp-rand 0.1 1) 0) 0 1)) n*))
       (square (lambda (a) (* a a)))
       (m2 (map mul m1 (map square (take 100 (enumFromThenTo 1 (- 1 (/ 1 n)) 0)))))
       (i (map (lambda (_) (lf-pulse (pow 2 (i-rand -3 5)) 0 0.3)) n*))
       (m3 (map mul m2 i))
       (p (replicate n 0.0))
       (c1 (fft* (local-buf 1 512) (f-sin-osc 440 0)))
       (c2 (pack-fft c1 512 0 (- n 1) 1 (packfft-data m3 p)))
       (s (ifft* c2)))
  (mce2 s s))}


