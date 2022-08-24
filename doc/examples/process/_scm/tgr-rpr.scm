; tgr-rpr (rd)

(define dustR
  (lambda (r lo hi)
    (let ((d (Dseq dinf (Dwhite 1 lo hi))))
      (TDuty r d 0 0 (Abs (WhiteNoise r)) 1))))

(define rpr
  (lambda (n t)
    (let ((i (CtlIn 2 n)))
      (TRand (mceChannel i 0) (mceChannel i 1) t))))

(define rSet
  (lambda (r)
    (if (> r 0.5)
        (list (s:rand 0.005 0.025) (s:rand 0.05 0.25)
              (s:rand 0.75 0.95)   (s:rand 1.05 1.25)
              (s:rand 0.001 0.01)  (s:rand 0.02 0.04)
              (s:rand 0.1 0.2)     (s:rand 0.2 0.4)
              (s:rand 0.0 0.45)    (s:rand 0.55 1.0)
              (s:rand -1 0)        (s:rand 0 1.0))
        (list (s:rand 0.005 0.025) (s:rand 0.05 0.25)
              (s:rand -1.25 -1.05) (s:rand -0.95 -0.75)
              (s:rand 0.001 0.01)  (s:rand 0.02 0.04)
              (s:rand 0.1 0.2)     (s:rand 0.2 0.4)
              (s:rand 0.0 0.45)    (s:rand 0.55 1.0)
              (s:rand -1 0)        (s:rand 0 1.0)))))

; (tgrRpr 10)
(define tgrRpr
  (lambda (b)
    (let* ((clk (DustRange (CtlIn 1 0) (CtlIn 1 1)))
           (rat (rpr 2 clk))
           (dur (rpr 4 clk))
           (pos (Mul (rpr 8 clk) (BufDur b)))
           (pan (rpr 10 clk))
           (amp (rpr 6 clk)))
      (TGrains 2 (kr: clk) b rat pos dur pan amp 2)))) ; clk must be kr

(define pattern
  (lambda (fd)
    (begin
      (sendMessage fd (c_setn1 0 (rSet (s:rand 0 1))))
      (thread-sleep (s:l-choose (list 0.25 0.75 1.5)))
      (pattern fd))))

(withSc3
 (lambda (fd)
   (begin
     (async fd (b_allocRead 10 "/home/rohan/data/audio/metal.wav" 0 0))
     (play fd (Out 0 (tgrRpr 10)))
     (pattern fd))))
