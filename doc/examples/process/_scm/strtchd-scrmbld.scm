; strtchd-scrmbld (rd)

(define dustR
  (lambda (r lo hi)
    (let ((d (Dseq dinf (Dwhite 1 lo hi))))
      (TDuty r d 0 0 (Abs (WhiteNoise r)) 1))))

(define rpr
  (lambda (n t)
    (let ((i (CtlIn 2 n)))
      (TRand (mceChannel i 0) (mceChannel i 1) t))))

(define strtchd
  (lambda (b z)
    (let* ((clk (DustRange (CtlIn 1 0) (CtlIn 1 1)))
           (rat (rpr 2 clk))
           (dur (rpr 4 clk))
           (bdr (BufDur b))
           (hbd (Mul bdr 0.5))
           (pos (Add (Mul (rpr 8 clk) bdr) (MulAdd (LFSaw z 0) hbd hbd)))
           (pan (rpr 10 clk))
           (amp (rpr 6 clk)))
      (TGrains 2 clk b rat pos dur pan amp 2))))

(define scrmbld
  (lambda (u b t)
    (let* ((f (fftDefaults b u))
           (g (PV_BinScramble
	       f
               (MouseX 0.5 1.0 0 0.1)
               (MouseY 0.5 1.0 0 0.1)
               t)))
      (ifftDefaults g))))

(define strtchd-scrmbld
  (let ((t0 (Dust 0.01))
        (t1 (Dust 0.02))
        (u (Add (strtchd 10 0.015) (strtchd 10 0.0175))))
    (Mce2
     (scrmbld (mceChannel u 0) 20 t0)
     (scrmbld (mceChannel u 1) 30 t1))))

(define mk-r-set
  (lambda ()
    (list (s:rand 0.005 0.001)   (s:rand 0.0075 0.0125)
          (s:rand 0.90 0.975)    (s:rand 1.025 1.10)
          (s:rand 0.005 0.075)   (s:rand 0.075 0.125)
          (s:rand 0.005 0.01)    (s:rand 0.15 0.25)
          (s:rand -0.015 -0.005) (s:rand 0.005 0.015)
          (s:rand -1 0)          (s:rand 0 1.0))))

(define pattern
  (lambda (fd)
    (begin
      (sendMessage fd (c_setn1 0 (mk-r-set)))
      (thread-sleep (s:l-choose (list 0.05 0.15 0.25 0.5 0.75 1.25)))
      (pattern fd))))

(withSc3
 (lambda (fd)
   (begin
     (async fd (b_allocRead 10 "/home/rohan/data/audio/metal.wav" 0 0))
     (async fd (b_alloc 20 2048 1))
     (async fd (b_alloc 30 2048 1))
     (play fd (Out 0 strtchd-scrmbld))
     (pattern fd))))
