; a shorter variant, using some simple syntax, and a non-local buffer
(define seq (lambda (s l) (if (null? l) s (seq ((car l) s) (cdr l)))))

(define-syntax seq*
  (syntax-rules ()
    ((_ i s f ...)
       (seq i (list (lambda (s) f) ...)))))

; allocate and set non-local buffer=10
(withSc3
 (lambda (fd)
   (let ((a (list 97.999 195.998 523.251 466.164 195.998
		  233.082 87.307 391.995 87.307 261.626
		  195.998 77.782 233.082 195.998 97.999
		  155.563)))
     (async fd (b_alloc 10 128 1))
     (sendMessage fd (b_setn1 10 0 a)))))

;---- broken...
(let* ((b (ctl "buf" 0))
       (rate (MouseX 1 5 1 0.1))
       (clock (Impulse rate 0))
       (env (Decay2 clock 0.002 2.5))
       (index (Stepper clock 0 0 15 1 0))
       (freq (BufRd 1 b Index 1 1))
       (ffreq (Add (Lag2 freq 0.1) (Mce2 0 0.3)))
       (lfo (SinOsc 0.2 (Mce4 0 (/ pi 2) 0.0024 0.0025)))
       (rvb (lambda (s) (AllpassN s
				  0.05
				  (RandN 2 0 0.05)
				  (Rand 1.5 2.0))))
       (init (Mix (LFPulse (Mul freq (Mce3 1 3/2 2)) 0 0.3))))
  (seq*
   init
   s
   (Mul (RLPF s ffreq 0.3) env)
   (Mul (RLPF s ffreq 0.3) env)
   (Mul s 0.2)
   (MulAdd (CombL s 1 (Fdiv 0.66 rate) 2) 0.8 s)
   (Add s (Mul (seq s (replicate 5 rvb)) 0.3))
   (LeakDC s 0.1)
   (Add (DelayL s 0.1 lfo) s)
   (OnePole s 0.9)))

; pattern randomizer
(withSc3
 (lambda (fd)
   (let ((p (map (lambda (e)
		   (s:midi-cps (+ 36 (s:degree-to-key e (list 0 3 5 7 10) 12))))
		 (map floor (replicateM 16 (lambda () (s:rand 0 15)))))))
     (sendMessage fd (b_setn1 10 0 p)))))
