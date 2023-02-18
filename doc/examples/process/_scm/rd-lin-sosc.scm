;; lin-sosc (rd)

(define twoPi (* 2.0 pi))

(define mk-line
  (lambda (n l r)
    (enumFromThenTo l (+ l (/ (- r l) n)) r)))

(define geom
  (lambda (n i s)
    (if (= n 0)
        nil
        (cons i (geom (- n 1) (* i s) s)))))

(define mk-xline
  (lambda (n l r)
    (geom n l (expt (/ r l) (/ 1 n)))))

(define rng
  (lambda (l r)
    (let ((m (- l r)))
      (lambda (e)
        (+ l (* e m))))))

(define s-rng
  (lambda (l r)
    (let* ((m (/ (- l r) 2))
           (n (+ m l)))
      (lambda (e)
        (+ n (* e m))))))

(define cmp
  (lambda (f g)
    (lambda (n)
      (f (g n)))))

(define tbl-m
  (lambda (b)
    (PlayBuf 1 b (Mul (MouseX 0.001 1.0 0 0.1) (BufRateScale b)) 0 0 1 doNothing)))

(define tbl-c
  (lambda (b c)
    (PlayBuf 1 b (Mul (In 1 c) (BufRateScale b)) 0 0 1 doNothing)))

;; (withSc3 (lambda (fd) (settr fd 1024)))
(define settr
  (lambda (fd n)
    (let* ((freq
            (list
             (mk-line n 440.0 444.0)
             (mk-line n 40.0 16000.0)
             (mk-xline n 40.0 16000.0)
             (map (cmp (s-rng 20 21000) sin) (mk-line n 0 twoPi))
             (map (cmp (s-rng 20 12000) cos) (mk-line n 0 twoPi))
             (map (cmp (s-rng 20 22000) tan) (mk-line n -0.76 0.76))
             (map (cmp (s-rng 20 90) tan) (mk-line n -0.76 0.76))))
           (ampl
            (list
             (mk-line n 0.1 1.0)
             (mk-line n 1.0 0.1)
             (mk-line n 0.5 0.01)
             (mk-line n 0.01 0.5)
             (mk-xline n 1.0 0.1)
             (mk-xline n 0.1 1.0)
             (map sin (mk-line n 0.0 twoPi))
             (map cos (mk-line n 0.0 twoPi))
             (map (lambda (n) (* n 0.001))
                  (map tan (mk-line n 0.0 twoPi)))))
           (f (s:l-choose freq))
           (a (s:l-choose ampl)))
      (begin
        (sendMessage fd (b_setn1 0 0 f))
        (sendMessage fd (b_setn1 1 0 a))
        (sendMessage fd (c_set1 0 (s:l-choose (list 0.005 0.0075 0.01 0.025 0.05 0.075
                                                     0.1 0.25 0.5 0.75
                                                     0.8 0.85 1.0 1.005))))
        (s:l-choose (list 0.01 0.05 0.1 0.15 0.25 0.5 0.75))))))

(define lsi
  (Clip2
   (Pan2 (Mul (SinOsc (tbl-m 0) 0) (tbl-m 1)) (tbl-c 1 0) 0.025)
   0.25))

(define pattern
  (lambda (fd n)
    (begin
      (thread-sleep (settr fd n))
      (pattern fd n))))

(define lin-sosc
  (lambda (n)
    (lambda (fd)
      (begin
        (async fd (b_alloc 0 n 1))
        (async fd (b_alloc 1 n 1))
        (play fd (Out 0 lsi))
        (settr fd n)
        (pattern fd n)))))

(withSc3 (lin-sosc 1024))
