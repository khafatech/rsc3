#lang racket

(require rsc3 rhs/rhs)

(with-sc3 (lambda (fd)
            (send fd (g-new1 1 add-to-tail 0))))

#|
(define cymbalism
  (let* ((p 15)
         (f1 (rand 500 2500))
         (f2 (rand 0 (mouse-x)))
         (y
          (lambda ()
            (let ((f (replicate-m p (add f1 (rand 0 f2))))
                  (rt (replicate-m p (rand 1 5)))
                  (a (replicate p 1)))
              (klank-data f a rt))))
         (z (mce2 (y) (y)))
         (t (impulse ar (rand 0.5 3.5) 0))
         (n (mul (white-noise ar) 0.03))
         (s (mul (decay t 0.004) n)))
    (klank s 1 0 1 (mce-transpose z))))

(audition (out 0 cymbalism))

|#

(with-sc3 reset)

#|
(let* ((x (mouse-x kr 220 440 0 0.1))
       (f (mce2 x (lag2 x 0.2))))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))

|#

;; help/ugen/filters/moog-ff.help.scm
#;(let* ((n (mul (white-noise ar) 0.1))
         (y (mouse-y kr 100 10000 1 0.1))
         (x (mouse-x kr 0 4 0 0.1)))
    (audition (out 0 (moog-ff n y x 0))))

;; lorenz-l.help.scm
#;(let* ((n (mul (white-noise ar) 0.1))
       (y (mouse-y kr 40 80 1 0.1))
       (x (mouse-x kr 2 10 0 0.1))
       (lz (mul (lorenz-l ar sample-rate
                         x ;(mul-add (lf-noise0 kr x) 2 10)
                         y ;(mul-add (lf-noise0 kr 20) 20 38)
                         1.6; (mul-add (lf-noise0 kr 1) 1.5 2)
                         0.05
                         0.1 0.0 0.0)
               0.2)))
  (audition
   (out 0 lz)))

#;(let ((fb (line kr 0.01 4 10 do-nothing)))
  (audition
   (out 0 (mul (fb-sine-c ar sample-rate 1 (mouse-x kr 1 5 1 0.1) 1.1 (mouse-y kr 0.5 1 1 0.1) 0.1 0.1) 0.2))))

;; fb-sine-c.help.scm
#;(let* ((x (mouse-x kr 0.1 1 0 0.1))
       (f (lambda (m a) (mul-add (lf-noise2 kr x) m a))))
  (audition
   (out 0 (mul (fb-sine-c ar
			  (f 1e4 1e4)
			  (f 32 33)
			  (f 0.5 0)
			  (f 0.05 1.05)
			  (f 0.3 0.3)
			  0.1
			  0.1)
	       0.2))))

(define (simple-mouse-x start end)
  (mouse-x kr start end 0 0.1))

;; like old telephone ring. uses key-state aaaa
; (let ([freq (mouse-x kr 600 2000 1 0.1)])
(let ([freq (simple-mouse-x 600 1500)])
  (audition (out 0 
                 (mul (sin-osc ar 
                               (mul-add (lf-pulse ar 15 0 0.5) 200 freq)
                               0)
                      (key-state kr 38 0 0.1 0.1)))))

