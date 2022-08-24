; subsample-offset


; demonstrate cubic subsample interpolation, an impulse train that can be moved between samples
(with-sc3
 (lambda (fd)
   (send-synth
    fd
    "s"
    (letc ((out 0)
	   (add-offset 0))
      (let* ((i (mul (impulse 2000 0) 0.3))
	     (d sample-dur)
	     (x 4)
	     (o (add (sub 1 subsample-offset)
		     (mouse-x 0 add-offset 0 0.1)))
	     (r (delay-c i (mul d (add 1 x)) (mul d (add o x)))))
	(offset-out out r))))))

; Create two pulse trains one sample apart, move one relative to the
; other.  When cursor is at the left, the impulsese adjacent, on
; the right, theye exactly 1 sample apart.  View this with an
; oscilloscope.

(with-sc3
 (lambda (fd)
   (let ((t (utcr))
	 (dt (/ 1 (server-sample-rate-actual fd))))
     (send fd (bundle (+ t 0.2)
		      (list (s-new1 "s" -1 1 1 "addOffset" 3))))
     (send fd (bundle (+ t 0.2 dt)
		      (list (s-new1 "s" -1 1 1 "addOffset" 0)))))))
