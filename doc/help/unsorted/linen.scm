; linen ; open gate for random interval
(let* ((r (s:rand 0.05 0.4))
       (u (letc ((gate 0))
	    (let ((e (linen gate 0.1 0.2 0.1 do-nothing)))
	      (out 0 (mul (sin-osc 440 0) e)))))
       (g (encode-graphdef (synthdef "linen" u))))
  (with-sc3
   (lambda (fd)
     (async fd (d-recv g))
     (send fd (s-new0 "linen" 1001 1 1))
     (send fd (bundle (utcr) (list (n-set1 1001 "gate" 1))))
     (send fd (bundle (+ (utcr) r) (list (n-set1 1001 "gate" 0))))
     (pauseThread (* r 4))
     (send fd (n-free1 1001)))))
