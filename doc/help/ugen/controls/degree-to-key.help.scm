;; (degree-to-key bufnum in octave)

;; Convert signal to modal pitch

;; The input signal value is truncated to an integer value and used as
;; an index into an octave repeating table of note values. indices
;; wrap around the table and shift octaves as they do.

;; bufnum - index of the buffer which contains the steps for each
;;          scale degree.

;; in - the input signal.

;; octave - the number of steps per octave in the scale.

(let* ((b 0)
       (p (list 0 2 3.2 5 7 9 10))
       (x (mouse-x kr 0 15 0 0.1))
       (k (degree-to-key 0 x 12))
       (c (lambda (n r)
	    (let* ((o (mul (sin-osc ar (midi-cps (add3 r k n)) 0) 0.1))
		   (t (lf-pulse ar (midi-cps (mce2 48 55)) 0.15 0.5))
		   (f (midi-cps (mul-add (sin-osc kr 0.1 0) 10 r)))
		   (d (mul (rlpf t f 0.1) 0.1))
		   (m (add o d)))
	      (add (comb-n m 0.31 0.31 2) m)))))
  (with-sc3
   (lambda (fd)
     (async fd (b-alloc 0 7 1))
     (send fd (b-setn1 0 0 p))
     (let ((n (mul (lf-noise1 kr (mce2 3 3.05)) 0.04)))
       (play fd (out 0 (mul (add (c n 48) (c n 72)) 0.25)))))))
