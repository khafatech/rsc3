#lang scribble/manual
@(require (for-label racket))

@title{allocate and fill tables 0 to 7, c.f. v-osc3}


@racketblock[
(with-sc3
 (lambda (fd)
   (let* ((square
	   (lambda (a) (* a a)))
	  (nth
	   (lambda (i)
	     (async fd (b-alloc i 1024 1))
	     (let* ((n (expt (+ i 1) 2))
		    (a (map (lambda (j) (square (/ (- n j) n)))
			    (enumFromTo 0 (- n 1)))))
	       (async fd (b-gen1 i "sine1" (cons 7 a)))))))
     (for-each nth (enumFromTo 0 7)))))
]

v-osc ; buffers 0 through 7

@racketblock[
(let ((b (mouse-x 0 7 0 0.1))
      (f (mce2 120 121)))
  (mul (v-osc b f 0) 0.1))
]

reallocate buffers while oscillator is running

@racketblock[
(with-sc3
 (lambda (fd)
   (for-each
    (lambda (i)
      (async fd (b-gen1  i "sine1" (cons 7 (replicateM 16 (lambda () (s:rand 0 1)))))))
    (enumFromTo 0 7))))
]


