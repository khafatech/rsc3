#lang scribble/manual
@(require (for-label racket))

@title{(tw-index in normalize array)}

triggered windex.  When triggered, returns a random index value
based on array as a list of probabilities.  By default the list of
probabilities should sum to 1.0, when the normalize flag is set to
1, the values get normalized by the ugen (less efficient)
 
Assuming normalized values 

(audition
 (let* ((prob (mce3 1/5 2/5 2/5))
	(freq (mce3 400 500 600))
	(f (select (tw-index (impulse kr 6 0) 0.0 prob) freq)))
   (out 0 (mul (sin-osc ar f 0) 0.2))))

Modulating probability values 

(audition
 (let* ((t (impulse kr 6 0))
	(a (mce3 1/4 1/2 (mul-add (sin-osc kr 0.3 0) 0.5 0.5)))
	(f (select (tw-index t 1.0 a) (mce3 400 500 600))))
   (out 0 (mul (sin-osc ar f 0) 0.2))))

