#lang scribble/manual
@(require (for-label racket))

@title{(select which array)}


The output is selected from an array of inputs.


@racketblock[
(audition
 (let* ((a (mce3 (sin-osc ar 440 0) (saw ar 440) (pulse ar 440 0.1)))
	(cycle 3/2)
	(w (mul-add (lf-saw kr 1 0) cycle cycle)))
   (out 0 (mul (select w a) 0.2))))
]

Note: all the ugens are continously running. This may not be the
most efficient way if each input is cpu-expensive.

Here used as a sequencer:


@racketblock[
(audition
 (let* ((n 32)
	(a (make-mce (map (compose midi-cps u:floor) 
			  (replicate-m n (rand 30 80)))))
	(cycle (/ n 2))
	(w (mul-add (lf-saw kr 1/2 0) cycle cycle)))
   (out 0 (mul (saw ar (select w a)) 0.2))))
]


