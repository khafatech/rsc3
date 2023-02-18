#lang scribble/manual
@(require (for-label racket))

@title{(sweep trig rate)}


triggered linear ramp.  Starts a linear raise by rate/sec from zero
when trig input crosses from non-positive to positive.
	
Using sweep to modulate sine frequency


@racketblock[
(let* ((t (impulse kr (mouse-x kr 0.5 20 1 0.1) 0))
       (f (add (sweep t 700) 500)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
]

Using sweep to index into a buffer


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 0 "/home/rohan/audio/metal.wav" 0 0))))
]


@racketblock[
(let* ((t (impulse ar (mouse-x kr 0.5 20 1 0.1) 0))
       (i (sweep t (buf-sample-rate ir 0))))
  (audition (out 0 (buf-rd 1 ar 0 i 0 2))))
]

Backwards, variable offset


@racketblock[
(let* ((t (impulse ar (mouse-x kr 0.5 10 1 0.1) 0))
       (r (buf-sample-rate ir 0))
       (i (add (sweep t (neg r)) (mul (buf-frames ir 0) (lf-noise0 kr 15)))))
  (audition (out 0 (buf-rd 1 ar 0 i 0 2))))
]

Raising rate


@racketblock[
(let* ((t (impulse ar (mouse-x kr 0.5 10 1 0.1) 0))
       (r (add (sweep t 2) 0.5))
       (i (sweep t (mul (buf-sample-rate ir 0) r))))
  (audition (out 0 (buf-rd 1 ar 0 i 0 2))))
]


