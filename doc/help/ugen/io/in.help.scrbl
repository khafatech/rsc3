#lang scribble/manual
@(require (for-label racket))

@title{(in num-channels rate bus)}


Read signal from an audio or control bus.
 
Patching input to output.


@racketblock[
(audition (out 0 (in 2 ar num-output-buses)))
]

Patching input to output, with summed delay.


@racketblock[
(let ((i (in 2 ar num-input-buses)))
  (audition (out 0 (add i (delay-n i 0.5 0.5)))))
]

Write noise to bus 10, then read it out.  The Mrg is ordered.


@racketblock[
(audition (mrg2 (out 0 (in 1 ar 10))
		(out 10 (mul (pink-noise ar) 0.3))))
]

Reading a control bus.


@racketblock[
(with-sc3
 (lambda (fd)
   (send fd (c-set1 0 (random 200 5000)))))
]


@racketblock[
(audition (out 0 (mul (sin-osc ar (in 1 kr 0) 0) 0.1)))
]


