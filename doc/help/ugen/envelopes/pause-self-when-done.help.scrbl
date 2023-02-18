#lang scribble/manual
@(require (for-label racket))

@title{(pause-self-when-done src)}


pause the synth when the 'done' flag of the unit at `src' is set.


@racketblock[
(let* ((x (mouse-x kr -1 1 0 0.1))
       (e (linen x 1 0.1 1 pause-synth)))
  (audition (out 0 (mul (sin-osc ar 440 0) e))))
]


@racketblock[
(let* ((x (mouse-x kr -1 1 0 0.1))
       (e (linen x 2 0.1 2 do-nothing)))
  (audition (mrg2 (pause-self-when-done e)
		  (out 0 (mul (sin-osc ar 440 0) e)))))
]


