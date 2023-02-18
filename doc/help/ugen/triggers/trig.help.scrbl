#lang scribble/manual
@(require (for-label racket))

@title{(trig in dur)}


When `in' is trigerred output the trigger value for `dur' seconds.


@racketblock[
(let ((s (f-sin-osc ar 800 0))
      (g (trig (dust ar 1) 0.2)))
  (audition (out 0 (mul3 s g 0.5))))
]


@racketblock[
(audition (out 0 (trig (dust ar 4) 0.1)))
]


