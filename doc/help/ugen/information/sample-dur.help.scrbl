#lang scribble/manual
@(require (for-label racket))

@title{sample-dur}


Duration of one sample.  Equivalent to 1 / sample-rate.

Compare a sine tone derived from sample rate with a 440Hz tone.


@racketblock[
(let ((freq (mce2 (mul (recip sample-dur) 0.01) 440)))
  (audition (out 0 (mul (sin-osc ar freq 0) 0.1))))
]


