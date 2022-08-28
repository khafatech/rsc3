#lang scribble/manual
@(require (for-label racket))

@title{(buf-sample-rate rate bufnum)}


Buffer sample rate.


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 0 "/home/rohan/audio/metal.wav" 0 0))))
]

Compare a sine tone derived from sample rate of a buffer with a
440Hz tone.


@racketblock[
(let ((freq (mce2 (mul (buf-sample-rate ir 0) 0.01) 440)))
  (audition (out 0 (mul (sin-osc ar freq 0) 0.1))))
]


