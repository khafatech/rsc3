#lang scribble/manual
@(require (for-label racket))

@title{(saw rate freq)}


Band limited sawtooth wave generator.


@racketblock[
(let ((f (x-line kr 40 4000 6 do-nothing)))
  (audition (out 0 (mul (saw ar f) 0.2))))
]

Two band limited sawtooth waves thru a resonant low pass filter


@racketblock[
(let ((f (x-line kr 8000 400 5 do-nothing)))
  (audition (out 0 (rlpf (mul (saw ar (mce2 100 250)) 0.1) f 0.05))))
]


