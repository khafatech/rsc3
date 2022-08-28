#lang scribble/manual
@(require (for-label racket))

@title{formant}


@racketblock[
(import (rsc3))
]

Modulate fundamental frequency, formant frequency stays constant.

@racketblock[
(audition
 (let ((f (x-line kr 400 1000 8 remove-synth)))
   (out 0 (mul (formant ar f 2000 800) 0.125))))
]

Modulate formant frequency, fundamental frequency stays constant.

@racketblock[
(audition
 (let ((f (x-line kr 400 4000 8 remove-synth)))
   (out 0 (mul (formant ar (mce2 200 300) f 200) 0.125))))
]

Modulate width frequency, other frequencies stay constant.

@racketblock[
(audition
 (let ((w (x-line kr 800 8000 8 remove-synth)))
   (out 0 (mul (formant ar 400 2000 w) 0.125))))
]


