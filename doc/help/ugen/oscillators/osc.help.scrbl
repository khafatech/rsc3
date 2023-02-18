#lang scribble/manual
@(require (for-label racket))

@title{(osc rate bufnum freq phase)}


linear interpolating wavetable lookup oscillator with frequency and
phase modulation inputs.

This oscillator requires a buffer to be filled with a wavetable
format signal.  This preprocesses the Signal into a form which can
be used efficiently by the oscillator.  The buffer size must be a
power of 2.

This can be acheived by creating a Buffer object and sending it one
of the "b_gen" messages ( sine1, sine2, sine3 ) with the wavetable
flag set to true.

Note about wavetables: oscN requires the b_gen sine1 wavetable flag
to be OFF.  osc requires the b_gen sine1 wavetable flag to be ON.


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 512 1))
   (async fd (b-gen1 10 "sine1" (list (+ 1 2 4) 1 1/2 1/3 1/4 1/5)))))
]


@racketblock[
(audition (out 0 (mul (osc ar 10 220 0) 0.1)))
]

Modulate freq


@racketblock[
(let ((f (x-line kr 2000 200 1 remove-synth)))
  (audition (out 0 (mul (osc ar 10 f 0) 0.5))))
]

Modulate freq


@racketblock[
(let* ((f1 (x-line kr 1 1000 9 remove-synth))
       (f2 (mul-add (osc ar 10 f1 0) 200 800)))
  (audition (out 0 (mul (osc ar 10 f2 0) 0.25))))
]

Modulate phase


@racketblock[
(let* ((f (x-line kr 20 8000 10 remove-synth))
       (p (mul (osc ar 10 f 0) (* 2 pi))))
  (audition (out 0 (mul (osc ar 10 800 p) 0.25))))
]

Change the buffer while its playing


@racketblock[
(audition (out 0 (mul (osc ar 10 220 0) 0.1)))
]


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-gen1 10 "sine1" (list (+ 1 2 4) 1 (random 0 1) 1/4)))))
]


