#lang scribble/manual
@(require (for-label racket))

@title{(shaper bufnum in)}


Wave shaper.  Performs waveshaping on the input signal by indexing
into the table.

bufnum - the number of a buffer filled in wavetable format
         containing the transfer function.
in     - the input signal.


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 512 1))
   (async fd (b-gen1 10 "cheby" (list 0 1 0 1 1 0 1)))))
]


@racketblock[
(audition
 (let* ((a (line kr 0 1 6 remove-synth))
	(s (mul (sin-osc ar 300 0) a)))
   (out 0 (mul (shaper 10 s) 0.5))))
]


