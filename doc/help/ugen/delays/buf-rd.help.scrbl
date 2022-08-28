#lang scribble/manual
@(require (for-label racket))

@title{(buf-rd numChannels rate bufnum phase loop interpolation)}


Plays the content of a buffer.

The number of channels must be a fixed integer. The architechture
of the SynthDef cannot change after it is compiled. NOTE: if you
supply a bufnum of a buffer that has a different numChannels then
you have specified to the buf-rd, it will fail silently.

The interpolation type is an integer: 1 no interpolation, 2 linear
interpolation, 4 cubic interpolation.


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 0 "/home/rohan/audio/metal.wav" 0 0))))
]


@racketblock[
(audition (out 0 (buf-rd 1 ar 0 (mul (sin-osc ar 0.1 0) (buf-frames ir 0)) 0 2)))
]


@racketblock[
(let ((phase (mul (lf-noise1 ar (mouse-x kr (mce2 5 10) 100 0 0.1))
		  (buf-frames ir 0))))
  (audition (out 0 (buf-rd-l 1 ar 0 phase 0))))
]


@racketblock[
(let ((phase (add (lf-tri ar 0.1 0)
		  (mul (lf-tri ar 0.23 0) (buf-frames ir 0)))))
  (audition (out 0 (buf-rd-l 1 ar 0 phase 0))))
]

Use a phasor index into the file


@racketblock[
(let ((phase (phasor ar 
		     0 
		     (mul (mouse-x kr 0.5 2 0 0.1)
			  (buf-rate-scale kr 0)) 
		     0 
		     (buf-frames kr 0) 
		     0)))
  (audition (out 0 (buf-rd 1 ar 0 phase 1 (mouse-y kr 0 5 0 0.1)))))
]


