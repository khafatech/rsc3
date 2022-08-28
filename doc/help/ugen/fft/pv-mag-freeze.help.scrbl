#lang scribble/manual
@(require (for-label racket))

@title{(PV_Magfreeze buffer freeze)}


freeze magnitudes.  freezes magnitudes at current levels when
freeze > 0.

buffer - fft buffer.
freeze - if > 0 then magnitudes are frozen at current levels.


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))
]


@racketblock[
(let ((dup (lambda (a) (mce2 a a)))
      (s (sin-osc ar (mul-add (lf-noise1 kr 5.2) 250 400) 0))
      (f (sin-osc kr 0.2 0)))
  (audition (out 0 (dup (mul 0.25 (ifft* (pv-mag-freeze (fft* 10 s) f)))))))
]


@racketblock[
(let ((dup (lambda (a) (mce2 a a)))
      (s (play-buf 1 12 (buf-rate-scale kr 12) 1 0 1))
      (f (gt (mouse-y kr 0 1 0 0.1) 0.5)))
  (audition (out 0 (dup (mul 0.25 (ifft* (pv-mag-freeze (fft* 10 s) f)))))))
]


