#lang scribble/manual
@(require (for-label racket))

@title{(lf-tri rate freq iphase)}

A non-band-limited triangular waveform oscillator. output ranges
from -1 to +1.

(audition (out 0 (mul (lf-tri ar 500 1) 0.1)))

Used as both oscillator and LFO.

(let ((f (mul-add (lf-tri kr 4 0) 400 400)))
  (audition (out 0 (mul (lf-tri ar f 0) 0.1))))

