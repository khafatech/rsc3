#lang scribble/manual
@(require (for-label racket))

@title{(dust2 rate density)}

Generates random impulses from -1 to +1.  The `density' is in
impulses per second.

(audition (out 0 (mul (dust2 ar 200) 0.5)))

(let ((r (x-line kr 20000 2 10 remove-synth)))
  (audition (out 0 (mul (dust2 ar r) 0.5))))

