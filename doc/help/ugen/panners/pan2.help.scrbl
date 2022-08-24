#lang scribble/manual
@(require (for-label racket))

@title{(pan2 in pos level)}

Two channel equal power panner.  The pan position is bipolar, -1 is
left, +1 is right.  The level is a control rate input.

(let ((p (f-sin-osc kr 2 0)))
  (audition (out 0 (pan2 (pink-noise ar) p 0.3))))

(let ((x (mouse-x kr -1 1 0 0.1))
      (y (mouse-y kr 0 1 0 0.1)))
  (audition (out 0 (pan2 (pink-noise ar) x y))))

