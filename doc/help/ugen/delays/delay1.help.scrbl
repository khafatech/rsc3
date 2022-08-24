#lang scribble/manual
@(require (for-label racket))

@title{(delay1 in)}

Fixed Single sample delay.

(let ((s (impulse ar 1 0)))
  (audition
   (out 0 (add s (delay1 s)))))

