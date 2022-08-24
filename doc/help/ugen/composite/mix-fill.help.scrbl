#lang scribble/manual
@(require (for-label racket))

@title{(mix-fill n f)

(let ((n 6)
      (o (lambda (_) (mul (f-sin-osc ar (rand 200 700) 0) 0.1))))
  (audition (out 0 (mix-fill n o))))}


