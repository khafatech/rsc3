#lang scribble/manual
@(require (for-label racket))

@title{(cubed a)}

Cube.

(let* ((a (line ar -2 2 3 remove-synth))
       (b (cubed a))
       (f (mul-add (mce2 a b) 600 900)))
  (audition 
   (out 0 (mul (sin-osc ar f 0) 0.1))))

