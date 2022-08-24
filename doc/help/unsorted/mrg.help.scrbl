#lang scribble/manual
@(require (for-label racket))

@title{mrg}

the implementation is not thorough (error...)
(let ((l (mul (sin-osc (mce2 300 400) 0) 0.1))
      (r (out 1 (mul (sin-osc 900 0) 0.1))))
  (add (mrg2 l r) (mrg2 l r)))

