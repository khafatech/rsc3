#lang scribble/manual
@(require (for-label racket))

@title{(mrg2 left right)}

mrg2 defines a node indicating a multiple root graph.

(let ((l (out 0 (mul (sin-osc ar 300 0) 0.1)))
      (r (out 1 (mul (sin-osc ar 900 0) 0.1))))
  (audition
   (mrg2 l r)))

there is a leftmost rule, so that mrg nodes need not
be terminal.

(let ((l (mul (sin-osc ar 300 0) 0.1))
      (r (out 1 (mul (sin-osc ar 900 0) 0.1))))
  (audition (out 0 (mrg2 l r))))

the leftmost node may be an mce node

(let ((l (mul (sin-osc ar (mce2 300 400) 0) 0.1))
      (r (out 1 (mul (sin-osc ar 900 0) 0.1))))
  (audition (out 0 (mrg2 l r))))

the implementation is not thorough

(let ((l (mul (sin-osc ar (mce2 300 400) 0) 0.1))
      (r (out 1 (mul (sin-osc ar 900 0) 0.1))))
  (audition (out 0 (add (mrg2 l r)
                        (mrg2 l r)))))

