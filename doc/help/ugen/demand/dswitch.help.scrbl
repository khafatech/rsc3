#lang scribble/manual
@(require (for-label racket))

@title{(dswitch index array)}

demand rate generator for switching between inputs

index - which of the inputs to return
array - array of values or other ugens

In difference to dswitch1, dswitch embeds all items
of an input demand ugen first before looking up the
next index.

(let* ((a0 (dwhite 2 3 4))
       (a1 (dwhite 2 0 1))
       (a2 (dseq 2 (make-mce (list 1 1 1 0))))
       (i (dseq 2 (make-mce (list 0 1 2 1 0))))
       (d (dswitch i (make-mce (list a0 a1 a2))))
       (t (impulse kr 4 0))
       (f (mul-add (demand t 0 d) 300 400))
       (o (mul (sin-osc ar f 0) 0.1)))
  (audition (out 0 o)))

compare with dswitch1

(let* ((a0 (dwhite 2 3 4))
       (a1 (dwhite 2 0 1))
       (a2 (dseq 2 (make-mce (list 1 1 1 0))))
       (i (dseq 2 (make-mce (list 0 1 2 1 0))))
       (d (dswitch1 i (make-mce (list a0 a1 a2))))
       (t (impulse kr 4 0))
       (f (mul-add (demand t 0 d) 300 400))
       (o (mul (sin-osc ar f 0) 0.1)))
  (audition (out 0 o)))

