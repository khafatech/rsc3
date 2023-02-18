#lang scribble/manual
@(require (for-label racket))

@title{rand-id}


graphs (r & s) to generate noise in the context of a given RNG and to reset a specified RNG

@racketblock[
(with-sc3
 (lambda (fd)
   (send-synth
    fd "r"
    (letc ((bus 0) (id 1))
      (mrg2 (rand-id id)
            (out bus (add (mul (white-noise) 0.05) (dust2 70))))))
   (send-synth
    fd "s"
    (letc ((seed 1910) (id 1))
      (mrg2 (rand-id id)
            (rand-seed (impulse (mul-add (f-sin-osc 0.2 0) 10 11) 0) seed))))))
]

start synths on left and right channel with a different randgen ids

@racketblock[
(with-sc3
 (lambda (fd)
   (send fd (s-new2 "r" 1001 1 1 "bus" 0 "id" 1))
   (send fd (s-new2 "r" 1002 1 1 "bus" 1 "id" 2))))
]

reset the seed of randgen 1

@racketblock[
(with-sc3 (lambda (fd) (send fd (s-new1 "s" 1003 1 1 "id" 1))))
]

change the target RNG with ID 2, ie. effect right channel

@racketblock[
(with-sc3 (lambda (fd) (send fd (n-set1 1003 "id" 2))))
]

free nodes

@racketblock[
(with-sc3
 (lambda (fd)
   (send fd (n-free1 1001))
   (send fd (n-free1 1002))
   (send fd (n-free1 1003))))
]


