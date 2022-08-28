#lang scribble/manual
@(require (for-label racket))

@title{convolution2}



@racketblock[
(with-sc3
 (lambda (fd)
   (for-each
    (lambda (b)
      (async fd (b-alloc b 2048 1)))
    (list 10 11 12))
   (for-each
    (lambda (n)
      (send fd (b-set1 10 (+ (* 400 n) 100) 1)))
    (enumFromTo 0 2))
   (for-each
    (lambda (n)
      (send fd (b-set1 11 (+ (* 20 n) 10) (s:rand 0 1))))
    (enumFromTo 0 49))
   (for-each
    (lambda (n)
      (send fd (b-set1 12 (+ (* 40 n) 20) 1)))
    (enumFromTo 0 19))
   (send-synth
    fd "c"
    (letc ((k 0) (t 0))
      (let ((i (impulse 1 0)))
	(out 0 (mul (convolution2 i k t 2048) 0.5)))))))
]


@racketblock[
(define send-to
  (lambda (m)
    (with-sc3
     (lambda (fd)
       (send fd m)))))
]


@racketblock[
(define async-to
  (lambda (m)
    (with-sc3
     (lambda (fd)
       (async fd m)))))
]


@racketblock[
(send-to (s-new1 "c" 1001 1 1 "k" 10))
]


@racketblock[
(send-to (n-set1 1001 "k" 11))
]

@racketblock[
(send-to (n-set1 1001 "t" 0))
]

@racketblock[
(send-to (n-set1 1001 "t" 1))
]


@racketblock[
(send-to (n-set1 1001 "k" 12))
]

@racketblock[
(send-to (n-set1 1001 "t" 0))
]

@racketblock[
(send-to (n-set1 1001 "t" 1))
]


@racketblock[
(async-to (b-zero 12))
]


@racketblock[
(for-each
 (lambda (n)
   (send-to (b-set1 12 (+ (* 20  n) 10)  1)))
 (enumFromTo 0 39))
]


@racketblock[
(send-to (n-set1 1001 "t" 0))
]

@racketblock[
(send-to (n-set1 1001 "t" 1))
]

with soundfile


@racketblock[
(async-to (b-alloc-read 10 "/home/rohan/data/audio/metal.wav" 0 0))
]


@racketblock[
(let ((i (sound-in 0)))
  (mul (convolution2 i 10 0 512) 0.5))
]


