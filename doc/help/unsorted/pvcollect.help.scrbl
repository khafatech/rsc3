#lang scribble/manual
@(require (for-label racket))

@title{pvcollect}



@racketblock[
(define no-op
  (lambda (m p _)
    (list m p)))
]


@racketblock[
(define rand-phase
  (lambda (m p _)
    (list m (rand 0 3.14))))
]


@racketblock[
(define noise-phase
  (lambda (m p _)
    (list m (lin-lin (lf-noise0 3) -1 1 0 3.14))))
]


@racketblock[
(define combf
  (lambda (m p i)
    (list (if (= (modulo i 7) 0) m 0) p)))
]


@racketblock[
(define noise-mag
  (lambda (m p _)
    (list (mul (gt (lf-noise0 10) 0) m) p)))
]


@racketblock[
(define spectral-delay
  (lambda (m p _)
    (let ((v (lin-lin (lf-par 0.5 0) -1 1 0.1 1)))
      (list (add m (delay-n m 1 v)) p))))
]


@racketblock[
(define (bpf-sweep nf)
  (lambda (m p i)
    (let ((e (u:abs (sub i (lin-lin (lf-par 0.1 0) -1 1 2 (/ nf 20))))))
      (list (mul (lt e 10) m) p))))
]


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 1024 1))
   (async fd (b-alloc-read 11 "/home/rohan/data/audio/metal.wav" 0 0))))
]


@racketblock[
(let* ((nf 1024)
       (i (play-buf 1 11 (buf-rate-scale 11) 1 0 loop do-nothing))
       (c1 (fft* 10 i))
       (c2 (pvcollect c1 nf spectral-delay 0 250 0)))
  (mul 0.1 (ifft* c2)))
]


