#lang racket

(require rsc3 rhs/rhs)


(provide load-sample
         play-sample)


(struct sample (buf len))

(define current-buf-id 10)
(define (gen-buf-id)
  (set! current-buf-id (add1 current-buf-id))
  (sub1 current-buf-id))

;; path -> sample
(define (load-sample path)
  (if (not (file-exists? path))
      (display (format "Error: The file \"~a\" does not exist\n" path))
      (let ([id (gen-buf-id)])
        (send-async-msg (b-alloc-read id path 0 0))
        (sample id 0))))
  

(define sample-player-ugen
  (letc ([buf 0]
         [bus 0]
         [rate 1])
        (out bus (play-buf 2 buf rate 1 0 0))))
  

(define (play-sample smpl [rate 1])
  (send-msg (s-new2 "sample-player" -1 1 1
                    "buf" (sample-buf smpl)
                    "rate" rate)))

;; send sample-player synthdef
(with-sc3 (lambda (fd)
              (send-synth fd "sample-player" sample-player-ugen)))





      