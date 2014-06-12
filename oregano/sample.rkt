#lang racket

(require rsc3 rhs/rhs)

(reset)
(send-msg (dump-osc 1))

;; load a smple in a buffer
; (send-async-msg (b-alloc-read 42 "./Samples/Synth Hits/Synth Hit 01.wav" 0 0))

(define fname "./Samples/Synth Hits/Synth Hit 01.wav")

#|
(send-async-msg (b-alloc 0 16380 2))
(send-async-msg (b-read 0 fname 0 -1 0 1))

(audition (out 0 (disk-in 2 ar 0)))
|#

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
  
(with-sc3 (lambda (fd)
              (send-synth fd "sample-player" sample-player-ugen)))


(define (play-sample smpl [rate 1])
  (send-msg (s-new2 "sample-player" -1 1 1
                    "buf" (sample-buf smpl)
                    "rate" rate)))




#;(with-sc3 (lambda (fd)
            (send-synth fd "sample-player"
                        (letc ([buf 0]
                               [bus 0])
                              (out bus (disk-in 2 ar buf))))))

#|

; make a synth and free it.
(with-sc3 (lambda (fd)
              (send-synth fd "sample-player" sample-player-ugen)))

; (send-async-msg (b-alloc-read 3 "./Samples/Synth Hits/Synth Hit 01.wav" 0 0))
(send-async-msg (b-alloc-read 3 "/home/pack/bass2.wav" 0 0))

(send-msg (s-new1 "sample-player" 1001 1 1  "buf" 3))


> (audition (out 0 (disk-in 2 ar 3)))

---
;; more civil
(define s (load-sample "/home/pack/bass2.wav"))

(play-sample s)



|#


      