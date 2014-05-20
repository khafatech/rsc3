#lang racket

(require rsc3 rhs/rhs)


(with-sc3 reset)

(send-msg (dump-osc 1))

;; load a smple in a buffer
; (send-async-msg (b-alloc-read 42 "./Samples/Synth Hits/Synth Hit 01.wav" 0 0))

(define fname "./Samples/Synth Hits/Synth Hit 01.wav")

(send-async-msg (b-alloc 0 16380 2))
(send-async-msg (b-read 0 fname 0 -1 0 1))

(audition (out 0 (disk-in 2 ar 0)))


