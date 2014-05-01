#lang racket

#|
tracks:

0 - output left speaker
1 - output right speaker
3 - first track

## example usage

; add reverb to track 3
(append-effect track3 (reverb-effect 0.5 0.9))

(append-effect track3 (reverb-effect 0.5 0.9))

; done internally:
; (process-track 3)

(define my-piano (make-instrument "piano"))

(note-on my-piano C#3 track3)

|#
 
(define effects-list '())



;; adds an effect to the end of the effect chain of the given track
(define (append-effect track-num effect-ugen)
  ; TODO
  empty)

;; creates a ugen that is routed
;; in-bus: input bus channel
;; out-bus: output bus channel
(define (make-effect-ugen effect-ugen in-bus out-bus)
  ; TODO
  empty)

;; example effect ugen
#;(audition (out 0 (free-verb (in 1 ar 16) 0.5
                            (mouse-y kr 0 1 0 0.1)
                            0.5)))

(define (make-freeverb-with-in-out in-bus out-bus)
  (out out-bus (free-verb (in 1 ar in-bus) 0.5
                          (mouse-y kr 0 1 0 0.1)
                          0.5)))



