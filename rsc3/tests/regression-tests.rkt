#lang racket

;; regression tests for rsc3/main.rkt

(require rsc3
         rackunit)


;; name, ugen expr -> bytes (synthdef)
;; similar to send-synth in rsc3
(define (ugens->synthdef name ugens)
  (encode-graphdef (synthdef name ugens)))


;; these should not break

(check-equal? (ugens->synthdef "sine" (mul (sin-osc ar 440 0) 0.1))
              (bytes-append
               #"SCgf\0\0\0\0\0\1\4sine\0\3C\334\0\0\0\0\0\0=\314\314\315"
               #"\0\0\0\0\0\2\6SinOsc\2\0\2\0\1\0\0\377\377\0\0\377\377\0\1\2"
               #"\fBinaryOpUGen\2\0\2\0\1\0\2\0\0\0\0\377\377\0\2\2"))

(check-equal? (ugens->synthdef "sine0" (out 0 (mul (sin-osc ar 440 0) 0.1)))
              (bytes-append
               #"SCgf\0\0\0\0\0\1\5sine0\0\3\0\0\0\0C\334\0\0=\314\314\315"
               #"\0\0\0\0\0\3\6SinOsc\2\0\2\0\1\0\0\377\377\0\1\377\377\0\0\2"
               #"\fBinaryOpUGen\2\0\2\0\1\0\2\0\0\0\0\377\377\0\2\2\3Out\2\0\2"
               #"\0\0\0\0\377\377\0\0\0\1\0\0"))

(check-equal? (ugens->synthdef "ring" (out 0 (mul (ring4 (f-sin-osc ar 800 0)
                                                         (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0))
                                                  0.125)))
              (bytes-append
               #"SCgf\0\0\0\0\0\1\4ring\0\6\0\0\0\0DH\0\0CH\0\0C\372\0\0@\240\0\0>"
               #"\0\0\0\0\0\0\0\0\6\5XLine\1\0\4\0\1\0\0\377\377\0\2\377\377\0\3"
               #"\377\377\0\4\377\377\0\0\1\aFSinOsc\2\0\2\0\1\0\0\0\0\0\0\377\377"
               #"\0\0\2\aFSinOsc\2\0\2\0\1\0\0\377\377\0\1\377\377\0\0\2\fBinaryOpUGen"
               #"\2\0\2\0\1\0!\0\2\0\0\0\1\0\0\2\fBinaryOpUGen\2\0\2\0\1\0\2\0\3\0\0\377\377"
               #"\0\5\2\3Out\2\0\2\0\0\0\0\377\377\0\0\0\4\0\0"))

