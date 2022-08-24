#lang racket

;; (require rsc3)

(require (file "/Users/zzk/code/github-zzkt/rsc3/rsc3/main.rkt")
         (prefix-in sosc: (file "/Users/zzk/code/github-zzkt/rsc3/sosc/sosc.rkt")))

;; start udp SuperCollider server with: scsynth -u 57110

(with-sc3 (lambda (fd) (sosc:send fd (g-new1 1 add-to-tail 0))))

;; play sin wave
(audition (out 0 (mul (sin-osc ar 440 0) 0.1)))

;; reset
; (with-sc3 reset)



