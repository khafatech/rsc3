#lang racket

(require "instrument.rkt"
         "gui.rkt"
         "sample.rkt"
         "system.rkt")

;; setup
(require rsc3)

;; FIXME - not running
(display "Hello! from main.rkt\n")
(run-super-collider)


;; show osc messages on server
(send-msg (dump-osc 1))
(reset)
(sleep 0.1)

(provide
 (all-from-out "instrument.rkt"
               "gui.rkt"
               "sample.rkt"))
 

