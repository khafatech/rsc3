#lang racket


(require "instrument.rkt"
         "gui.rkt"
         "sample.rkt")

;; setup
(require rsc3)

;; TODO - run scsynth

;; show osc messages on server
(send-msg (dump-osc 1))
(with-sc3 reset)
(sleep 0.1)


(provide
 (all-from-out "instrument.rkt"
               "gui.rkt"
               "sample.rkt"))
 

