#lang racket


(require rsc3 rhs/rhs)

(define sin-instrument
  (letc ([bus 0]
         [freq 440])
        (out bus (mul 0.2 (sin-osc ar freq 0)))))


;; setup
(send-synth fd "sin-inst" sin-instrument)


  
(define (make-instrument ins)
  (match ins
    ['sin (begin
            (with-sc3 (lanbda (fd)
                              (send fd
                                    (s-new "sin-inst" -1 1 1
                                                            
                              ]
    [else (error "unknown instrument used")]))h