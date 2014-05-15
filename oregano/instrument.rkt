#lang racket


(require rsc3 rhs/rhs)




;; simplifies sending messages to server
(define (send-msg msg)
  (with-sc3 (lambda (fd)
            (send fd msg))))

(define current-node-id 1000)
(define (gen-node-id)
  (set! current-node-id (add1 current-node-id))
  current-node-id)


(define (instrument wave-func)
  (letc ([bus 0]
         [freq 440])
        (out bus (mul 0.2 (wave-func ar freq 0)))))
  
(define sin-instrument
  (letc ([bus 0]
         [freq 440])
        (out bus (mul 0.2 (sin-osc ar freq 0)))))

(define saw-instrument
  (letc ([bus 0]
         [freq 440])
        (out bus (mul 0.2 (sin-osc ar freq 0)))))


;; setup
;; show osc messages on server
(with-sc3 (lambda (fd)
            (send fd (dump-osc 1))))
(with-sc3 reset)

;; send synthdefs
(with-sc3 (lambda (fd)
                  (send-synth fd "sin-inst" sin-instrument)))


(define (make-instrument ins)
  (match ins
    ['sin (let ([node-id (gen-node-id)])
            (send-msg (s-new0 "sin-inst" node-id 1 1))
            ; don't make sound upon creation
            (send-msg (n-run1 node-id 0))
            node-id)]
    [else (error "unknown instrument used")]))

#|

- to stop/run:
  (send-msg (n-run1 1001 1))

-



|#

;; ======== test run ===========

(define my-sin (make-instrument 'sin))




