#lang racket


(require rsc3)


(provide (all-defined-out))

;; TODO - remove. using "signal-slider" for testing
(require "gui.rkt")


(define current-node-id 1000)
(define (gen-node-id)
  (set! current-node-id (add1 current-node-id))
  current-node-id)


(define (wave-instrument wave-func)
  (letc ([bus 0]
         [freq 440])
        (out bus (mul 0.2 (wave-func ar freq 0)))))
  
(define sin-instrument
  (letc ([bus 0]
         [freq 440])
        ;; TODO - remove slider
        (out bus (mul (signal-slider "amplitude" 100 800 200) (sin-osc ar freq 0)))))

(define saw-instrument
  (letc ([bus 0]
         [freq 440])
        (out bus (mul 0.2 (saw ar freq)))))

(define moog-instrument
  (letc ([bus 0]
         [freq 440])
        (out bus (moog-ff
                  (mul (saw ar freq) 0.1)
                  (mouse-y kr 200 30000 1 0.1) 3 0))))

;; TODO
(define (make-instrument graph)
  (letc ([bus 0])
        (out bus graph)))

;; setup
;; show osc messages on server
(send-msg (dump-osc 1))
(with-sc3 reset)

(define perset-instrument-map
  `(("sin-inst" ,sin-instrument)
    ("saw-inst" ,saw-instrument)
    ("moog-inst" ,moog-instrument)))

;; send synthdefs
(map (lambda (pair)
       (with-sc3 (lambda (fd)
                   (send-synth fd (first pair) (second pair)))))
     perset-instrument-map)

(define (create-synth name node-id)
  (send-msg (s-new0 name node-id 1 1))
  ; don't make sound upon creation
  (send-msg (n-run1 node-id 0))
  node-id)


; === user instrument funcs ===

(define (preset-instrument name)
  (let ([node-id (gen-node-id)])
    (create-synth name node-id)))


(define (note-on inst freq track)
  (send-msg (n-set1 inst "freq" freq))
  (send-msg (n-set1 inst "bus" track))
  (send-msg (n-run1 inst 1)))

(define (inst-on inst)
  (send-msg (n-run1 inst 1)))

(define (inst-off inst)
  (send-msg (n-run1 inst 0)))

(define (set-inst-param inst name val)
  (send-msg (n-set1 inst name val)))

#|

- to stop/run:
  (send-msg (n-run1 1001 1))

|#


;; ========  example useage ===========
#|
(define my-sin (preset-instrument "sin-inst"))

(param-slider "change frequency" 300 1000 400
                (lambda (val)
                  (set-inst-param my-sin "freq" val)))

(param-check-box "synth on" #f
                 (lambda (v)
                   (if v
                       (inst-on my-sin)
                       (inst-off my-sin))))

(show-gui)

;; example:
(sleep 0.5)
; (note-on my-sin 500 1)

; (note-off my-sin)

|#
