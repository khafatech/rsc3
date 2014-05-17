#lang racket
(require rsc3 rhs/rhs)


;; --- gui stuff

;; TODO - put in another file.

(require (prefix-in gui: racket/gui))

(define frame (new gui:frame% [label "Sliders"]))
(gui:send frame show #t)

;; parent should be a frame
(new gui:slider% [parent frame]
     [label "freq"]
     [min-value 300]
     [max-value 1000]
     [init-value 400]
     ;; callback receives the slider object and an event object
     [callback (lambda (s event)
                   (send-msg (n-set1 1001 "freq" (gui:send s get-value))))])


;; hypothetical usage
#;(add-filter track2 (lpf #:resonance .3
                        #:cutoff (slider 300 800 500)))

(define (slider name min-n max-n init)
  ;; TODO - determine available bus
  (define bus-id 16)
  
  (new gui:slider% [parent frame]
     [label name]
     [min-value min-n]
     [max-value max-n]
     [init-value init]
     ;; callback receives the slider object and an event object
     [callback (lambda (s event)
                   (send-msg
                    (c-set1 bus-id (/ (gui:send s get-value) 1000))))])
  (send-msg (c-set1 bus-id (/ init 1000)))
  (in 1 kr bus-id))


(new gui:check-box%
     [label "note-on"]
     [parent frame]
     [value #f]
     [callback (lambda (element event)
                   (if (gui:send element get-value)
                       (send-msg (n-run1 1001 1))
                       (send-msg (n-run1 1001 0))))])
                      


; ------- end of gui stuff





;; simplifies sending osc messages to server
(define (send-msg msg)
  (with-sc3 (lambda (fd)
              (send fd msg))))

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
        (out bus (mul (slider "amplitude" 100 800 200) (sin-osc ar freq 0)))))

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

(define (preset-instrument name)
  (let ([node-id (gen-node-id)])
    (create-synth name node-id)))


(define (note-on inst freq track)
  (send-msg (n-set1 inst "freq" freq))
  (send-msg (n-set1 inst "bus" track))
  (send-msg (n-run1 inst 1)))

(define (note-off inst)
  (send-msg (n-run1 inst 0)))

#|

- to stop/run:
  (send-msg (n-run1 1001 1))

|#

;; ======== test run ===========

(define my-sin (preset-instrument "sin-inst"))


;; example:
(sleep 1)
; (note-on my-sin 500 1)

; (note-off my-sin)



