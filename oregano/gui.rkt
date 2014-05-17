#lang racket

(require rsc3 rhs/rhs)
(require (prefix-in gui: racket/gui))

(provide (all-defined-out))

(define frame (new gui:frame% [label "Sliders"]))

(define (show-gui)
  (gui:send frame show #t))

(define (hide-gui)
  (gui:send frame show #f))


(define (param-slider name min-n max-n init-n func)
  (new gui:slider% [parent frame]
       [label name]
       [min-value min-n]
       [max-value max-n]
       [init-value init-n]
       ;; callback receives the slider object and an event object
       [callback (lambda (s event)
                   (func (gui:send s get-value)))])
  (func init-n))

#;(param-slider "change frequency" 300 1000 400
                (lambda (val)
                  (set-inst-param 1001 "freq" val)))

#;(new gui:slider% [parent frame]
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

(define (signal-slider name min-n max-n init)
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

(define (param-check-box name init-value func)
  (new gui:check-box%
       [label name]
       [parent frame]
       [value init-value]
       [callback (lambda (element event)
                   (func (gui:send element get-value)))]))
  
#;(new gui:check-box%
     [label "note-on"]
     [parent frame]
     [value #f]
     [callback (lambda (element event)
                   (if (gui:send element get-value)
                       (send-msg (n-run1 1001 1))
                       (send-msg (n-run1 1001 0))))])
                      

