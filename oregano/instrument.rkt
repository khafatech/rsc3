#lang racket


(require rsc3 "system.rkt")

(provide
 (all-from-out rsc3))

(provide (all-defined-out))

;; TODO - remove. using "signal-slider" for testing
(require "gui.rkt")

;(run-super-collider)


(define current-node-id 1000)
(define (gen-node-id)
  (set! current-node-id (add1 current-node-id))
  current-node-id)

(define custom-synth-num 1)
(define (gen-synth-name)
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
        ;(out bus (mul (signal-slider "amplitude" 100 800 200) (sin-osc ar freq 0)))))
        (out bus (mul 0.2 (sin-osc ar freq 0)))))

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

;; other way
#;(define (make-instrument name graph)
    (let ([sd (letc ([bus 0])
                    (out bus ugen))]
          ;[name (format "synth~a" current-node-id)]
          )
      (with-sc3 (lambda (fd)
                  (send-synth fd name sd)))    
      ))

(define-syntax-rule (make-instrument inst-name ([argname argdefault] ...) ugen)
  (let ([sd (letc ([bus 0]
                   [argname argdefault] ...)
                  (out bus ugen))]
        ;[name (format "synth~a" current-node-id)]
        )
    (with-sc3 (lambda (fd)
                (send-synth fd inst-name sd)))))

;; example of definst macro like overtone's definst
#;(define-syntax-rule (define-instrument inst-name [[argname argdefault] ...] ugen)
    (define (inst-name [argname argdefault] ...)
      (+ argname ...)))

(define perset-instrument-map
  `(("sin-inst" ,sin-instrument)
    ("saw-inst" ,saw-instrument)
    ("moog-inst" ,moog-instrument)))

;; send synthdefs
(for ([pair perset-instrument-map])
  (with-sc3 (lambda (fd)
              (send-synth fd (first pair) (second pair)))))





; === user note funcs ===

(struct note (id [freq #:mutable]) #:transparent)

(define (create-synth name play-on-start)
  (define node-id (gen-node-id))
  (send-msg (s-new0 name node-id 1 1))
  ;; TODO - send in bundle. it run 0 may be sent after
  ; don't make sound upon creation
  (if play-on-start
      empty
      (send-msg (n-run1 node-id 0)))
  node-id)


(define (make-note/option inst-name freq play-on-start)
  (define id (create-synth inst-name play-on-start))
  (send-msg (n-set1 id "freq" freq))
  (note id freq))

(define (play-note inst-name freq)
  (make-note/option inst-name freq #t))

(define (make-note inst-name freq)
  (make-note/option inst-name freq #f))



(define (note-on the-note)
  ;(send-msg (n-set1 inst "bus" track)) ; TODO
  (send-msg (n-run1 (note-id the-note) 1))
  (send-msg (n-set1 (note-id the-note) "freq" (note-freq the-note)))
  (void))

(define (note-off the-note)
  (send-msg (n-run1 (note-id the-note) 0))
  (void))

(define (delete-note the-note)
  (send-msg (n-free (note-id the-note)))
  (void))
  

(define (set-note-param the-note name val)
  (if (eq? name "freq")
      (set-note-freq! the-note val)
      empty)
  (send-msg (n-set1 (note-id the-note) name val))
  (void))


(define (mouse/x start end)
  (mouse-x kr start end 0 0.1))

(define (mouse/y start end)
  (mouse-y kr start end 0 0.1))


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



; (synth-on my-sin 500 1)

; (synth-off my-sin)


(note "weird-synth" 400)

(play-note-at 50 mynote)

(envelope A S D R)


|#