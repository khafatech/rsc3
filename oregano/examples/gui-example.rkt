#lang racket

(require oregano)


(display "in example.rkt")
;(sleep 0.1)

(define my-sin  (make-note "saw-inst" 880))


(param-slider "change frequency" 300 1000 400
                (lambda (val)
                  (set-note-param my-sin "freq" val)))

(param-check-box "synth on" #f
                 (lambda (v)
                   (if v
                       (note-on my-sin)
                       (note-off my-sin))))

(show-gui)

;; example:
(sleep 0.5)
; (note-on my-sin 500 1)

; (note-off my-sin)