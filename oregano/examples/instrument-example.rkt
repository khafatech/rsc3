#lang racket

(require oregano)


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