#lang racket

(require oregano)

;; press 'a'
(make-instrument "my-inst" ([freq 500])
                 (mul (sin-osc ar (mul-add (lf-pulse ar 15 0 0.5) 200 freq) 0)
                      (mouse-button kr 0 0.1 0.1)))

(define phone-note (play-note "my-inst" 600))

(define track0 0)

(sleep 1)

(reverb track0 0.5)

(sleep 3)


(moog-filter track0 800)