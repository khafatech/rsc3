#lang racket

(require oregano)

(display "start\n")
(define my-note (play-note "sin-inst" 440))
(sleep 1)
;; stop playing note
(note-off my-note)
(display "end note1\n")

(define my-note2 (play-note "saw-inst" 880))
(sleep 2)
(note-off my-note2)
(display "end note2\n")

(sleep 1)

(define my-note3 (play-note "sin-inst" 500))
(sleep 1)
;; stop playing note
(note-off my-note3)


#;(
(define my-note2 (make-note "sin-inst" 880))
(note-on my-note2)
(sleep 1)
(note-off my-note2)
)

#|
(make-instrument "my-inst" ([freq 500] [mod 20])
  (mul (sin-osc ar mod 0)
       (sin-osc ar freq 0)))
(define weird-note (play-note "my-inst" 440))

(param-slider "change frequency" 1  100 40
                (lambda (val)
                  (set-note-param weird-note "mod" val)))

(show-gui)

|#