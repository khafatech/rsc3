#lang racket

(require oregano)



(define my-note (play-note "sin-inst" 440))
(sleep 1)
;; stop playing note
(note-off my-note)



(define my-note2 (make-note "sin-inst" 880))
(note-on my-note2)
(sleep 1)
(note-off my-note2)
