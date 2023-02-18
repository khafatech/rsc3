#lang racket

; 2023-2-18
; this demonstrates modulating a pink noise signal using a bandpass filter
; Pair-programmed by f00f & naltroc (http://naltroc.live/)
;
;
; >>> "Don't get drowned out by the noise of the world, let it inspire you!" --naltroc

(require oregano)


#(run-super-collider)


;; this plays the key 
; (define my-note (play-note "sin-inst" 440))
;(sleep 1)


(make-instrument "my-pink" ([freq 500] [mod 20] [amp 1] [band 1])
  (let ([pink (mul (pink-noise ar) amp)])
   (bpf pink freq band)
        ))

(define my-note (play-note "my-pink" 440))

;(sleep 1)

;; stop playing note
;(note-off my-note)


(define k (load-sample "E:\\src\\rsc3\\oregano\\samples\\bass.wav"))


(for/list ([i (range 1000)])
  (let ([the-amp (if (even? i) 1 0.5)])
    (set-note-param my-note "amp" the-amp)
    (set-note-param my-note "freq" (if (even? i) 333 999))
    (sleep 0.5)
    (play-sample k)))

(stop)