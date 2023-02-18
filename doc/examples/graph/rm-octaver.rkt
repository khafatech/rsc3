#lang racket
;; status - not working as of 20220820 (no sound. mce-channel?)

(require rsc3)

;; rm-octaver (andrea valle, miller puckette)

(let* ((default-pitch
	 (lambda (i)
	   (pitch i 440 60 4000 100 16 1 0.01 0.5 1)))
       (i (sound-in 4))
       (p (default-pitch i))
       (f (mce-channel p 0)))
  (audition (out 0 (mul-add (sin-osc ar (mul f 0.5) 0) i i))))
