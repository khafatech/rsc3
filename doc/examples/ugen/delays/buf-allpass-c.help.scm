;; (buf-allpass-c buf in delaytime decaytime)

;; Buffer based all pass delay line with cubic interpolation

;; All pass delay line with cubic interpolation which uses a buffer
;; for its internal memory. See also Bufallpass-n which uses no
;; interpolation, and Bufallpass-l which uses linear
;; interpolation. Cubic interpolation is more computationally
;; expensive than linear, but more accurate.

;; See also allpass-c.

;; buf - buffer number.

;; in - the input signal.

;; delaytime - delay time in seconds.

;; decaytime - time for the echoes to decay by 60 decibels. If this
;;             time is negative then the feedback coefficient will be
;;             negative, thus emphasizing only odd harmonics at an
;;             octave lower.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 0 44100 1))))

(let ((x (mul3 (decay (dust ar 1) 0.2) (white-noise ar) 0.5)))
  (audition (out 0 (buf-allpass-n 0 x 0.25 6))))
