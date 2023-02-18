;; (buf-delay-c buf in delaytime)

;; Buffer based simple delay line with cubic interpolation.

;; Simple delay line with cubic interpolation which uses a buffer for
;; its internal memory. See also buf-delay-n which uses no
;; interpolation, and buf-delay-l which uses linear interpolation. Cubic
;; interpolation is more computationally expensive than linear, but
;; more accurate.

;; See also delay-c.

;; buf       - buffer number.
;; in        - the input signal.
;; delaytime - delay time in seconds.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 0 44100 1))))

(let ((z (mul3 (decay (dust ar 1) 0.5) 0.3 (white-noise ar))))
  (audition (out 0 (add (buf-delay-c 0 z 0.2) z))))
