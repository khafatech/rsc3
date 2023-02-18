;; (grain-fm nc tr dur carfreq modfreq index pan envbuf)

;; Granular synthesis with frequency modulated sine tones

;; nc - the number of channels to output. If 1, mono is returned and
;;      pan is ignored.

;; tr - a kr or ar trigger to start a new grain. If ar, grains after
;;      the start of the synth are sample accurate.

;; The following args are polled at grain creation time

;; dur - size of the grain.

;; carfreq - the carrier freq of the grain generators internal
;;           oscillator

;; modfreq - the modulating freq of the grain generators internal
;;           oscillator

;; index - the index of modulation

;; pan - a value from -1 to 1. Determines where to pan the output in
;;       the same manner as pan-az.

;; envbuf - the buffer number containing a singal to use for the grain
;;          envelope. -1 uses a built-in Hanning envelope.

(let* ((x (mouse-x kr -0.5 0.5 0 0.1))
       (y (mouse-y kr 0 400 0 0.1))
       (n (white-noise kr))
       (fd (add 440 (mul n y)))
       (t (impulse kr 10 0))
       (i (lin-lin (lf-noise1 kr 500) -1 1 1 10)))
  (audition (out 0 (mul (grain-fm 2 t 0.1 fd 200 i x -1) 0.1))))
