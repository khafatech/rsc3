;; (Grainin nc tr dur in pan envbuf)

;; Granulate an input signal

;; nc - the number of channels to output. If 1, mono is
;;      returned and pan is ignored.

;; tr - a kr or ar trigger to start a new grain. If ar, grains
;;      after the start of the synth are sample accurate.

;; The following args are polled at grain creation time

;; dur - size of the grain.

;; in - the input to granulate

;; pan - a value from -1 to 1. Determines where to pan the output in
;;       the same manner as pan-az.

;; envbuf - the buffer number containing a singal to use for the
;;          grain envelope. -1 uses a built-in Hanning envelope.

(let* ((x (mouse-x kr -0.5 0.5 0 0.1))
       (y (mouse-y kr 5 25 0 0.1))
       (n (pink-noise ar))
       (t (impulse kr y 0)))
  (audition (out 0 (mul (grain-in 2 t 0.1 n x -1) 0.1))))
