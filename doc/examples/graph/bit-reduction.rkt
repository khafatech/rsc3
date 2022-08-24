#lang racket
;; status - working as of 20220820

(require rsc3)

;; bit reduction (adc)

(let* ((f (lf-noise2 kr 8))
       (nh (lf-noise2 kr 3))
       (src (blip ar (mul-add f 200 300) (mul-add nh 10 20)))
       (sr (mouse-x* kr 1000 (mul sample-rate 0.1) 1 0.2)))
  (audition (out 0 (latch src (impulse ar sr 0)))))

(let* ((f (lf-noise2 kr 8))
       (nh (lf-noise2 kr 3))
       (src (blip ar (mul-add f 200 300) (mul-add nh 10 20)))
       (sr (mouse-x* kr 1000 (mul sample-rate 0.1) 1 0.2))
       (bit-sz (mouse-y* kr 1 24 1 0.2))
       (down-sample (latch src (impulse ar sr 0)))
       (bit-redux (u:round down-sample (pow 0.5 bit-sz))))
  (audition (out 0 (mce2 down-sample bit-redux))))
