;; (moog-ff in freq gain reset)

;; Moog VCF implementation, designed by Federico Fontana. A digital
;; implementation of the Moog VCF (filter).

;; in - the input signal
;; freq - the cutoff frequency
;; gain - the filter resonance gain, between zero and 4
;; reset - when greater than zero, this will reset the 
;;         state of the digital filters at the beginning 
;;         of a computational block.

;; The design of this filter is described in the conference paper
;; Fontana, F. (2007) Preserving the Digital Structure of the Moog
;; VCF. in Proc. ICMC07, Copenhagen, 25-31 August 2007

(let ((n (mul (white-noise ar) 0.1))
      (y (mouse-y kr 100 10000 1 0.1))
      (x (mouse-x kr 0 4 0 0.1)))
  (audition (out 0 (moog-ff n y x 0))))

(let* ((p (pulse ar (mce2 40 121) (mce2 0.3 0.7)))
       (f0 (lin-lin (lf-noise0 kr 0.43) -1 1 0.001 2.2))
       (f1 (lin-lin (sin-osc kr f0 0) -1 1 30 4200))
       (y (mouse-y kr 1 4 0 0.1)))
  (audition (out 0 (moog-ff p f1 (mul 0.83 y) 0))))
