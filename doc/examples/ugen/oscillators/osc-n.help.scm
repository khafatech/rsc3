;; (osc-n rate bufnum freq phase)

;; Noninterpolating wavetable lookup oscillator with frequency and
;; phase modulation inputs.  It is usually better to use the
;; interpolating oscillator.

;; The buffer size must be a power of 2.  The buffer should NOT be
;; filled using Wavetable format (b_gen commands should set wavetable
;; flag to false.  

