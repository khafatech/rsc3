#lang scribble/manual
@(require (for-label racket))

@title{(freq-shift input shift phase)}

freq-shift implements single sideband amplitude modulation, also
known as frequency shifting, but not to be confused with pitch
shifting.  Frequency shifting moves all the components of a signal
by a fixed amount but does not preserve the original harmonic
relationships.

input - audio input
shift - amount of shift in cycles per second
phase - phase of the frequency shift (0 - 2pi) 

shifting a 100Hz tone by 1 Hz rising to 500Hz

(let ((i (sin-osc ar 100 0))
      (s (x-line kr 1 500 5 remove-synth)))
  (audition (out 0 (mul (freq-shift i s 0) 0.1))))

shifting a complex tone by 1 Hz rising to 500Hz

(let ((i (klang ar 1 0 (klang-data (list 101 303 606 808) 
				   (replicate 4 1)
				   (replicate 4 1))))
      (s (x-line kr 1 500 5 remove-synth)))
  (audition (out 0 (mul (freq-shift i s 0) 0.1))))

modulating shift and phase

(let ((i (sin-osc ar 10 0))
      (s (mul (lf-noise2 ar 0.3) 1500))
      (p (lin-lin (sin-osc ar 500 0) -1 1 0 (* 2 pi))))
  (audition (out 0 (mul (freq-shift i s p) 0.1))))

shifting bandpassed noise

(let ((i (bpf (white-noise ar) 1000 0.001))
      (s (mul (lf-noise0 ar 5.5) 1000)))
  (audition (out 0 (mul (freq-shift i s 0) 32))))

