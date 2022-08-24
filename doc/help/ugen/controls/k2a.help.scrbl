#lang scribble/manual
@(require (for-label racket))

@title{(k2a in)}

Control rate to audio rate converter.

To be able to play a control rate UGen into an audio rate UGen,
sometimes the rate must be converted.  k2a converts via linear
interpolation.

in - input signal

(audition
 (out 0 (k2a (mul (white-noise kr) 0.3))))

(audition
 (out 0 (mce2 (k2a (mul (white-noise kr) 0.3))
	      (mul (white-noise ar) 0.3))))

(let* ((block-size 64) no ugen for this?
       (freq (mul (fdiv (mouse-x kr 0.1 40 1 0.1) block-size) sample-rate)))
  (audition
   (out 0 (mul (mce2 (k2a (lf-noise0 kr freq))
		     (lf-noise0 ar freq))
	       0.3))))

