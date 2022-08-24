#lang scribble/manual
@(require (for-label racket))

@title{(slew in up dn)}

Has the effect of removing transients and higher frequencies.

(audition
 (out 0 (slew (mul (saw ar 800) 0.2) 400 400)))

