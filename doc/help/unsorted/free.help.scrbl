#lang scribble/manual
@(require (for-label racket))

@title{free
(withSc3
 (lambda (fd)
  (sendSynth fd "a" (Out 0 (Mul (SinOsc 800 0) 0.1)))
  (sendSynth fd "b" (Mrg2 (Out 1 (Mul (PinkNoise) 0.1)) (Free (Dust 6) 1001)))
  (sendMessage fd (s_new0 "a" 1001 0 0))
  (sendMessage fd (s_new0 "b" -1 0 0))))

(withSc3 reset)}


