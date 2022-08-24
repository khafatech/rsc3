#lang scribble/manual
@(require (for-label racket))

@title{pause
(with-sc3
 (lambda (fd)
   (send-synth fd "a" (out 0 (mul (sin-osc 800 0) 0.1)))
   (send-synth fd "b" (letc ((g 1)) (mrg2 (out 1 (mul (pink-noise) 0.05)) (pause g 1001))))
   (send fd (s-new0 "a" 1001 0 0))
   (send fd (s-new0 "b" 1002 0 0))
   (pause-thread 1)
   (send fd (n-set1 1002 "g" 0))
   (pause-thread 1)
   (send fd (n-set1 1002 "g" 1))
   (pause-thread 1)
   (reset fd)))}


