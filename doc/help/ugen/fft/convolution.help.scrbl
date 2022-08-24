#lang scribble/manual
@(require (for-label racket))

@title{(convolution in kernel framesize)}

Strict convolution of two continuously changing inputs. Also see
[convolution2] for a cheaper CPU cost alternative for the case of a
fixed kernel which can be changed with a trigger message.

in        - processing target
kernel    - processing kernel.
framesize - size of fft frame, must be a power of two

(audition
 (let ((input (sound-in (mce2 0 1)))
       (kernel (white-noise ar)))
   (out 0 (mul (convolution input kernel 2048) 0.1))))

(let ((a 2048)
      (b 0))
  (with-sc3
   (lambda (fd)
     (async fd (b-alloc b a 1))
     (send fd (b-set1 b 0 1.0))
     (replicate-m 100 (send fd (b-set1 b (random-integer a) (random 0.0 1.0))))
     (play fd (out 0 (mul (convolution 
			   (sound-in (mce2 0 1)) 
			   (play-buf 1 b (buf-rate-scale kr b) 1 0 1)
			   (* 2 a))
			  0.2))))))

