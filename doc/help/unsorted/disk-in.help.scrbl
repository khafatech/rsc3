#lang scribble/manual
@(require (for-label racket))

@title{disk-in

(let ((f "/home/rohan/data/audio/metal.wav")
      (nc 1))
  (with-sc3
   (lambda (fd)
     (async fd (b-alloc 0 8192 nc))
     (async fd (b-read 0 f 0 -1 0 1))
     (play fd (out 0 (disk-in nc 0 0))))))

(with-sc3 reset)

(with-sc3
 (lambda (fd)
   (async fd (b-close 0))
   (async fd (b-free 0))))}


