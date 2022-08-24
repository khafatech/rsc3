#lang scribble/manual
@(require (for-label racket))

@title{poll
(with-sc3
 (lambda (fd)
   (letrec ((print (lambda (e) (display e) (newline)))
	    (showing (lambda (f) (lambda () (let ((v (f))) (print v) v))))
	    (repeat (lambda (f) (if (f) (repeat f) #f))))
     (async fd (notify 1))
     (repeat (showing (lambda () (wait fd "/tr"))))
     (async fd (notify 0)))))}


