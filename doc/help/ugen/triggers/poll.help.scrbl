#lang scribble/manual
@(require (for-label racket))

@title{(poll trig in trigid label)}

Print/query the current output value of a UGen.

trig - a non-positive to positive transition telling poll to return
       a value

in - the signal you want to poll

trigid - if greater then 0, a '/tr' message is sent back to the
         client (similar to send-trig)

label - a string or symbol to be printed with the polled value

poll returns its in signal (and is therefore transparent).
WARNING: Printing values from the Server in intensive for the
CPU. poll should be used for debugging purposes.

(define (string->ugen s)
  (make-mce 
   (cons (string-length s)
	 (map char->integer (string->list s)))))

(let ((t (impulse kr 2 0))
      (i (line kr 0 1 5 remove-synth)))
  (audition (poll t i 0 (string->ugen "Test"))))

(with-sc3
 (lambda (fd)
   (letrec ((print (lambda (e) (display e) (newline)))
	    (showing (lambda (f) (lambda () (let ((v (f))) (print v) v))))
	    (repeat (lambda (f) (if (f) (repeat f) #f))))
     (async fd (/notify 1))
     (repeat (showing (lambda () (wait fd "/tr"))))
     (async fd (/notify 0)))))

multichannel Expansion (Broken...)

(define (poll* trig in trigId label)
  (poll trig in trigId (string->ugen label)))

(poll* (impulse kr (mce2 10 5) 0)
       (line kr 0 (mce2 1 5) (mce2 1 2) do-nothing)
       0
       "Test")

(with-sc3 server-status)

