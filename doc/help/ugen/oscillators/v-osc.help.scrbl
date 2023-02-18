#lang scribble/manual
@(require (for-label racket))

@title{(v-osc rate bufpos freq phase)}


Variable wavetable oscillator.  A wavetable lookup oscillator which
can be swept smoothly across wavetables. All the wavetables must be
allocated to the same size. Fractional values of table will
interpolate between two adjacent tables.

This oscillator requires a buffer to be filled with a wavetable
format signal.  This preprocesses the Signal into a form which can
be used efficiently by the oscillator.  The buffer size must be a
power of 2.

This can be acheived by creating a Buffer object and sending it one
of the "b_gen" messages ( sine1, sine2, sine3 ) with the wavetable
flag set to true.

This can also be acheived by creating a Signal object and sending
it the 'asWavetable' message, saving it to disk, and having the
server load it from there.

Note about wavetables: Vosc requires the b_gen sine1 wavetable flag
to be ON.

Allocate and fill tables 0 to 7 [see also Vosc3]


@racketblock[
(with-sc3
 (lambda (fd)
   (let* ((square
	   (lambda (a) (* a a)))
	  (nth
	   (lambda (i)
	     (async fd (b-alloc i 1024 1))
	     (let* ((n (expt (+ i 1) 2))
		    (a (map1 (lambda (j) (square (/ (- n j) n)))
			     (enum-from-to 0 (- n 1)))))
	       (async fd (b-gen1 i "sine1" (cons 7 a)))))))
     (for-each nth (enum-from-to 0 7)))))
]

oscillator at buffers 0 through 7.


@racketblock[
(let ((b (mouse-x kr 0 7 0 0.1))
      (f (mce2 120 121)))
  (audition (out 0 (mul (v-osc ar b f 0) 0.3))))
]

Reallocate buffers while oscillator is running.


@racketblock[
(with-sc3
 (lambda (fd)
   (for-each
    (lambda (i)
      (async fd (b-gen1  i "sine1" (cons 7 (replicate-m 16 (random 0 1))))))
    (enum-from-to 0 7))))
]


