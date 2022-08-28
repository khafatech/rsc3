#lang scribble/manual
@(require (for-label racket))

@title{(mouse-button rate minval maxval lag)}


Report the status of the first pointer button.  The button is
either pressed, or not pressed.


@racketblock[
(audition
 (out 0 (mul (sin-osc ar 800 0)
	     (mouse-button kr 0 0.1 0.1))))
]


