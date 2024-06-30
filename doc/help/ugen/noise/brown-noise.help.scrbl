#lang scribble/manual
@(require (for-label racket))

@title{brown-noise}

@defproc[(brown-noise [r rate]) noise?]{Allocate buffer space.}

Generates noise whose spectrum falls off in power by 6 dB per
octave.


@racketblock[
(audition (out 0 (mul (brown-noise ar) 0.1)))
]
