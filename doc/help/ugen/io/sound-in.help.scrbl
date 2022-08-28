#lang scribble/manual
@(require (for-label racket))

@title{(sound-in channel)}


Read audio from the sound input hardware.

channel - input channel number to read, 
          indexed from zero, can be mce.


@racketblock[
(audition (out 0 (sound-in 0)))
]


@racketblock[
(audition (out 0 (sound-in (mce2 0 1))))
]


@racketblock[
(audition (out 0 (sound-in (mce4 0 2 1 3))))
]


