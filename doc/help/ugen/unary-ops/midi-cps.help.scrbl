#lang scribble/manual
@(require (for-label racket))

@title{(midi-cps a)}


Convert MIDI note to cycles per second.


@racketblock[
(audition 
 (out 0 (mul (saw ar (midi-cps (line kr 24 108 10 remove-synth))) 0.2)))
]


