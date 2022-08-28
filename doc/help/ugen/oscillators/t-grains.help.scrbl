#lang scribble/manual
@(require (for-label racket))

@title{(t-grains numChannels trigger bufnum rate centerPos dur pan amp interp)}


Buffer granulator.  triggers generate grains from a buffer. Each
grain has a Hanning envelope (sin^2(x) for x from 0 to pi) and is
panned between two channels of multiple outputs.

numChannels - number of output channels.
	
trigger - at each trigger, the following arguments are sampled and
          used as the arguments of a new grain.  A trigger occurs
          when a signal changes from <= 0 to > 0.  If the trigger
          is audio rate then the grains will start with sample
          accuracy.

bufnum - the index of the buffer to use. It must be a one channel
         (mono) buffer.
	
rate - 1.0 is normal, 2.0 is one octave up, 0.5 is one octave down
       and -1.0 is backwards normal rate ... etc.  Unlike play-buf,
       the rate is multiplied by BufRate, so you needn't do that
       yourself.

centerPos - the position in the buffer in seconds at which the
            grain envelope will reach maximum amplitude.

dur - duration of the grain in seconds.

pan - a value from -1 to 1. Determines where to pan the output in
      the same manner as pan-az.

amp - amplitude of the grain.
	
interp - 1, 2, or 4. Determines whether the grain uses (1) no
         interpolation, (2) linear interpolation, or (4) cubic
         interpolation.


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 10 "/home/rohan/audio/metal.wav" 0 0))))
]


@racketblock[
(audition
 (let* ((b 10)
	(trate (mouse-y kr 2 200 1 0.1))
	(dur (fdiv 4 trate))
	(t (impulse ar trate 0))
	(i (mouse-x kr 0 (buf-dur kr b) 0 0.1)))
  (out 0 (t-grains 2 t b 1 i dur 0 0.1 2))))
]


@racketblock[
(audition
 (let* ((b 10)
	(trate (mouse-y kr 8 120 1 0.1))
	(dur (fdiv 12 trate))
	(clk (impulse ar trate 0))
	(x (mouse-x kr 0 (buf-dur kr b) 0 0.1))
	(pos (add x (t-rand 0 0.01 clk)))
	(pan (mul (white-noise kr) 0.6)))
   (out 0 (t-grains 2 clk b 1 pos dur pan 0.1 2))))
]


@racketblock[
(audition 
 (let* ((b 10)
	(trate (mouse-y kr 8 120 1 0.1))
	(dur (fdiv 4 trate))
	(clk (dust ar trate))
	(x (mouse-x kr 0 (buf-dur kr b) 0 0.1))
	(pos (add x (t-rand 0 0.01 clk)))
	(pan (mul (white-noise kr) 0.6)))
   (out 0 (t-grains 2 clk b 1 pos dur pan 0.1 2))))
]

The SC3 ** operator is the ShiftLeft binary UGen.


@racketblock[
(audition
 (let* ((b 10)
	(trate (mouse-y kr 2 120 1 0.1))
	(dur (fdiv 1.2 trate))
	(clk (impulse ar trate 0))
	(pos (mouse-x kr 0 (buf-dur kr b) 0 0.1))
	(pan (mul (white-noise kr) 0.6))
	(rate (shift-left 1.2 (u:round (mul (white-noise kr) 3) 1))))
   (out 0 (t-grains 2 clk b rate pos dur pan 0.1 2))))
]


