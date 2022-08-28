#lang scribble/manual
@(require (for-label racket))

@title{(warp1 nc buf ptr freqScale windowSize envbuf overlaps windowrandRatio interp)}


Warp a buffer with a time pointer

inspired by Chad Kirby's SuperCollider2 warp1 class, which was
inspired by Richard Karpen's sndwarp for CSound. A granular time
strecher and pitchshifter.

nc - the number of channels in the soundfile used in bufnum.

buf - the buffer number of a mono soundfile.

ptr - the position in the buffer.  The value should be between 0
      and 1, with 0 being the begining of the buffer, and 1 the
      end.

freqScale - the amount of frequency shift. 1.0 is normal, 0.5 is
            one octave down, 2.0 is one octave up. Negative values
            play the soundfile backwards.

windowSize - the size of each grain window.

envbuf - the buffer number containing a singal to use for the grain
         envelope. -1 uses a built-in Hanning envelope.

overlaps - the number of overlaping windows.

windowrandRatio - the amount of randomness to the windowing
                  function.  Must be between 0 (no randomness) to
                  1.0 (probably to random actually)

interp - the interpolation method used for pitchshifting grains. 1
         = no interpolation. 2 = linear. 4 = cubic interpolation
         (more computationally intensive).


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 10 "/home/rohan/audio/metal.wav" 0 0))))
]


@racketblock[
(let* ((p (lin-lin (lf-saw kr 0.05 0) -1 1 0 1))
       (x (mouse-x kr 0.5 2 0 0.1))
       (w (warp1 1 10 p x 0.1 -1 8 0.1 2)))
  (audition (out 0 w)))
]


