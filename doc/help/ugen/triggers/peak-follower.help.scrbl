#lang scribble/manual
@(require (for-label racket))

@title{(peak-follower in decay)}


Track peak signal amplitude.  outputs the peak amplitude of the
signal received at the input.  If level is below maximum, the level
decreases by the factor given in decay.

in    - input signal.
decay - decay factor.

internally, the absolute value of the signal is used, to prevent
underreporting the peak value if there is a negative DC offset. To
obtain the minimum and maximum values of the signal as is, use the
running-min and running-max UGens.


No decay


@racketblock[
(let* ((s (mul (dust ar 20) (line kr 0 1 4 do-nothing)))
       (f (mul-add (peak-follower s 1.0) 1500 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
]

A little decay


@racketblock[
(let* ((s (mul (dust ar 20) (line kr 0 1 4 do-nothing)))
       (f (mul-add (peak-follower s 0.999) 1500 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
]

Mouse x controls decay


@racketblock[
(let* ((x (mouse-x kr 0.99 1.0001 1 0.1))
       (s (mul (dust ar 20) (line kr 0 1 4 do-nothing)))
       (f (mul-add (peak-follower s (u:min x 1.0)) 1500 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
]

Follow a sine lfo


@racketblock[
(let* ((x (mouse-x kr 0.99 1.0001 1 0.1))
       (s (sin-osc kr 0.2 0))
       (f (mul-add (peak-follower s (u:min x 1.0)) 200 500)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
]


