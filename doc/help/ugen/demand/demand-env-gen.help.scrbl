#lang scribble/manual
@(require (for-label racket))

@title{(demand-env-gen rate levels times shapes curves gate reset}

                levelScale levelOffset timeScale doneAction)

levels - a demand ugen or any other ugen

times  - a demand ugen or any other ugen if one of these ends,
         the doneAction is evaluated

shapes - a demand ugen or any other ugen, the number given is
         the shape number according to Env

curves - a demand ugen or any other ugen, if shape is 5, this
         is the curve factor some curves/shapes don't work if
         the duration is too short. have to see how to improve
         this. also some depend on the levels obviously, like
         exponential cannot cross zero.
       
gate   - if gate is x >= 1, the ugen runs, if gate is 0 > x > 1,
         the ugen is released at the next level (doneAction), if
         gate is x < 0, the ugen is sampled end held

reset  - if reset crosses from nonpositive to positive, the ugen
         is reset at the next level, if it is > 1, it is reset
         immediately.

Frequency envelope with random times.


@racketblock[
(let* ((l (dseq dinf (make-mce (list 204 400 201 502 300 200))))
       (t (drand dinf (make-mce (list 1.01 0.2 0.1 2.0))))
       (y (mouse-y kr 0.01 3 1 0.1))
       (f (demand-env-gen ar l (mul t y) 7 0 1 1 1 0 1 do-nothing)))
  (audition (out 0 (mul (sin-osc ar (mul f (mce2 1 1.01)) 0) 0.1))))
]

Frequency modulation


@racketblock[
(let* ((x (mouse-x kr -0.01 -4 0 0.1))
       (y (mouse-y kr 1 3000 1 0.1))
       (l (lambda () (dseq dinf (clone 32 (exp-rand 200 1000)))))
       (t (mul sample-dur y))
       (f (demand-env-gen ar (mce2 (l) (l)) t 5 x 1 1 1 0 1 do-nothing)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
]

 gate. Mouse x on right side of screen toggles gate.


@racketblock[
(let* ((x (mouse-x kr 0 1 0 0.1))
       (l (u:round (dwhite dinf 300 1000) 100))
       (f (demand-env-gen kr l 0.1 5 0.3 (gt x 0.5) 1 1 0 1 do-nothing)))
  (audition (out 0 (mul (sin-osc ar (mul f (mce2 1 1.21)) 0) 0.1))))
]


