#lang racket


(require rsc3 rhs/rhs)
(with-sc3 reset)


;; test 
(let* ([freq (mouse-x kr 600 2000 1 0.1)]
       [sig (mul (sin-osc ar
                          (mul-add (lf-pulse ar 15 0 0.5) 200 freq)
                          0)
                 (key-state kr 38 0 0.1 0.1))])
  (audition (out 16 sig)))

(audition (out 0 (free-verb (in 1 ar 16) 0.5
                            (mouse-y kr 0 1 0 0.1)
                            0.5)))



