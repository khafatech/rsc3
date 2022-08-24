#lang racket

(require rsc3)


;; from rsc3/help/ugen/binary-ops/ring3.help.scm


;; make group 1
(with-sc3 (lambda (fd) (send fd (g-new1 1 add-to-tail 0))))

(audition
 (out 0 (mul (ring4 (f-sin-osc ar 800 0)
                    (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0))
             0.125)))

(let ((a (f-sin-osc ar 800 0))
      (b (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0)))
  (audition
   (out 0 (mul (sub (mul3 a a b) (mul3 a b b)) 
               0.125))))


(audition
 (out 0 (mul (ring3 (f-sin-osc ar 800 0)
                    (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0))
             0.125)))

;; to stop sound and reset nodes/groups
;; (with-sc3 reset)
