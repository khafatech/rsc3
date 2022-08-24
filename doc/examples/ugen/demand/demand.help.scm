;; (demand trig reset ugens)

;; demand results from demand rate ugens.

;; When there is a trigger at the trig input, a value is demanded from
;; each ugen in the list and output. The unit generators in the list
;; should be 'demand' rate.

;; When there is a trigger at the reset input, the demand rate ugens
;; in the list are reset.

;; trig - trigger. trigger can be any signal. A trigger happens when
;;                 the signal changes from non-positive to positive.

;; reset - trigger. Resets the list of ugens when triggered.

(define (mirror1 l)
  (append l (cdr (reverse (cdr l)))))

(let* ((t (impulse kr 24 0))
       (s (drand dinf (mce2 (dseq 1 (make-mce (mirror1 (enum-from-to 1 5))))
                               (drand 8 (make-mce (enum-from-to 4 11))))))
       (f (demand t 0 (mul s 100)))
       (x (mouse-x kr -1 1 0 0.1))
       (o (sin-osc ar (mce2 f (add f 0.7)) 0)))
  (audition (out 0 (mul (scale-neg (cubed (cubed o)) x) 0.1))))

(let* ((t (impulse kr 10 0))
       (r (dust kr 1))
       (s (dgeom dinf (midi-cps 72) (midi-ratio 1)))
       (f (demand t r s))
       (o (sin-osc ar (mce2 f (add f 0.7)) 0)))
  (audition (out 0 (mul (u:max (cubed o) 0) 0.1))))

(let* ((t (impulse kr 10 0))
       (s (midi-cps (diwhite dinf 60 72)))
       (f (demand t 0 s))
       (o (sin-osc ar (mce2 f (add f 0.7)) 0)))
  (audition (out 0 (mul (cubed (cubed o)) 0.1))))
