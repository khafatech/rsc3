;; (dbrown  length lo hi step)
;; (dibrown length lo hi step)

;; demand rate brownian movement generators.

;; lo     - minimum value
;; hi     - maximum value
;; step   - maximum step for each new value
;; length - number of values to create

;; dbrown returns numbers in the continuous range between lo and hi,
;; dibrown returns integer values.  The arguments can be a number or
;; any other ugen.

(let ((f (lambda (u)
           (let* ((a (u dinf 0 15 1))
                  (t (impulse kr (mouse-x kr 1 40 1 0.1) 0))
                  (f (mul-add (demand t 0 a) 30 340)))
             (mul (sin-osc ar f 0) 0.1)))))
  (audition (out 0 (mce2 (f dbrown) (f dibrown)))))
