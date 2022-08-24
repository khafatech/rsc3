;; (rossler rate chaosParam dt)

;; The Rossler attractor is a well known chaotic function.  The
;; chaosParam can be varied from 1.0 to 25.0 with a dt of 0.04.  Valid
;; ranges for chaosParam vary depending on dt.

(audition
 (out 0 (mul (rossler ar 4 0.08) 0.1)))
