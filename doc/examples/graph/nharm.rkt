#lang racket
;; status - working as of 20220820

(require rsc3)

;; nharm (rd)

(define nharm
  (lambda (n f)
    (if (<= n 0)
        (list)
        (cons f (nharm (- n 1) (add f f))))))

(define klg
  (lambda (m u)
    (let* ((n (rand-int 4 u))
           (d (rand-int 9 12))
           (a 0.5)
           (e (env-gen kr 1 0.9 0 1 remove-synth (env-sine d a)))
           (s (klang-data (nharm n (midi-cps (rand-float m (+ m 2))))
                          (replicate-m n (rand-float 0.01 0.02))
                          (replicate n 0))))
      (pan2 (klang ar 1 0 s)
            (rand-float -1 1)
            e))))

(define pattern
  (lambda (fd)
    (play fd (out 0 (klg (rand-float 32 92)
                         (rand-int 9 24))))
    (thread-sleep (rand-float 0.25 0.75))
    (pattern fd)))

(with-sc3 pattern)
