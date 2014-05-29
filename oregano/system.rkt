#lang racket



(provide (all-defined-out))

;; run scsynth
(define (run-super-collider)
  (display "in run-super-collider\n")
  (match (system-type 'os)
    ('unix (if (system "ps -e | grep scsynth > /dev/null")
               (display "SuperCollider Running\n")
               (begin
                 (display "Starting SuperCollider...")
                 (process "./start_server_linux.sh")
                 (sleep 0.5)
                 (if (system "ps -e | grep scsynth > /dev/null")
                     (display "OK")
                     (display "Error")))))
    ('macosx 1)
    ('windows  1)
    (else 1)))

