#lang racket



(provide (all-defined-out))

;; run scsynth
(define (run-super-collider)
  (match (system-type 'os)
    ('unix (when (not (system "ps -e | grep scsynth > /dev/null"))
             (thread (lambda ()
                     (system "./start_server_linux.sh")))))
    ('macosx 1)
    ('windows  1)
    (else 1)))

