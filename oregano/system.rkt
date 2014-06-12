#lang racket



(provide (all-defined-out))

(define (sc-running/unix?)
  (system "ps -e | grep scsynth | grep -v grep > /dev/null"))


(define (get-scsynth-path/osx)
  (define path1 "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth")
  (define path2 "/Volumes/SuperCollider/SuperCollider/SuperCollider.app/Contents/Resources/scsynth")
  (cond [(file-exists? path1) path1]
         [(file-exists? path2) path2]
         [else (error "Could not find scsynth for running SuperCollider")]))

;; run scsynth
(define (run-super-collider)
  ;(display "in run-super-collider\n")
  (match (system-type 'os)
    ('unix (if (sc-running/unix?)
               (display "SuperCollider Running\n")
               (begin
                 (display "Starting SuperCollider...")
                 (process "./start_server_linux.sh")
                 (sleep 1)
                 (if (sc-running/unix?)
                     (display "OK\n")
                     (display "Error\n")))))
    ('macosx (if (sc-running/unix?)
               (display "SuperCollider Running\n")
               (begin
                 (display "Starting SuperCollider...")
                 (process* (get-scsynth-path/osx) "-u" "57110")
                 (sleep 0.5)
                 (if (sc-running/unix?)
                     (display "OK\n")
                     (display "Error\n")))))
    ('windows 1)
    (else 1)))

(define (stop-super-collider)
  ;(display "in run-super-collider\n")
  (match (system-type 'os)
    ('unix 1)
    ('macosx (begin
               (display "Stopping SuperCollider...")
               (process "pkill scsynth")
               (display (if (sc-running/unix?) "Error stopping\n" "OK\n"))))
    ('windows 1)
    (else 1)))

(run-super-collider)
