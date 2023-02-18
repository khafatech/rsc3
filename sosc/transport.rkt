#lang racket

(require "bytevector.rkt"
         "encoding.rkt"
         "ip.rkt")

(provide send
         recv
         wait)

;; socket -> osc -> ()
(define send
  (lambda (fd m)
    (let ((b (encode-osc m)))
      (cond ((udp:socket? fd)
             (udp:send fd b))
            ((tcp:socket? fd)
             (tcp:send fd (encode-u32 (bytevector-length b)))
             (tcp:send fd b))))))

;; port -> maybe osc
(define recv
  (lambda (fd)
    (cond ((udp:socket? fd)
           (let ((b (udp:recv fd)))
             (and b (decode-osc b))))
          ((tcp:socket? fd)
           (let* ((b (tcp:read fd 4))
                  (n (decode-u32 b)))
             (decode-osc (tcp:read fd n)))))))

;; port -> string -> osc
(define wait
  (lambda (fd s)
    (let ((p (recv fd)))
      (cond
       ((not p) (error "error" "Could not connect to the SuperCollider server"))
       ((not (string=? (car p) s)) (error "wait" "bad return packet" p s))
       (else p)))))
