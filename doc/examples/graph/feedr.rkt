#lang racket
;; status - not working as of 20220820 (no sound. "/d_recv", DATA[7947] )

(require rsc3)

(with-sc3
 (lambda (fd)
   (let* ((delay-wr
           (lambda (b in)
             (record-buf b 0 1 0 1 1 0 in)))
          (tap
           (lambda (nc b delay-time)
             (play-buf nc b 1 0 (mul delay-time (neg sample-rate)) 1)))
          (dl 6)
          (ff (* dl (server-sample-rate-actual fd)))
          (nc 2))
     (send fd (b-alloc 10 ff nc))
     (audition
      (let* ((n 18)
             (t (replicate-m n (rand 0.0 dl)))
             (g (replicate-m n (rand 0.4 1.0)))
             (f (replicate-m n (rand 0.9 0.95)))
             (d (zip-with
                 (lambda (t g)
                   (mul (tap nc 10 t) g))
                 t g))
             (x (mouse-x* kr 0.02 1.0 1 0.1)))
        (make-mrg
         (out 0 (clip2 (leak-dc (hpf (foldl1 add d) 20) 0.995) 1))
         (delay-wr 10 (foldl add
                             (in nc ar num-output-buses)
                             (map
                              (lambda (e)
                                (mul e x))
                              (zip-with mul d f))))))))))

(with-sc3
 (lambda (fd)
   (send fd (b-zero 10))))
