#lang racket
;; status - not working (no sound) as of 20220819

#|

[ "/b_alloc", 0, 24, 1 ]
[ "/b_alloc", 1, 24, 1 ]
[ "/b_alloc", 2, 24, 1 ]
[ "/b_alloc", 3, 24, 1 ]
[ "/b_alloc", 4, 24, 1 ]
[ "/b_alloc", 5, 24, 1 ]
[ "/d_recv", DATA[8729] ]
[ "/s_new", "anonymous", -1, 1, 1 ]

|#


(define rand-l
  (lambda (n l r)
    (replicate-m n (rand-float l r))))

(define adso
  (lambda (n)
    (let* ((b-get
            (lambda (b n)
              (let ((j (make-mce (enum-from-to 0 (- n 1)))))
                (buf-rd-n 1 kr b j 0))))
           (m (sin-osc kr (b-get 3 n) 0))
           (f (mul (midi-cps (b-get 0 n))
                   (mul-add m (b-get 4 n) 1))))
      (mix (pan2 (sin-osc ar f 0)
                        (b-get 2 n)
                        (b-get 1 n))))))

(define pattern
  (lambda (fd n)
    (lambda (t)
      (let ((z (map floor
                    (replicate-m
                     n (rand-float (rand-float 22 48)
                                   (rand-float 54 122)))))
            (rn (lambda (i j k)
                  (sosc:send fd (b-setn1 i 0 (rand-l n j k))))))
        (sosc:send fd (b-setn1 0 0 z))
        (rn 1 0 0.1)
        (rn 2 -1 1)
        (rn 3 2 12)
        (rn 4 0.001 0.0075)
        (rn 5 1 24)
        (rn 6 0.05 (rand-float 0.075 2.4))
        (thread-sleep t)))))

(let ((n 24))
  (with-sc3
   (lambda (fd)
     (for-each
      (lambda (i)
        (async fd (b-alloc i n 1)))
      (range 0 6))
     (play fd (out 0 (adso n)))
     (map (pattern fd n)
          (replicate-m
           32
           (choose
            (list 0.025 0.05 0.075 0.1 0.125 2.5))))
     (reset fd))))


