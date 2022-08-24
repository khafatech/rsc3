; adso (rd)

(define rand-l
  (lambda (n l r)
    (replicateM n (lambda () (s:rand l r)))))

; (adso 4)
(define adso
  (lambda (n)
    (let* ((b-get
            (lambda (b n)
              (let ((j (asMce (enumFromTo 0 (- n 1)))))
                (kr: (BufRd 1 b j 0 1)))))
           (m (SinOsc (b-get 3 n) 0))
           (f (Mul (MidiCps (b-get 0 n)) (MulAdd m (b-get 4 n) 1))))
      (Mix (Pan2 (SinOsc f 0) (b-get 2 n) (b-get 1 n))))))

(define pattern
  (lambda (fd n)
    (lambda (t)
      (let ((z (map floor
                    (replicateM
                     n (lambda () (s:rand (s:rand 22 48)
                                          (s:rand 54 122))))))
            (rn (lambda (i j k)
                  (sendMessage fd (b_setn1 i 0 (rand-l n j k))))))
        (sendMessage fd (b_setn1 0 0 z))
        (rn 1 0 0.1)
        (rn 2 -1 1)
        (rn 3 2 12)
        (rn 4 0.001 0.0075)
        (rn 5 1 24)
        (rn 6 0.05 (s:rand 0.075 2.4))
        (thread-sleep t)))))

(let ((n 24))
  (withSc3
   (lambda (fd)
     (for-each
      (lambda (i)
        (async fd (b_alloc i n 1)))
      (enumFromTo 0 6))
     (play fd (Out 0 (adso n)))
     (map (pattern fd n)
          (replicateM
           32
           (lambda ()
             (s:l-choose
              (list 0.025 0.05 0.075 0.1 0.125 2.5)))))
     (reset fd))))
