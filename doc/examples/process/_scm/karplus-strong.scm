;; karplus strong (alex mclean)

(define a-a
  (list
   "a-a"
   (list 800 1150 2800 3500 4950)
   (list 0 -4   -20 -36  -60)
   (list 80 90  120 130 140)))

(define a-u
  (list
   "a-u"
   (list 325 700 2530 3500 4950)
   (list 0 -12  -30 -40 -64)
   (list 50 60  170 180 200)))

(define cs
  (lambda (l)
    (concat
     (list
      (listRef l 1)
      (listRef l 2)
      (listRef l 3)))))

(define vf
  (lambda (i s)
    (let ((f (CtlIn 5 i))
          (a (CtlIn 5 (Add i 5)))
          (b (CtlIn 5 (Add i 10))))
      (Mix (Mul (Resonz s f (Fdiv b f)) (DbAmp a))))))

(define probSwitch
  (lambda (n0 i prob)
    (UgenIf (Gt n0 prob) i (Neg i))))

(define ks
  (lambda (n d)
    (let* ((x (MouseX 0 0.01 linear 0.1)) ; delay
           (y (MouseY 0.85 1 linear 0.1)) ; blend / gain
           (n0 (Add (Fdiv n 2) 0.5))
           (laggedDelay (Lag x 0.01))
           (o (SinOsc 200 0))
           (a0 (Mul (Decay d 0.025) o))
           (a1 (Add (LocalIn 1 0) (Mul a0 (Sub y 0.25))))
           (a2 (DelayN a1 0.01 laggedDelay))
           (a3 (Delay1 a2))
           (a4 (Fdiv (Add a2 a3) 2.0))
           (a5 (probSwitch n0 a4 y))
           (a6 (vf (Mul (ToggleFF d) 15) a5))
           (a7 (Mul a6 1.5)))
      (Mrg2
       (mce2 a7 a7)
       (LocalOut (Mul a5 0.99))))))

(define karplusStrong
  (lambda (fd)
    (begin
      (sendMessage fd (c_setn1 0 (cs a-a)))
      (sendMessage fd (c_setn1 15 (cs a-u)))
      (play fd (Out 0 (ks (WhiteNoise) (Dust 4)))))))

(withSc3 karplusStrong)

; (ks (WhiteNoise) (Dust 4))
