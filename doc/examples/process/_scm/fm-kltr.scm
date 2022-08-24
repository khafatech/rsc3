; fm-kltr (rd)

(define fm-instr
  (letc ((trig 0)
         (amp 0.1)
         (dur 0.1)
         (freq 400)
         (index 40)
         (pan 0)
         (freq2 600))
    (let* ((p (EnvPerc 0.01 dur 1 (replicate 2 -4)))
           (e (EnvGen 1 amp 0 1 removeSynth p))
           (x (XLine freq (Mul freq (s:rand 0.975 1.025)) dur 0))
           (o (SinOsc x 0))
           (l (Line index (Mul freq (s:rand 0.5 1.5)) dur 0))
           (x2 (XLine freq2 (Mul freq2 (s:rand 0.975 1.025)) dur 0))
           (m (MulAdd o l x2)))
      (Pan2
       (SinOsc m 0)
       (Line pan (Mul pan (s:rand 0.75 1.25)) dur 0)
       e))))

(define fm
  (lambda (fd f ff a d i)
    (sendMessage
     fd
     (s_new "fm" -1 1 1
            (list "freq" (s:midi-cps f)
                  "freq2" (+ (s:midi-cps ff) (s:rand -1 1))
                  "amp" a
                  "dur" d
                  "index" i)))))

(define r-note
  (lambda (o p)
    (+ (* (s:l-choose o) 12) (s:l-choose p))))

(define low-f
  (lambda (fd)
    (fm
     fd
     (r-note (list 3 5) (list 0 3 7 8))
     (s:rand 36 72)
     (s:rand 0.0 0.2)
     (s:rand 1.2 7.2)
     (s:rand 240 1480))))

(define high-f
  (lambda (fd)
    (fm
     fd
     (r-note (list 7 10) (list 0 2 5 10))
     (s:rand 84 120)
     (s:rand 0.1 0.6)
     (s:rand 0.2 1.2)
     (s:rand 240 1480))))

(define cmp-f
  (lambda (a b)
    (compare (car a) (car b))))

(define low-t
  (integrate
   (cons
    0
    (replicateM
     36
     (lambda () (s:l-choose (list 0.25 0.5 0.75 1.0 1.5)))))))

(define high-t
  (integrate
   (cons
    0
    (replicateM
     36
     (lambda () (s:l-choose (list 0.05 0.15 0.25 0.5 0.75)))))))

(define fm-kltr
  (lambda (fd)
    (sendSynth fd "fm" (Out 0 fm-instr))
    (map
     (lambda (x)
       ((cadr x) fd)
       (thread-sleep (car x)))
     (sortBy
      cmp-f
      (append (map (lambda (x) (list x low-f)) low-t)
	      (map (lambda (x) (list x high-f)) high-t))))))

(withSc3 fm-kltr)
