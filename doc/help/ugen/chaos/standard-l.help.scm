;; (standard-l rate freq k xi yi)
;; (standard-n rate freq k xi yi)

;; Standard map chaotic generator.

;; freq - iteration frequency in Hertz
;; k    - perturbation amount
;; xi   - initial value of x
;; yi   - initial value of y

;; A linear-interpolating sound generator based on the difference
;; equations:

;; 	xn+1 = (xn + yn+1) % 2pi
;; 	yn+1 = (yn + ksin(xn)) % 2pi

;; The standard map is an area preserving map of a cylinder discovered by
;; the plasma physicist Boris Chirikov.

;; Vary frequency

(audition
 (out 0 (mul (standard-l ar (mouse-x kr 20 sample-rate 0 0.1) 1 0.5 0) 0.3)))

;; Mouse-controlled parameter.

(let ((f (fdiv sample-rate 2))
      (x (mouse-x kr 0.9 4 0 0.1)))
  (audition
   (out 0 (mul (standard-l ar f x 0.5 0) 0.3))))

;; As a frequency control

(let* ((x (mouse-x kr 0.9 4 0 0.1))
       (f (mul-add (standard-l ar 40 x 0.5 0) 800 900)))
  (audition
   (out 0 (mul (sin-osc ar f 0) 0.4))))
