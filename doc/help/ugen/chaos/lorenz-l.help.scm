;; (lorenz-l rate freq s r b h xi yi zi)

;; freq - iteration frequency in Hertz
;; s, r, b - equation variables
;; h - integration time step
;; xi - initial value of x
;; yi - initial value of y
;; zi - initial value of z

;; Lorenz chaotic generator.  A strange attractor discovered by Edward
;; N. Lorenz while studying mathematical models of the atmosphere.
;; The system is composed of three ordinary differential equations:

;; x' = s(y - x)
;; y' = x(r - z) - y
;; z' = xy - bz

;; The time step amount h determines the rate at which the ODE is
;; evaluated.  Higher values will increase the rate, but cause more
;; instability.  A safe choice is the default amount of 0.05.

;; vary frequency

(audition
 (out 0 (mul (lorenz-l ar (mouse-x kr 20 sample-rate 0 0.1)
		       10 28 2.667 0.05 0.1 0 0)
	     0.3)))

;; randomly modulate params

(audition
 (out 0 (mul (lorenz-l ar sample-rate
		       (mul-add (lf-noise0 kr 1) 2 10)
		       (mul-add (lf-noise0 kr 1) 20 38)
		       (mul-add (lf-noise0 kr 1) 1.5 2)
		       0.05
		       0.1 0.0 0.0)
	     0.2)))

;; as a frequency control

(audition
 (out 0 (mul
	 (sin-osc ar (mul-add
		      (lag
		       (lorenz-l ar 
				 (mouse-x kr 1 200 0 0.1) 
				 10 
				 28 
				 2.667 
				 0.05 
				 0.1 
				 0 
				 0)
		       0.003)
		      800 900)
		  0)
	 0.4)))
