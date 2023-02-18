;; (mouse-button rate minval maxval lag)

;; Report the status of the first pointer button.  The button is
;; either pressed, or not pressed.

(audition
 (out 0 (mul (sin-osc ar 800 0)
	     (mouse-button kr 0 0.1 0.1))))
