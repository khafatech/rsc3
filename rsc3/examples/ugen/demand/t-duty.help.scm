;; (t-duty rate duration reset doneAction level gap)

;; demand results as trigger from demand rate ugens.

;; A value is demanded each ugen in the list and output as a trigger
;; according to a stream of duration values.  The unit generators in
;; the list should be 'demand' rate.  When there is a trigger at the
;; reset input, the demand rate ugens in the list and the duration are
;; reset.  The reset input may also be a demand ugen, providing a
;; stream of reset times.

;; NOTE: sclang reorders the inputs to be 'duration reset level
;; doneAction', rsc does not.

;; duration   - time values. Can be a demand ugen or any signal.
;; 	        The next trigger value is acquired after the
;;              duration provided by the last time value.

;; reset      - trigger or reset time values. Resets the list of ugens
;;              and the duration ugen when triggered. The reset input 
;;              may also be a demand ugen, providing a stream of reset 
;;              times.

;; doneAction - a doneAction that is evaluated when the duration 
;;              stream ends.

;; level      - demand ugen providing the output values.

;; Play a little rhythm

(let ((s (dseq dinf (make-mce (list 0.1 0.2 0.4 0.3)))))
  (audition (out 0 (t-duty ar s 0 0 1 0))))

;; Amplitude changes

(let ((t (t-duty ar 
		 (dseq dinf (make-mce (list 0.1 0.2 0.4 0.3)))
		 0
		 0
		 (dseq dinf (make-mce (list 0.1 0.4 0.01 0.5 1.0)))
		 0)))
  (audition (out 0 (ringz t 1000 0.1))))

(let ((t (t-duty ar
		 (mouse-x kr 0.001 2 1 0.1)
		 0
		 0
		 (dseq dinf (make-mce (list 0.1 0.4 0.01 0.5 1.0)))
		 0)))
  (audition (out 0 (ringz t 1000 0.1))))
