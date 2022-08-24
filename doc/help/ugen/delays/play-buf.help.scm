;; (play-buf numChannels bufnum rate trigger startPos loop) 

;; Sample playback oscillator.  Plays back a memory resident sample.

;; numChannels - number of channels that the buffer will be.  This
;;               must be a fixed integer. The architechture of the
;;               SynthDef cannot change after it is compiled.
;;               Warning: if you supply a bufnum of a buffer that has
;;               a different numChannels then you have specified to
;;               the play-buf, it will fail silently.

;; bufnum - the index of the buffer to use

;; rate - 1.0 is the server's sample rate, 2.0 is one octave up, 0.5
;;        is one octave down -1.0 is backwards normal rate
;;        etc. interpolation is cubic.  Note: If the buffer's sample
;;        rate is different from the server's, you will need to
;;        multiply the desired playback rate by (file's rate /
;;        server's rate). The UGen buf-rate-scale.kr(bufnum) returns
;;        this factor. See examples below. buf-rate-scale should be used
;;        in virtually every case.

;; trigger - a trigger causes a jump to the startPos.  A trigger
;; occurs when a signal changes from <= 0 to > 0.

;; startPos - sample frame to start playback.

;; loop - 1 means true, 0 means false.  This is modulate-able.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 10 "/home/rohan/audio/metal.wav" 0 0))))

;; Play once only.

(audition (out 0 (play-buf 1 10 (buf-rate-scale kr 10) 1 0 0)))

;; Play in infinite loop.

(audition (out 0 (play-buf 1 10 (buf-rate-scale kr 10) 1 0 1)))

;; trigger playback at each pulse.

(audition (out 0 (play-buf 1 10 (buf-rate-scale kr 10) (impulse kr 2 0) 0 0)))

;; trigger playback at each pulse (diminishing intervals).

(let ((t (impulse kr (x-line kr 0.1 100 10 remove-synth) 0)))
  (audition (out 0 (play-buf 1 10 (buf-rate-scale kr 10) t 0 0))))

;; Loop playback, accelerating pitch.

(let ((rate (x-line kr 0.1 100 60 remove-synth)))
  (audition (out 0 (play-buf 1 10 rate 1 0 1))))

;; Sine wave control of playback rate, negative rate plays backwards.

(let ((r (mul-add (f-sin-osc kr (x-line kr 0.2 8 30 remove-synth) 0) 3 0.6)))
  (audition (out 0 (play-buf 1 10 (mul (buf-rate-scale kr 10) r) 1 0 1))))

;; Release buffer.

(with-sc3
 (lambda (fd)
   (async fd (b-free 10))))
