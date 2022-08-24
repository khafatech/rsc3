#lang scribble/manual
@(require (for-label racket))

@title{(record-buf bufnum offset reclevel prelevel run loop trigger inputs)}

Records input into a Buffer.

If recLevel is 1.0 and preLevel is 0.0 then the new input
overwrites the old data.  If they are both 1.0 then the new data is
added to the existing data. (Any other settings are also valid.)

bufnum     - the index of the buffer to use
offset     - an offset into the buffer in samples, default 0
recLevel   - value to multiply by input before mixing with
             existing data. Default is 1.0.
preLevel   - value to multiply to existing data in buffer before
             mixing with input. Default is 0.0.
run        - If zero, then recording stops, otherwise recording
             proceeds. Default is 1.
loop       - If zero then don't loop, otherwise do.  This is
             modulate-able. Default is 1.
trigger    - a trigger causes a jump to the start of the Buffer.
             A trigger occurs when a signal changes from <=0 to >0.
inputArray - an Array of input channels

(with-sc3
 (lambda (fd)
   (send-synth 
    fd 
    "recorder"
    (letc ((in 0) 
	   (bufnum 0)
	   (offset 1) 
	   (recLevel 1) 
	   (preLevel 0)
	   (run 1) 
	   (loop 1) 
	   (trigger 1))
      (let ((i (in 2 ar in)))
	(out 0 (record-buf bufnum offset recLevel preLevel run loop trigger i)))))
   (let ((b 10)
	 (y 1001)
	 (z 1002))
     (async fd (/b_alloc b 44100 2))
     (send fd (/s_new "recorder" y add-to-tail 1 "bufnum" b "in" 8))
     (send fd (/n_trace y))
     (send-synth 
      fd
      "player"
      (letc ((bufnum 0) 
	     (rate 1)
	     (trigger 1) 
	     (startPos 0) 
	     (loop 1) 
	     (gain 1))
	(out 0 (mul (play-buf 2 bufnum rate trigger startPos loop) gain))))
     (send fd (/s_new "player" z add-to-tail 1 "bufnum" b)))))

(define do-send
  (lambda (m)
    (with-sc3
     (lambda (fd)
       (send fd m)))))

(do-send (/n_set 1001 "run" 1))

(do-send (/n_set 1002 "loop" 1))
(do-send (/n_set 1002 "gain" 2))
(do-send (/n_set 1002 "trigger" 1))

(do-send (/n_free 1001))
(do-send (/n_free 1002))

(with-sc3
 (lambda (fd)
   (async fd (/b_free 10))))

