#lang scribble/manual
@(require (for-label racket))

@title{(fft buffer in hop wintype active)}

(fft* b i) => (fft b i 0.5 0 1)

Fast fourier transform.  The fast fourier transform analyzes the
frequency content of a signal.  fft uses a local buffer for holding
the buffered audio.  The inverse transform, Ifft, reconstructs an
audio signal.
 
Note that the UGens the SC3 language provides do not use rate
extensions, since only a single rate is valid for each UGen class.
The fft and PV_ UGens must run at control rate, the ifft UGen at
audio rate.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))))

(let* ((s (mul (white-noise ar) 0.05))
       (c (fft* 10 s)))
  (audition (out 0 (ifft* c))))

(let* ((f1 (Squared (mul-add (sin-osc kr 0.08 0) 6 6.2)))
       (f2 (mul-add (sin-osc kr f1 0) 100 800))
       (s (sin-osc ar f2 0)))
  (audition (out 0 (ifft* (fft* 10 s)))))

