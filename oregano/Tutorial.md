
## Installing

You need to install SuperCollider and the Dr Racket package.

### 1. Installing SuperCollider

Go to https://supercollider.github.io/download.html and download the latest version (3.6.6 as of the time of writing.)

Then follow these instructions per operating system.

#### Linux

- install jack. On Debian based Linux distributions  (e.g. Ubuntu, Mint), type in a terminal:
   
    `sudo apt-get install jack-tools ant openjdk-6-jdk fftw3 qjackctl`

#### Windows

TODO

#### Mac OS X

- Install SuperCollider in Applications


### 2. Installing oregano

This can be installed in two different ways:
- Either through the command line: `raco pkg install rsc3`

- Or through DrRacket: TODO



## Concepts

You can play notes from instruments on specific tracks.

The purpose of playing notes on different tracks is we can have different filters on different tracks. For example, the melody track can have a low pass filter, and and the drums track could have a delay filter.


- Instrument: This is equivalent to choosing what the note sounds like. There are default instruments (saw wave, piano, etc.) and you can also define your own using samples.
- Note: specifies a key from an instrument, and can be played on a track
- Track: a place to play notes and add filters.
- Filters: can be added to a track. The order in which they are added matters.


## Examples

All these examples start with `(require oregano)`.

### 1. Playing a note using a preset instrument

You can either use a preset instrument or define your own instrument. There are a few preset instruments: "sin-inst", "saw-inst", "moog-inst"

```scheme
#lang racket
(require oregano)

;; this plays the key 
(define my-note (play-note "sin-inst" 440))
(sleep 1)
;; stop playing note
(note-off my-note)
```
<!--
Or you can create a note object then play it.

```scheme

(define my-note2 (make-note "sin-inst" 880))

(note-on my-note2)
(sleep 1)
(note-off my-note2)
```
-->

### 2. Creating an instrument
 A custom instrument is composed of three parts:
- it name. This is a string and is used when playing notes
- Instrument arguments and default values. You could change these parameters when a note is playing, in real time.
- the signal. This can use SuperCollider ugens. (TODO link)

- some are:

```scheme
(saw ar freq)
(sin-osc ar freq phrase)
```

```scheme

;; create a custom instrument
;; can use oscillators and envelopes
;; "freq" is the frequency parameter
(make-instrument "my-inst" ([freq 500] [mod 20])
  (mul (sin-osc ar mod 0)
       (sin-osc ar freq 0)))

(define weird-note (play-note "my-inst" 440))

; change frequency
(set-note-param weird-note "freq" 808)

; change modulation
(set-note-param weird-note "mod" 40)

```


### 3 Sliders


You can easily create a slider for a specific note parameter. You have to provide the title, start value, end value, default value, and a callback function.

For example, previous note, `weird-note`:

```scheme
(param-slider "change modulation" 1  100 40
                (lambda (val)
                  (set-note-param weird-note "mod" val)))

;; show the slider
(show-gui)
```



### 4. Add filters to a track

You can add filters to a specific track.

Add a reverb to track 0. (currently only track 0 works.)

```scheme
(make-instrument "phone-inst" ([freq 500])
                 (mul (sin-osc ar (mul-add (lf-pulse ar 15 0 0.5) 200 freq) 0)
                      (mouse-button kr 0 0.1 0.1)))

;; click any mouse button to hear the note
(define phone-note (play-note "phone-inst" 600))

;; apply a reverb effect on track 0
(reverb 0 0.5)

(sleep 3)

(moog-filter 0 800)

```


### mouse/x and mouse/y

You can parameters to filters and instruments using the mouse.

    (mouse/x start-value end-value)

For example, if you want to control the cutoff frequency for a filter using the left-right position of the mouse.

    (low-pass-filter 0 (mouse/x kr 200 500)))

When the mouse is at the left of the screen, the frequency is 200, when the mouse is at the right, the frequency is 500.



### Loading samples

The syntax is

* (load-sample file-name) ; returns a sample

* (play-sample sample rate)

`load-sample` returns a sample object which is passed to play-sample.

The `rate`, an optional parameter, controls the frequency at which the sample is played. 1 means normal frequency. 2 is an octave above, and 0.5 is an octave below.

Example:

```scheme
(define synth-hit1 (load-sample "/path/to/bass.wav"))

(play-sample synth-hit1)

(sleep 1)

(play-sample synth-hit1 2)
```

### Play a note in the future

Currently not working, or the start-time syntax is wrong.

`(play-note start-time duration instrument frequency)`

`(play-note-at 2 1 "sin-inst" 440)`



---
### Envelopes

```scheme
Didn't implement envelopes yet.
;; add envelope to instrument
(define my-inst (preset-instrument "sine"
    (envelope A S D R))
```



## Functions

### Oscilators

These can be used in defining instruments.

- sin, saw, triangle, square

- 


### List of Filters

- hpf, lpf

- moog

- reverb

- delay

- comb




---


##


### Sliders

    (add-effect track3 (low-pass-filter resonance 
                                        (slider "name" 200 500 300)))


How should I deal with one instrument on multiple tracks?

keep a list of instruments per track.

create a separate synth per track.

; bus = 2
; should create a new synth
(play-note my-inst 880 track2)


; bus = 3
(play-note my-inst 880 track3)









