
## Installing

You need to install SuperCollider and the Dr Racket package.

### 1. Installing SuperCollider

Go to https://supercollider.github.io/download.html and download the latest version (3.6.6 as of the time of writing.)

Then follow these instructions per operating system.

#### Linux

- install jack

#### Windows

#### Mac OS X


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

Or you can create a note object then play it.

```scheme

(define my-note2 (make-note "sin-inst" 880))

(note-on my-note2)
(sleep 1)
(note-off my-note2)
```

### 2. Creating an instrument
 A custom instrument is composed of three parts:
- it's name. This is a string and is used when playing notes
- Instrjument arguments and default values. You could change these paramers when a note is playing, in real time.
- the signal. This can use SuperCollider ugens. (TODO link)

- some are:

```scheme
(saw ar freq)
(sin-osc ar freq phrase)
```

```scheme

;; create a custom instrument
;; can use oscilators and envelopes
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


### Envelopes
```scheme
Didn't implement envelopes yet.
;; add envelope to instrument
(define my-inst (preset-instrument "sine"
    (envelope A S D R))



### 3. Playing a note using a custom instrument

Now that we have an instrument, we can use it to play notes on a specific track.

```scheme
;; this plays the key C#, octave 3 on track 2.
(note-on my-piano C#3 track2)

;; to stop playing
(note-off my-piano C#3 track2)

;; TODO - find a way to turn off a note, or make a note play for a specific time
```


### 4. Add filters to a track

You can add filters to a specific track.

    (add-effect track3 (reverb 0.5 0.9))


## Functions

### Oscilators

These can be used in defining instruments.

- sin, saw, tringle, square

- 

### mouse-x and mouse-y

You can parameters to filters and instruments using the mouse.

    (mouse-x start-value end-value)

For example, if you want to control the cutoff frequency for a filter using the left-right position of the mouse.

    (add-effect track3 (low-pass-filter resonance 
                                        (mouse-x kr 200 500)))

When the mouse is at the left of the screen, the frequency is 200, when the mouse is at the right, the frequency is 500.

### List of Filters

- hpf, lpf

- moog

- reverb

- delay

- comb

### Loading samples

* (load-sample file-name)

* (play-sample sample)




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









