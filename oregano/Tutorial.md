
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
- Track: 
- Filters: can be added to a track. The order in which they are added matters.


## Example

### 1. Creating an instrument

You can either use a preset instrument or define your own instrument

    ;; this uses the piano preset
    (define my-piano (make-instrument 'piano))
    
    ;; create a custom instrument
    ;; can use oscilators and envelopes
    (define my-instrument (make-instrument ...)
    
    ;; add envelope to instrument

### 2. Playing a note

Now that we have an instrument, we can use it to play notes on a specific track.

    ;; this plays the key C#, octave 3 on track 2.
    (note-on my-piano C#3 track2)
    
    ;; to stop playing
    (note-off my-piano C#3 track2)


TODO:
- use an instrument to play a note on a track.
- add filters to  a track


    (append-effect track3 (reverb-effect 0.5 0.9))

