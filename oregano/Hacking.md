
## SuperCollider Racket library


## Goal

I planned on writing a client for the SuperCollider (SC) music synthesis
server for use in the Music Programming section of CSC123. Using the
interface to SuperCollider, students will have access to real-time
synthesis techniques.

## Initial steps

In the first quarter, after examining implementations in various
languages, I set out on creating a new implementation in Racket from
scratch. Quickly, however, it appeared that two quarters is not enough
time, so I looked into porting the scheme implementation of a
supercollider client, rsc3, to Racket.

### The Porting process

I first tried rsc3 in a scheme interpreter, including ikarus and
DrRacket's r6rs mode (#lang r6rs). rsc3 ran with very little
modification (one library funciton name was wrong). However, rsc3 could
not be installed as a Racket package, due to a conflict of the way
scheme and racket handle libraries.  Installing rsc3 as a package is an
important aspect since that is how students will install rsc3.

What I did is instead of using "#lang r6rs", which makes racket behave
like a scheme interpreter confirming to the r6rs scheme standard, I used
"#lang racket" and imported the rnrs library that has scheme functions
in a racket environment. This way I could use Racket's provide and
package functionality works.

The process of porting was relatively straightforward. I compiled a
file, looked for errors, and fixed them. Some defined utility functions
were not necessary because racket had native implementations.

### Challenges when porting

One tricky part was a difference in the list representations between
r6rs scheme and Racket. Racket uses mutable pairs to construct lists,
while scheme uses immutable pairs. This makes it so that a list created
in Racket cannot be used in a scheme function that accepts a list. One
way around this is to construct lists using mcons in Racket then pass it
to a scheme function.

## Features added

It would be hard for first year students to use rsc3 directly, so I
created simplified abstractions (functions and structures) to perform
common tasks of creating and controlling sounds.

### Instruments


Students could create instruments with parameters that could be changed
in real-time. Students can use this defined instruments to play notes

I used a Racket macro to create the instrument creation form.

An instrument corresponds to a Synthdef in SuperCollider. A Synthdef is
a composition of functions called ugens. ugens, which stands for unit
generator,

Instruments are associated with functions that play notes. A note
contains the instrument name and frequency. The functions can turn on a
note, turn it off, and change some instrument parameters, which will
only affect the playing note, while the note is playing.

### GUI elements to control parameters

One interesting feature I implemented is the ability to easily create
GUI elements to control note parameters. Currently there is a slider and
checkbox. The slider works by passing a label, start value, end,
initial, values, and a callback function that accepts the value of the
slider.

While this uses a lambda function, it could be easily changed to accept
a particular note and the parameter string to change.

I also added a slider funciton that can be used directly in the
definition of an instrument. This encourages experimentation with
parameter values.


### Filters

I added functions that can be used to chain effects, such as reverb and
low pass filters. An effect added after a previous effect will act on
the output of the previous effect. To implement this, I originally
planned on chaining nodes using SuperCollider's audio buses. However, I
found a ugen that could do that called replace-out that provides this
functionality. That is, replace-out can read, modify, and write to the
same bus.

### Scheduling notes

Using the Bundle feature of the OSC protocol, I could schedule events to
happen in the future. The function accepts the utc seconds and a
fraction to denote milliseconds, duration, and the note to play.

## Cross platform issues

SuperCollider has versions for Windows, Linux and OS X. On Windows and
OS X, SuperCollider doesn't need dependencies, but on Linux,
SuperCollider requires a Jack server to be running. Students will have
to install Jack, which is a different procedure depending on their Linux
distribution. It's one line in a terminal on a Debian based distribution
such as Ubuntu.

I added a function that starts the SuperCollider server if it's not
already started whenever the library is initialized. Racket has
functions to return the OS type, and I issue a system command to start
the server based on the OS. For Linux, I wrote a script to also start
the Jack audio server and connect SuperCollider's outputs to the system
audio output.
