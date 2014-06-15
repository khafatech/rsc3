
## SuperCollider Racket library


## Goal
I planned on writing a client for the SuperCollider (SC) music synthesis server for use in the Music Programming section of CSC123. Using the interface to SuperCollider, students will have access to real-time synthesis techniques.

## First hope
In the first quarter, after examining implementations in various languages, I set out on creating a new implementation in Racket from scratch. Quickly, however, it appeared that two quarters is not enough time, so I looked into porting the scheme implementation of a supercollider client, rsc3, to Racket.

### The Porting process

I first tried rsc3 in a scheme interpreter, including ikarus and DrRacket's r6rs mode (#lang r6rs). rsc3 ran with very little modification (one library funciton name was wrong). However, rsc3 could not be installed as a Racket package, due to a conflict of the way scheme and racket handle libraries. 
Installing rsc3 as a package is an important aspect since that is how students will install rsc3.

What I did is instead of using "#lang r6rs", which makes racket behave like a scheme interpreter confirming to the r6rs scheme standard, I used "#lang racket" and imported the rnrs library that has scheme functions in a racket environment. This way I could use Racket's provide and package functionality works.

The process of porting was relatively straightforward. I compiled a file, looked for errors, and fixed them. Some defined utility functions were not necessary because racket had native implementations.

### Challenges when porting
One tricky part was a difference in the list representations between r6rs scheme and Racket. Racket uses mutable pairs to construct lists, while scheme uses immutable pairs. This makes it so that a list created in Racket cannot be used in a scheme function that accepts a list. One way around this is to construct lists using mcons in Racket then pass it to a scheme function.



## Features added

It would be hard for first year students to use rsc3 directly, so I created simplified functions to perform common tasks of creating and controlling sounds.

### Instruments


One main feature of SuperCollider is the Synthdef.

Students could create instruments with parameters that could be changed in real-time.


## Outline

- scratch & porting rsc3
  - how I started
  - challenges

- features
  - instruments
  - sliders
  - effect chaining

- cross platform


