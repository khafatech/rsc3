#lang scribble/manual
@(require (for-label racket))

@title{08. Scoping and Plotting}
 Getting Started With SuperCollider@section{categories}
  Tutorials>Getting-Started
@section{related}
  Tutorials/Getting-Started/00-Getting-Started-With-SC

@section{section}
 Scoping Out Some Plots

Function has two other useful audio related methods. The first you've already seen some results of, Function-plot:


@racketblock[
{ PinkNoise.ar(0.2) + SinOsc.ar(440, 0, 0.2) + Saw.ar(660, 0.2) }.plot;
::

This makes a graph of the signal produced by the output of the Function. You can specify some arguments, such as the duration. The default is 0.01 seconds, but you can set it to anything you want.

]

@racketblock[
{ PinkNoise.ar(0.2) + SinOsc.ar(440, 0, 0.2) + Saw.ar(660, 0.2) }.plot(1);
::

This can be useful to check what's happening, and if you're getting the output you think you're getting.

The second method, Function-scope, shows an oscilloscope-like display of the Function's output.

So let's try to scope some audio:

]

@racketblock[
{ PinkNoise.ar(0.2) + SinOsc.ar(440, 0, 0.2) + Saw.ar(660, 0.2) }.scope;
::

This should open a window which looks something like this:

]
@section{image}
 Scoping-and-Plotting01.png::

This also works for multiple channels:


@racketblock[
{ [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)] }.scope;
::

Scope also has a zoom argument. Higher values 'zoom out'.

]

@racketblock[
{ [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)] }.scope(zoom: 10);
::

Like Function-plot, Function-scope can be useful for testing purposes, and to see if you're actually getting out what you think you are.

]
@section{section}
 Scoping on Demand

You can also scope the output of the server at any time, by calling 'scope' on it.


@racketblock[
{ [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)] }.play;
s.scope;
::

You can do the same thing by clicking on the server window and pressing the 's' key.

For more information see:

link::Classes/Function::, link::Classes/Server::, link::Classes/Stethoscope::

]
@section{section}
 Suggested Exercise

Experiment with scoping and plotting some of the Function examples from earlier sections, or some Functions of your own creation. Try experimenting with different duration or zoom values.

____________________

This document is part of the tutorial strong::Getting Started With SuperCollider::.

Click here to go on to the next section: link::Tutorials/Getting-Started/09-Getting-Help::

Click here to return to the table of Contents: link::Tutorials/Getting-Started/00-Getting-Started-With-SC::


