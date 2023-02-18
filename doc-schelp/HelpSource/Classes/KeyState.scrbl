#lang scribble/manual
@(require (for-label racket))

@title{KeyState}
 Respond to the state of a key@section{related}
  Classes/MouseButton, Classes/MouseX, Classes/MouseY
@section{categories}
   UGens>User interaction


@section{description}

Respond to the state of a key.

@section{note}
 
Note that this UGen does not prevent normal typing. It therefore may be
helpful to select a GUI window rather than an SC document when using
KeyState, as the latter will be altered by any keystrokes.
::

@section{classmethods}
 

@section{method}
 kr

@section{argument}
 keycode
The keycode value of the key to check. This corresponds to the
keycode values passed into the keyDownActions of SCViews. See the
example below.

@section{argument}
 minval
The value to output when the key is not pressed.

@section{argument}
 maxval
The value to output  when the key is pressed.

@section{argument}
 lag
A lag factor.

@section{instancemethods}
 
@section{private}
  signalRange

@section{Examples}
 


@racketblock[
s.boot;

// execute the code below to find out a key's keycode
// the char and keycode of any key you press will be printed in the post window
(
w = Window.new("I catch keystrokes");
w.view.keyDownAction = { arg view, char, modifiers, unicode, keycode;  [char, keycode].postln; };
w.front;
)

// then execute this and then press the 'j' key
(
w.front; // something safe to type on
{ SinOsc.ar(800, 0, KeyState.kr(38, 0, 0.1)) }.play;
)
::

]


