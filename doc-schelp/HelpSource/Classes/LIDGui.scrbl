#lang scribble/manual
@(require (for-label racket))

@title{LIDGui}
 A GUI for an LID@section{categories}
  Platform>Linux, External Control>HID
@section{related}
  Classes/LID

@section{description}

This class creates a simple GUI for an LID device.


@section{CLASSMETHODS}
 

@section{METHOD}
  new
Create a new GUI.

@section{ARGUMENT}
  device
An instance of LID, for which to create the GUI.



@section{INSTANCEMETHODS}
 
@section{private}
  init, bkeys, rkeys, akeys, updater;


@section{METHOD}
  win
The window that the LIDGui is in.


@section{METHOD}
  device
The device for which this GUI is.


@section{EXAMPLES}
 


@racketblock[
LID.findAvailable;
LID.postAvailable; // pick one that you want to open, and fill in the vendor and product id in the next line:
d = LID.open( 2, 10 ); // trackpoint

g = LIDGui.new( d );

// or the shortcut method:
g = d.makeGui;
]


