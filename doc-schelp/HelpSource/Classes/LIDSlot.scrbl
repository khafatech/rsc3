#lang scribble/manual
@(require (for-label racket))

@title{LIDSlot}
 Handles incoming LID data.@section{categories}
  Platform>Linux, External Control>HID
@section{related}
  Classes/LID

@section{description}



@section{CLASSMETHODS}
 

@section{private}
  new, initClass

@section{METHOD}
  slotTypeStrings
An IdentityDictionary mapping the evtTypes to descriptive strings.


@section{INSTANCEMETHODS}
 

@section{private}
  initSpec, init


@section{METHOD}
  postInfo
Post the slots properties in a nice, human readable way.

@section{METHOD}
  debug
Turn on or off the debug posting for this slot.


@section{METHOD}
  action
Set the action for this slot.


@section{METHOD}
  value
Get the current value of this slot, mapped according to its spec.

@section{METHOD}
  spec
The ControlSpec to map this slots value.


@section{METHOD}
  rawValue
Get the rawValue; the setter of this method is called from the primitive 
@racketblock[LID.prHandleEvent:: and should not be called by the user.



]
@section{METHOD}
  next
Convenience method to use a LIDSlot in a pattern; this will call the value of the slot.



@section{METHOD}
  createBus
Create a bus on the server. The slot's value will automatically be set to this bus on the server.

@section{ARGUMENT}
  server
The server on which to create the bus and forward the value to. By default this is 
@racketblock[Server.default::


]
@section{METHOD}
  bus
The bus on the server that this slot's value is mapped to.


@section{METHOD}
  freeBus
Free the bus on the server and remove the action to forward the value.

@section{METHOD}
  kr
JITLib support to access the bus in NodeProxy's. This will create the bus if it does not already exist.


@section{METHOD}
  device
The device to which this slot belongs.

@section{METHOD}
  type
The type of slot that this is.

@section{METHOD}
  code
The eventCode for this slot.

@section{METHOD}
  key
The key by which this slot is known in the spec of the device.


@section{EXAMPLES}
 


@racketblock[
LID.findAvailable;
LID.postAvailable; // pick one that you want to open, and fill in the vendor and product id in the next line:
d = LID.open( 2, 10 ); // trackpoint

s.boot;

d.slot(2,1).createBus;

Ndef( \checkbus, { d.slot(2,1).kr.poll } );

d.slot(2,1).freeBus;

Ndef( \checkbus ).clear;

]


