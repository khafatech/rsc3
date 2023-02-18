#lang scribble/manual
@(require (for-label racket))

@title{AbstractResponderFunc}
 Abstract superclass of responder func objects@section{categories}
  External Control>Abstract Classes
@section{related}
  Classes/OSCFunc, Classes/OSCdef, Classes/MIDIFunc, Classes/MIDIdef, Classes/AbstractDispatcher

@section{description}

AbstractResponderFunc is the abstract superclass of responder funcs, which are classes which register one or more functions to respond to a particular type of input. It provides some common functionality such as introspection. Its two main subclasses are link::Classes/OSCFunc::, and link::Classes/MIDIFunc::. By default responder funcs do not persist beyond Cmd-. (see link::#-permanent:: below).

Instances will register with a dispatcher (an instance of a subclass of link::Classes/AbstractDispatcher::), which will actually dispatch incoming messages to an instance's Function(s).


@section{CLASSMETHODS}
 

@section{private}
  initClass

@section{METHOD}
  allFuncProxies
Get all current instances of this classes concrete subclasses, sorted by type.

@section{returns}
  An link::Classes/IdentityDictionary::.

@section{METHOD}
  allEnabled
As allFuncProxies above, but only return those instances currently listening for input.

@section{returns}
  An link::Classes/IdentityDictionary::.

@section{METHOD}
  allDisabled
As allFuncProxies above, but only return those instances currently not listening for input.

@section{returns}
  An link::Classes/IdentityDictionary::.


@section{INSTANCEMETHODS}
 
@section{private}
  cmdPeriod, prFunc

@section{METHOD}
  func
Get or set this objects response function.

@section{returns}
  The getter returns a link::Classes/Function:: or similar object.

@section{METHOD}
  srcID
Get this object's source.

@section{returns}
  The return type will depend on subclass. For link::Classes/OSCFunc:: this will be a link::Classes/NetAddr::, for link::Classes/MIDIFunc:: a UID. This can can be nil, which indicates that the object will respond to any source.

@section{METHOD}
  enabled
Check if this object is currently responding to incoming messages.

@section{returns}
  A link::Classes/Boolean::.

@section{METHOD}
  dispatcher
et this object's dispatcher. This is the object which matches incoming messages with responder funcs. Instances can use custom dispatchers to support arbitrary matching schemes.

@section{returns}
  An instance of an appropriate subclass of link::Classes/AbstractDispatcher::. (The return type will depend on subclass.)

@section{METHOD}
  permanent
Get or set whether this responder func is persists when the user executes Cmd-. If false this will be disabled and removed from the global lists. The default is false.

@section{argument}
  bool
A link::Classes/Boolean:: indicating if this object is permanent.

@section{returns}
  The getter returns a link::Classes/Boolean::.

@section{METHOD}
  enable
Enable this object to receive incoming messages. This is done automatically at creation time.

@section{METHOD}
  disable
Stop this object from receiving incoming messages.

@section{METHOD}
  add
Add a new function to the list of functions which will be executed when this object receives an incoming message.

@section{argument}
  newFunc
A link::Classes/Function:: or similar object to be added.

@section{METHOD}
  remove
Remove a function from the list of functions which will be executed when this object receives an incoming message.

@section{argument}
  removeFunc
The link::Classes/Function:: to be removed.

@section{METHOD}
  gui
Open a subclass specific GUI. (Not yet implemented)

@section{returns}
  The GUI object.

@section{METHOD}
  oneShot
Indicate that this object should execute only once and then free itself.

@section{METHOD}
  fix
A synonym for link::#permanent::

@section{METHOD}
  free
Disable this object and remove it from the global lists. This should be done when you are finished using this object.

@section{METHOD}
  clear
Remove all active functions from this object's function list.


@section{EXAMPLES}
 

See link::Classes/OSCFunc:: and link::Classes/MIDIFunc::.


