#lang scribble/manual
@(require (for-label racket))

@title{AbstractDispatcher}
 Dispatches incoming messages to Functions@section{categories}
  External Control>Abstract Classes
@section{related}
  Classes/AbstractWrappingDispatcher, Classes/OSCMessageDispatcher, Classes/OSCMessagePatternDispatcher, Classes/MIDIMessageDispatcher, Classes/OSCFunc, Classes/OSCdef, Classes/MIDIFunc, Classes/MIDIdef, Classes/AbstractResponderFunc

@section{description}

Instances of AbstractDispatcher dispatch incoming messages (e.g. MIDI, OSC), to registered instances of link::Classes/AbstractResponderFunc::. There will be a default dispatcher for each message type, but one can have multiple dispatchers per type in order to implement custom dispatching for groups of ResponderFuncs. (The main example of this is OSC pattern matching with link::Classes/OSCMessagePatternDispatcher::.) Normally users do not need to access dispatcher instances directly.

Dispatchers must be registered at the appropriate central point (e.g. Main:recvOSCfunc for OSC messages). In this capacity their interfaces mimic link::Classes/Function:: and link::Classes/Function@section{List}
 .


@section{CLASSMETHODS}
 
@section{private}
  initClass

@section{METHOD}
  all
Get a collection of all currently active dispatchers.

@section{returns}
  An link::Classes/IdentitySet::.

@section{METHOD}
  new
Make a new dispatcher.

@section{returns}
  A new instance.


@section{INSTANCEMETHODS}
 
@section{private}
  init

@section{METHOD}
  add
Add a responder func to this dispatcher. Subclasses should override this to do any necessary bookkeeping. Generally this method should add this dispatcher as a dependant of the responder func, so that it can respond to any changes.

@section{argument}
  funcProxy
An instance of a subclass of link::Classes/AbstractResponderFunc:: to add.

@section{METHOD}
  remove
Remove a responder func from this dispatcher.

@section{argument}
  funcProxy
An instance of a subclass of link::Classes/AbstractResponderFunc:: to remove.

@section{METHOD}
  value
Evaluate an incoming message to see if it matches. Subclasses should override this message to take appropriate arguments. If a matching responder func is found, this method should call value on it, passing the message.

@section{METHOD}
  valueArray
As link::#-value:: above, but with the arguments passed as a single link::Classes/Array::. This method is needed so that subclasses can work in FunctionLists in central message registration points such as Main:recvOSCMessage.

@section{argument}
  args
An link::Classes/Array:: containing the message and appropriate arguments.

@section{METHOD}
  register
Register this dispatcher at the appropriate central point (e.g. Main:recvOSCfunc) to receive its message type. Subclasses should take care to not override any other registered objects. (So for example use Main:addOSCFunc for OSC messages rather than Main:recvOSCfunc_.) Generally speaking, dispatchers should register themselves automatically if needed when a responder func is added.

@section{METHOD}
  unregister
Remove this dispatcher from the appropriate central registration point, i.e. deactivate it. Generally speaking a dispatcher should unregister itself automatically when its last responder func is removed.

@section{METHOD}
  free
link::#-unregister:: this dispatcher and remove it from link::#*all::. After this the dispatcher should be discarded.

@section{METHOD}
  typeKey
Subclasses should override this method to return a key indicating the type of message this dispatcher responds to, e.g. 
@racketblock['OSC matched':: or ]

@racketblock['MIDI control'::.

]
@section{returns}
  A link::Classes/Symbol::.

@section{METHOD}
  update
Subclasses should override this to do any necessary updating when a dispatchers responder funcs indicate they have changed via the standard dependancy mechanism. The default implementation does nothing.



