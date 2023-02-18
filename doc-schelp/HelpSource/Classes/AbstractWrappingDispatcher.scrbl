#lang scribble/manual
@(require (for-label racket))

@title{AbstractWrappingDispatcher}
 A dispatcher which wraps functions to support multiple paramter matching@section{categories}
  External Control>Abstract Classes
@section{related}
  Classes/AbstractDispatcher, Classes/AbstractMessageMatcher, Classes/OSCMessageDispatcher, Classes/OSCMessagePatternDispatcher, Classes/MIDIMessageDispatcher, Classes/OSCFunc, Classes/OSCdef, Classes/MIDIFunc, Classes/MIDIdef, Classes/AbstractResponderFunc

@section{description}

AbstractWrappingDispatcher extends AbstractDispatcher to provide the facility to wrap response functions in specialised objects (instances of subclasses of link::Classes/AbstractMessageMatcher:: to efficiently support matching of multiple parameters. Its subclasses link::Classes/OSCMessageDispatcher:: and link::Classes/MIDIMessageDispatcher:: match using a flat dictionary lookup of the 'most significant' parameter, and only attempt to match other parameters if an initial match is found. This approach is faster than others (such as multi-level dictionaries) for most configurations.


@section{CLASSMETHODS}
 


@section{INSTANCEMETHODS}
 
@section{private}
  init

@section{METHOD}
  wrappedFuncs
Get a dictionary of all currently wrapped functions, stored using their owning responder funcs as keys.

@section{returns}
  An link::Classes/IdentityDictionary::.

@section{METHOD}
  add
Add a responder func to this dispatcher. Subclasses should extend this to do any necessary bookkeeping. Generally this method should add this dispatcher as a dependant of the responder func, so that it can respond to any changes.

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
  updateFuncForFuncProxy
This method is called within link::#-update:: to update any changes to one of this dispatcher's responder funcs' function(s). Users should not call this method directly, but subclasses may need to extend this method to do additional bookkeeping.

@section{argument}
  funcProxy
An instance of a subclass of link::Classes/AbstractResponderFunc::.

@section{METHOD}
  wrapFunc
Subclasses should override this method to implement wrapping of functions by instances of appropriate subclasses of link::Classes/AbstractMessageMatcher::.

@section{METHOD}
  getKeysForFuncProxy
Subclasses should override this to return an Array containing all the keys at which the specified responder func's functions are stored in this dispatchers active dictionary.

@section{argument}
  funcProxy
An instance of a subclass of link::Classes/AbstractResponderFunc::.

@section{returns}
  An link::Classes/Array::.

@section{METHOD}
  update
Subclasses of link::Classes/AbstractResponderFunc:: should call update on their dispatcher whenever their function (or something else significant) changes.

@section{argument}
  funcProxy
An instance of a subclass of link::Classes/AbstractResponderFunc::.

@section{argument}
  what
A link::Classes/Symbol:: indicating what has changed. Currently the only thing supported is 
@racketblock[\function::.

]
@section{METHOD}
  free
This method removes this dispatcher from its responder func's dependants dictionaries, and deactivates it. Users should only call this method if you are finished with this dispatcher.




