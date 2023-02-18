#lang scribble/manual
@(require (for-label racket))

@title{Nil}
@section{categories}
 Core
Represents uninitialized data
@section{description}


Nil has a single instance named nil and is used to represent uninitialized data,
bad values, or terminal values such as end-of-stream.

@section{instancemethods}
 

@section{private}
 do, reverseDo, pairsDo, collect, select, reject, detect, collectAs, selectAs, rejectAs, pop, source, source_, changed, 	addDependant, removeDependant, release, update, swapThisGroup, performMsg, remove, seconds_, throw, superclassesDo, !?, play, printOn, storeOn, archiveAsCompileString, set, addDependant


@section{method}
 isNil
Answers true because this is nil. In class link::Classes/Object:: this message is defined to answer false.

@racketblock[
[1, 2, nil, 3].collect(_.isNil);
::

]
@section{method}
 notNil
Answer false. In class link::Classes/Object:: this message answers true.

@racketblock[
[1, 2, nil, 3].collect(_.notNil);
::

]
@section{method}
 ?
return first non-nil argument. Since this IS nil then return anObject.
In class link::Classes/Object::, ? is defined to answer the receiver.

@racketblock[
[1, 2, nil, 3].collect { |x| x ? -1 }; // replace nil by -1
::

]
@section{method}
 ??
If the receiver is nil, evaluate the function and return the result.  Since this IS nil, then evaluate the function and return the result. In class link::Classes/Object::, ?? is defined to answer the receiver.


@racketblock[
[nil, 2, nil, 3].collect { |x| x ?? { 100.rand } }; // replace nil by a random number
::

]
@section{method}
 booleanValue
Returns false.

@racketblock[
[1, 2, nil, 3].collect(_.booleanValue);
// compare:
[true, false, false, true].collect(_.binaryValue);
::

]
@section{method}
 rate
Returns nil.

@section{method}
 numChannels
Returns nil.

@section{method}
 isPlaying
Returns false.

@section{method}
 dependants
Returns an empty IdentitySet.

@section{method}
 awake
Returns nil.

@section{method}
 nextTimeOnGrid
Returns clock.nextTimeOnGrid.

@section{method}
 asQuant
Returns Quant.default.

@section{method}
 matchItem
Returns true.

See also link::Reference/matchItem::.



@racketblock[
[3, 2, 1].select(nil.matchItem(_))); // returns all
// compare:
[3, 2, 1].select([1, -1, 2].matchItem(_))); // returns only those in the key collection
::


]
@section{method}
 asCollection
Returns empty array.

@section{method}
 get
Returns prevVal.


@section{method}
 asSpec
Returns the default ControlSpec

@section{method}
 handleError
Either report error or inspect error and halt execution.

@section{method}
 push
Executes function.

@section{method}
 appendStream
Returns stream.

@section{subsection}
 Dependancy

All the messages for the Dependancy protocol (See class link::Classes/Object::) are defined in class Nil
to do nothing. This eliminates the need to check for nil when sending dependancy messages.

@section{subsection}
 Other Methods

Many other messages are defined in class Nil to do nothing. This eliminates the need to check for nil.

@section{subsection}
 Generic Collectors

There are a number of methods that can be applied to nil so that variables do not need to be initialized. Nil is just the "ground" (default case) from which the rest is bootstrapped.

@section{method}
 add
Returns an array with the value. This makes it unnecessary to initialize when adding to a variable.

@racketblock[
x = nil;
x = x.add(8);  // returns an array
x = x.add(7); // appends to the array
::

]
@section{method}
 addAll
Returns an array with all the values. This makes it unnecessary to initialize when adding to a variable.

@racketblock[
x = nil;
x = x.addAll([0, 2, 1, 2]);  // returns an array
x = x.addAll(7); // single objects are converted
::

]
@section{method}
 remove
For nil, it just returns nil. This makes it unnecessary to initialize when removing from a variable and adding to it again.

@racketblock[
x = nil;
x.remove(1); // stays nil, returns nil
x = x.addAll([0, 2, 1, 2]);  // returns an array
x.remove(1); // returns 1
x;
::

]
@section{method}
 ++
Returns an array with all the values. This makes it unnecessary to initialize when adding to a variable.

@racketblock[
x = nil;
x = x ++ [7, 8, 9]; // returns the receiver
x = x ++ [3, 0, 1, 2]; // adds to the array
::

]
@section{method}
 addFunc
Returns a function or a FunctionList.
This method is used to add multiple functions to already existing ones.

@racketblock[
f = nil;
f = f.addFunc { "----------****".scramble };
f = f.addFunc { 1.0.rand };
f.value;
::

]
@section{method}
 removeFunc
This method is used to remove multiple functions from already existing ones. For Nil, it just returns itself.


@racketblock[
f = { 1.0.rand };
g = { "you have produced a random value".postln };
f = f.addFunc(g);
f.value;
f.removeFunc(g);
f.value;
::


]
@section{method}
 transformEvent
This method is used to operate on events which are passed through the system as an argument.


@racketblock[
// for Nil: return the argument unmodified (an event).
nil.transformEvent((x: 8));
// for Dictionary (and thus for Event): add to the argument.
(y: 100, z: 1).transformEvent((x: 8));
// for Association: add the association to the event
(\a -> \x).transformEvent((x: 8));
// for Function: use the function receive the event as argument.
{ |event| event.use { ~x = ~x + 1 }; event }.transformEvent((x: 8));
::


]


