#lang scribble/manual
@(require (for-label racket))

@title{ApplicationStart}
 register functions to be evaluated on Application start@section{related}
  Classes/StartUp, Classes/ServerBoot
@section{categories}
  Control, Platform>macOS (OS X)

@section{description}


Available in macOS SuperCollider.app only.

ApplicationStart allows you to register functions or objects to perform an action only  on application start.
The functions will be evaluated last; After the library has been compiled, the startup file has run and StartUp actions have been evaluated.

See also link::Classes/StartUp:: for functions that are evaluated emphasis::every:: time the ClassLibrary is recompiled.

@section{ClassMethods}
 

@section{method}
 add
Registers an object or function. Objects will be receive a strong::doOnApplcationStart:: message on application start. Functions will be evaluated.

@section{method}
 remove
Removes a function that was previously registered.

@section{method}
 run
Evaluates the functions or objects in order.

@section{Examples}
 


@racketblock[
SomeStartClass {
	*initClass {
		ApplicationStart.add {
			// something to do when the app has been launched...
		}
	}
}

// or...
SomeStartClass {
	*initClass {
		ApplicationStart.add(this);
	}
	*doOnApplicationStart { "something started".postln }
}
::
]


