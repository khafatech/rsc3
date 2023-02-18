#lang scribble/manual
@(require (for-label racket))

@title{EnvironmentRedirect}
 base class for environment redirects@section{categories}
  Libraries>JITLib>Environments, Collections>Unordered, Live Coding
@section{related}
  Classes/Environment

@section{description}

Environment that redirects access (strong::put::) and assignment (strong::at::). It is used as a base class for redirecting Environments. For example uses, see link::Classes/LazyEnvir:: and link::Classes/ProxySpace::.

@section{ClassMethods}
 

@section{method}
 new
Create new environment redirect, if envir is given, it is used as a basis.

@section{subsection}
  replacing Environment class methods

EnvironmentRedirect implements some of the interface of link::Classes/Environment::

@section{method}
 push, pop, make, use

@section{InstanceMethods}
 

@section{method}
 envir
return or replace the source environment

@racketblock[
e = LazyEnvir.new;
e.put(\x, 9);
e.envir; // look into the envir itself: for a LazyEnvir it contains Maybe as placeholders
::

]
@section{subsection}
 redirecting objects

Overriding these methods, one can redirect where objects go when they are assigned to the space. This is done for example in link::Classes/LazyEnvir:: and link::Classes/ProxySpace::.

@section{method}
 at, put, localPut, removeAt

@section{method}
 dispatch
A function or object that is called when the environment is modified. The key and the changed object are passed as arguments.

@racketblock[
e = LazyEnvir.new;
e.dispatch = { |key, val| [key, val].postln };
e.put(\x, 9);
::

]
@section{subsection}
  replacing Environment instance methods

EnvironmentRedirect implements some of the interface of link::Classes/Environment::, which it can replace where needed.

@section{method}
 push, pop, make, use, do, clear, keysValuesDo, keysValuesArrayDo, findKeyForValue, sortedKeysValuesDo, choose, know, doesNotUnderstand


@section{section}
 Networking

EnvironmentRedirect and its subclasses can be used to dispatch assignment over a network. To do this, a dispatch function can be supplied - see Public in strong::JITLibExtensions:: quark.


