#lang scribble/manual
@(require (for-label racket))

@title{Frame}
 Stack Frame@section{categories}
  Core>Kernel


@section{description}

Frames are used to contain the arguments, variables and other information for active Functions.

There are no instance variables or methods.

Since Frames are often created on the stack, it is too dangerous to allow access to them. Dangling pointers could result.

Frame instances are inaccessible to the user.

For error handling routines, the relevant information from a Frame can be transferred into a DebugFrame object which can safely be inspected.


@racketblock[
	this.getBackTrace.inspect
::


]


