#lang scribble/manual
@(require (for-label racket))

@title{UniqueID}
source for unique numbers@section{categories}
 Core

@section{classmethods}
 

@section{method}
 initClass

initialize the starting id.

@section{method}
 next

get next id, which is unique to the system

@section{examples}
 


@racketblock[
// example
UniqueID.next;
UniqueID.next;
UniqueID.next;
]


