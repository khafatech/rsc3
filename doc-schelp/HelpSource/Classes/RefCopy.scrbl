#lang scribble/manual
@(require (for-label racket))

@title{RefCopy}
a reference to the copy of a value@section{categories}
 Core

@section{description}

A Ref  instance is an object with a single slot named 'value' that serves as a holder of  an object.
RefCopy, in difference to Ref, returns only copies of the value when next is called.
This can be useful when the original is to be kept unchanged.

see link::Classes/Ref:: for other methods.

@section{examples}
 


@racketblock[
a = [1, 2, 3];
x = RefCopy(a);
b = x.next;
b.put(0, 100); // modify b
a; // a is unchanged.
]


