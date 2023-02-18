#lang scribble/manual
@(require (for-label racket))

@title{Message}
 A message to an object@section{categories}
  Language
@section{related}
  Classes/MethodQuote

@section{description}

A message to an object, to be evaluated later.

@section{CLASSMETHODS}
 

@section{METHOD}
  new
@section{argument}
  receiver
the receiver of the message
@section{argument}
  selector
the method to be called
@section{argument}
  args
arguments to the call

@section{INSTANCEMETHODS}
 

@section{METHOD}
  receiver
the object to which the message is relayed

@section{METHOD}
  selector
the method to be called

@section{METHOD}
  args
the arguments to the call

@section{METHOD}
  value
send the message to the receiver and call the selector with the arguments
@section{argument}
   ... moreArgs

@section{private}
  storeArgs

@section{EXAMPLES}
 


@racketblock[
// an object
a = 36


// a message to the object
m = Message(a, \sqrt)

// deliver the message
m.value
// -> 6

// a message that lacks an argument
m = Message(a, '+')

// evaluate with the argument
m.value(6)
// -> 42

m.value(-13)
// -> 23
]


