#lang scribble/manual
@(require (for-label racket))

@title{Association}
relate two objects@section{categories}
 Collections

@section{description}

Associates a key with a value.
Associations can be created via the -> operator which is defined in class link::Classes/Object::.

Associations are used internally in link::Classes/Dictionary::.

@section{CLASSMETHODS}
 

@section{method}
 new
Create an Association between two objects.

@racketblock[
(
x = 'name' -> 100;
x.postln;
)
::
]
@section{argument}
 key
any object
@section{argument}
 value
any object

@section{INSTANCEMETHODS}
 

@section{subsection}
 Accessing

@section{method}
 key
the key object.

@section{method}
 value
the value object.

@section{subsection}
 Testing

@section{method}
 ==
Compare the keys of two Associations.

@section{method}
 <
Compare the keys of two Associations.

@section{method}
 hash
Compute the hash value of the Association.

@section{subsection}
 Writing to streams

@section{method}
 printOn
Write a string representation to the stream.

@section{method}
 storeOn
Write a compilable string representation to the stream.

@section{EXAMPLES}
 


@racketblock[
// associations can be a good way to store named data in order:
(
a = [\x -> 700, \y -> 200, \z -> 900];

fork {
	a.do { |assoc|
		assoc.key.postln;
		assoc.value.postln;
		(freq: assoc.value).play;
		2.wait;
	}
};
)
::
]


