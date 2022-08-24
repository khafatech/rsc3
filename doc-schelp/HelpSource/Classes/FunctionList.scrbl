#lang scribble/manual
@(require (for-label racket))

@title{FunctionList}
 A function that composes multiple functions into one@section{categories}
 Core>Kernel

@section{description}


A FunctionList is a function that composes multiple functions into one. This allows allow to deal transparently with several functions as if they were one and to append new functions at a later point. The functions are evaluated in the order they have in the FunctionList's array, which is by default the order in which they have been added to it.

See the link::Reference/Functions:: help file for a basic introduction.


@racketblock[
a = FunctionList.new;
fork { loop { 0.7.wait; a.value.postln } };
a.addFunc({ 800.rand });
a.addFunc({ "another".scramble });
::

]
@section{classMethods}
 

@section{method}
 new

create a new instance.
@section{argument}
  functions
An array of functions or objects

@section{instanceMethods}
 

@section{method}
 array

Set/get the FunctionList's array. New functions can be added to the array directly, e.g.

@racketblock[
x = FunctionList(...some functions);
x.array = x.array.insert(2, aFunction);
::

]
@section{method}
 addFunc

This message is used to be able to add to an Object, to a Function, or to a FunctionList.

@racketblock[nil.addFunc:: returns a function, if only one function is passed in the argument.
]

@racketblock[function.addFunc:: then returns a FunctionList.

]
@section{method}
 removeFunc

remove a function from the list.

@section{returns}
  the last function when only one function is left, or 
@racketblock[nil:: when the last function was removed.

]
@section{discussion}
 

@racketblock[addFunc:: and ]

@racketblock[removeFunc:: are implemented for link::Classes/Nil::, link::Classes/Object:: and link::Classes/Function]
@section{List}
 


@racketblock[
nil.addFunc(f) // returns f
obj.addFunc(f) // returns FunctionList([obj, f])
nil.removeFunc(f) // returns nil
obj.removeFunc(f) // returns nil, if f === obj, else obj is returned
::

]
@section{examples}
 


@racketblock[
// example

a = nil;
a = a.addFunc { |x="", y=""| "this % is an % example\n".postf(x, y); 1 };
a.postln;
a = a.addFunc { |x="", y=""| "there is no % that is %\n".postf(x, y); 2 };
a.value;
a.value("text", "extraordinary well written")
a.valueArray(["x", "y"]);
::

]

@racketblock[
// Function:do vs FunctionList:do (same)
a.do { |x| x.value };
{ 4 }.do { |x| x.value.postln }

(
().use {
	~x = "array";
	~y = "ominous";
	a.valueEnvir;
	a.valueEnvir("list");
}
)
::

]

@racketblock[
// removing a function
x = { "removal test".postln };
a.addFunc(x);
a.value;
a = a.removeFunc(x);
a.value;

// mathematics
a = nil;
a = a.addFunc({ 1.0.rand }).addFunc({ [0, 1].choose });
a = a.squared.linexp(0, 1, 1.0, 500);

a.value;
::

]

@racketblock[
// compatibility with function multichannel expansion
a = nil;
a = a.addFunc { |x=0| if(x > 0) { 7 } { 1000.rand } };
a = a.addFunc { |x=0| if(x < 0) { 17 } { -1000.rand } };
a.value

a = a.flop;
a.value
a.value([-1, 1])
::

]

@racketblock[
// typical usage in a Document action
// see also SCView: addAction example.

d = Document.current;
d.keyDownAction = { "You touched the keyboard.".postln };

d.keyDownAction = d.keyDownAction.addFunc {:x, x<-(1..), :: "already % times\n\n".postf(x) };


d.keyDownAction = nil;

// even if you don't know if there is already an action defined
// one can add one.

(
d.keyDownAction = nil;
d.keyDownAction = d.keyDownAction.addFunc {:x, x<-(1..), :: "already % times\n\n".postf(x) };

);

d.keyDownAction = nil;
::



]


