#lang scribble/manual
@(require (for-label racket))

@title{LazyEnvir}
 lazy environment@section{categories}
  Libraries>JITLib>Environments, Live Coding, Collections>Unordered
@section{related}
  Classes/Maybe, Classes/Fdef, Classes/Environment, Classes/ProxySpace, Overviews/JITLib

@section{description}

Environment with deferred evaluation and default values.

Consequently, calculations can be done with nonexisting objects which can then be assigned later.
Per default, a LazyEnvir returns instances of link::Classes/Maybe::. See also link::Classes/Fdef::.

@section{note}
 While the method put is treated as transparent and implicitly creates a placeholder, all other methods, like at, collect, do, etc. pass the placeholder. In order to retrieve the object itself, use .source - in order to reduce it to a value, use: value::


@racketblock[
e = LazyEnvir.new;
e.use { ~x = ~y + ~z };
e.at(\x);
e.at(\x).source; // the source is a binary operation (addition on the placeholders)
e.use { ~y = 5; ~z = 7 };
e.at(\x).value; // the value is 12
::

]
@section{InstanceMethods}
 

@section{method}
 put
Sets the value of the reference at key.

@section{method}
 at
Returns a reference to the object at key.


@racketblock[
l = LazyEnvir.push;

// default objects are created on access
~a;
~a.value; // defaults to nil

// operations on placeholders
(
~c = ~a + ~b;

~c.value; // doesn't fail, instead returns nil
)

// variables can be assigned later
(
~a = 800;
~b = { 1.0.rand };

~c.value;
)

// variables can be exchanged later
(
~b = { 1000.rand };
~c.value;
)
::

]
@section{method}
 copy
Copies the environment into a new one, with each placeholder being copied as well.

@section{method}
 localPut
Sets the value of the key directly. This method is mainly used internally.

@section{method}
 proxyClass
Specify what placeholder object the environment uses by supplying a class name (link::Classes/Symbol::). The default is a link::Classes/Maybe::. Any object that responds to the methods source, source_ and clear can be a placeholder.


@racketblock[

// making a pattern space using LazyEnvir

a = LazyEnvir.new;
a.proxyClass=\PatternProxy;

a.push;

~x = Pseq([1, 2, 30], 1);
~y = Pseq([~x], inf);

z = ~y.asStream;

z.next;
z.next;
z.next;
~x = Pseq([100, 2, 300], 1);
z.next;
z.next;
z.next;

a.pop;
::

]
@section{method}
 removeAt
Removes the placeholder from the environment and clears it.

@section{method}
 makeProxy
Returns a new placeholder object. This is used internally and can be overridden to implement other lazy environments.


