#lang scribble/manual
@(require (for-label racket))

@title{Symbol}
unique name@section{categories}
 Core

@section{description}

A Symbol is a name that is guaranteed to be unique. They can be used to represent
symbolic constant values, link::Classes/Dictionary:: keys, etc.

Symbols are represented syntactically as literals which are described in link::Reference/Literals#Symbols::.

@section{subsection}
 Creating a Symbol

A symbol can be written by surrounding characters by single quotes (may include whitespace):


@racketblock['foo bar'::

Or by a preceding backslash (then it may not include whitespace):

]

@racketblock[\foo::

A String can be converted into a symbol:

]

@racketblock["arbeit".scramble.asSymbol;::

]
@section{classmethods}
 
@section{private}
 new

@section{instancemethods}
 

@section{subsection}
 Testing

@section{method}
 isClassName

Answer whether the symbol can be a class name. This does not say if the class exists.


@racketblock[
\Array.isClassName;
\Bauxite.isClassName;
::

]
@section{method}
 isMetaClassName

Answer whether the symbol can be meta class name. This does not say if the class exists.


@racketblock[
\Meta_Array.isMetaClassName;
::

]
@section{method}
 isSetter

Answer whether the symbol has a trailing underscore.


@racketblock[
'action_'.isSetter;
::

]
@section{method}
 isPrimitiveName

Answer whether the symbol is a valid primitive name


@racketblock[
'_SymbolIsClassName'.isPrimitiveName;
::

]
@section{subsection}
 Conversion

@section{method}
 asString

Convert to a String

@section{method}
 asInteger

Convert to an Integer

@section{method}
 asClass

Answer the Class named by the receiver.

@section{method}
 asSetter

Return a symbol with a trailing underscore added.

@section{method}
 asGetter

Return a symbol with a trailing underscore removed.

@section{method}
 ascii

return the ascii codes as an array

@section{method}
 asSpec

Convert to a ControlSpec

@section{method}
 asTuning

Convert to a Tuning

@section{method}
 asScale

Convert to a Scale

@section{subsection}
 Environments

Symbols are used as keys to look up objects in dictionaries and environments, but also in arrays.
See link::Classes/IdentityDictionary::, link::Classes/Environment::, link::Classes/Event::


@racketblock[
a = ();
a.put(\commune, 1871);
a.at(\commune);
::

]
@section{method}
 envirPut

put a value to the current environment using receiver as key

@section{method}
 envirGet

return a value from the current environment using receiver as key
@section{discussion}
 

@racketblock[
\foo.envirPut(100);
\foo.envirGet;
\foo.envirPut(nil);
::

]
@section{subsection}
 Math

Symbols respond to all unary and binary math operations by returning themselves. The result of any math operation between a Number or other math object and a Symbol is to return the Symbol. This allows for example operations on lists of notes which contain 'rest's to preserve the rests.


@racketblock[Pseq([1, 3, \rest, 2, 4] + 8);::

]
@section{method}
 applyTo

Use the symbol as a method selector and perform the message on firstArg, with args as arguments. This is used for mixing functions with method selectors (see also: Function).
@section{discussion}
 

@racketblock[
'%'.applyTo(2553, 345);
['+', '-', '*', { |a, b| a.rand + b.rand } ].choose.applyTo(2, 3);
::


]
@section{subsection}
 Synthesis

Inside SynthDefs and UGen functions, symbols can be used to conveniently specify control inputs of different rates and with lags (see:  NamedControl, ControlName, and Control).


@section{method}
 kr

Return a control rate NamedControl input with a default value (val), and if supplied, with a lag. If val is an array, the control will be multichannel.
@section{discussion}
 

@racketblock[
a = { SinOsc.ar(\freq.kr(440, 1.2)) }.play;
a.set(\freq, 330);
a.release;
a = { SinOsc.ar(\freq.kr([440, 460], 1.2)) }.play;
a.setn(\freq, [330, 367]);
a.release;
::

]
@section{method}
 ar

Return an audio rate NamedControl input with a default value (val), and if supplied, with a lag. If val is an array, the control will be multichannel.

@section{method}
 ir

Return an initialization rate NamedControl input with a default value (val). If val is an array, the control will be multichannel.

@section{method}
 tr

Return a TrigControl input with a default value (val). If val is an array, the control will be multichannel.
@section{discussion}
 

@racketblock[
a = { Ringz.ar(T2A.ar(\trig.tr), \freq.kr(500, 1), 0.8) }.play;
a.set(\freq, 330, \trig, 1);
a.set(\freq, 830, \trig, 1);
a.release;
::

]


