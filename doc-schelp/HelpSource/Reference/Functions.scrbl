#lang scribble/manual
@(require (for-label racket))

@title{Functions}
 lambda expressions@section{categories}
  Language
@section{related}
  Classes/Function, Classes/AbstractFunction, Classes/FunctionDef

@section{section}
  Introduction

A link::Classes/Function:: is an expression which defines operations to be performed when it is sent the 
@racketblock[value:: message. In functional languages, a function would be known as a lambda expression.
Function definitions are enclosed in curly brackets ]

@racketblock[{}::. Argument declarations, if any, follow the open bracket. Variable declarations follow argument declarations. An expression follows the declarations.
]

@racketblock[
{ arg a, b, c;  var d;   d = a * b; c + d }
::

Functions are not evaluated immediately when they occur in code, but are passed as values just like integers or strings.

A function may be evaluated by passing it the ]

@racketblock[value:: message and a list of arguments.

When evaluated, the function returns the value of its expression.
]

@racketblock[
f = { arg a, b; a + b };
f.value(4, 5).postln;
f.value(10, 200).postln;
::

An empty function returns the value nil when evaluated.
]

@racketblock[
{}.value.postln;
::

A function can be thought as a machine able to perform a task on demand, e.g. a calculator. The calculator can receive input (args) and can output a value, the result of the performed operations. The function definition can then be thought as the building of the calculator: once built, the calculator does nothing until it is requested to work (by passing the value method to a function).
The following figure depicts an empty function, input without output, output without input, and the general case with input and output.

]
@section{image}
 functions.png#Functions::

@section{section}
  Arguments

An argument list immediately follows the open curly bracket of a function definition. An argument list either begins with the reserved word 
@racketblock[arg::, or is contained between two vertical bars. If a function takes no arguments, then the argument list may be omitted.

Names of arguments in the list may be initialized to a default value using the following syntax forms. Arguments which are not explicitly initialized will be set to nil if no value is passed for them.

"arg" style, default value is a literal:
]

@racketblock[{ arg x = 1; .... } :: link::#[1]::

"arg" style, default value is an expression:
]

@racketblock[{ arg x = 10.rand; ... } :: link::#[2]::

"arg" style, default value is a literal but you want to treat it like an expression:
]

@racketblock[{ arg x = (2); ... } :: link::#[2]::

Pipe style, default value is a literal:
]

@racketblock[{ |x = 1| ... } :: link::#[1]::

Pipe style, default value is an expression:
]

@racketblock[{ |x = (10.rand)| ... } :: link::#[2]::

If the last argument in the list is preceded by three dots (an ellipsis), then all the remaining arguments that were passed will be assigned to that variable as an link::Classes/Array::. Arguments must be separated by commas.

examples:
]

@racketblock[
{ arg a, b, c=3; } // is equivalent to:

{ |a, b, c=3| }

{ arg x='stop', y, z=0; } // these args are initialised

{ arg a, b, c ... d; } // any arguments after the first 3 will be assigned to d as an Array
::

If you want all the arguments put in an Array
]

@racketblock[
arg ... z;
::

In general arguments may be initialized to literals or expressions, but in the case of Function:play or SynthDef:play, they may only be initialized to literals.
]

@racketblock[
// this is okay:

{ arg a = Array.geom(4, 100, 3); a * 4 }.value;

// this is not:

{ arg freq = Array.geom(4, 100, 3); Mix(SinOsc.ar(freq, 0, 0.1)) }.play; // silence

// but this is:
{ arg freq =  #[ 100, 300, 900, 2700 ]; Mix(SinOsc.ar(freq, 0, 0.1)) }.play; // silence
::

See link::Reference/Literals:: for more information.

]
@section{anchor}
 [1]::
@section{subsection}
  [1] Literal argument defaults

Argument defaults that are literals are stored as part of the link::Classes/FunctionDef::. Arguments passed at runtime -- including nil -- always override the defaults:


@racketblock[
f = { arg x = 1; x };
f.value(2);  // prints 2

f.value;   // prints 1

f.value(nil);  // prints nil
::

]
@section{anchor}
 [2]::
@section{subsection}
  [2] Expression argument defaults

Since expressions are evaluated when the function is called, they cannot be stored in the link::Classes/FunctionDef::. They are executed only if the passed-in value is nil.


@racketblock[
f = { arg x = 10.rand; x };
f.value(100);  // prints 100

f.value;   // prints a number 0-9

f.value(nil);   // prints a number 0-9!
::

This means you can use expression-style to define a default that cannot be overridden by nil.

]

@racketblock[
f = { arg x = (3); x };
f.value(nil);   // prints 3
::

Note: Parentheses are required when initializing an argument to an expression, if the argument list is written inside ]

@racketblock[||:: pipes.

]

@racketblock[
(
var abc = 2;
{ arg x = abc+1; x }   // OK
)

(
var abc = 2;
{ |x = abc+1| x }
)
ERROR: Parse error
   in file 'selected text'
   line 1 char 10:
  { |x = abc•+1| x }
-----------------------------------
ERROR: Command line parse failed

(
var abc = 2;
{ |x = (abc+1)| x }   // OK
)

(
var abc = 2;
{ |x (abc+1)| x }   // In ||, the = may be omitted if () are there
)
::

This is because the pipe character also serves as a binary operator. Without parentheses, expressions such as the following are ambiguous:

]

@racketblock[
{ |a, b, c = a | b | c }
::

The following produce identical function definitions. Expression-style defaults are simply a shortcut syntax for the latter.

]

@racketblock[
{ arg x = 10.rand; x };

{	arg x;
	x ?? { x = 10.rand };
	x
};
::

]
@section{section}
  Variables

Following the argument declarations are the variable declarations. These may be declared in any order. Variable lists are preceded by the reserved word 
@racketblock[var::. There can be multiple var declaration lists if necessary. Variables may be initialized to default values in the same way as arguments. Variable declarations lists may not contain an ellipsis.

]

@racketblock[
var level=0, slope=1, curve=1;
::

]


