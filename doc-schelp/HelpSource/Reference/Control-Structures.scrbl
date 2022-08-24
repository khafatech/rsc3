#lang scribble/manual
@(require (for-label racket))

@title{Control Structures}
 flow control@section{categories}
  Language
@section{related}
  Classes/Boolean

Control structures in SuperCollider are implemented via message sends. Here are a few of those available.
See link::Reference/Syntax-Shortcuts:: for the various ways expressions can be written.

@section{section}
  Basic Control Structures

@section{method}
  if

Conditional execution is implemented via the 
@racketblock[if:: message. The ]

@racketblock[if:: message is sent to an expression which must return a link::Classes/Boolean:: value.
In addition it takes two arguments: a function to execute if the expression is true and another optional function to execute if the expression is false. The ]

@racketblock[if:: message returns the value of the function which is executed. If the falseFunc is not present and the expression is false then the result of the if message is nil.

]
@section{discussion}
 
Syntax

@racketblock[
if (expr, trueFunc, falseFunc);
::
--or--
]

@racketblock[
expr.if (trueFunc, falseFunc);
::

Examples
]

@racketblock[
if ( [false, true].choose,				// Boolean expression (chooses one at random)
	{ "expression was true".postln },	// true function
	{ "expression was false".postln }	// false function
)

(
var a = 1, z;
z = if (a < 5, { 100 },{ 200 });
z.postln;
)

(
var x;
if (x.isNil, { x = 99 });
x.postln;
)
::



]
@section{method}
  while

The while message implements conditional execution of a loop. If the testFunc answers true when evaluated, then the bodyFunc is evaluated and the process is repeated. Once the testFunc returns false, the loop terminates.

@section{discussion}
 
Syntax

@racketblock[
while ( testFunc, bodyFunc );
::
--or--
]

@racketblock[
testFunc.while( bodyFunc );
::

Example
]

@racketblock[
(
i = 0;
while ( { i < 5 }, { i = i + 1; "boing".postln });
)
::

while expressions are also optimized by the compiler if they do not contain variable declarations in the testFunc and the bodyFunc.


]
@section{method}
  for

The for message implements iteration over an integer series from a starting value to an end value stepping by one each time. A function is evaluated each iteration and is passed the iterated numeric value as an argument.

@section{discussion}
 
Syntax

@racketblock[
for ( startValue, endValue, function )
::
--or--
]

@racketblock[
startValue.for ( endValue, function )
::

Example
]

@racketblock[
for (3, 7, { arg i; i.postln }); // prints values 3 through 7
::

]
@section{method}
  forBy

The forBy selector implements iteration over an integer series with a variable step size. A function is evaluated each iteration and is passed the iterated numeric value as an argument.

@section{discussion}
 
Syntax

@racketblock[
forBy ( startValue, endValue, stepValue, function );
::
--or--
]

@racketblock[
startValue.forBy ( endValue, stepValue, function );
::

Example
]

@racketblock[
forBy (0, 8, 2, { arg i; i.postln }); // prints values 0 through 8 by 2's
::


]
@section{method}
  do

Do is used to iterate over a link::Classes/Collection::. Positive Integers also respond to 
@racketblock[do:: by iterating from zero up to their value. Collections iterate, calling the function for each object they contain. Other kinds of Objects respond to do by passing themselves to the function one time. The function is called with two arguments, the item, and an iteration counter.

]
@section{discussion}
 
Syntax

@racketblock[
do ( collection, function )
::
--or--
]

@racketblock[
collection.do(function)
::

Example
]

@racketblock[
[ 1, 2, "abc", (3@4) ].do({ arg item, i; [i, item].postln; });

5.do({ arg item; item.postln }); // iterates from zero to four

"you".do({ arg item; item.postln }); // a String is a collection of characters

'they'.do({ arg item; item.postln }); // a Symbol is a singular item

(8..20).do({ arg item; item.postln }); // iterates from eight to twenty

(8,10..20).do({ arg item; item.postln }); // iterates from eight to twenty, with stepsize two

Routine({ var i=10; while { i > 0 } { i.yield; i = i - 5.0.rand } }).do({ arg item; item.postln });
::

]
@section{Note}
  The syntax 
@racketblock[(8..20).do:: uses an optimization to avoid generating an array that is used only for iteration (but which would be discarded thereafter). The return value of ]

@racketblock[ (8..20).do({ |item| item.postln }) :: is 8, the starting value.
]

@racketblock[
(8..20) do: { |item| item.postln } // is not optimized, and returns the array.
::
::

]
@section{method}
  switch

Object implements a switch method which allows for conditional evaluation with multiple cases. These are implemented as pairs of test objects (tested using if this == test.value) and corresponding functions to be evaluated if true.
The switch statement will be inlined if the test objects are all Floats, Integers, Symbols, Chars, nil, false, true and if the functions have no variable or argument declarations. The inlined switch uses a hash lookup (which is faster than nested if statements), so it should be very fast and scale to any number of clauses.

@section{discussion}
 
Syntax

@racketblock[
switch (value,
        testvalue1, trueFunction1,
        testvalue2, trueFunction2,
        ...
        testvalueN, trueFunctionN,
        defaultFunction);
::

Examples
]

@racketblock[
(
var x=0; //also try 1
switch(x,0,{"hello"}, 1, {"goodbye"})
)

(
var x, z;
z = [0, 1, 1.1, 1.3, 1.5, 2];
switch (z.choose.postln,
	1,   { \no },
	1.1, { \wrong },
	1.3, { \wrong },
	1.5, { \wrong },
	2,   { \wrong },
	0,   { \true }
).postln;
)
::
or:
]

@racketblock[
(
var x, z;
z = [0, 1, 1.1, 1.3, 1.5, 2];
x = switch (z.choose)
	{1}   { \no }
	{1.1} { \wrong }
	{1.3} { \wrong }
	{1.5} { \wrong }
	{2}   { \wrong }
	{0}   { \true };
x.postln;
)
::

]
@section{method}
  case

Function implements a case method which allows for conditional evaluation with multiple cases. Since the receiver represents the first case this can be simply written as pairs of test functions and corresponding functions to be evaluated if true. Case is inlined and is therefore just as efficient as nested if statements.
@section{discussion}
 
Example

@racketblock[
(
var i, x, z;
z = [0, 1, 1.1, 1.3, 1.5, 2];
i = z.choose;
x = case
	{ i == 1 }   { \no }
	{ i == 1.1 } { \wrong }
	{ i == 1.3 } { \wrong }
	{ i == 1.5 } { \wrong }
	{ i == 2 }   { \wrong }
	{ i == 0 }   { \true };
x.postln;
)
::

]
@section{section}
  Other Control Structures

Using Functions, many control structures can be defined like the ones above. In the class link::Classes/Collection#iteration:: there are many more messages defined for iterating over Collections.

@section{section}
  Inline optimization


@racketblock[if::, ]

@racketblock[while::, ]

@racketblock[switch:: and ]

@racketblock[case:: expressions are optimized (i.e. inlined) by the compiler if they do not contain variable declarations in the functions. The optimization plucks the code from the functions and uses a more efficient jump statement:
]

@racketblock[
(
{
	if(6 == 9, {
		"hello".postln;
	},{
		"world".postln;
	})
}.def.dumpByteCodes
)

BYTECODES: (20)
  0   2C 06    PushInt 6
  2   2C 09    PushInt 9
  4   E6       SendSpecialBinaryArithMsg '=='
  5   F8 00 07 JumpIfFalse 7  (15)
  8   41       PushLiteral "hello"
  9   B0       TailCallReturnFromFunction
 10   C1 3A    SendSpecialMsg 'postln'
 12   FC 00 04 JumpFwd 4  (19)
 15   40       PushLiteral "world"
 16   B0       TailCallReturnFromFunction
 17   C1 3A    SendSpecialMsg 'postln'
 19   F2       BlockReturn
-> < closed FunctionDef >
::

Failure to inline due to variable declarations:
]

@racketblock[
(
{
	if(6 == 9, {
		var notHere;
		"hello".postln;
	},{
		"world".postln;
	})
}.def.dumpByteCodes
)

WARNING: FunctionDef contains variable declarations and so will not be inlined.
  in file 'selected text'
  line 4 char 14:

  		var notHere;

  		"hello".postln;
-----------------------------------
BYTECODES: (13)
  0   2C 06    PushInt 6
  2   2C 09    PushInt 9
  4   E6       SendSpecialBinaryArithMsg '=='
  5   04 00    PushLiteralX instance of FunctionDef - closed
  7   04 01    PushLiteralX instance of FunctionDef - closed
  9   B0       TailCallReturnFromFunction
 10   C3 0B    SendSpecialMsg 'if'
 12   F2       BlockReturn
-> < closed FunctionDef >
::

You can switch on and off the above warning (see: link::Classes/LanguageConfig#*postInlineWarnings::)

]

@racketblock[
LanguageConfig.postInlineWarnings_(true) // warn
LanguageConfig.postInlineWarnings_(false) // ignore it.
::


]


