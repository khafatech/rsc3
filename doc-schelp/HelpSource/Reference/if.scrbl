#lang scribble/manual
@(require (for-label racket))

@title{if}
@section{categories}
 Core, Common methods
@section{related}
  Reference/Control-Structures
 conditional execution
@section{method}
  if

@section{section}
 example

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
::

UGens can also use if

the condition ugen is 0 / 1

]

@racketblock[
(
	{
		if( LFNoise1.kr(1.0,0.5,0.5) , SinOsc.ar, Saw.ar )
	}.play
)
::

]
@section{section}
  optimization

the functions will be inlined, which plucks the code from the functions and uses a more efficient jump statement.


@racketblock[
{
	if( 6 == 9,{
		"hello".postln;
	},{
		"hello".postln;
	})
}.def.dumpByteCodes

BYTECODES: (18)
  0   FE 06    PushPosInt 6
  2   FE 09    PushPosInt 9
  4   E6       SendSpecialBinaryArithMsg '=='
  5   F8 00 06 JumpIfFalse 6  (14)
  8   42       PushLiteral "hello"
  9   A1 00    SendMsg 'postln'
 11   FC 00 03 JumpFwd 3  (17)
 14   41       PushLiteral "hello"
 15   A1 00    SendMsg 'postln'
 17   F2       BlockReturn
a FunctionDef in closed FunctionDef
::




failure to inline due to variable declarations
]

@racketblock[
{

	if( 6 == 9,{
		var notHere;
		"hello".postln;
	},{
		"hello".postln;
	})

}.def.dumpByteCodes

WARNING: FunctionDef contains variable declarations and so will not be inlined.
   in file 'selected text'
   line 4 char 14 :
  		var notHere;•
  		"hello".postln;
-----------------------------------
BYTECODES: (12)
  0   FE 06    PushPosInt 6
  2   FE 09    PushPosInt 9
  4   E6       SendSpecialBinaryArithMsg '=='
  5   04 00    PushLiteralX instance of FunctionDef in closed FunctionDef
  7   04 01    PushLiteralX instance of FunctionDef in closed FunctionDef
  9   C3 0B    SendSpecialMsg 'if'
 11   F2       BlockReturn
a FunctionDef in closed FunctionDef
::

]

@racketblock[
{
	if( 6 == 9,{
		"hello".postln;
	},{
		"hello".postln;
	})
}.def.dumpByteCodes

BYTECODES: (18)
  0   FE 06    PushPosInt 6
  2   FE 09    PushPosInt 9
  4   E6       SendSpecialBinaryArithMsg '=='
  5   F8 00 06 JumpIfFalse 6  (14)
  8   42       PushLiteral "hello"
  9   A1 00    SendMsg 'postln'
 11   FC 00 03 JumpFwd 3  (17)
 14   41       PushLiteral "hello"
 15   A1 00    SendMsg 'postln'
 17   F2       BlockReturn
a FunctionDef in closed FunctionDef
::
]


