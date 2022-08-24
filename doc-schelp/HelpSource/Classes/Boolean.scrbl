#lang scribble/manual
@(require (for-label racket))

@title{Boolean}
 abstract class whose instances represent a logical value@section{categories}
 Core
@section{related}
  Reference/Control-Structures

@section{description}

Boolean is the superclass of link::Classes/True:: and link::Classes/False:: which are the concrete realizations.
In code True and False are represented by the literal values 
@racketblock[true:: and ]

@racketblock[false::.

]
@section{instanceMethods}
 

@section{private}
  while
@section{private}
  storeOn
@section{private}
  trace
@section{private}
  printOn
@section{private}
  archiveAsCompileString

@section{method}
 xor

@section{returns}
  the exclusive or of the receiver and another Boolean.

@section{method}
 and

If the receiver is true then answer the evaluation of function.
If the receiver is false then function is not evaluated and the message answers false.

@section{method}
 or

If the receiver is false then answer the evaluation of function.
If the receiver is true then function is not evaluated and the message answers true.

@section{method}
 &&

@section{returns}
  true if the receiver is true and aBoolean is true.

@section{method}
 ||

@section{returns}
  true if either the receiver is true or aBoolean is true.

@section{method}
 nand

@section{returns}
  true unless both the operands are true (Sheffer stroke)

@section{method}
 not

@section{returns}
  true if the receiver is false, and false if the receiver is true.

@section{method}
 if

If the receiver is true, answer the evaluation of the trueFunc. If the receiver is false, answer the evaluation of the falseFunc.

@section{method}
 asInteger
@section{method}
 binaryValue

@section{returns}
  1 if the receiver is true, and 0 if the receiver is false.

@section{method}
 asBoolean
@section{method}
 booleanValue
@section{Returns}
  The receiver. The same message is understood by link::Classes/SimpleNumber:: and can be used to convert it to boolean.

@section{method}
 keywordWarnings

turn on/off warnings if a keyword argument is not found




