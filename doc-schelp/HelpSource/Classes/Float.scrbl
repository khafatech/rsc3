#lang scribble/manual
@(require (for-label racket))

@title{Float}
 Floating point number@section{categories}
  Math

@section{description}

A 64-bit double precision floating point number. Float inherits most of its behaviour from its superclass.

Note that despite its name, link::Classes/FloatArray:: only holds 32-bit (single precision) floats.
For a raw array of 64-bit floats, use link::Classes/DoubleArray::.

@section{ClassMethods}
 

@section{method}
  from32Bits
@section{returns}
  a new Float from a 32 bit word.

@section{method}
  from64Bits
@section{returns}
  a new Float from a 64 bit word.

@section{InstanceMethods}
 

@section{method}
  do
iterates a link::Classes/Function:: from 
@racketblock[0:: to ]

@racketblock[this-1::. See also: link::Classes/Integer#-do::, link::Classes/Collection#-do::
]
@section{argument}
  function
The function to iterate.

@section{method}
  reverseDo
iterates function from this-1 to 0
@section{argument}
  function
The function to iterate.

@section{method}
  coin
Perform a random test whose probability of success in a range from
zero to one is this and return the result.
@section{returns}
  a link::Classes/Boolean::
@section{discussion}
 

@racketblock[
0.2.coin; // 20 % chance for true.
::
See also: link::Guides/Randomness::

]
@section{method}
  isFloat
@section{returns}
  
@racketblock[true:: since this is a Float.

]
@section{method}
  asFloat
@section{returns}
  
@racketblock[this:: since this is a Float.

]
@section{method}
  as32Bits
@section{returns}
  an Integer which is the bit pattern of this as a 32bit single precision float

@section{method}
  high32Bits
@section{returns}
  an Integer which is the bit pattern of high 32 bits of the 64 bit double precision floating point value

@section{method}
  low32Bits
@section{returns}
  an Integer which is the bit pattern of high 32 bits of the 64 bit double precision floating point value

@section{method}
  asStringPrec
Returns a string representation of the number, with the desired precision (i.e. number of significant figures).
@section{discussion}
 

@racketblock[
// example:
pi
pi.asStringPrec(3)
pi.asStringPrec(6)
(pi * 0.0001).asStringPrec(3)
7.4.asStringPrec(5)
7.4.asStringPrec(50)
::
]


