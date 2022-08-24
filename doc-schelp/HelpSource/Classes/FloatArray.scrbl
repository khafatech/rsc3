#lang scribble/manual
@(require (for-label racket))

@title{FloatArray}
an array of 32-bit single precision floating point numbers@section{related}
 Classes/DoubleArray, Classes/Signal
@section{categories}
 Collections>Ordered

@section{description}

An array of 32-bit single precision floating point numbers.

Note that despite the "Float" in its name, FloatArray does not hold a sequence
of SuperCollider double precision link::Classes/Float##floats::, but rather
32-bit (single precision) floats. For a raw array of 64-bit floats, use
link::Classes/DoubleArray::.

FloatArray and its subclass link::Classes/Signal:: are commonly used to hold
audio data in SuperCollider. Since almost all audio has 16-bit or 24-bit
precision, using double precision floats for this purpose would be a waste of
space. In other words, FloatArray is meant for storage of large amounts of
lower-precision data, but it is not meant for highly accurate math operations.

The complete list of RawArray types in SuperCollider is:

@section{list}
 
## link::Classes/Int8Array:: - 8 bit integer
## link::Classes/Int16Array:: - 16 bit integer
## link::Classes/Int32Array:: - 32 bit integer
## FloatArray - 32 bit floating point
## DoubleArray - 64 bit floating point
## link::Classes/SymbolArray:: - symbols
::

@section{INSTANCEMETHODS}
 

@section{method}
 readFromStream


