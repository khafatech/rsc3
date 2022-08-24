#lang scribble/manual
@(require (for-label racket))

@title{Syntax Shortcuts}
 syntactic sugar@section{categories}
  Language
@section{related}
  Overviews/SymbolicNotations

@section{section}
  Introduction

This file shows a number of syntax equivalences in the compiler.

Because of the multiple syntax equivalences, some expressions can be written in many different ways. All of the following do the same thing and compile to the same code.

@racketblock[
// new argument syntax

(1..10).collect({|n| n.squared }); // receiver syntax
collect((1..10), {|n| n.squared }); // function call syntax
(1..10).collect {|n| n.squared }; // receiver syntax with trailing function arg
collect ((1..10)) {|n| n.squared }; // function call syntax with trailing function arg
(1..10) collect: {|n| n.squared }; // binary operator syntax


// old argument syntax

(1..10).collect({ arg n; n.squared }); // receiver syntax
collect((1..10), { arg n; n.squared }); // function call syntax
(1..10).collect { arg n; n.squared }; // receiver syntax with trailing function arg
collect ((1..10)) { arg n; n.squared }; // function call syntax with trailing function arg
(1..10) collect: { arg n; n.squared }; // binary operator syntax


// partial application syntax

(1..10).collect( _.squared ); // receiver syntax
collect((1..10), _.squared ); // function call syntax
(1..10) collect: _.squared ; // binary operator syntax
::

You could even start expanding out the equivalent of (1..10) which is really a shortcut for ]

@racketblock[ series(1, nil, 10) ::. This could also be written ]

@racketblock[ 1.series(nil,10) ::. This adds another 26 variations to the 13 variations above.

]
@section{section}
  Objects, functions, messages and arguments

@section{subsection}
  functional and receiver notation
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ f(x, y) :: || ]

@racketblock[ x.f(y) ::
## ]

@racketblock[ f(g(x)) :: || ]

@racketblock[ x.g.f ::
::

]
@section{subsection}
  defining instance variable accessor methods
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[
Thing { var x;
    x { ^x }
    x_ { arg z; x = z; }
}
:: || ]

@racketblock[ Thing { var <>x; } ::
::

]
@section{subsection}
  calling an instance variable setter method
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ p.x_(y) :: || ]

@racketblock[ p.x = y :: or ]

@racketblock[ x(p) = y ::
::

]
@section{subsection}
  use a selector as binary operator
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ min(x, y) :: || ]

@racketblock[ x min: y ::
::

]
@section{subsection}
  instantiate object
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ Point.new(3, 4); :: || ]

@racketblock[ Point(3, 4) ::
::

]
@section{subsection}
  moving blocks out of argument lists
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ if (x<3, {\abc}, {\def}); :: || ]

@racketblock[ if (x<3) {\abc} {\def} ::
## ]

@racketblock[ z.do({|x| x.play }); :: || ]

@racketblock[ z.do {|x| x.play }; ::
## ]

@racketblock[ while({ a < b },{ a = a * 2 }); :: || ]

@racketblock[ while { a < b } { a = a * 2 }; ::
::

]
@section{subsection}
  shorter argument lists
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ { arg x; x < 2 } :: || ]

@racketblock[ {|x| x < 2 } ::
## ]

@racketblock[ { arg x = 123; x < 2 } :: || ]

@racketblock[ {|x = 123| x < 2 } ::
## ]

@racketblock[ { arg x = 10.rand; x < 2 } :: || ]

@racketblock[ {|x = (10.rand)| x < 2 } :: or ]

@racketblock[ {|x(10.rand)| x < 2 } ::
::
]
@section{note}
 
When using the new 
@racketblock[||:: syntax, the default value needs to be enclosed in parenthesis if it's not a literal.
::

]
@section{subsection}
  calling the 'value' method
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ f.value(x) :: || ]

@racketblock[ f.(x) ::
::

]
@section{subsection}
  calling performList
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ object.performList(\method, a, b, array) :: || ]

@racketblock[ object.method(a, b, *array) ::
::

]
@section{subsection}
  partial application
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ {|x| object.msg(a, x, b) } :: || ]

@racketblock[ object.msg(a, _, b) ::
## ]

@racketblock[ {|x,y| object.msg(a, x, y) } :: || ]

@racketblock[ object.msg(a, _, _) ::
## ]

@racketblock[ {|x| a + x } :: || ]

@racketblock[ a + _ ::
## ]

@racketblock[ {|x| [a, b, x] } :: || ]

@racketblock[ [a, b, _] ::
## ]

@racketblock[ {|x| (a: x) } :: || ]

@racketblock[ (a: _) ::
::


]
@section{section}
  Collections

@section{subsection}
  create a collection
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ Set.new.add(3).add(4).add(5); :: || ]

@racketblock[ Set[3, 4, 5] ::
## ]

@racketblock[ Array[3, 4, 5]; :: || ]

@racketblock[ [3, 4, 5] ::
::

]
@section{subsection}
  indexing elements
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ z.at(i) :: || ]

@racketblock[ z[i] ::
## ]

@racketblock[ z.put(i, y); :: || ]

@racketblock[ z[i] = y; ::
::

]
@section{subsection}
  creating Events
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ Event[\a -> 1, \b -> 2] :: || ]

@racketblock[ (a: 1, b: 2) ::
::

]
@section{subsection}
  creating Arrays with key-value pairs
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ [\a, 1, \b, 2] :: || ]

@racketblock[ [a: 1, b: 2] ::
::

]
@section{subsection}
  creating arithmetic series
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ Array.series(16,1,1) :: or ]

@racketblock[ series(1,nil,16) :: || ]

@racketblock[ (1..16) ::
## ]

@racketblock[ Array.series(6,1,2) :: or ]

@racketblock[ series(1,3,11) :: || ]

@racketblock[ (1,3..11) ::
::
There is also the similar syntax for creating an iterating link::Classes/Routine:: :
]
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ seriesIter(1,3,11) :: || ]

@racketblock[ (:1,3..11) ::
::

]
@section{subsection}
  accessing subranges of Arrays
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ a.copyRange(4,8) :: || ]

@racketblock[ a[4..8] ::
## ]

@racketblock[ a.copyToEnd(4) :: || ]

@racketblock[ a[4..] ::
## ]

@racketblock[ a.copyFromStart(4) :: || ]

@racketblock[ a[..4] ::
::

]
@section{section}
  Other shortcuts

@section{subsection}
  multiple assignment
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ x = z.at(0); y = z.at(1); :: || ]

@racketblock[ # x, y = z; ::
::

]
@section{subsection}
  accessing environment variables
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ 'myName'.envirGet :: || ]

@racketblock[ ~myName ::
## ]

@racketblock[ 'myName'.envirSet(9); :: || ]

@racketblock[ ~myName = 9; ::
::

]
@section{subsection}
  shorthand for Symbols
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ 'mySymbol' :: || ]

@racketblock[ \mySymbol ::
::

]
@section{subsection}
  creating a Ref
@section{table}
 
## instead of writing: || you can write:
## 
@racketblock[ Ref.new(thing) :: || ]

@racketblock[ `thing ::
::

]


