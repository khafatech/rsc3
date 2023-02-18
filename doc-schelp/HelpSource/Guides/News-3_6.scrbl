#lang scribble/manual
@(require (for-label racket))

@title{News in 3.6}
 A summary of news in SC 3.6@section{categories}
  News

@section{SECTION}
  SuperCollider IDE

A new cross-platform SuperCollider coding environment.

Read link::Guides/SCIde##the guide::!


@section{SECTION}
  Language-side news

@section{subsection}
  More informative syntax errors
The parser now posts the details of syntax errors, example:

@racketblock[
[1,2,%,4];
123;
::
Posts the following error message:
teletype::
ERROR: syntax error, unexpected BINOP, expecting ']'
  in file 'selected text'
  line 1 char 6:

  [1,2,%,4];
       ^
  123;
-----------------------------------
ERROR: Command line parse failed
::

]
@section{subsection}
  Remove old syntax

@racketblock[#(a:1):: was valid syntax, but yielded nonsense results. This will now result in a syntax error instead.

]
@section{subsection}
  YAML/JSON parser
link::Classes/String#-parseYAML:: and link::Classes/String#-parseYAMLFile:: can be used to parse YAML or JSON.

@section{subsection}
  SynthDef optimizations for additive terms

When the SynthDef is compiled, separate additive ugens are combined via the new link::Classes/Sum3:: and
link::Classes/Sum4:: ugens.

@section{subsection}
  Basic dead code elimination for SynthDefs

The process of building synthdefs now performs a simple dead code elimination pass, which removes all
link::Classes/PureUGen:: instances without successor.

@section{subsection}
  Case sensitive String comparison
String comparison operators (teletype::==, !=, <=, >=, >, <::) are now case sensitive.

@racketblock[
"Foo" == "fOo"; // false
::

]
@section{subsection}
  SplayAz Bug Fix
Positioning of SplayAz was broken. The semantics of the TELETYPE::spread:: and TELETYPE::center:: arguments has
been changed in order to fix the behavior.

@section{subsection}
  Array primitives respect mutability
The array primitives now respect object mutability: writing to an immutable object now fails and changing an
immutable object with TELETYPE::add::, TELETYPE::addAll::, TELETYPE::insert::, TELETYPE::extend::,
TELETYPE::growClear:: and TELETYPE::overwrite:: will return a newly allocated object.

@section{SECTION}
  Server-side news

@section{subsection}
  SynthDef2 fileformat

@section{subsection}
  C++ base class for Unit Generators

A new C++ base class has been introduced, which extends the plain c-style Unit struct by a C++ interface.


