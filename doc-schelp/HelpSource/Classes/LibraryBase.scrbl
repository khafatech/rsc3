#lang scribble/manual
@(require (for-label racket))

@title{LibraryBase}
Abstract global storage class@section{categories}
  Collections

@section{description}

Base class for link::Classes/Library:: and link::Classes/Archive::

There is only one global instance: Archive.global, or Library.global, which is initialized automatically in the subclasses.

@section{CLASSMETHODS}
 

@section{method}
 global
Subclass responsibility

@section{method}
 clear
Clear the dictionary

@section{method}
 at
Access the dictionary at a path with keys. The keys may be any object, but are usually link::Classes/Symbol::s.

@section{method}
 put
Store an object in the dictionary at a path, given as a list of keys and the object to be stored as last argument. The keys may be any object, but are usually link::Classes/Symbol::s.

@section{method}
 atList
Access the dictionary at a path, given as a list of keys. The keys may be any object, but are usually link::Classes/Symbol::s.

@section{method}
 putList
Store an object in the dictionary at a path, given as a list of keys and the object to be stored as last argument. The keys may be any object, but are usually link::Classes/Symbol::s.

@section{EXAMPLES}
 


@racketblock[
// an example from the subclass Library:

Library.put(\multi, \level, \addressing, \system, "i'm the thing you are putting in here");
Library.at(\multi, \level, \addressing, \system).postln;
Library.atList([\multi, \level, \addressing, \system]).postln;
::
]


