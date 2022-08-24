#lang scribble/manual
@(require (for-label racket))

@title{Library}
keeping objects in a central place@section{related}
 Classes/Archive, Classes/LibraryBase
@section{categories}
  Collections

@section{description}

Library is a global MultiLevelIdentityDictionary.
The Library can be used as a place to store data that you want globally accessible. It is an alternative to using class variables. It is a nice place to store menus, annotations, and commonly reusable functions.

@section{CLASSMETHODS}
 

@section{private}
 initClass

@section{method}
 postTree
Post a formatted description of the entire library.

@racketblock[
Library.postTree;
::

]
@section{method}
 put
The last argument to put is the object being inserted:

@racketblock[
Library.put(\multi, \level, \addressing, \system, "i'm the thing you are putting in here");
Library.at(\multi, \level, \addressing, \system).postln;
Library.atList([\multi, \level, \addressing, \system]).postln;
::

]


