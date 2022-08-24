#lang scribble/manual
@(require (for-label racket))

@title{20_Debugging}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 My code doesn't work!

Code doesn't always run as one might hope. In such cases, SuperCollider sometimes tells you why and sometimes it doesn't. When SuperCollider does supply information, it's usually to describe either a syntax or a runtime error.

When SuperCollider doesn't give information, it's often because the code works but not as expected. Example of this are synths (nodes) that execute in the wrong order (a source placed after, instead of before, an effect) and adding instead of multiplying (biasing an amplitude instead of scaling it).

For context, here are links that describe debugging (fixing errors in code) in languages other than SuperCollider.

@section{list}
 
## http://www.elanus.net/book/debugging.html
## http://www.javaworld.com/javaworld/jw-07-1996/jw-07-javascript.html
## http://heather.cs.ucdavis.edu/~matloff/UnixAndC/CLanguage/Debug.html
::

go to link::Tutorials/Mark_Polishook_tutorial/21_Syntax_errors::


