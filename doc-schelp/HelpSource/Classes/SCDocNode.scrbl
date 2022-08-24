#lang scribble/manual
@(require (for-label racket))

@title{SCDocNode}
 An SCDoc parsed document node@section{related}
  Classes/SCDoc
@section{categories}
  HelpSystem

@section{description}

This class is used internally by link::Classes/SCDoc:: to represent a node in the parsed document tree returned by the  parser. It has an id symbol, optional text and optional children.

@section{classmethods}
 

@section{method}
  new
Create a new node

@section{instancemethods}
 

@section{private}
  addDivAfter, makeDiv, notPrivOnly, printOn, sort, sortClassDoc

@section{method}
  id
The node ID. A link::Classes/Symbol::

@section{method}
  text
Text associated with this node. A link::Classes/String:: or nil.

@section{method}
  children
Children of this node. A link::Classes/Array:: or nil.

@section{method}
  merge
Merge another document node tree with this one. Used by document additions (*.ext.schelp)
@section{argument}
  root2
Another SCDocNode instance.

@section{method}
  findChild
Find the first child of this node with specified id.
@section{argument}
  id
A link::Classes/Symbol::


