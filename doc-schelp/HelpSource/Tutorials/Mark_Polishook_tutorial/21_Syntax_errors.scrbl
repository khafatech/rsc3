#lang scribble/manual
@(require (for-label racket))

@title{21_Syntax_errors}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 Syntax and grammar

Before it actually runs a program, SuperCollider examines the code to ensure that syntax and grammar are correct. For example, are all variable names and/or keywords spelled correctly in a program? Are statements terminated by semi-colons?

If syntax or grammar errors are found, SuperCollider writes a notification to the post window. Such messages are descriptive but terse.


@racketblock[
* ERROR: Parse error
   in file 'selected text'
   line 1 char 2 :
  4,
-----------------------------------
* ERROR: Command line parse failed
nil
::

]
@section{section}
 Common errors

@section{numberedList}
 
## the name of a class or a variable is misspelled
## a variable is used before being declared
## a parenthesis or a square or curly brace is missing or used in the wrong context
## a required comma or semicolon is missing or used improperly
::

////////////////////////////////////////////////////////////////////////////////////////////////////

Two helpful commands in the SuperCollider Edit menu:

@section{numberedList}
 
## "Go to Line ..." transports you to the line number of your choice. Use this when an error message identifies the line number on which a problem occurred.

## "Find" searches for words or phrases. Use "Find" to locate code that has been identified in error messages or to replace all instances of an improperly spelled word.
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/22_Runtime_errors::


