#lang scribble/manual
@(require (for-label racket))

@title{Class}
A Class describes the structure and implementation of a set objects which are its instances.@section{categories}
 Core>Kernel, Language>OOP

@section{ClassMethods}
 
@section{method}
  allClasses
@section{returns}
  an link::Classes/Array:: of all Classes

@section{InstanceMethods}
 

@section{method}
 browse

Open a graphical browser for this Class. Shows methods, arguments, variables, subclasses, and has buttons for navigating to the superclass, source, helpfile, etc.

@section{method}
 findMethod

Find the Method referred to by name. If not found, return nil.

@section{method}
 findRespondingMethodFor

As above, but climb the class tree to see if the method is inherited from a superclass. If not found, return nil.

@section{method}
 dumpAllMethods

Post all instance methods which instances of this class responds too, including inherited ones. 
@racketblock[this.class.dumpAllMethods:: will post all class methods which this class responds to.

]
@section{method}
 dumpByteCodes

Dump the byte codes of the named method.

@section{method}
 dumpClassSubtree

Post the tree of all Classes that inherit from this class.

@section{method}
 dumpInterface

Post all the methods defined by this Class and their arguments.

@section{method}
 dumpFullInterface

Post all the class and instance methods that this class responds to (i.e. those defined in this class and those inherited by it).

@section{method}
 help

Opens the help file for this Class if it exists.

@section{method}
 helpFilePath

Returns the path of this Class's helpfile as a String.

@section{method}
 helpFileForMethod

Opens the helpfile for the class in which the responding method is implemented.


@racketblock[
Array.helpFileForMethod('select'); // This will open the Collection helpfile
::

]
@section{method}
 asClass

Return this.

@section{method}
 asString

Return the name of the class as a String.


@section{subsection}
  Accessing

@section{method}
 name

A Symbol that is the name of the class.

@section{method}
 nextclass

The next class in a linked list of all classes.

@section{method}
 superclass

The Class from which this class directly inherits.

@section{method}
 superclasses

An Array of this class's superclasses, going back to Object.

@section{method}
 subclasses

An Array of the direct subclasses of this.

@section{method}
 allSubclasses

An Array of all subclasses of this.

@section{method}
 methods

An Array of the methods of this class.

@section{method}
 instVarNames

An Array of the names of the instance variables for this class.

@section{method}
 classVarNames

An Array of the names of the class variables for this class.

@section{method}
 iprototype

An Array of the initial values of instance variables.

@section{method}
 cprototype

An Array of the initial values of class variables.

@section{method}
 filenameSymbol

A Symbol which is a path to the file which defines the Class.



