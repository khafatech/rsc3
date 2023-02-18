#lang scribble/manual
@(require (for-label racket))

@title{ObjectTable}
associate objects with IDs@section{categories}
  Collections>Unordered

@section{description}

An ObjectTable is used to associate an id with an object. This is useful
for enabling references to objects on remote systems via Open Sound Control.

@section{CLASSMETHODS}
 

@section{private}
 initClass

@section{method}
 add
Put an object in the main ObjectTable and generate an Integer id.
@section{argument}
 obj
the object to put in the table.

@section{method}
 put
Put an object in the main ObjectTable under a specific key.
@section{argument}
 key
a link::Classes/Symbol::.
@section{argument}
 obj
the object to put in the table.

@section{method}
 at
Get an object in the main ObjectTable.
@section{argument}
 id
an link::Classes/Integer:: or link::Classes/Symbol::.

@section{method}
 getID
Get the ID of an object in the table.
@section{argument}
 obj
an object in the table.

@section{INSTANCEMETHODS}
 

@section{method}
 add
Put an object in an ObjectTable and generate an Integer id.
@section{argument}
 obj
the object to put in the table.


