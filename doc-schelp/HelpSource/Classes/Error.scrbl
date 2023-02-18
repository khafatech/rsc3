#lang scribble/manual
@(require (for-label racket))

@title{Error}
@section{categories}
 Core
superclass of all Errors
@section{description}

Error and its subclasses separate different types of error is that can occur in the SuperCollider program into distinct classes, so that they can be reported differently to the user.

Anywhere that an error must be reported to the user and execution must stop, an error object must be created and thrown.

@racketblock[
(
var file, path = "betcha-this-file-doesnt-exist.txt";
if((file = File(path, "r")).isOpen) {
	"File is % bytes long.\n".postf(file.length);
	file.close;
} {
	Error("File % could not be opened.".format(path)).throw;
};
)
::

For backward compatibility, ]

@racketblock[.die():: creates the error for you.

]

@racketblock[
"Nothing is really wrong; just committing suicide for fun. A Harold and Maude moment.".die;
::

Throwing an error object gives the caller the opportunity to catch, and possibly recover from, the error. See the Exception help file for more information about this.

]
@section{subsection}
 Error hierarchy

The following error classes exist in the main library.

@section{list}
 
## link::Classes/Error::
## link::Classes/DeprecatedError:: : this method is no longer supported.
## link::Classes/MethodError:: : generic error occurring within a method.
## link::Classes/DoesNotUnderstandError:: : the receiver does not understand the method name.
## link::Classes/BinaryOpFailureError:: : a binary operator cannot work with the operand classes.
## link::Classes/ImmutableError:: : attempted to modify an immutable object.
## link::Classes/MustBeBooleanError:: : a test (in if or while) returned a non-Boolean value.
## link::Classes/NotYetImplementedError:: : the method name exists, but isn't implemented yet.
## link::Classes/OutOfContextReturnError:: : a method return by ^ took place outside of a method.
## link::Classes/PrimitiveFailedError:: : an error occurred inside a primitive.
## link::Classes/ShouldNotImplementError:: : you called a method on a class that has no business implementing it.
## link::Classes/SubclassResponsibilityError:: : you called a method on an abstract class.
::

The exact inheritance tree looks like this:
@section{classtree}
 Error



