#lang scribble/manual
@(require (for-label racket))

@title{initClass}
@section{categories}
 Core>Kernel, Language>OOP
 Class initialization@section{related}
  Classes/Class

@section{method}
  initClass

When SuperCollider starts up, just after it has compiled the library, it initializes all the classes from Object down, a depth first traversal of subclasses.

In this method, any class that requires it may initialize classvars or other resources.

@section{method}
  initClassTree

In some cases you will require another class to be initialized before you can initialize your own.  You may depend on its resources (a classvar).  This can be accomplished by:


@racketblock[
YourClass {
    *initClass {
        Class.initClassTree(OtherClass);

        ..

        //

        ..
    }

    ..

}
::

Each class will be initialized once, and the OtherClass will have all of its subclasses initialized before the method returns.

For those methods that need pre-initialized data (such as path names) should defer this function by using StartUp:

]

@racketblock[
YourClass {
    *initClass {
        StartUp.add {
            ..
        }
    }

    ..

}
::

]


