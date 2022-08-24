#lang scribble/manual
@(require (for-label racket))

@title{TwoWayIdentityDictionary}
associative collection mapping keys to values and back@section{related}
 Classes/IdentityDictionary
@section{categories}
 Collections>Unordered

@section{description}

Similar to link::Classes/IdentityDictionary::, but allows to go efficiently from element to key and back. The contents of a TwoWayIdentityDictionary are strong::unordered::. You must not depend on the order of items.

@section{INSTANCEMETHODS}
 

@section{private}
 init

@section{method}
 getID
Find the key for a given object. If object is not element of the dictionary, it returns nil.

@section{EXAMPLES}
 


@racketblock[
a = TwoWayIdentityDictionary.new;
a.put(\test, 999);
a.put(["some", "strings"], 1200);
a.at(\test);
a.getID(999);
a.getID(1200);
a.getID(888); // nil
]


