#lang scribble/manual
@(require (for-label racket))

@title{Collections}
 A hierarchical overview of Collection subclasses@section{categories}
  Collections
@section{related}
  Classes/Collection

SuperCollider has a rich hierarchy of Collection subclasses, detailed below. Subclasses of a given class are indented (sub-lists) relative to the class. Classes labelled "abstract" are not for direct use, but classes lower down the tree may inherit methods from them. For this reason it is important to consult the helpfiles of classes farther up the tree in order to get a complete list of available methods.

@section{section}
  Hierarchy

@section{classtree}
 Collection

@section{subsection}
  Notes

@section{definitionlist}
 
## link::Classes/@section{List}
 
|| is an expandable link::Classes/SequenceableCollection:: (compare to link::Classes/ArrayedCollection:: and link::Classes/Array::).
## link::Classes/Array::
|| is more efficient than link::Classes/@section{List}
 .
## link::Classes/SparseArray::
|| is an array of elements optimized for huge gaps between them.
## link::Classes/TwoWayIdentityDictionary::
|| is similar to link::Classes/IdentityDictionary:: and allows easy searching by both key and value. It is faster than link::Classes/IdentityDictionary:: on reverse lookup, but with more memory overhead.
## link::Classes/Environment::
|| is an link::Classes/IdentityDictionary::, one of which is always current; useful for creating sets of persistent variables.
## link::Classes/Event::
|| is a dictionary mapping names of musical parameters to their values.
## link::Classes/IdentitySet::
|| is an unordered collection of unidentical objects (compare to link::Classes/Set::).
::


