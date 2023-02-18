#lang scribble/manual
@(require (for-label racket))

@title{MultiLevelIdentityDictionary}
tree of dictionaries@section{related}
  Classes/IdentityDictionary
@section{categories}
  Collections>Unordered

@section{description}

A tree of IdentityDictionaries. Addresses within the tree are specified with a series of keys. link::Classes/Library:: is its most useful subclass.

@section{INSTANCEMETHODS}
 

@section{private}
 add, remove, removeFail, prChooseFrom, prPutTree, leaves, prNestedValuesFromDict, prRemoveAtPathRecursive, storeOn, printOn

@section{method}
 at
Retrieves a leaf node or nil if not found.

@section{method}
 put
Puts the item as a leaf node, internally creating new branches as needed to accommodate the list of keys.

@section{method}
 choose
Choose a branch at each level, descend the tree until a leaf is chosen.
By using arguments strong::key1, key2 ... keyN::, one can start at an address within the tree, descend the tree until a leaf is chosen.

@section{method}
 putTree
A way to insert objects into the tree with a syntax similar to the organization of the tree itself.

@racketblock[
//pseudo code:
putTree(key1,[
	key2a, item1-2a,
	key2b, item1-2b,
	[
		key3, item1-3
	] // etc...
]);
::

]
@section{method}
 removeAt
Remove only the item located by the path.

@section{method}
 removeEmptyAt
Remove the item located by the path. This might make the item's parent dictionary empty. In that case, it will remove the parent and continue up the chain, removing empty dictionaries as it goes. This is slower but cleaner.

@section{EXAMPLES}
 


@racketblock[
// Example of the difference between removeAt and removeEmptyAt

m = MultiLevelIdentityDictionary.new;
m.put(\a, \b, \c, 1);

m.removeAt(\a, \b, \c);
m	// note, \a and \b dictionaries remain

m.put(\a, \b, \c, 2);
m.removeEmptyAt(\a, \b, \c);
m	// now the entire MultiLevelIdentityDictionary is empty
::
]


