#lang scribble/manual
@(require (for-label racket))

@title{ContiguousBlockAllocator}
 for better handling of dynamic allocation@section{related}
  Classes/Server, Classes/PowerOfTwoAllocator
@section{categories}
  Control

@section{description}


A more robust replacement for the default server block allocator, link::Classes/PowerOfTwoAllocator::. May be used in the link::Classes/Server:: class to allocate audio/control bus numbers and buffer numbers.

To configure a server to use ContiguousBlockAllocator, execute the following:

@racketblock[
aServer.options.blockAllocClass = ContiguousBlockAllocator;
::
Normally you will not need to address the allocators directly. However, ContiguousBlockAllocator adds one feature not present in PowerOfTwoAllocator, namely the emphasis::reserve:: method.

]
@section{ClassMethods}
 

@section{method}
 new
Create a new allocator with emphasis::size:: slots. You may block off the first emphasis::pos:: slots (the server's audioBusAllocator does this to reserve the hardware input and output buses).

@section{InstanceMethods}
 

@section{private}
 prReserve, prSplit

@section{method}
 alloc
Return the starting index of a free block that is emphasis::n:: slots wide. The default is 1 slot.

@section{method}
 free
Free a previously allocated block starting at emphasis::address::.

@section{method}
 reserve
Mark a specific range of addresses as used so that the alloc method will not return any addresses within that range.




