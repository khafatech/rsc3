#lang scribble/manual
@(require (for-label racket))

@title{Glossary}
 Glossary of some relevant words@section{categories}
  Help

@section{definitionlist}
 
## buffer
@section{keyword}
  buffer
|| A server-side header and array of 32 bit floating point sample data. Buffers are used for sound files, delay lines, arrays of global controls, and arrays of inter-synth patch points. Represented by the client-side class link::Classes/Buffer::.

## class
@section{keyword}
  class
|| A description of the state and behaviour of a set of objects.

## client
@section{keyword}
  client
|| SC is divided into two separate applications: The client and the server. The client is where the SuperCollider language is implemented and where one executes code. The server actually synthesizes the audio, contains the node tree of synths and groups and responds to Open Sound Control messages from the client. See link::Guides/ClientVsServer:: for more information.

## group
@section{keyword}
  group
|| A linked list of nodes. Groups provide ways to control execution of many nodes at once. A group is a kind of node. Colloquially one can understand a group as an ordered grouping of other nodes, which may include both synths and other groups. Represented by the client-side class link::Classes/Group::.

## interface
@section{keyword}
  interface
|| The set of messages to which an object responds.

## instance
@section{keyword}
  instance
|| One of the objects described by a class.

## instance variable
@section{keyword}
  variable
|| A part of an object's internal state

## message
@section{keyword}
  message
|| A request for an object to perform an operation.

## method
@section{keyword}
  method
|| A description of the operations necessary to implement a message for a particular class.

## MIDI
@section{keyword}
  midi
|| A protocol for sending music control data between synthesizers.

## node
@section{keyword}
  node
|| One point in a tree of nodes executed in a depth first traversal order by the synth engine. There are two types of nodes, synths and groups. These are represented by the client-side classes link::Classes/Synth:: and link::Classes/Group::, and their abstract superclass link::Classes/Node::. The node tree defines the order of execution for synths.

## object
@section{keyword}
  object
|| Something that has data, representing the object's state, and a set of operations that can be performed on the object.

## Open Sound Control
@section{keyword}
  OSC, opensoundcontrol
|| a protocol defined by CNMAT at UCBerkeley for controlling synthesizers. See http://opensoundcontrol.org/. SuperCollider communicates between the client and server using OSC messages over UDP or TCP.

## OSC
|| See Open Sound Control.

## polymorphism
@section{keyword}
  polymorphism
|| The ability for different kinds of objects to respond differently to the same message.

## protocol
@section{keyword}
  protocol
|| A set of messages that implement a specific kind of behaviour.

## receiver
@section{keyword}
  receiver
|| The object to which a message is sent.

## server
@section{keyword}
  server
|| SC is divided into two separate applications: The client and the server. The client is where the SuperCollider language is implemented and where one executes code. The server actually synthesizes the audio, contains the node tree of synths and groups and responds to Open Sound Control messages from the client. See See link::Guides/ClientVsServer:: for more information.

## synth
@section{keyword}
  synth
|| A sound processing module, based upon a particular synth definition. Similar to "voice " in other systems. Synths are referred to by a number. Represented by the client-side class link::Classes/Synth::.

## synth definition
@section{keyword}
  synthdef
|| A definition for creating new synths. Synth definitions are like a pattern or design for synths. Similar to "instrument" in other systems. Represented by the client-side class link::Classes/SynthDef::.

## TCP
@section{keyword}
  tcp
|| A protocol for streaming data over a network.

## UDP
@section{keyword}
  udp
|| A protocol for sending datagrams over a network.
::


