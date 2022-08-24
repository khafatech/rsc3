#lang scribble/manual
@(require (for-label racket))

@title{OSCdef}
 OSC response reference definition@section{categories}
  External Control>OSC>Dispatchers
@section{related}
  Guides/OSC_communication, Classes/OSCFunc, Classes/OSCresponderNode, Classes/NetAddr

@section{description}

OSCdef provides a global reference to the functionality of its superclass link::Classes/OSCFunc::. Essentially it stores itself at a key within a global dictionary, allowing replacement at any time. Most methods are inherited from its superclass.

@section{note}
  Use of the older classes OSCresponder, OSCresponderNode, and OSCpathResponder can be unsafe, since responders in user and class code can override each other unintentionally. link::Classes/OSCFunc:: and OSCdef are faster, safer, have more logical argument order, and support pattern matching. Thus they are the recommended way of registering for incoming OSC messages as of SC 3.5. (See below for an example demonstrating OSCpathResponder-type arg matching.)::


@section{CLASSMETHODS}
 
@section{private}
  initClass

@section{METHOD}
  all
Get the global dictionary of all OSCdefs.

@section{returns}
  An link::Classes/IdentityDictionary::.

@section{METHOD}
  new
Create a new, enabled OSCdef. If an OSCdef already exists at this key, its parameters will be replaced with the ones provided (args for which nil is passed will use the old values).

@section{argument}
  key
The key at which to store this OSCdef in the global collection. Generally this will be a link::Classes/Symbol::.

@section{argument}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. When evaluated it will be passed the arguments msg, time, addr, and recvPort, corresponding to the message as an link::Classes/Array:: in the form 
@racketblock[[OSCAddress, other args]::, the time that the message was sent (plus the latency if the message was in a bundle), a link::Classes/NetAddr:: corresponding to the IP address of the sender, and an link::Classes/Integer:: corresponding to the port on which the message was received.

]
@section{argument}
  path
A link::Classes/Symbol:: indicating the path of the OSC address of this object. Note that OSCdef demands OSC compliant addresses. If the path does not begin with a / one will be added automatically.

@section{argument}
  srcID
An optional instance of link::Classes/NetAddr:: indicating the IP address of the sender. If set this object will only respond to messages from that source.

@section{argument}
  recvPort
An optional link::Classes/Integer:: indicating the port on which messages will be received. If set this object will only respond to message received on that port. This method calls link::Classes/Main#-openUDPPort:: to ensure that the port is opened.

@section{argument}
  argTemplate
An optional link::Classes/Array:: composed of instances of link::Classes/Integer:: or link::Classes/Function:: (or objects which respond to the method link::Overviews/Methods#matchItem::) used to match the arguments of an incoming OSC message. If a Function, it will be evaluated with the corresponding message arg as an argument, and should return a link::Classes/Boolean:: indicating whether the argument matches and this OSCdef should respond (providing all other arguments match). Template values of nil will match any incoming argument value.

@section{argument}
  dispatcher
An optional instance of an appropriate subclass of link::Classes/AbstractDispatcher::. This can be used to allow for customised dispatching. Normally this should not be needed.

@section{returns}
  An instance of OSCdef.

@section{METHOD}
  newMatching
A convenience method to create a new, enabled OSCdef whose dispatcher will perform pattern matching on incoming OSC messages to see if their address patterns match this object's path.

@section{argument}
  key
The key at which to store this OSCdef in the global collection. Generally this will be a link::Classes/Symbol::.

@section{argument}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. When evaluated it will be passed the arguments msg, time, addr, and recvPort, corresponding to the message as an link::Classes/Array:: [OSCAddress, other args], the time that the message was sent, a link::Classes/NetAddr:: corresponding to the IP address of the sender, and an link::Classes/Integer:: corresponding to the port on which the message was received.

@section{argument}
  path
A link::Classes/Symbol:: indicating the path of the OSC address of this object. Note that OSCdef demands OSC compliant addresses. If the path does not begin with a / one will be added automatically. Pattern matching will be applied to any incoming messages to see if they match this address. Note that according to the OSC spec, regular expression wildcards are only permitted in the incoming message's address pattern. Thus path should not contain wildcards. For more details on OSC pattern matching, see http://opensoundcontrol.org/spec-1_0

@section{argument}
  srcID
An optional instance of link::Classes/NetAddr:: indicating the IP address of the sender. If set this object will only respond to messages from that source.

@section{argument}
  recvPort
An optional link::Classes/Integer:: indicating the port on which messages will be received.

@section{argument}
  argTemplate
An optional link::Classes/Array:: composed of instances of link::Classes/Integer:: or link::Classes/Function:: (or objects which respond to the method link::Overviews/Methods#matchItem::) used to match the arguments of an incoming OSC message. If a Function, it will be evaluated with the corresponding message arg as an argument, and should return a link::Classes/Boolean:: indicating whether the argument matches and this OSCFunc should respond (providing all other arguments match). Template values of nil will match any incoming argument value.

@section{returns}
  An instance of OSCdef.

@section{METHOD}
  freeAll
Clears and deactivates all OSCdefs from the global collection.

@section{INSTANCEMETHODS}
 
@section{private}
  addToAll, printOn

@section{METHOD}
  key
Get this OSCdef's key.

@section{returns}
  Usually a link::Classes/Symbol::.

@section{METHOD}
  free
Clears this OSCdef from the global collection and deactivates it.

@section{SECTION}
 Timing in incoming bundles
The time argument indicates the time the message was sent plus, if given, the latency added to the bundle:

@racketblock[
(
OSCdef(\x, { |msg, time|
	"reception time: %\nscheduling time: %\ndelta: %\n\n".postf(Main.elapsedTime, time, time - Main.elapsedTime)
}, \time);

n = NetAddr("127.0.0.1", 57120);
)

(
n.sendBundle(0.0, [\time]);
n.sendBundle(1.0, [\time]);
)
::



]
@section{EXAMPLES}
 


@racketblock[
n = NetAddr("127.0.0.1", 57120); // local machine

OSCdef(\test, {|msg, time, addr, recvPort| \unmatching.postln}, '/chat', n); // def style
OSCdef.newMatching(\test2, {|msg, time, addr, recvPort| \matching.postln}, '/chat', n); // path matching
OSCdef(\test3, {|msg, time, addr, recvPort| \oneShot.postln}, '/chat', n).oneShot; // once only


m = NetAddr("127.0.0.1", 57120); // loopback

m.sendMsg("/chat", "Hello App 1");
m.sendMsg("/chat", "Hello App 1"); // oneshot gone
m.sendMsg("/ch?t", "Hello App 1");
m.sendMsg("/*", "Hello App 1");
m.sendMsg("/chit", "Hello App 1"); // nothing

// Introspection

AbstractResponderFunc.allFuncProxies
AbstractResponderFunc.allEnabled
OSCdef(\test).disable;
AbstractResponderFunc.allDisabled

// change funcs
OSCdef(\test).enable;
OSCdef(\test, {|msg, time, addr, recvPort| 'Changed Unmatching'.postln}, '/chat', n); // replace at key \test
m.sendMsg("/chat", "Hello App 1");
OSCdef(\test).add(f = {\foo.postln}); // add another func
m.sendMsg("/chat", "Hello App 1");
OSCdef(\test).clear; // remove all functions
m.sendMsg("/chat", "Hello App 1");
OSCdef(\test).free;  // unregister OSCdef


//////// Use an argTemplate for finer grained matching

s.boot;
x = Synth(\default);
OSCdef(\watchForXEnd, { 'ended!'.postln }, '/n_end', s.addr, nil, [x.nodeID]).oneShot;
x.release(3);

// Args for which nil is passed will use the old values. While this facilitates swapping only parts of an OSCdef, such as the function, it also means you will have to free and recreate an OSCdef to remove parts.

OSCdef(\argtest, {|msg ... args| msg[1].postln}, '/hey', argTemplate: ['you']);
m.sendMsg("/hey", "you"); // oscdef will respond
m.sendMsg("/hey", "there"); // oscdef will not respond, filtered out by argTemplate

OSCdef(\argtest, argTemplate: nil); // this has no effect
m.sendMsg("/hey", "you"); // oscdef will respond
m.sendMsg("/hey", "there"); // oscdef will not respond, still filtered out by argTemplate

OSCdef(\argtest).free; // in order to get rid of the argTemplate, free the def...
OSCdef(\argtest, {|msg ... args| msg[1].postln}, '/hey'); // ... and recreate it.
m.sendMsg("/hey", "you"); // oscdef will respond
m.sendMsg("/hey", "there"); // oscdef will respond

::
]


