#lang scribble/manual
@(require (for-label racket))

@title{OSCFunc}
 Fast Responder for incoming Open Sound Control Messages@section{categories}
  External Control>OSC
@section{related}
  Guides/OSC_communication, Classes/OSCdef, Classes/OSCresponderNode, Classes/NetAddr

@section{description}

OSCFunc (and its subclass link::Classes/OSCdef::) registers one or more functions to respond to an incoming OSC message which matches a specified OSC Address. Many of its methods are inherited from its superclass link::Classes/AbstractResponderFunc::. OSCFunc supports pattern matching of wildcards etc. in incoming messages. For efficiency reasons you must specify that an OSCFunc will employ pattern matching by creating it with the link::#*newMatching:: method, or by passing a matching dispatcher to link::#*new::. For details on the Open Sound Control protocol, see http://opensoundcontrol.org/spec-1_0

@section{note}
  Use of the older classes OSCresponder, OSCresponderNode, and OSCpathResponder can be unsafe, since responders in user and class code can override each other unintentionally. OSCFunc and link::Classes/OSCdef:: are faster, safer, have more logical argument order, and support pattern matching and custom listening ports. Thus they are the recommended way of registering for incoming OSC messages as of SC 3.5. (See below for an example demonstrating OSCpathResponder-type arg matching.)::


@section{CLASSMETHODS}
 
@section{private}
  initClass, cmdPeriod

@section{METHOD}
  defaultDispatcher
Get or set the default dispatcher object for OSCFuncs (this is what you get if you pass nil as the dispatcher argument to link::#*new::). This object will decide if any of its registered OSCFuncs should respond to an incoming OSC message.

@section{returns}
  By default this will be an link::Classes/OSCMessageDispatcher::, but it can be set to any instance of an appropriate subclass of link::Classes/AbstractDispatcher::.

@section{METHOD}
  defaultMatchingDispatcher
Get or set the default matching dispatcher object for OSCFuncs (this is what you get if when you create an OSCFunc using link::#*newMatching::). This object will decide if any of its registered OSCFuncs should respond to an incoming OSC message using pattern matching.

@section{returns}
  By default this will be an link::Classes/OSCMessagePatternDispatcher::, but it can be set to any instance of an appropriate subclass of link::Classes/AbstractDispatcher::.

@section{METHOD}
  new
Create a new, enabled OSCFunc.

@section{argument}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. When evaluated it will be passed the arguments msg, time, addr, and recvPort, corresponding to the message as an link::Classes/Array:: in the form 
@racketblock[[OSCAddress, ...otherArgs]::, the time that the message was sent (plus the latency if the message was in a bundle), a link::Classes/NetAddr:: corresponding to the IP address of the sender, and an link::Classes/Integer:: corresponding to the port on which the message was received.

]
@section{argument}
  path
A link::Classes/Symbol:: indicating the path of the OSC address of this object. Note that OSCFunc demands OSC compliant addresses. If the path does not begin with a / one will be added automatically.

@section{argument}
  srcID
An optional instance of link::Classes/NetAddr:: indicating the IP address of the sender. If set this object will only respond to messages from that source.

@section{argument}
  recvPort
An optional link::Classes/Integer:: indicating the port on which messages will be received. If set this object will only respond to message received on that port. This method calls link::Classes/Main#-openUDPPort:: to ensure that the port is opened.

@section{argument}
  argTemplate
An optional link::Classes/Array:: composed of instances of link::Classes/Integer:: or link::Classes/Function:: (or objects which respond to the method link::Overviews/Methods#matchItem::) used to match the arguments of an incoming OSC message. If a Function, it will be evaluated with the corresponding message arg as an argument, and should return a link::Classes/Boolean:: indicating whether the argument matches and this OSCFunc should respond (providing all other arguments match). Template values of nil will match any incoming argument value.

@section{argument}
  dispatcher
An optional instance of an appropriate subclass of link::Classes/AbstractDispatcher::. This can be used to allow for customised dispatching. Normally this should not be needed.

@section{returns}
  A new instance of OSCFunc.

@section{METHOD}
  newMatching
A convenience method to create a new, enabled OSCFunc whose dispatcher will perform pattern matching on incoming OSC messages to see if their address patterns match this object's path.

@section{argument}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. When evaluated it will be passed the arguments msg, time, addr, and recvPort, corresponding to the message as an link::Classes/Array:: [OSCAddress, other args], the time that the message was sent, a link::Classes/NetAddr:: corresponding to the IP address of the sender, and an link::Classes/Integer:: corresponding to the port on which the message was received.

@section{argument}
  path
A link::Classes/Symbol:: indicating the path of the OSC address of this object. Note that OSCFunc demands OSC compliant addresses. If the path does not begin with a / one will be added automatically. Pattern matching will be applied to any incoming messages to see if they match this address. Note that according to the OSC spec, regular expression wildcards are only permitted in the incoming message's address pattern. Thus path should not contain wildcards. For more details on OSC pattern matching, see http://opensoundcontrol.org/spec-1_0

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
  A new instance of OSCFunc.

@section{METHOD}
  trace
A convenience method which dumps all incoming OSC messages.

@section{argument}
  bool
A link::Classes/Boolean:: indicating whether dumping is on or off.

@section{argument}
  hideStatusMsg
A link::Classes/Boolean:: indicating whether server status messages are excluded from the dump or not.

@section{INSTANCEMETHODS}
 

@section{private}
  init, printOn

@section{METHOD}
  path
Get the path of this OSCFunc's OSC Address.

@section{returns}
  A link::Classes/String::

@section{METHOD}
  recvPort
Get this OSCFunc's receiving port.

@section{returns}
  An link::Classes/Integer::



@section{EXAMPLES}
 


@racketblock[
n = NetAddr("127.0.0.1", NetAddr.langPort); // local machine

OSCFunc.newMatching({|msg, time, addr, recvPort| \matching.postln}, '/chat', n); // path matching
OSCFunc({|msg, time, addr, recvPort| \oneShot.postln}, '/chat', n).oneShot; // once only
OSCdef(\test, {|msg, time, addr, recvPort| \unmatching.postln}, '/chat', n); // def style

m = NetAddr("127.0.0.1", NetAddr.langPort); // loopback

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

//////// Use an argTemplate for finer grained matching

s.boot;
x = Synth(\default);
OSCFunc({ 'ended!'.postln }, '/n_end', s.addr, nil, [x.nodeID]).oneShot;
x.release(3);
::
]


