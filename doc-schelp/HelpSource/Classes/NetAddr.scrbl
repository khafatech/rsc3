#lang scribble/manual
@(require (for-label racket))

@title{NetAddr}
 network address@section{related}
  Classes/OSCFunc
@section{categories}
  Control, External Control>OSC

@section{ClassMethods}
 

@section{private}
 initClass

@section{method}
 new
create new net address.
@section{note}
 To send messages internally, loopback IP is used: "127.0.0.1"::

@section{argument}
 hostname
a link::Classes/String::, either an IP number (e.g. "192.168.34.56") or a hostname such as "otherHost.local".

@section{argument}
 port
a port number, like 57110.

@section{method}
 fromIP
create new net address using an integer IP number.

@section{method}
 langPort
Get the port sclang is currently listening on (may change after a recompile).

@section{method}
 localAddr
Get a NetAddr which corresponds to localhost and the port sclang is listening on.

@section{method}
 disconnectAll
close all TCP connections.

@section{method}
 broadcastFlag
Get or set the broadcast flag (whether or not broadcast messages can be sent).

@section{method}
 matchLangIP
Test an IP address to see if it matches that of one of the NICs on this computer.

@section{argument}
 ipstring
A link::Classes/String:: to test containing an IP number in dot decimal notation (e.g. "192.168.34.56").

@section{returns}
 A link::Classes/Boolean:: indicating whether a match was found.

@section{InstanceMethods}
 

@section{private}
 prConnect, prDisconnect, prConnectionClosed, recover

@section{method}
 sendMsg
Convert the argument list to an OSC message and send it to the NetAddr without a timestamp. The first argument is the OSC address, and the remaining arguments are the arguments in the OSC message. If you leave off the initial "/" in the OSC address, one will be prepended. The technical details of how sclang objects are converted to OSC messages is given in the link::Guides/OSC_communication:: helpfile.


@racketblock[
n = NetAddr("localhost", 12345);
n = s.addr;

// Example sending symbols, integers, and a float
n.sendMsg('/s_new', \default, 2000, 0, s.defaultGroup.nodeID, \freq, 60.midicps);

// The initial forward slash can be omitted
n.sendMsg(\n_set, 2000, \gate, 0);

// Using the performList syntax, you can use an array to store an OSC message
~msg = [\n_set, 2000, \gate, 0];
n.sendMsg(*~msg);
::

]
@section{method}
 sendBundle
send a bundle with timestamp to the addr.

@section{method}
 sendRaw
send a raw message without timestamp to the addr.

@section{method}
 connect
open TCP connection.

@section{argument}
 disconnectHandler
called when the connection is closed (either by the client or by the server).

@section{method}
 disconnect
close TCP connection.

@section{method}
 ip
returns the ip number (as a link::Classes/String::).

@racketblock[
n = NetAddr("localhost", 57110);
n.ip;
::

]
@section{method}
 isLocal
Test if this NetAddr ip number matches that of one of this hosts NICs, or the loopback address.
@section{returns}
 A link::Classes/Boolean::.

@section{Examples}
 


@racketblock[
n = NetAddr("127.0.0.1", 57120); // 57120 is sclang default port
r = OSCFunc({ arg msg, time; [time, msg].postln }, '/good/news', n);

n.sendMsg("/good/news", "you", "not you");
n.sendMsg("/good/news", 1, 1.3, 77);


n.sendBundle(0.2, ["/good/news", 1, 1.3, 77]);

r.free;
n.disconnect;

// note that different NetAddr objects with the same port and ip are independent.

r = OSCFunc({ "message arrived".postln }, '/x');

n = NetAddr("127.0.0.1", 57120);
n.sendMsg("/x")


u = NetAddr("127.0.0.1", 57120);
u.sendMsg("/x");

n.disconnect

u.sendMsg("/x");

r.free;
u.disconnect;
::


]


