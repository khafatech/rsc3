#lang scribble/manual
@(require (for-label racket))

@title{Main}
@section{categories}
 Core>Kernel
 The concrete instance of Process@section{related}
  Classes/StartUp

@section{description}


Main is the concrete instance of link::Classes/Process:: (the runtime environment for the virtual machine and interpreter).
Main overrides some methods of Process. There are two methods of interest. One is named startup and is
called after the class library has been compiled. The other is called shutdown which gets called when the library gets re-compiled.

@section{method}
  thisProcess
The singleton instance of Main is available through the special keyword thisProcess.
For example, to find out what platform you're on:


@racketblock[
thisProcess.platform;	// --> e.g. "an OSXPlatform", "a LinuxPlatform", ...
::

]
@section{classMethods}
 

@section{subsection}
  SuperCollider version
These class methods tell you which version of SuperCollider you are running and whether that version complies to your required minimum / maximum settings:

@section{method}
 version

@section{returns}
  the current version as a human readable string

@section{method}
 versionAtLeast
check if we are running at least version maj.min

@racketblock[
Main.versionAtLeast( 3, 1 );
::
]
@section{returns}
  true or false

@section{method}
 versionAtMost
check if we are running version maj.min or older

@racketblock[
Main.versionAtMost( 3, 1 );
::
]
@section{returns}
  true or false

@section{instanceMethods}
 

@section{private}
 prArgv, prOpenUDPPort

@section{method}
 startup

Called after the class library has been compiled.

@section{discussion}
 
This calls the superclass' startup, which among other things initializes the link::Classes/AppClock:: and the top-level link::Classes/Environment::.

Main's startup then stores Server.default in the interpreter variable s, sets the platform default's link::Classes/GUI:: kit, calls a link::Classes/Platform:: specific startup method (for example, OSXPlatform's startup opens the server windows), and finally invokes StartUp.run.

To add your own startup functionalities, you could either edit the special startup-file (discussed in link::Reference/StartupFile::), or use StartUp.add as discussed in the link::Classes/StartUp:: help file.

@section{method}
 shutdown

Called after SuperCollider is quit or the class library is about to be re-compiled.

@section{discussion}
 
This will quit all audio link::Classes/Server:: instances, perform a platform specific shutdown (e.g. the HID subsystem is released), finally Process' shutdown method is called, resulting in successive calls to UI.shutdown, NetAddr.disconnectAll, File.closeAll, and Archive.write. To register your own shutdown code, use a call like this:


@racketblock[
ShutDown.add({ "Good bye!!".postln });
::

]
@section{method}
 run

Override this to do whatever you want, e. g. add a class extension file like this to the class library:


@racketblock[
+ Main {
	run { "myPatch.rtf".load }
}
::

]
@section{argument}
 newFunc
A link::Classes/Function:: or similar object to be set. When evaluated, this function will be passed the arguments time, replyAddr, and message, corresponding to the time the message was sent, the link::Classes/NetAddr:: of the sender, and the message itself as an link::Classes/Array::.

@section{method}
 addOSCRecvFunc

Register a link::Classes/Function:: to be evaluated whenever SuperCollider language (the client) receives an OSC message. This will not overwrite any previously registered functions.

@section{argument}
 func
A link::Classes/Function:: or similar object to be added. When evaluated, this function will be passed the arguments msg, time, replyAddr, and recvPort, corresponding to the message itself as an link::Classes/Array::, the time the message was sent, the link::Classes/NetAddr:: of the sender, and the port on which the message was received. Note that this order differs from that used by the deprecated method link::#-recvOSCfunc::.


@racketblock[
// post all incoming traffic except the server status messages
// basically the same as OSCFunc.trace
(
f = { |msg, time, replyAddr, recvPort|
	if(msg[0] != '/status.reply') {
		"At time %s received message % from % on port%\n".postf( time, msg, replyAddr, recvPort )
	}
};
thisProcess.addOSCRecvFunc(f);
);

// stop posting.
thisProcess.removeOSCRecvFunc(f);
::

]
@section{method}
 removeOSCRecvFunc

Remove a link::Classes/Function:: from the list of those evaluated whenever SuperCollider language (the client) receives an OSC message. This will leave any other registered functions in place.

@section{argument}
 func
A link::Classes/Function:: or similar object to be removed.

@section{method}
 replaceOSCRecvFunc

Replace a link::Classes/Function:: in the list of those evaluated whenever SuperCollider language (the client) receives an OSC message with a different one. This will leave any other registered functions in place.

@section{argument}
 func
The link::Classes/Function:: or similar object to be replaced.

@section{argument}
 newFunc
A link::Classes/Function:: or similar object to be replace the one being removed. When evaluated, this function will be passed the arguments time, replyAddr, recvPort, and message, corresponding to the time the message was sent, the link::Classes/NetAddr:: of the sender, the port on which the message was received, and the message itself as an link::Classes/Array::.

@section{method}
 openUDPPort
Attempt to open a new UDP port for receiving OSC traffic. If another application has already bound to the requested port this will fail. Once opened, ports remain bound until SC is recompiled.

If the port was already opened by SC it will return true directly without trying to open the port again.

@section{argument}
 portNum
An link::Classes/Integer:: indicating the port to attempt to bind.

@section{returns}
 A link::Classes/Boolean:: indicating whether the attempt was successful.


@racketblock[
thisProcess.openUDPPort(3000); // will return true or false.
thisProcess.openPorts; // returns all open ports
::

]
@section{method}
 openPorts
Get a collection of all active UDP ports, including the main sclang port 
@racketblock[NetAddr.langPort::.

]
@section{returns}
 A link::Classes/Set::.

@section{method}
 pid

@section{Returns}
  The operating system's pid (process ID) for the process.

@section{method}
 recompile

Recompiles the class library. This is equivalent to restarting SC. Currently macOS (SuperCollider.app) only.

@section{method}
 platform
Get the current link::Classes/Platform::

@section{method}
 argv
Get the command-line arguments passed to sclang.



