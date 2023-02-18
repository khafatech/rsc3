#lang scribble/manual
@(require (for-label racket))

@title{Server}
 Object representing a server application@section{categories}
  Server>Abstractions
@section{related}
  Classes/ServerOptions, Reference/Server-Architecture, Reference/Server-Command-Reference

@section{description}


A Server object is a representation of a server application. It is used to control scsynth (or supernova) from the SuperCollider language. (See link::Guides/Server-Guide::, as well as link::Guides/ClientVsServer:: for more details on the distinction.) It forwards OSC messages and has a number of allocators that keep track of IDs for nodes, buses and buffers.

The server application represented by a Server object might be running on the same machine as the sclang, or it may be running on a remote machine.

Most of a Server's options are controlled through its instance of ServerOptions. See the link::Classes/ServerOptions:: helpfile for more detail.

A server also holds an instance of a link::Classes/Recorder:: (for recording output into a file) and a link::Classes/Volume:: (master level).

@section{note}
 
By default, there is always one default server, which is stored in the interpreter variable 's'. E.g. you can boot the defult server by calling 
@racketblock[s.boot::
::

]

@racketblock[
s.boot;
{ SinOsc.ar * 0.1 }.play(s); // play a synth on the server
::

The server application may be in three different states: running, not running, or unresponsive (for example while loading large files from disk).

]

@racketblock[
s.serverRunning // returns true if it is true
::


]
@section{ClassMethods}
 
@section{private}
  initClass

@section{method}
  new
Create a new Server instance.

@section{argument}
  name
a symbol;  each Server object is stored in one global classvariable under its name.

@section{argument}
  addr
an optional instance of link::Classes/NetAddr::, providing host and port.
The default is the localhost address using port 57110; the same as the local server.

@section{argument}
  options
an optional instance of link::Classes/ServerOptions::. If 
@racketblock[nil::, an instance of ServerOptions will be created, using the default values.

]
@section{argument}
  clientID
an integer. In multi-client situations, every client can be given separate ranges for link::Classes/Node##Nodes::, link::Classes/Buffer##Buffers::, or link::Classes/Bus##Busses::. In normal usage the server will supply an ID automatically when a client registers for link::#-notify#notifications:: so you should not need to supply one here. N.B. In multi-client situations you will need to set the link::Classes/ServerOptions#-maxLogins:: to at least the number of clients you wish to allow. This must be the same in the Server instances on every client.

@section{method}
  remote
Create a new Server instance corresponding to a server app running on a separate machine. This method assumes the remote app has been booted, and starts listening immediately. You should not call link::#-boot:: on an instance created using this method.

@section{argument}
  name
a symbol;  each Server object is stored in one global classvariable under its name.

@section{argument}
  addr
an optional instance of link::Classes/NetAddr::, providing ip address of the remote machine and port the app is listening on.

@section{argument}
  options
an optional instance of link::Classes/ServerOptions::. If 
@racketblock[nil::, an instance of ServerOptions will be created, using the default values.

]
@section{argument}
  clientID
an integer. In multi-client situations, every client can be given separate ranges for link::Classes/Node##Nodes::, link::Classes/Buffer##Buffers::, or link::Classes/Bus##Busses::. In normal usage the server will supply an ID automatically when a client registers for link::#-notify#notifications:: so you should not need to supply one here. N.B. In multi-client situations you will need to set the link::Classes/ServerOptions#-maxLogins:: to at least the number of clients you wish to allow. This must be the same in the Server instances on every client.


@section{method}
  local
get/set the local server, stored in classvar 
@racketblock[local:: (created already on initClass)

]
@section{method}
  internal
get/set the internal server, stored in classvar 
@racketblock[internal:: (created already on initClass)
See: link::Guides/Server-Guide::

]
@section{method}
  default
Get or set the default server. By default this is the local server (see above).
@section{discussion}
 
Setting this will also assign it to the link::Classes/Interpreter:: variable 's'.

@racketblock[
// set the internal Server to be the default Server
Server.default = Server.internal;
s.postln; // internal
::

]
@section{subsection}
 Accessing all servers

@section{method}
  all
get a link::Classes/Set:: of all servers


@racketblock[
Server.all
::

]
@section{method}
  allRunningServers
@section{returns}
  the set of all running servers.


@racketblock[
Server.allRunningServers
::

]
@section{method}
  named
get an link::Classes/IdentityDictionary:: of all servers listed by their name


@racketblock[
Server.named.at(\default)
::


]
@section{method}
  quitAll
quit all registered servers

@section{method}
  killAll
query the system for any sc-server apps and hard quit them

@section{method}
  freeAll
free all nodes in all registered servers

@section{method}
  hardFreeAll
try to free all nodes in all registered servers, even if the server seems not to be running

@section{method}
 sync_s
If kept true (default), when the default server is changed, also the interpreter variable s is changed.

@racketblock[
Server.default = Server(\alice, NetAddr("127.0.0.1", 57130));
s.postln; // see: it is alice.
::

]
@section{subsection}
 Switching the server application

@section{method}
  supernova

Switches the server program to supernova. Check link::Classes/ParGroup:: how to make use of multicore hardware with the supernova server.

@section{method}
  scsynth

Switches the server program to scsynth. This is the default server.


@section{InstanceMethods}
 

@section{private}
  doSend

@section{method}
  sendMsg
send an OSC-message to the server.
@section{discussion}
 

@racketblock[
s.sendMsg("/s_new", "default", s.nextNodeID, 0, 1);
::

]
@section{method}
  sendBundle
send an OSC-bundle to the server.
@section{discussion}
 
Since the network may have irregular performance, time allows for the bundle to be evaluated at a specified point in the future.
Thus all messages are synchronous relative to each other, but delayed by a constant offset.
If such a bundle arrives late, the server replies with a late message but still evaluates it.

@racketblock[
s.sendBundle(0.2, ["/s_new", "default", x = s.nextNodeID, 0, 1], ["/n_set", x, "freq", 500]);
::

]
@section{method}
  sendRaw

@section{method}
  listSendMsg
as sendMsg, but takes an array as argument.

@section{method}
  listSendBundle
as sendBundle, but takes an array as argument.
@section{discussion}
 
This allows you to collect messages in an array and then send them.

@racketblock[
s.listSendBundle(0.2, [["/s_new", "default", x = s.nextNodeID, 0, 1],
    ["/n_set", x, "freq", 600]]);
::

]
@section{method}
  sendSynthDef
send a synthDef to the server that was written in a local directory

@section{method}
  loadSynthDef
load a synthDef that resides in the remote directory

@section{method}
  loadDirectory
load all the SynthDefs in the directory dir.
@section{argument}
  dir
a link::Classes/String:: which is a valid path.
@section{argument}
  completionMsg

@section{method}
  nextNodeID
get a unique nodeID.

@section{method}
  nextPermNodeID
get a permanent node ID. This node ID is in a reserved range and will be held until you explicitly free it.

@section{method}
  freePermNodeID
free a permanent node ID for later reuse.

@section{method}
  wait
this can be used within a link::Classes/Routine:: to wait for a server reply

@section{method}
  waitForBoot
Evaluate "onComplete" as soon as the server has booted. This method will boot the server for you if it is not already running or booting. If the server is already running, "onComplete" is executed immediately.
@section{argument}
  onComplete
A function to evaluate after the server has booted successfully.
@section{argument}
  limit
The number of times to check for a successful boot. (5 times/sec)
@section{argument}
  onFailure
A function to evaluate after the server fails to boot. If onFailure is not given, an error message is posted. Providing a function suppresses the error message. If you want to supply a function and print the normal error message, make sure that your function returns "false," e.g. 
@racketblock[s.waitForBoot(onFailure: { ... custom action...; false })::.

]
@section{method}
  doWhenBooted
Evaluate "onComplete" as soon as the server has booted. This method assumes the server is being booted explicitly through a separate 
@racketblock[boot:: call. If the server is already running, "onComplete" is executed immediately.
]
@section{argument}
  onComplete
A function to evaluate after the server has booted successfully.
@section{argument}
  limit
The number of times to check for a successful boot.
@section{argument}
  onFailure
A function to evaluate after the server fails to boot. If onFailure is not given, an error message is posted. Providing a function suppresses the error message. If you want to supply a function and print the normal error message, make sure that your function returns "false," e.g. 
@racketblock[s.doWhenBooted(onFailure: { ... custom action...; false })::.

]
@section{method}
  boot
boot the remote server, create new allocators.
@section{argument}
  startAliveThread
If true, start a Routine to send a /status message to the server every so often. The interval between the messages is set by 
@racketblock[theServer.aliveThreadPeriod = (seconds)::. The default period is 0.7. If false, /status will not be sent and the server's window will not update.
]
@section{argument}
  recover
If true, create a new node ID allocator for the server, but use the old buffer and bus allocators. This is useful if the server process did not actually stop. In normal use, the default value "false" should be used.
@section{argument}
  onFailure
In this method, the onFailure argument is for internal use only. If you wish to take specific actions when the server boots or fails to boot, it is recommended to use link::#-waitForBoot:: or link::#-doWhenBooted::.

@section{discussion}
 
You cannot boot a server app on a remote machine, but you can initialise the allocators by calling this message.

@section{method}
  quit
quit the server application

@section{argument}
 onComplete
A function that is called when quit has completed.

@section{argument}
 onFailure
A function that is called when quit has failed.

@section{argument}
 watchShutDown
a boolean to tell the server whether to watch status during shutdown.

@section{method}
  reboot
quit and restart the server application

@section{argument}
 func
a function that is called between quit and (re-)boot.

@section{argument}
 onFailure
A function that is called when quit has failed.

@section{method}
  freeAll
free all nodes in this server

@section{method}
  status
query the server status

@section{method}
  notify
The server sends notifications, for example if a node was created, a 'tr' message from a link::Classes/SendTrig::, or a strong::/done:: action. if 
@racketblock[flag:: is set to false, these messages will not be sent to this client. The default is true. If true the server will respond with a clientID (scsynth only) which can be useful in multi-client situations. If this is different from any previously received ID new allocators will be created. See link::#Local vs. Remote Servers, Multi-client Configurations:: for more information.

]
@section{method}
  ping
measure the time between server and client, which may vary. the 
@racketblock[func:: is
evaluated after ]

@racketblock[n:: number of times and is passed the resulting maximum.

]
@section{method}
  options
Get or set this Server's link::Classes/ServerOptions:: object. Changes to options only take effect when the server is rebooted.

@section{method}
  defaultGroup
@section{returns}
  this Server's default group.

@section{method}
  volume
Get an instance of Volume that runs after the default group, or sets the Volume of the Server's output to level. Level is in db.

@section{method}
  mute
mute the server's output. This can also be toggled from the Server window with the 'm' key.

@section{method}
  unmute
unmute the server. This can also be toggled from the Server window with the 'm' key.

@section{method}
  reorder
Move the nodes in 
@racketblock[node]
@section{List}
  to the location specified by 
@racketblock[target:: and ]

@racketblock[addAction::, placing them there in the order indicated by nodeList.
]
@section{discussion}
 
Any nodes which have already been freed will be skipped. Passing nil for target and addAction will result in the location being the head of the default group.

@racketblock[
g = Group.new;
x = Array.fill(5, {Synth(\default)});
s.queryAllNodes;
s.reorder(x, g, \addToTail);
s.queryAllNodes;
::

]
@section{method}
  inputBus
Return a link::Classes/Bus:: object that represents the input audio bus.

@section{method}
  outputBus
Return a link::Classes/Bus:: object that represents the output audio bus.


@section{subsection}
  Information and debugging

@section{method}
  dumpOSC
@section{argument}
  code
@section{table}
 
## 0 || turn dumping OFF.
## 1 || print the parsed contents of the message.
## 2 || print the contents in hexadecimal.
## 3 || print both the parsed and hexadecimal representations of the contents.
::

@section{note}
  teletype::/status:: messages won't be posted, when dumping is enabled::

@section{method}
  queryAllNodes
Post a representation of this Server's current node tree to the post window. See link::#-plot@section{Tree}
  for a graphical variant.
@section{discussion}
 
Very helpful for debugging. For local servers this uses g_dumpTree and for remote g_queryTree. See link::Classes/Group:: and link::Reference/Server-Command-Reference:: for more info.

@racketblock[
s.boot;
s.queryAllNodes; // note the root node (ID 0) and the default group (ID 1)
s.quit;
::

]
@section{method}
  peakCPU, avgCPU
Get peak and average CPU usage.

@section{method}
  latency
The current latency of the server. See link::Guides/ServerTiming:: for details.

@section{method}
  sampleRate
An integer representing the nominal sample rate of the server; in other words, the sample rate that was requested of the server when it was booted.

@section{method}
  actualSampleRate
A floating-point number representing the current hardware sample rate, which may drift.

@section{method}
  numSynths
Get number of running link::Classes/Synth::s.

@section{method}
  numGroups
Get number of link::Classes/Group::s.

@section{method}
  numUGens
Get number of running link::Classes/UGen::s.

@section{method}
  numSynthDefs
Get number of loaded link::Classes/SynthDef::initions.

@section{method}
  pid
Get process ID of running server (if not internal).

@section{method}
  addr
The network address of the server as a link::Classes/NetAddr::.

@section{method}
  hasShmInterface
Returns true if a link::Classes/ServerShmInterface:: is available. See also link::Classes/Bus#Synchronous control bus methods::.
The shared memory interface is initialized after first server boot.

@section{method}
  serverBooting

@racketblock[true:: if the server is booting, ]

@racketblock[false:: otherwise.

]
@section{method}
  serverRunning

@racketblock[true:: if the server is running, ]

@racketblock[false:: otherwise.

]
@section{method}
  unresponsive

@racketblock[true:: if the server is unresponsive (specifically, if it has failed to respond after link::Classes/ServerOptions#-pingsBeforeConsideredDead:: ping attempts); ]

@racketblock[false:: otherwise.

]
@section{method}
  isLocal

@racketblock[true:: if the server is running on the same machine as sclang, ]

@racketblock[false:: otherwise.

]
@section{method}
  remoteControlled

@racketblock[true:: if the server is not being controlled by the machine on which it is running, ]

@racketblock[false:: otherwise. This value is the same as ]

@racketblock[isLocal:: unless explicitly set.

]
@section{subsection}
  Message Bundling

Server provides support for automatically bundling messages. This is quite convenient in object style, and ensures synchronous execution. See also link::Guides/Bundled-Messages::

@section{method}
  makeBundle
The Function 
@racketblock[func:: is evaluated, and all OSC messages generated by it are deferred and added to a bundle.
]
@section{argument}
  time
If set to nil or a number the bundle will be automatically sent and executed after the corresponding delay in seconds. If 
@racketblock[time:: is set to false the bundle will not be sent.
]
@section{argument}
  func
The function to evaluate.
@section{argument}
  bundle
allows you to pass in a preexisting bundle and continue adding to it.
@section{returns}
  The bundle so that it can be further used if needed.
@section{discussion}
 
Calling 
@racketblock[sync:: inside func will split the bundle and wait for asynchronous actions to complete before continuing.

If an error is encountered while evaluating ]

@racketblock[func:: this method will throw an link::Classes/Error:: and stop message deferral.
]

@racketblock[
s.boot;
(
// send a synth def to server
SynthDef("tpulse", { arg out=0,freq=700,sawFreq=440.0;
	Out.ar(out, SyncSaw.ar(freq,  sawFreq,0.1) )
}).add;
)

// all OSC-commands generated in the function contained below will be added to a bundle
// and executed simultaneously after 2 seconds.
(
s.makeBundle(2.0, {
	x = Synth.new("tpulse");
	a = Bus.control.set(440);
	x.map(\freq, a);
});
)
x.free;

// don't send
(
b = s.makeBundle(false, {
	x = { PinkNoise.ar(0.1) * In.kr(0, 1); }.play;
});
)
// now pass b as a pre-existing bundle, and start both synths synchronously
(
s.makeBundle(nil, { // nil executes ASAP
	y = { SinOsc.kr(0.2).abs }.play(x, 0, 0, \addBefore); // sine envelope
}, b);
)
x.free; y.free;

// Throw an Error
(
try {
	s.makeBundle(nil, {
		s.farkermartin;
	});
} { |error|
	("Look Ma, normal operations resume even though:\n" + error.errorString).postln;
	x = { FSinOsc.ar(440, 0, 0.2) }.play; // This works fine
}
)
x.free;

// use sync
(
s.makeBundle(nil, {
	b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
	s.sync; // wait until load is done and then send the rest of the bundle
	x = { PlayBuf.ar(1, b) * 0.5 }.play;
});
)
x.free; b.free;
::

]
@section{method}
  bind
Just as in 
@racketblock[makeBundle::, the Function ]

@racketblock[func:: is evaluated, and all OSC messages generated by it are deferred and added to a bundle, which is sent to the server, using the server default latency.
]
@section{discussion}
 

@racketblock[
(
s.bind {
	a = { |freq=100| SinOsc.ar(freq, LFTri.ar(freq)) * 0.2 }.play;
    s.sync;
	a.set(\freq, 400);
}
)
::

]
@section{subsection}
  Shared Controls

The internal server has a number of shared control buses. Their values can be set or polled using the methods below.

@section{method}
  getSharedControl
get the current value of a shared control bus. num is the index of the bus to poll. This command is synchronous and only works with the internal server.

@section{method}
  setSharedControl
set the current value of a shared control bus to value. num is the index of the bus to set. This command is synchronous and only works with the internal server.

@section{method}
  allocSharedControls
set the number of shared control buses. Must be done before the internal server is booted. The default is 1024.

@section{subsection}
  Persistent Node Trees

The class link::Classes/Server@section{Tree}
  can be used to store functions which will be evaluated after the server is booted, after all nodes are freed, and after cmd-. is pressed.
This allows, for example, for one to create a persistent basic node structure. ServerTree is evaluated in the method initTree after the default group is created, so its existence can be relied upon.

@section{method}
  initTree
This method initializes the link::Reference/default_group:: and runs link::Classes/Server@section{Tree}
 .
@section{discussion}
 
This method is called automatically when you boot a Server from the language. N.B. If you started a server app from the command line you will have to call initTree manually if you need this functionality.

@racketblock[
s.quit;
f = {Group.new(s.defaultGroup); "Other code can be evaluated too".postln;};
ServerTree.add(f);
s.boot;
s.queryAllNodes; // note the group within the default group
ServerTree.remove(f);
::
link::Classes/ServerBoot:: and link::Classes/ServerQuit:: provide similar functionality at boot and quit times.

]
@section{subsection}
  GUI methods

@section{method}
  makeGui
Create and show the server window. The window responds to a number of keyboard shortcuts:
@section{table}
 
## strong::key:: || strong::action::
## teletype::n:: || Post a representation of this Server's current node tree to the post window. (See link::#-queryAllNodes::)
## teletype::N:: || As 'n' above but include controls.
## teletype::l:: || Show input/output level meters. (See link::#-meter::)
## teletype::p:: || Show graphical view of the node tree. (See link::#-plot@section{Tree}
 )
## (space) || Boot server if not already booted. (See link::#-boot::)
## teletype::s:: || Show scope window. (See link::#-scope::)
## teletype::f:: || Show frequency analyzer window. (See link::#-freqscope::)
## teletype::d:: || Toggle dumping of OSC messages.
## teletype::m:: || Toggle mute.
## teletype::0:: || Reset volume to 0 db.
::

@section{method}
  makeWindow
On most platforms, this is equivalent to 
@racketblock[makeGui::.
If you are running SuperCollider on Emacs, it makes a server view composed of Emacs widgets.

]
@section{method}
  scope
Open a scope window showing the output of the Server.
see link::Classes/Stethoscope:: for further details.

@section{argument}
  numChannels
the number of channels to be scoped out. The default is this server's options' numOutputBusChannels.
@section{argument}
  index
the first channel to be output. The default is 0.
@section{argument}
  bufsize
the size of the buffer for the ScopeView. The default is 4096.
@section{argument}
  zoom
a zoom value for the scope's X axis. Larger values show more. The default is 1.
@section{argument}
  rate
whether to display audio or control rate buses (either \audio or \control)

@section{method}
  freqscope
Show frequency analyzer window.

@section{method}
  meter
Show input/output level meters.

@section{method}
  plotTree
Plot the node/group tree. As link::#-queryAllNodes:: but graphical.

@section{argument}
  interval
Polling interval.

@section{method}
  plotTreeView
Plot the node/group tree graphically on a given view.

@section{argument}
  interval
Polling interval.

@section{argument}
  parent
Parent view.

@section{argument}
  actionIfFail
Function to be evaluated in case communication with the server cannot be established.

@section{returns}
  Function to be evaluated in order to clean up all actions performed inside the method. To be called for instance by the onClose method of the enclosing window.

@section{subsection}
  Recording Support

The following methods are for convenience use. For recording with sample accurate start and stop times you should make your own nodes. See the link::Classes/DiskOut:: helpfile for more info. For non-realtime recording, see the link::Guides/Non-Realtime-Synthesis:: helpfile.

This functionality is also available through the recording button on the server windows.
Pressing it once calls record, and pressing it again calls stopRecording (see below). When doing so the file created will be in your recordings folder and be named for the current date and time.
The default location of the recordings folder varies from platform to platform but is always stored in 
@racketblock[thisProcess.platform.recordingsDir::. Setting this variable allows you to change the default.

]
@section{NOTE}
 
record creates the recording synth after the Server's default group and uses In.ar. Thus if you add nodes after the recording synth their output will not be captured.
To avoid this, either use Node objects (which use the default node as their target) or (when using messaging style) use a target nodeID of 1.

@racketblock[
s.sendMsg("/s_new", "default", s.nextNodeID, 1,1);
::
::

For more detail on this subject see link::Guides/Order-of-execution::, link::Reference/default_group::, and link::Guides/NodeMessaging::.

See link::Classes/SoundFile:: for information on the various sample and header formats.
Not all sample and header formats are compatible. Note that the sampling rate of the output file will be the same as that of the server app. This can be set using the Server's link::Classes/ServerOptions::.

Example:
]

@racketblock[
s.boot; // start the server

// something to record
(
SynthDef("bubbles", {
	var f, zout;
	f = LFSaw.kr(0.4, 0, 24, LFSaw.kr([8,7.23], 0, 3, 80)).midicps; // glissando function
	zout = CombN.ar(SinOsc.ar(f, 0, 0.04), 0.2, 0.2, 4); // echoing sine wave
	Out.ar(0, zout);
}).add;
)

x = Synth.new("bubbles");

s.prepareForRecord; // if you want to start recording on a precise moment in time, you have to call this first.

s.record;

s.pauseRecording; // pausable

s.record // start again

s.stopRecording; // this closes the file and deallocates the buffer recording node, etc.

x.free; // stop the synths

// look in your recordings folder and you'll find a file named for this date and time
::

Recording is done via an of link::Classes/Recorder:: - a server holds one instance implicitly.

]
@section{method}
  prepareForRecord
Allocates the necessary buffer, etc. for recording the server's output. (See 
@racketblock[record:: below.)
]
@section{argument}
  path
a link::Classes/String:: representing the path and name of the output file.
@section{argument}
  numChannels
a link::Classes/String:: the number of output channels to record.

@section{discussion}
 
If you do not specify a path than a file will be created in your recordings folder (see the note above on this) called SC_thisDateAndTime. Changes to the header or sample format, or to the number of channels must be made strong::before:: calling this.

@section{method}
  record
Starts or resumes recording the output.
@section{argument}
  path
this is optional, and is passed to 
@racketblock[prepareForRecord:: (above).
]
@section{argument}
  bus
the bus to record - defaults to 0
@section{argument}
  numChannels
the number of channels to record - defaults to server numChannels
@section{argument}
  node
the node to record - defaults to server rootnode
@section{argument}
  duration
duration to record - defaults to inf

@section{discussion}
 
If you have not called prepareForRecord first (see above) then it will be invoked for you (but that adds a slight delay before recording starts for real).

@section{method}
  pauseRecording
Pauses recording. Can be resumed by executing record again.

@section{method}
  stopRecording
Stops recording, closes the file, and frees the associated resources.
@section{discussion}
 
You must call this when finished recording or the output file will be unusable. Cmd-. while recording has the same effect.


@section{method}
  recChannels
Get/set the number of channels (int) to record. Is automatically set to the value of link::Classes/ServerOptions#-numOutputBusChannels:: when booting the server. Must be called strong::before:: prepareForRecord.

@section{method}
  recHeaderFormat
Get/set the header format (string) of the output file. The default is "aiff". Must be called strong::before:: prepareForRecord.

@section{method}
  recSampleFormat
Get/set the sample format (string) of the output file. The default is "float". Must be called strong::before:: prepareForRecord.

@section{method}
 recBufSize
Get/set the size of the link::Classes/Buffer:: to use with the link::Classes/DiskOut:: UGen. This must be a power of two. The default is the 
@racketblock[sampleRate.nextPowerOfTwo:: or the first power of two number of samples longer than one second. Must be called strong::before:: prepareForRecord.

]
@section{subsection}
  Asynchronous Commands

Server provides support for waiting on the completion of asynchronous OSC-commands such as reading or writing soundfiles. N.B. The following methods must be called from within a running link::Classes/Routine::. Explicitly passing in a link::Classes/Condition:: allows multiple elements to depend on different conditions. The examples below should make clear how all this works.

@section{method}
  bootSync
Boot the Server and wait until it has completed before resuming the thread.
@section{argument}
  condition
an optional instance of link::Classes/Condition:: used for evaluating this.

@section{method}
  sendMsgSync
Send the following message to the wait until it has completed before resuming the thread.
@section{argument}
  condition
an optional instance of link::Classes/Condition:: used for evaluating this.
@section{argument}
  ... args
one or more valid OSC messages.

@section{method}
  sync
Send a 
@racketblock[/sync:: message to the server, which will replie with the message ]

@racketblock[/synced:: when all pending asynchronous commands have been completed.
]
@section{argument}
  condition
an optional instance of link::Classes/Condition:: used for evaluating this.
@section{argument}
  bundles
one or more OSC messages which will be bundled before the sync message (thus ensuring that they will arrive before the /sync message). @section{argument}
  latency
allows for the message to be evaluated at a specific point in the future.

@section{discussion}
 
This may be slightly less safe then sendMsgSync under UDP on a wide area network, as packets may arrive out of order, but on a local network should be okay. Under TCP this should always be safe.

@racketblock[
(
Routine.run {
	var c;

	// create a condition variable to control execution of the Routine
	c = Condition.new;

	s.bootSync(c);
	\BOOTED.postln;

	s.sendMsgSync(c, "/b_alloc", 0, 44100, 2);
	s.sendMsgSync(c, "/b_alloc", 1, 44100, 2);
	s.sendMsgSync(c, "/b_alloc", 2, 44100, 2);
	\b_alloc_DONE.postln;
};
)

(
Routine.run {
	var c;

	// create a condition variable to control execution of the Routine
	c = Condition.new;

	s.bootSync(c);
	\BOOTED.postln;

	s.sendMsg("/b_alloc", 0, 44100, 2);
	s.sendMsg("/b_alloc", 1, 44100, 2);
	s.sendMsg("/b_alloc", 2, 44100, 2);
	s.sync(c);
	\b_alloc_DONE.postln;
};
)
::


]


