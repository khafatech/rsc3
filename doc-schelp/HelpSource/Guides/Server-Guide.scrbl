#lang scribble/manual
@(require (for-label racket))

@title{Server Guide}
 Using Server objecs in different situations@section{categories}
  Server>Abstractions
@section{related}
  Classes/Server, Classes/ServerOptions, Reference/Server-Architecture, Reference/Server-Command-Reference

@section{description}


A Server object is the client-side representation of a server app and is used to control the app from the SuperCollider language application. (See link::Guides/ClientVsServer:: for more details on the distinction.)
It forwards OSC messages and has a number of allocators that keep track of IDs for nodes, buses and buffers.

The server application is a commandline program, so all commands apart from OSC messages are UNIX commands.

The server application represented by a Server object might be running on the same machine as the client (in the same address space as the language application or separately; see below), or it may be running on a remote machine.

Most of a Server's options are contolled through its instance of ServerOptions. See the link::Classes/ServerOptions:: helpfile for more detail.

@section{subsection}
  Paths

Server apps running on the local machine have two UNIX environment variables: 
@racketblock[SC_SYNTHDEF_PATH:: and ]

@racketblock[SC_PLUGIN_PATH::. These indicate directories of synthdefs and ugen plugins that will be loaded at startup. These are in addition to the default synthdef/ and plugin/ directories which are hard-coded.

These can be set within SC using the getenv and setenv methods of class link::Classes/String::.
]

@racketblock[
// all defs in this directory will be loaded when a local server boots
"SC_SYNTHDEF_PATH".setenv("~/scwork/".standardizePath);
"echo $SC_SYNTHDEF_PATH".unixCmd;
::

]
@section{subsection}
  The default group

When a Server is booted there is a top level group with an ID of 0 that defines the root of the node tree. (This is represented by a subclass of link::Classes/Group:: : link::Classes/RootNode::.)
If the server app was booted from within SCLang (as opposed to from the command line) the method 
@racketblock[init]
@section{Tree}
  will be called automatically after booting.
This will also create a link::Reference/default_group:: with an ID of 1, which is the default group for all link::Classes/Node::s when using object style.
This provides a predictable basic node tree so that methods such as Server-scope, Server-record, etc. can function without running into order of execution problems.

The default group is persistent, i.e. it is recreated after a reboot, pressing cmd-., etc. See link::Classes/RootNode:: and link::Reference/default_group:: for more information.
@section{Note}
 
If a Server has been booted from the command line you must call 
@racketblock[init]
@section{Tree}
  manually in order to initialize the default group, if you want it. See 
@racketblock[init]
@section{Tree}
  below.
::

@section{subsection}
  Local vs. Internal

In general, when working with a single machine one will probably be using one of two Server objects which are created at startup and stored in the class variables link::#*local:: and link::#*internal::. In SuperCollider.app (OSX), two GUI windows are created to control these. Use link::#-makeGui:: to create a GUI window manually.

The difference between the two is that the local server runs as a separate application with its own address space, and the internal server runs within the same space as the language/client app.

Both local and internal server supports link::#-scope#scoping:: and link::Classes/Bus#Synchronous control bus methods#synchronous bus access::.

The local server, and any other server apps running on your local machine, have the advantage that if the language app crashes, it (and thus possibly your piece) will continue to run. It is thus an inherently more robust arrangement. But note that even if the synths on the server continue to run, any language-side sequencing and control will terminate if the language app crashes.

At the current time, there is generally no benefit in using the internal server, but it remains for the purposes of backwards compatibility.

@section{subsection}
  The default Server

There is always a default Server, which is stored in the class variable 
@racketblock[default::. Any link::Classes/Synth::s or link::Classes/Group::s created without a target will be created on the default server. At startup this is set to be the local server (see above), but can be set to be any Server.

]
@section{subsection}
  Local vs. Remote Servers, Multi-client Configurations

Most of the time users work with a server app running on the same machine as the SC language client. It is possible to use a server running on a different machine via a network, providing you know the IP address and port of that server. The link::#*remote:: method provides a convenient way to do this.

One common variant of this approach is multiple clients using the same server. If you wish to do this you will need to set the server's link::Classes/ServerOptions#-maxLogins:: to at least the number of clients you wish to allow.  When a client registers for link::#-notify#notifications:: the server will supply a client ID. This also configures the allocators to avoid conflicts when allocating link::Classes/Node##Nodes::, link::Classes/Buffer##Buffers::, or link::Classes/Bus##Busses::.

In order to use a remote server with tcp one should first boot the remote server using the 
@racketblock[-t:: option and then run the following code:

]

@racketblock[
(
s.options.protocol_(\tcp);
s.addr.connect;
s.startAliveThread( 0 );
s.doWhenBooted({ "remote tcp server started".postln; s.notify; s.initTree });
)
::


]


