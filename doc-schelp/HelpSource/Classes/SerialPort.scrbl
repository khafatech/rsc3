#lang scribble/manual
@(require (for-label racket))

@title{SerialPort}
 serial port interface@section{categories}
  External Control

@section{ClassMethods}
 

@section{private}
 initClass

@section{method}
 new
opening the port.

@section{argument}
 port
device path or index.

@section{argument}
 baudrate
baudrate [4800..230400]

@section{argument}
 databits
5 | 6 | 7 | 8

@section{argument}
 stopbit
true | false

@section{argument}
 parity
nil | 'even' | 'odd'

@section{argument}
 crtscts
hardware flow control (true | false)

@section{argument}
 xonxoff
software flow control (true | false)

@section{argument}
 exclusive
open the device exclusively (true | false)

@section{method}
 devices
returns an array of available device.

@racketblock[
SerialPort.devices;
::

]
@section{method}
 listDevices
prints to postbuffer

@racketblock[
SerialPort.listDevices;
::

]
@section{method}
 devicePattern
change device selection

@racketblock[
SerialPort.devicePattern = "/dev/ttyUSB*"; // Linux usb serial
SerialPort.devices;

SerialPort.devicePattern = nil;
SerialPort.devices;
::

]
@section{method}
 closeAll
close all ports.

@section{InstanceMethods}
 

@section{private}
 initSerialPort, prOpen, prClose, primCleanup, prCleanup, prPut, prDataAvailable, prDoneAction

@section{method}
 next
Read a byte from the device. Non-blocking read.

@section{method}
 read
Read a byte from the device. Blocking read.

@section{method}
 rxErrors
Rx errors since last query.

@section{method}
 put
Write a byte to the device. Always blocks.

@section{method}
 putAll
Write multiple bytes to the device. Collection may be link::Classes/Int8Array:: or link::Classes/String::.

@section{method}
 doneAction
A link::Classes/Function:: which will be evaluated if the port gets closed (maybe unexpectedly so, due to hardware failure or accidental disconnection). This allows you to for example to make a backup solution and activate it (like using fake input data for your algorithm, or trying to reopen the device). By default it will post a message to the post window.

@section{method}
 close
close the port.

@section{Examples}
 


@racketblock[
(
p = SerialPort(
	"/dev/tty.usbserial-181",
	baudrate: 9600,
	crtscts: true);
)

// read a byte from the device

p.next;			// doesn't block
fork{p.read.postln};	// may suspend thisThread - should be called within a routine

// write a byte to the device

fork{p.put(42)};	// may suspend thisThread - should be called within a routine

// write multiple bytes to the device

p.putAll("whaddayawant");
p.putAll(Int8Array[13, 10]);

p.doneAction = { "my serial port got closed".postln; }

p.close;	// close the port

SerialPort.closeAll;	// close all ports
::

]
@section{subsection}
 Arduino write example

First load the sketch Examples/Communication/Dimmer. See http://www.arduino.cc/en/Tutorial/Dimmer

@section{note}
 
Always make sure the serial monitor is closed in the Arduino application before opening the port in SuperCollider.
::


@racketblock[
(
p = SerialPort(
	"/dev/tty.usbserial-A800crTT",	//edit to match your port. SerialPort.listDevices
	baudrate: 9600,	//check that baudrate is the same as in arduino sketch
	crtscts: true);
)

//send serial data - slow pulsating
(
r= Routine({
	inf.do{|i|
		p.put(i.fold(0, 100).linexp(0, 100, 1, 255).asInteger.postln);
		0.02.wait;
	};
}).play;
)

r.stop;
p.close;
::

]
@section{subsection}
 Arduino read example

First load the sketch Examples/Communication/Graph. See http://www.arduino.cc/en/Tutorial/Graph

@section{note}
 
Always make sure the serial monitor is closed in the Arduino application before opening the port in SuperCollider.
::


@racketblock[
(
p = SerialPort(
	"/dev/tty.usbserial-A800crTT",	//edit to match your port. SerialPort.listDevices
	baudrate: 9600,	//check that baudrate is the same as in arduino sketch
	crtscts: true);
)

//read 10bit serial data sent from Arduino's Serial.println
(
r= Routine({
	var byte, str, res;
	99999.do{|i|
		if(p.read==10, {
			str = "";
			while({byte = p.read; byte !=13 }, {
				str= str++byte.asAscii;
			});
			res= str.asInteger;
			("read value:"+res).postln;
		});
	};
}).play;
)

r.stop;
p.close;
::
]


