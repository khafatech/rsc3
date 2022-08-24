#lang scribble/manual
@(require (for-label racket))

@title{WiiMote}
 use the Nintendo (tm) Wii Remote@section{related}
  Classes/HID
@section{categories}
  External Control

@section{description}

The WiiMote class allows you to access the WiiMote from SuperCollider, both to receive data from the device, as well as send data to the device.

@section{subsection}
 Some Important Issues Regarding WiiMote

This class has been developed to work both on the Mac and on Linux. The interface is mostly the same, but there are some usage issues on the Mac.
Personally, I found that it works better with an external BlueTooth receiver, than with the internal one (tested on the MacBook Pro). I also found that on the Mac, I have to connect, disconnect and then reconnect to get everything to work properly.

The IR options, as well as the Classic controller have not been tested (due to lack of access to either complementary device by the developer).

This class is not implemented for Windows, and thus will not work on that platform.

@section{ClassMethods}
 

@section{private}
 initClass, prStart, prStop, prDiscover

@section{method}
 new
@section{note}
 
Should not be called directly. See link::#*discover::.
::

@section{method}
 start
Starts the eventloop. Called automatically by link::#*discover::, so no real need to call this method.

@section{argument}
 updtime
updatetime of the eventloop in milliseconds.

@section{method}
 discover
Discovers a new device. This calls for the creation of a new device and class instance by calling the method link::#*new::. ( strong::new:: should not be called directly). This method is synchronous, and will block until a device is found or until it times out.

When discover is called, the buttons 1 and 2 on the Wii Remote should be pushed to put the device in discovery mode.

@section{returns}
  A new WiiMote object for the device discovered, or 
@racketblock[nil:: if no device was discovered.

]
@section{discussion}
 
Example to start up:

@racketblock[
w = WiiMote.discover;   // discover a new device
w.battery;              // post the battery status of the device

// cleanup:
WiiMote.closeAll;       // close all devices
WiiMote.stop;
::

]
@section{method}
 all
Returns an link::Classes/Array:: with all WiiMote devices.

@section{method}
 closeAll
Close all WiiMote devices.

@section{method}
 stop
Stops the eventloop. Only really necessary on Mac, but use it for cross platform robustness.

@section{InstanceMethods}
 

@section{private}
 prInit, prOpen, prClose, prAddress, prConnect, prDisconnect, prCalibration, prEnable, prEnableExpansion, prEnableIRSensor, prEnableMotionSensor, prEnableButtons, prSetVibration, prWiiSetLED, prHandleBatteryEvent, prHandleExtensionEvent, prHandleButtonEvent, prHandleNunchukEvent, prHandleClassicEvent, prHandleIREvent, prHandleAccEvent, prHandleEvent, prReadError, prConnectAction, prDisconnectAction

@section{method}
 dumpEvents
dump incoming events for debugging purposes.

@section{method}
 spec
Returns the device specification, with symbolic names for each item. Each name links to the current value.

@section{method}
 actionSpec
Returns the device action specification, with symbolic names for each item. Each name in the dictionary links to an action to be performed upon receiving a new value.

@section{method}
 closeAction
Set an action to be performed when the device closes.

@section{method}
 connectAction
Set an action to be performed when the device connects.

@section{method}
 disconnectAction
Set an action to be performed when the device disconnects.

@section{method}
 at
Get the value of a device property at the given key.

@section{method}
 setAction
Set an action to be performed when the value of strong::key:: changes. The key name must be one that occurs in the spec.

@section{method}
 removeAction
Remove the previously defined action at the strong::key::.

@section{method}
 close
Close the device.

@section{subsection}
 The properties of the Wii Remote

@section{method}
 battery
Returns the current battery status of the device.

@section{method}
 ext_type
Returns the extension type that is connected.

@section{method}
 remote_buttons
Returns an link::Classes/Array:: with the current button values.

@section{method}
 remote_motion
Returns an link::Classes/Array:: with the current acceleration values (x,y,z, orientation). Orientation is Mac only.

@section{method}
 remote_ir
Returns an link::Classes/Array:: with the found IR objects. (not tested!).

@section{method}
 remote_led
Returns an link::Classes/Array:: with the current LED values.

@section{method}
 setLEDState
Set the LED with number strong::id:: to value strong::value:: (1=on, 0=off).

@section{method}
 rumble
Turn on the rumble, strong::value:: (1=on, 0=off).

@section{method}
 enable
Enable the device.

@section{method}
 enableExpansion
Enable the device expansion (nunchuk or classic controller).

@section{method}
 enableButtons
Enable the buttons on the device.

@section{method}
 enableMotionSensor
Enable the motion sensor on the device.

@section{method}
 enableIRSensor
Enable the IR sensor on the device.

@section{subsection}
 The properties of the NunChuk

@section{method}
 nunchuk_buttons
Returns an link::Classes/Array:: with the current button values.

@section{method}
 nunchuk_motion
Returns an link::Classes/Array:: with the current acceleration values (x,y,z, orientation). Orientation is Mac only.

@section{method}
 nunchuk_stick
Returns an link::Classes/Array:: with the current stick values.

@section{subsection}
 The properties of the Classic Controller

@section{method}
 classic_buttons
Returns an link::Classes/Array:: with the current button values.

@section{method}
 classic_stick1
Returns an link::Classes/Array:: with the current stick values of stick 1.

@section{method}
 classic_stick2
Returns an link::Classes/Array:: with the current stick values of stick 2.

@section{method}
 classic_analog
Returns an link::Classes/Array:: with the current analog values.

@section{Examples}
 


@racketblock[
// Example to start up and view values
WiiMote.start;    // start the eventloop
w = WiiMote.discover; // discover a new device (wait for post about connected)

WiiMote.all;      // post an array of all devices

x = WiiMoteGUI.new( w ); // create a GUI (only covers the WiiMote and NunChuk currently)

w.enableMotionSensor( 1 );
w.enableExpansion( 1 );

w.setLEDState( 0,1 ); // turn the first LED on
w.rumble( 1 ); // rumble the device
w.rumble( 0 ); // rumble the device

w.setAction( \bA, { |v| v.postln; } ); // post the value when button A changes.
w.removeAction( \bA );

// (macOS) if you do not see any changes in the motion sensors, then the connection is bad.
// push the red button inside the battery compartment, or the buttons 1 and 2 on the WiiMote and start over again to discover...

WiiMote.discover; // discover a new device
WiiMote.all;      // post an array of all devices

w = WiiMote.all[1];
x.w.close; // close previous window
x = WiiMoteGUI.new( w ); // create a GUI (only covers the WiiMote and NunChuk currently)

// now it should work..., if not, repeat the exercise...


// clean up
WiiMote.closeAll; // close all devices
WiiMote.stop;
x.w.close;
::

]
@section{subsection}
  IR tracking
This example shows a window displaying the objects being tracked by the WiiMote IR camera.

@racketblock[
(
var q, w, u;
q = WiiMote.discover;
if(q.isNil) {
    "No wiimote found, aborting.".error;
} {
    q.enableIRSensor(1);
    w = Window("IR sensor", Rect(100, 100, 400, 400)).front;
    w.onClose = { q.close };
    u = UserView(w, Rect(0, 0, 400, 400));
    u.background = Color.black;
    u.drawFunc = {
        var p;
        q.remote_ir.do {|ir, i|
            if(ir.valid>0) {
                p = Point(ir.posx*400, ir.posy*400);
                Pen.addArc(p, ir.size*500.0, 0, pi*2);
                Pen.fillColor = Color.hsv(i/5, 1, 1);
                Pen.fill;
                Pen.stringAtPoint(
                    "% @ % %".format(
                        ir.posx.round(0.01),
                        ir.posy.round(0.01),
                        (ir.size*1000).floor
                    ),
                    p,
                    Font(Font.defaultSansFace, 9),
                    Color.white
                );
            };
        };
    };
    u.animate = true;
};
)
::

]


