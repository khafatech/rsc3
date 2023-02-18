#lang scribble/manual
@(require (for-label racket))

@title{HIDFunc}
 Fast responder for incoming data from human input devices (HID)@section{categories}
  External Control>HID
@section{related}
  Classes/HID, Classes/HIDdef, Classes/HIDProto, Classes/HIDElementProto, Classes/OSCFunc, Classes/MIDIFunc, Guides/Working_with_HID

@section{description}

Human input devices can be used as controllers for making music. This class provides you with access to them in a way similar to OSCFunc and MIDIFunc.

HIDFunc (and its subclass link::Classes/HIDdef::) registers one or more functions to respond to an incoming HID message. Many of its methods are inherited from its superclass link::Classes/AbstractResponderFunc::.

@section{note}
  HIDFuncs are removed on Cmd-. by default. This can be overridden using either of the fix or permanent methods.::


The development of this SuperCollider implementation of HID access was funded by the SuperCollider community and BEK, Bergen Elektronisk Kunst, Bergen, Norway, http://www.bek.no


@section{CLASSMETHODS}
 

@section{PRIVATE}
  initClass, cmdPeriod

@section{METHOD}
  defaultDispatchers
Get or set an link::Classes/IdentityDictionary:: containing the default dispatcher objects for HIDFuncs of different types (these are what you get if you pass nil as the dispatcher argument to link::#*new::). These objects will decide if any of their registered HIDFuncs should respond to an incoming HID message. The dictionary should have the keys 
@racketblock[[\usage, \usageID, \device, \proto, \element]:: and values of an appropriate subclass of link::Classes/AbstractDispatcher:: for each message type. By default these will be instances of ]

@racketblock[HIDUsageDispatcher::, ]

@racketblock[HIDElementProtoDispatcher::, ]

@racketblock[HIDDeviceDispatcher::, ]

@racketblock[HIDElementProtoDispatcher:: and ]

@racketblock[HIDElementDispatcher:: respectively.

]
@section{returns}
  The getter returns an link::Classes/IdentityDictionary::.


@section{METHOD}
  usage
A convenience method to filter an incoming HID value based on the name of its control usage. E.g. the name of an X-axis of a joystick or the horizontal movement of a mouse has the name 
@racketblock[\X::. These usage names are standardized by manufacturers and are looked up in usage tables based on the information coming from the device. If you have an open HID device, you can look up the available usages with: ]

@racketblock[~hid.postUsages::.

]
@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...

@section{ARGUMENT}
  elUsageName
The name of the usage to look for. This can be one usage name, or an array of usage names. If nil, it will match any usage.

@section{ARGUMENT}
  devUsageName
The name of the device usage to look for, e.g. 
@racketblock[\GamePad:: or ]

@racketblock[\Mouse::. If left blank, the ]

@racketblock[HIDFunc:: will match any device.

]
@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: or link::Classes/HIDProto:: with a more detailed filtering for a device.

@section{ARGUMENT}
  argTemplate
This should be an object that implements the method 
@racketblock[matchItem::. Depending on the ]

@racketblock[argTemplateType::, it will be passed either the rawValue of the value of the element to be matched.

]
@section{ARGUMENT}
  argTemplateType
If the argTemplateType is 
@racketblock[\rawValue:: (the default) then the matching is done based on the incoming raw value of the element (not mapped according to the logical min and max). Otherwise the matching is done according to the mapped value.

]
@section{ARGUMENT}
  dispatcher
An optional instance of an appropriate subclass of link::Classes/AbstractDispatcher::. This can be used to allow for customised dispatching. Normally this should not be needed. The default for this type of 
@racketblock[HIDFunc:: is ]

@racketblock[HIDUsageDispatcher::

]
@section{returns}
  A new instance of HIDFunc which responds to a specific element usage and device type.


@section{METHOD}
  device
A convenience method to filter an incoming HID value based on the name of the device. This type of HIDFunc differs from 
@racketblock[HIDFunc.usage:: in that it filter specifically by device name, rather than device usage, otherwise the arguments are the same.

]
@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...

@section{ARGUMENT}
  elUsageName
The name of the usage to look for. This can be one usage name, or an array of usage names.

@section{ARGUMENT}
  deviceName
The name of the device to look for, note that this has to match the string as returned by the device exactly. You can look this string up in the device list: 
@racketblock[HID.postAvailable::

]
@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: or link::Classes/HIDProto:: with a more detailed filtering for a device.

@section{ARGUMENT}
  argTemplate
This should be an object that implements the method 
@racketblock[matchItem::. Depending on the ]

@racketblock[argTemplateType::, it will be passed either the rawValue of the value of the element to be matched.

]
@section{ARGUMENT}
  argTemplateType
If the argTemplateType is 
@racketblock[\rawValue:: (the default) then the matching is done based on the incoming raw value of the element (not mapped according to the logical min and max). Otherwise the matching is done according to the mapped value.

]
@section{ARGUMENT}
  dispatcher
An optional instance of an appropriate subclass of link::Classes/AbstractDispatcher::. This can be used to allow for customised dispatching. Normally this should not be needed. The default for this type of 
@racketblock[HIDFunc:: is ]

@racketblock[HIDDeviceDispatcher::

]
@section{returns}
  A new instance of HIDFunc which responds to a specific element usage for a specific device.


@section{METHOD}
  usageID
A convenience method to filter an incoming HID value based on the number of its control usage. If the device is using a non-standard usage number, then this method can be used to look for it. A controls usage is fully specified by the combination of its usage ID and its usage page.

@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...usage

@section{ARGUMENT}
  elUsageID
The id (an link::Classes/Integer::) of the usage to look for. This can be one usage id, or an array of usage ids. If nil, it will match any usage id.

@section{ARGUMENT}
  elPageID
The id (an link::Classes/Integer::) of the page of the usage to look for. This can be one page id, or an array of page ids.

@section{ARGUMENT}
  deviceName
Since this type of HIDFunc is meant for non-standardized controls, you can filter by a specific device name, rather than a general usage, similar to 
@racketblock[HIDFunc.device::

]
@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: or link::Classes/HIDProto:: with a more detailed filtering for a device.


@section{ARGUMENT}
  argTemplate
This should be an object that implements the method 
@racketblock[matchItem::. Depending on the ]

@racketblock[argTemplateType::, it will be passed either the rawValue of the value of the element to be matched.

]
@section{ARGUMENT}
  argTemplateType
If the argTemplateType is 
@racketblock[\rawValue:: (the default) then the matching is done based on the incoming raw value of the element (not mapped according to the logical min and max). Otherwise the matching is done according to the mapped value.

]
@section{ARGUMENT}
  dispatcher
An optional instance of an appropriate subclass of link::Classes/AbstractDispatcher::. This can be used to allow for customised dispatching. Normally this should not be needed. The default for this type of 
@racketblock[HIDFunc:: is ]

@racketblock[HIDElementProtoDispatcher::

]
@section{returns}
  A new instance of HIDFunc which responds to a specific element usage id for a specific device.

@section{METHOD}
  proto
A convenience method to filter an incoming HID value based on a matching template of an element (a link::Classes/HIDElementProto::). If you have number of conditions for the element that should be matched, then this method can be used to look for it.

@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...usage

@section{ARGUMENT}
  protoElement
The id (an link::Classes/Integer::) of the usage to look for. This can be one usage id, or an array of usage ids.

@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: or link::Classes/HIDProto:: with a more detailed filtering for a device.

@section{ARGUMENT}
  argTemplate
This should be an object that implements the method 
@racketblock[matchItem::. Depending on the ]

@racketblock[argTemplateType::, it will be passed either the rawValue of the value of the element to be matched.

]
@section{ARGUMENT}
  argTemplateType
If the argTemplateType is 
@racketblock[\rawValue:: (the default) then the matching is done based on the incoming raw value of the element (not mapped according to the logical min and max). Otherwise the matching is done according to the mapped value.


]
@section{ARGUMENT}
  dispatcher
An optional instance of an appropriate subclass of link::Classes/AbstractDispatcher::. This can be used to allow for customised dispatching. Normally this should not be needed. The default for this type of 
@racketblock[HIDFunc:: is ]

@racketblock[HIDElementProtoDispatcher::

]
@section{returns}
  A new instance of HIDFunc which responds to a specific prototype element.


@section{METHOD}
  element
A convenience method to filter an incoming HID value based on the index of its element. If the device is using something non-standard, or you want to access keyboard elements directly, then this method can be used to look for it. Note that the element index is not necessarily the same across different operating systems (i.e. it may vary between Linux and macOS and Windows).

@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...

@section{ARGUMENT}
  elID
The id (an link::Classes/Integer::) of the element to look for. This can be one element id, or an array of element ids.

@section{ARGUMENT}
  deviceName
Since this type of HIDFunc is meant for non-standardized elements, you can filter by a specific device name, rather than a general usage, similar to 
@racketblock[HIDFunc.device::

]
@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: or link::Classes/HIDProto:: with a more detailed filtering for a device.

@section{ARGUMENT}
  argTemplate
This should be an object that implements the method 
@racketblock[matchItem::. Depending on the ]

@racketblock[argTemplateType::, it will be passed either the rawValue of the value of the element to be matched.

]
@section{ARGUMENT}
  argTemplateType
If the argTemplateType is 
@racketblock[\rawValue:: (the default) then the matching is done based on the incoming raw value of the element (not mapped according to the logical min and max). Otherwise the matching is done according to the mapped value.


]
@section{ARGUMENT}
  dispatcher
An optional instance of an appropriate subclass of link::Classes/AbstractDispatcher::. This can be used to allow for customised dispatching. Normally this should not be needed. The default for this type of 
@racketblock[HIDFunc:: is ]

@racketblock[HIDElementDispatcher::

]
@section{returns}
  A new instance of HIDFunc which responds to a specific element id for a specific device.

@section{SUBSECTION}
  Debugging

@section{METHOD}
  trace
A convenience method which dumps all incoming HID messages.

@section{argument}
  bool
A link::Classes/Boolean:: indicating whether dumping is on or off.


@section{INSTANCEMETHODS}
 

@section{PRIVATE}
  initUsageID, initDevice, printOn, initElement, init, initUsage, initProtoElement

@section{METHOD}
  type
The type of HIDFunc.

@section{returns}
  a link::Classes/Symbol::, one of 
@racketblock[\usage::, ]

@racketblock[\device::, ]

@racketblock[\usageID::, ]

@racketblock[\proto:: or ]

@racketblock[\element::.

]
@section{METHOD}
  elUsage
The usage name, usage id, or element id of the element/control to match, depending on the type of HIDFunc

@section{METHOD}
  elementTemplate
An instance of HIDElementProto, describing the template for the element to match.

@section{METHOD}
  devUsage
The device usage or device name of the element/control to match, depending on the type of HIDFunc

@section{METHOD}
  deviceTemplate
An instance of HIDProto, describing the template for the device to match.


@section{METHOD}
  argTemplate
This should be an object that implements the method 
@racketblock[matchItem::. Depending on the ]

@racketblock[argTemplateType::, it will be passed either the rawValue of the value of the element to be matched.

]
@section{METHOD}
  argTemplateType
If the argTemplateType is 
@racketblock[\rawValue:: (the default) then the matching is done based on the incoming raw value of the element (not mapped according to the logical min and max). Otherwise the matching is done according to the mapped value.

]
@section{EXAMPLES}
 

For all the examples below here, you will need to have initialized an HID device (see also link::Guides/Working_with_HID::). The examples below should work with a standard USB mouse.



@racketblock[
HID.findAvailable; // check which devices are attached
HID.postAvailable; // post the available devices
~myhid = HID.open( 1103, 53251 ); // adapt this line for  the device that you want to open!
::

]
@section{SUBSECTION}
  Filtering based on usage


@racketblock[
// filter all events coming from the x-axis of a mouse
a = HIDFunc.usage( { |...args| args.postln; }, \X, \Mouse );
// disable the function again:
a.free;

// filter all events coming from a mouse
a = HIDFunc.usage( { |...args| args.postln; }, nil, \Mouse );
// disable the function again:
a.free;

// filter all events coming from an X-axis (could be from a mouse or a joystick or a gamepad)
a = HIDFunc.usage( { |...args| args.postln; }, \X );
// disable the function again:
a.free;

// filter all events coming from an X-axis or an Y-axis (could be from a mouse or a joystick or a gamepad)
a = HIDFunc.usage( { |...args| args.postln; }, [\X,\Y] );
// disable the function again:
a.free;


// usage of argTemplate: matching the rawValue (is the default behaviour)

// only react when the values are below zero:
a = HIDFunc.usage( { |...args| args.postln; }, [\X,\Y], argTemplate: { |val| val < 0 } );
a.free;

// only match when rawvalue == -1
a = HIDFunc.usage( { |...args| args.postln; }, [\X,\Y], argTemplate: -1 );
a.free;

// only match when rawvalue is one of [-3,-2,-1]
a = HIDFunc.usage( { |...args| args.postln; }, [\X,\Y], argTemplate: [-3,-2,-1] );
a.free;

// usage of argTemplate: matching the scaled value when smaller than 0.5
a = HIDFunc.usage( { |...args| args.postln; }, [\X,\Y], argTemplate: { |val| val < 0.5 }, argTemplateType: \value );
a.free;


// using deviceInfo rather than deviceUsage (you can add more device specifications to match)
a = HIDFunc.usage( { |...args| args.postln; }, \X, deviceInfo: ( usageName: \Mouse ) );
a.free;

a = HIDFunc.usage( { |...args| args.postln; }, nil, deviceInfo: ( usageName: \Mouse ) );
a.free;
::

]
@section{SUBSECTION}
  Filtering based on usage ID


@racketblock[
// filter by usage ID 48 on usage page 1
a = HIDFunc.usageID( { |...args| args.postln; }, 48, 1 );
a.free;

// filter by usage ID 48 or 49 on usage page 1
a = HIDFunc.usageID( { |...args| args.postln; }, [48,49], 1 );
a.free;

// filter by any usage ID on usage page 1
a = HIDFunc.usageID( { |...args| args.postln; }, nil, 1 );
a.free;

// filter by usage ID 48 on usage page 1, of a device with an empty string as a name (fill in the name of your mouse there).
a = HIDFunc.usageID( { |...args| args.postln; }, 48, 1, "" );
a.free;

// filter by usage ID 48 on usage page 1, of a device with path "/dev/hidraw2" (adapt this path to the device you want to match)
a = HIDFunc.usageID( { |...args| args.postln; }, 48, 1, deviceInfo: ( path: "/dev/hidraw2" )  );
a.free;
::

]
@section{SUBSECTION}
  Filtering based on a device


@racketblock[
// filter for device with name "", and element with usage \X.
a = HIDFunc.device( { |...args| args.postln; }, \X, "" );
a.free;
::

]
@section{SUBSECTION}
  Filtering based on a prototype element


@racketblock[
// create an prototype element with usageName \X
c = HIDElementProto.new.usageName_( \X );
a = HIDFunc.proto( { |...args| args.postln; }, c );
a.free;
::


]
@section{SUBSECTION}
  Filtering based on an element ID


@racketblock[
// filter for elements with element id 6:
a = HIDFunc.element( { |...args| args.postln; }, 6 );
a.free;
::
]


