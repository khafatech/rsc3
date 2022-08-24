#lang scribble/manual
@(require (for-label racket))

@title{HIDdef}
 HID response reference definition@section{categories}
  External Control>HID
@section{related}
  Classes/HID, Classes/HIDFunc, Classes/OSCdef, Classes/MIDIdef, Guides/Working_with_HID

@section{description}

HIDdef provides a global reference to the functionality of its superclass HIDFunc. Essentially it stores itself at a key within a global dictionary, allowing replacement at any time. Most methods are inherited from its superclass.

@section{CLASSMETHODS}
 

@section{PRIVATE}
  initClass

@section{METHOD}
  all
Get the global dictionary of all HIDdefs.

@section{returns}
  An LINK::Classes/IdentityDictionary::


@section{METHOD}
  freeAll
Clears and deactivates all HIDdefs from the global collection.


@section{METHOD}
  new
Access an existing HIDdef. This is a shortcut to access an HIDdef created with one of the methods below, and allows to change its function, or call free on it.

@section{ARGUMENT}
  key
The key at which to store this HIDdef in the global collection. Generally this will be a link::Classes/Symbol::.

@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...


@section{METHOD}
  usage
Create a new, enabled HIDdef. If an HIDdef already exists at this key, its parameters will be replaced with the ones provided (args for which nil is passed will use the old values).

A convenience method to filter an incoming HID value based on the name of its control usage. E.g. the name of an X-axis of a joystick or the horizontal movement of a mouse has the name 
@racketblock[\X::. These usage names are standardized by manufacturers and are looked up in usage tables based on the information coming from the device. If you have an open HID device, you can look up the available usages with: ]

@racketblock[~hid.postUsages::.


]
@section{ARGUMENT}
  key
The key at which to store this HIDdef in the global collection. Generally this will be a link::Classes/Symbol::.

@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...

@section{ARGUMENT}
  elUsageName
The name of the usage to look for. This can be one usage name, or an array of usage names.

@section{ARGUMENT}
  devUsageName
The name of the device usage to look for, e.g. 
@racketblock[\GamePad:: or ]

@racketblock[\Mouse::. If left blank, the ]

@racketblock[HIDdef:: will match any device.

]
@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: with a more detailed filtering for a device.

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
@racketblock[HIDdef:: is ]

@racketblock[HIDUsageDispatcher::

]
@section{returns}
  A new instance of HIDdef which responds to a specific element usage and device type.


@section{METHOD}
  device
Create a new, enabled HIDdef. If an HIDdef already exists at this key, its parameters will be replaced with the ones provided (args for which nil is passed will use the old values).

A convenience method to filter an incoming HID value based on the name of the device. This type of HIDdef differs from 
@racketblock[HIDdef.usage:: in that it filter specifically by device name, rather than device usage, otherwise the arguments are the same.

]
@section{ARGUMENT}
  key
The key at which to store this HIDdef in the global collection. Generally this will be a link::Classes/Symbol::.

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
An link::Classes/IdentityDictionary:: with a more detailed filtering for a device.

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
@racketblock[HIDdef:: is ]

@racketblock[HIDDeviceDispatcher::

]
@section{returns}
  A new instance of HIDdef which responds to a specific element usage for a specific device.


@section{METHOD}
  usageID
Create a new, enabled HIDdef. If an HIDdef already exists at this key, its parameters will be replaced with the ones provided (args for which nil is passed will use the old values).

A convenience method to filter an incoming HID value based on the number of its control usage. If the device is using a non-standard usage number, then this method can be used to look for it. A controls usage is fully specified by the combination of its usage ID and its usage page.

@section{ARGUMENT}
  key
The key at which to store this HIDdef in the global collection. Generally this will be a link::Classes/Symbol::.

@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...usage

@section{ARGUMENT}
  elUsageID
The id (an link::Classes/Integer::) of the usage to look for. This can be one usage id, or an array of usage ids.

@section{ARGUMENT}
  elPageID
The id (an link::Classes/Integer::) of the page of the usage to look for. This can be one page id, or an array of page ids.

@section{ARGUMENT}
  deviceName
Since this type of HIDdef is meant for non-standardized controls, you can filter by a specific device name, rather than a general usage, similar to 
@racketblock[HIDdef.device::

]
@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: with a more detailed filtering for a device.


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
@racketblock[HIDdef:: is ]

@racketblock[HIDElementProtoDispatcher::

]
@section{returns}
  A new instance of HIDdef which responds to a specific element usage id for a specific device.




@section{METHOD}
  proto
Create a new, enabled HIDdef. If an HIDdef already exists at this key, its parameters will be replaced with the ones provided (args for which nil is passed will use the old values).

A convenience method to filter an incoming HID value based on a matching template of an element (a link::Classes/HIDElementProto::). If you have number of conditions for the element that should be matched, then this method can be used to look for it.

@section{ARGUMENT}
  key
The key at which to store this HIDdef in the global collection. Generally this will be a link::Classes/Symbol::.

@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...usage

@section{ARGUMENT}
  protoElement
The id (an link::Classes/Integer::) of the usage to look for. This can be one usage id, or an array of usage ids.

@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: with a more detailed filtering for a device.

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
@racketblock[HIDdef:: is ]

@racketblock[HIDElementProtoDispatcher::

]
@section{returns}
  A new instance of HIDdef which responds to a specific prototype element.




@section{METHOD}
  element
Create a new, enabled HIDdef. If an HIDdef already exists at this key, its parameters will be replaced with the ones provided (args for which nil is passed will use the old values).

A convenience method to filter an incoming HID value based on the index of its element. If the device is using something non-standard, or you want to access keyboard elements directly, then this method can be used to look for it. Note that the element index is not necessarily the same across different operating systems (i.e. it may vary between Linux and macOS and Windows).

@section{ARGUMENT}
  key
The key at which to store this HIDdef in the global collection. Generally this will be a link::Classes/Symbol::.

@section{ARGUMENT}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. It will be passed...

@section{ARGUMENT}
  elID
The id (an link::Classes/Integer::) of the element to look for. This can be one element id, or an array of element ids.

@section{ARGUMENT}
  deviceName
Since this type of HIDdef is meant for non-standardized elements, you can filter by a specific device name, rather than a general usage, similar to 
@racketblock[HIDdef.device::

]
@section{ARGUMENT}
  deviceInfo
An link::Classes/IdentityDictionary:: with a more detailed filtering for a device.

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
@racketblock[HIDdef:: is ]

@racketblock[HIDElementDispatcher::

]
@section{returns}
  A new instance of HIDdef which responds to a specific element id for a specific device.


@section{INSTANCEMETHODS}
 

@section{PRIVATE}
 addToAll, printOn

@section{METHOD}
  key
Get this HIDdef's key.

@section{returns}
  Usually a link::Classes/Symbol::.


@section{METHOD}
  free
Clears this HIDdef from the global collection and deactivates it.


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
HIDdef.usage( \example, { |...args| args.postln; }, \X, \Mouse );

// filter all events coming from a mouse
HIDdef.usage( \example, { |...args| args.postln; }, nil, \Mouse );

// filter all events coming from an X-axis (could be from a mouse or a joystick or a gamepad)
HIDdef.usage( \example, { |...args| args.postln; }, \X );

// filter all events coming from an X-axis or an Y-axis (could be from a mouse or a joystick or a gamepad)
HIDdef.usage( \example, { |...args| args.postln; }, [\X,\Y] );

// usage of argTemplate: matching the rawValue (is the default behaviour)

// only react when the values are below zero:
HIDdef.usage( \example, { |...args| args.postln; }, [\X,\Y], argTemplate: { |val| val < 0 } );

// only match when rawvalue == -1
HIDdef.usage( \example, { |...args| args.postln; }, [\X,\Y], argTemplate: -1 );

// only match when rawvalue is one of [-3,-2,-1]
HIDdef.usage( \example, { |...args| args.postln; }, [\X,\Y], argTemplate: [-3,-2,-1] );

// usage of argTemplate: matching the scaled value when smaller than 0.5
HIDdef.usage( \example, { |...args| args.postln; }, [\X,\Y], argTemplate: { |val| val < 0.5 }, argTemplateType: \value );

// using deviceInfo rather than deviceUsage (you can add more device specifications to match)
HIDdef.usage( \example, { |...args| args.postln; }, \X, deviceInfo: ( usageName: \Mouse ) );

HIDdef.usage( \example, { |...args| args.postln; }, nil, deviceInfo: ( usageName: \Mouse ) );

HIDdef( \example ).free;
::

]
@section{SUBSECTION}
  Filtering based on usage ID


@racketblock[
// filter by usage ID 48 on usage page 1
HIDdef.usageID( \example2, { |...args| args.postln; }, 48, 1 );

// filter by usage ID 48 or 49 on usage page 1
HIDdef.usageID( \example2, { |...args| args.postln; }, [48,49], 1 );

// filter by any usage ID on usage page 1
HIDdef.usageID( \example2, { |...args| args.postln; }, nil, 1 );

// filter by usage ID 48 on usage page 1, of a device with an empty string as a name (fill in the name of your mouse there).
HIDdef.usageID( \example2, { |...args| args.postln; }, 48, 1, "" );

// filter by usage ID 48 on usage page 1, of a device with path "/dev/hidraw2" (adapt this path to the device you want to match)
HIDdef.usageID( \example2, { |...args| args.postln; }, 48, 1, deviceInfo: ( path: "/dev/hidraw2" )  );

HIDdef( \example2 ).free;
::

]
@section{SUBSECTION}
  Filtering based on a device


@racketblock[
// filter for device with name "", and element with usage \X.
HIDdef.device( \example3, { |...args| args.postln; }, \X, "" );
HIDdef( \example3 ).free;
::

]
@section{SUBSECTION}
  Filtering based on a prototype element


@racketblock[
// create an prototype element with usageName \X
c = HIDElementProto.new.usageName_( \X );
HIDdef.proto( \example4, { |...args| args.postln; }, c );
HIDdef( \example4 ).free;
::


]
@section{SUBSECTION}
  Filtering based on an element ID


@racketblock[
// filter for elements with element id 6:
HIDdef.element( \example5, { |...args| args.postln; }, 6 );
HIDdef( \example5 ).free;
::
]


