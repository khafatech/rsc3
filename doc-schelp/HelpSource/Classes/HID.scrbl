#lang scribble/manual
@(require (for-label racket))

@title{HID}
 This class provides access to human input devices, or in short HID, such as joysticks, gamepads, mice, keyboard, and so on.@section{categories}
  External Control>HID
@section{related}
  Classes/HIDFunc, Guides/Working_with_HID, Classes/HIDElement, Classes/HIDCollection, Classes/HIDUsage, Classes/HIDInfo

@section{description}

Human input devices can be used as controllers for making music. This class provides you with access to them in a simple and transparent way.

The development of this SuperCollider implementation of HID access was funded by the SuperCollider community and BEK, Bergen Elektronisk Kunst, Bergen, Norway, http://www.bek.no

@section{SUBSECTION}
  Introduction
In general using an Human Input Device follows this scheme:

@section{DEFINITIONLIST}
 
	## Find available devices:
	|| 
@racketblock[ HID.findAvailable; ::
	## Print a readable list of available devices:
	|| ]

@racketblock[ HID.postAvailable; ::
	## Open a specific device:
	|| ]

@racketblock[ ~myhid = HID.open( 1103, 53251 ); ::
	## Set actions for specific elements:
	|| ]

@racketblock[ ~myhid.elements[1].action = { |...args| args.postln; };
::

See LINK::Guides/Working_with_HID:: for a full introduction.

::


]
@section{CLASSMETHODS}
 

@section{PRIVATE}
  doPrAction, prInitHID, prCloseAll, prOpenDevice, prCloseDevice, prHIDDeviceClosed, prGetDeviceInfo, prGetNumberOfCollections, prGetCollectionInfo, prGetNumberOfElements, prGetElementInfo, prSetElementOutput, prSetElementRepeat, prbuildDeviceList, deviceClosed, prHIDElementData, prHIDDeviceData, basicNew, mergeUsageDict, removeUsageDict

@section{SUBSECTION}
  Finding devices

@section{METHOD}
  findAvailable
queries the operating system which HID devices are attached to the system and can be accessed. When using HID this is the first method you need to execute, before you can access any device.

@section{returns}
  an IdentityDictionary of available devices

@section{METHOD}
  available
A dictionary of available devices, or rather info about them in an instance of LINK::Classes/HIDInfo::, populated by the method findAvailable

@section{returns}
  an IdentityDictionary

@section{METHOD}
  postAvailable
posts a human readable list of available HID devices and their properties (see also LINK::Classes/HIDInfo::)


@section{METHOD}
  findBy
Find devices in the available device dictionary by specifying one or more characteristics of the device

@section{ARGUMENT}
  vendorID
The vendor ID of the device, this is a number encoded by the device itself, and the same across platforms.

@section{ARGUMENT}
  productID
The product ID of the device, this is a number encoded by the device itself, and the same across platforms.

@section{ARGUMENT}
  path
The path of the device, this is a path defined by the operating system, and thus not the same across platforms, but essential to distinguish devices with the same vendor and product ID from each other.

@section{ARGUMENT}
  serial
The serial number of the device. This is dependent on the operating system, e.g. on Linux it is not set.

@section{ARGUMENT}
  releaseNumber
The release number of the device, this is a number encoded by the device itself, and the same across platforms.

@section{returns}
  an IdentityDictionary of devices the match the search query, or nil if no arguments are given


@section{METHOD}
  availableUsages
A dictionary of available usages from all HIDs, populated automatically when devices are opened and closed.

@section{returns}
  an IdentityDictionary


@section{METHOD}
  postAvailableUsages
posts a human readable list of available HID usages and their properties (see also LINK::Classes/HIDElement:: and LINK::Classes/HIDUsage::)


@section{SUBSECTION}
  Opening devices

@section{METHOD}
  open
Open a device with a given vendorID, product ID and optionally a path.

@section{ARGUMENT}
  vendorID
The vendor ID of the device

@section{ARGUMENT}
  productID
The product ID of the device

@section{ARGUMENT}
  path
(optional) The path in the operating system, e.g. "/dev/hidraw0" on Linux.
If not specified, the method will look for a matching device in the device list, and open the first match it finds.

@section{returns}
  The HID device - an instance of HID.

@section{METHOD}
  new
Same as HID.open


@section{METHOD}
  openPath
Open a device using its path in the operating system

@section{ARGUMENT}
  path
The path in the operating system, e.g. "/dev/hidraw0" on Linux

@section{returns}
  The HID device - an instance of HID.

@section{METHOD}
  openAt
Open a device using its index in the dictionary of available devices

@section{ARGUMENT}
  index
The index into the dictionary of available devices

@section{returns}
  The HID device - an instance of HID.


@section{METHOD}
  openDevices
A dictionary of the opened devices

@section{returns}
  an IdentityDictionary


@section{SUBSECTION}
  Adding functions to HID events

Whenever data comes in from an opened HID device, there are two types of actions fired. An action for the incoming element data and an action for the device, indicating that there has been a change in one of the elements. In most cases you will want to use the first action; only in cases where the order of parsing the element data is important, you may want to use the second type - e.g. when dealing with very accurately timed button press combinations.

There are three levels where you can set actions:
@section{LIST}
 
	## at the global level - called for any HID device, for any element
	## at the device level - called for the specific device, for any element
	## at the element level - called for the specific element of the specific device
::

Alternately, you can also use the LINK::Classes/HIDFunc:: interface.



@section{METHOD}
  debug
When set to true, the incoming data from any opened HID device will be printed to the post window.



@section{METHOD}
  action
Set or get the action to be performed upon receiving element data from the device. The function will be passed the following arguments: the value (mapped between 0 and 1), the raw value, element usage page, the element usage, the element id, the device id, the device (an instance of HID).

@section{ARGUMENT}
  function
The function to be performed upon receiving element data from the device


@section{METHOD}
  addRecvFunc
add a function to the internal FunctionList that will be evaluated whenever element data comes in from an open device. The arguments passed to the function are as defined above.
Use this method if you want to add actions to HID functions from classes you write, so that you still keep the option to add an action on the fly from user code.

@section{ARGUMENT}
  function
The function to be added to the list.


@section{METHOD}
  removeRecvFunc
remove a function to the internal FunctionList that will be evaluated whenever data comes in from a device.


@section{ARGUMENT}
  function
The function to remove from the list, this must be a reference to the Function that was originally added to the list


@section{METHOD}
  deviceAction
set an action or function to be performed whenever there is an update to any device's elements.

@section{ARGUMENT}
  function
The function to be performed upon receiving data from the device


@section{METHOD}
  addDevFunc
add a function to the internal FunctionList that will be evaluated whenever data comes in from a device.

@section{ARGUMENT}
  function
The function to be performed upon receiving data from the device


@section{METHOD}
  removeDevFunc
remove a function to the internal FunctionList that will be evaluated whenever data comes in from a device.


@section{ARGUMENT}
  function
The function to remove from the list, this must be a reference to the Function that was originally added to the list




@section{SUBSECTION}
  Managing the HID subsystem

The following methods are used internally to initialize and finalize the HID subsystem, but in rare cases you may wish to manage these methods manually.

@section{METHOD}
  initializeHID
Initialize the HID subsystem, this method is called automatically when calling the method findAvailable.


@section{METHOD}
  running
Indicates whether or not the HID subsystem is running.


@section{METHOD}
  closeAll
This method is called automatically upon Shutdown, if the HID subsystem was initialized. It can be stopped manually, in order to save system resources. This method will close all opened HID devices.


@section{INSTANCEMETHODS}
 

@section{PRIVATE}
 init, getInfo, getElements, getElementInfo, getCollections, getCollectionInfo, getUsages, id, valueAction, valueDeviceAction, info_, prDeviceClosed


@section{METHOD}
  elements
An IdentityDictionary holding all the elements, i.e. controls, of the device


@section{METHOD}
  findElementWithUsage
Find all elements with a certain usage and usagePage.

@section{ARGUMENT}
  elUsage
The usage index of the element

@section{ARGUMENT}
  elUsagePage
The usage page of the element

@section{returns}
  an Array with the found elements


@section{METHOD}
  getElementWithID
Get the element with the given index

@section{ARGUMENT}
  elid
The index of the element

@section{returns}
  the HIDElement

@section{METHOD}
  close
Close the HID device, closing a device is asynchronous. You can set a closeAction (see below), which will be performed when the device closes.

@section{METHOD}
  isOpen
Returns true if the device is open, false if the device was closed.

returns: a Boolean


@section{METHOD}
  collections
An IdentityDictionary holding all the collections, i.e. groups of controls, of the device



@section{SUBSECTION}
  Adding functionality to the device

@section{METHOD}
  debug
When set to true, the incoming data from this HID device will be printed to the post window.


@section{METHOD}
  closeAction
Function to be performed when device is closed.


@section{METHOD}
  action
Set or get the action to be performed upon receiving element data from the device. The function will be passed the following arguments: the value (mapped between 0 and 1), the raw value, element usage page, the element usage, the element id

@section{ARGUMENT}
  function
The function to be performed upon receiving element data from the device


@section{METHOD}
  deviceAction
set an action or function to be performed whenever there is an update to any of the device's elements.

@section{ARGUMENT}
  function
The function to be performed upon receiving data from the device



@section{SUBSECTION}
  Posting human readable information about the device

@section{METHOD}
  postInfo
Post the HIDInfo of this device in a human readable format

@section{METHOD}
  postCollections
Post information about all the collections of this device in a human readable format

@section{METHOD}
  postElements
Post information about all the elements of this device in a human readable format

@section{METHOD}
  postInputElements
Post information about all the input elements of this device in a human readable format

@section{METHOD}
  postOutputElements
Post information about all the output elements of this device in a human readable format

@section{METHOD}
  postFeatureElements
Post information about all the feature elements of this device in a human readable format


@section{METHOD}
  postUsages
Post information about all the usages of this device in a human readable format


@section{SUBSECTION}
  Properties of the device

@section{METHOD}
  info
Retrieve the HIDInfo of this device

@section{returns}
  an instance of HIDInfo


@section{METHOD}
  usage
Retrieve the usage index of a collection of this device, or the main usage of the device (if called without an argument).

@section{ARGUMENT}
  collectionID
The collection of which to retrieve the usage. Default is 0, which should define the primary usage of the device.

@section{returns}
  the usage index of this device

@section{METHOD}
  usageName
Retrieve the usage name of a collection of this device, or the main usage of the device (if called without an argument). The name is looked up from the standardized HID usage tables using the usage index.

@section{ARGUMENT}
  collectionID
The collection of which to retrieve the usage. Default is 0, which should define the primary usage of the device.

@section{returns}
  the usage name of this device


@section{METHOD}
  usagePage
Retrieve the usage page index of a collection of this device, or the main page of the device (if called without an argument). The name is looked up from the standardized HID usage tables using the usage page index.

@section{ARGUMENT}
  collectionID
The collection of which to retrieve the usage page. Default is 0, which should define the primary usage of the device.

@section{returns}
  the usage page index of this device


@section{METHOD}
  pageName

Retrieve the page name of a collection of this device, or the main page of the device (if called without an argument). The name is looked up from the standardized HID usage tables using the usage page index.

@section{ARGUMENT}
  collectionID
The collection of which to retrieve the usage page name. Default is 0, which should define the primary usage of the device.

@section{returns}
  the usage page name of this device


@section{METHOD}
  vendor
Retrieve the vendor id of this device

@section{returns}
  the vendor id


@section{METHOD}
  product
Retrieve the product id of this device

@section{returns}
  the product id


@section{METHOD}
  usages
Retrieve the usages of the elements of this device.

@section{returns}
  an IdentityDictionary with usages as keys and lists of elements as corresponding elements



@section{EXAMPLES}
 


@racketblock[
HID.findAvailable; // check which devices are attached
~myhid = HID.open( 1103, 53251 ); // open the Run'N' Drive game controller

s.boot; // boot the server

Ndef( \sinewave, { |freq=500, amp=0.1| SinOsc.ar( freq, 0, amp * 0.2 ) } );
Ndef( \sinewave ).play;

~freqRange = [500, 5000, \exponential].asSpec; // create a frequency range

HIDdef.usage( \freq, { |value| Ndef( \sinewave ).set( \freq, ~freqRange.map( value ) ); }, \X );
HIDdef.usage( \amp, { |value| Ndef( \sinewave ).set( \amp, value ); }, \Y );
]


