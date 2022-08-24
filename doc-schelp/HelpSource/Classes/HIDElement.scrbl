#lang scribble/manual
@(require (for-label racket))

@title{HIDElement}
 A class describing an element of an HID device@section{categories}
   External Control>HID
@section{related}
  Classes/HID, Guides/Working_with_HID, Classes/HIDCollection, Classes/HIDUsage

@section{description}

An HIDElement describes an element, or a control, of an HID device.
These are created for the device automatically when you open a device. The only interaction a user will have with elements are to query the properties of the element (with 
@racketblock[.postElement::), query the ]

@racketblock[value:: or ]

@racketblock[rawValue::, or set the value, set the ]

@racketblock[repeat:: property or set an ]

@racketblock[action:: to be performed when new data comes in.

]
@section{CLASSMETHODS}
 

@section{PRIVATE}
  new


@section{INSTANCEMETHODS}
 

@section{PRIVATE}
  setValueFromInput, mapValueFromRaw, initElementRepeat


@section{METHOD}
  action
Set or get the action to be performed upon receiving element data. The function will be passed the following arguments: the value (mapped between 0 and 1) and the raw value.


@section{METHOD}
  value
set or get the value of the element. Setting only makes sense for an output element.

@section{ARGUMENT}
  val
The raw value to send to the device


@section{METHOD}
  repeat
By default element's data from the device is not updated unless the data is changing. For certain elements however, you may want to receive updates even if the data is not changing, e.g. for scrollwheel of mice.

@section{ARGUMENT}
  rp
a Boolean to turn repeat on or off


@section{METHOD}
  rawValue
The raw value of the element.

@section{METHOD}
  logicalValue
The logical value of the element. In principal the same as value.

@section{METHOD}
  physicalValue
The physical value of the element. This can be calculated from the raw value and the device's specification for conversion: the physical minimum, the physical maximum, the unit and unit exponent. How the conversion works is described in the USB HID standard documentation. @section{NOTE}
 The conversion is not yet implemented in the backend.::

@section{METHOD}
  arrayValue
The array value of the element. This value is only of importance for those elements which can represent multiple usages, such as from keyboards. In that case it indicates the key that is pressed, and by adding this number to the usage of the element you know which function the key has.

@section{NOTE}
  values from a keyboard are parsed in two ways: first as the element how they come in, second just with the usage and the value (on or off) as the data comes in. ::


@section{SUBSECTION}
  Properties of the element

@section{METHOD}
  postElement
Post a human readable description of the element to the post window.


@section{METHOD}
  id
The index of this element. This index may vary between operating systems.

@section{METHOD}
  device
Get the device to which this element belongs.
@section{NOTE}
  do not set this as a user!::

@section{returns}
  an instance of HID

@section{METHOD}
  collection
Get the collection index to which this element belongs.


@section{METHOD}
  usage
Retrieve the usage index of this collection.

@section{returns}
  a Number - the usage index of this element


@section{METHOD}
  usagePage
Retrieve the usage page index of this element.

@section{returns}
  a Number- the usage page index


@section{METHOD}
  usageName
Retrieve the usage name of this element. The name is looked up from the standardized HID usage tables using the usage page index.

@section{returns}
  a String - the usage name

@section{METHOD}
  pageName
Retrieve the page name of this element. The name is looked up from the standardized HID usage tables using the usage page index.

@section{returns}
  a String - the usage page name


@section{METHOD}
  type
A byte describing the type of element.

@section{returns}
  a number describing the type of element.

@section{METHOD}
  typeSpec
The type of element, decoded from the type byte.

@section{returns}
  an Array with Strings describing the type of element.

@section{METHOD}
  ioType
Type of the element, input (1), output (2) or feature (3)

@section{returns}
  a Number indicating the ioType

@section{METHOD}
  iotypeName
Type of the element, one of 
@racketblock[\input::, ]

@racketblock[\output::, or ]

@racketblock[\feature::

]
@section{returns}
  a Symbol indicating the type


@section{METHOD}
  logicalMin
Minimum value of the range that is to be expected. This is reported by the device. The element's raw value is mapped between the logical minimum and maximum to obtain the element's value.


@section{METHOD}
  logicalMax
Maximum value of the range that is to be expected. This is reported by the device. The element's raw value is mapped between the logical minimum and maximum to obtain the element's value.


@section{METHOD}
  physicalMin
Minimum value of the range that is to be expected in a physical sense. This is reported by the device. For example, for a hat switch the physical range may be the direction in degrees in which the hat switch is pointing.

@section{METHOD}
  physicalMax
Maximum value of the range that is to be expected in a physical sense. This is reported by the device. For example, for a hat switch the physical range may be the direction in degrees in which the hat switch is pointing.

@section{METHOD}
  unit
Index for the unit of the physical range.

@section{METHOD}
  unitExponent
The exponent for the physical range.


@section{METHOD}
  usageMin
Minimum value of the usage range that is to be expected. This is reported by the device. This is only relevant for elements that report array values.


@section{METHOD}
  usageMax
Maximum value of the usage range that is to be expected. This is reported by the device. This is only relevant for elements that report array values.

@section{METHOD}
  getUsages
This method is used to get a dictionary of all the usages that this element produces. In most cases an element has only one usage, but in the case of an array-element it will have several uses (for a keyboard, an element represents one keypress, but they can be various different keys).

@section{METHOD}
  reportID
The report ID with which this element receives the data. This is a low level device specific identifier


@section{METHOD}
  reportSize
The report size in bits with which this element receives the data. This is a low level device specific identifier

@section{METHOD}
  reportIndex
The report index with which this element receives the data. This is a low level device specific identifier


@section{EXAMPLES}
 


@racketblock[
HID.findAvailable; // find available devices
HID.postAvailable; // post available devices
~myhid = HID.open( 1103, 53251 ); // open a device
~myhid.postElements; // post available elements
//Set actions for the second element:
~myhid.elements[1].action = { |...args| args.postln; };
]


