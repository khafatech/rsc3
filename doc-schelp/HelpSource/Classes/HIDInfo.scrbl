#lang scribble/manual
@(require (for-label racket))

@title{HIDInfo}
 This class contains the basic information about an HID device to access and open it.@section{categories}
  External Control>HID
@section{related}
  Classes/HID, Guides/Working_with_HID

@section{description}

This class contains the basic information about an HID device to access and open it.
The class is mostly used internally and rarely accessed directly by the user.

This class is populated with information read from the device, and represents some of the internal information of the device.

@section{CLASSMETHODS}
 

@section{PRIVATE}
  new

@section{INSTANCEMETHODS}
 

@section{PRIVATE}
 printOn, setUsageAndPage

@section{METHOD}
  open
Open the device that is described by this HIDInfo

@section{returns}
  an HID - the device

@section{METHOD}
  postInfo
post the HIDInfo in a human readable way


@section{METHOD}
  path
The path of the device, this is a path defined by the operating system, and thus not the same across platforms, but essential to distinguish devices with the same vendor and product ID from each other.

@section{METHOD}
  vendorID
The vendor ID of the device, this is a number encoded by the device itself, and the same across platforms.

@section{METHOD}
  productID
The product ID of the device, this is a number encoded by the device itself, and the same across platforms.

@section{METHOD}
  vendorName
The vendor name of the device, this is a string encoded by the device itself, and the same across platforms.

@section{METHOD}
  productName
The product name of the device, this is a string encoded by the device itself, and the same across platforms.

@section{METHOD}
  serialNumber
The serial number of the device. This is dependent on the operating system, e.g. on Linux it is not set.

@section{METHOD}
  releaseNumber
The release number of the device, this is a number encoded by the device itself, and the same across platforms.

@section{METHOD}
  interfaceNumber
Type of interface of the device, can be an index standing for USB, Bluetooth, etc.


@section{METHOD}
  usage
Retrieve the usage index of this collection.

@section{returns}
  a Number - the usage index of this collection

@section{METHOD}
  usagePage
Retrieve the usage page index of this collection.

@section{returns}
  a Number- the usage page index

@section{METHOD}
  usageName
Retrieve the usage name of this collection. The name is looked up from the standardized HID usage tables using the usage page index.

@section{returns}
  a String - the usage name


@section{METHOD}
  pageName
Retrieve the page name of this collection. The name is looked up from the standardized HID usage tables using the usage page index.

@section{returns}
  a String - the usage page name


