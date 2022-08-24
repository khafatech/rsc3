#lang scribble/manual
@(require (for-label racket))

@title{LIDInfo}
 Helper class for LID to hold the information about an LID device.@section{categories}
  Platform>Linux, External Control>HID
@section{related}
  Classes/LID

@section{description}

This class contains the basic information about an LID device to access and open it.
The class is mostly used internally and rarely accessed directly by the user.

This class is populated with information read from the device, and represents some of the internal information of the device.


@section{INSTANCEMETHODS}
 

@section{METHOD}
  open
Open the device that is described by this LIDInfo

@section{returns}
  an LID - the device

@section{METHOD}
  postInfo
post the LIDInfo in a human readable way

@section{METHOD}
  findArgs
An Array of the arguments that can be passed into 
@racketblock[LID.findBy:: to find this device.

]
@section{METHOD}
  name
The name of the device, as reported by the device itself.

@section{METHOD}
  vendorID
The vendor ID of the device, this is a number encoded by the device itself.

@section{METHOD}
  productID
The product ID of the device, this is a number encoded by the device itself.

@section{METHOD}
  path
The path of the device, this is a path defined by the operating system, essential to distinguish devices with the same vendor and product ID from each other.

@section{METHOD}
  version
The version number of the device.

@section{METHOD}
  physical
The physical location of the device, as defined by the operating system.

@section{METHOD}
  unique
The unique identifier of the device, as defined by the operating system.

@section{METHOD}
  bustype
The bustype of this device


