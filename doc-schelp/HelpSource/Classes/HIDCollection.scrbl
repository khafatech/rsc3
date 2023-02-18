#lang scribble/manual
@(require (for-label racket))

@title{HIDCollection}
 A class describing a group of elements of an HID device@section{categories}
   External Control>HID
@section{related}
  Classes/HID, Guides/Working_with_HID, Classes/HIDElement, Classes/HIDUsage

@section{description}

An HIDCollection defines a group of controls on a low level of the HID device, i.e. the device reports these groups. The main use to access it is to retrieve information about what kind of function the HID device performs, e.g. a GamePad, a Joystick or a Mouse.

This class is populated with information read from the device, and represents some of the internal information of the device.

@section{CLASSMETHODS}
 

@section{PRIVATE}
 new


@section{INSTANCEMETHODS}
 
@section{PRIVATE}
 printOn

@section{METHOD}
  postCollection
Post a human readable description of the collection to the post window.


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


@section{METHOD}
  type
The type of collection.

@section{returns}
  a number describing the type of collection.


@section{METHOD}
  device
Get the device to which this collection belongs.
@section{NOTE}
  do not set this as a user!::

@section{returns}
  an instance of HID


@section{METHOD}
  id
Index of this collection

@section{returns}
  a Number

@section{METHOD}
  parent
Index of the parent of this collection.

@section{returns}
  a Number

@section{METHOD}
  numElements
Number of elements in this collection

@section{returns}
  a Number

@section{METHOD}
  firstElement

The first element that is part of this collection.

@section{returns}
  a Number - an index indicating the first element



@section{METHOD}
  numCollections
Number of (child) collections in this collection

@section{returns}
  a Number

@section{METHOD}
  firstCollection
The first collection that is part of this collection.

@section{returns}
  a Number - an index indicating the first collection


