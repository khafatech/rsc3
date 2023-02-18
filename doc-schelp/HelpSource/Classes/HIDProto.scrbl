#lang scribble/manual
@(require (for-label racket))

@title{HIDProto}
 Prototype HID device to match with HIDFunc@section{categories}
  External Control>HID
@section{related}
   Classes/HIDFunc, Classes/HIDdef, Classes/HIDElementProto, Classes/HID, Classes/HIDInfo, Guides/Working_with_HID

@section{description}

Human input devices can be used as controllers for making music. This class can be used in conjunction with link::Classes/HIDFunc:: or link::Classes/HIDdef:: to match incoming messages with a particular link::Classes/HID:: device.

HIDProto has all the variables that specify an HID device. The more of these variables you specify, the more need to be matched when filtering the incoming HID data.

@section{CLASSMETHODS}
 

@section{METHOD}
  new
Create a new instance of HIDProto.


@section{METHOD}
  newType
Create a new instance of HIDProto based on usage and usagePage of the device.

@section{ARGUMENT}
  uName
Name of the usage id

@section{ARGUMENT}
  pName
Name of the usage page id

@section{returns}
   an HIDProto

@section{METHOD}
  newProduct
Create a new instance of HIDProto based on the product information.

@section{ARGUMENT}
  pName
The product name to match.

@section{ARGUMENT}
  vName
The vendor name to match.

@section{returns}
  an HIDProto


@section{METHOD}
  newFromDict
Create a new instance of HIDProto based on an IdentityDictionary with a set of parameters to match.

@section{ARGUMENT}
  dict
An IdentityDictionary with a set of parameters to match. The keys in the dictionary should be one of the instance variables of HIDProto.

@section{returns}
  an HIDProto


@section{INSTANCEMETHODS}
 

@section{PRIVATE}
  init

@section{SUBSECTION}
  Instance variables that can be used to match a device

@section{METHOD}
  id
The device id that should be matched. This is dependent on the order of opening HID devices.

@section{METHOD}
  productName
The product name to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  vendorName
The vendor name to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  productID
The product id to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  vendorID
The vendor id to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  interfaceNumber
The interface number to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  releaseNumber
The release number to match (see also link::Classes/HIDInfo::).


@section{METHOD}
  serialNumber
The serial number to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  path
The path to match (see also link::Classes/HIDInfo::).



@section{METHOD}
  usage
The usage ID of the device to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  usagePage
The usage page ID of the device to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  usageName
The usage name of the device to match (see also link::Classes/HIDInfo::).

@section{METHOD}
  pageName
The page name of the device to match (see also link::Classes/HIDInfo::).


@section{SUBSECTION}
  Methods to match

@section{METHOD}
  matches
Match the argument with the template.

@section{ARGUMENT}
  hid
An instance of HID.

@section{returns}
  a Boolean indicating whether the incoming HID matches the template

@section{METHOD}
  shouldMatch
The variables that should be matched when filtering

@section{returns}
  a Set with variable names.

@section{SUBSECTION}
  Methods to add matching parameters


@section{METHOD}
  addTypeMatch
Add a match for usage name and usage page name of the device.

@section{ARGUMENT}
  uName
The usage name to match

@section{ARGUMENT}
  pName
The page name to match

@section{METHOD}
  addProductMatch
Add a match for product name and vendor name of the device.

@section{ARGUMENT}
  pName
The product name to match

@section{ARGUMENT}
  vName
The vendor name to match


@section{METHOD}
  addDictionaryMatch
Add an IdentityDictionary with a set of parameters to match. The keys in the dictionary should be one of the instance variables of HIDProto.


@section{ARGUMENT}
  dict
An IdentityDictionary with a set of parameters to match.


@section{EXAMPLES}
 


@racketblock[
b = HIDProto.newFromDict( ( path: "/dev/hidraw2" ) );

a = HIDFunc.usage( { |...args| args.postln; }, \X, deviceInfo: b );
a.free

b = HIDProto.newType( \Mouse, \GenericDesktop );

a = HIDFunc.usage( { |...args| args.postln; }, \X, deviceInfo: b );
a.free;

b = HIDProto.newProduct( "USB Mouse", "Logitech" );

a = HIDFunc.usage( { |...args| args.postln; }, \X, deviceInfo: b );
a.free;
]


