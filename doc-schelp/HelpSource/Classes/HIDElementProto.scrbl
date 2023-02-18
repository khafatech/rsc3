#lang scribble/manual
@(require (for-label racket))

@title{HIDElementProto}
 Prototype HID element to match with HIDFunc@section{categories}
  External Control>HID
@section{related}
  Classes/HIDFunc, Classes/HIDdef, Classes/HIDProto, Classes/HID, Classes/HIDElement, Classes/HIDInfo, Guides/Working_with_HID

@section{description}

Human input devices can be used as controllers for making music. This class can be used in conjunction with link::Classes/HIDFunc:: or link::Classes/HIDdef:: to match incoming messages with a particular link::Classes/HID:: device.

HIDElementProto has all the variables that specify an HID element. The more of these variables you specify, the more need to be matched when filtering the incoming HID data.


@section{CLASSMETHODS}
 

@section{METHOD}
  new
Create a new instance of HIDElementProto.

@section{METHOD}
  newType
Create a new instance of HIDElementProto based on usage id and usage page id of the element.

@section{ARGUMENT}
  uName
Name of the usage ID to match

@section{ARGUMENT}
  pName
Name of the usage page to match

@section{returns}
  an HIDElementProto


@section{METHOD}
  newTypeID
Create a new instance of HIDElementProto based on usage id and usage page id of the element.

@section{ARGUMENT}
  uID
Usage ID to match

@section{ARGUMENT}
  pID
Usage page ID to match

@section{returns}
  an HIDElementProto

@section{METHOD}
  newFromDict
Create a new instance of HIDElementProto based on an IdentityDictionary with a set of parameters to match.


@section{ARGUMENT}
  dict
An IdentityDictionary with a set of parameters to match. The keys in the dictionary should be one of the instance variables of HIDElementProto.

@section{returns}
  an HIDElementProto



@section{INSTANCEMETHODS}
 

@section{SUBSECTION}
  Instance variables that can be used to match a device

@section{METHOD}
  id
The element index that should be matched. This index may vary between operating systems (see also link::Classes/HIDElement::).


@section{METHOD}
  usageName
The usage name of the element to match (see also link::Classes/HIDElement::).

@section{METHOD}
  pageName
The usage page name of the element to match (see also link::Classes/HIDElement::).

@section{METHOD}
  usage
The usage index of the element to match (see also link::Classes/HIDElement::).

@section{METHOD}
  usagePage
The usage page index of the element to match (see also link::Classes/HIDElement::).


@section{METHOD}
  usageMin
The minimum usage index of the element to match (see also link::Classes/HIDElement::).


@section{METHOD}
  usageMax
The maximum usage index of the element to match (see also link::Classes/HIDElement::).



@section{METHOD}
  type
The type of the element to match (see also link::Classes/HIDElement::).


@section{METHOD}
  typeSpec
The typeSpec of the element to match (see also link::Classes/HIDElement::).


@section{METHOD}
  ioType
The IO type of the element to match - input (1), output (2) or feature (3) (see also link::Classes/HIDElement::).


@section{METHOD}
  iotypeName
The IO type of the element to match - 
@racketblock[\input::, ]

@racketblock[\output:: or ]

@racketblock[\feature:: (see also link::Classes/HIDElement::).


]
@section{SUBSECTION}
  Methods to match

@section{METHOD}
  matches
Match the argument with the template.


@section{ARGUMENT}
  ele
An instance of HIDElement

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
Add a match for usage name and usage page name of the element.

@section{ARGUMENT}
  uName
The usage name to match

@section{ARGUMENT}
  pName
The page name to match

@section{METHOD}
  addTypeIDMatch
Add a match for usage id and usage page id of the element.

@section{ARGUMENT}
  uID
The usage id to match

@section{ARGUMENT}
  pID
The usage page id to match


@section{METHOD}
  addDictionaryMatch
Add an IdentityDictionary with a set of parameters to match. The keys in the dictionary should be one of the instance variables of HIDElementProto.

@section{ARGUMENT}
  dict
An IdentityDictionary with a set of parameters to match.



@section{EXAMPLES}
 


@racketblock[
// create an prototype element with usageName \X
c = HIDElementProto.new.usageName_( \X );
a = HIDFunc.proto( { |...args| args.postln; }, c );
a.free;
]


