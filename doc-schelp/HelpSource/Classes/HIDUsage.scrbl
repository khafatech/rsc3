#lang scribble/manual
@(require (for-label racket))

@title{HIDUsage}
 Helper class to read usage information from HID usage tables@section{categories}
  External Control>HID
@section{related}
  Classes/HID, Classes/HIDElement, Classes/HIDCollection, Guides/Working_with_HID

@section{description}

HID functionality is described by the USB HID standard usage tables. Each element and collection has a usage page and index, describing the type of control that it provides. This class allows to query the name of a usage and page based on the indices read from the device. This class is primarily used internally by other HID classes.


@section{CLASSMETHODS}
 

@section{PRIVATE}
  initClass


@section{METHOD}
  getUsageDescription
Retrieve the standard usage name and pagename of an HID usage element or collection.

@section{ARGUMENT}
  usagePage
usage page number

@section{ARGUMENT}
  usage
usage index

@section{returns}
  an Array with the pageName and usageName



@section{METHOD}
  hutDirectory
Directory where the yaml files with the HID usage tables are stored.


@section{METHOD}
  readHUTFile
Reads and parses the HID usage table file. Called from getUsageDescription to read in the usage table.

@section{ARGUMENT}
  yamlfile
the filename of the yamlfile with a particular usage table, relative to the hutDirectory.

@section{returns}
  an IdentityDictionary representing the table

@section{METHOD}
  getUsageIds
Retrieve usage id and page id from the usageName.

@section{ARGUMENT}
  usageName
the usage name

@section{returns}
  an Array with the page id and the usage id


@section{METHOD}
  idsToName
Retrieve the standard usage name and pagename of an HID usage element or collection.

@section{ARGUMENT}
  page
the usage page id

@section{ARGUMENT}
  usage
the usage id

@section{returns}
  the usage name

@section{METHOD}
  usageIDsToName
MultiLevelIdentityDictionary containing a map of page ids, usage ids to usage names.

@section{METHOD}
  usageNameToIDs
IdentityDictionary containing a map of usageNames to page ids and usage ids.



@section{EXAMPLES}
 

Get the usage description for a collection or element with usage page 1 and usage index 5


@racketblock[
HIDUsage.getUsageDescription( 1, 5 );
]


