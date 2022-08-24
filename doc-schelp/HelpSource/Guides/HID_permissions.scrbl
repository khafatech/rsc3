#lang scribble/manual
@(require (for-label racket))

@title{HID permissions}
 Details how to configure your computer to set the permissions to access HID's@section{categories}
  External Control>HID
@section{related}
  Guides/Working_with_HID, Classes/HID

@section{section}
  On Linux

You will have to set the permissions with udev to be correct, create a file in the folder: 
@racketblock[ /etc/udev/rules.d/ ::, and name it (e.g.) ]

@racketblock[ 90-hidraw-permissions.rules ::

In the file, you will need this line:

]

@racketblock[ KERNEL=="hidraw*", SUBSYSTEM=="hidraw", MODE="0664", GROUP="plugdev" ::

This will give read and write permissions to hidraw-devices to users that are in the ]

@racketblock[ plugdev :: group on your system.

To check whether you belong to that group execute the command ]

@racketblock[ groups :: in the terminal.

After you have added the udev rules file, you can access the device after plugging and replugging the device.

You can change the permission manually (as root) from the terminal with the command:
]

@racketblock[
sudo chmod 664 /dev/hidraw*
sudo chgrp plugdev /dev/hidraw*
::

Check the permissions with:

]

@racketblock[
"ls /dev/hidraw* -lah".unixCmd;
::
]


