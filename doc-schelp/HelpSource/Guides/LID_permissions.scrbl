#lang scribble/manual
@(require (for-label racket))

@title{LID permissions}
 Details how to configure your computer to set the permissions to access LID's@section{categories}
  External Control>HID
@section{related}
  Guides/HID_permissions, Classes/LID

@section{section}
  On Linux

You will have to set the permissions with udev to be correct, create a file in the folder: 
@racketblock[ /etc/udev/rules.d/ ::, and name it (e.g.) ]

@racketblock[ 90-lid-permissions.rules ::

In the file, you will need this line:

]

@racketblock[ KERNEL=="event[0-9]*", NAME="input/%k", MODE="0664", GROUP="plugdev" ::

This will give read and write permissions to lid-devices to users that are in the ]

@racketblock[ plugdev :: group on your system.

To check whether you belong to that group execute the command ]

@racketblock[ groups :: in the terminal.

After you have added the udev rules file, you can access the device after plugging and replugging the device.

You can change the permission manually (as root) from the terminal with the command:
]

@racketblock[
sudo chmod 664 /dev/input/event*
sudo chgrp plugdev /dev/input/event*
::

Check the permissions with:

]

@racketblock[
"ls /dev/input/event* -lah".unixCmd;
::
]


