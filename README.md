
ljremarkable: A LuaJIT interface to the reMarkable tablet and Raspberry Pi ⟷ rM application
===========================================================================================

Table of Contents
-----------------

**[Introduction](#introduction)**\
**[Requirements](#requirements)**\
**[Preliminary setup](#preliminary-setup)**\
**[Installation](#installation)**\
**[Running](#running)**\
**[Using](#using)**\
**[Details and troubleshooting](#details-and-troubleshooting)**\
**[Acknowledgements](#acknowledgements)**\
**[License](#license)**


Introduction
------------

[LuaJIT]: https://luajit.org/
[Raspberry Pi OS]: https://www.raspberrypi.org/downloads/raspberry-pi-os/

*ljremarkable* is a LuaJIT library supporting its main application: making the reMarkable
tablet usable as a front-end to the Raspberry Pi. The application, called `grabscreen` for
historical reasons, allows you to view the content of your screen on the rM as well as take
input from the tablet and translate it to mouse events in a Raspberry Pi OS desktop user
session.

Requirements
------------

* reMarkable 1 tablet
* Raspberry Pi 4 running 32-bit [Raspberry Pi OS]<sup>**[1]**</sup>
* [LuaJIT] 2.1

<sup>**[1]**</sup> <small>Pi 3 might work and was used originally, but has not since been
tested. Some aspects of the setup are simpler on the Pi 3 though. Likewise, other
distributions have not been tested (with one exception).</small>

Preliminary setup
-----------------

### On the reMarkable tablet

It is assumed that SSH access has been [set up](https://remarkablewiki.com/tech/ssh).

The application will refuse to run as root, so a user account needs to be created on the
reMarkable: logging in as `root`, this is simply done using `busybox adduser` (passing the
desired user name as argument). For convenience, it makes sense that the newly created user
account is made accessible [without providing a
password](https://remarkablewiki.com/tech/ssh/#setting_up_ssh-keys).

For me, the home directory of the new user has persisted across updates of the reMarkable
software.

### On the Raspberry Pi

The user needs to be made a member of group `video`, since the application will read
directly from the Linux framebuffer device `/dev/fb0`:

    sudo adduser $USER video

(This is the last form as documented in `man 8 adduser`.)

Because of the direct framebuffer access, it is not possible to have the DRM VC4 V3D driver
enabled on the Pi 4: in
[`/boot/config.txt`](https://www.raspberrypi.org/documentation/configuration/config-txt/README.md),
the line starting with `dtoverlay` in

    [pi4]
    # Enable DRM VC4 V3D driver on top of the dispmanx display stack
    dtoverlay=vc4-fkms-v3d

has to be commented out.

*Optionally*, the user may also be added to group `input`. Doing so will enable the
application to react on (otherwise discarded) keyboard events by initiating a re-scan for
updated screen regions.<sup>**[2]**</sup>

Finally, for the application to carry out mouse actions on the desktop for certain gestures
made on the tablet, `xdotool` needs to be installed, available as Raspberry Pi OS `apt`
package. Running it does not require any special privileges.

<sup>**[2]**</sup> <small>Checking for updates happens in intervals with an exponential
backoff, so enabling this may shorten the time from typing something to seeing the effects
of the input on the rM. However, the preferred mode of interaction is via the
tablet.</small>

### On the rM: adding the user to required groups

On the reMarkable, the non-root user needs to be a member of groups `video` (for writing to
the framebuffer) and `input` (for reading the input). Since the `busybox`-provided `adduser`
does not seem to support the convenience form present in Raspberry Pi OS, this has to be
done by editing `/etc/group` directly. Refer to `man 5 group` for its format.

> **CAUTION**: Do **not** leave the SSH session as `root` until you have verified that the
edits have their intended effect! That is, after editing and saving the file, one should
verify that it is still possible to log in as `root` and as the new user.

### Putting it together

For connecting the Pi to the rM, a USB cable can be used initially. Experimental evidence
suggests better chance of working with thicker, shorter cables. On my Pi 4 (but not the Pi
3), using the cable shipped with the rM fails, with Linux reporting in `dmesg`:

    usb 1-1-port1: Cannot enable. Maybe the USB cable is bad?

(The reMarkable can still be charged this way, though.)

A successful wired link looks like this in `dmesg`:

    usb 1-1.1: new high-speed USB device number 10 using xhci_hcd
    usb 1-1.1: New USB device found, idVendor=04b3, idProduct=4010, bcdDevice= 4.09
    usb 1-1.1: New USB device strings: Mfr=1, Product=2, SerialNumber=0
    usb 1-1.1: Product: RNDIS/Ethernet Gadget
    usb 1-1.1: Manufacturer: Linux 4.9.84-zero-gravitas with 2184000.usb
    cdc_ether 1-1.1:1.0 usb0: register 'cdc_ether' at usb-0000:01:00.0-1.1, CDC Ethernet Device, <MAC address>

For the best experience, it makes sense to set up the Raspberry Pi as a wireless access
point.

Installation
------------

### LuaJIT

The application is implemented entirely in Lua with heavy usage of LuaJIT's FFI.

It is possible to use the binary from the Raspberry Pi OS `luajit` APT package on the reMarkable.

#### Comparison of `/proc/cpuinfo`

| | reMarkable | Raspberry Pi 4 |
| --- | --- | --- |
| `model name`       |  `ARMv7 Processor rev 10 (v7l)`                       | `ARMv7 Processor rev 3 (v7l)` |
| `Features`         |  `half thumb fastmult vfp edsp neon vfpv3 tls vfpd32` | additionally: `vfpv4 idiva idivt lpae evtstrm crc32` |
| `CPU implementer`  |  `0x41`                                               | same |
| `CPU architecture` |  `7`                                                  | same |
| `CPU variant`      |  `0x2`                                                | `0x0` |
| `CPU part`         |  `0xc09`                                              | `0xd08` |
| `CPU revision`     |  `10`                                                 | `3` |

On the Pi, `lscpu` gives:

    Vendor ID:           ARM
    Model:               3
    Model name:          Cortex-A72

### Packaging the application

[musl]: https://musl.libc.org/
[Alpine Linux]: https://alpinelinux.org/
[Alpine Docker image]: https://hub.docker.com/_/alpine

The final application is bundled into a single file `grabscreen.app.lua`, obtained by
invoking `make app`.

> **TODO**: document prerequisites.

For the time being, please refer to the [`Dockerfile`](./Dockerfile). Since it describes an
environment under the [musl]-based [Alpine Linux] distribution (using an official [Alpine
Docker image]), slight adjustments are made relative to a build under Raspberry Pi OS.

### Placing files

- On the Pi: `make install`, which places the generated `grabscreen.app.lua` and the helper
  script `pi-rM-control.sh` into `$HOME/bin` by default.
- To the reMarkable: `make upload`. Besides the unity app file, this will also copy the
  necessary resources, currently only `rM_ul_eye_menu_hidden_46-28.dat`.

Several variables such as the rM user and host name are configurable in
[`config.make`](config.make).


Running
-------

The application needs to be started first on the reMarkable to await a request for a
connection initiated by the counterpart on the Pi.

    Usage:
      grabscreen.app.lua [--fork] c <host name or IPv4 address>              # on the Raspberry Pi
      grabscreen.app.lua [--fork] s [<timeout waiting for connection (ms)>]  # on the reMarkable
    
    (...)
    
    A passed host name is resolved by reading and parsing /etc/hosts.

The helper script [`pi-rM-control.sh`](pi-rM-control.sh), intended to be invoked from
hotkeys, makes the above procedure a one-step process and features related convenience
functionality:

    Usage: pi-rM-control.sh {after-login|ping|connect|kill} [<rM-host>]
     * <rM-host> defaults to 'remarkable'
     * If '/usr/bin/pigs' is present, expects GPIO pins 12 and 13 to
       be connected to an LED for success and failure, respectively

> **Attention**:
>  * If the mentioned GPIO pins are already used otherwise, the script can and should be
>    adapted
>  * Commands `connect` and `kill` currently kill any LuaJIT process indescriminately using
>    `killall luajit`
>  * When connecting an LED, do not forget an appropriate resistor
>
> <small>Feel free to notify me if the limitations implied by the the first two points cause
> any inconvenience.</small>

Using
-----

Since the application co-exists with the rM's main user-visible process `xochitl`, there
needs to be a way for the two to live together peacefully. Currently, the Pi screen will
display on the rM only if the touch-sensitive "eye" of the tablet UI is in the upper left
and "looking down", that is, when the menu is in its default *portrait* orientation and
*hidden*.

### Controls

Various gestures carried out on the tablet lead to the injection of mouse events into the
graphical session from which the application was started on the Pi.

Some terminology: The endpoint of the application running on the Pi is called the *client*
because it initiates the connection to the waiting *server* on the rM.**<sup>[3]</sup>** The
screen portion of the rM display that contains the visible portion of the Pi desktop will
here be called *view* for brevity.

* Tap on a point on the view: single mouse click. If held for more than a short threshold
  duration,**<sup>[4]</sup>** a right click is issued instead of a left click. Holding even
  longer (currently, for at least two seconds) produces a middle click.

* Drag with a single finger starting on the Pi screen portion:

  - When the finger rests on the initial tap position less than a short threshold
    time before moving,**<sup>[4]</sup>** only vertical swipes are allowed, within some
    tolerance. On the Pi, a number of mouse wheel events proportional to the length of the
    trail of the finger are injected. The final position may be off the view.  Before the
    event is injected, the mouse is temporarily moved to the point *initially tapped* on the
    view.
    
    This provides a fairly reasonable emulation of dragging a page beneath one's finger in a
    variety of use cases such as browsers, PDF readers and text editors. The initial
    positioning means that e.g frames in web pages will work as expected. The downside of
    using mouse wheel events is that the distance traveled on the tablet screen only
    approximately and coincidentally corresponds to the respective distance on the Pi
    desktop.

  - When the threshold time is exceeded, *general drag mode* is activated: the swipe does
    not need to be vertical, but has to be fully inside the view. On the Pi desktop, the
    mouse is moved to the initially tapped point, left-clicked and held, moved to the final
    point, and released. This can be used for dragging a page in a distance-matched
    fashion with software that interprets the sequence accordingly, such as PDF readers.
    
    This gesture can also be used to select a portion of text in e.g. a web browser. Take
    care though: `xochitl` being active means that it will interpret a left or right swipe
    as moving one page forward or back, respectively. In order to prevent it from doing so,
    it seems to suffice to carry out the drag slowly. Note that non-horizontal drags are not
    interpreted, so another way to avoid accidentally changing the page (and overdrawing the
    Pi screen view) is to move the finger in an arc.

* Drag with a single finger from *below* the Pi screen portion to *above* it: request the
  client to re-send the complete screen contents. Useful after an accidental page change, or
  when artifacts have accumulated on the rM screen and one wishes to refresh the view.

* Drag with a single finger from the top right corner to the bottom right corner of the rM
  screen: shut down the connection and exit the application.

**<sup>[3]</sup>** This is exactly reverse to the notion of the server being side that is
biased towards *sending* data.\
**<sup>[4]</sup>** (currently half a second)

### Miscellaneous features

#### Frame rate limiting

Under several update modes of the reMarkable screen, including the one currently used in the
application, changing the value of a pixel to or from a gray may transition via one of the
two extreme colors, leading to a visible "flash". This makes many videos appear garbled as
pixels spend most of the time in that indirect transition stage. For this reason, a
heuristic is implemented to detect areas with fast-changing content and send updates in them
only once in a while.
  
> **Note**: This feature works best on videos in which the the whole image changes every
> frame, for example because the camera is moving. It does not work as well with videos
> where there is a static background.
>
> **CAUTION**: It is important to keep in mind that the image displayed on the rM is
> always somewhat behind what would be displayed on a monitor, even without this
> feature. (This is even more compounded by latency incurred by connecting wirelessly.)
> Currently, it is possible that a tap on a point in the view will issue a mouse click on
> a portion that has changed in the time since the tap.

#### Other

* It is explicitly a feature to be able to unplug the keyboard from the Pi, even if the
  application is watching keyboard input (because it was started with group `input`
  permissions present).

* Unintentional but maybe useful: when starting to erase any portion of the screen using the
  menu eraser tool, the whole rendering of the Pi screen is cleared.

Details and troubleshooting
---------------------------

**Q:** There is a horizontal stripe at the top missing!

**A:** The application operates on two kinds of *tiles* into which it decomposes the Pi
framebuffer image: 8-pixel-by-8-pixel tiles is the granularity at which changes are
detected, each such tile being sampled for a pseudo-random pixel within it. Over the
network, 16x16 tiles are sent to the rM for reconstruction of the image.

Currently, the height of the target image has to be evenly divisibly by the side length of
these "big tiles". The remainder of dividing 1080 (presumably the maximum vertical
resolution when not using `vc4-fkms-v3d`) by 16 is 8. The decision to crop at the top is
somewhat arbitrary, and it is planned to remove the limitation in the future.

**Q:** There is a vertical stripe at the right not covered!

**A:** The visible portion of the rM screen is 1404 pixels wide. A line of the rM
framebuffer is actually 1408 pixels wide and thus *is* evenly divisible by 16. The decision
to clamp the horizontal resolution to 1392 is due to the fact that scroll bars happen to be
already usable only with difficulty, being (i) reduced in physical size and (ii) likely
suffering from an analogous problem as when attempting to write with the stylus near the
right or bottom borders.


Acknowledgements
----------------

The [`libremarkable`](https://github.com/canselcik/libremarkable) library by Can Selcik was
crucial in obtaining an understanding for the functioning of the reMarkable.

Several C declarations for use with the LuaJIT FFI are obtained directly from a header of
the "legacy C implementation" of that project.

Portions of this software are copyright (C) 1996-2020 The FreeType Project
([www.freetype.org](https://www.freetype.org)). All rights reserved.


License
-------

Copyright (C) 2019-2020 Philipp Kutin.

The main application `grabscreen.lua` is GPL3-licensed. See
[`LICENSE.GPL3.txt`](LICENSE.GPL3.txt) for details. \
The "library part" -- the non-generated source code in this repository that is used by the
application -- is MIT-licensed, unless otherwise noted. See
[`LICENSE.MIT.txt`](LICENSE.MIT.txt) for details.
