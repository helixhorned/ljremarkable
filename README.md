
ljremarkable: A LuaJIT interface to the reMarkable tablet and Raspberry Pi ⟷ rM application
===========================================================================================

Table of Contents
-----------------

**[Introduction](#introduction)**\
**[Requirements](#requirements)**\
**[Preliminary setup](#preliminary-setup)**\
**[Building](#building)**\
**[Acknowledgements](#acknowledgements)**\
**[License](#license)**


Introduction
------------

[LuaJIT]: https://luajit.org/
[Raspbian]: https://www.raspberrypi.org/downloads/raspbian/

*ljremarkable* is a LuaJIT library supporting its main application: making the reMarkable
tablet usable as a front-end to the Raspberry Pi. The application, called `grabscreen` for
historical reasons, allows you to view the content of your screen on the rM as well as take
input from the tablet and translate it to mouse events in a Raspbian desktop user session.

Requirements
------------

* reMarkable 1 tablet
* Raspberry Pi 4 running 32-bit [Raspbian]<sup>**[1]**</sup>
* LuaJIT 2.1 [built](https://luajit.org/install.html) for the Pi and for the rM

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

### On the Raspberry Pi

The user needs to be made a member of group `video`, since the application will read
directly from the Linux framebuffer device `/dev/fb0`:

    sudo adduser $USER video

(This is the last form as documented in `man 8 adduser`.) \
For me, the home directory of the new user has persisted across updates of the reMarkable
software.

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

<sup>**[2]**</sup> <small>Checking for updates happens in intervals with an exponential
backoff, so enabling this may shorten the time from typing something to seeing the effects
of the input on the rM. However, the preferred mode of interaction is via the
tablet.</small>

### On the rM: adding the user to required groups

On the reMarkable, the non-root user needs to be a member of groups `video` (for writing to
the framebuffer) and `input` (for reading the input). Since the `busybox`-provided `adduser`
does not seem to support the convenience form present in Raspbian, this has to be done by
editing `/etc/group` directly. Refer to `man 5 group` for its format.

> **CAUTION**: Do **not** leave the SSH session as `root` until you have verified that the
edits have their intended effect! That is, after editing, one should verify that it is still
possible to log in as `root` and as the new user.

### Putting it together

For connecting the Pi to the rM, a USB cable can be used initially. Experimental evidence
suggests better chance of working with thicker, shorter cables. On my Pi4 (but not the Pi3),
using the cable shipped with the rM fails, with Linux reporting in `dmesg`:

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

Building
--------

### Building LuaJIT

The only software that needs to be built in the classical sense is
[LuaJIT](https://luajit.org/install.html). The application is implemented entirely in Lua
with heavy usage of LuaJIT's FFI. It seems reasonable to match the compiler flags with those
used in the [reMarkable SDK](https://remarkable.engineering/deploy/sdk/) and so build two
versions: one targeting the Pi and one for the rM. The rM build of LuaJIT would just need
the following in `src/Makefile`, replacing the line reading "`CCOPT_arm=`":

    CCOPT_arm= -mfpu=neon -mfloat-abi=hard -mcpu=cortex-a9

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

### Building the application

[musl]: https://musl.libc.org/
[Alpine Linux]: https://alpinelinux.org/
[Alpine Docker image]: https://hub.docker.com/_/alpine

The final application is bundled into a single file `grabscreen.app.lua` obtained with `make
app`.

> **TODO**: document prerequisites.

For the time being, please refer to the [`Dockerfile`](./Dockerfile). Since it describes an
environment under the [musl]-based [Alpine Linux] distribution (using an official [Alpine
Docker image]), slight adjustments are made relative to a build under Raspbian.

Acknowledgements
----------------

The [`libremarkable`](https://github.com/canselcik/libremarkable) library by Can Selcik was
crucial in obtaining an understanding for the functioning of the reMarkable.

Several C declarations for use with the LuaJIT FFI are obtained directly from a header of
the "legacy C implementation" of that project.


License
-------

Copyright (C) 2019-2020 Philipp Kutin.

The main application `grabscreen.lua` is GPL3-licensed. See
[`LICENSE.GPL3.txt`](LICENSE.GPL3.txt) for details. \
The "library part" -- the non-generated source code in this repository that is used by the
application -- is MIT-licensed. See [`LICENSE.MIT.txt`](LICENSE.MIT.txt) for details.
