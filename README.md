# TashTalk

## Elevator Pitch

It's a LocalTalk interface, contained entirely within a single Microchip PIC12F1840 (8 pins, ~$1.50) microcontroller.

It handles all the time-sensitive aspects of LocalTalk, bidirectionally bitbanging the SDLC and FM0 (a.k.a. differential
Manchester) based protocol at the data link and physical layers, sending and responding to control frames with CRC
calculation and checking, and collision avoidance and retransmission. It can also respond to any number/combination of
node IDs for use in bridging applications. It can be interfaced directly to user-mode software on a Raspberry Pi or 
BeagleBone or full PC, or it can be part of a larger embedded system. It slices, it dices, etc.


## Project Status

Stable and fairly battle-tested.


## Caveats

Because of the PIC12F1840's limited memory and the way its UART is used, the host's UART needs to be able to handle a baud
rate of 1 Mbps. Conventional UARTs don't expect to be pushed beyond 115.2 kbps. In addition, the PIC has a 128-byte receiver
queue, which is considerably smaller than the largest possible LocalTalk frame (605 bytes). As such, the host needs to respect
when the CTS hardware flow control line is deasserted so the queue doesn't overflow and be quick about resuming transmission
when CTS is reasserted so the queue doesn't underflow either.

Also, "single-chip" doesn't include the separate and necessary driver/receiver chip for RS-422/485. Sorry. =)


## Building Firmware

Building the firmware requires Microchip MPASM, which is included with their development environment, MPLAB.  Note that you
**must** use MPLAB X version 5.35 or earlier or MPLAB 8 as later versions of MPLAB X have removed MPASM.


## PCBs and Products

### End User Hardware

* [AirTalk](https://68kmla.org/bb/index.php?threads/introducing-and-interest-check-airtalk-wireless-plug-and-play-localtalk-dongles.39661/) by [cheesestraws](https://68kmla.org/bb/index.php?members/cheesestraws.19339/)
  * Plug-and-play WiFi-LocalTalk dongle
  * [Buy](https://airtalk.shop/product/airtalk-complete/) from [airtalk.shop](https://airtalk.shop/)


### Serial Adapters

* TashTalkHat by [bdurbrow](https://68kmla.org/bb/index.php?members/bdurbrow.6275/)
  * Raspberry Pi hat using v1.x firmware
  * Pending release
  * [Forum post](https://68kmla.org/bb/index.php?threads/tashtalk-single-chip-localtalk-interface.38955/page-4#post-422138)
* TashTalk 2 Hat by [Tashtari](https://github.com/lampmerchant)
  * Raspberry Pi hat using v2.x firmware
  * [Files](https://github.com/lampmerchant/tashtalk/tree/main/tashtalk2-rpihat)
  * See latest release in this repository for gerbers
  * Buy [fully assembled](https://ko-fi.com/s/4d01fa5b8a) or as kit with [regular](https://ko-fi.com/s/60b561a0e3) or [stackable](https://ko-fi.com/s/64219426b8) header from [Tashtari](https://ko-fi.com/tashtari)
* USB2LT by [twelvetone12](https://68kmla.org/bb/index.php?members/twelvetone12.23810/)
  * USB serial adapter using v2.x firmware
  * Pending release
  * [Forum thread](https://68kmla.org/bb/index.php?threads/usb2lt-tashtalk-usb-to-localtalk.45282/)

 
### Software

* tashtalkd by [Tashtari](https://github.com/lampmerchant)
  * Simple LocalTalk (via TashTalk) to LToUDP bridge
  * [Files](https://github.com/lampmerchant/tashtalk/tree/main/tashtalkd)
* MultiTalk by [sfiera](https://github.com/sfiera/)
  * Bridge between EtherTalk and LocalTalk (via LToUDP and TashTalk)
  * [Project page](https://github.com/sfiera/multitalk/)
* TashRouter by [Tashtari](https://github.com/lampmerchant)
  * Full-fledged AppleTalk router supporting EtherTalk and LocalTalk (via LToUDP and TashTalk)
  * [Project page](https://github.com/lampmerchant/tashrouter/)
* PicoATP by [Tashtari](https://github.com/lampmerchant)
  * Tiny AppleTalk stack for PIC12F1840 and other 8-bit mid-range core PICs
  * [Project page](https://github.com/lampmerchant/picoatp)


### Yours?

File a PR or an issue to add to these lists!


## Version Comparison

**AirTalk users:** do **not** upgrade your TashTalk PIC to v2.x, it will stop working because of the change in pinout.  See
the Compatibility Fix section below if you are having issues with Power Macintosh 6100 computers or certain LocalTalk
printers.

| Version | Pinout                      | CRC Calculation                              |
| ------- | --------------------------- | -------------------------------------------- |
| v1.0    | RA3 input, RA5 output       | Self-generated frames only                   |
| v2.0    | RA3 !MCLR, RA5 input/output | Self-generated frames only                   |
| v2.1    | RA3 !MCLR, RA5 input/output | Self-generated frames, optionally all frames |


### Pinout Change in v2.0

This change was made in order to enable applications where it is desirable to disable LocalTalk and allow other circuitry to
use the RS-422 driver - pulling RA3/!MCLR low will hold the PIC in reset, tristating its outputs.  Using RS-422 interface ICs
such as the SN65HVD series, RA5 can be connected to both the receiver output and the driver input while RA4/Driver Enable is
connected to both the (active low) receiver enable and the (active high) driver enable.


### UART Protocol

As of the time of this writing, the base UART protocol is unchanged between all available versions.  v2.1 adds the "Set
Features" command, but this can be ignored and is not used by tashtalkd.


### Internal Calculation of CRC

v2.1 adds optional calculation and checking of the frame CRC.  As of the time of this writing, this is known to be used only
by [PicoATP](https://github.com/lampmerchant/picoatp).


### Compatibility Fix

[v2.1.3](https://github.com/lampmerchant/tashtalk/releases/tag/v2.1.3) adds a fix for compatibility with the Power Macintosh
6100 and certain LocalTalk printers (possibly other devices as well).  This fix has also been backported to v1.x in
[v1.0.1](https://github.com/lampmerchant/tashtalk/releases/tag/v1.0.1) for AirTalk users and other users of devices based
around the v1.x pinout.  See the release notes for details.
