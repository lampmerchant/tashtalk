# TashTalk

## Elevator Pitch

It's a LocalTalk interface, contained entirely within a single Microchip PIC12F1840 (8 pins, ~$1.50) microcontroller.

It handles all the time-sensitive aspects of LocalTalk, bidirectionally bitbanging the SDLC and FM0 (a.k.a. differential
Manchester) based protocol at the data link and physical layers, sending and responding to control frames with CRC
calculation and checking, and collision avoidance and retransmission. It can also respond to any number/combination of
node IDs for use in bridging applications. It can be interfaced directly to user-mode software on a Raspberry Pi or 
BeagleBone or full PC, or it can be part of a larger embedded system. It slices, it dices, etc.


## Project Status

Stable enough to release a v1.0.0.


## Caveats

Because of the PIC12F1840's limited memory and the way its UART is used, the host's UART needs to be able to handle a baud
rate of 1 Mbps. Conventional UARTs don't expect to be pushed beyond 115.2 kbps. In addition, the PIC has a 128-byte receiver
queue, which is considerably smaller than the largest possible LocalTalk frame (605 bytes). As such, the host needs to respect
when the CTS hardware flow control line is deasserted so the queue doesn't overflow and be quick about resuming transmission
when CTS is reasserted so the queue doesn't underflow either.

Also, "single-chip" doesn't include the separate and necessary driver/receiver chip for RS-422/485. Sorry. =)


## Building Firmware

Building the firmware requires Microchip MPASM, which is included with their development environment, MPLAB.  Note that you **must** use MPLAB X version 5.35 or earlier or MPLAB 8 as later versions of MPLAB X have removed MPASM.


## Projects Using It

* [TashTalkHat](https://68kmla.org/bb/index.php?threads/tashtalk-single-chip-localtalk-interface.38955/page-4#post-422138)
  by bdurbrow - hat for Raspberry Pi
* [AirTalk](https://68kmla.org/bb/index.php?threads/introducing-and-interest-check-airtalk-wireless-plug-and-play-localtalk-dongles.39661/)
  by cheesestraws - wireless plug-and-play LocalTalk dongle
* Yours? =)
