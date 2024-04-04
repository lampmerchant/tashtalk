# TashTalk

## Elevator Pitch

It's a LocalTalk interface, contained entirely within a single Microchip PIC12F1840 (8 pins, ~$1.50) microcontroller.

It handles all the time-sensitive aspects of LocalTalk, bidirectionally bitbanging the SDLC and FM0 (a.k.a. differential
Manchester) based protocol at the data link and physical layers, sending and responding to control frames with CRC
calculation and checking, and collision avoidance and retransmission. It can also respond to any number/combination of
node IDs for use in bridging applications. It can be interfaced directly to user-mode software on a Raspberry Pi or 
BeagleBone or full PC, or it can be part of a larger embedded system. It slices, it dices, etc.


## Project Status

Stable.


## Caveats

Because of the PIC12F1840's limited memory and the way its UART is used, the host's UART needs to be able to handle a baud
rate of 1 Mbps. Conventional UARTs don't expect to be pushed beyond 115.2 kbps. In addition, the PIC has a 128-byte receiver
queue, which is considerably smaller than the largest possible LocalTalk frame (605 bytes). As such, the host needs to respect
when the CTS hardware flow control line is deasserted so the queue doesn't overflow and be quick about resuming transmission
when CTS is reasserted so the queue doesn't underflow either.

Also, "single-chip" doesn't include the separate and necessary driver/receiver chip for RS-422/485. Sorry. =)


## Building Firmware

Building the firmware requires Microchip MPASM, which is included with their development environment, MPLAB.  Note that you **must** use MPLAB X version 5.35 or earlier or MPLAB 8 as later versions of MPLAB X have removed MPASM.


## PCBs and Products

* [TashTalkHat](https://68kmla.org/bb/index.php?threads/tashtalk-single-chip-localtalk-interface.38955/page-4#post-422138)
  by bdurbrow - hat for Raspberry Pi using v1.x firmware
* [AirTalk](https://68kmla.org/bb/index.php?threads/introducing-and-interest-check-airtalk-wireless-plug-and-play-localtalk-dongles.39661/)
  by cheesestraws - wireless plug-and-play LocalTalk dongle
   * [Buy](https://airtalk.shop/product/airtalk-complete/) from [airtalk.shop](https://airtalk.shop/)
* [TashTalk 2 Hat](https://github.com/lampmerchant/tashtalk/tree/main/tashtalk2-rpihat) - hat for Raspberry Pi using v2.x firmware
   * Buy [fully assembled](https://ko-fi.com/s/4d01fa5b8a) or as kit with [regular](https://ko-fi.com/s/60b561a0e3) or
     [stackable](https://ko-fi.com/s/64219426b8) header from [Tashtari](https://ko-fi.com/tashtari)
* Yours? =)


## Version Comparison

**AirTalk users: do not upgrade your TashTalk PIC to v2.x, it will stop working because of the change in pinout.**

| Version | Pinout                      | CRC Calculation                              |
| ------- | --------------------------- | -------------------------------------------- |
| v1.0    | RA3 input, RA5 output       | Self-generated frames only                   |
| v2.0    | RA3 !MCLR, RA5 input/output | Self-generated frames only                   |
| v2.1    | RA3 !MCLR, RA5 input/output | Self-generated frames, optionally all frames |


### UART Protocol

As of the time of this writing, the base UART protocol is unchanged between all available versions.  v2.1 adds the "Set Features"
command, but this can be ignored and is not used by tashtalkd.


### Pinout Change in v2.0

This change was made in order to enable applications where it is desirable to disable LocalTalk and allow other circuitry to use
the RS-422 driver - pulling RA3/!MCLR low will hold the PIC in reset, tristating its outputs.  Using RS-422 interface ICs such as
the SN65HVD series, RA5 can be connected to both the receiver output and the driver input while RA4/Driver Enable is connected to
both the (active low) receiver enable and the (active high) driver enable.
