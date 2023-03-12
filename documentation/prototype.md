TashTalk v1.x Prototype Details
===============================

Raspberry Pi
------------

A Raspberry Pi 3 Model B was used.  Note that these instructions are specific to this model and may differ for other models.

### UART Setup

#### raspi-config

Under `Interface Options` > `Serial Port`, answer "No" to "Would you like a login shell to be accessible over serial?" and then "Yes" to "Would you like the serial port hardware to be enabled?".

#### /boot/config.txt

Add the following lines:

* `dtoverlay=miniuart-bt`
   * Ordinarily, the Bluetooth device uses the PL011 UART (/dev/ttyAMA0); this line forces it to use the mini-UART instead.  We want the PL011 UART available because it's the only one that supports hardware flow control.
* `gpio=16-17=a3`
   * This line switches GPIO16 and GPIO17 to be CTS and RTS for hardware flow control.

### Connections

| PIC12F1840 Pin       | RPi 3 Pin     |
| -------------------- | ------------- |
| 01 Supply            | 01 3v3 Power  |
| 02 RA5/LocalTalk Out |               |
| 03 RA4/Driver Enable |               |
| 04 RA3/LocalTalk In  |               |
| 05 RA2/UART CTS      | 36 GPIO16/CTS |
| 06 RA1/UART RX       | 08 GPIO14/TXD |
| 07 RA0/UART TX       | 10 GPIO15/RXD |
| 08 Ground            | 06 Ground     |


Driver/Receiver
---------------

While the LocalTalk bus is half-duplex, LocalTalk and PhoneNet dongles use a hybrid transformer to accommodate the SCC's separate TxD and RxD lines.  Because a PhoneNet dongle was used for the prototype, a pair of SN65HVD08 were used, one as a receiver and one as a driver.

Other RS-422/485 drivers may be suitable, but it is important to experimentally verify that their receivers output a logic '1' when the bus is not being driven.

### Connections

| Receiver SN65HVD08 Pin | PIC12F1840 Pin       | LocalTalk/PhoneNet |
| ---------------------- | -------------------- | ------------------ |
| 01 R                   | 04 RA3/LocalTalk In  |                    |
| 02 !RE                 | 08 Ground            |                    |
| 03 DE                  | 08 Ground            |                    |
| 04 D                   |                      |                    |
| 05 GND                 | 08 Ground            | SGnd               |
| 06 A                   |                      | RxD+               |
| 07 B                   |                      | RxD-               |
| 08 Vcc                 | 01 Supply            |                    |

| Transmitter SN65HVD08 Pin | PIC12F1840 Pin       | LocalTalk/PhoneNet |
| ------------------------- | -------------------- | ------------------ |
| 01 R                      |                      |                    |
| 02 !RE                    | 03 RA4/Driver Enable |                    |
| 03 DE                     | 03 RA4/Driver Enable |                    |
| 04 D                      | 02 RA5/LocalTalk Out |                    |
| 05 GND                    | 08 Ground            | SGnd               |
| 06 A                      |                      | TxD+               |
| 07 B                      |                      | TxD-               |
| 08 Vcc                    | 01 Supply            |                    |
