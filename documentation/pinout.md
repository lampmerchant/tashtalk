# TashTalk Pinout

## Version 2.x

```
                       .--------.
               Supply -|01 \/ 08|- Ground
    LocalTalk <-> RA5 -|02    07|- RA0 --> UART Out
Driver Enable <-- RA4 -|03    06|- RA1 <-- UART In
        !MCLR --> RA3 -|04    05|- RA2 --> UART Flow Control
                       '--------'
```


## Version 1.x

```
                       .--------.
               Supply -|01 \/ 08|- Ground
LocalTalk Out <-- RA5 -|02    07|- RA0 --> UART Out
Driver Enable <-- RA4 -|03    06|- RA1 <-- UART In
 LocalTalk In --> RA3 -|04    05|- RA2 --> UART Flow Control
                       '--------'
```


## Remarks

### Driver Enable

Driver Enable is high when the RS-422 transmitter should drive the LocalTalk
bus according to RA5 and low when the RS-422 receiver should drive RA5 (in
v2.x) or RA3 (in v1.x) according to the LocalTalk bus.  In v2.x, it is
synchronized such that RA5 is a push-pull output when Driver Enable is high
and tristated when Driver Enable is low.


### UART Pins

If TashTalk is connected to a device that is considered a DTE from an RS-232
perspective (the typical case), RA0 should be connected to the device's RxD
pin, RA1 to the device's TxD pin, and RA2 to the device's CTS pin.  If
connected to a device that is considered a DCE, RA0 should be connected to the
device's TxD pin, RA1 to the device's RxD pin, and RA2 to the device's RTS pin.

RA2 is low when the connected device should send data and high when the
connected device should not send data.


### !MCLR

When !MCLR is high, TashTalk operates normally.  When !MCLR is low, TashTalk is
held in reset and all its pins are tristated.  !MCLR has an internal weak
pullup to supply and may safely be left unconnected if normal operation is
desired at all times.
