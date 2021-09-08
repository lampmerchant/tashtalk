;;; 80 characters wide please ;;;;;;;;;;;;;;;;;;;;;;;;;; 8-space tabs please ;;;


;
;;;
;;;;;  TashTalk: Single-Chip UART-LocalTalk Interface
;;;
;


;;; License ;;;

;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Connections ;;;

;;;                                                               ;;;
;                               .--------.                          ;
;                       Supply -|01 \/ 08|- Ground                  ;
;    LocalTalk Out <---    RA5 -|02    07|- RA0/TX ---> UART TX     ;
;    Driver Enable <---    RA4 -|03    06|- RA1/RX <--- UART RX     ;
;     LocalTalk In --->    RA3 -|04    05|- RA2    ---> UART CTS    ;
;                               '--------'                          ;
;;;                                                               ;;;


;;; Assembler Directives ;;;

	list		P=PIC12F1840, F=INHX32, ST=OFF, MM=OFF, R=DEC, X=ON
	#include	P12F1840.inc
	__config	_CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_ON & _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_OFF & _CLKOUTEN_OFF & _IESO_OFF & _FCMEN_OFF
			;_FOSC_INTOSC	Internal oscillator, I/O on RA5
			;_WDTE_OFF	Watchdog timer disabled
			;_PWRTE_ON	Keep in reset for 64 ms on start
			;_MCLRE_OFF	RA3/!MCLR is RA3
			;_CP_OFF	Code protection off
			;_CPD_OFF	Data memory protection off
			;_BOREN_OFF	Brownout reset off
			;_CLKOUTEN_OFF	CLKOUT disabled, I/O on RA4
			;_IESO_OFF	Internal/External switch not needed
			;_FCMEN_OFF	Fail-safe clock monitor not needed
	__config	_CONFIG2, _WRT_OFF & _PLLEN_ON & _STVREN_ON & _LVP_OFF
			;_WRT_OFF	Write protection off
			;_PLLEN_ON	4x PLL on
			;_STVREN_ON	Stack over/underflow causes reset
			;_LVP_OFF	High-voltage on Vpp to program


;;; Macros ;;;

DELAY	macro	value		;Delay 3*W cycles, set W to 0
	movlw	value
	decfsz	WREG,F
	bra	$-1
	endm

DNOP	macro
	bra	$+1
	endm


;;; Constants ;;;

LR_FRM	equ	7	;Set when received frame registers have changed
CTS_PLS	equ	6	;Set when a CTS frame should trigger a send from queue


;;; Variable Storage ;;;

	cblock	0x70	;Bank-common registers
	
	FLAGS
	LR_BUF		;Receiver buffer
	LR_STATE	;Receiver state pointer
	UR_LEN		;Current length of UART receiver queue
	GBACKOFF	;Global backoff mask
	LBACKOFF	;Local backoff mask
	COL_HIST	;Collision history for last 8 transmissions
	DEF_HIST	;Deferral history for last 8 transmissions
	UR_DEST		;First five bytes of frame loaded for transmission
	UR_SRC		; "
	UR_TYPE		; "
	UR_4TH		; "
	UR_5TH		; "
	ATTEMPTS	;Number of attempts at this transmission
	D1
	D0
	
	endc

	cblock	0xD0	;Upper end of bank 1 registers
	
	NODEBM0		;Node ID bitmap
	NODEBM1		; "
	NODEBM2		; "
	NODEBM3		; "
	NODEBM4		; "
	NODEBM5		; "
	NODEBM6		; "
	NODEBM7		; "
	NODEBM8		; "
	NODEBM9		; "
	NODEBM10	; "
	NODEBM11	; "
	NODEBM12	; "
	NODEBM13	; "
	NODEBM14	; "
	NODEBM15	; "
	NODEBM16	; "
	NODEBM17	; "
	NODEBM18	; "
	NODEBM19	; "
	NODEBM20	; "
	NODEBM21	; "
	NODEBM22	; "
	NODEBM23	; "
	NODEBM24	; "
	NODEBM25	; "
	NODEBM26	; "
	NODEBM27	; "
	NODEBM28	; "
	NODEBM29	; "
	NODEBM30	; "
	NODEBM31	; "
	
	endc

	cblock	0x120	;Bank 2 registers
	
	FSR1L_TM	;Temp registers when using FSR1 to modify the node ID
	INDF1_TM	; bitmap
	LT_CRC1		;Running CRC value calculated by SendByte
	LT_CRC2		; "
	LT_CRCX		;Temp variable used by SendByte when calculating CRC
	LT_LENH		;Number of bytes to be read from queue by SendFromQueue
	LT_LENL		; "
	LT_BUF		;Buffer used to hold the byte being sent over LocalTalk
	LT_BUF2		;Temporary buffer
	LT_ONES		;Count of consecutive ones sent by LT transmitter
	LR_CCRC1	;CRC register used to calculate the CRC of incoming
	LR_CCRC2	; control frames
	
	endc

	cblock	0x16B	;Upper end of bank 2 registers
	
	LR_DEST		;First five bytes of last received frame
	LR_SRC		; "
	LR_TYPE		; "
	LR_CRC1		; "
	LR_CRC2		; "
	
	endc


;;; Vectors ;;;

	org	0x0		;Reset vector
	bra	Init

	org	0x4		;Interrupt vector


;;; Interrupt Handler ;;;

Interrupt
	movlp	high LtReceiver	;02
	btfsc	INTCON,IOCIF	;03
	goto	LtReceiver	;04(-05)
	movlb	0
	btfsc	PIR1,RCIF
	bra	RxInterrupt
	retfie

RxInterrupt
	movlb	3		;Get the received byte
	movf	RCREG,W		; "
	movwi	FSR0++		;Push it onto the queue
	bcf	FSR0L,7		;Wrap the queue around
	incf	UR_LEN,F	;Increment the queue length
	movlw	B'00001011'	;If the UART receiver queue length >= 64,
	movlb	2		; deassert CTS so the host stops sending us
	btfsc	UR_LEN,6	; data
	movwf	LATA		; "
	movlb	31		;Store the changed UART receiver push point
	movf	FSR0L,W		; back in its shadow register so it stays the
	movwf	FSR0L_SHAD	; same when we return from interrupt
	retfie


;;; Mainline ;;;

Init
	banksel	OSCCON		;32 MHz (w/PLL) high-freq internal oscillator
	movlw	B'11110000'
	movwf	OSCCON
	
	banksel	IOCAN		;RA4 sets IOCAN[IOCAF3] on pos/neg edge
	movlw	B'00001000'
	movwf	IOCAN
	movwf	IOCAP
	
	banksel	RCSTA		;UART async mode, 800 kbps
	movlw	B'01001000'
	movwf	BAUDCON
	clrf	SPBRGH
	movlw	9
	movwf	SPBRGL
	movlw	B'00100110'
	movwf	TXSTA
	movlw	B'10010000'
	movwf	RCSTA
	
	banksel	OPTION_REG	;Timer0 uses instruction clock
	movlw	B'11011111'
	movwf	OPTION_REG
	
	banksel	T1CON		;Timer1 ticks with instruction clock
	movlw	B'00000001'
	movwf	T1CON
	
	banksel	T2CON		;Timer2 has 1:16 prescaler, 1:10 postscaler, it
	movlw	B'01001010'	; interrupts after 20 * (PR2 + 1) microseconds
	movwf	T2CON
	
	banksel	ANSELA		;All pins digital, not analog
	clrf	ANSELA
	
	banksel	LATA		;Drivers off, CTS asserted
	movlw	B'00001111'
	movwf	LATA
	
	banksel	TRISA		;TX, RA5,4,2 outputs, RX, RA3 inputs
	movlw	B'00001010'
	movwf	TRISA
	
	clrf	UR_LEN		;Set up UART receiver queue (0x2000-0x207F),
	movlw	0x20		; for which FSRs are push (FSR0) and pop (FSR1)
	movwf	FSR0H		; pointers
	movwf	FSR1H
	clrf	FSR0L
	clrf	FSR1L
	
	banksel	PIE1		;UART Rx and IOC interrupts on, interrupt
	movlw	B'00100000'	; subsystem on
	movwf	PIE1
	movlw	B'11001000'
	movwf	INTCON
	
	bra	AwaitCommand

;entered with BSR = 2
AwaitNodeId
	btfsc	FLAGS,LR_FRM	;If there's been a change in incoming frame
	call	DealWithFrame	; regs, deal with the frame
	movf	UR_LEN,W	;If there aren't yet 32 bytes in the UART
	addlw	-32		; receiver queue, loop around again
	btfss	STATUS,C	; "
	bra	AwaitNodeId	; "
	movwf	UR_LEN		;Decrement the UART receiver queue size by 32
	movlw	B'00000100'	;If the pop off the queue dropped the length
	btfss	UR_LEN,6	; below 64, assert CTS so the host sends us
	iorwf	LATA,F		; data again
	movlb	1		;Switch to bank 1 where the node ID bitmap is
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM0		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM1		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM2		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM3		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM4		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM5		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM6		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM7		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM8		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM9		;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM10	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM11	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM12	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM13	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM14	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM15	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM16	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM17	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM18	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM19	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM20	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM21	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM22	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM23	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM24	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM25	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM26	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM27	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM28	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM29	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM30	;Store this byte into the node ID bitmap
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	NODEBM31	;Store this byte into the node ID bitmap
	;fall through

;entered with BSR = ?
AwaitCommand
	movlb	2
	btfsc	FLAGS,LR_FRM	;If there's been a change in incoming frame
	call	DealWithFrame	; regs, deal with the frame
	movf	UR_LEN,F	;If there's no data in the UART receiver queue,
	btfsc	STATUS,Z	; loop around again
	bra	AwaitCommand	; "
	decf	UR_LEN,F	;Decrement the UART receiver queue size
	movlw	B'00000100'	;If the pop off the queue dropped the length
	btfss	UR_LEN,6	; below 64, assert CTS so the host sends us
	iorwf	LATA,F		; data again
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	addlw	-1		;If it's one, that's the command byte for
	btfsc	STATUS,Z	; sending a frame, so go wait for the first
	bra	AwaitDest	; byte of that frame (the destination)
	addlw	-1		;If it's two, that's the command byte for
	btfsc	STATUS,Z	; setting the node ID bitmap, so go wait for
	bra	AwaitNodeId	; that data
	bra	AwaitCommand	;All other commands are considered no-ops

;entered with BSR = 2
AwaitDest
	btfsc	FLAGS,LR_FRM	;If there's been a change in incoming frame
	call	DealWithFrame	; regs, deal with the frame
	movf	UR_LEN,F	;If there's no data in the UART receiver queue,
	btfsc	STATUS,Z	; loop around again
	bra	AwaitDest	; "
	decf	UR_LEN,F	;Decrement the UART receiver queue size
	movlw	B'00000100'	;If the pop off the queue dropped the length
	btfss	UR_LEN,6	; below 64, assert CTS so the host sends us
	iorwf	LATA,F		; data again
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	UR_DEST		;Save this byte as the loaded frame destination
	;fall through

;entered with BSR = 2
AwaitSrc
	btfsc	FLAGS,LR_FRM	;If there's been a change in incoming frame
	call	DealWithFrame	; regs, deal with the frame
	movf	UR_LEN,F	;If there's no data in the UART receiver queue,
	btfsc	STATUS,Z	; loop around again
	bra	AwaitSrc	; "
	decf	UR_LEN,F	;Decrement the UART receiver queue size
	movlw	B'00000100'	;If the pop off the queue dropped the length
	btfss	UR_LEN,6	; below 64, assert CTS so the host sends us
	iorwf	LATA,F		; data again
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	UR_SRC		;Save this byte as the loaded frame source
	;fall through

;entered with BSR = 2
AwaitType
	btfsc	FLAGS,LR_FRM	;If there's been a change in incoming frame
	call	DealWithFrame	; regs, deal with the frame
	movf	UR_LEN,F	;If there's no data in the UART receiver queue,
	btfsc	STATUS,Z	; loop around again
	bra	AwaitType	; "
	decf	UR_LEN,F	;Decrement the UART receiver queue size
	movlw	B'00000100'	;If the pop off the queue dropped the length
	btfss	UR_LEN,6	; below 64, assert CTS so the host sends us
	iorwf	LATA,F		; data again
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	UR_TYPE		;Save this byte as the loaded frame type
	;fall through

;entered with BSR = 2
Await4th
	btfsc	FLAGS,LR_FRM	;If there's been a change in incoming frame
	call	DealWithFrame	; regs, deal with the frame
	movf	UR_LEN,F	;If there's no data in the UART receiver queue,
	btfsc	STATUS,Z	; loop around again
	bra	Await4th	; "
	decf	UR_LEN,F	;Decrement the UART receiver queue size
	movlw	B'00000100'	;If the pop off the queue dropped the length
	btfss	UR_LEN,6	; below 64, assert CTS so the host sends us
	iorwf	LATA,F		; data again
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	UR_4TH		;Save this byte as the loaded frame 4th byte
	;fall through

;entered with BSR = 2
Await5th
	btfsc	FLAGS,LR_FRM	;If there's been a change in incoming frame
	call	DealWithFrame	; regs, deal with the frame
	movf	UR_LEN,F	;If there's no data in the UART receiver queue,
	btfsc	STATUS,Z	; loop around again
	bra	Await5th	; "
	decf	UR_LEN,F	;Decrement the UART receiver queue size
	movlw	B'00000100'	;If the pop off the queue dropped the length
	btfss	UR_LEN,6	; below 64, assert CTS so the host sends us
	iorwf	LATA,F		; data again
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movwf	UR_5TH		;Save this byte as the loaded frame 5th byte
	;fall through

;entered with BSR = 2
SetUpBackoffs
	movlw	-3		;If there have been more than two collisions
	btfsc	COL_HIST,7	; during the last eight transmissions, left-
	addlw	1		; shift a one (up to a maximum of four) into
	btfsc	COL_HIST,6	; the global backoff mask and clear the
	addlw	1		; collision history
	btfsc	COL_HIST,5	; "
	addlw	1		; "
	btfsc	COL_HIST,4	; "
	addlw	1		; "
	btfsc	COL_HIST,3	; "
	addlw	1		; "
	btfsc	COL_HIST,2	; "
	addlw	1		; "
	btfsc	COL_HIST,1	; "
	addlw	1		; "
	btfsc	COL_HIST,0	; "
	addlw	1		; "
	bsf	STATUS,C	; "
	btfss	WREG,7		; "
	rlf	GBACKOFF,F	; "
	bcf	GBACKOFF,4	; "
	btfss	WREG,7		; "
	clrf	COL_HIST	; "
	movlw	-2		;If there have been fewer than two deferrals
	btfsc	DEF_HIST,7	; during the last eight transmissions, right-
	addlw	1		; shift the global backoff mask and fill the
	btfsc	DEF_HIST,6	; deferral history
	addlw	1		; "
	btfsc	DEF_HIST,5	; "
	addlw	1		; "
	btfsc	DEF_HIST,4	; "
	addlw	1		; "
	btfsc	DEF_HIST,3	; "
	addlw	1		; "
	btfsc	DEF_HIST,2	; "
	addlw	1		; "
	btfsc	DEF_HIST,1	; "
	addlw	1		; "
	btfsc	DEF_HIST,0	; "
	addlw	1		; "
	btfsc	WREG,7		; "
	lsrf	GBACKOFF,F	; "
	btfsc	WREG,7		; "
	clrf	DEF_HIST	; "
	btfsc	WREG,7		; "
	decf	DEF_HIST	; "
	lslf	COL_HIST,F	;Progress the collision and deferral history
	lslf	DEF_HIST,F	; logs to make space for what happens this time
	movf	GBACKOFF,W	;Copy the global backoff mask into the local
	movwf	LBACKOFF	; backoff mask
	movlw	33		;Set attempts counter to 32 (plus one since we
	movwf	ATTEMPTS	; immediately decrement it)
	;fall through

;entered with BSR = ?
SetTimer2
	decf	ATTEMPTS,F	;Decrement the attempts counter; if it's hit
	btfsc	STATUS,Z	; zero, give up trying to transmit this frame
	bra	GiveUp		; and if necessary drain the frame payload
	movlb	0		;XORing together the bytes of Timer1 is a cheap
	movf	TMR1L,W		; but reasonable way to get a 'random' number;
	xorwf	TMR1H,W		; AND it with the local backoff mask to get our
	andwf	LBACKOFF,W	; backoff value in multiples of 100us
	addlw	4		;Add 400us to this for minimum interdialog gap
	movwf	PR2		;Multiply the value in WREG by 5, because
	lslf	WREG,W		; Timer2 will interrupt after 20us times the
	lslf	WREG,W		; value in PR2
	addwf	PR2,F		; "
	decf	PR2,F		; "
	bsf	T2CON,TMR2ON	;Activate Timer2
	clrf	TMR2		;Reset Timer2
	bcf	PIR1,TMR2IF	;Clear the Timer2 interrupt flag
	;fall through

;entered with BSR = 0
WaitToSendCtrl
	bsf	INTCON,GIE	;Reenable interrupts in case they were disabled
	btfsc	T2CON,TMR2ON	;If Timer2 has been stopped, it means a frame
	bra	WTSC1		; came in while we were waiting; handle it and
	call	DealWithFrame	; defer and retry
	bra	Deferred	; "
WTSC1	bcf	INTCON,GIE	;Disable interrupts before checking Timer2
	btfss	PIR1,TMR2IF	;Unless Timer2 has interrupted, loop around
	bra	WaitToSendCtrl	; again
	btfss	UR_TYPE,7	;If the loaded frame is a control frame, just
	bra	WTSC2		; go ahead and send it and then await the next
	call	SendControlFrame; command byte
	bra	AwaitCommand	; "
WTSC2	call	SendRts		;If it's a data frame, send an RTS frame first
	movlb	0		;Set Timer2 to interrupt after 200us, the
	movlw	9		; maximum interframe gap
	movwf	PR2		; "
	bsf	T2CON,TMR2ON	;Activate Timer2
	clrf	TMR2		;Reset Timer2
	bcf	PIR1,TMR2IF	;Clear the Timer2 interrupt flag
	incf	UR_DEST,W	;If frame's destination is broadcast (0xFF)
	btfsc	STATUS,Z	; then we want to wait for 200us of silence
	bra	WaitForSilence	; before sending; otherwise we want to wait for
	bra	WaitForCts	; a CTS frame and 200us is our timeout

;entered with BSR = ?
GiveUp
	btfsc	UR_TYPE,7	;If loaded frame is a control frame, we already
	bra	AwaitCommand	; have it in full
	movlb	2		;If loaded frame is a data frame, we need to
	movf	UR_4TH,W	; extract the length from the 4th and 5th bytes
	andlw	B'00000011'	; and drain that many characters from the UART
	movwf	LT_LENH		; receiver queue (even if it doesn't already
	movf	UR_5TH,W	; contain all of them)
	movwf	LT_LENL		; "
GiveUp1	movlw	-1		;Decrement the length counter; if it was
	addwf	LT_LENL,F	; already zero, we're done
	addwfc	LT_LENH,F	; "
	btfss	STATUS,C	; "
	bra	AwaitCommand	; "
GiveUp2	btfsc	FLAGS,LR_FRM	;If there's been a change in incoming frame
	call	DealWithFrame	; regs, deal with the frame
	movf	UR_LEN,F	;If there's no data in the UART receiver queue,
	btfsc	STATUS,Z	; loop around again
	bra	GiveUp2		; "
	decf	UR_LEN,F	;Decrement the UART receiver queue size
	moviw	FSR1++		;Pop the next byte off the UART receiver queue
	bcf	FSR1L,7		; "
	movlw	B'00000100'	;If the pop off the queue dropped the length
	btfss	UR_LEN,6	; below 64, assert CTS so the host sends us
	iorwf	LATA,F		; data again
	bra	GiveUp1		;Go back to decrement counter again

;entered with BSR = 2
Deferred
	bsf	DEF_HIST,0	;We had to defer, so note that in deferral log
	bsf	LBACKOFF,0	;Expand local backoff mask if it's zero
	bra	SetTimer2	;And retry

;entered with BSR = ?
Collided
	bsf	COL_HIST,0	;We collided, so note that in collision log
	lslf	LBACKOFF,F	;Expand local backoff mask by one bit, up to a
	bsf	LBACKOFF,0	; maximum of 0b1111
	bcf	LBACKOFF,4	; "
	bra	SetTimer2	;And retry

;entered with BSR = 0
WaitForSilence
	bsf	INTCON,GIE	;Reenable interrupts in case they were disabled
	btfsc	T2CON,TMR2ON	;If Timer2 has been stopped, it means a frame
	bra	WFS1		; came in while we were waiting; handle it and
	call	DealWithFrame	; consider this a collision and retry
	bra	Collided	; "
WFS1	bcf	INTCON,GIE	;Disable interrupts before checking Timer2
	btfss	PIR1,TMR2IF	;Unless Timer2 has interrupted, loop around
	bra	WaitForSilence	; again
	call	SendDataFrame	;If Timer2 has interrupted, send our broadcast
	bra	AwaitCommand	; frame and return to await next command

;entered with BSR = 0
WaitForCts
	bsf	FLAGS,CTS_PLS	;Raise the flag that we want a CTS frame
	btfsc	T2CON,TMR2ON	;If Timer2 has been stopped, it means a frame
	bra	WFC1		; came in while we were waiting; handle it
	call	DealWithFrame	; "
	btfss	FLAGS,CTS_PLS	;If the flag has been cleared, data's been sent
	bra	AwaitCommand	; and we can return to await a new command;
	bcf	FLAGS,CTS_PLS	; otherwise consider it a collision and retry
	bra	Collided	; "
WFC1	btfss	PIR1,TMR2IF	;Unless Timer2 has interrupted, loop around
	bra	WaitForCts	; again
	bcf	FLAGS,CTS_PLS	;Our RTS must have collided, send it again
	bra	Collided	; "


;;; LocalTalk Transmitter Subprograms ;;;

DealWithFrame
	movlb	2
	btfss	FLAGS,LR_FRM	;If the incoming frame regs haven't changed,
	return			; we have nothing to do here
	bcf	FLAGS,LR_FRM	;This is so we know if regs changed under us
	btfss	LR_TYPE,7	;If the frame is not a control frame, there's
	return			; nothing to do; host handles data frames
	movf	FSR1L,W		;Using the node ID bitmap, check whether this
	movwf	FSR1L_TM	; packet's destination is one we represent,
	movf	LR_DEST,W	; and if it's not, we don't respond to it
	movwf	FSR1L		; "
	movlw	B'00000001'	; "
	btfsc	FSR1L,1		; "
	movlw	B'00000100'	; "
	btfsc	FSR1L,0		; "
	lslf	WREG,W		; "
	btfsc	FSR1L,2		; "
	swapf	WREG,W		; "
	lsrf	FSR1L,F		; "
	lsrf	FSR1L,F		; "
	lsrf	FSR1L,F		; "
	bsf	FSR1L,7		; "
	andwf	INDF1,W		; "
	movwf	INDF1_TM	; "
	movf	FSR1L_TM,W	; "
	movwf	FSR1L		; "
	movf	INDF1_TM,F	; "
	btfsc	STATUS,Z	; "
	return			; "
	clrf	LR_CCRC1	;Clear the CRC check registers to all ones so
	clrf	LR_CCRC2	; we can start checking the CRC of the frame
	decf	LR_CCRC1,F	; "
	decf	LR_CCRC2,F	; "
	movf	LR_DEST,W	;Update the CRC check registers with the frame
	xorwf	LR_CCRC1,W	; destination
	movwf	LR_CCRC1	; "
	movlp	high CrcLut1	; "
	callw			; "
	xorwf	LR_CCRC2,W	; "
	xorwf	LR_CCRC1,F	; "
	xorwf	LR_CCRC1,W	; "
	xorwf	LR_CCRC1,F	; "
	movlp	high CrcLut2	; "
	callw			; "
	movwf	LR_CCRC2	; "
	movf	LR_SRC,W	;Update the CRC check registers with the frame
	xorwf	LR_CCRC1,W	; source
	movwf	LR_CCRC1	; "
	movlp	high CrcLut1	; "
	callw			; "
	xorwf	LR_CCRC2,W	; "
	xorwf	LR_CCRC1,F	; "
	xorwf	LR_CCRC1,W	; "
	xorwf	LR_CCRC1,F	; "
	movlp	high CrcLut2	; "
	callw			; "
	movwf	LR_CCRC2	; "
	movf	LR_TYPE,W	;Update the CRC check registers with the frame
	xorwf	LR_CCRC1,W	; type
	movwf	LR_CCRC1	; "
	movlp	high CrcLut1	; "
	callw			; "
	xorwf	LR_CCRC2,W	; "
	xorwf	LR_CCRC1,F	; "
	xorwf	LR_CCRC1,W	; "
	xorwf	LR_CCRC1,F	; "
	movlp	high CrcLut2	; "
	callw			; "
	movwf	LR_CCRC2	; "
	movlp	0		; "
	comf	LR_CCRC1,W	;If the calculated CRC does not match the
	xorwf	LR_CRC1,W	; received one, the frame is invalid and we're
	btfss	STATUS,Z	; done
	return			; "
	comf	LR_CCRC2,W	; "
	xorwf	LR_CRC2,W	; "
	btfss	STATUS,Z	; "
	return			; "
	bcf	INTCON,GIE	;Disable interrupts so frame vars don't change
	btfsc	FLAGS,LR_FRM	;If this flag is set, regs changed under us so
	bra	DealWiR		; we can't act, reenable interrupts and return
	movf	LR_TYPE,W	;If the frame type is 0x81 (ENQ), jump into
	addlw	-0x81		; sending an ACK frame and return from there
	btfsc	STATUS,Z	; "
	bra	SendAck		; "
	addlw	-3		;If the frame type is 0x84 (RTS), jump into
	btfsc	STATUS,Z	; sending a CTS frame and return from there
	bra	SendCts		; "
	addlw	-1		;If the frame type is anything but 0x85 (CTS),
	btfss	STATUS,Z	; reenable interrupts and return, the frame is
	bra	DealWiR		; not something we know how to act on
	btfss	FLAGS,CTS_PLS	;If we weren't expecting a CTS, reenable
	bra	DealWiR		; interrupts and return
	movf	LR_SRC,W	;If the source of the incoming frame doesn't
	xorwf	UR_DEST,W	; match the destination of the loaded outgoing
	btfss	STATUS,Z	; frame, this frame is not for the sender,
	bra	DealWiR		; so reenable interrupts and return
	movf	LR_DEST,W	;If the destination of the incoming frame
	xorwf	UR_SRC,W	; doesn't match the source of the loaded
	btfss	STATUS,Z	; outgoing frame, this frame is not for the
	bra	DealWiR		; sender, so reenable interrupts and return
	bcf	FLAGS,CTS_PLS	;Clear the flag so caller knows we sent frame
	bra	SendDataFrame	;Jump into sending frame and return from there
DealWiR	bsf	INTCON,GIE	;Reenable interrupts
	return

SendControlFrame
	movlb	2
	bcf	INTCON,GIE	;Disable interrupts
	call	SendPreamble	;XX-25
	movf	UR_DEST,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_SRC,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_TYPE,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_4TH,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_5TH,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	DNOP			;26-27
	DNOP			;28-29
	DNOP			;30-31
	call	SendPostamble	;32-XX
	movlb	7		;Clear the inevitable IOC interrupt that came
	bcf	IOCAF,IOCAF3	; in while (and because) we were transmitting
	movlb	2		; before reenabling interrupts
	bsf	INTCON,GIE	;Reenable interrupts
	return

SendDataFrame
	movlb	2
	bcf	INTCON,GIE	;Disable interrupts
	call	SendPreamble	;XX-25
	movf	UR_DEST,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_SRC,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_TYPE,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_4TH,W	;26
	movwf	LT_BUF		;27
	andlw	B'00000011'	;28
	movwf	LT_LENH		;29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_5TH,W	;26
	movwf	LT_BUF		;27
	movwf	LT_LENL		;28
	nop			;29
	DNOP			;30-31
	call	SendByte	;32-25
	DNOP			;26-27
	DNOP			;28-29
	DNOP			;30-31
	call	SendFromQueue	;32-25 (Branches into SendPostamble)
	movlb	7		;Clear the inevitable IOC interrupt that came
	bcf	IOCAF,IOCAF3	; in while (and because) we were transmitting
	movlb	2		; before reenabling interrupts
	bsf	INTCON,GIE	;Reenable interrupts
	return

SendRts
	movlb	2
	bcf	INTCON,GIE	;Disable interrupts
	call	SendPreamble	;XX-25
	movf	UR_DEST,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	UR_SRC,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movlw	0x84		;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	comf	LT_CRC1,W	;26
	movwf	LT_BUF		;27
	comf	LT_CRC2,W	;28
	movwf	LT_BUF2		;29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	LT_BUF2,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	DNOP			;26-27
	DNOP			;28-29
	DNOP			;30-31
	call	SendPostamble	;32-XX
	movlb	7		;Clear the inevitable IOC interrupt that came
	bcf	IOCAF,IOCAF3	; in while (and because) we were transmitting
	movlb	2		; before reenabling interrupts
	bsf	INTCON,GIE	;Reenable interrupts
	return

SendAck
	movlb	2
	bcf	INTCON,GIE	;Disable interrupts
	call	SendPreamble	;XX-25
	movf	LR_DEST,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	LR_DEST,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movlw	0x82		;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	comf	LT_CRC1,W	;26
	movwf	LT_BUF		;27
	comf	LT_CRC2,W	;28
	movwf	LT_BUF2		;29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	LT_BUF2,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	DNOP			;26-27
	DNOP			;28-29
	DNOP			;30-31
	call	SendPostamble	;32-XX
	movlb	7		;Clear the inevitable IOC interrupt that came
	bcf	IOCAF,IOCAF3	; in while (and because) we were transmitting
	movlb	2		; before reenabling interrupts
	bsf	INTCON,GIE	;Reenable interrupts
	return

SendCts
	movlb	2
	bcf	INTCON,GIE	;Disable interrupts
	call	SendPreamble	;XX-25
	movf	LR_SRC,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	LR_DEST,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	movlw	0x85		;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	comf	LT_CRC1,W	;26
	movwf	LT_BUF		;27
	comf	LT_CRC2,W	;28
	movwf	LT_BUF2		;29
	DNOP			;30-31
	call	SendByte	;32-25
	movf	LT_BUF2,W	;26
	movwf	LT_BUF		;27
	DNOP			;28-29
	DNOP			;30-31
	call	SendByte	;32-25
	DNOP			;26-27
	DNOP			;28-29
	DNOP			;30-31
	call	SendPostamble	;32-XX
	movlb	7		;Clear the inevitable IOC interrupt that came
	bcf	IOCAF,IOCAF3	; in while (and because) we were transmitting
	movlb	2		; before reenabling interrupts
	bsf	INTCON,GIE	;Reenable interrupts
	return

;All subs below assume that they're being called with BSR in bank 2

SendPreamble
	movlw	B'01111110'	;Load the flag byte into the buffer for later
	movwf	LT_BUF		; "
	movlw	4		;Set the counter to send four pre-flag ones
	movwf	LT_ONES		; "
	movlw	B'00001111'	;Make sure driver is turned off and LocalTalk
	andwf	LATA,F		; pin is ready to output 0 when it's turned on
	movlw	B'00010000'	;Load pattern to turn on driver
SendPr1	nop			;33
	nop			;34
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	clrf	LT_CRC1		;15 Reset the running CRC registers to all
	decf	LT_CRC1,F	;16  ones; we're going to end up doing this
	clrf	LT_CRC2		;17  repeatedly, but it doesn't matter
	decf	LT_CRC2,F	;18  "
	DELAY	3		;19-27
	nop			;28
	movlw	B'00100000'	;29 Load pattern for inverting LocalTalk pin
	decfsz	LT_ONES,F	;30 Decrement pre-ones counter and loop if it
	bra	SendPr1		;31(-32)  is not yet zero
	movlw	24		;32 Rotate buffer 24 times and we'll send three
	movwf	LT_ONES		;33  flag bytes
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
SendPr2	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,0	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lsrf	LT_BUF,F	;18 Rotate the buffer
	btfsc	STATUS,C	;19  "
	bsf	LT_BUF,7	;20  "
	decfsz	LT_ONES,F	;21 Decrement the loop counter; if it's not 0
	bra	SendPr3		;22(-23)  yet, skip the next two lines
	incf	LT_ONES,F	;23 Reset ones counter to be ready for SendByte
	return			;24-25
SendPr3	DELAY	2		;24-29
	DNOP			;30-31
	movlw	B'00100000'	;32 Load pattern for inverting LocalTalk pin
	bra	SendPr2		;33-34

SendByte
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,0	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	LT_BUF,0	;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	movf	LT_BUF,W	;24 Update LT_CRC1 with the byte being sent;
	xorwf	LT_CRC1,W	;25  note that we don't movlp back to 0 because
	movwf	LT_CRCX		;26  we don't have time; this only works
	movlp	high CrcLut1	;27  because CrcLut1 is below the page boundary
	callw			;28-31  "
	xorwf	LT_CRC2,W	;32  "
	movwf	LT_CRC1		;33  "
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,1	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	LT_BUF,1	;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	movf	LT_CRCX,W	;24 Update LT_CRC2 with the byte being sent
	movlp	high CrcLut2	;25  "
	callw			;26-29  "
	movlp	0		;30  "
	movwf	LT_CRC2		;31  "
	DNOP			;32-33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,2	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	LT_BUF,2	;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,3	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	LT_BUF,3	;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,4	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	LT_BUF,4	;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,5	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	LT_BUF,5	;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,6	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	LT_BUF,6	;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,7	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	LT_BUF,7	;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	return			;24-25

SendFromQueue
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,0		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,0		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	movlw	-1		;24 Decrement the send length by one to start
	addwf	LT_LENL,F	;25  with because this decrement method only
	addwfc	LT_LENH,F	;26  signals if the register is already zero
	DELAY	2		;27-32
	nop			;33
SendFrL	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,1		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,1		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,2		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,2		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,3		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,3		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,4		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,4		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,5		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,5		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,6		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,6		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	DELAY	3		;24-32
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,7		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,7		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	moviw	FSR1++		;24 Advance the receiver queue pop pointer
	bcf	FSR1L,7		;25  "
	decf	UR_LEN,F	;26 Decrement the receiver queue length
	nop			;27
	movlw	-1		;28 Decrement the send length; if it was
	addwf	LT_LENL,F	;29  already zero, this was our last byte and
	addwfc	LT_LENH,F	;30  carry will not be set
	btfss	STATUS,C	;31 If this was our last byte, jump into
	bra	SendPostamble	;32(-33)  sending the postamble
	nop			;33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	INDF1,0		;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lslf	LT_ONES,F	;18 Increment the consecutive ones counter if
	btfss	INDF1,0		;19  this bit is a one, otherwise reset it
	clrf	LT_ONES		;20  "
	bsf	LT_ONES,0	;21  "
	btfsc	LT_ONES,5	;22 If the consecutive ones counter has reached
	call	SendByteStuff	;23(-24)  five, stuff a zero
	movlw	B'00000100'	;24 If the pop off the queue dropped the length
	btfss	UR_LEN,6	;25  below 64, assert CTS so the host sends us
	iorwf	LATA,F		;26  data again
	DNOP			;27-28
	DNOP			;29-30
	nop			;31
	bra	SendFrL		;32-33

SendPostamble
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	nop			;15
	movlw	B'00100000'	;16 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;17 Invert LocalTalk pin to signal a zero
	movlw	B'10111111'	;18 Load the rest of the flag byte into buffer
	movwf	LT_BUF		;19  "
	movlw	20		;20 Shift this buffer 20 times (shifting in
	movwf	LT_ONES		;21  ones) and we'll send a flag and 13 ones
	DELAY	4		;22-33
SendPoL	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	movlw	B'00100000'	;15 Load pattern for inverting LocalTalk pin
	btfss	LT_BUF,0	;16 Unless this bit is a one...
	xorwf	LATA,F		;17 ...Invert LocalTalk pin to signal a zero
	lsrf	LT_BUF,F	;18 Rotate a 1 into the buffer
	bsf	LT_BUF,7	;19  "
	DELAY	3		;20-28
	nop			;29
	movlw	B'00011111'	;30 Load pattern for driving 0 on the bus
	decfsz	LT_ONES,F	;31 Decrement the loop counter; if it's not 0
	bra	SendPoL		;32(-33)  yet, loop to send the next bit
	btfss	LATA,5		;33 If bus is already being driven to 0, load
	movlw	B'00001111'	;34  pattern for turning off the driver
	andwf	LATA,F		;00 Drive the bus to 0 or turn off the driver
	btfss	LATA,4		;01 If driver is already off, we're done
	return			;02  "
	call	SendDoUartSvc	;03-04 (05-16) Service the UART receiver
	DELAY	5		;17-31
	DNOP			;32-33
	movlw	B'00001111'	;34 Load pattern for turning off driver
	andwf	LATA,F		;00 Turn off driver, end transmission
	return

SendByteStuff
	DELAY	3		;25-33
	movlw	B'00100000'	;34 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;00 Invert LocalTalk pin for clock
	call	SendDoUartSvc	;01-02 (03-14) Service the UART receiver
	nop			;15
	movlw	B'00100000'	;16 Load pattern for inverting LocalTalk pin
	xorwf	LATA,F		;17 Invert LocalTalk pin for our stuffed zero
	clrf	LT_ONES		;18 Reset the ones counter
	bsf	LT_ONES,0	;19  "
	nop			;20
	nop			;21
	return			;22-23

SendDoUartSvc
	movlb	0		;03 Check if there is a byte waiting from the
	btfss	PIR1,RCIF	;04  UART; if there isn't, check if we need to
	bra	SendDoUartCts	;05(-06)  set CTS instead
	movlb	3		;06 Get the received byte
	movf	RCREG,W		;07  "
	movlb	2		;08  "
	movwi	FSR0++		;09 Push it onto the queue
	bcf	FSR0L,7		;10 Wrap the queue around
	incf	UR_LEN,F	;11 Increment the queue length
	nop			;12
	return			;13-14
SendDoUartCts
	movlb	2		;07 If the UART receiver queue length >= 64,
	movlw	B'11111011'	;08  deassert CTS so the host stops sending us
	btfsc	UR_LEN,6	;09  data
	andwf	LATA,F		;10  "
	DNOP			;11-12
	return			;13-14


;;; CRC Lookup Tables ;;;

	org	0x700

CrcLut1
	retlw	0x00
	retlw	0x89
	retlw	0x12
	retlw	0x9B
	retlw	0x24
	retlw	0xAD
	retlw	0x36
	retlw	0xBF
	retlw	0x48
	retlw	0xC1
	retlw	0x5A
	retlw	0xD3
	retlw	0x6C
	retlw	0xE5
	retlw	0x7E
	retlw	0xF7
	retlw	0x81
	retlw	0x08
	retlw	0x93
	retlw	0x1A
	retlw	0xA5
	retlw	0x2C
	retlw	0xB7
	retlw	0x3E
	retlw	0xC9
	retlw	0x40
	retlw	0xDB
	retlw	0x52
	retlw	0xED
	retlw	0x64
	retlw	0xFF
	retlw	0x76
	retlw	0x02
	retlw	0x8B
	retlw	0x10
	retlw	0x99
	retlw	0x26
	retlw	0xAF
	retlw	0x34
	retlw	0xBD
	retlw	0x4A
	retlw	0xC3
	retlw	0x58
	retlw	0xD1
	retlw	0x6E
	retlw	0xE7
	retlw	0x7C
	retlw	0xF5
	retlw	0x83
	retlw	0x0A
	retlw	0x91
	retlw	0x18
	retlw	0xA7
	retlw	0x2E
	retlw	0xB5
	retlw	0x3C
	retlw	0xCB
	retlw	0x42
	retlw	0xD9
	retlw	0x50
	retlw	0xEF
	retlw	0x66
	retlw	0xFD
	retlw	0x74
	retlw	0x04
	retlw	0x8D
	retlw	0x16
	retlw	0x9F
	retlw	0x20
	retlw	0xA9
	retlw	0x32
	retlw	0xBB
	retlw	0x4C
	retlw	0xC5
	retlw	0x5E
	retlw	0xD7
	retlw	0x68
	retlw	0xE1
	retlw	0x7A
	retlw	0xF3
	retlw	0x85
	retlw	0x0C
	retlw	0x97
	retlw	0x1E
	retlw	0xA1
	retlw	0x28
	retlw	0xB3
	retlw	0x3A
	retlw	0xCD
	retlw	0x44
	retlw	0xDF
	retlw	0x56
	retlw	0xE9
	retlw	0x60
	retlw	0xFB
	retlw	0x72
	retlw	0x06
	retlw	0x8F
	retlw	0x14
	retlw	0x9D
	retlw	0x22
	retlw	0xAB
	retlw	0x30
	retlw	0xB9
	retlw	0x4E
	retlw	0xC7
	retlw	0x5C
	retlw	0xD5
	retlw	0x6A
	retlw	0xE3
	retlw	0x78
	retlw	0xF1
	retlw	0x87
	retlw	0x0E
	retlw	0x95
	retlw	0x1C
	retlw	0xA3
	retlw	0x2A
	retlw	0xB1
	retlw	0x38
	retlw	0xCF
	retlw	0x46
	retlw	0xDD
	retlw	0x54
	retlw	0xEB
	retlw	0x62
	retlw	0xF9
	retlw	0x70
	retlw	0x08
	retlw	0x81
	retlw	0x1A
	retlw	0x93
	retlw	0x2C
	retlw	0xA5
	retlw	0x3E
	retlw	0xB7
	retlw	0x40
	retlw	0xC9
	retlw	0x52
	retlw	0xDB
	retlw	0x64
	retlw	0xED
	retlw	0x76
	retlw	0xFF
	retlw	0x89
	retlw	0x00
	retlw	0x9B
	retlw	0x12
	retlw	0xAD
	retlw	0x24
	retlw	0xBF
	retlw	0x36
	retlw	0xC1
	retlw	0x48
	retlw	0xD3
	retlw	0x5A
	retlw	0xE5
	retlw	0x6C
	retlw	0xF7
	retlw	0x7E
	retlw	0x0A
	retlw	0x83
	retlw	0x18
	retlw	0x91
	retlw	0x2E
	retlw	0xA7
	retlw	0x3C
	retlw	0xB5
	retlw	0x42
	retlw	0xCB
	retlw	0x50
	retlw	0xD9
	retlw	0x66
	retlw	0xEF
	retlw	0x74
	retlw	0xFD
	retlw	0x8B
	retlw	0x02
	retlw	0x99
	retlw	0x10
	retlw	0xAF
	retlw	0x26
	retlw	0xBD
	retlw	0x34
	retlw	0xC3
	retlw	0x4A
	retlw	0xD1
	retlw	0x58
	retlw	0xE7
	retlw	0x6E
	retlw	0xF5
	retlw	0x7C
	retlw	0x0C
	retlw	0x85
	retlw	0x1E
	retlw	0x97
	retlw	0x28
	retlw	0xA1
	retlw	0x3A
	retlw	0xB3
	retlw	0x44
	retlw	0xCD
	retlw	0x56
	retlw	0xDF
	retlw	0x60
	retlw	0xE9
	retlw	0x72
	retlw	0xFB
	retlw	0x8D
	retlw	0x04
	retlw	0x9F
	retlw	0x16
	retlw	0xA9
	retlw	0x20
	retlw	0xBB
	retlw	0x32
	retlw	0xC5
	retlw	0x4C
	retlw	0xD7
	retlw	0x5E
	retlw	0xE1
	retlw	0x68
	retlw	0xF3
	retlw	0x7A
	retlw	0x0E
	retlw	0x87
	retlw	0x1C
	retlw	0x95
	retlw	0x2A
	retlw	0xA3
	retlw	0x38
	retlw	0xB1
	retlw	0x46
	retlw	0xCF
	retlw	0x54
	retlw	0xDD
	retlw	0x62
	retlw	0xEB
	retlw	0x70
	retlw	0xF9
	retlw	0x8F
	retlw	0x06
	retlw	0x9D
	retlw	0x14
	retlw	0xAB
	retlw	0x22
	retlw	0xB9
	retlw	0x30
	retlw	0xC7
	retlw	0x4E
	retlw	0xD5
	retlw	0x5C
	retlw	0xE3
	retlw	0x6A
	retlw	0xF1
	retlw	0x78

	org	0x800

CrcLut2
	retlw	0x00
	retlw	0x11
	retlw	0x23
	retlw	0x32
	retlw	0x46
	retlw	0x57
	retlw	0x65
	retlw	0x74
	retlw	0x8C
	retlw	0x9D
	retlw	0xAF
	retlw	0xBE
	retlw	0xCA
	retlw	0xDB
	retlw	0xE9
	retlw	0xF8
	retlw	0x10
	retlw	0x01
	retlw	0x33
	retlw	0x22
	retlw	0x56
	retlw	0x47
	retlw	0x75
	retlw	0x64
	retlw	0x9C
	retlw	0x8D
	retlw	0xBF
	retlw	0xAE
	retlw	0xDA
	retlw	0xCB
	retlw	0xF9
	retlw	0xE8
	retlw	0x21
	retlw	0x30
	retlw	0x02
	retlw	0x13
	retlw	0x67
	retlw	0x76
	retlw	0x44
	retlw	0x55
	retlw	0xAD
	retlw	0xBC
	retlw	0x8E
	retlw	0x9F
	retlw	0xEB
	retlw	0xFA
	retlw	0xC8
	retlw	0xD9
	retlw	0x31
	retlw	0x20
	retlw	0x12
	retlw	0x03
	retlw	0x77
	retlw	0x66
	retlw	0x54
	retlw	0x45
	retlw	0xBD
	retlw	0xAC
	retlw	0x9E
	retlw	0x8F
	retlw	0xFB
	retlw	0xEA
	retlw	0xD8
	retlw	0xC9
	retlw	0x42
	retlw	0x53
	retlw	0x61
	retlw	0x70
	retlw	0x04
	retlw	0x15
	retlw	0x27
	retlw	0x36
	retlw	0xCE
	retlw	0xDF
	retlw	0xED
	retlw	0xFC
	retlw	0x88
	retlw	0x99
	retlw	0xAB
	retlw	0xBA
	retlw	0x52
	retlw	0x43
	retlw	0x71
	retlw	0x60
	retlw	0x14
	retlw	0x05
	retlw	0x37
	retlw	0x26
	retlw	0xDE
	retlw	0xCF
	retlw	0xFD
	retlw	0xEC
	retlw	0x98
	retlw	0x89
	retlw	0xBB
	retlw	0xAA
	retlw	0x63
	retlw	0x72
	retlw	0x40
	retlw	0x51
	retlw	0x25
	retlw	0x34
	retlw	0x06
	retlw	0x17
	retlw	0xEF
	retlw	0xFE
	retlw	0xCC
	retlw	0xDD
	retlw	0xA9
	retlw	0xB8
	retlw	0x8A
	retlw	0x9B
	retlw	0x73
	retlw	0x62
	retlw	0x50
	retlw	0x41
	retlw	0x35
	retlw	0x24
	retlw	0x16
	retlw	0x07
	retlw	0xFF
	retlw	0xEE
	retlw	0xDC
	retlw	0xCD
	retlw	0xB9
	retlw	0xA8
	retlw	0x9A
	retlw	0x8B
	retlw	0x84
	retlw	0x95
	retlw	0xA7
	retlw	0xB6
	retlw	0xC2
	retlw	0xD3
	retlw	0xE1
	retlw	0xF0
	retlw	0x08
	retlw	0x19
	retlw	0x2B
	retlw	0x3A
	retlw	0x4E
	retlw	0x5F
	retlw	0x6D
	retlw	0x7C
	retlw	0x94
	retlw	0x85
	retlw	0xB7
	retlw	0xA6
	retlw	0xD2
	retlw	0xC3
	retlw	0xF1
	retlw	0xE0
	retlw	0x18
	retlw	0x09
	retlw	0x3B
	retlw	0x2A
	retlw	0x5E
	retlw	0x4F
	retlw	0x7D
	retlw	0x6C
	retlw	0xA5
	retlw	0xB4
	retlw	0x86
	retlw	0x97
	retlw	0xE3
	retlw	0xF2
	retlw	0xC0
	retlw	0xD1
	retlw	0x29
	retlw	0x38
	retlw	0x0A
	retlw	0x1B
	retlw	0x6F
	retlw	0x7E
	retlw	0x4C
	retlw	0x5D
	retlw	0xB5
	retlw	0xA4
	retlw	0x96
	retlw	0x87
	retlw	0xF3
	retlw	0xE2
	retlw	0xD0
	retlw	0xC1
	retlw	0x39
	retlw	0x28
	retlw	0x1A
	retlw	0x0B
	retlw	0x7F
	retlw	0x6E
	retlw	0x5C
	retlw	0x4D
	retlw	0xC6
	retlw	0xD7
	retlw	0xE5
	retlw	0xF4
	retlw	0x80
	retlw	0x91
	retlw	0xA3
	retlw	0xB2
	retlw	0x4A
	retlw	0x5B
	retlw	0x69
	retlw	0x78
	retlw	0x0C
	retlw	0x1D
	retlw	0x2F
	retlw	0x3E
	retlw	0xD6
	retlw	0xC7
	retlw	0xF5
	retlw	0xE4
	retlw	0x90
	retlw	0x81
	retlw	0xB3
	retlw	0xA2
	retlw	0x5A
	retlw	0x4B
	retlw	0x79
	retlw	0x68
	retlw	0x1C
	retlw	0x0D
	retlw	0x3F
	retlw	0x2E
	retlw	0xE7
	retlw	0xF6
	retlw	0xC4
	retlw	0xD5
	retlw	0xA1
	retlw	0xB0
	retlw	0x82
	retlw	0x93
	retlw	0x6B
	retlw	0x7A
	retlw	0x48
	retlw	0x59
	retlw	0x2D
	retlw	0x3C
	retlw	0x0E
	retlw	0x1F
	retlw	0xF7
	retlw	0xE6
	retlw	0xD4
	retlw	0xC5
	retlw	0xB1
	retlw	0xA0
	retlw	0x92
	retlw	0x83
	retlw	0x7B
	retlw	0x6A
	retlw	0x58
	retlw	0x49
	retlw	0x3D
	retlw	0x2C
	retlw	0x1E
	retlw	0x0F


;;; LocalTalk Receiver Code ;;;

;                  _________________
;|________________|                 |
;
;|''''|''''|''''|''''|''''|''''|''''|''''|''''|
;0    5    10   15   20   25   30   35   40   45
;                          \________________/
;        Time period when we should be checking for a clock-inversion

	org	0x900

OutNothingSkip
	movlw	B'00001011'	;16 If the UART receiver queue length >= 64,
	btfsc	UR_LEN,6	;17  deassert CTS so the host stops sending us
	movwf	PORTA		;18  data
	movlb	7		;19  "
	bra	OutCheckInv	;20-21

LtReceiver
	movlb	7		;06 Ready to detect inversion
	bcf	IOCAF,IOCAF3	;07  "
	clrf	LR_STATE	;08 Reset receiver state
	movlw	0xEB		;09 Don't need pop pointer here, so point FSR1
	movwf	FSR1L		;10  to LocalTalk receiver frame registers
	nop			;11
	;fall through

OutNothing
	movlb	0		;12 Check if there is a byte waiting from the
	btfss	PIR1,RCIF	;13  UART; if there isn't, skip the rest of
	bra	OutNothingSkip	;14(-15)  this
	movlb	3		;15 Get the received byte
	movf	RCREG,W		;16  "
	movlb	7		;17  "
	movwi	FSR0++		;18 Push it onto the queue
	bcf	FSR0L,7		;19 Wrap the queue around
	incf	UR_LEN,F	;20 Increment the queue length
	nop			;21
	;fall through

OutCheckInv
	bsf	STATUS,C	;22 If the IOC flag was set while the preceding
	btfsc	IOCAF,IOCAF3	;23  code was running, this bit is a 0; in
	bcf	STATUS,C	;24  either case, copy it into the carry bit
	bcf	IOCAF,IOCAF3	;25 Clear the IOC flag so we can look for clock
	btfsc	IOCAF,IOCAF3	;26 Check if the line has inverted, indicating
	bra	OutReceive	;27  a clock
	btfsc	IOCAF,IOCAF3	;28  "
	bra	OutReceive	;29  "
	btfsc	IOCAF,IOCAF3	;30  "
	bra	OutReceive	;31  "
	btfsc	IOCAF,IOCAF3	;32  "
	bra	OutReceive	;33  "
	btfsc	IOCAF,IOCAF3	;34  "
	bra	OutReceive	;35  "
	btfsc	IOCAF,IOCAF3	;36  "
	bra	OutReceive	;37  "
	btfsc	IOCAF,IOCAF3	;38  "
	bra	OutReceive	;39  "
	btfsc	IOCAF,IOCAF3	;40  "
	bra	OutReceive	;41  "
	btfsc	IOCAF,IOCAF3	;42  "
	bra	OutReceive	;43  "
	bra	OutLostClock	;By this point, we must assume we've lost clock

EmpNothingSkip
	movlw	B'00001011'	;16 If the UART receiver queue length >= 64,
	btfsc	UR_LEN,6	;17  deassert CTS so the host stops sending us
	movwf	PORTA		;18  data
	movlb	7		;19  "
	bra	EmpCheckInv	;20-21

OutFlag
EmpFlag
EmpNothing
	movlb	0		;12 Check if there is a byte waiting from the
	btfss	PIR1,RCIF	;13  UART; if there isn't, skip the rest of
	bra	EmpNothingSkip	;14(-15)  this
	movlb	3		;15 Get the received byte
	movf	RCREG,W		;16  "
	movlb	7		;17  "
	movwi	FSR0++		;18 Push it onto the queue
	bcf	FSR0L,7		;19 Wrap the queue around
	incf	UR_LEN,F	;20 Increment the queue length
	nop			;21
	;fall through

EmpCheckInv
	bsf	STATUS,C	;22 If the IOC flag was set while the preceding
	btfsc	IOCAF,IOCAF3	;23  code was running, this bit is a 0; in
	bcf	STATUS,C	;24  either case, copy it into the carry bit
	bcf	IOCAF,IOCAF3	;25 Clear the IOC flag so we can look for clock
	btfsc	IOCAF,IOCAF3	;26 Check if the line has inverted, indicating
	bra	EmpReceive	;27  a clock
	btfsc	IOCAF,IOCAF3	;28  "
	bra	EmpReceive	;29  "
	btfsc	IOCAF,IOCAF3	;30  "
	bra	EmpReceive	;31  "
	btfsc	IOCAF,IOCAF3	;32  "
	bra	EmpReceive	;33  "
	btfsc	IOCAF,IOCAF3	;34  "
	bra	EmpReceive	;35  "
	btfsc	IOCAF,IOCAF3	;36  "
	bra	EmpReceive	;37  "
	btfsc	IOCAF,IOCAF3	;38  "
	bra	EmpReceive	;39  "
	btfsc	IOCAF,IOCAF3	;40  "
	bra	EmpReceive	;41  "
	btfsc	IOCAF,IOCAF3	;42  "
	bra	EmpReceive	;43  "
	bra	EmpLostClock	;By this point, we must assume we've lost clock

InNothingSkip
	movlw	B'00001011'	;16 If the UART receiver queue length >= 64,
	btfsc	UR_LEN,6	;17  deassert CTS so the host stops sending us
	movwf	PORTA		;18  data
	movlb	7		;19  "
	bra	InCheckInv	;20-21

InNothing
	movlb	0		;12 Check if there is a byte waiting from the
	btfss	PIR1,RCIF	;13  UART; if there isn't, skip the rest of
	bra	InNothingSkip	;14(-15)  this
	movlb	3		;15 Get the received byte
	movf	RCREG,W		;16  "
	movlb	7		;17  "
	movwi	FSR0++		;18 Push it onto the queue
	bcf	FSR0L,7		;19 Wrap the queue around
	incf	UR_LEN,F	;20 Increment the queue length
	nop			;21
	;fall through

InCheckInv
	bsf	STATUS,C	;22 If the IOC flag was set while the preceding
	btfsc	IOCAF,IOCAF3	;23  code was running, this bit is a 0; in
	bcf	STATUS,C	;24  either case, copy it into the carry bit
	bcf	IOCAF,IOCAF3	;25 Clear the IOC flag so we can look for clock
	btfsc	IOCAF,IOCAF3	;26 Check if the line has inverted, indicating
	bra	InReceive	;27  a clock
	btfsc	IOCAF,IOCAF3	;28  "
	bra	InReceive	;29  "
	btfsc	IOCAF,IOCAF3	;30  "
	bra	InReceive	;31  "
	btfsc	IOCAF,IOCAF3	;32  "
	bra	InReceive	;33  "
	btfsc	IOCAF,IOCAF3	;34  "
	bra	InReceive	;35  "
	btfsc	IOCAF,IOCAF3	;36  "
	bra	InReceive	;37  "
	btfsc	IOCAF,IOCAF3	;38  "
	bra	InReceive	;39  "
	btfsc	IOCAF,IOCAF3	;40  "
	bra	InReceive	;41  "
	btfsc	IOCAF,IOCAF3	;42  "
	bra	InReceive	;43  "
	bra	InLostClock	;By this point, we must assume we've lost clock

OutReceive
	bcf	IOCAF,IOCAF3	;00 Ready to detect inversion
	movlp	high OutFSA	;01 Point PCLATH to the out-of-frame FSA for 0
	btfsc	STATUS,C	;02 Increment to the FSA for 1 if we got a 1
	incf	PCLATH,F	;03  "
	movf	LR_STATE,W	;04 Jump to the state pointed to by STATE
	movwf	PCL		;05-06  "

EmpReceive
	bcf	IOCAF,IOCAF3	;00 Ready to detect inversion
	movlp	high EmpFSA	;01 Point PCLATH to the empty-frame FSA for 0
	btfsc	STATUS,C	;02 Increment to the FSA for 1 if we got a 1
	incf	PCLATH,F	;03  "
	movf	LR_STATE,W	;04 Jump to the state pointed to by STATE
	movwf	PCL		;05-06  "

InReceive
	bcf	IOCAF,IOCAF3	;00 Ready to detect inversion
	movlp	high InFSA	;01 Point PCLATH to the in-frame FSA for 0
	btfsc	STATUS,C	;02 Increment to the FSA for 1 if we got a 1
	incf	PCLATH,F	;03  "
	movf	LR_STATE,W	;04 Jump to the state pointed to by STATE
	movwf	PCL		;05-06  "

InFlag
	movlb	3		;12 Transmit a zero, which is an escape
	clrf	TXREG		;13  character
	movlw	0xFD		;14 Transmit 0xFD, which signifies 'Frame Done'
	movwf	TXREG		;15  "
	movlb	7		;16  "
	bsf	FLAGS,LR_FRM	;17 Raise the flag that frame regs have changed
	DNOP			;18-19
	bra	OutCheckInv	;20-21 Switch to out-of-frame inversion check

EmpByte
InByte
	movf	LR_BUF,W	;12 Save the received byte into the frame regs
	movwi	FSR1++		;13  using FSR1
	btfss	STATUS,Z	;14 If the received byte is not a zero, just
	bra	InByte2		;15(-16)  transmit it as-is
	movlb	3		;16 If the received byte is a zero, transmit
	clrf	TXREG		;17  escape sequence 0x00 0xFF
	decf	TXREG,F		;18  "
	movlb	7		;19  "
	bra	InCheckInv	;20-21	
InByte2	movlb	3		;17 Transmit the received byte on the UART
	movwf	TXREG		;18  "
	movlb	7		;19  "
	bra	InCheckInv	;20-21

OutFErr
EmpFErr
InFErr
	bcf	IOCAF,IOCAF3	;Ready to detect inversion
	movlb	0		;00 Check if there is a byte waiting from the
	bcf	STATUS,C	;01  UART
	btfsc	PIR1,RCIF	;02  "
	bsf	STATUS,C	;03  "
	movlb	3		;04 If there is a byte waiting from the UART,
	btfsc	STATUS,C	;05  push it onto the queue
	movf	RCREG,W		;06  "
	movlb	0		;07  "
	btfsc	STATUS,C	;08  "
	movwi	FSR0++		;09  "
	bcf	FSR0L,7		;10 Wrap the queue around
	btfsc	STATUS,C	;11 If there was a byte waiting from the UART,
	incf	UR_LEN,F	;12  increment the queue length
	movlw	B'00001011'	;13 If the UART receiver queue length >= 64,
	btfsc	UR_LEN,6	;14  deassert CTS so the host stops sending us
	movwf	PORTA		;15  data
	bcf	STATUS,C	;16 Check if there is a byte waiting from the
	btfsc	PIR1,RCIF	;17  UART
	bsf	STATUS,C	;18  "
	movlb	3		;19 If there is a byte waiting from the UART,
	btfsc	STATUS,C	;20  push it onto the queue
	movf	RCREG,W		;21  "
	movlb	0		;22  "
	btfsc	STATUS,C	;23  "
	movwi	FSR0++		;24  "
	bcf	FSR0L,7		;25 Wrap the queue around
	btfsc	STATUS,C	;26 If there was a byte waiting from the UART,
	incf	UR_LEN,F	;27  increment the queue length
	movlw	B'00001011'	;28 If the UART receiver queue length >= 64,
	btfsc	UR_LEN,6	;29  deassert CTS so the host stops sending us
	movwf	PORTA		;30  data
	bcf	STATUS,C	;31 Check if there is a byte waiting from the
	btfsc	PIR1,RCIF	;32  UART
	bsf	STATUS,C	;33  "
	movlb	3		;34 If there is a byte waiting from the UART,
	btfsc	STATUS,C	;35  push it onto the queue
	movf	RCREG,W		;36  "
	movlb	0		;37  "
	btfsc	STATUS,C	;38  "
	movwi	FSR0++		;39  "
	bcf	FSR0L,7		;40 Wrap the queue around
	btfsc	STATUS,C	;41 If there was a byte waiting from the UART,
	incf	UR_LEN,F	;42  increment the queue length
	movlw	B'00001011'	;43 If the UART receiver queue length >= 64,
	btfsc	UR_LEN,6	;44  deassert CTS so the host stops sending us
	movwf	PORTA		;45  data	
	movlb	7		;If there's been an inversion in the elapsed
	btfsc	IOCAF,IOCAF3	; ~1.25 bit times, the line is not yet idle and
	bra	OutFErr		; we should try again
	movlb	3		;Transmit a zero, which is an escape character
	clrf	TXREG		; "
	movlw	0xFE		;Transmit 0xFE, which signifies 'Frame Error'
	movwf	TXREG		; "
	bra	FinishUp

InLostClock
	movlb	0		;Wait until the UART transmitter is ready for
	btfss	PIR1,TXIF	; a byte
	bra	$-1		; "
	movlb	3		;Transmit a zero, which is an escape character
	clrf	TXREG		; "
	movlb	0		;Wait until the UART transmitter is ready for
	btfss	PIR1,TXIF	; another byte
	bra	$-1		; "
	movlb	3		;Transmit 0xFA, which signifies 'Frame Aborted'
	movlw	0xFA		; "
	movwf	TXREG		; "
	bra	FinishUp

OutLostClock
EmpLostClock
FinishUp
	movlb	0		;Deactivate Timer2 and clear its interrupt flag
	bcf	T2CON,TMR2ON	; so anything that was waiting for it knows
	bcf	PIR1,TMR2IF	; that a receive event happened while waiting
	movf	FSR0L,W		;Store the changed UART receiver push point
	movlb	31		; back in its shadow register so it stays the
	movwf	FSR0L_SHAD	; same when we return from interrupt
	retfie


;;; LocalTalk Receiver State Machines ;;;

OutFSA	org	0xA00

Oswait0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c0r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c1r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c2r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c3r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c4r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c5r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c6r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c7r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c0r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c1r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c2r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c3r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c4r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c5r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c6r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c7r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c0r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c1r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c2r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c3r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c4r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c5r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c6r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c7r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c0r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c1r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c2r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c3r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c4r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c5r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c6r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c7r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c0r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c1r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c2r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c3r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c4r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c5r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c6r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c7r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c0r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c1r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c2r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c3r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c4r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c5r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c6r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c7r0	bcf	LR_BUF,0	;07
	movlw	low Os0c1r0	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os6c7r0	bcf	LR_BUF,7	;07
	movlw	low Os0c0r0	;08
	movwf	LR_STATE	;09
	goto	OutFlag		;10-11

	org	0xB00

Oswait1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c0r1	bsf	LR_BUF,0	;07
	movlw	low Os1c1r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c1r1	bsf	LR_BUF,1	;07
	movlw	low Os1c2r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c2r1	bsf	LR_BUF,2	;07
	movlw	low Os1c3r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c3r1	bsf	LR_BUF,3	;07
	movlw	low Os1c4r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c4r1	bsf	LR_BUF,4	;07
	movlw	low Os1c5r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c5r1	bsf	LR_BUF,5	;07
	movlw	low Os1c6r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c6r1	bsf	LR_BUF,6	;07
	movlw	low Os1c7r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os0c7r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os1c0r1	bsf	LR_BUF,0	;07
	movlw	low Os2c1r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c1r1	bsf	LR_BUF,1	;07
	movlw	low Os2c2r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c2r1	bsf	LR_BUF,2	;07
	movlw	low Os2c3r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c3r1	bsf	LR_BUF,3	;07
	movlw	low Os2c4r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c4r1	bsf	LR_BUF,4	;07
	movlw	low Os2c5r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c5r1	bsf	LR_BUF,5	;07
	movlw	low Os2c6r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c6r1	bsf	LR_BUF,6	;07
	movlw	low Os2c7r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os1c7r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os2c0r1	bsf	LR_BUF,0	;07
	movlw	low Os3c1r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c1r1	bsf	LR_BUF,1	;07
	movlw	low Os3c2r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c2r1	bsf	LR_BUF,2	;07
	movlw	low Os3c3r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c3r1	bsf	LR_BUF,3	;07
	movlw	low Os3c4r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c4r1	bsf	LR_BUF,4	;07
	movlw	low Os3c5r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c5r1	bsf	LR_BUF,5	;07
	movlw	low Os3c6r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c6r1	bsf	LR_BUF,6	;07
	movlw	low Os3c7r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os2c7r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os3c0r1	bsf	LR_BUF,0	;07
	movlw	low Os4c1r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c1r1	bsf	LR_BUF,1	;07
	movlw	low Os4c2r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c2r1	bsf	LR_BUF,2	;07
	movlw	low Os4c3r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c3r1	bsf	LR_BUF,3	;07
	movlw	low Os4c4r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c4r1	bsf	LR_BUF,4	;07
	movlw	low Os4c5r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c5r1	bsf	LR_BUF,5	;07
	movlw	low Os4c6r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c6r1	bsf	LR_BUF,6	;07
	movlw	low Os4c7r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os3c7r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os4c0r1	bsf	LR_BUF,0	;07
	movlw	low Os5c1r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c1r1	bsf	LR_BUF,1	;07
	movlw	low Os5c2r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c2r1	bsf	LR_BUF,2	;07
	movlw	low Os5c3r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c3r1	bsf	LR_BUF,3	;07
	movlw	low Os5c4r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c4r1	bsf	LR_BUF,4	;07
	movlw	low Os5c5r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c5r1	bsf	LR_BUF,5	;07
	movlw	low Os5c6r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c6r1	bsf	LR_BUF,6	;07
	movlw	low Os5c7r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os4c7r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os5c0r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os5c1r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os5c2r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os5c3r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os5c4r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os5c5r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os5c6r1	bsf	LR_BUF,6	;07
	movlw	low Os6c7r1	;08
	movwf	LR_STATE	;09
	goto	OutNothing	;10-11
Os5c7r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11
Os6c7r1	nop			;07
	movlw	low Oswait1	;08
	movwf	LR_STATE	;09
	goto	OutFErr		;10-11

EmpFSA	org	0xC00

Eswait0	bcf	LR_BUF,0	;07
	movlw	low Es0c1r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c0r0	bcf	LR_BUF,0	;07
	movlw	low Es0c1r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c1r0	bcf	LR_BUF,1	;07
	movlw	low Es0c2r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c2r0	bcf	LR_BUF,2	;07
	movlw	low Es0c3r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c3r0	bcf	LR_BUF,3	;07
	movlw	low Es0c4r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c4r0	bcf	LR_BUF,4	;07
	movlw	low Es0c5r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c5r0	bcf	LR_BUF,5	;07
	movlw	low Es0c6r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c6r0	bcf	LR_BUF,6	;07
	movlw	low Es0c7r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c7r0	bcf	LR_BUF,7	;07
	movlw	low Es0c0r0	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es1c0r0	bcf	LR_BUF,0	;07
	movlw	low Es0c1r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c1r0	bcf	LR_BUF,1	;07
	movlw	low Es0c2r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c2r0	bcf	LR_BUF,2	;07
	movlw	low Es0c3r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c3r0	bcf	LR_BUF,3	;07
	movlw	low Es0c4r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c4r0	bcf	LR_BUF,4	;07
	movlw	low Es0c5r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c5r0	bcf	LR_BUF,5	;07
	movlw	low Es0c6r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c6r0	bcf	LR_BUF,6	;07
	movlw	low Es0c7r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c7r0	bcf	LR_BUF,7	;07
	movlw	low Es0c0r0	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es2c0r0	bcf	LR_BUF,0	;07
	movlw	low Es0c1r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c1r0	bcf	LR_BUF,1	;07
	movlw	low Es0c2r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c2r0	bcf	LR_BUF,2	;07
	movlw	low Es0c3r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c3r0	bcf	LR_BUF,3	;07
	movlw	low Es0c4r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c4r0	bcf	LR_BUF,4	;07
	movlw	low Es0c5r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c5r0	bcf	LR_BUF,5	;07
	movlw	low Es0c6r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c6r0	bcf	LR_BUF,6	;07
	movlw	low Es0c7r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c7r0	bcf	LR_BUF,7	;07
	movlw	low Es0c0r0	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es3c0r0	bcf	LR_BUF,0	;07
	movlw	low Es0c1r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c1r0	bcf	LR_BUF,1	;07
	movlw	low Es0c2r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c2r0	bcf	LR_BUF,2	;07
	movlw	low Es0c3r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c3r0	bcf	LR_BUF,3	;07
	movlw	low Es0c4r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c4r0	bcf	LR_BUF,4	;07
	movlw	low Es0c5r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c5r0	bcf	LR_BUF,5	;07
	movlw	low Es0c6r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c6r0	bcf	LR_BUF,6	;07
	movlw	low Es0c7r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c7r0	bcf	LR_BUF,7	;07
	movlw	low Es0c0r0	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es4c0r0	bcf	LR_BUF,0	;07
	movlw	low Es0c1r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c1r0	bcf	LR_BUF,1	;07
	movlw	low Es0c2r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c2r0	bcf	LR_BUF,2	;07
	movlw	low Es0c3r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c3r0	bcf	LR_BUF,3	;07
	movlw	low Es0c4r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c4r0	bcf	LR_BUF,4	;07
	movlw	low Es0c5r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c5r0	bcf	LR_BUF,5	;07
	movlw	low Es0c6r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c6r0	bcf	LR_BUF,6	;07
	movlw	low Es0c7r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c7r0	bcf	LR_BUF,7	;07
	movlw	low Es0c0r0	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es5c0r0	nop			;07
	movlw	low Es0c0r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es5c1r0	nop			;07
	movlw	low Es0c1r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es5c2r0	nop			;07
	movlw	low Es0c2r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es5c3r0	nop			;07
	movlw	low Es0c3r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es5c4r0	nop			;07
	movlw	low Es0c4r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es5c5r0	nop			;07
	movlw	low Es0c5r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es5c6r0	nop			;07
	movlw	low Es0c6r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es5c7r0	nop			;07
	movlw	low Es0c7r0	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es6c7r0	bcf	LR_BUF,7	;07
	movlw	low Es0c0r0	;08
	movwf	LR_STATE	;09
	goto	EmpFlag		;10-11

	org	0xD00

Eswait1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c0r1	bsf	LR_BUF,0	;07
	movlw	low Es1c1r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c1r1	bsf	LR_BUF,1	;07
	movlw	low Es1c2r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c2r1	bsf	LR_BUF,2	;07
	movlw	low Es1c3r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c3r1	bsf	LR_BUF,3	;07
	movlw	low Es1c4r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c4r1	bsf	LR_BUF,4	;07
	movlw	low Es1c5r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c5r1	bsf	LR_BUF,5	;07
	movlw	low Es1c6r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c6r1	bsf	LR_BUF,6	;07
	movlw	low Es1c7r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es0c7r1	bsf	LR_BUF,7	;07
	movlw	low Es1c0r1	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es1c0r1	bsf	LR_BUF,0	;07
	movlw	low Es2c1r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c1r1	bsf	LR_BUF,1	;07
	movlw	low Es2c2r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c2r1	bsf	LR_BUF,2	;07
	movlw	low Es2c3r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c3r1	bsf	LR_BUF,3	;07
	movlw	low Es2c4r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c4r1	bsf	LR_BUF,4	;07
	movlw	low Es2c5r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c5r1	bsf	LR_BUF,5	;07
	movlw	low Es2c6r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c6r1	bsf	LR_BUF,6	;07
	movlw	low Es2c7r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es1c7r1	bsf	LR_BUF,7	;07
	movlw	low Es2c0r1	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es2c0r1	bsf	LR_BUF,0	;07
	movlw	low Es3c1r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c1r1	bsf	LR_BUF,1	;07
	movlw	low Es3c2r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c2r1	bsf	LR_BUF,2	;07
	movlw	low Es3c3r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c3r1	bsf	LR_BUF,3	;07
	movlw	low Es3c4r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c4r1	bsf	LR_BUF,4	;07
	movlw	low Es3c5r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c5r1	bsf	LR_BUF,5	;07
	movlw	low Es3c6r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c6r1	bsf	LR_BUF,6	;07
	movlw	low Es3c7r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es2c7r1	bsf	LR_BUF,7	;07
	movlw	low Es3c0r1	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es3c0r1	bsf	LR_BUF,0	;07
	movlw	low Es4c1r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c1r1	bsf	LR_BUF,1	;07
	movlw	low Es4c2r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c2r1	bsf	LR_BUF,2	;07
	movlw	low Es4c3r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c3r1	bsf	LR_BUF,3	;07
	movlw	low Es4c4r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c4r1	bsf	LR_BUF,4	;07
	movlw	low Es4c5r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c5r1	bsf	LR_BUF,5	;07
	movlw	low Es4c6r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c6r1	bsf	LR_BUF,6	;07
	movlw	low Es4c7r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es3c7r1	bsf	LR_BUF,7	;07
	movlw	low Es4c0r1	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es4c0r1	bsf	LR_BUF,0	;07
	movlw	low Es5c1r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c1r1	bsf	LR_BUF,1	;07
	movlw	low Es5c2r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c2r1	bsf	LR_BUF,2	;07
	movlw	low Es5c3r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c3r1	bsf	LR_BUF,3	;07
	movlw	low Es5c4r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c4r1	bsf	LR_BUF,4	;07
	movlw	low Es5c5r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c5r1	bsf	LR_BUF,5	;07
	movlw	low Es5c6r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c6r1	bsf	LR_BUF,6	;07
	movlw	low Es5c7r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es4c7r1	bsf	LR_BUF,7	;07
	movlw	low Es5c0r1	;08
	movwf	LR_STATE	;09
	goto	EmpByte		;10-11
Es5c0r1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpFErr		;10-11
Es5c1r1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpFErr		;10-11
Es5c2r1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpFErr		;10-11
Es5c3r1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpFErr		;10-11
Es5c4r1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpFErr		;10-11
Es5c5r1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpFErr		;10-11
Es5c6r1	bsf	LR_BUF,6	;07
	movlw	low Es6c7r1	;08
	movwf	LR_STATE	;09
	goto	EmpNothing	;10-11
Es5c7r1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpFErr		;10-11
Es6c7r1	nop			;07
	movlw	low Eswait1	;08
	movwf	LR_STATE	;09
	goto	EmpFErr		;10-11

InFSA	org	0xE00

Iswait0	bcf	LR_BUF,0	;07
	movlw	low Is0c1r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c0r0	bcf	LR_BUF,0	;07
	movlw	low Is0c1r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c1r0	bcf	LR_BUF,1	;07
	movlw	low Is0c2r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c2r0	bcf	LR_BUF,2	;07
	movlw	low Is0c3r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c3r0	bcf	LR_BUF,3	;07
	movlw	low Is0c4r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c4r0	bcf	LR_BUF,4	;07
	movlw	low Is0c5r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c5r0	bcf	LR_BUF,5	;07
	movlw	low Is0c6r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c6r0	bcf	LR_BUF,6	;07
	movlw	low Is0c7r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c7r0	bcf	LR_BUF,7	;07
	movlw	low Is0c0r0	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is1c0r0	bcf	LR_BUF,0	;07
	movlw	low Is0c1r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c1r0	bcf	LR_BUF,1	;07
	movlw	low Is0c2r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c2r0	bcf	LR_BUF,2	;07
	movlw	low Is0c3r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c3r0	bcf	LR_BUF,3	;07
	movlw	low Is0c4r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c4r0	bcf	LR_BUF,4	;07
	movlw	low Is0c5r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c5r0	bcf	LR_BUF,5	;07
	movlw	low Is0c6r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c6r0	bcf	LR_BUF,6	;07
	movlw	low Is0c7r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c7r0	bcf	LR_BUF,7	;07
	movlw	low Is0c0r0	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is2c0r0	bcf	LR_BUF,0	;07
	movlw	low Is0c1r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c1r0	bcf	LR_BUF,1	;07
	movlw	low Is0c2r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c2r0	bcf	LR_BUF,2	;07
	movlw	low Is0c3r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c3r0	bcf	LR_BUF,3	;07
	movlw	low Is0c4r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c4r0	bcf	LR_BUF,4	;07
	movlw	low Is0c5r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c5r0	bcf	LR_BUF,5	;07
	movlw	low Is0c6r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c6r0	bcf	LR_BUF,6	;07
	movlw	low Is0c7r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c7r0	bcf	LR_BUF,7	;07
	movlw	low Is0c0r0	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is3c0r0	bcf	LR_BUF,0	;07
	movlw	low Is0c1r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c1r0	bcf	LR_BUF,1	;07
	movlw	low Is0c2r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c2r0	bcf	LR_BUF,2	;07
	movlw	low Is0c3r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c3r0	bcf	LR_BUF,3	;07
	movlw	low Is0c4r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c4r0	bcf	LR_BUF,4	;07
	movlw	low Is0c5r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c5r0	bcf	LR_BUF,5	;07
	movlw	low Is0c6r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c6r0	bcf	LR_BUF,6	;07
	movlw	low Is0c7r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c7r0	bcf	LR_BUF,7	;07
	movlw	low Is0c0r0	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is4c0r0	bcf	LR_BUF,0	;07
	movlw	low Is0c1r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c1r0	bcf	LR_BUF,1	;07
	movlw	low Is0c2r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c2r0	bcf	LR_BUF,2	;07
	movlw	low Is0c3r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c3r0	bcf	LR_BUF,3	;07
	movlw	low Is0c4r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c4r0	bcf	LR_BUF,4	;07
	movlw	low Is0c5r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c5r0	bcf	LR_BUF,5	;07
	movlw	low Is0c6r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c6r0	bcf	LR_BUF,6	;07
	movlw	low Is0c7r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c7r0	bcf	LR_BUF,7	;07
	movlw	low Is0c0r0	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is5c0r0	nop			;07
	movlw	low Is0c0r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is5c1r0	nop			;07
	movlw	low Is0c1r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is5c2r0	nop			;07
	movlw	low Is0c2r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is5c3r0	nop			;07
	movlw	low Is0c3r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is5c4r0	nop			;07
	movlw	low Is0c4r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is5c5r0	nop			;07
	movlw	low Is0c5r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is5c6r0	nop			;07
	movlw	low Is0c6r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is5c7r0	nop			;07
	movlw	low Is0c7r0	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is6c7r0	bcf	LR_BUF,7	;07
	movlw	low Iswait0	;08
	movwf	LR_STATE	;09
	goto	InFlag		;10-11

	org	0xF00

Iswait1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c0r1	bsf	LR_BUF,0	;07
	movlw	low Is1c1r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c1r1	bsf	LR_BUF,1	;07
	movlw	low Is1c2r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c2r1	bsf	LR_BUF,2	;07
	movlw	low Is1c3r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c3r1	bsf	LR_BUF,3	;07
	movlw	low Is1c4r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c4r1	bsf	LR_BUF,4	;07
	movlw	low Is1c5r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c5r1	bsf	LR_BUF,5	;07
	movlw	low Is1c6r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c6r1	bsf	LR_BUF,6	;07
	movlw	low Is1c7r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is0c7r1	bsf	LR_BUF,7	;07
	movlw	low Is1c0r1	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is1c0r1	bsf	LR_BUF,0	;07
	movlw	low Is2c1r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c1r1	bsf	LR_BUF,1	;07
	movlw	low Is2c2r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c2r1	bsf	LR_BUF,2	;07
	movlw	low Is2c3r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c3r1	bsf	LR_BUF,3	;07
	movlw	low Is2c4r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c4r1	bsf	LR_BUF,4	;07
	movlw	low Is2c5r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c5r1	bsf	LR_BUF,5	;07
	movlw	low Is2c6r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c6r1	bsf	LR_BUF,6	;07
	movlw	low Is2c7r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is1c7r1	bsf	LR_BUF,7	;07
	movlw	low Is2c0r1	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is2c0r1	bsf	LR_BUF,0	;07
	movlw	low Is3c1r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c1r1	bsf	LR_BUF,1	;07
	movlw	low Is3c2r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c2r1	bsf	LR_BUF,2	;07
	movlw	low Is3c3r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c3r1	bsf	LR_BUF,3	;07
	movlw	low Is3c4r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c4r1	bsf	LR_BUF,4	;07
	movlw	low Is3c5r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c5r1	bsf	LR_BUF,5	;07
	movlw	low Is3c6r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c6r1	bsf	LR_BUF,6	;07
	movlw	low Is3c7r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is2c7r1	bsf	LR_BUF,7	;07
	movlw	low Is3c0r1	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is3c0r1	bsf	LR_BUF,0	;07
	movlw	low Is4c1r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c1r1	bsf	LR_BUF,1	;07
	movlw	low Is4c2r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c2r1	bsf	LR_BUF,2	;07
	movlw	low Is4c3r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c3r1	bsf	LR_BUF,3	;07
	movlw	low Is4c4r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c4r1	bsf	LR_BUF,4	;07
	movlw	low Is4c5r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c5r1	bsf	LR_BUF,5	;07
	movlw	low Is4c6r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c6r1	bsf	LR_BUF,6	;07
	movlw	low Is4c7r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is3c7r1	bsf	LR_BUF,7	;07
	movlw	low Is4c0r1	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is4c0r1	bsf	LR_BUF,0	;07
	movlw	low Is5c1r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c1r1	bsf	LR_BUF,1	;07
	movlw	low Is5c2r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c2r1	bsf	LR_BUF,2	;07
	movlw	low Is5c3r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c3r1	bsf	LR_BUF,3	;07
	movlw	low Is5c4r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c4r1	bsf	LR_BUF,4	;07
	movlw	low Is5c5r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c5r1	bsf	LR_BUF,5	;07
	movlw	low Is5c6r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c6r1	bsf	LR_BUF,6	;07
	movlw	low Is5c7r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is4c7r1	bsf	LR_BUF,7	;07
	movlw	low Is5c0r1	;08
	movwf	LR_STATE	;09
	goto	InByte		;10-11
Is5c0r1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InFErr		;10-11
Is5c1r1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InFErr		;10-11
Is5c2r1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InFErr		;10-11
Is5c3r1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InFErr		;10-11
Is5c4r1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InFErr		;10-11
Is5c5r1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InFErr		;10-11
Is5c6r1	bsf	LR_BUF,6	;07
	movlw	low Is6c7r1	;08
	movwf	LR_STATE	;09
	goto	InNothing	;10-11
Is5c7r1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InFErr		;10-11
Is6c7r1	nop			;07
	movlw	low Iswait1	;08
	movwf	LR_STATE	;09
	goto	InFErr		;10-11


;;; End of Program ;;;
	end
