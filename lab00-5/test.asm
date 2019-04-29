.include "m2560def.inc"
	.def temp = r16
	.def temp1 = r20
	.def temp2 = r21

.dseg
	.org 0x200

TempCounter:
	.byte 2
SecondCounter:
	.byte 2
PWM:
	.byte 1

.cseg
.org 0x0000
	jmp RESET
.org OVF3addr
	jmp OVF3address

RESET:
	ldi temp1, high(RAMEND)
	out SPH, temp1
	ldi temp1, low(RAMEND)
	out SPL, temp1

	ser temp
	out DDRE, temp
	clr temp
	out PORTE, temp
	ldi temp, 0b00010000
	sts PWM, temp ; Bit 4 will function as OC5A.
	;clr temp
	;out PORTA, temp
	ldi temp, 0x2F ; the value controls the PWM duty cycle
	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp
	; Set the Timer5 to Phase Correct PWM mode.
	ldi temp, (1 << CS30)
	sts TCCR3B, temp
	ldi temp, (1<< WGM30)|(1<<COM3A1)
	sts TCCR3A, temp
	ldi temp1, 1 << TOIE3
	sts TIMSK3, temp1
	sei
halt:
	;lds r24, OCR3BL
	;lds r25, OCR3BH
	lds r24, OCR3BL
	;ser r24
	
	out PORTE, r24
	rjmp halt

OVF3address:
	in temp1, SREG ;temp1 is temp 
	push temp1
	push YH
	push YL



	pop YL
	pop YH
	pop r20
	out SREG, temp1
	reti