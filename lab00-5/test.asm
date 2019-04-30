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

.org INT0addr
	jmp EXT_INT0
.org INT1addr
	jmp EXT_INT1






RESET:
	ldi temp1, high(RAMEND)
	out SPH, temp1
	ldi temp1, low(RAMEND)
	out SPL, temp1

	ldi temp1, 0b00010000
	;ser temp1
	out DDRE, temp1

;	sts PWM, temp ; Bit 4 will function as OC3A.
	;clr temp
	;out PORTA, temp
	ldi temp1, 0xFF ; the value controls the PWM duty cycle
	sts OCR3BL, temp1
	clr temp1
	sts OCR3BH, temp1	
	; Set the Timer5 to Phase Correct PWM mode.
	ldi temp1, (1 << CS30)
	sts TCCR3B, temp1
	ldi temp1, ((1<< WGM30)|(1<<COM3B1))
	sts TCCR3A, temp1


;	ldi temp1, (2 << ISC01) ; set INT2 as fallingsts EICRA, temp1 ; edge triggered interrupt
;	sts EICRA, temp1

	in temp1, EIMSK ; enable INT2
	ori temp1, (1<<INT1)
	out EIMSK, temp1

	in temp1, EIMSK ; enable INT2
	ori temp1, (1<<INT0)
	out EIMSK, temp1

	ldi temp1, (2 << ISC00 | 2 << ISC01 | 2 << ISC10) ; set INT2 as fallingsts EICRA, temp1 ; edge triggered interrupt
	sts EICRA, temp1

	sei

halt:
	rjmp halt



EXT_INT0:
	push r24
	push r25


	ldi temp, 0xFF ; the value controls the PWM duty cycle

	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp

	pop r25
	pop r24
	reti

EXT_INT1:
	push r24
	push r25
	ldi temp, 0x2A ; the value controls the PWM duty cycle

	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp

	pop r25
	pop r24
	reti