.include "m2560def.inc"
	.def temp = r16
	ser temp
	;ldi temp, 0b00001000
	out DDRA, temp ; Bit 3 will function as OC5A.
	clr temp
	out PORTA, temp
	ldi temp, 0x4A ; the value controls the PWM duty cycle
	sts OCR5AL, temp
	clr temp
	sts OCR5AH, temp
	; Set the Timer5 to Phase Correct PWM mode.
	ldi temp, (1 << CS50)
	sts TCCR5B, temp
	ldi temp, (1<< WGM50)|(1<<COM5A1)
	sts TCCR5A, temp
halt:
	rjmp halt
