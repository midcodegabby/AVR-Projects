;***********************************************************
;*
;*	This is the file for Lab 6 of ECE 375
;*
;*	 Author: Gabriel Rodgers and Will Corby
;*	   Date: 3/5/2024
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register

.def	leds = r25				; register to hold the speed leds

; this part copied from Lab 1
.def	waitcnt = r20				; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter

.equ	WTime = 50			; Time to wait in wait loop

.equ	EngEnR = 5				; right Engine Enable Bit
.equ	EngEnL = 6				; left Engine Enable Bit
.equ	EngDirR = 4				; right Engine Direction Bit
.equ	EngDirL = 7				; left Engine Direction Bit

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

; place instructions in interrupt vectors here, if needed
.org	$0002	; this is mapped to INT0
		rcall	SPEED_DOWN		; call the SPEED_DOWN subroutine
		reti					; return 

.org	$0004	; this is mapped to INT1
		rcall	SPEED_UP		; call the SPEED_UP subroutine
		reti

.org	$0008	; this is mapped to INT3
		rcall	SPEED_MAX		; call the SPEED_MAX subroutine
		reti

.org	$0056					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer
		; Initialize the Stack Pointer (VERY IMPORTANT!!!!)
		ldi		mpr, low(RAMEND) ; this limits the stack to RAMEND low
		out		SPL, mpr		; Load SPL with low byte of RAMEND (Stack Pointer Low)
		ldi		mpr, high(RAMEND) ; this limits the stack to RAMEND high
		out		SPH, mpr		; Load SPH with high byte of RAMEND (Stach Pointer High)

		; Configure I/O ports
		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State (active low)

		; Initialize Port B for output to LEDs
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low

		; Configure External Interrupts, if needed
		; first we need to ignore all interrupts while we set interrupts to falling edge, so set eimsk first
		ldi		mpr, 0b00000000 
		out		EIMSK, mpr

		; now we can set interrupts to trigger on falling edge:
		ldi		mpr, 0b10001010
		sts		EICRA, mpr

		; now we can set EIMSK to only care about INT3, INT1, and INT0
		ldi		mpr, 0b00001011
		out		EIMSK, mpr

		; Configure 16-bit Timer/Counter 1A and 1B: 
		; setting the COM, WGM, and CS bits in TCCR1A and TCCR1B: we are using inverting mode
		; set TCCR1A
		ldi		mpr, 0b11110001		; Load binary values into mpr
		sts		TCCR1A, mpr			; store mpr to TCCR1A

		; set TCCR1B
		ldi		mpr, 0b00001001		; Load binary values into mpr
		sts		TCCR1B, mpr			; store mpr to TCCR1B

		; set OCR to 0
		ldi		mpr, 0
		sts		OCR1AL, mpr
		sts		OCR1BL, mpr

		; setting to fast PWM (8 bits) sets TOP to $FF

		; Set TekBot to Move Forward (1<<EngDirR|1<<EngDirL) on Port B
		; initialize bumpbot to go forwards
		; Initialize TekBot Forward Movement
		ldi		mpr, MovFwd		; Load Move Forward Command
		out		PORTB, mpr		; Send command to motors

		; for an initial speed of 0 we need to have PB5 and PB4 ON max brightness;
		; so this means we need to set the duty cycle to start at 100% since we are using inv mode

		; Enable global interrupts (if any are used)
		sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		; infinite loop again; we are using interrupts
		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func:	SPEED_UP
; Desc:	This function increases OCR1A and OCR1B to decrease the duty cycle
;-----------------------------------------------------------
SPEED_UP:

		; first we need to check if the OCR1A value has already reached the top:
		; first load OCR1A contents into mpr
		lds		mpr, OCR1AL ; only need the lower 8 bits
		ldi		r17, $FF	; use r17 to hold a value of $FF (TOP)

		; now do the check thing
		cpse	mpr, r17	; this does a comparison and skips the next line if mpr and r17 are equal
		rjmp	upCheck		; jump to upCheck if mpr has not reached the top
		ret					; return if mpr has reached top

upCheck:
		; if the code gets here, that means we need to increment OCR1A and OCR1B by 17
		ldi		r17, 17
		add		mpr, r17	; add 17 to mpr

		; now we need to store mpr to OCRnx
		sts		OCR1AL, mpr
		sts		OCR1BL, mpr

		; now we need to update the LED values: increment the leds register and output it to PORTB
		in		leds, PORTB		; store PORTB to leds
		inc		leds			; increment leds
		out		PORTB, leds		; store leds to PORTB
		
		ldi		waitcnt, Wtime	; set the waitcnt register
		rcall	Wait	; call the wait function 

		; clear queued interrupts
		ldi		mpr, 0b00001011
		out		EIFR, mpr

		ret						; End a function with RET


;-----------------------------------------------------------
; Func:	SPEED_DOWN
; Desc:	This function decreases OCR1A and OCR1B to increase the duty cycle
;-----------------------------------------------------------
SPEED_DOWN:

		; first we need to check if the OCR1A value has already reached the top:
		; first load OCR1A contents into mpr
		lds		mpr, OCR1AL ; only need the lower 8 bits
		ldi		r17, 0	; use r17 to hold a value of 0 (Bottom)

		; now do the check thing
		cpse	mpr, r17	; this does a comparison and skips the next line if mpr and r17 are equal
		rjmp	downCheck		; jump to downCheck if mpr has not reached the bottom
		ret		; return and do nothing if we have reached the bottom

downCheck:
		; if the code gets here, that means we need to decrement OCR1A and OCR1B by 17
		ldi		r17, 17
		sub		mpr, r17	; subtract 17 from mpr

		; now we need to store mpr to OCRnx
		sts		OCR1AL, mpr
		sts		OCR1BL, mpr

		; now we need to update the LED values: decrement the leds register and output it to PORTB
		in		leds, PORTB		; store PORTB to leds
		dec		leds			; decrement leds
		out		PORTB, leds		; store leds to PORTB

		ldi		waitcnt, Wtime	; set the waitcnt register
		rcall	Wait	; call the wait function 

		; clear queued interrupts
		ldi		mpr, 0b00001011
		out		EIFR, mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	SPEED_MAX
; Desc:	This function increases OCR1A and OCR1B to decrease the duty cycle to the lowest amount
;-----------------------------------------------------------
SPEED_MAX:

		; first we need to check if the OCR1A value has already reached the top:
		; first load OCR1A contents into mpr
		lds		mpr, OCR1AL ; only need the lower 8 bits
		ldi		r17, $FF	; use r17 to hold a value of $FF (TOP)

		; now do the check thing
		cpse	mpr, r17	; this does a comparison and skips the next line if mpr and r17 are equal
		rjmp	maxCheck		; jump to maxCheck if mpr has not reached the top
		ret		; return and do nothing if we have reached the top

maxCheck:
		; if the code gets here, that means we need to set OCRnx to $FF

		; now we need to store r17 to OCRnx
		sts		OCR1AL, r17
		sts		OCR1BL, r17

		; now we need to update the LED values: set PORTB3:0 to 1111
		in		leds, PORTB		; store PORTB to leds
		ori		leds, 0b00001111		; mask leds with all 1s in the bottom 4 bits
		out		PORTB, leds		; store leds to PORTB

		ldi		waitcnt, Wtime	; set the waitcnt register
		rcall	Wait	; call the wait function 

		; clear queued interrupts
		ldi		mpr, 0b00001011
		out		EIFR, mpr

		ret						; End a function with RET

; this function was copied from Lab 1
;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly
;		waitcnt*10ms.  Just initialize wait for the specific amount
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			(((((3*ilcnt)-1+4)*olcnt)-1+4)*waitcnt)-1+16
;----------------------------------------------------------------
Wait:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224/2		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt		; Decrement wait
		brne	Loop			; Continue Wait loop

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret				; Return from subroutine


;***********************************************************
;*	Stored Program Data
;***********************************************************
		; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program

