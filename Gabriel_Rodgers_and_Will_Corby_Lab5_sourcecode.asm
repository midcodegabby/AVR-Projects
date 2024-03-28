;***********************************************************
;*	This is the .asm file for Lab 5 of ECE 375
;*
;*	 Author: Gabriel Rodgers and Will Corby
;*	   Date: 2/29/24
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
	
; this part copied from Lab 1
.def	waitcnt = r17				; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter
.def	lCount = r4			; left whisker counter
.def	rCount = r5			; right whisker counter

.equ	WTime = 50			; Time to wait in wait loop

; create vars to hold the data memory spaces for each lcount and rcount number
.equ	lStart = $0104
.equ	rStart = $010B

.equ	WskrR = 4				; Right Whisker Input Bit
.equ	WskrL = 5				; Left Whisker Input Bit
.equ	EngEnR = 5				; Right Engine Enable Bit
.equ	EngEnL = 6				; Left Engine Enable Bit
.equ	EngDirR = 4				; Right Engine Direction Bit
.equ	EngDirL = 7				; Left Engine Direction Bit

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00				; Move Backward Command
.equ	TurnR = (1<<EngDirL)			; Turn Right Command
.equ	TurnL = (1<<EngDirR)			; Turn Left Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used
.org	$0002	; this is mapped to INT0
		rcall	HitRight		; call the HitRight subroutine
		reti					; return 

.org	$0004	; this is mapped to INT1
		rcall	HitLeft			; call the HitLeft subroutine
		reti

.org	$0008	; this is mapped to INT3
		rcall	Clear			; call the clear subroutine
		reti

.org	$0056					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		; Initialize the Stack Pointer (VERY IMPORTANT!!!!)
		ldi		mpr, low(RAMEND) ; this limits the stack to RAMEND low
		out		SPL, mpr		; Load SPL with low byte of RAMEND (Stack Pointer Low)
		ldi		mpr, high(RAMEND) ; this limits the stack to RAMEND high
		out		SPH, mpr		; Load SPH with high byte of RAMEND (Stach Pointer High)

		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State

		; Initialize Port B for output to LEDs
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low

		; Initialize external interrupts
		
		; first we need to ignore all interrupts while we set interrupts to falling edge, so set eimsk first
		ldi		mpr, 0b00000000 
		out		EIMSK, mpr

		; now we can set interrupts to trigger on falling edge:
		ldi		mpr, 0b10001010
		sts		EICRA, mpr

		; now we can set EIMSK to only care about INT3, INT1, and INT0
		ldi		mpr, 0b00001011
		out		EIMSK, mpr

		; initialize bumpbot to go forwards
		; Initialize TekBot Forward Movement
		ldi		mpr, MovFwd		; Load Move Forward Command
		out		PORTB, mpr		; Send command to motors

		; set lCount and rCount to $00 by first loading $00 into mpr then into lcount and rcount
		ldi		mpr, $00
		mov		lCount, mpr
		mov		rCount, mpr

		; Initialize LCD Display by calling the LCDInit function
		rcall LCDInit ; Initialize LCD peripheral interface

		rcall LCDClr ; clear because it was outputting text at start

		; load and store rCount to the LCD screen !!!!!!!!!!!!!!!!!!!!!!!!!!!
		; first set up the X register to point to the LCD screen memory: 
		ldi		XL, low(lStart)
		ldi		XH, high(lStart)

		; copy lCount to mpr then use Bin2ASCII function
		rcall	Bin2ASCII

		; load and store rCount to the LCD screen !!!!!!!!!!!!!!!!!!!!!!!!!!!
		; first set up the X register to point to the LCD screen memory: 
		ldi		XL, low(rStart)
		ldi		XH, high(rStart)

		; copy lCount to mpr then use Bin2ASCII function
		rcall	Bin2ASCII

		; write 0s to the LCD screen
		rcall	LCDWrite

		; Turn on interrupts
		; NOTE: This must be the last thing to do in the INIT function
		sei		; enable interrupts
		; cli disables interrupts


;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		; TODO: not much since we are doing interrupts.

		rjmp	MAIN			; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
;	You will probably want several functions, one to handle the
;	left whisker interrupt, one to handle the right whisker
;	interrupt, and maybe a wait function
;------------------------------------------------------------

;-----------------------------------------------------------
; Func: HitLeft
; Desc: This function will handle the situation where the INT1 external interrupt is detected,
;		causing the bumpbot to reverse for 1 second and then turn right for a second, then move forwards again
;-----------------------------------------------------------
HitLeft:							; Begin a function with a label
		
		; by this point, the processor has already pushed pc+1 onto the stack and set SREG I-flag to 1, which prevents any other interrupts
		; Save variable by pushing them to the stack
		push	mpr
		push	waitcnt
		push	ilcnt
		push	olcnt ; push all registers onto the stack
		
		; now we have to do the actual bumpbot behavior: 

		; add one to the left counter
		inc		lCount

		; load and store lCount to the LCD screen !!!!!!!!!!!!!!!!!!!!!!!!!!!
		; first set up the X register to point to the LCD screen memory: specifically start out at $0104 for ls character
		ldi		XL, low(lStart)
		ldi		XH, high(lStart)

		; clear the data memory
		rcall LCDClr

		; copy lCount to mpr then use Bin2ASCII function
		mov		mpr, lCount
		rcall	Bin2ASCII

		; load and store rCount to the LCD screen !!!!!!!!!!!!!!!!!!!!!!!!!!!
		; first set up the X register to point to the LCD screen memory: specifically start out at $0108 for ms character
		ldi		XL, low(rStart)
		ldi		XH, high(rStart)

		; copy lCount to mpr then use Bin2ASCII function
		mov		mpr, rCount
		rcall	Bin2ASCII

		; now, data memory of the LCD screen pointed to by X holds the ASCII equivalent of mpr
		; call the LCDWrite function to put the numbers on the LCD screen
		rcall LCDWrite

		; this part copied from Lab 1
		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Turn right for a second
		ldi		mpr, TurnR	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Move Forward again
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		; clear queued interrupts
		ldi		mpr, 0b00001011
		out		EIFR, mpr

		; Restore variable by popping them from the stack in reverse order
		pop		olcnt
		pop		ilcnt
		pop		waitcnt
		pop		mpr

		; processor automatically enables interrupts by setting I flag in SREG
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: HitRight
; Desc: This function will handle the situation where the INT0 external interrupt is detected,
;		causing the bumpbot to reverse for 1 second and then turn left for a second, then move forwards again
;-----------------------------------------------------------
HitRight:							; Begin a function with a label
		
		; by this point, the processor has already pushed pc+1 onto the stack and set SREG I-flag to 1, which prevents any other interrupts
		; Save variable by pushing them to the stack
		push	mpr
		push	waitcnt
		push	ilcnt
		push	olcnt ; push all registers onto the stack
		
		; now we have to do the actual bumpbot behavior: 

		; add one to the right counter
		inc		rCount

		; load and store rCount to the LCD screen !!!!!!!!!!!!!!!!!!!!!!!!!!!
		; first set up the X register to point to the LCD screen memory: specifically start out at $0108 for ms character
		ldi		XL, low(rStart)
		ldi		XH, high(rStart)

		; clear the lcd screen
		rcall	LCDClr

		; copy lCount to mpr then use Bin2ASCII function
		mov		mpr, rCount
		rcall	Bin2ASCII

		; load and store lCount to the LCD screen !!!!!!!!!!!!!!!!!!!!!!!!!!!
		; first set up the X register to point to the LCD screen memory: specifically start out at $0104 for ls character
		ldi		XL, low(lStart)
		ldi		XH, high(lStart)

		; copy lCount to mpr then use Bin2ASCII function
		mov		mpr, lCount
		rcall	Bin2ASCII

		; now, data memory of the LCD screen pointed to by X holds the ASCII equivalent of mpr
		; call the LCDWrite function to put the numbers on the LCD screen
		rcall LCDWrite

		; this part copied from Lab 1
		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Move Forward again
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		; clear queued interrupts
		ldi		mpr, 0b00001011
		out		EIFR, mpr

		; Restore variable by popping them from the stack in reverse order
		pop		olcnt
		pop		ilcnt
		pop		waitcnt
		pop		mpr

		; processor automatically enables interrupts by setting I flag in SREG
		ret						; End a function with RET


;-----------------------------------------------------------
; Func: Clear
; Desc: This function will handle the situation where the INT3 external interrupt is detected,
;		causing LCD screen counters to be cleared and set to 0
;-----------------------------------------------------------
Clear:							; Begin a function with a label
		
		; load 0 into mpr, then store mpr to rCount and lCount
		ldi		mpr, $00

		; copy mpr to rCount and lCount
		mov		rcount, mpr
		mov		lcount, mpr

		; clear the lcd screen
		rcall LCDClr

		; now do LCD stuff: 
		; set lcount to 0 on LCD screen
		; first set up the X register to point to the LCD screen memory: specifically start out at $0104 for ms character
		ldi		XL, low(lStart)
		ldi		XH, high(lStart)

		; copy lCount to mpr then use Bin2ASCII function
		rcall	Bin2ASCII

		; now do the same thing for the rcount number on the LCD screen
		ldi		XL, low(rStart)
		ldi		XH, high(rStart)

		rcall	Bin2ASCII

		; now, data memory of the LCD screen pointed to by X holds the ASCII equivalent of mpr
		; call the LCDWrite function to put the numbers on the LCD screen
		rcall LCDWrite

		; clear queued interrupts
		ldi		mpr, 0b00001011
		out		EIFR, mpr

		ret



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

Loop:	ldi		olcnt, 224		; load olcnt register
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


.include "LCDDriver.asm"		; Include the LCD Driver

; if you don't have a newline then this include statement is ignored

