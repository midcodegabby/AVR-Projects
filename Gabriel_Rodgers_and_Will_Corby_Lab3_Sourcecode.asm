;
; Gabriel_Rodgers_and_Will_Corby_Lab3_sourcecode.asm
;
; Created: 2/2/2024 12:01:55 PM
; Author : Gabriel Rodgers
;***********************************************************
;*	This is the code for Lab 3 of ECE 375
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register is required for LCD Driver
.def	waitcnt = r17			; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter
.def	temp = r23			; temporary register

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

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
		; initialize the buttons (PDx, 4 <= x <= 7)
		ldi		mpr, $00		; Set Port D Data Direction Register (inputs)
		out		DDRD, mpr		; for input (ddr is setting input or output, for 0 it is set as input)
		ldi		mpr, $FF		; Initialize Port D Data Register as pull down or active low
		out		PORTD, mpr		; so all Port D inputs are Tri-State (pull down)

		; Initialize LCD Display by calling the LCDInit function
		rcall LCDInit ; Initialize LCD peripheral interface

		rcall LCDClr ; clear because it was outputting text at start

		; NOTE that there is no RET or RJMP from INIT,
		; this is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:					
		
		in		mpr, PIND		; get inputs from PIND

		; this section is for checking for a clear input (PD4 has been inputted)

		; check mpr against 0b10110000:
		; if the x-bit is a zero in mpr (input found for the xth bit), then that means that the 
		; x-bit result stored in mpr after performing a logical AND with 0b10110000 will also be a zero,
		; meaning that an input has been found if its bit in mpr is a zero.
		andi	mpr, 0b10110000 ; perform the logical and between mpr and its mask
		; if mpr[x-bit] = 1, then that means there was no input for the xth bit

		; compare mpr to 0b00010000: checking for a clear input: 
		; if mpr has only the 4th bit ON (is a zero), then this means that the result of cpi will be zero
		; and thus Z flag in SReg will be on. 
		cpi		mpr, 0b10100000	; Check for clear input

		; if z flag was not set: want to go to diff section as there was no clear input
		brne	WRITE	; if Z flag has not been set, go to WRITE section
		rcall	LCDClr	; Call the clear function if mpr received a clear input

		rjmp MAIN		; loop back to check for inputs again

WRITE:

		; this section is for checking for a write input (PD5 inputted)
		; use MOVE and WRITE functions

		; compare mpr to 0b00100000: checking for a write input: 
		; if mpr has only the 5th bit ON (is a zero), then this means that the result of cpi will be zero
		; and thus Z flag in SReg will be on. 
		cpi		mpr, 0b10010000	; Check for write input

		; if z flag was not set: want to go to diff section as there was no write input
		brne	ROTATE	; if Z flag has not been set, go to ROTATE section

		; move the string from program memory to data memory
		rcall MOVE ; call the move function

		; write the stuff to the LCDWrite function
		rcall LCDWrite ; MOVE already put the string in the data memory, so this does not need
						; any inputs

		rjmp MAIN		; loop back to check for inputs again

ROTATE: 

		; this section is for checking for a scroll input (PD7 inputted)

		; if PD6 has input: 
		; use SCROLL function
		
		; compare mpr to 0b00110000: checking for a clear input: 
		; if mpr has only the 7th bit ON (is a zero), then this means that the result of cpi will be zero
		; and thus Z flag in SReg will be on. 
		cpi		mpr, 0b00110000	; Check for croll input

		; if z flag was not set: want to go back to main as there was no scroll input
		brne	MAIN	; if Z flag has not been set, go to WRITE section

		; now to actually do the scrolling: 
		rcall	SCROLL		; call the scroll function



		rjmp	MAIN			; jump back to main and create an infinite
								; while loop.  Generally, every main program is an
								; infinite while loop, never let the main program
								; just run off

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: MOVE
; Desc: This function moves data from program memory to the register and then to the data memory
; this function also initializes the Z and Y registers that are used to facilitate the movement
; from program memory to data memory
;-----------------------------------------------------------
MOVE:		

		; initialize the registers		
		; Initialize the Z register to STRING_BEG
		; create a pointer from ZL and ZH to the 16 bits of STRING_BEG
		ldi		ZL, low(STRING_BEG << 1) 
		ldi		ZH, high(STRING_BEG << 1)

		; after this, now we have the Z register initialized to point to the same spot as the 
		; STRING_BEG, and we can then just compare Z to STRING_END location when we are
		; moving the string from progmem to datamem
		; Z POINTS TO THE LOCATION OF STRING_BEG IN THE PROGRAM MEMORY

		; Initialize the Y register to the first data memory adddress for the LCD
		; $0100-$010F is the range of addresses for the first line of the LCD
		; $0110-$011F is the range of addresses for the second line of the LCD
		; load the lower 8 bits of the first line of the LCD data memory into YL
		ldi		YL, low($0100) 
		; load the higher 8 bits of the first line of the LCD data memory into YH
		ldi		YH, high($0100)

		; after this, now the Y register points to the same part of memory as the 
		; first line of the LCD display in data memory. 
		; Y POINTS TO THE LOCATION OF THE LCD FIRST LINE IN DATA MEMORY

		; now we need to copy the string from progmem[z] to mpr to datamem[y]
		; start a while loop
		mloop: 
			; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
			lpm		mpr, Z+ ; post increment Z

			; store mpr to datamem[y], then increment y by 1
			; store indirect command because we are using a pointer as an address rather than a constant register
			st		Y+, mpr ; post increment y

			; now we must check both Y and Z for if they reached their respective ends:
			; if ZL != low(STRING_END << 1) then loop again
			; if ZH != high(STRING_END << 1) then loop again

			; compare immediate (cpi, constant) to ZL
			cpi		ZL, low(STRING_END << 1)

			; branch back into the loop if the SReg (status register) does not have the Z (zero) flag set
			brne mloop

			; compare immediate (cpi, constant) to ZH
			cpi		ZH, high(STRING_END << 1)

			; branch back into the loop if the SReg does not have the Z flag set
			brne mloop

		ret						; End a function with RET


;-----------------------------------------------------------
; Func: SCROLL
; Desc: This function scrolls the string from left to right
;-----------------------------------------------------------
SCROLL: 
		
		; this function will scroll the text on both lines of the LCD by one character left to right
		; per 0.25s of pressing

		; wait for 0.25 seconds
		ldi		waitcnt, 13	; load a value of 13 into the waitcnt register
		rcall	Wait			; Call wait function

		; $0100-$010F is the range of addresses for the first line of the LCD
		; $0110-$011F is the range of addresses for the second line of the LCD
		; first initialize the y pointer to point to the end of the string
		ldi		YL, low($011F)
		ldi		YH, high($011F)
		; Now Y is a pointer to the end of the string

		; shift each byte x to x + 1, except for byte 31, which gets shifted to 0.
		; use a loop to shift each byte
		; use mpr to move each byte to the next spot, except 31, which gets held in temp (r20)

		; store the 31st byte to the temp register
		ld		temp, Y 
		dec		YL ; subtract 1 from Y, thereby Y is now pointing to the 30th byte

		sloop: 
			
			; load the 30th byte to mpr
			ld		mpr, Y+ ; now y is the 31st byte

			st		Y, mpr ; store the 31st byte to Y, thereby shifting by one

			subi	YL, $2 ; subtract 2 from Y, thereby Y is now pointing to the 29th byte

			; compare YL to the lowest address for the string
			cpi		YL, low($0100) ; sets Z flag if Y has reached the beginning of the string

			; branch back to the top of sloop if YL has not reached the beginning of the string
			brne	sloop

			; compare YH to the lowest address for the string
			cpi		YH, high($0100)

			; branch back to the top of sloop if YH has not reached the beginning of the string
			brne	sloop

		; out of the loop, we still need to shift the 0th byte to the 1st byte
		; also, Y is currently set to the 0th byte
		ld		mpr, Y+		; Y is now the 1st byte, and mpr holds the 0th byte
		st		Y, mpr		; store the 0th byte to Y
		dec		YL		; subtract 1 from Y, thereby Y is now pointing to the 0th byte again

		; now we can set the 0th byte to the 31st byte which is held in temp
		st		Y, temp		; store the 31st byte to the 0th byte

		; write to the LCD
		rcall LCDWrite 
	
		ret		; end the function with RET

;-----------------------------------------------------------
; Func: Wait
; Desc: waits for 0.25 seconds
;-----------------------------------------------------------
; this function was copied from lab 1
Wait:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

w1loop:	ldi		olcnt, 224		; load olcnt register
w2loop:	ldi		ilcnt, 237		; load ilcnt register
w3loop:	dec		ilcnt			; decrement ilcnt
		brne	w3loop			; Continue Inner Loop
		dec		olcnt			; decrement olcnt
		brne	w2loop			; Continue Outer Loop
		dec		waitcnt			; Decrement wait
		brne	w1loop			; Continue Wait loop

		pop		olcnt			; Restore olcnt register
		pop		ilcnt			; Restore ilcnt register
		pop		waitcnt			; Restore wait register
		ret						; Return from subroutine


;***********************************************************
;*	Stored Program Data
;***********************************************************


;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING_BEG:
.DB		"Gabriel  Rodgers"		; Declaring data in ProgMem: 1st line
.DB		"  Hello  World  "		; Declaring data in ProgMem: 2nd line
STRING_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************

.include "LCDDriver.asm"		; Include the LCD Driver

