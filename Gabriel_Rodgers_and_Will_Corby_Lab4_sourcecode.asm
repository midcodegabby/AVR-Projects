;***********************************************************
;*	This is code file for Lab 4 of ECE 375
;*
;*	 Author: Gabriel Rodgers and Will Corby
;*	   Date: 2/22/2024
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0056					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine

		; Initialize Stack Pointer
		; Initialize the Stack Pointer (VERY IMPORTANT!!!!)
		ldi		mpr, low(RAMEND) ; this limits the stack to RAMEND low
		out		SPL, mpr		; Load SPL with low byte of RAMEND (Stack Pointer Low)
		ldi		mpr, high(RAMEND) ; this limits the stack to RAMEND high
		out		SPH, mpr		; Load SPH with high byte of RAMEND (Stach Pointer High)

		; TODO

		clr		zero			; Set the zero register to zero, maintain
										; these semantics, meaning, don't
										; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program

		; Call function to load ADD16 operands
		rcall ldADD16

		nop ; Check load ADD16 operands (Set Break point here #1)

		; Call ADD16 function to display its results (calculate FCBA + FFFF)
		rcall ADD16

		nop ; Check ADD16 result (Set Break point here #2)


		; Call function to load SUB16 operands
		rcall ldSUB16

		nop ; Check load SUB16 operands (Set Break point here #3)

		; Call SUB16 function to display its results (calculate FCB9 - E420)
		rcall SUB16

		nop ; Check SUB16 result (Set Break point here #4)

		; Call function to load MUL24 operands
		rcall ldMUL24

		nop ; Check load MUL24 operands (Set Break point here #5)

		; Call MUL24 function to display its results (calculate FFFFFF * FFFFFF)
		rcall MUL24

		nop ; Check MUL24 result (Set Break point here #6)

		; Setup the COMPOUND function direct test
		rcall ldCOMPOUND

		nop ; Check load COMPOUND operands (Set Break point here #7)

		; Call the COMPOUND function
		rcall COMPOUND

		nop ; Check COMPOUND result (Set Break point here #8)

DONE:	rjmp	DONE			; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ldADD16
; Desc: loads the program memory holding the ADD16 operands to the data memory
;
;-----------------------------------------------------------
ldADD16:						
		; Save variable by pushing them to the stack
		push mpr;

		; initialize the registers		
		; Initialize the Z register to OperandA (first operand of ADD16)
		; create a pointer from ZL and ZH to the 16 bits of OperandA in program memory
		ldi		ZL, low(OperandA << 1) 
		ldi		ZH, high(OperandA << 1)

		; Initialize the Y register to the first operand of the ADD16 function in data memory,
		; which is denoted by ADD16_OP1
		; load the lower 8 bits of the first ADD16 operand into YL
		ldi		YL, low(ADD16_OP1) 
		; load the higher 8 bits of the first ADD16 operand into YH
		ldi		YH, high(ADD16_OP1)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the first ADD16 operand is to be stored.

		; now we need to copy the first operand from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z
		st		Y, mpr
		
		; repeat the previous lines for the 2nd operand
		; Initialize the Z register to OperandB (second operand of ADD16)
		; create a pointer from ZL and ZH to the 16 bits of OperandB in program memory
		ldi		ZL, low(OperandB << 1) 
		ldi		ZH, high(OperandB << 1)

		; Initialize the Y register to the second operand of the ADD16 function in data memory,
		; which is denoted by ADD16_OP2
		; load the lower 8 bits of the second ADD16 operand into YL
		ldi		YL, low(ADD16_OP2) 
		; load the higher 8 bits of the second ADD16 operand into YH
		ldi		YH, high(ADD16_OP2)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the second ADD16 operand is to be stored.

		; now we need to copy the second operand from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z
		st		Y, mpr

		pop mpr			; restore mpr to its original state

		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET

		;-----------------------------------------------------------
; Func: ldSUB16
; Desc: loads the program memory holding the SUB16 operands to the data memory
;
;-----------------------------------------------------------
ldSUB16:						
		; Save variable by pushing them to the stack
		push mpr;

		; initialize the registers		
		; Initialize the Z register to OperandC (first operand of SUB16)
		; create a pointer from ZL and ZH to the 16 bits of OperandC in program memory
		ldi		ZL, low(OperandC << 1) 
		ldi		ZH, high(OperandC << 1)

		; Initialize the Y register to the first operand of the SUB16 function in data memory,
		; which is denoted by SUB16_OP1
		; load the lower 8 bits of the first SUB16 operand into YL
		ldi		YL, low(SUB16_OP1) 
		; load the higher 8 bits of the first ADD16 operand into YH
		ldi		YH, high(SUB16_OP1)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the first SUB16 operand is to be stored.

		; now we need to copy the first operand from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z
		st		Y, mpr
		
		; repeat the previous lines for the 2nd operand
		; Initialize the Z register to OperandD (second operand of SUB16)
		; create a pointer from ZL and ZH to the 16 bits of OperandD in program memory
		ldi		ZL, low(OperandD << 1) 
		ldi		ZH, high(OperandD << 1)

		; Initialize the Y register to the second operand of the SUB16 function in data memory,
		; which is denoted by SUB16_OP2
		; load the lower 8 bits of the second SUB16 operand into YL
		ldi		YL, low(SUB16_OP2) 
		; load the higher 8 bits of the second SUB16 operand into YH
		ldi		YH, high(SUB16_OP2)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the second SUB16 operand is to be stored.

		; now we need to copy the second operand from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z
		st		Y, mpr

		pop mpr			; restore mpr to its original state

		ret						; End a function with RET

; Func: ldMUL24
; Desc: loads the program memory holding the MUL24 operands to the data memory
;
;-----------------------------------------------------------
ldMUL24:						
		; Save variable by pushing them to the stack
		push mpr;

		; initialize the registers		
		; Initialize the Z register to OperandE1 (part of first operand of MUL24)
		; create a pointer from ZL and ZH to the 16 bits of OperandE1 in program memory
		ldi		ZL, low(OperandE1 << 1) 
		ldi		ZH, high(OperandE1 << 1)

		; Initialize the Y register to the first operand of the MUL24 function in data memory,
		; which is denoted by MUL24_OP1
		; load the lower 8 bits of the first MUL24 operand into YL
		ldi		YL, low(MUL24_OP1) 
		; load the higher 8 bits of the first MUL24 operand into YH
		ldi		YH, high(MUL24_OP1)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the first 16/24 bits of the MUL24 operand is to be stored.

		; now we need to copy the first operand from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z+
		st		Y+, mpr

		; repeat the loading and storing for the last h 8 bits
		; because E1 and E2 are sequential
		lpm		mpr, Z
		st		Y, mpr

		;NOT NEEDED
		; now we repeat this for the lower 8 bits of OperandE2 (16-23 bits of the first op)
		;ldi		ZL, low(OperandE2 << 1) 
		; load the lower 8 bits of the first MUL24 operand into YL
		;ldi		YL, low(MUL24_OP1) 

		; create a pointer from ZL and ZH to the 16 bits of OperandE1 in program memory
		ldi		ZL, low(OperandF1 << 1) 
		ldi		ZH, high(OperandF1 << 1)

		; repeat the previous lines for the 2nd operand
		; Initialize the Y register to the first operand of the MUL24 function in data memory,
		; which is denoted by MUL24_OP2
		; load the lower 8 bits of the second MUL24 operand into YL
		ldi		YL, low(MUL24_OP2) 
		; load the higher 8 bits of the first MUL24 operand into YH
		ldi		YH, high(MUL24_OP2)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the first 16/24 bits of the MUL24 operand is to be stored.

		; now we need to copy the first operand from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z+
		st		Y+, mpr

		; repeat the loading and storing for the last h 8 bits
		; because E1 and E2 are sequential
		lpm		mpr, Z
		st		Y, mpr

		pop mpr			; restore mpr to its original state

		ret						; End a function with RET


;-----------------------------------------------------------
; Func: ldCOMPOUND
; Desc: loads the program memory holding the COMPOUND operands to the data memory
;
;-----------------------------------------------------------
ldCOMPOUND:						
		; Save variable by pushing them to the stack
		push mpr;

		; initialize the registers		
		; Initialize the Z register to OperandG
		; create a pointer from ZL and ZH to the 16 bits of OperandG in program memory
		ldi		ZL, low(OperandG << 1) 
		ldi		ZH, high(OperandG << 1)

		; Initialize the Y register to SUB16_OP1 of the SUB16_OP1 fn
		; load the lower 8 bits of SUB16_OP1 into YL
		ldi		YL, low(SUB16_OP1) 
		; load the higher 8 bits of  operand into YH
		ldi		YH, high(SUB16_OP1)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the 16 bits of OperandG is to be stored

		; now we need to copy OperandG from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z+
		st		Y+, mpr

		; now we repeat the entire process but this time for OperandH:
		; initialize the registers		
		; Initialize the Z register to OperandH
		; create a pointer from ZL and ZH to the 16 bits of OperandH in program memory
		ldi		ZL, low(OperandH << 1) 
		ldi		ZH, high(OperandH << 1)

		; Initialize the Y register to SUB16_OP2 of the SUB16 function
		; load the lower 8 bits of SUB16_OP2 into YL
		ldi		YL, low(SUB16_OP2) 
		; load the higher 8 bits of the first MUL24 operand into YH
		ldi		YH, high(SUB16_OP2)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the 16 bits of OperandH is to be stored

		; now we need to copy OperandH from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z+
		st		Y+, mpr

		; now we repeat the entire process but this time for OperandI:
		; initialize the registers		
		; Initialize the Z register to OperandI
		; create a pointer from ZL and ZH to the 16 bits of OperandI in program memory
		ldi		ZL, low(OperandI << 1) 
		ldi		ZH, high(OperandI << 1)

		; Initialize the Y register to ADD16_OP1 of the ADD16 fn
		; load the lower 8 bits of COMPOUND_I into YL
		ldi		YL, low(ADD16_OP1) 
		; load the higher 8 bits of the first MUL24 operand into YH
		ldi		YH, high(ADD16_OP1)

		; after this, now the Y register points to the same part of data memory as the 
		; point where the 16 bits of OperandI is to be stored

		; now we need to copy OperandI from progmem[z] to mpr to datamem[y]
		; load program memory in 8 bit chunks from progmem[z] to mpr, then increment z by 1
		lpm		mpr, Z+ ; post increment Z

		; store mpr to datamem[y], then increment y by 1
		; store indirect command because we are using a pointer as an address rather than a constant register
		st		Y+, mpr ; post increment y

		; repeat the loading and storing for the high 8 bits
		lpm		mpr, Z+
		st		Y+, mpr

		; restore initial value of mpr
		pop		mpr

		ret


;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;       where the high byte of the result contains the carry
;       out bit.
;-----------------------------------------------------------
ADD16:

		; save mpr's original value and zeros og val
		push	mpr
		push	zero

		clr		zero

		; Load address of first operand into X
		ldi		XL, low(ADD16_OP1)	; Load low byte of address
		ldi		XH, high(ADD16_OP1)	; Load high byte of address

		; Load address of second operand into Y
		ldi		YL, low(ADD16_OP2)	; Load low byte of address
		ldi		YH, high(ADD16_OP2)	; Load high byte of address

		; Load address of ADD16_Result into Z
		ldi		ZL, low(ADD16_Result) ; Load low byte of address
		ldi		ZH, high(ADD16_Result) ; load high byte of address

		; load contents of ADD16_OP1 (low 8 bits) into R5 then post increment
		ld		R5, X+
		ld		R6, Y+ ; do the same for R6 but of ADD16_OP2

		; add the low 8 bits of both operands together and store the result in R6
		add		R6, R5

		; store the result into the Z register then post increment Z
		st		Z+, R6

		; load contents of ADD16_OP1 (high 8 bits) into R5
		ld		R5, X
		ld		R6, Y ; do the same for R6 but of ADD16_OP2

		; add the high 8 bits of both operands together (with the carry flag) and store result in R6
		adc		R6, R5

		; store the result into the Z register then post increment Z
		st		Z+, R6

		; add with carry an empty register to mpr then store to Z

		clr		R6

		adc		R6, zero
		st		Z, R6

		; restore mpr's and zero's orginal content
		pop zero
		pop mpr
		


		ret						; End a function with RET

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;       result. Always subtracts from the bigger values.
;-----------------------------------------------------------
SUB16:

		; Load address of first operand into X
		ldi		XL, low(SUB16_OP1)	; Load low byte of address
		ldi		XH, high(SUB16_OP1)	; Load high byte of address

		; Load address of second operand into Y
		ldi		YL, low(SUB16_OP2)	; Load low byte of address
		ldi		YH, high(SUB16_OP2)	; Load high byte of address

		; Load address of SUB16_Result into Z
		ldi		ZL, low(SUB16_Result) ; Load low byte of address
		ldi		ZH, high(SUB16_Result) ; load high byte of address

		; load contents of SUB16_OP1 (low 8 bits) into R5 then post increment
		ld		R5, X+
		ld		R6, Y+ ; do the same for R6 but of SUB16_OP2

		; subtract the low 8 bits of both operands together and store the result in R5
		sub		R5, R6

		; store the result into the Z register then post increment Z
		st		Z+, R5

		; load contents of SUB16_OP1 (high 8 bits) into R5
		ld		R5, X
		ld		R6, Y ; do the same for R6 but of SUB16_OP2

		; subtract the high 8 bits of both operands together and store result in R5
		sub		R5, R6

		; store the result into the Z register then post increment Z
		st		Z+, R5
		
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit
;       result.
;-----------------------------------------------------------
MUL24:
;* - Simply adopting MUL16 ideas to MUL24 will not give you steady results. You should come up with different ideas.
		; Execute the function here
		
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of MUL24_OP1
		ldi		YL, low(MUL24_OP1)	; Load low byte
		ldi		YH, high(MUL24_OP1)	; Load high byte

		; Set Z to beginning address of MUL24_Result
		ldi		ZL, low(MUL24_Result)	; Load low byte
		ldi		ZH, high(MUL24_Result); Load high byte

		; Begin outer for loop
		ldi		oloop, 3		; Load counter
MUL24_OLOOP:

		; Set X to beginning address of MUL24_OP2
		ldi		XL, low(MUL24_OP2)	; Load low byte
		ldi		XH, high(MUL24_OP2)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 3		; Load counter

MUL24_ILOOP:
		ld		A, X+			; Get byte of op2
		ld		B, Y			; Get byte of op1
		mul		A, B			; Multiply op2 and op1, and stores the result in r1:r0 (rhi:rlo)
		ld		A, Z+			; Get a result byte from memory and store the content to A
		ld		B, Z+			; Get the next result byte from memory and store the content to B
		add		rlo, A			; rlo <= rlo + A (add the result for the low byte to the same-byte product)
		adc		rhi, B			; rhi <= rhi + B + carry (add the result for the high byte to the same-byte product, with carry)
		ld		A, Z+			; Get the third (highest) byte from the result and post-increment
		adc		A, zero			; Add carry to A (which can cause another carry, which must be added to the fourth byte)

		clr		r7				; clear r7 because we put stuff in r7 and we don't want that to interfere in future loops
		adc		r7, zero		; add the carry from the previous adc to r7
		st		Z, r7			; store the carry to the fourth byte of z
		
		st		-Z, A			; Store the third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1 (increment the pointer); this moves the operations up by one place
		dec		iloop			; Decrement counter
		brne	MUL24_ILOOP 	; Loop if iLoop != 0

		; End inner for loop

		sbiw	ZH:ZL, 2		; Z <= Z - 2 (decrement the pointer); this moves the operations back down by two places
								; because the inner loop runs 3 times, the two decrements of the Z register  effectively 
								; moves the Z register to one place above where it started in the previous loop

		adiw	YH:YL, 1		; Y <= Y + 1 (increment the other variable)
		dec		oloop			; Decrement counter
		brne	MUL24_OLOOP		; Loop if oLoop != 0

		; End outer for loop

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((G - H) + I)^2
;       by making use of SUB16, ADD16, and MUL24.
;
;       D, E, and F are declared in program memory, and must
;       be moved into data memory for use as input operands.
;
;       All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:
		
		; save mpr
		push mpr
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		; call the SUB16 function
		call	SUB16

		; need to set the second operand of ADD16 to the result of SUB16:
		ldi		XL, low(SUB16_Result) 
		ldi		XH, high(SUB16_Result) ; this sets the X register to hold the address of the SUB16_Result

		; need to set Y to ADD16_OP2 location:
		ldi		YL, low(ADD16_OP2) 
		ldi		YH, high(ADD16_OP2) ; this sets the Y register to hold the address of ADD16_OP2

		; now need to copy X to ADD16_OP2 by loading and storing using mpr
		ld 		mpr, X+
		st		Y+, mpr

		; repeat for higher 8 bits
		ld		mpr, X
		st		Y, mpr
		nop
		; call the ADD16 function
		call	ADD16
		nop
		clr		mpr

		; now need to set the result of ADD16 to MUL24_OP1 and MUL24_OP2
		; first load address of ADD16_Result into register X
		ldi		XL, low(ADD16_Result)
		ldi		XH, high(ADD16_Result)

		; need to set Y to MUL24_OP1 location:
		ldi		YL, low(MUL24_OP1) 
		ldi		YH, high(MUL24_OP1) ; this sets the Y register to hold the address of MUL24_OP1

		; need to set r8 to MUL24_OP2 location:
		ldi		ZL, low(MUL24_OP2) 
		ldi		ZH, high(MUL24_OP2) ; this sets the Z register to hold the address of MUL24_OP2

		; now to load and store using mpr:
		ld		mpr, X+		; load 1st byte of ADD16_Result into mpr
		st		Y+, mpr		; store 1st byte of ADD16_Result into Y
		st		Z+, mpr		; store 1st byte of ADD16_Result into Z

		nop 

		; load and store next byte
		ld		mpr, X+
		st		Y+, mpr
		st		Z+, mpr

		; load and store for last byte
		ld		mpr, X
		st		Y, mpr
		st		Z, mpr

		; call the MUL24 function
		call MUL24
		
		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		pop		mpr		; restore mpr's state

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;       A - Operand A is gathered from address $0101:$0100
;       B - Operand B is gathered from address $0103:$0102
;       Res - Result is stored in address
;             $0107:$0106:$0105:$0104
;       You will need to make sure that Res is cleared before
;       calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP 		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: 
; Desc: 
;
;-----------------------------------------------------------
FUNC:						
		; Save variable by pushing them to the stack

		; Execute the function here

		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;*	Do not  section.
;*	Note that each operand is 16 bits.
;***********************************************************
; ADD16 operands
OperandA:
	.DW 0xFCBA
OperandB:
	.DW 0xFFFF

; SUB16 operands
OperandC:
	.DW 0XFCB9
OperandD:
	.DW 0XE420

; MUL24 operands
OperandE1:
	.DW	0XFFFF
OperandE2:
	.DW	0X00FF
OperandF1:
	.DW	0XFFFF
OperandF2:
	.DW	0X00FF

; Compound operands
OperandG:
	.DW	0xFCBA				; test value for operand G
OperandH:
	.DW	0x2022				; test value for operand H
OperandI:
	.DW	0x21BB				; test value for operand I

;***********************************************************
;*	Data Memory Allocation
;***********************************************************
.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.

//ADDITION operands
.org	$011F				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$013F				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result


//SUBTRACTION operands
.org	$015F				; data memory allocation for operands
SUB16_OP1:
		.byte 2				; allocate two bytes for first operand of SUB16
SUB16_OP2:
		.byte 2				; allocate two bytes for second operand of SUB16
			
.org	$017F				; data memory allocation for results
SUB16_Result:
		.byte 2				; allocate two bytes for SUB16 result

//MUL24 operands
.org	$019F				; data memory allocation for operands
MUL24_OP1:
		.byte 3				; allocate three bytes for first operand of MUL24
MUL24_OP2:
		.byte 3				; allocate three bytes for second operand of MUL24

.org	$01BF			; data memory allocation for results
MUL24_Result: 
		.byte 6				; allocate 6 bytes for MUL24 result

//COMPOUND operands
.org	$01DF			; data memory allocation for results
COMPOUND_Result:
		.byte 6				; allocate 6 bytes for results

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program

