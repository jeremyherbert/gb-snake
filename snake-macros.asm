; snake-macros.inc
;
; macros that make coding snake that little bit easier

;--------------- 
; WRITE_16 (MACRO)
;	loads a 16 bit number into memory
;	\1 -> target address
;	\2 -> 16 bit value
;---------------
WRITE_16: MACRO
	push af

	ld hl, \2	; put the 16 bit value into hl
	
	ld a, l		; little endian; lower byte goes first
	ld [\1], a	; and we put it in the target address (we need to use a because there is no "ld nnnn, l" instruction)

	ld a, h
	ld [\1+1], a

	pop af
ENDM
	
;--------------- 
; WRITE_16_WITH_ADD (MACRO)
;	loads a 16 bit number into memory after adding \3. useful for a bit o' pointer arithmetic 
;	\1 -> target address
;	\2 -> 16 bit value
;	\3 -> value to add
;---------------
WRITE_16_WITH_ADD: MACRO
	push af
	
	ld hl, \2	; 16 bit value into hl

	ld a, l		; put the first byte into a
	add a, \3	; add on the value
	ld [\1], a	; save it to memory

	ld a, h		; grab the high bit
	adc a, 0	; we do an add with carry here because the add of the lower bit might have generated a carry
	ld [\1+1], a

	pop af
ENDM

;--------------- 
; WRITE_16_WITH_HL (MACRO)
;	writes a 16 bit number from hl into \1 
;	\1 -> target address
;---------------
WRITE_16_WITH_HL: MACRO
	push af
	push de
	push hl
	
	; we need hl for memory ops, so swap it with de
	; ld de, hl is not allowed so we have to do it manually :(
	ld e, l
	ld d, h

	ld hl, \1	; put our target address into hl	

	; lower byte first
	ld a, e
	ld [hl+], a

	; write the higher byte
	ld a, d
	ld [hl+], a	

	pop hl
	pop de
	pop af
ENDM

;--------------- 
; LOAD_16_INTO_HL (MACRO)
;	loads a 16 bit number from \1 into register hl 
;	\1 -> source address
;---------------
LOAD_16_INTO_HL: MACRO
	push af

	ld a, [\1]	; get the value at address \1
	ld l, a		; put it in l

	ld a, [\1+1]	; and get the higher order byte
	ld h, a		; put it in h

	pop af
ENDM
