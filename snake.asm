; gb snake
; by Jeremy Herbert (jeremyherbert.net)
;
; I've never written any decent sized assembler programs (nor have I ever written gameboy-style Z80 assembly), this was a learning experience so apologies in advance for any terrible code.
; You could also argue that this file is over-commented. Yes, it is. Please see above ^
;
; started Jan 2011
;
; any comments with  ll. 1234-1235 means "see lines 1234-1235 in gbspec.txt"

INCLUDE "gbhw.inc"			; standard header to define the io registers
INCLUDE "ibmpc1.inc"			; nice ascii tileset from devrs.com

; First we set up the IRQs to return back out (since we aren't making use of them) 
SECTION	"Vblank",HOME[$0040]
	jp $FF80		; DMA code
SECTION	"LCDC",HOME[$0048]
	reti
SECTION	"Timer_Overflow",HOME[$0050]
	jp timer_interrupt
SECTION	"Serial",HOME[$0058]
	reti
SECTION	"p1thru4",HOME[$0060]
	reti

SECTION "variables",BSS
variables_start:
OamData:		ds 40*4		; we need this so that it pushes all of the other variables away from our OAM ram bank (40 sprites, 4 bytes each)
GameState:		ds 1		; a variable to hold the current state of the game
GameStateAscii:		ds 1		; the ascii version of GameState

SnakeLength:		ds 1		; the length of the snake
SnakePieces:		ds 2*30		; snake blocks; 2 bytes per piece (x and y coords), 30 pieces max 
SnakeHead:		ds 2		; snake head pointer
SnakeTail:		ds 2		; snake tail pointer
SnakeDirection:		ds 1		; 00 -> up, 01 -> right, 02-> down, 03 -> left

SnakeShouldMove:	ds 1		; triggered by the timer, allows for movement in the main code loop

NumberOfFoods:		ds 1		; number of food objects that exist (as sprites)

TimerTicks:		ds 1		; count the number of ticks in the timer (to slow down the movement speed)

DEBUG_BLANK_DONT_TOUCH: ds 5		; this gets allocated to zeroes, makes it easier to see our ram block in the debugger
variables_end:

; the entry point for all gameboy games is at $0100. We only have enough space for two instructions here, so the standard approach is to nop and jump to our actual code
SECTION	"start",HOME[$0100]
nop
jp	init

; now we include a special header directive
	ROM_HEADER	ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBYTE

; some macros to make video memory operations easier
INCLUDE "memory.asm"

; and some other helper macros
INCLUDE "snake-macros.asm"

; and initialise the ascii tileset
TileData:
	chr_IBMPC1	1,8

;------------------------------------------------------------------------

; this is where the actual code begins
init:
	nop				; everyone seems to put a nop here, who am I to be any different
	di				; disable interrupts since we aren't using them

	; first up, we need to init the DMA code
	;
	; Why are we using DMA? (and what the hell is it?) (ll. 1639-1689)
	;	DMA allows very fast and easy copying of data into the OAM (object attribute memory). The reason we don't just dump our data straight into the OAM memory is because  
	;	if it is currently being read by other parts of the gb then we will do an invalid write and things will break. The DMA code (which we are going to load into HRAM) 
	;	will only run when the memory is not busy. Also, the reason we need to put our code in HRAM (high ram) is because the lower parts of memory are not accessible during a DMA write.
	call setup_dma

	ld sp, $FFFF			; set the stack pointer to RAMEND, the stack will grow toward $0

	ld a, %11100100			; load the palette we want to use into a. 11 (darkest), 10 (high-mid dark), 01 (low-mid dark), 00 (light); (ll. 1692-1704)
	ld [rBGP], a			; set it as the background palette 

	; there are only two sprite palettes on the gameboy (0 and 1), we are going to set both to our standard palette
	ldh [rOBP0], a			; set it as the palette for the first sprite palette
	ldh [rOBP1], a			; (ll. 1706-1720)

	; now we set the background scroll position registers to zero
	ld a, 0		
	ld [rSCX], a
	ld [rSCY], a

	; we can't do anything (well, shouldn't) while the LCD is still updating, so first we turn it off
	call stoplcd

	; now that the LCD isn't updating, we copy our ascii tileset into vram
	; using: mem_CopyMono (hl->pSource, de->pDest, bc->bytecount)
	ld hl, TileData 		; this is the label we set earlier
	ld de, _VRAM			; send to vram
	ld bc, 256*8			; 256 chars, 8 bytes a piece
	call mem_CopyMono

	; clear out the object attribute memory (OAM)
	; using: mem_Set (a->val, hl->pMem, bc->byte count)
	ld a, 0
	ld hl, _RAM			; start of ram
	ld bc, $A0			; the full size of the OAM area: 40 bytes, 4 bytes per sprite
	call mem_Set

	; turn the LCD back on (ll. 1505-1544, gbhw.inc 70-85)
	; the following flags mean:
	;	LCDCF_ON		-> turn the LCD on
	;	LCDCF_BG8000	-> use the area starting at $8000 for the background tiles (same area as the object data; ie ascii)
	;	LCDCF_BG9800	-> use background 0
	;	LCDCF_OBJ8		-> sprite size (8x8)
	;	LCDCF_OBJON		-> sprite display
	;	LCDCF_BGON		-> background on
	ld a, LCDCF_ON | LCDCF_BG8000 | LCDCF_BG9800 | LCDCF_OBJ8 | LCDCF_OBJON | LCDCF_BGON
	ld [rLCDC], a

	; clear the background tiles
	call clear_bg

	; when the gameboy starts, the ram is filled with random data. we need to zero this out on all of our variables
	; using: mem_Set (a->val, hl->pMem, bc->byte count)
init_variables:
	ld a, 0
	ld hl, variables_start
	ld bc, variables_end-variables_start
	call mem_Set

;------------------------------------------------------------------------

; this code displays the splash screen
splash:
	; we are going to copy across the background tiles from the splash_text label
	; using: mem_copyVRAM (hl->pSource, de->pDest, bc->byte count)
	ld hl, splash_text
	ld de, _SCRN0+SCRN_VY_B*6		; this will move the text down one line (we are adding the length of one line of tiles on)
	ld bc, splash_text_end-splash_text	; the length of the BSS block, plus an extra 20 because it makes it easier to see in the debugger
	call mem_CopyVRAM

	; now we are getting into actual game code, so let's start the interrupts again
	ld a, IEF_VBLANK		; enable the vblank interrupt
	ld [rIE], a			; store the setting in the register
	ei				; enable interrupts

	; wait until the A button is pressed (using the P1 register)
splash_wait_for_press:	
	ld a, P1F_4			; set bit 5, this means we are selecting the button keys to be read (ll. 999-1072)
	ld [rP1], a			; write it into the register
	
	ld a, [rP1]			; pull the button data back out of the register into a
	ld a, [rP1]			; ll. 1035 reccommends doing this twice so that the cpu has a few cycles to deal with our earlier write
	and $01				; cut out all the bits except for the LSB (which contains the A button info)
	cp 1
	jr z, splash_wait_for_press	; if the a key has not been pressed, the instruction above will set the zero flag and we will jump back	

;------------------------------------------------------------------------

; ok, time to play the game!
init_game:
	; once the key has been pressed, clear the background tiles
	call clear_bg	

	; update the game state
	ld a, 1
	ld [GameState], a

	; draw the initial snake
init_snake:
	ld hl, SnakePieces		; grab the starting address of our array

	; now we load a pointer to the tail of the snake into SnakeTail
	WRITE_16 SnakeTail, SnakePieces
	; and put the head 6 bytes up (2 bytes for x and y coords
	WRITE_16_WITH_ADD SnakeHead, SnakePieces, 3*2

	ld a, 3
	ld [SnakeLength], a		; set the initial snake length

	; now we need to set the x,y bytes for the three snake pieces we have just created
	; we are going to put them at (9,8), (9,10) and (9,11) 
	; we could do this with mem_Set and db, but I think this better demonstrates how to load stuff into memory
	ld a, [SnakeTail]		; put the address to start writing in hl 
	ld l, a
	ld a, [SnakeTail+1]
	ld h, a

	ld a, 9				; piece 0, x
	ld [hl+], a			; load it in to memory at hl (which is SnakeTail) and then increment hl

	ld a, 8
	ld [hl+], a			; piece 0, y is also 8

	ld a, 9
	ld [hl+], a			; piece 1, x is also 10

	ld a, 9				; piece 1, y
	ld [hl+], a			

	ld a, 9				; piece 2, x
	ld [hl+], a

	ld a, 10			; piece 2, y
	ld [hl+], a

	; now we start the timer interrupts (ll. 828-852)
	ld a, TACF_START | TACF_4KHZ	; turn on, set to 4khz (timer will interrupt every (255 * 1/4000) ~= 63.75ms
	ld [rTAC], a

	ld a, IEF_VBLANK | IEF_TIMER	; enable vblank and timer interrupts
	ld [rIE], a			; save the register

; finally, our main code loop!
main:
;	halt				; sleep the cpu until an interrupt fires
;	nop				; a bug in the cpu means that the instruction after a halt might get skipped
	call draw_snake
	jr main

;------------------------------------------------------------------------

;--------------- 
; timer_interrupt
;	handles the timer overflow interrupt
;---------------
timer_interrupt:
	push af				; save af so we can use it
	; we want to only move the snake every 8 interrupts (~ half a second)
	ld a, [TimerTicks]		; load our tick number in
	cp a, 8				; is a == 8 ?
	jr z, timer_interrupt_equal	; if they are the same, jump

	; or if they aren't the same
	inc a
	ld [TimerTicks], a
	jp timer_interrupt_end
	
timer_interrupt_equal:
	; first let's reset the ticker
	ld a, 0
	ld [TimerTicks], a

	; and tell our main loop that the snake should move
	ld a, 1
	ld [SnakeShouldMove], a
	
timer_interrupt_end:
	pop af				; restore af
	reti				; interrupt escape

;--------------- 
; draw_snake
;	draws all of the snake tiles between SnakeTail and SnakeHead
;---------------
draw_snake:
	push af				; save all of the registers to the stack so we can do what we want with them
	push bc
	push de
	push hl

	; first we want to get SnakeTail. This is the address where we are going to start drawing blocks
	LOAD_16_INTO_HL SnakeTail

	; and we are going to put our SnakePiece counter into b (so that we only draw as many blocks as we need)
	ld a, [SnakeLength]
	ld b, a

	; this is where we draw SnakeLength-many tiles on the bg
draw_snake_loop:
	di
	ld a, [hl+]			; get the first byte (this should be the x coord)
	ld d, a				; put the x shift in e

	ld a, [hl+]			; and the next should be the y coord
	ld e, a

	; de have the x and y coords, let's get the pointer to the bg tile into hl
	push hl				; save hl first
	push bc				; and bc
	call convert_xy_to_screen_addr	

	; we are going to set the background tile to the block character now
	ld a, $DB
	; hl already has the address we want
	ld bc, 1			; only copying 1 byte
	call mem_SetVRAM
	
	pop bc
	pop hl				; now restore hl and bc from before just before the multiply

	dec b

	ld a, b
	cp 0				; set the zero bits if b is zero
	jr nz, draw_snake_loop		; and if it isnt zero, we aren't done drawing yet

	pop hl				; load our registers back out in reverse order
	pop de
	pop bc
	pop af
	
	ei				; enable interrupts again

	ret				; and escape!

;--------------- 
; convert_xy_to_screen_addr
;	takes x and y coordinates and converts them to the memory address of the bg tile
;	using: convert_xy_to_screen_addr (d->xCoord, e->yCoord): hl->pBgTile
;---------------
convert_xy_to_screen_addr:
	; first thing we need to do is get the y offset of the memory
	push de				; save de so we can get the values back later
	
	ld d, SCRN_VY_B			; since e has the y component, load our multiplier into d
	call multiply_to_hl

	; hl now has the y offset, let's add on the x offset
	pop de				; get de back
	ld e, d				; we don't care about the y coord anymore and we need it in the lower byte
	ld d, 0				; zero out d

	add hl, de			; 16 bit add

	; now we add on the screen offset in memory
	ld de, _SCRN0
	add hl, de

	; hl now contains the address of our tile!
	ret

;--------------- 
; clear_bg
;	clears all the background tiles
;---------------
; write ascii blanks to all bg tiles
; using: mem_SetVRAM (a->value, hl->pMem, bc->byte count)
clear_bg:
	push af				; save the registers to the stack
	push hl
	push bc

	ld a, 32
	ld hl, _SCRN0			; Screen0 address
	ld bc, SCRN_VX_B * SCRN_VY_B	; virtual screen width * virtual screen height
	call mem_SetVRAM

	pop bc				; restore the registers (note the opposite order)
	pop hl
	pop af

	ret				; go back to what we were doing

;--------------- 
; splash_text
;	the text to be displayed on the splash screen (rows are 32 chars wide)
;---------------
splash_text:
	db	"       Snake!                   "
	db	"                                "
	db	"  By Jeremy Herbert             "
	db	"   Press A to play              "
splash_text_end:

gamestate_text:
	db "gamestate: "

;--------------- 
; stoplcd
;	a function to wait until the LCD has fully drawn a frame and then turn it off
;---------------
stoplcd:
	ld a, [rLCDC]			; load the lcd control register into a
	rlca				; roll the highest bit into the carry flag. this is the bit that designates on/off (1=on, 0=off) (ll. 1505-1544)
	ret nc				; this is a special return; return only if the carry bit is not set (ie the lcd is already turned off)

	; we are not allowed to turn the lcd off unless we are in V-blank (ll. 1541-1544). gbspec reccommends that you test to see that LY >= 144 (LCD Y coord)
stoplcd_wait:
	ld a, [rLY]			; grab the LY register
	cp 145				; set the carry bit if a > 145 (called the 'borrow', essentially means underflow). essentially do a-145 and the carry bit will be set to 1 if the result is below zero
	jr nc, stoplcd_wait		; if the carry bit hasn't been set, we aren't in V-blank yet so we need to jump back to the wait function

	; now we switch off the lcd
	ld a, [rLCDC]			; put the LCD control reg back in a
	res 7, a			; (re)sets the 7th bit of a to zero
	ld [rLCDC], a			; put our new value back into LCDC

	ret				; all done here!

;--------------- 
; multiply_to_hl
;	implements HL = D * E
;---------------
multiply_to_hl:
	push bc

	; the following code does HL = D * E
	ld hl, 0			; clear hl
	ld a, d				; load e in for multiply
	
	or a				; same as cp 0
	ret z				; if the value is zero, we are done

	ld b, 0				; setup bc for our looped addition
	ld c, e

multiply_to_hl_loop:
	add hl, bc			; add our value onto hl
	dec a				; and loop until a is zero
	jr nz, multiply_to_hl_loop

	pop bc
	ret

;--------------- 
; init_dma
;	copies the dma code to HRAM
;---------------

setup_dma:
	; using: mem_copyVRAM (hl->pSource, de->pDest, bc->byte count)
	ld hl, dma_copy			; address of our dma code in the code block
	ld de, $FF80			; $FF80 is the start of the HRAM (ll. 1669)
	ld bc, dma_copy_end-dma_copy	; length of our dma code block
	call mem_CopyVRAM

	ret				; go back to what we were doing

;--------------- 
; init_dma
;	copies the dma code to HRAM
;---------------
dma_copy:
	push af				; store the old a and status reg (f) on the stack so we can use them for our own purposes
	ld a, $C0			; OamData variable
	ldh [rDMA], a			; once we put this address into the DMA register, the transfer will begin ($A0 bytes from $C000)

	; now we want to delay for 160us while the data gets copied
	; this uses a basic countdown-jump back loop
	ld a, $28			; this is our countdown value ($28 to 0)
dma_copy_wait:
	dec a
	jr nz, dma_copy_wait		; if a is not 0, jump back to the wait loop
	
	; once we are here, the dma is all done
	pop af				; restore af to its old value
	reti				; and go back to what we were doing
dma_copy_end:

;------------------------------------------------------------------------


