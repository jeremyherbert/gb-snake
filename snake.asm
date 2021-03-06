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

SnakePieces:		ds 2*100	; snake blocks; 2 bytes per piece (x and y coords), 60 pieces max
SnakePiecesEnd:

SnakeHead:		ds 2		; snake head pointer
SnakeTail:		ds 2		; snake tail pointer

SnakeDirection:		ds 1		; 00 -> up, 01 -> right, 02-> down, 03 -> left
SnakeNextDirection:	ds 1		; allows us to change the direction of the snake while it is not moving

SnakeShouldGrow:	ds 1		; should the snake grow ?
SnakeShouldMove:	ds 1		; triggered by the timer, allows for movement in the main code loop

NumberOfFoods:		ds 1		; number of food objects that exist (as sprites)

TimerTicks:		ds 1		; count the number of ticks in the timer (to slow down the movement speed)

LFSRSeed:		ds 1		; seed value for our lfsr

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

	; we also need to write the SnakePieces to FFs
	ld a, $FF
	ld hl, SnakePieces
	ld bc, SnakePiecesEnd-SnakePieces
	call mem_Set

;------------------------------------------------------------------------

; this code displays the splash screen
splash:
	
	; turn on the timer, we need to use its value for our LFSR seed
	ld a, TACF_START | TACF_262KHZ		; make it super fast to stop people gaming it
	ld [rTAC], a

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

	; save our LFSR seed
	ld a, [rTIMA]			; counter value into a
	ld [LFSRSeed], a		; save it!

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
	; and put the head 4 bytes up (2 bytes for x and y coords)
	WRITE_16_WITH_ADD SnakeHead, SnakePieces, 2*2

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

	ld a, 10
	ld [hl+], a			; piece 0, y is also 8

	ld a, 9
	ld [hl+], a			; piece 1, x is also 10

	ld a, 9				; piece 1, y
	ld [hl+], a			

	ld a, 9				; piece 2, x
	ld [hl+], a

	ld a, 8				; piece 2, y
	ld [hl+], a

	; draw our initial snake
	call draw_snake

	; now we start the timer interrupts (ll. 828-852)
	; start the timer, we need this for our LIFR
	ld a, TACF_START | TACF_4KHZ	; turn on, set to 4khz (timer will interrupt every [255 * 1/4000] = 63.75ms)
	ld [rTAC], a

	ld a, IEF_VBLANK | IEF_TIMER | IEF_HILO	; enable vblank, timer, joypad interrupts
	ld [rIE], a			; save the register

;;;;;;;;;;;;;;;;;; DEBUG (make the snake grow by default)
	ld a, 10
	ld [SnakeShouldGrow], a
;;;;;;;;;;;;;;;;;; DEBUG

; finally, our main code loop!
main:
;	halt				; sleep the cpu until an interrupt fires
;	nop				; a bug in the cpu means that the instruction after a halt might get skipped
	ei

	call lfsr_generate		; so we get nice random numbers

	ld a, [GameState]
	cp 2				; does GameState == 2? (the gameover state)
	jr z, gameover			; show a nice message

	call read_joypad		; read the joypad in for movement

	ld a, [SnakeShouldMove]
	or a				; is a 0 ?
	jr z, main			; if it is, skip the move
	
	; but if it isn't
	ld a, 0				; to reset SnakeShouldMove
	ld [SnakeShouldMove], a		; save it
	
	call move_snake			; move the snake!
	call draw_snake

	jp main				; loop

gameover:
	call draw_gameover
	jp main

;	jr main

;------------------------------------------------------------------------
;--------------- 
; lfsr_generate
;	generate a random number and put it in LFSRSeed
;	see http://en.wikipedia.org/wiki/Linear_feedback_shift_register for info on how this works
;	uses taps at 8, 6, 5, 4 
;---------------
lfsr_generate:
	ld a, [LFSRSeed]		; load up the seed
	ld e, a				; e will store our seed value

lfsr_generate_loop:
	ld d, a

	rr d				; roll four times
	rr d
	rr d
	rr d
	xor d				; xor it

	rr d				; five rolls in total
	xor d

	rr d				; up to 6 rolls
	xor d				; xor it in

	rr d
	rr d				; 8 rolls, this is our fourth and last tap
	xor d
	
	; not really sure if secure crypto allows me to do this, but frankly if you would like to game my system then go for it
	ld b, a
	ld a, [rDIV]			; source of some more random
	xor b				; add it in

	cp e
	jr z, lfsr_generate_loop	; make sure it is different

	; save it
	ld [LFSRSeed], a

	ret

;--------------- 
; tail_collision_scan
;	scan the array for any tail collisions 
;---------------
tail_collision_scan:
	push hl
	push bc
	push de
	; first, we grab the snake head data
	LOAD_16_INTO_HL	SnakeHead	; load it

	;ld bc, hl (can't do this, grr nintendo)
	ld a, [hl+]
	ld b, a

	ld a, [hl+]
	ld c, a				; save it so we can use hl

	ld e, SnakePiecesEnd-SnakePieces-2 ; this is the max number of pieces
	
tail_collision_scan_loop:

	; we need to test for array wrapping
	push hl
	push de
	
	ld de, SnakePiecesEnd		; we are going to compare hl
	call subtract_16bit		; hl - de

	jr nc, tail_collision_wrap_ptr

	pop de
	pop hl

	jr tail_collision_skip_wrap

tail_collision_wrap_ptr:
	pop de
	pop hl

	ld hl, SnakePieces 

tail_collision_skip_wrap:

	ld a, [hl+]			; get the x coord

	cp b				; is it the same as the current head x coord

	jr nz, tail_collision_continue_loop	; they didn't match
	jr z, tail_collision_check_y	; the x matched, check y

tail_collision_continue_loop:
	inc hl
	jr tail_collision_dec

tail_collision_check_y:
	ld a, [hl+] 			; load in the y value
	
	cp c				; and is it the same
	jr z, tail_collision_die	; uh oh, snake is dead
	
tail_collision_dec:
	dec e				; decrement e
	dec e				; do it twice because x and y take a byte each
	ld a, e				; 
	cp 0				; see if we are done
	jr nz, tail_collision_scan_loop ; if we aren't, go back

	jr tail_collision_end		; if we get here, there have been no collisions

tail_collision_die:
	ld a, 2
	ld [GameState], a		; game over

tail_collision_end:
	pop de
	pop bc
	pop hl
	ret

;--------------- 
; subtract_16bit
;	implements hl = hl - de
;---------------
subtract_16bit:
	scf			
	ccf				; clear the carry flag

	ld a, l				; grab the lower bit 
	sub e				; and subtract from it
	ld l, a				; save it back

	ld a, h				; load the higher bit
	sbc a, d			; subtract d plus any carry
	ld h, a				; save it

	ret

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
	inc a				; increment and save TimerTicks
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

;------------------------------------------------------------------------

;--------------- 
; read_joypad
;	reads the keys from the joypad into a (ll. 999-1072)
;---------------
read_joypad:
	push bc
	ld a, P1F_5			; select the joypad
	ld [rP1], a			; save it into the register

	ld a, [rP1]			; read the keypress register in
	ld a, [rP1]			; apparently you are supposed to do this a bunch of times to allow for the hardware
	ld a, [rP1]
	ld a, [rP1]
	ld a, [rP1]
	ld a, [rP1]
	
	cpl				; invert all the bits
	ld b, a				; backup our read value into b
	
	; test for the right key
	and $01				; cut out all the bits except the one we want
	cp $01				; and see if ours is left over
	jr z, read_joypad_right		; if it is, the button has been pressed
read_joypad_right_return:		; need to jump back so we can detect all key presses (ie up and right, etc)

	; test for the left key
	ld a, b
	and $02
	cp $02
	jr z, read_joypad_left
read_joypad_left_return:

	; test for up key
	ld a, b
	and $04
	cp $04
	jr z, read_joypad_up
read_joypad_up_return:

	; test for down key
	ld a, b
	and $08
	cp $08
	jr z, read_joypad_down
read_joypad_down_return:
	
	pop bc
	;jp read_joypad_end		; we are done here!
	ret				; all done

	; the major problem here is that we don't want the snake to be able to move back in on itself.
	; we need to test against SnakeDirection to make sure that this won't happen
read_joypad_up:
	ld a, [SnakeDirection]
	
	cp a, 2				; is the snake moving down?
	jr z, read_joypad_up_return	; if it is, we don't update the direction

	; but if it isn't, write the new direction in
	ld a, 0				; up = 0
	ld [SnakeNextDirection], a	; write it!

	jp read_joypad_up_return	; we are done here!
	
read_joypad_right:			; these follow the same format as above, see the comments there for explanations
	ld a, [SnakeDirection]

	cp a, 3				; is the snake moving left?
	jr z, read_joypad_right_return
	
	ld a, 1				; right = 1
	ld [SnakeNextDirection], a

	jp read_joypad_right_return

read_joypad_down:
	ld a, [SnakeDirection]

	cp a, 0				; is the snake moving up?
	jr z, read_joypad_down_return
	
	ld a, 2				; down = 3
	ld [SnakeNextDirection], a

	jp read_joypad_down_return

read_joypad_left:
	ld a, [SnakeDirection]

	cp a, 1				; is the snake moving left?
	jr z, read_joypad_left_return
	
	ld a, 3				; right = 1
	ld [SnakeNextDirection], a

	jp read_joypad_left_return

;--------------- 
; draw_gameover
;	draws the gameover message
;---------------
draw_gameover:
	push af
	push hl
	push de

	; time to copy our gameover tiles to the screen
	; using: mem_CopyVRAM (hl->pSource, de->pDest, bc->byte count)
	ld hl, gameover_text
	ld de, _SCRN0+SCRN_VY_B*8+5 ; 3 lines down, 4 along
	ld bc, gameover_text_end-gameover_text
	call mem_CopyVRAM

	pop de
	pop hl
	pop af
	ret
	
;--------------- 
; move_snake
;	moves the snake one step forward by SnakeNextDirection (and grows if SnakeShouldGrow is set)
;---------------
move_snake:
	di				; we don't want any joypad or timer interrupts going off during this
	
	; first let's update SnakeDirection
	ld a, [SnakeNextDirection]
	ld [SnakeDirection], a

	LOAD_16_INTO_HL SnakeHead	; put the pointer to SnakeHead into hl

	ld a, [hl+]			; grab the x coord
	ld d, a

	ld a, [hl+]			; and now grab the y coord
	ld e, a

	ld a, [SnakeNextDirection]
	
	cp 0				; compare a with 0
	jr z, move_snake_up		; if they are the same, we want to move the snake upwards
	
	cp 1				; these are all the same as above but with different jump targets
	jr z, move_snake_right		; this is equivalent to a C switch() statement

	cp 2
	jr z, move_snake_down

	cp 3
	jr z, move_snake_left

	
move_snake_up:
	; we are moving up, so decrement the y coord
	dec e

	ld a, e
	cp 255				; if the register overflowed
	jp z, move_snake_die		; snake is dead!

	jp move_snake_write		; otherwise write our new snake block

move_snake_right:
	; we are moving right, so increment the x coord
	inc d

	ld a, d				; put d into a so we can compare with it
	cp 20				; is the coord 20 ?
	jp z, move_snake_die		; if it is, snake is dead!
	
	jp move_snake_write

move_snake_down:
	; we are moving down, so increment the y coord
	inc e

	ld a, e				; put e into a so we can compare
	cp 18				; is the y coord 18
	jp z, move_snake_die		; if it is, snake is dead!

	jp move_snake_write

move_snake_left:
	; we are moving left, so decrement the x coord
	dec d
	
	ld a, d
	cp 255				; did we get an overflow ?
	jp z, move_snake_die		; we are off the screen, so die
	jp move_snake_write

move_snake_write:
	; we need to test if hl is larger than SnakePiecesEnd
	push hl
	push de

	ld de, SnakePiecesEnd
	
	
	call subtract_16bit		; 16 bit subtract, hl = hl - bc
					; larger hl means positive result
	
	pop de				; restore de
	
	; if we have an underflow (ie result is negative), the carry will be set
	; if the carry is not set, then the result was positive and we need to fix the address (because our hl is out of bounds)
	jr nc, move_snake_wrap_ptr
	
	pop hl
	jp move_snake_continue

move_snake_wrap_ptr:
	pop hl				; needed for stack sync
	ld hl, SnakePieces		; load in the start address of snake pieces to wrap the scan
	
move_snake_continue:

	WRITE_16_WITH_HL SnakeHead

	; d and e have been updated with our new values, so lets put them in memory
	ld a, d
	ld [hl+], a
	
	ld a, e
	ld [hl+], a

	; we have moved the head up two blocks, so we need to update the pointer
	LOAD_16_INTO_HL SnakeHead

	; now we need to test if the snake should grow
	ld a, [SnakeShouldGrow]
	cp 0				; should the snake stay the same length?
	jr z, move_snake_cut_tail	; if SnakeShouldGrow == 0, don't grow

	; if we get here, the snake needs to grow
	; to do this, we are going to skip the tail erase and reset our grow flag
	dec a
	ld [SnakeShouldGrow], a		; write it to zero
	ld a, [SnakeLength]		; load in the snake length
	inc a				; crank it up
	ld [SnakeLength], a		; and save

	call tail_collision_scan	; scan for tail collision

	jp move_snake_end		; skip the tail shift

move_snake_cut_tail:
	; erase the old tail piece that we don't want anymore
	LOAD_16_INTO_HL SnakeTail
	push hl				; save the pointer

	; check if SnakeTail is out of bounds
	ld de, SnakePiecesEnd
	call subtract_16bit		; do the subtract

	jr nc, move_snake_cut_tail_wrap_ptr ; if we are out of bounds

	pop hl
	jp move_snake_cut_tail_continue

move_snake_cut_tail_wrap_ptr:
	pop hl
	ld hl, SnakePieces

move_snake_cut_tail_continue:

	ld a, [hl]			; load x
	ld d, a
	ld a, $FF
	ld [hl+], a			; erase (to fix the collision detect)

	ld a, [hl]			; load y
	ld e, a	
	ld a, $FF
	ld [hl+], a			; erase 

	; we need to save the new snake tail pointer
	WRITE_16_WITH_HL SnakeTail

	call tail_collision_scan	; scan for tail collision


	ld a, [GameState]
	cp 2				; if GameState == 2
	jr z, move_snake_end		; snake died

	call convert_xy_to_screen_addr	; convert hl to a memory address on the screen

	ld a, 32			; ASCII blank space
	ld bc, 1			; only set one byte
	call mem_SetVRAM		; write it
	
	jp move_snake_end

move_snake_die:
	ld a, 2				; this is the game state for dead
	ld [GameState], a		; write it

move_snake_end:
	ei				; turn interrupts back on
	ret

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
	di				; we don't want a vblank occuring while we are doing this

	; if we reach the bound, we need to start hl again at the end
	ld de, SnakePiecesEnd		; load in the address on the back end
	push hl

	call subtract_16bit		; 16 bit subtract

	; if the carry flag is not set, the result is positive and hl is out of bounds
	jr nc, draw_snake_wrap_ptr
	
	pop hl				; restore our old hl
	jp draw_snake_continue		; if we get here, we don't need to correct anything

draw_snake_wrap_ptr:
	pop hl				; needed so that stack stays in sync
	ld hl, SnakePieces		; load the address of the starting point into the block

draw_snake_continue:

	ld a, [hl+]			; get the first byte (this should be the x coord) and put it in d
	ld d, a
	ld a, [hl+]			; and the next should be the y coord and it goes in e
	ld e, a

	; de have the x and y coords, let's get the pointer to the bg tile into hl
	push hl				; save hl first
	push bc				; and bc

	; using: convert_xy_to_screen_addr (d->xCoord, e->yCoord): hl->pBgTile
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
	add hl, de			; 16 bit add again

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

gameover_text:
	db	"Game over!"
gameover_text_end:

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


