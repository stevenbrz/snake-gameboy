INCLUDE "hardware.inc"

; Setup IRQ
SECTION "Timer Interrupt", ROM0[$50]
jp timer_interrupt		

SECTION "Other Interupts", ROM0[$53]
; I don't use any other interrupts so fill with nops
REPT $100 - $53
	db 0
ENDR

SECTION "HEADER", ROM0[$100]
; Documentation suggests to put a nop before the jump to start
nop
jp Start

REPT $150 - $104
db 0
ENDR

SECTION "Program Start", ROM0[$150]

Start:

	di  ; disable interrupts

	; start the timer
	ld a, TACF_START | TACF_4KHZ ; turn on, set to 4khz (timer will interrupt every [255 * 1/4000] = 63.75ms)
	ld [rTAC], a

	ld a, IEF_TIMER ; enable timer interrupt
	ld [rIE], a     ; save the register

	ld sp, $FFFE     ; set the stack to $FFFE

	call lcd_off
	call clear_background

	; Set the background palette
	ld hl, rBGP
	ld [hl], %11100100

	ld hl, Characters ; Load tiles into bg memory
	ld de, $9000
	ld bc, 344 ; number of bytes to move
	call memcpy_monochrome

	ld hl, Border ; Load tiles into bg memory
	ld de, $9300
	ld bc, 56 ; number of bytes to move
	call memcpy_monochrome

	ld hl, snake_ascii
	ld de, $98C7
	ld bc, 5
	call memcpy_ascii
	 
	ld hl, press_start_ascii
	ld de, $9964
	ld bc, 11
	call memcpy_ascii

	ld hl, author_ascii
	ld de, $9A00
	ld bc, 20
	call memcpy_ascii

	ld hl, class_ascii
	ld de, $9820
	ld bc, 20
	call memcpy_ascii
	 
	; turn on the display
	ld hl, rLCDC
	ld [hl], LCDCF_ON | LCDCF_BGON

title_screen_wait:  
	ld a, P1F_4     ; set bit 4, which means read from second column below
	ld [rP1], a     ; write it into the memory
	
	ld a, [rP1] ; read button data multiple times
	ld a, [rP1] ; FF00 -- JOYPAD [RW] Joypad port
	ld a, [rP1] ;       Bit5   Bit4    | In order to scan the keys, output 0 into either Bit4
	ld a, [rP1] ; Bit3  DOWN   START   | or Bit5 of JOYPAD, wait for some time and read JOYPAD.
	ld a, [rP1] ; Bit2  UP     SELECT  | Bits 0-3 will be set to zeroes if corresponding
	ld a, [rP1] ; Bit1  LEFT   B       | buttons are pressed. Bits 6 and 7 are not used. Bits
							; Bit0  RIGHT  A       | 0-3 are connected to input lines P10-P13. Bits 4 and 5
							;                      | are connected to ouput lines P14 and P15.
	cpl
	or a, $F0 ; Set all msb's to 1
	cp a, $F8 ; if a contains 0b1111.1000 -> only start button is pressed
	jp z, start_game ; if the a key has been pressed, the instruction above will set the zero flag and we will start the game
	jp title_screen_wait

start_game:
	
	call lcd_off

	; Set the background palette
	ld hl, rBGP
	ld [hl], %11111100 ; monochrome

	call clear_inner_background
	call draw_border
	call initalize_variables
	call draw_snake
	call add_food
	call draw_food
	call lcd_on
	ei

loop:
	jp loop

timer_interrupt:
	push af
	ld a, [timer_ticks]
	cp 2 ; draw a frame every 3rd interrupt
	jp z, start_timer_interrupt
	inc a
	ld [timer_ticks], a
	pop af
	reti 

start_timer_interrupt:
	di
	ld a, 0
	ld [timer_ticks], a
	pop af
	call lcd_off
	call read_direction
	call set_directions
	call check_bounds
	call clear_inner_background
	call read_direction
	call draw_border
	call read_direction
	call check_food_hit
	call check_increase_length
	call move_snake
	call read_direction
	call draw_food
	call draw_snake
	call read_direction
	call lcd_on
	ei
	reti 

SECTION "Support Routines", ROM0

; For copying tiles that are monochrome
memcpy_monochrome:
	ld a, b
	or c
	ret z
	ld a, [hl+]
	ld [de], a
	inc de
	ld [de], a
	inc de
	dec bc
	jp memcpy_monochrome

; For copying tiles that are ascii characters
memcpy_ascii:
	ld a, b
	or c
	ret z
	ld a, [hl+]
	sub a, $30
	ld [de], a
	inc de
	dec bc
	jp memcpy_ascii

clear_background:
	ld a, $2B  ; the NUL character
	ld l, a
	ld de, $9800   ; we use the background tile map at $9800
	ld bc, 32 * 32 ; the total background area is 32 * 32 bytes
	call mem_set
	ret

clear_inner_background:
	ld a, $2B  ; the NUL character
	ld l, a
	ld de, $9842   
	ld bc, 16 * 8 
	call mem_set
	ld de, $9862
	ld bc, 16 * 8 
	call mem_set
	ld de, $9882
	ld bc, 16 * 8 
	call mem_set
	ld de, $98A2
	ld bc, 16 * 8 
	call mem_set
	ld de, $98C2
	ld bc, 16 * 8 
	call mem_set
	ld de, $9902
	ld bc, 16 * 8 
	call mem_set
	ld de, $9922
	ld bc, 16 * 8 
	call mem_set
	ld de, $9942
	ld bc, 16 * 8 
	call mem_set
	ld de, $9962
	ld bc, 16 * 8 
	call mem_set
	ld de, $9982
	ld bc, 16 * 8 
	call mem_set
	ld de, $99A2
	ld bc, 16 * 8 
	call mem_set
	ld de, $99C2
	ld bc, 16 * 8 
	call mem_set
	ld de, $99E2
	ld bc, 16 * 8 
	call mem_set
	ret

draw_border:
	; Load in border top
	ld a, $34
	ld l, a
	ld de, $9800
	ld bc, 20
	call mem_set

	; Load in second border top
	ld a, $33
	ld l, a
	ld de, $9821
	ld bc, 18
	call mem_set

	; Load in border bottom
	ld a, $34
	ld l, a
	ld de, $9A20
	ld bc, 20
	call mem_set

	; Load in second border bottom
	ld a, $31
	ld l, a
	ld de, $9A01
	ld bc, 18
	call mem_set

	; Load in border left
	ld a, $34
	ld l, a
	ld de, $9820
	ld bc, 16
	call mem_set_vertical

	; Load in second border left
	ld a, $30
	ld l, a
	ld de, $9841
	ld bc, 14
	call mem_set_vertical

	; Load in border right
	ld a, $34
	ld l, a
	ld de, $9833
	ld bc, 16
	call mem_set_vertical

	; Load in second border right
	ld a, $32
	ld l, a
	ld de, $9852
	ld bc, 14
	call mem_set_vertical

	; Fix corners
	ld a, $34
	ld de, $9821
	ld [de], a

	ld de, $9832
	ld [de], a

	ld de, $9A12
	ld [de], a

	ld de, $9A01
	ld [de], a
	ret

lcd_off:
	ld HL, rLCDC
	res 7, [HL]
	ret

lcd_on:
	ld HL, rLCDC
	set 7, [HL]
	ret

; For setting background tiles
mem_set:
	inc bc
 	jp mem_set_check
mem_set_loop:
 	ld a, l
 	ld [de], a
 	inc de
mem_set_check:
 	dec bc
 	ld a, b
 	or c
 	jp nz, mem_set_loop
 	ret

; For setting consective verticle background tiles
mem_set_vertical:
 	inc bc
 	jp mem_set_vertical_check

mem_set_vertical_loop:
	ld a, l
	ld [de], a
	push hl
	push bc
	ld bc, $20
	ld h, d
	ld l, e
	add hl, bc
	ld d, h
	ld e, l
	pop bc
	pop hl

mem_set_vertical_check:
	dec bc
	ld a, b
	or c
	jp nz, mem_set_vertical_loop
	ret

initalize_variables:
	push hl
	ld hl, snake_pos
	ld [hl], $99
	inc hl
	ld [hl], $09
	inc hl
	ld [hl], $99
	inc hl
	ld [hl], $0A
	inc hl
	ld [hl], $99
	inc hl
	ld [hl], $0B
	inc hl
	REPT 512 - 6
	ld [hl], 0
	inc hl
	ENDR
	ld [hl], 3
	inc hl
	ld [hl], 3
	inc hl
	ld [hl], 3
	inc hl
	REPT 256 - 3
	ld [hl], 0
	inc hl
	ENDR
	ld [hl], 3 ; snake len is 3
	inc hl
	ld [hl], 3 ; snake direction is right where 1:up 2:down 3:left 4:right
	inc hl
	ld [hl], 0 ; timer_ticks
	inc hl
	ld [hl], 0 ; length_ticks
	inc hl
	ld [hl], 0 ; snake_directions_addess
	inc hl
	ld [hl], 0 
	inc hl
	ld [hl], 0 ; snake_pos_address
	inc hl
	ld [hl], 0
	inc hl
	ld [hl], 0 ; food_address
	inc hl
	ld [hl], 0
	pop hl
	ret

draw_game_over:
	di
	call clear_background
	ld hl, game_over_ascii
	ld de, $98E5
	ld bc, 9
	call memcpy_ascii

	ld hl, try_again_ascii
	ld de, $9925
	ld bc, 9
	call memcpy_ascii

	ld hl, press_start_ascii
	ld de, $99C4
	ld bc, 11
	call memcpy_ascii
	call lcd_on

game_over_wait:  
	ld a, P1F_4     ; set bit 4, which means read from second column below
	ld [rP1], a     ; write it into the memory
	
	ld a, [rP1] ; read button data multiple times
	ld a, [rP1] ; FF00 -- JOYPAD [RW] Joypad port
	ld a, [rP1] ;       Bit5   Bit4    | In order to scan the keys, output 0 into either Bit4
	ld a, [rP1] ; Bit3  DOWN   START   | or Bit5 of JOYPAD, wait for some time and read JOYPAD.
	ld a, [rP1] ; Bit2  UP     SELECT  | Bits 0-3 will be set to zeroes if corresponding
	ld a, [rP1] ; Bit1  LEFT   B       | buttons are pressed. Bits 6 and 7 are not used. Bits
							; Bit0  RIGHT  A       | 0-3 are connected to input lines P10-P13. Bits 4 and 5
							;                      | are connected to ouput lines P14 and P15.
	cpl
	or a, $F0 ; Set all msb's to 1
	cp a, $F8 ; if a contains 0b1111.1000 -> only start button is pressed
	jp z, start_game
	jp game_over_wait
	

increase_length:
	; save state of registers we will clobber
	push af
	push bc
	push de
	push hl

	; keep old length in b
	ld hl, snake_length
	ld b, [hl]
	dec b

	; length++
	ld a, [hl]
	inc a
	ld [hl], a

	ld hl, snake_pos
	push bc
	call loop_through_hl_2
	pop bc
	ld d, [hl]
	inc hl
	ld e, [hl]
	inc hl
	ld [hl], d
	inc hl
	ld [hl], e

	pop af
	pop bc
	pop de
	pop hl
	ret

loop_through_hl_1:
	inc hl
	dec b
	jp nz, loop_through_hl_1
	ret

loop_through_hl_2:
	inc hl
	inc hl
	dec b
	jp nz, loop_through_hl_2
	ret

check_bounds:
	push hl
	push de

	ld de, snake_pos
	ld a, [de]
	inc de
	cp $9A ; outside bottom border
	jp z, draw_game_over
	cp $99 
	jp z, check_0x99
	cp $98 
	jp z, check_0x98
	jp check_snake
	
check_0x99:
	ld a, [de]
	cp $12
	jp z, draw_game_over
	cp $32
	jp z, draw_game_over
	cp $52
	jp z, draw_game_over
	cp $72
	jp z, draw_game_over
	cp $92
	jp z, draw_game_over
	cp $B2
	jp z, draw_game_over
	cp $D2
	jp z, draw_game_over
	cp $F2
	jp z, draw_game_over
	cp $01
	jp z, draw_game_over
	cp $21
	jp z, draw_game_over
	cp $41
	jp z, draw_game_over
	cp $61
	jp z, draw_game_over
	cp $81
	jp z, draw_game_over
	cp $A1
	jp z, draw_game_over
	cp $C1
	jp z, draw_game_over
	cp $E1
	jp z, draw_game_over
	jp check_snake

check_0x98:
	ld a, [de]
	cp $E1
	jp z, draw_game_over
	cp $C1
	jp z, draw_game_over
	cp $A1
	jp z, draw_game_over
	cp $81
	jp z, draw_game_over
	cp $61
	jp z, draw_game_over
	cp $41
	jp z, draw_game_over
	cp $21
	jp z, draw_game_over
	cp $22
	jp z, draw_game_over
	cp $23
	jp z, draw_game_over
	cp $24
	jp z, draw_game_over
	cp $25
	jp z, draw_game_over
	cp $26
	jp z, draw_game_over
	cp $27
	jp z, draw_game_over
	cp $28
	jp z, draw_game_over
	cp $29
	jp z, draw_game_over
	cp $2A
	jp z, draw_game_over
	cp $2B
	jp z, draw_game_over
	cp $2C
	jp z, draw_game_over
	cp $2D
	jp z, draw_game_over
	cp $2E
	jp z, draw_game_over
	cp $2F
	jp z, draw_game_over
	cp $30
	jp z, draw_game_over
	cp $31
	jp z, draw_game_over
	cp $32
	jp z, draw_game_over
	cp $52
	jp z, draw_game_over
	cp $72
	jp z, draw_game_over
	cp $92
	jp z, draw_game_over
	cp $B2
	jp z, draw_game_over
	cp $D2
	jp z, draw_game_over
	cp $F2
	jp z, draw_game_over
	jp check_snake

check_snake:
	ld hl, snake_length
	ld b, [hl]

	ld hl, snake_pos
	ld a, [hl]
	inc hl
	ld c, [hl]
	inc hl

check_snake_loop:
	push af
	push bc
	ld b, [hl]
	cp a, b
	jp nz, check_snake_loop_continue
	ld a, c
	inc hl
	ld b, [hl]
	cp a, b
	jp z, draw_game_over

check_snake_loop_continue:
	inc hl
	pop bc
	pop af
	dec b
	jp nz, check_snake_loop
	pop hl
	pop de
	ret

add_food:
	push hl
	push de

	ld hl, $FF04
	ld a, [hl]

	and a, $F0
	swap a 

	ld b, a

	ld a, [hl]
	and a, $0F

	; a and b now contain "random" nibbles

get_food_address:
	ld hl, $9842
	cp a, 0
	jp z, add_columns
	dec a
	jp z, add_columns
	dec a
	jp z, add_columns
	dec a
	jp z, add_columns
	dec a
	jp z, add_columns
add_rows:
	push bc
	ld bc, $20
	add hl, bc
	pop bc
	dec a
	jp nz, add_rows
add_columns:
	inc hl
	dec b
	jp nz, add_columns

check_food_address:
	push hl
	ld hl, snake_length
	ld b, [hl]
	pop hl

	ld a, h
	ld c, l

	ld hl, snake_pos

check_food_address_loop:
	push af
	push bc
	ld b, [hl]
	cp a, $9A
	jp z, add_food_reset
	cp a, b
	jp nz, check_food_address_loop_continue
	ld a, c
	inc hl
	ld b, [hl]
	cp a, b
	jp z, add_food_reset
	inc hl
	pop bc
	pop af
	dec b
	jp nz, check_food_address_loop
	jp update_food_address

check_food_address_loop_continue:
	inc hl
	inc hl
	pop bc
	pop af
	dec b
	jp nz, check_food_address_loop

update_food_address:
	ld hl, food_address
	ld [hl], a
	inc hl
	ld [hl], c
	pop de
	pop hl
	ret

add_food_reset:
	pop bc
	pop af
	pop de
	pop hl
	jp add_food

check_food_hit:
	push hl
	push af
	push bc

	ld hl, food_address
	ld a, [hl]
	inc hl
	ld b, [hl]
	ld hl, snake_pos

	ld c, [hl]
	cp a, c
	jp z, continue_check_food_hit
	pop bc
	pop af
	pop hl
	ret

continue_check_food_hit:
	inc hl
	ld c, [hl]
	ld a, b
	cp a, c
	jp z, food_hit
	pop bc
	pop af
	pop hl
	ret

food_hit:
	ld hl, increase_length_next_tick
	ld [hl], 1
	call add_food
	pop bc
	pop af
	pop hl
	ret

check_increase_length:
	push hl
	push af
	ld hl, increase_length_next_tick
	ld a, [hl]
	cp a, 1
	jp nz, check_increase_length_ret
	call increase_length
	ld hl, increase_length_next_tick
	ld [hl], 0

check_increase_length_ret:
	pop af
	pop hl
	ret


draw_snake:
	; save state of registers we will clobber
	push af
	push bc
	push de
	push hl

	ld a, $36
	ld hl, snake_length
	ld b, [hl]
	ld hl, snake_pos

draw_snake_loop:
	ld d, [hl] ; load first half of address into d
	inc hl
	ld e, [hl] ; load second half of address into e
	inc hl

	ld [de], a ; put snake tile in [de]
	
	dec b
	jp nz, draw_snake_loop 

draw_snake_cleanup:
	pop af
	pop bc
	pop de
	pop hl
	ret

draw_food:
	push af
	push de
	push hl
	ld a, $35
	ld hl, food_address
	ld d, [hl] ; load first half of address into d
	inc hl
	ld e, [hl] ; load second half of address into e
	ld [de], a
	pop hl
	pop de
	pop af
	ret

move_snake:
	; save state of registers we will clobber
	push af
	push bc
	push de
	push hl

	; snake_directions_address = &snake_directions
	ld de, snake_directions
	ld hl, snake_directions_address
	ld [hl], d
	inc hl
	ld [hl], e

	; snake_pos_address = &snake_pos
	ld de, snake_pos
	ld hl, snake_pos_address
	ld [hl], d
	inc hl
	ld [hl], e

	; b = snake_length
	ld hl, snake_length
	ld b, [hl]
	ld c, 0

	push bc

	ld hl, snake_directions

move_snake_loop:
	ld a, [hl]
	inc hl

	ld h, d
	ld l, e

	ld d, [hl] ; load first half of address into d
	inc hl
	ld e, [hl] ; load second half of address into e 

	push bc
	ld b, h
	ld c, l

	ld h, d
	ld l, e

	ld d, b
	ld e, c
	pop bc

	; de = snake_pos_address
	; hl = snake_pos
	; a = snake_direction
	
	push af
	dec de
	cp a, 1
	jp z, move_snake_piece_up
	cp a, 2
	jp z, move_snake_piece_down
	cp a, 3
	jp z, move_snake_piece_left
	cp a, 4
	jp z, move_snake_piece_right
	cp a, 0
	jp z, move_snake_continue

	
move_snake_piece_up:
	push bc
	ld bc, -32
	add hl, bc
	pop bc
	ld a, h
	ld [de], a
	ld a, l
	inc de
	ld [de], a
	inc de
	
	jp move_snake_continue

move_snake_piece_down:
	push bc
	ld bc, 32
	add hl, bc
	pop bc
	ld a, h
	ld [de], a
	ld a, l
	inc de
	ld [de], a
	
	inc de
	
	jp move_snake_continue

move_snake_piece_left:
	push bc
	ld bc, -1
	add hl, bc
	pop bc
	ld a, h
	ld [de], a
	ld a, l
	inc de
	ld [de], a
	
	inc de
	
	jp move_snake_continue

move_snake_piece_right:
	push bc
	ld bc, 1
	add hl, bc
	pop bc
	ld a, h
	ld [de], a
	ld a, l
	inc de
	ld [de], a
	
	inc de
	
	jp move_snake_continue

move_snake_continue: 
	pop af
	dec b
	jp z, update_snake_directions

	push de
	push af

	ld hl, snake_directions_address
	ld d, [hl]
	inc hl
	ld e, [hl]
	ld h, d
	ld l, e
	inc hl
	ld a, h
	ld de, snake_directions_address
	ld [de], a
	inc de
	ld a, l
	ld [de], a

	pop af
	pop de

	jp move_snake_loop


update_snake_directions:
	; snake_directions_address = &snake_directions
	ld de, snake_directions
	ld hl, snake_directions_address
	ld [hl], d
	inc hl
	ld [hl], e

	; b = snake_length
	ld hl, snake_length
	ld b, [hl]

	; de = snake_directions
	; hl = snake_directions_address
	; a = snake_direction
	; b = snake_length

update_snake_directions_loop:
	ld c, [hl]
	ld [hl], a
	ld c, a
	dec b
	jp z, move_snake_cleanup
	jp update_snake_directions_loop

move_snake_cleanup:
	pop bc

	ld hl, snake_length
	ld [hl], b

	pop af
	pop bc
	pop de
	pop hl
	ret

read_direction:
	push af
	push hl
	ld a, P1F_5     ; set bit 5, which means read from second column below
	ld [rP1], a     ; write it into the memory
	
	ld a, [rP1] ; read button data multiple times
	ld a, [rP1] ; FF00 -- JOYPAD [RW] Joypad port
	ld a, [rP1] ;       Bit5   Bit4    | In order to scan the keys, output 0 into either Bit4
	;ld a, [rP1] ; Bit3  DOWN   START   | or Bit5 of JOYPAD, wait for some time and read JOYPAD.
	;ld a, [rP1] ; Bit2  UP     SELECT  | Bits 0-3 will be set to zeroes if corresponding
	;ld a, [rP1] ; Bit1  LEFT   B       | buttons are pressed. Bits 6 and 7 are not used. Bits
							; Bit0  RIGHT  A       | 0-3 are connected to input lines P10-P13. Bits 4 and 5
							;                      | are connected to ouput lines P14 and P15.
	cpl
	or a, $F0 ; Set all msb's to 1
	cp a, $F8 ; if a contains 0b1111.1000 -> only down button is pressed
	jr z, set_direction_down
	cp a, $F4 ; if a contains 0b1111.0100 -> only up button is pressed
	jr z, set_direction_up
	cp a, $F2 ; if a contains 0b1111.0010 -> only left button is pressed
	jr z, set_direction_left
	cp a, $F1 ; if a contains 0b1111.0001 -> only right button is pressed
	jr z, set_direction_right
	jp set_direction_default

set_direction_down:
	ld a, 1
	ld hl, snake_direction
	cp a, [hl]
	jp z, set_direction_done
	ld a, 2
	ld [hl], a
	jp set_direction_done

set_direction_up:
	ld a, 2
	ld hl, snake_direction
	cp a, [hl]
	jp z, set_direction_done
	ld a, 1
	ld hl, snake_direction
	ld [hl], a
	jp set_direction_done

set_direction_left:
	ld a, 4
	ld hl, snake_direction
	cp a, [hl]
	jp z, set_direction_done
	ld a, 3
	ld hl, snake_direction
	ld [hl], a
	jp set_direction_done

set_direction_right:
	ld a, 3
	ld hl, snake_direction
	cp a, [hl]
	jp z, set_direction_done
	ld a, 4
	ld hl, snake_direction
	ld [hl], a
	jp set_direction_done

set_direction_default:
	ld hl, snake_direction
	ld a, [hl]
	ld hl, snake_direction
	ld [hl], a

set_direction_done:
	pop hl
	pop af
	ret

set_directions:
	push af
	push bc
	ld hl, snake_direction
	ld a, [hl]
	ld hl, snake_length
	ld b, [hl]
	ld hl, snake_directions
	

set_directions_loop:
	ld c, [hl]
	ld [hl], a
	ld a, c
	inc hl
	dec b
	jp nz, set_directions_loop


set_directions_done:
	pop bc
	pop af
	ret


SECTION "VRAM_TILEMAP", ROM0
; These act like offsets into VRAM tilemap

press_start_ascii:
	DB "PRESS START"

snake_ascii:
	DB "SNAKE"

author_ascii:
	DB "BY STEVEN BRZOZOWSKI"

class_ascii:
	DB "CS 552 FINAL PROJECT"

game_over_ascii:
	DB "GAME OVER"

try_again_ascii:
	DB "TRY AGAIN"

inner_wall:
	DB $34, $34, $34, $34, $34, $34, $34, $34


SECTION "VARIABLES", WRAMX

; Byte array that represents if the snake is present in a given tile or not
snake_pos:
	DS 512

; Byte array that represents each snake segment's previous moves
snake_directions:
	DS 256

snake_length:
	DS 1

snake_direction:
	DS 1

timer_ticks:    
	DS 1  

increase_length_next_tick:   
	DS 1  

snake_directions_address:
	DS 2

snake_pos_address:
	DS 2

food_address:
	DS 2


SECTION "Tiles", ROM0

Characters:
	DB      %01111100
	DB      %11000110
	DB      %11001110
	DB      %11011110
	DB      %11110110
	DB      %11100110
	DB      %01111100
	DB      %00000000

	DB      %00110000
	DB      %01110000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %11111100
	DB      %00000000

	DB      %01111000
	DB      %11001100
	DB      %00001100
	DB      %00111000
	DB      %01100000
	DB      %11001100
	DB      %11111100
	DB      %00000000

	DB      %01111000
	DB      %11001100
	DB      %00001100
	DB      %00111000
	DB      %00001100
	DB      %11001100
	DB      %01111000
	DB      %00000000

	DB      %00011100
	DB      %00111100
	DB      %01101100
	DB      %11001100
	DB      %11111110
	DB      %00001100
	DB      %00011110
	DB      %00000000

	DB      %11111100
	DB      %11000000
	DB      %11111000
	DB      %00001100
	DB      %00001100
	DB      %11001100
	DB      %01111000
	DB      %00000000

	DB      %00111000
	DB      %01100000
	DB      %11000000
	DB      %11111000
	DB      %11001100
	DB      %11001100
	DB      %01111000
	DB      %00000000

	DB      %11111100
	DB      %11001100
	DB      %00001100
	DB      %00011000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %00000000

	DB      %01111000
	DB      %11001100
	DB      %11001100
	DB      %01111000
	DB      %11001100
	DB      %11001100
	DB      %01111000
	DB      %00000000

	DB      %01111000
	DB      %11001100
	DB      %11001100
	DB      %01111100
	DB      %00001100
	DB      %00011000
	DB      %01110000
	DB      %00000000

	DB      %00000000
	DB      %00110000
	DB      %00110000
	DB      %00000000
	DB      %00000000
	DB      %00110000
	DB      %00110000
	DB      %00000000

	DB      %00000000
	DB      %00110000
	DB      %00110000
	DB      %00000000
	DB      %00000000
	DB      %00110000
	DB      %00110000
	DB      %01100000

	DB      %00011000
	DB      %00110000
	DB      %01100000
	DB      %11000000
	DB      %01100000
	DB      %00110000
	DB      %00011000
	DB      %00000000

	DB      %00000000
	DB      %00000000
	DB      %11111100
	DB      %00000000
	DB      %00000000
	DB      %11111100
	DB      %00000000
	DB      %00000000

	DB      %01100000
	DB      %00110000
	DB      %00011000
	DB      %00001100
	DB      %00011000
	DB      %00110000
	DB      %01100000
	DB      %00000000

	DB      %01111000
	DB      %11001100
	DB      %00001100
	DB      %00011000
	DB      %00110000
	DB      %00000000
	DB      %00110000
	DB      %00000000

	DB      %01111100
	DB      %11000110
	DB      %11011110
	DB      %11011110
	DB      %11011110
	DB      %11000000
	DB      %01111000
	DB      %00000000

	DB      %00110000
	DB      %01111000
	DB      %11001100
	DB      %11001100
	DB      %11111100
	DB      %11001100
	DB      %11001100
	DB      %00000000

	DB      %11111100
	DB      %01100110
	DB      %01100110
	DB      %01111100
	DB      %01100110
	DB      %01100110
	DB      %11111100
	DB      %00000000

	DB      %00111100
	DB      %01100110
	DB      %11000000
	DB      %11000000
	DB      %11000000
	DB      %01100110
	DB      %00111100
	DB      %00000000

	DB      %11111000
	DB      %01101100
	DB      %01100110
	DB      %01100110
	DB      %01100110
	DB      %01101100
	DB      %11111000
	DB      %00000000

	DB      %01111110
	DB      %01100000
	DB      %01100000
	DB      %01111000
	DB      %01100000
	DB      %01100000
	DB      %01111110
	DB      %00000000

	DB      %01111110
	DB      %01100000
	DB      %01100000
	DB      %01111000
	DB      %01100000
	DB      %01100000
	DB      %01100000
	DB      %00000000

	DB      %00111100
	DB      %01100110
	DB      %11000000
	DB      %11000000
	DB      %11001110
	DB      %01100110
	DB      %00111110
	DB      %00000000

	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %11111100
	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %00000000

	DB      %01111000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %01111000
	DB      %00000000

	DB      %00011110
	DB      %00001100
	DB      %00001100
	DB      %00001100
	DB      %11001100
	DB      %11001100
	DB      %01111000
	DB      %00000000

	DB      %11100110
	DB      %01100110
	DB      %01101100
	DB      %01111000
	DB      %01101100
	DB      %01100110
	DB      %11100110
	DB      %00000000

	DB      %01100000
	DB      %01100000
	DB      %01100000
	DB      %01100000
	DB      %01100000
	DB      %01100000
	DB      %01111110
	DB      %00000000

	DB      %11000110
	DB      %11101110
	DB      %11111110
	DB      %11111110
	DB      %11010110
	DB      %11000110
	DB      %11000110
	DB      %00000000

	DB      %11000110
	DB      %11100110
	DB      %11110110
	DB      %11011110
	DB      %11001110
	DB      %11000110
	DB      %11000110
	DB      %00000000

	DB      %00111000
	DB      %01101100
	DB      %11000110
	DB      %11000110
	DB      %11000110
	DB      %01101100
	DB      %00111000
	DB      %00000000

	DB      %11111100
	DB      %01100110
	DB      %01100110
	DB      %01111100
	DB      %01100000
	DB      %01100000
	DB      %11110000
	DB      %00000000

	DB      %01111000
	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %11011100
	DB      %01111000
	DB      %00011100
	DB      %00000000

	DB      %11111100
	DB      %01100110
	DB      %01100110
	DB      %01111100
	DB      %01101100
	DB      %01100110
	DB      %11100110
	DB      %00000000

	DB      %01111000
	DB      %11001100
	DB      %11100000
	DB      %01111000
	DB      %00011100
	DB      %11001100
	DB      %01111000
	DB      %00000000

	DB      %11111100
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %00110000
	DB      %00000000

	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %11111100
	DB      %00000000

	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %01111000
	DB      %00110000
	DB      %00000000

	DB      %11000110
	DB      %11000110
	DB      %11000110
	DB      %11010110
	DB      %11111110
	DB      %11101110
	DB      %11000110
	DB      %00000000

	DB      %11000110
	DB      %11000110
	DB      %01101100
	DB      %00111000
	DB      %00111000
	DB      %01101100
	DB      %11000110
	DB      %00000000

	DB      %11001100
	DB      %11001100
	DB      %11001100
	DB      %01111000
	DB      %00110000
	DB      %00110000
	DB      %01111000
	DB      %00000000

	DB      %11111110
	DB      %00000110
	DB      %00001100
	DB      %00011000
	DB      %00110000
	DB      %01100000
	DB      %11111110
	DB      %00000000

Border:
  ; left_border:
	DB      %11111111
	DB      %11111100
	DB      %11111000
	DB      %11111100
	DB      %11111111
	DB      %11111110
	DB      %11111100
	DB      %11111110

  ; bottom_border:
	DB      %10001000
	DB      %10001101
	DB      %11011111
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111

  ; right_border:
	DB      %11111111
	DB      %00111111
	DB      %00011111
	DB      %00111111
	DB      %11111111
	DB      %01111111
	DB      %00111111
	DB      %01111111

  ; top_border:
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11011111
	DB      %10001101
	DB      %10001000

  ; border_filling:
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111
	DB      %11111111

  ; f00d:
	DB      %00000000
	DB      %01111110
	DB      %01100110
	DB      %01011010
	DB      %01011010
	DB      %01100110
	DB      %01111110
	DB      %00000000
  ; snek:
	DB      %11111111
	DB      %11111111
	DB      %11000011
	DB      %11000011
	DB      %11000011
	DB      %11000011
	DB      %11111111
	DB      %11111111
