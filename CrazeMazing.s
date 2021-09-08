; ***************
; * iNES header *
; ***************
;
; The ROM file opens with an iNES header containing info for the emulator.
.segment "HEADER"

INES_MAPPER = 0 ; NROM (Same as all the NES launch games)
INES_MIRROR = 0 ; Mirroring doesn't matter for us (because we're not scrolling)
INES_SRAM   = 0 ; No battery-backed RAM

.byte 'N', 'E', 'S', $1A ; iNES file signature
.byte $02 ; 16k PRG chunk count (2x16k = 32k of PRG ROM)
.byte $01 ; 8k CHR chunk count  (8k of CHR ROM: 4k of Sprite, 4k of Background)
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4) ; $00 for us
.byte (INES_MAPPER & %11110000) ; This also works out to $00 here
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; Padding out the 16 byte header


; ***********
; * CHR ROM *
; ***********
;
; NROM games inlude 8k of graphics CHR ROM
.segment "GRAPHICS"
.incbin "background.chr"    ; 4k for background graphics...
.incbin "sprite.chr"        ; ...and 4k for sprites.


; *****************
; * VECTORS & RAM *
; *****************
;
; The last 6 bytes of PRG memory comprise the 3 vectors.
.segment "VECTORS"
.word nmi
.word reset
.word irq

; Reserve zeropage space for variables
.segment "ZEROPAGE"
random: .res 1 ; random number; gets updated every frame
player1_x: .res 1
player1_y: .res 1
controller1_state: .res 1
tmp: .res 1
x_coord: .res 1
y_coord: .res 1
; mazegen variables
current_cell: .res 1
cell_count: .res 1
tried_count: .res 1
the_maze: .res 120 ; the collision map of the generated maze uses 4 x 30 bytes

; Reserve a page for sprite data
.segment "OAM"
oam: .res 256        ; sprite OAM data

; Reserve a page for cell array for mazegen
.segment "CELLS"
cells: .res 256        ; cell data

; Reserve a page for vertical walls for mazegen
.segment "VWALLS"
vwalls: .res 256        ; vwall data

; Reserve a page for horizontal walls for mazegen
.segment "HWALLS"
hwalls: .res 256        ; hwall data

; ********
; * CODE *
; ********
;
; The codebase resides in PRG ROM (32k available).
.segment "CODE"

; *** Constants ***
PPU_CTRL = $2000 ; Determine NMI, PPU, sprite height, bkg, sprite, etc.
PPU_MASK = $2001 ; Determine color emphasis, sprite/bkg enable, greyscale, etc.
PPU_STATUS = $2002 ; Read current state of PPU
OAM_ADDR = $2003 ; Specify the address in sprite RAM to write data to
OAM_DATA = $2004 ; Read/write to the address specified by $2003
PPU_SCROLL = $2005; Set vert / horiz scrolling
PPU_ADDR = $2006 ; Specify the address in VRAM to write data to
PPU_DATA = $2007 ; Read/write to the address specified by $2006
OAM_DMA = $4014	; Write address from which to upload data into PPU OAM
CONTROLLER_1 = $4016 ; Poll either controller, read controller 1 status
CONTROLLER_2 = $4017 ; Read controller 2 status
CONTROLLER_A      = $01 ; Controller_state holds a byte, each bit of which...
CONTROLLER_B      = $02 ; ...corresponds to one of these specified buttons.
CONTROLLER_SELECT = $04
CONTROLLER_START  = $08
CONTROLLER_UP     = $10
CONTROLLER_DOWN   = $20
CONTROLLER_LEFT   = $40
CONTROLLER_RIGHT  = $80
XSIZEFINAL = #29
YSIZEFINAL = #25
XSIZECELLS = #15
YSIZECELLS = #13
TOTSIZECELLS = #195
XSIZEVERT  = #15
YSIZEVERT  = #12
TOTSIZEVERT = #180
XSIZEHORIZ = #14
YSIZEHORIZ = #13
TOTSIZEHORIZ = #182
SIZEMAZE = #120

; *** NMI ***
nmi:
	; Stash current state of registers
	pha
	txa
	pha
	tya
	pha

	; Write data from oam into the PPU
	ldx #0
	stx OAM_ADDR
	lda #>oam
	sta OAM_DMA

	; Lock scroll at zero.
	lda #%10001000
	sta PPU_CTRL
	lda #0
	sta PPU_SCROLL
	lda #0
    sta PPU_SCROLL

    ; Update the random number.
    jsr rng

    ; Player input
    jsr read_controllers
    lda controller1_state
    and #CONTROLLER_UP
    beq :+
        lda player1_x ; We use a pixel on the middle of the right side of...
        adc #4 ; ... the sprite for rightward collision detection.
        tax
        lda player1_y
        adc #0
        tay
        jsr check_for_collision
        bne up_wall ; If there's a wall in the way, don't move.
            ; *** Lock horiz to 8-px vert stripe
            lda player1_x
            clc
            adc #4
            and #%11111000
            sta player1_x
            ; *** Move up
            dec player1_y
            ; Y values wrap at 240
            lda player1_y
            cmp #240
            bcc :+
                lda #239
                sta player1_y
        up_wall:
    :
    lda controller1_state
    and #CONTROLLER_DOWN
    beq :+
        lda player1_x ; We use a pixel on the middle of the bottom side of...
        adc #4 ; ... the sprite for downward collision detection.
        tax
        lda player1_y
        adc #9
        tay
        jsr check_for_collision
        bne down_wall ; If there's a wall in the way, don't move.
            ; *** Lock horiz to 8-px vert stripe
            lda player1_x
            clc
            adc #4
            and #%11111000
            sta player1_x
            ; *** Move down
            inc player1_y
            ; Y values wrap at 240
            lda player1_y
            cmp #240
            bcc :+
                lda #0
                sta player1_y
        down_wall:
    :
    lda controller1_state
    and #CONTROLLER_LEFT
    beq :+
        lda player1_x ; We use a pixel on the middle of the left side of...
        sbc #0 ; ... the sprite for leftward collision detection.
        tax
        lda player1_y
        adc #4
        tay
        jsr check_for_collision
        bne left_wall ; If there's a wall in the way, don't move.
            ; *** Lock vert to 8-px horiz stripe
            lda player1_y
            clc
            adc #4
            and #%11111000
            tay
            dey
            sty player1_y
            ; *** Move left
            ; X values wrap the same place bytes do, at 256
            dec player1_x
        left_wall:
    :
    lda controller1_state
    and #CONTROLLER_RIGHT
    beq :+
        lda player1_x ; We use a pixel on the middle of the right side of...
        adc #8 ; ... the sprite for rightward collision detection.
        tax
        lda player1_y
        adc #4
        tay
        jsr check_for_collision
        bne right_wall ; If there's a wall in the way, don't move.
            ; *** Lock vert to 8-px horiz stripe
            lda player1_y
            clc
            adc #4
            and #%11111000
            tay
            dey
            sty player1_y
            ; *** Move right
            ; X values wrap the same place bytes do, at 256
            inc player1_x
        right_wall:
    :

	; Restore stashed registers before returning
	pla
	tay
	pla
	tax
	pla
    rti

; *** Reset ***
reset:
    ; setup background
    lda #%00000000    ; Disable everything before loading background
    ; first nametable, start by clearing to empty
    lda PPU_STATUS ; reset latch
    lda #$20
    sta PPU_ADDR
    lda #$00
    sta PPU_ADDR

    ; load hardcoded maze into nametable
    ldy #00 ; 120 bytes per map
    :
        ldx #00 ; 8 bits per byte
        :
            lda hard_maze, y
            and bit_mask, x
            beq hop1
                lda #$31
            hop1:
            bne hop2
                lda #$30
            hop2:
            sta PPU_DATA
            inx
            cpx #8
            bne :-
        iny
        cpy #120
        bne :--
    ; set all attributes to 0
    lda #0
    ldx #64 ; 64 bytes
    :
        sta PPU_DATA
        dex
        bne :-

    lda #%10001000    ; Enable NMI, sprites, and background (table 0)
    sta PPU_CTRL
    lda #%00011110    ; Enable sprites & Backgrounds
    sta PPU_MASK
    lda $00     ; Turn off background scrolling
    sta PPU_ADDR
    sta PPU_ADDR
    sta PPU_SCROLL
    sta PPU_SCROLL

    ; load palettes
    lda #$3F
    sta $2006
    ldx #0
    stx $2006 ; set PPU address to $3F00
    :
        lda palettes, X
        sta $2007
        inx
        cpx #32
        bcc :-

    jmp main

; *** IRQ ***
irq:
	rti

; *** Main ***
main:
    ; initialize main
    lda #$8
    sta player1_x
    lda #$C7
    sta player1_y

    jsr maze_gen
    jsr make_collision_map
    jsr load_maze

@main_loop:
    ; Draw screen
    jsr draw_sprites
    ; Repeat forever
    jmp @main_loop


; *** Generate Maze ***
maze_gen:
    ; First pick a random cell, mark it as part of the maze,
    ; and then add it to the stack:
    lda #$00
    pha                 ; Push 0 to the stack.  We'll use this later to know
        ; that we're done looping through gen_loop
    jsr rng             ; Get a random number.
    and #%01111111      ; Drop it to between 0 and $7F (dec 127) inclusive.
    adc #$20            ; Add twentex (dec 32) to it.
    sta current_cell    ; We'll use that number as our starting cell.
    pha                 ; Push the starting cell to the stack
    tax                 ; Put starting cell in X.
    lda #$01
    sta cell_count      ; Initialize cell count at 1.
    sta cells, x        ; Mark starting cell in cells array with count of 1.
gen_loop:
    pla
    beq make_collision_map  ; If we've popped the 0 off the stack, we're done.
    sta current_cell    ; We got the current cell off the stack.
    ldy XSIZECELLS
    jsr divide  ; Divide takes in the current cell via the accumulator and the
        ; width of the cells array via the X register and gives us back the
        ; x-coordinate of the current cell in the accumulator, and the
        ; y-coordinate in the Y register.
    sta x_coord
    sty y_coord
    lda #$4
    sta tried_count ; We try up to all 4 directions once, and count this down.
    jsr rng         ; Get a random number,
    and #%00000011  ; then drop it to 0-3 to use as a random direction.
    beq dir_up  ; If random number is zero, start with up.
    tax         ; Put random number in X,
    dex         ; subtract one,
    beq dir_down; keep iterating until you've chosen an initial direction.
    dex
    beq dir_left
    dex
    beq dir_right;
dir_up:
    dec tried_count     ; If we've tried all the directions and failed, ...
    beq gen_loop        ; ... give up and go back to the top.
    lda current_cell    ; If currentCell < xSizeCells - 1, we can't go up.
    inc
    sbc XSIZECELLS
    bmi dir_down
    lda current_cell    ; If cellsArray[currentCell - xSizeCells] != 0, ...
    sbc XSIZECELLS      ; ... we can't go up.
    tax
    lda cells, x
    bne dir_down
    inc cell_count
    lda current_cell
    pha ; Push the curret cell to the stack.
    ldy y_coord ; Get ((y_coord-1) * XSIZEVERT) + x_coord into X,...
    dey
    lda XSIZEVERT
    jsr multiply
    adc x_coord
    tax
    lda cell_count      ; ... then remove the vertical wall.
    sta vwalls, x
    lda current_cell    ; Mark the cell as visited with cell_count.
    sbc XSIZECELLS
    tax
    lda cell_count
    sta cells, x
    lda current_cell    ; And update the current cell...
    sbc XSIZECELLS
    sta current_cell    ; ... to be the new one we stepped up to.
    pha current_cell    ; Push the new cell to the stack, ...
    jmp gen_loop        ; ... then head back to the top.
dir_down:
    dec tried_count     ; If we've tried all the directions and failed, ...
    beq gen_loop        ; ... give up and go back to the top.
    lda TOTSIZECELLS    ; If current_cell > TOTSIZECELLS - XSIZECELLS, ...
    sbc XSIZECELLS      ; ... we can't go down.
    sbc current_cell
    bmi dir_left
    lda current_cell    ; If cells[current_cell + XSIZECELLS] != 0, ...
    adc XSIZECELLS      ; ... we can't go down.
    tax
    lda cells, x
    bne dir_left
    inc cell_count
    lda current_cell
    pha ; Push the curret cell to the stack.
    lda XSIZEVERT   ; Get (y_coord * XSIZEVERT) + x_coord into X, ...
    ldy y_coord
    jsr multiply
    adc x_coord
    tax
    lda cell_count  ; ... then remove the vertical wall.
    sta vwalls, x
    lda current_cell    ; Mark the cell as visited with cell_count.
    adc XSIZECELLS
    tax
    lda cell_count
    sta cells, x
    lda current_cell    ; And update the current cell...
    adc XSIZECELLS
    sta current_cell    ; ... to be the new one we've just stepped to.
    pha current_cell    ; Push the new current cell to the stack...
    jmp gen_loop        ; ... and head back to the top.
dir_left:
    dec tried_count     ; If we've tried all the directions and failed, ...
    beq gen_loop        ; ... give up and go back to the top.
    lda x_coord         ; If x_coord == 0, we can't go left.
    beq dir_right
    lda current_cell    ; If cells[current_cell - 1] != 0, we can't go down.
    sbc #1
    tax
    lda cells, x
    bne dir_right
    inc cell_count
    lda current_cell
    pha ; Push the current cell to the stack.
    lda XSIZEHORIZ  ; Get (y_coord * XSIZEHORIZ) + x_coord - 1 into X ...
    ldy y_coord
    jsr multiply
    adc x_coord
    sbc #1
    tax
    lda cell_count
    sta cells, x    ; ... then remove the horizontal wall.
    lda current_cell    ; Mark the cell as visited with cell_count.
    sbc #1
    tax
    lda cell_count
    sta cells, x
    lda current_cell    ; And update the current cell...
    sbc #1
    sta current_cell    ; ... to be the new one we've just stepped to.
    pha current_cell    ; Push the new current cell to the stack...
    jmp gen_loop        ; ... and head back to the top.
dir_right:
    dec tried_count     ; If we've tried all the directions and failed, ...
    beq gen_loop        ; ... give up and go back to the top.
    lda x_coord         ; If x_coord < xSizeCells - 1, we can't go up.
    inc
    sbc XSIZECELLS
    bmi dir_up
    lda current_cell    ; If cells[current_cell + 1] != 0, we can't go up.
    adc #1
    tax
    lda cells, x
    bne dir_up
    inc cell_count
    lda current_cell
    pha ; Push the current cell to the stack.
    lda XSIZEHORIZ  ; Get (y_coord * XSIZEHORIZ) + x_coord into X ...
    ldy y_coord
    jsr multiply
    adc x_coord
    tax
    lda cell_count
    sta cells, x    ; ... then remove the horizontal wall.
    lda current_cell    ; Mark the cell as visited with cell_count.
    adc #1
    tax
    lda cell_count
    sta cells, x
    lda current_cell    ; And update the current cell...
    adc #1
    sta current_cell    ; ... to be the new one we've just stepped to.
    pha current_cell    ; Push the new current cell to the stack...
    jmp gen_loop        ; ... and head back to the top.


; *** Make Collision Map ***
make_collision_map:
    ldx SIZEMAZE ; Start by loading the maze template into the maze.
    :
        lda maze_template, x
        sta the_maze, x
        dex
        bne :-
    ; Then load the vertical walls into the maze.
    lda #0
    tax     ; Init x at 0 as the index into the vwalls,
    sta tmp ; ... and tmp at 0 as a loop counter.
    lxy #16 ; First byte in the maze with vert walls (index into the_maze).
    vert_loop:
        lda vwalls, x
        bne :+
            lda the_maze, y ;
            sbc #%01000000
            sta the_maze, y ;
        :
        inx
        lda vwalls, x
        bne :+
            lda the_maze, y ;
            sbc #%00010000
            sta the_maze, y ;
        :
        inx
        lda vwalls, x
        bne :+
            lda the_maze, y ;
            sbc #%00000100
            sta the_maze, y ;
        :
        inx
        lda tmp
        and #%00000011
        sbc #%00000011
        bne hop1a
        ; If we ain't at the end of the row, also do:
        lda vwalls, x
        bne :+
            lda the_maze, y ;
            sbc #%00000001
            sta the_maze, y ;
        :
        inx
        jmp hop2b
        ; But if we is at the end of the row, do:
    hop1a:
        iny
        iny
        iny
        iny
    hop2b:
        iny
        inc tmp
        ; cpx TOTSIZEVERT
        cpy #108 ; First byte in the maze past the last of the vert walls
        bcc vert_loop; if y < 108, keep looping

    ; Then load the horizontal walls into the maze.
    lda #0
    tax     ; Init x at 0 as the index into the hwalls,
    sta tmp ; ... and tmp at 0 as a loop counter.
    lxy #12 ; First byte in the maze with horiz walls (index into the_maze).
    horiz_loop:
        lda tmp ; If we're at the first byte in the row, skip the first wall.
        and #%11111100
        sbc #%11111100
        bne hop0
        lda hwalls, x
        bne :+
            lda the_maze, y ;
            sbc #%10000000
            sta the_maze, y ;
        :
        inx
    hop0:
        lda hwalls, x
        bne :+
            lda the_maze, y ;
            sbc #%00100000
            sta the_maze, y ;
        :
        inx
        lda hwalls, x
        bne :+
            lda the_maze, y ;
            sbc #%00001000
            sta the_maze, y ;
        :
        inx
        lda tmp
        and #%00000011
        sbc #%00000011
        bne hop1b
        ; If we ain't at the end of the row, also do:
        lda hwalls, x
        bne :+
            lda the_maze, y ;
            sbc #%00000010
            sta the_maze, y ;
        :
        inx
        jmp hop2b
        ; But if we is at the end of the row, do:
    hop1b:
        iny
        iny
        iny
        iny
    hop2b:
        iny
        inc tmp
        ; cpx TOTSIZEVERT
        cpy #112 ; First byte in the maze past the last of the horiz walls
        bcc horiz_loop; if y < 112, keep looping


    ; Pick one of the 13 horiz lines at random; install the exit there.
    rts


; *** LOAD MAZE ***
    ; setup background
    lda #%00000000    ; Disable everything before loading background
    ; first nametable, start by clearing to empty
    lda PPU_STATUS ; reset latch
    lda #$20
    sta PPU_ADDR
    lda #$00
    sta PPU_ADDR
    ; load the_maze maze into ppu nametable
    ldy #00 ; 120 bytes per map
    :
        ldx #00 ; 8 bits per byte
        :
            lda the_maze, y
            and bit_mask, x
            beq hop1c
                lda #$31
            hop1c:
            bne hop2c
                lda #$30
            hop2c:
            sta PPU_DATA
            inx
            cpx #8
            bne :-
        iny
        cpy #120
        bne :--
    ; set all attributes to 0
    lda #0
    ldx #64 ; 64 bytes
    :
        sta PPU_DATA
        dex
        bne :-
    lda #%10001000    ; Enable NMI, sprites, and background (table 0)
    sta PPU_CTRL
    lda #%00011110    ; Enable sprites & Backgrounds
    sta PPU_MASK
    lda $00     ; Turn off background scrolling
    sta PPU_ADDR
    sta PPU_ADDR
    sta PPU_SCROLL
    sta PPU_SCROLL
    rts


; *** RNG ***
; Update the zero-page variable 'random' with a new value; gets called every
; frame when NMI is active.  This is a very basic 8-bit Linear-feedback shift
; register (LFSR).  Note that the newly updated random value is returned from
; the rng function in the accumulator.  X and Y remain untouched.
rng:
        lda random
        asl
        bcc :+
        eor #$CF
:
        sta random
        rts

; *** Read Controllers ***
read_controllers:
    ; latch the buttons
    lda #1
    sta CONTROLLER_1
    lda #0
    sta CONTROLLER_1
    ; read controller 1
    ldx #8
    :
        pha
        lda CONTROLLER_1
        ; combine low two bits and store in carry bit
        and #%00000011
        cmp #%00000001
        pla
        ; rotate carry into controller1_state variable
        ror
        dex
        bne :-
    sta controller1_state
    rts

; *** Draw Sprites ***
; 4 bytes per sprite: y position, which sprite in the sheet, attributes, and x
; position (in that order).  256 bytes per page / 4 per sprite = 64 sprites max
draw_sprites:
    lda player1_y
    sta oam + 0
    lda #$04
    sta oam + 1
    lda #%00000001 ; no flip, pallete 5
    sta oam + 2
    lda player1_x
    sta oam + 3
    rts

; *** Check for Collisions ***
check_for_collision:
    txa ; x / 64
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    sta tmp
    tya ; y / 8
    lsr
    lsr
    lsr
    asl ; * 4
    asl
    clc
    adc tmp
    tay ; byte index
    txa ; x / 8
    lsr
    lsr
    lsr
    and #%0111
    tax ; bit mask index
    lda hard_maze, y
    and bit_mask, x ; Sets zero flag in case of collision...
    rts ; ...and returns.


; *** Multiply ***
; multiply - pass in the multiplicand via the accumulator and the multiplier
; via the Y register.  The product will get returned via the accumulator.
multiply:
sta tmp
loop:
dey
beq done
adc tmp
jmp loop
done:
rts

; *** Divide ***
; divide - pass in the dividend via the accumulator & the divisor via the Y
; register.  The remainder of floor(a/tmp) will get returned via
; the accumulator; the quotient will get returned via Y.
; We're using 1-D sections of memory as 2-D arrays; when we pass in the number
; of a in cell in a 1-D array via the accumulator, and the width of the implied
; 2-D array via the X register, we get back the x-coordinate of that cell in the
; accumulator, and the y-coordinate in the Y register.
divide:
    sty tmp ; Stash Y in tmp.
    ldy #0  ; Reset Y.
    sec     ; Set the carry flag.
:
    sbc tmp ; Subtract the divisor from the dividend.
    bcc :+  ; If you've cleared the carry flag (subtracted below zero), quit.
    iny     ; Otherwise, increse the quotient by 1.
    bne :-  ; If you're not at zero, loop back and subtract some more.
:
    adc tmp
    rts


; *** Hardcoded Palettes ***
palettes:
; background palettes
.byte $38,$09,$1a,$28 ; greens on tan
.byte $0f,$06,$15,$27 ; reds on black
.byte $0f,$01,$1c,$22 ; blues on black
.byte $0f,$2d,$3d,$30 ; greyscale
; sprite palettes
.byte $38,$09,$1a,$28 ; greens on tan
.byte $0f,$06,$15,$27 ; reds on black
.byte $0f,$01,$1c,$22 ; blues on black
.byte $0f,$2d,$3d,$30 ; greyscale

; *** Bit Mask for collision lookup ***
bit_mask:
.byte %10000000
.byte %01000000
.byte %00100000
.byte %00010000
.byte %00001000
.byte %00000100
.byte %00000010
.byte %00000001

; *** Hardcoded Maze Maps (includes base maze template for mazegen) ***
    .include "HardcodedMap.s"

