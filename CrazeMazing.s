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
game_mode: .res 1   ; 0 for title screen, 1 for maze screen
active_maze: .res 1 ; should the players be able to move?
random: .res 1 ; random number; gets updated every frame
player1_x: .res 1
player1_y: .res 1
player2_x: .res 1
player2_y: .res 1
controller1_state: .res 1
controller2_state: .res 1
tmp: .res 1
x_coord: .res 1
y_coord: .res 1
; mazegen variables
current_cell: .res 1
cell_count: .res 1
tried_count: .res 1
maze_bias: .res 1
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
APU_STATUS = $4015  ; Writing 0 bits turns off the various audio channels
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
XSIZEFINAL = 29
YSIZEFINAL = 25
XSIZECELLS = 15
YSIZECELLS = 13
TOTSIZECELLS = 195
XSIZEVERT  = 15
YSIZEVERT  = 12
TOTSIZEVERT = 180
XSIZEHORIZ = 14
YSIZEHORIZ = 13
TOTSIZEHORIZ = 182
SIZEMAZE = 120

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

    ; Draw sprites
    jsr draw_sprites

    ; Player input
    jsr read_controllers
    lda active_maze
    bne yes_active_maze
    jmp no_active_maze
yes_active_maze:
    ; Player 1
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
            ; Lock horiz to 8-px vert stripe
            lda player1_x
            clc
            adc #4
            and #%11111000
            sta player1_x
            ; Move up
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
            ; Lock horiz to 8-px vert stripe
            lda player1_x
            clc
            adc #4
            and #%11111000
            sta player1_x
            ; Move down
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
            ; Lock vert to 8-px horiz stripe
            lda player1_y
            clc
            adc #4
            and #%11111000
            tay
            dey
            sty player1_y
            ; Move left
            ; X values wrap the same place bytes do, at 256
            dec player1_x
        left_wall:
    :
    lda controller1_state
    and #CONTROLLER_RIGHT
    beq :++
        lda player1_x ; We use a pixel on the middle of the right side of...
        adc #8 ; ... the sprite for rightward collision detection.
        tax
        lda player1_y
        adc #4
        tay
        jsr check_for_collision
        bne right_wall ; If there's a wall in the way, don't move.
            ; Lock vert to 8-px horiz stripe
            lda player1_y
            clc
            adc #4
            and #%11111000
            tay
            dey
            sty player1_y
            ; Move right
            ; X values wrap the same place bytes do, at 256
            inc player1_x
            lda player1_x   ; Check for the win.
            cmp #245
            bcc right_wall  ; If we're not out of the maze, keep rolling.
                lda #$00    ; If we're out, turn off movement...
                sta active_maze
                lda #$3F    ; ... and swap bg palette to our own
                sta PPU_ADDR
                lda #$00
                sta PPU_ADDR ; set PPU address to $3F00
                ldx #$04
                :
                    lda palettes, x
                    sta PPU_DATA
                    inx
                    cpx #$08
                    bcc :-
                lda $00     ; Turn off background scrolling
                sta PPU_ADDR
                sta PPU_ADDR
                sta PPU_SCROLL
                sta PPU_SCROLL
        right_wall:
    :
    lda controller1_state
    and #CONTROLLER_A
    beq :+
        ; jmp make_maze ; Uncomment for mazegen testing; just push A to gen maze
    :


    ; Player 2
    lda controller2_state
    and #CONTROLLER_UP
    beq :+
        lda player2_x ; We use a pixel on the middle of the right side of...
        adc #4 ; ... the sprite for rightward collision detection.
        tax
        lda player2_y
        adc #0
        tay
        jsr check_for_collision
        bne up_wall2 ; If there's a wall in the way, don't move.
            ; Lock horiz to 8-px vert stripe
            lda player2_x
            clc
            adc #4
            and #%11111000
            sta player2_x
            ; Move up
            dec player2_y
            ; Y values wrap at 240
            lda player2_y
            cmp #240
            bcc :+
                lda #239
                sta player2_y
        up_wall2:
    :
    lda controller2_state
    and #CONTROLLER_DOWN
    beq :+
        lda player2_x ; We use a pixel on the middle of the bottom side of...
        adc #4 ; ... the sprite for downward collision detection.
        tax
        lda player2_y
        adc #9
        tay
        jsr check_for_collision
        bne down_wall2 ; If there's a wall in the way, don't move.
            ; Lock horiz to 8-px vert stripe
            lda player2_x
            clc
            adc #4
            and #%11111000
            sta player2_x
            ; Move down
            inc player2_y
            ; Y values wrap at 240
            lda player2_y
            cmp #240
            bcc :+
                lda #0
                sta player2_y
        down_wall2:
    :
    lda controller2_state
    and #CONTROLLER_LEFT
    beq :+
        lda player2_x ; We use a pixel on the middle of the left side of...
        sbc #0 ; ... the sprite for leftward collision detection.
        tax
        lda player2_y
        adc #4
        tay
        jsr check_for_collision
        bne left_wall2 ; If there's a wall in the way, don't move.
            ; Lock vert to 8-px horiz stripe
            lda player2_y
            clc
            adc #4
            and #%11111000
            tay
            dey
            sty player2_y
            ; Move left
            ; X values wrap the same place bytes do, at 256
            dec player2_x
        left_wall2:
    :
    lda controller2_state
    and #CONTROLLER_RIGHT
    beq :++
        lda player2_x ; We use a pixel on the middle of the right side of...
        adc #8 ; ... the sprite for rightward collision detection.
        tax
        lda player2_y
        adc #4
        tay
        jsr check_for_collision
        bne right_wall2 ; If there's a wall in the way, don't move.
            ; Lock vert to 8-px horiz stripe
            lda player2_y
            clc
            adc #4
            and #%11111000
            tay
            dey
            sty player2_y
            ; Move right
            ; X values wrap the same place bytes do, at 256
            inc player2_x
            lda player2_x   ; Check for the win.
            cmp #245
            bcc right_wall2 ; If we're not out of the maze, keep rolling.
                lda #$00    ; If we're out, turn off movement...
                sta active_maze
                lda #$3F    ; ... and swap bg palette to our own
                sta PPU_ADDR
                lda #$00
                sta PPU_ADDR ; set PPU address to $3F00
                ldx #$08
                :
                    lda palettes, x
                    sta PPU_DATA
                    inx
                    cpx #$0C
                    bcc :-
        right_wall2:
    :
    jmp restore_return

no_active_maze:
    lda controller1_state
    and #CONTROLLER_A
    beq :+
        jmp make_maze
    :
    lda controller1_state
    and #CONTROLLER_START
    beq :+
        jmp make_maze
    :
    lda controller2_state
    and #CONTROLLER_A
    beq :+
        jmp make_maze
    :
    lda controller2_state
    and #CONTROLLER_START
    beq :+
        jmp make_maze
    :

restore_return:
	; Restore stashed registers before returning
	pla
	tay
	pla
	tax
	pla
    rti

; *** Reset ***
reset:
    jsr turn_stuff_off

    lda #$00  ; clear the NES RAM
    ldx #$00
    :
        sta $0000, x
        sta $0100, x
        sta $0200, x
        sta $0300, x
        sta $0400, x
        sta $0500, x
        sta $0600, x
        sta $0700, x
        inx
        bne :-
    ; Put all the sprites offscreen (Y=$FF is below the screen)
    lda #$FE
    ldx #0
    :
        sta oam, x
        inx
        inx
        inx
        inx
        bne :-
; load title screen palette
    lda #$3F
    sta PPU_ADDR
    lda #$00
    stx PPU_ADDR ; set PPU address to $3F00
    ldx #$08
    :
        lda palettes, x
        sta PPU_DATA
        inx
        cpx #$0C
        bcc :-
; seed random number generator with non-zero number
    lda #$06
    sta random
    jsr rng

; load title screen
    lda #%00000000    ; Disable everything before loading background
    ; first nametable, start by clearing to empty
    lda PPU_STATUS ; reset latch
    lda #$20
    sta PPU_ADDR
    lda #$00
    sta PPU_ADDR
    lda #$FF
    ldx #$00
    :
        sta PPU_DATA
        inx
        bne :-
    :
        lda game_title, x
        sta PPU_DATA
        inx
        cpx #$60
        bne :-
    ldx #$00
    lda #$FF
    :
        sta PPU_DATA
        inx
        bne :-
    :
        lda press_start, x
        sta PPU_DATA
        inx
        cpx #$60
        bne :-
    ldx #$00
    lda #$FF
    :
        sta PPU_DATA
        inx
        bne :-

    lda $00     ; Turn off background scrolling
    sta PPU_ADDR
    sta PPU_ADDR
    sta PPU_SCROLL
    sta PPU_SCROLL
    lda #%10001000    ; Enable NMI, sprites, and background (table 0)
    sta PPU_CTRL
    lda #%00001010    ; Enable sprites & Backgrounds
    sta PPU_MASK
    jmp main_loop

; make maze
make_maze:
    jsr turn_stuff_off  ; Turn NMI, sprites, background and Audio on.
    jsr maze_gen ; Make the walls arrays.
    jsr make_collision_map ; Translate the walls arrays into collision map.
    jsr load_maze   ; Put the collision map into the PPU as the background.
    jsr turn_stuff_on   ; Turn NMI, sprites, background and Audio on.
    lda #$01    ; Make maze active.
    sta active_maze

main_loop:
    jmp main_loop  ; Hold here forever


; *** IRQ ***
irq:
    rti


jmp_make_collision_map: ; beq range hack
    jmp make_collision_map

; *** Generate Maze ***
maze_gen:
    ; Chose the maze bias.
    jsr rng         ; Get a random number,
    sta maze_bias
    ; Reset the cells array to 0s, and the vert & horiz walls arrays to 1.
    ldx #$00
    :
    lda #$00
    sta cells, x
    lda #$01
    sta vwalls, x
    sta hwalls, x
    inx
    bne :-
    ; First pick a random cell, mark it as part of the maze,
    ; and then add it to the stack:
    lda #$FF
    pha                 ; Push ff to the stack.  We'll use this later to know
        ; that we're done looping through gen_loop
    jsr rng             ; Get a random number.
    and #%01111111      ; Drop it to between 0 and $7F (dec 127) inclusive.
    clc
    adc #$20            ; Add twentex (dec 32) to it.
    sta current_cell    ; We'll use that number as our starting cell.
    pha                 ; Push the starting cell to the stack
    tax                 ; Put starting cell in X.
    lda #$01
    sta cell_count      ; Initialize cell count at 1.
    sta cells, x        ; Mark starting cell in cells array with count of 1.

gen_loop:
    pla
    cmp #$FF
    beq jmp_make_collision_map  ; If we popped the 0 off the stack, we're done.
    sta current_cell    ; We got the current cell off the stack.
    ldy #XSIZECELLS
    jsr divide  ; Divide takes in the current cell via the accumulator and the
        ; width of the cells array via the X register and gives us back the
        ; x-coordinate of the current cell in the accumulator, and the
        ; y-coordinate in the Y register.
    sta x_coord
    sty y_coord
    lda #$5
    sta tried_count ; We try up to all 4 directions once, and count this down.
    jsr rng         ; Get a random number,
    cmp maze_bias   ; bias determines likelihood of vert vs. horiz moves
    bcs vert
    jmp horiz

vert:
    jsr rng
    and #$01
    beq dir_up  ; Given vert, up or down starts as 50:50
    jmp dir_down;

horiz:
    jsr rng
    and #$01
    beq jmp_dir_left ; Given horiz, l or r starts as 50:50
    jmp dir_right

jmp_dir_left:   ; This is because the dir_left label is out of range.
    jmp dir_left
dir_up:
    dec tried_count     ; If we've tried all the directions and failed, ...
    beq gen_loop        ; ... give up and go back to the top.
    lda current_cell    ; If currentCell < xSizeCells - 1, we can't go up.
    cmp #XSIZECELLS
    bcc dir_down
    clc
    lda current_cell    ; If cellsArray[currentCell - xSizeCells] != 0, ...
    sec
    sbc #XSIZECELLS      ; ... we can't go up.
    tax
    lda cells, x
    bne dir_down
    inc cell_count
    lda current_cell
    pha ; Push the curret cell to the stack.
    ldy y_coord ; Get ((y_coord-1) * XSIZEVERT) + x_coord into X,...
    dey
    lda #XSIZEVERT
    jsr multiply
    clc
    adc x_coord
    tax
    lda #$00              ; ... then remove the vertical wall.
    sta vwalls, x
    lda current_cell    ; Mark the cell as visited with cell_count.
    sec
    sbc #XSIZECELLS
    tax
    lda cell_count
    sta cells, x
    lda current_cell    ; And update the current cell...
    sec
    sbc #XSIZECELLS
    sta current_cell    ; ... to be the new one we stepped up to.
    pha                 ; Push the new cell to the stack, ...
    jmp gen_loop        ; ... then head back to the top.
dir_down:
    dec tried_count     ; If we've tried all the directions and failed, ...
    beq gen_loop        ; ... give up and go back to the top.
    lda #TOTSIZECELLS    ; If current_cell > TOTSIZECELLS - XSIZECELLS, ...
    sec
    sbc #XSIZECELLS      ; ... we can't go down.
    sbc #$01
    cmp current_cell
    bcc dir_left
    clc
    lda current_cell     ; If cells[current_cell + XSIZECELLS] != 0, ...
    clc
    adc #XSIZECELLS      ; ... we can't go down.
    tax
    lda cells, x
    bne dir_left
    inc cell_count
    lda current_cell
    pha ; Push the curret cell to the stack.
    lda #XSIZEVERT   ; Get (y_coord * XSIZEVERT) + x_coord into X, ...
    ldy y_coord
    jsr multiply
    clc
    adc x_coord
    tax
    lda #0              ; ... then remove the vertical wall.
    sta vwalls, x
    lda current_cell    ; Mark the cell as visited with cell_count.
    clc
    adc #XSIZECELLS
    tax
    lda cell_count
    sta cells, x
    lda current_cell    ; And update the current cell...
    clc
    adc #XSIZECELLS
    sta current_cell    ; ... to be the new one we've just stepped to.
    pha                 ; Push the new current cell to the stack...
jmp_gen_loop_1:
    jmp gen_loop        ; ... and head back to the top.
dir_left:
    dec tried_count     ; If we've tried all the directions and failed, ...
    beq jmp_gen_loop_1        ; ... give up and go back to the top.
    lda x_coord         ; If x_coord == 0, we can't go left.
    beq dir_right
    lda current_cell    ; If cells[current_cell - 1] != 0, we can't go left.
    sec
    sbc #1
    tax
    lda cells, x
    bne dir_right
    inc cell_count
    lda current_cell
    pha ; Push the current cell to the stack.
    lda #XSIZEHORIZ  ; Get (y_coord * XSIZEHORIZ) + x_coord - 1 into X ...
    ldy y_coord
    jsr multiply
    clc
    adc x_coord
    sec
    sbc #1
    tax
    lda #0
    sta hwalls, x    ; ... then remove the horizontal wall.
    lda current_cell    ; Mark the cell as visited with cell_count.
    sec
    sbc #1
    tax
    lda cell_count
    sta cells, x
    lda current_cell    ; And update the current cell...
    sec
    sbc #1
    sta current_cell    ; ... to be the new one we've just stepped to.
    pha                 ; Push the new current cell to the stack...
jmp_gen_loop_2:
    jmp gen_loop        ; ... and head back to the top.
jmp_dir_up:
    jmp dir_up          ; bcc range hack
dir_right:
    dec tried_count     ; If we've tried all the directions and failed, ...
    beq jmp_gen_loop_2  ; ... give up and go back to the top.
    lda x_coord         ; If x_coord >= xSizeCells - 1, we can't go right.
    clc
    adc #1
    cmp #XSIZECELLS
    bcs jmp_dir_up
    clc
    lda current_cell    ; If cells[current_cell + 1] != 0, we can't go right.
    clc
    adc #1
    tax
    lda cells, x
    bne jmp_dir_up
    inc cell_count
    lda current_cell
    pha ; Push the current cell to the stack.
    lda #XSIZEHORIZ  ; Get (y_coord * XSIZEHORIZ) + x_coord into X ...
    ldy y_coord
    jsr multiply
    clc
    adc x_coord
    tax
    lda cell_count
    lda #0
    sta hwalls, x    ; ... then remove the horizontal wall.
    lda current_cell    ; Mark the cell as visited with cell_count.
    clc
    adc #1
    tax
    lda cell_count
    sta cells, x
    lda current_cell    ; And update the current cell...
    clc
    adc #1
    sta current_cell    ; ... to be the new one we've just stepped to.
    pha ; Push the new current cell to the stack...
    jmp gen_loop        ; ... and head back to the top.


; *** Make Collision Map ***
make_collision_map:
    ldx #$00    ; Start by loading the maze template into the maze.
    :
        lda maze_template, x
        sta the_maze, x
        inx
        cpx #SIZEMAZE
        bne :-
load_vert:  ; Then load the vertical walls into the maze.
    lda #0
    tax     ; Init x at 0 as the index into the vwalls,
    sta tmp ; ... and tmp at 0 as a loop counter.
    ldy #16 ; First byte in the maze with vert walls (index into the_maze).
    vert_loop:
        lda vwalls, x ;
        bne :+
            lda the_maze, y ;
            and #%10111111
            sta the_maze, y ;
        :
        inx
        lda vwalls, x ;
        bne :+
            lda the_maze, y ;
            and #%11101111
            sta the_maze, y ;
        :
        inx
        lda vwalls, x ;
        bne :+
            lda the_maze, y ;
            and #%11111011
            sta the_maze, y ;
        :
        inx
        lda tmp
        and #%00000011
        cmp #%00000011
        beq hop1a
        ; If we ain't at the end of the row, also do:
        lda vwalls, x
        bne :+
            lda the_maze, y ;
            and #%11111110
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
        ; cpx #TOTSIZEVERT
        cpy #108 ; First byte in the maze past the last of the vert walls
        bcc vert_loop; if y < 108, keep looping
load_horiz: ; Then load the horizontal walls into the maze.
    lda #0
    tax     ; Init x at 0 as the index into the hwalls,
    sta tmp ; ... and tmp at 0 as a loop counter.
    ldy #12 ; First byte in the maze with horiz walls (index into the_maze).
    horiz_loop:
        lda tmp ; If we're at the first byte in the row, skip the first wall.
        and #%00000011
        cmp #%00000000
        beq hop0
        lda hwalls, x ;
        bne :+
            lda the_maze, y ;
            and #%01111111
            sta the_maze, y ;
        :
        inx
    hop0:
        lda hwalls, x ;
        bne :+
            lda the_maze, y ;
            and #%11011111
            sta the_maze, y ;
        :
        inx
        lda hwalls, x ;
        bne :+
            lda the_maze, y ;
            and #%11110111
            sta the_maze, y ;
        :
        inx
        lda tmp
        and #%00000011
        cmp #%00000011
        beq hop1c
        ; If we ain't at the end of the row, also do:
        lda hwalls, x ;
        bne :+
            lda the_maze, y ;
            and #%11111101
            sta the_maze, y ;
        :
        inx
        jmp hop2c
        ; But if we is at the end of the row, do:
    hop1c:
        iny
        iny
        iny
        iny
    hop2c:
        iny
        inc tmp
        ; cpx #TOTSIZEVERT
        cpy #112 ; First byte in the maze past the last of the horiz walls
        bcc horiz_loop; if y < 112, keep looping
    add_exit:   ; Then add the exit.
        jsr rng ; get random number
        and #%00000111  ; knock it down to 0 - 7
        ldy #$08    ; Multiply by 2 (skip vert rows) * 4 bytes per row.
        jsr multiply
        clc
        adc #$0F    ; Scoot down past the top wall.
        tax
        lda the_maze, x
        and #%11111100  ; Remove the last two bits to open the exit.
        sta the_maze, x
    reset_players:
        lda #$08    ; Players always start on the far left side of the maze.
        sta player1_x
        sta player2_x
        jsr rng ; Get a random number.
        and #%00000111  ; Knock it down to 0 - 7.
        asl    ; Multiply by 16 (8 px per row; skip vert wall rows).
        asl
        asl
        asl
        adc #$67    ; Scoot down to one of the bottom 8 guaranteed cells.
        clc
        sta player1_y   ; Put the players there.
        sta player2_y
        rts

; *** LOAD MAZE ***
load_maze:
    ; setup background
    lda #%00000000    ; Disable everything before loading background
    ; first nametable, start by clearing to empty
    lda PPU_STATUS ; reset latch
    lda #$20
    sta PPU_ADDR
    lda #$00
    sta PPU_ADDR
    ; load the_maze maze into ppu nametable
    ldy #$00 ; 120 bytes per map
    :
        ldx #$00 ; 8 bits per byte
        :
            lda the_maze, y
            and bit_mask, x
            beq hop1d
                lda #$31
            hop1d:
            bne hop2d
                lda #$30
            hop2d:
            sta PPU_DATA
            inx
            cpx #$08
            bne :-
        iny
        cpy #SIZEMAZE
        bne :--
    ; set all attributes to 0
    lda #0
    ldx #64 ; 64 bytes
    :
        sta PPU_DATA
        dex
        bne :-
    ; load palettes
        lda #$3F
        sta PPU_ADDR
        ldx #$00
        stx PPU_ADDR ; set PPU address to $3F00
        :
            lda palettes, x
            sta PPU_DATA
            inx
            cpx #$20
            bcc :-
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


; *** Turn Stuff Off
; Turn off NMI, sprites, and Audio.
turn_stuff_off:
    lda #%00000000
    sta PPU_CTRL    ; Turn off NMI
    sta PPU_MASK    ; Turn off sprites & backgrounds
    sta APU_STATUS  ; Turn off audio
    rts


; *** Turn Stuff On
; Turn NMI, sprites, background and Audio on.
turn_stuff_on:
    lda #%10001000    ; Enable NMI, sprites, and background (table 0)
    sta PPU_CTRL
    lda #%00011110    ; Enable sprites & Backgrounds
    sta PPU_MASK
    lda #%00001111  ; Enable audio channels noise, triangle, pulse 2 & pulse 1
    sta APU_STATUS
    rts


; *** Load Hardcoded Maze ***
; The ROM includes one hand-drawn maze.  This subroutine loads it.
load_hard_maze:
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
    RTS

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
    ldx #$08
    ; latch the buttons
    lda #$01
    sta CONTROLLER_1
    lda #$00
    sta CONTROLLER_1
    ; read the controllers
read_loop:
    lda CONTROLLER_1    ; 1st bit, C1
    lsr
    ror controller1_state
    lda CONTROLLER_2    ; 1st bit, C2
    lsr
    ror controller2_state
    dex
    bne read_loop
    rts

; *** Draw Sprites ***
; 4 bytes per sprite: y position, which sprite in the sheet, attributes, and x
; position (in that order).  256 bytes per page / 4 per sprite = 64 sprites max
draw_sprites:
    lda player1_y   ; Player 1
    sta oam + 0
    lda #$04        ; Memory address #$04 from sprite.chr has the player sprite
    sta oam + 1
    lda #%00000001 ; no flip, pallete 5
    sta oam + 2
    lda player1_x
    sta oam + 3
    lda player2_y   ; Player 2
    sta oam + 4
    lda #$04
    sta oam + 5
    lda #%00000010 ; no flip, pallete 5
    sta oam + 6
    lda player2_x
    sta oam + 7
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
    lda the_maze, y
    and bit_mask, x ; Sets zero flag in case of collision...
    rts ; ...and returns.


; *** Multiply ***
; multiply - pass in the multiplicand via the accumulator and the multiplier
; via the Y register.  The product will get returned via the accumulator.
multiply:
    sta tmp
    cpy #0
    beq zero_y
loop:
    dey
    beq done
    clc
    adc tmp
    jmp loop
zero_y:
    lda #0
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
    clc ; ???
    adc tmp
    rts


; *** Hardcoded Palettes ***
palettes:
; background palettes
.byte $38,$09,$1a,$28 ; greens on tan
.byte $0f,$06,$27,$15 ; reds on black
.byte $0f,$01,$1c,$22 ; blues on black
.byte $0f,$2d,$3d,$30 ; greyscale
; sprite palettes
.byte $38,$09,$1a,$28 ; greens on tan
.byte $0f,$27,$15,$06 ; reds on black
.byte $0f,$22,$1c,$01 ; blues on black
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

; *** Title Screen Nametable ***
    .include "TitleScreen.s"
