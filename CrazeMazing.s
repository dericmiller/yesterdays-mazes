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
player1_x: .res 1
player1_y: .res 1
controller1_state: .res 1
tmp: .res 1

; Reserve a page for sprite data
.segment "OAM"
oam: .res 256        ; sprite OAM data

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

	; Lock scroll at zero
	lda #%10001000
	sta PPU_CTRL
	lda #0
	sta PPU_SCROLL
	lda #0
    sta PPU_SCROLL

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
    lda #8
    sta player1_x
    lda #199
    sta player1_y

    ; jsr load_maze

@main_loop:
    ; Draw screen
    jsr draw_sprites
    ; Repeat forever
    jmp @main_loop

; *** Load Maze ***
load_maze:
    ; first nametable, start by clearing to empty
    lda PPU_STATUS ; reset PPU
    lda #$20
    sta PPU_ADDR
    lda #$00
    sta PPU_ADDR

    lda #$03
    ldx #$00
    ldy #$00
@loop:
    sta PPU_DATA
    iny
    cpy #$00
    bne @loop
    inx
    cpx #$04
    bne @loop
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

; *** Hardcoded Maze Map ***
    .include "HardcodedMap.s"

