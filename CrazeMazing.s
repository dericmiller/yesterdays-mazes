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

	; Restore stashed registers before returning
	pla
	tay
	pla
	tax
	pla
    rti

; *** Reset ***
reset:
    lda #%10001000    ; Enable NMI, sprites, and background (table 0)
    sta PPU_CTRL
    lda #%00011110    ; Enable sprites & Backgrounds
    sta PPU_MASK
    lda $00     ; Turn off background scrolling
    sta PPU_ADDR
    sta PPU_ADDR
    sta PPU_SCROLL
    sta PPU_SCROLL
    jmp main

; *** IRQ ***
irq:
	rti

; *** Main ***
main:
    ; initialize main
    lda #64
    sta player1_x
    lda #64
    sta player1_y

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
    ; jsr load_maze

@main_loop:
    jsr draw_sprites
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

; *** Draw Sprites
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

; *** Hardcoded Palettes ***
palettes:
; background palettes
.byte $38,$09,$1a,$28 ; greens on tan
.byte $0f,$01,$1c,$22 ; blues on black
.byte $0f,$06,$15,$27 ; reds on black
.byte $0f,$2d,$3d,$30 ; greyscale
; sprite palettes
.byte $38,$09,$1a,$28 ; greens on tan
.byte $0f,$01,$1c,$22 ; blues on black
.byte $0f,$06,$15,$27 ; reds on black
.byte $0f,$2d,$3d,$30 ; greyscale

; *** Hardcoded Maze Map ***
hardMap:
    .include "HardcodedMap.s"

