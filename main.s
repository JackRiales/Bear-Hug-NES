;-------------------------------------------
;- Bear Hug!
;- A little NES game by Jack Riales
;-------------------------------------------
;- NESASM header
;-------------------------------------------
    .inesprg 1    ; 1x 16KB bank of program rom
    .ineschr 1    ; 1x 8KB bank of chr rom
    .inesmap 0    ; Using mapper 0 (NROM, no bank swap)
    .inesmir 1    ; Set mirroring to setting 1

;-------------------------------------------
;- PRG Code
;-------------------------------------------
    .bank 0
    .org $C000

;- Startup code
RESET:
    sei             ; Disable IRQs
    cld             ; Disable decimal mode
    ldx #$40
    stx $4017       ; Disable APU frame IRQ
    ldx #$ff
    txs             ; Set up the stack
    inx             ; x = 0
    stx $2000       ; Disable nmi
    stx $2001       ; Disable rendering
    stx $4010       ; Disable DMC IRQs

;- Wait for the first vblank to make sure ppu is ready
_vblankinit:
    bit $2002
    bpl _vblankinit

;- Ensures everything cleared to 0
clearmem:
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    lda #$fe
    sta $0300, x
    inx
    bne clearmem

vblankwait:
    bit $2002
    bpl vblankwait

loadpalettes:
    lda $2002       ; Read PPU status to reset high/low latch
    lda #$3f        ; (Gets rid of status info)
    sta $2006       ; Write high byte of $3f00 addr
    lda #$00
    sta $2006       ; Write low byte of $3f00 addr
    ldx #$00        ; x = 0

_loadpalettes_loop:
    lda palette, x  ; Load byte of palette
    sta $2007       ; Write to the PPU
    inx             ; Go to the next byte (increment index)
    cpx #$20
    ;- if x = $20, 32 bytes copied to ppu, we're done
    bne _loadpalettes_loop

;- Sets sprite $A to position $x and $y
;- Precondition: $A, $x, $y set appropriately
;- Postcondition: $0200-$0203 updated
setsprite:
    sty $0200        ; Set y position from y register
    stx $0203        ; Set x position from x register
    sta $0201        ; Set tile id from accumulator
    lda #$00
    sta $0202        ; Color = 0, no flipping
    rts
    
setspritestatic:
    lda #$80
    sta $0200       ; Sprite 0 in center ($80) of the screen (y)
    sta $0203       ; Sprite 0 in center ($80) of the screen (x)
    lda #$00
    sta $0201       ; Tile number = 0
    sta $0202       ; Color = 0, no flippin'
    rts

drawsprite:
    lda #%10000000  ; Enable nmi and sprites from pattern table 0
    sta $2000       ; Write to ppuctrl
    lda #%00010000  ; Enable sprites
    sta $2001       ; Write to ppumask
    rts

;- Temporarily named main loop
    lda #$80    ; Initial x and y position
    sta $0200
    sta $0203

FOREVER:
    ldx $0203
    ldy $0200
    lda #$00
    jsr setspritestatic
    jsr drawsprite
    jmp FOREVER

NMI:
    lda #$00
    sta $2003   ; Set low byte of the ram address
    lda #$02
    sta $4014   ; Set the high byte of the ram address, start transfer
    rti         ; Return

;-------------------------------------------
;- Interrupts
;-------------------------------------------
    .bank 1

    .org $E000  ; Palettes
palette:
    .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
    .db $0F,$1C,$15,$14,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

    .org $FFFA  ; Vectors
    .dw NMI     ; When NMI happens, we'll jump here
    .dw RESET   ; When we initialize, we jump here
    .dw 0       ; When we set up IRQ, we'll set the label here

;-------------------------------------------
;- CHR data
;-------------------------------------------
    .bank 2
    .org $0000

    .incbin "mario.chr"

;----------------------- EOF -----------------------;