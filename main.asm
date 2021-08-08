                    !cpu 6510

DEBUG = 0
RELEASE = 0
; ==============================================================================
ENABLE              = 0x20
ENABLE_JMP          = 0x4C
DISABLE             = 0x2C

FLAG_LOOP           = %00000001
FLAG_END            = %00000000

BLACK               = 0x00
WHITE               = 0x01
RED                 = 0x02
CYAN                = 0x03
PURPLE              = 0x04
GREEN               = 0x05
BLUE                = 0x06
YELLOW              = 0x07
ORANGE              = 0x08
BROWN               = 0x09
PINK                = 0x0A
DARK_GREY           = 0x0B
GREY                = 0x0C
LIGHT_GREEN         = 0x0D
LIGHT_BLUE          = 0x0E
LIGHT_GREY          = 0x0F

MEMCFG              = 0x35

IRQ_LINE0           = 0x00
IRQ_LINE1           = 0x2E
IRQ_LINE2           = 0xF7

DISPLAY_START_X     = 0x00
DISPLAY_START_Y     = 0x0C

TDISPL_X            = 27
TDISPL_Y            = 8

COLORBG1            = GREEN
COLORBG2            = BLACK
COLORTEXT           = GREEN
COLORHIGHLIGHT      = LIGHT_GREEN
COLORTIME           = YELLOW
COLORSCROLL         = BLACK
COLORBORDER         = DARK_GREY
COLORBORDERHLT      = LIGHT_GREEN

MARKER_EORVAL       = 0x80

VOLUME_GT           = 0x108D

; ==============================================================================
zp_start            = 0x02
; 0x02 and 0x03 used by GoatTracker exports
num_songs           = zp_start+2
cur_song            = num_songs+1
zp_temp             = cur_song+1
zp_temp_lo          = zp_temp
zp_temp_hi          = zp_temp_lo+1
new_song            = zp_temp_hi+1
flag_irq_ready      = new_song+1
tune_end_flag       = flag_irq_ready+1
cur_data            = tune_end_flag+1
;cur_data SIZE 8
cur_colram          = cur_data+8
cur_colram_lo       = cur_colram
cur_colram_hi       = cur_colram_lo+1
marker_pos          = cur_colram_hi+1
marker_pos_lo       = marker_pos
marker_pos_hi       = marker_pos_lo+1
marker_song         = marker_pos_hi+1

; ==============================================================================
KEY_CRSRUP          = 0x91
KEY_CRSRDOWN        = 0x11
KEY_CRSRLEFT        = 0x9D
KEY_CRSRRIGHT       = 0x1D
KEY_RETURN          = 0x0D
KEY_STOP            = 0x03

getin               = 0xFFE4
keyscan             = 0xEA87
; ==============================================================================
code_start          = 0x2280
vicbank0            = 0x0000
charset0            = vicbank0+0x0800
vidmem0             = vicbank0+0x0400
sprite_data         = vicbank0+0x2240
sprite_base         = <((sprite_data-vicbank0)/0x40)
dd00_val0           = <!(vicbank0/0x4000) & 3
d018_val0           = <(((vidmem0-vicbank0)/0x400) << 4)+ <(((charset0-vicbank0)/0x800) << 1)
music_init          = 0x1000
music_play          = 0x1003
; ==============================================================================
                    !macro flag_set .flag {
                        lda #1
                        sta .flag
                    }
                    !macro flag_clear .flag {
                        lda #0
                        sta .flag
                    }
                    !macro flag_get .flag {
                        lda .flag
                    }
; ==============================================================================
                    *= vidmem0
                    !bin "gfx/intro.scr"

                    *= charset0
                    !bin "gfx/charset.chr"

                    *= sprite_data
                    !bin "gfx/bgsprite.spr"
; ==============================================================================
                    *= code_start
                    lda #0x7F
                    sta 0xDC0D
                    lda #MEMCFG
                    sta 0x01
                    jmp init_code
; ==============================================================================
                    !zone IRQ
                    NUM_IRQS = 0x03
                    !align 255,0
irq:                !if MEMCFG = 0x35 {
                        sta .irq_savea+1
                        stx .irq_savex+1
                        sty .irq_savey+1
                        lda 0x01
                        sta .irq_save0x01+1
                        lda #0x35
                        sta 0x01
                    }
irq_next:           jmp irq0
irq_end:            lda 0xD012
-                   cmp 0xD012
                    beq -
.irq_index:         ldx #0
                    lda irq_tab_lo,x
                    sta irq_next+1
                    lda irq_tab_hi,x
                    sta irq_next+2
                    lda irq_lines,x
                    sta 0xD012
                    inc .irq_index+1
                    lda .irq_index+1
                    cmp #NUM_IRQS
                    bne +
                    lda #0
                    sta .irq_index+1
+                   asl 0xD019
                    !if MEMCFG = 0x37 {
                        jmp 0xEA31
                    }
                    !if MEMCFG = 0x36 {
                        jmp 0xEA81
                    }
                    !if MEMCFG = 0x35 {
.irq_save0x01:          lda #0x35
                        sta 0x01
                        cmp #0x36
                        beq +
.irq_savea:             lda #0
.irq_savex:             ldx #0
.irq_savey:             ldy #0
                        rti
+                       jmp 0xEA81
                    }

irq0:
enable_music:       bit music_play
enable_timer:       bit timer_increase
enable_timer_check: bit timer_check
                    jmp irq_end

irq1:               lda #IRQ_LINE1+2
-                   cmp 0xD012
                    bne -
                    ldx #22
-                   dex
                    bpl -
                    bit 0xEA
                    lda #COLORBORDERHLT
                    sta 0xD020
                    lda #COLORBG2
                    ldx #10
-                   dex
                    bpl -
                    sta 0xD020
                    jmp irq_end

irq2:               lda #IRQ_LINE2+2
-                   cmp 0xD012
                    bne -
                    ldx #22
-                   dex
                    bpl -
                    bit 0xEA
                    lda #COLORBORDERHLT
                    sta 0xD020
                    lda #COLORBORDER
                    ldx #10
-                   dex
                    bpl -
                    sta 0xD020
                    +flag_set flag_irq_ready
                    jmp irq_end

irq_tab_lo:         !byte <irq0, <irq1, <irq2
irq_tab_hi:         !byte >irq0, >irq1, >irq2
irq_lines:          !byte IRQ_LINE0, IRQ_LINE1, IRQ_LINE2
; ==============================================================================
                    !zone INIT
init_code:          jsr init_nmi
                    jsr init_vic
                    jsr intro_part
                    jsr init_zp
                    jsr init_scr_and_songs
                    jsr init_sprites
                    jsr init_irq
                    jmp main

init_zp:            lda #0
                    ldx #0xFD
-                   sta zp_start-1,x
                    dex
                    bne -
                    lda #0
                    sta tune_end_flag
                    lda #1
                    sta new_song
                    rts

init_irq:           lda irq_lines
                    sta 0xD012
                    lda #<irq
                    sta 0x0314
                    !if MEMCFG = 0x35 {
                        sta 0xFFFE
                    }
                    lda #>irq
                    sta 0x0315
                    !if MEMCFG = 0x35 {
                        sta 0xFFFF
                    }
                    lda 0xD011
                    and #%01101111
                    ora #%00010000
                    sta 0xD011
                    lda #0x01
                    sta 0xD019
                    sta 0xD01A
                    rts

init_nmi:           lda #<nmi
                    sta 0x0318
                    !if MEMCFG = 0x35 {
                        sta 0xFFFA
                    }
                    lda #>nmi
                    sta 0x0319
                    !if MEMCFG = 0x35 {
                        sta 0xFFFB
                    }
                    rts

init_vic:           lda #dd00_val0
                    sta 0xDD00
                    lda #d018_val0
                    sta 0xD018
                    rts

init_scr_and_songs: lda #COLORBG2
                    sta 0xD020
                    sta 0xD021
                    ldx #0
-                   lda #COLORTEXT
                    sta 0xD800+0x000,x
                    sta 0xD800+0x100,x
                    sta 0xD800+0x200,x
                    sta 0xD800+0x2E8,x
                    lda #' '
                    sta vidmem0+0x000,x
                    sta vidmem0+0x100,x
                    sta vidmem0+0x200,x
                    sta vidmem0+0x2E8,x
                    inx
                    bne -
                    ldx #39
-                   lda gfx_header_screen+(0*40),x
                    sta vidmem0+(0*40),x
                    lda gfx_header_screen+(1*40),x
                    sta vidmem0+(1*40),x
                    lda gfx_header_screen+(2*40),x
                    sta vidmem0+(2*40),x
                    lda gfx_header_screen+(3*40),x
                    sta vidmem0+(3*40),x
                    lda gfx_header_screen+(4*40),x
                    sta vidmem0+(4*40),x
                    lda gfx_header_screen+(5*40),x
                    sta vidmem0+(5*40),x
                    lda gfx_header_screen+(6*40),x
                    sta vidmem0+(6*40),x
                    lda gfx_header_screen+(7*40),x
                    sta vidmem0+(7*40),x
                    lda gfx_header_screen+(8*40),x
                    sta vidmem0+(8*40),x
                    lda gfx_header_screen+(9*40),x
                    sta vidmem0+(9*40),x
                    lda gfx_header_screen+(10*40),x
                    sta vidmem0+(10*40),x
                    lda gfx_header_colram+(0*40),x
                    sta 0xD800+(0*40),x
                    lda gfx_header_colram+(1*40),x
                    sta 0xD800+(1*40),x
                    lda gfx_header_colram+(2*40),x
                    sta 0xD800+(2*40),x
                    lda gfx_header_colram+(3*40),x
                    sta 0xD800+(3*40),x
                    lda gfx_header_colram+(4*40),x
                    sta 0xD800+(4*40),x
                    lda gfx_header_colram+(5*40),x
                    sta 0xD800+(5*40),x
                    lda gfx_header_colram+(6*40),x
                    sta 0xD800+(6*40),x
                    lda gfx_header_colram+(7*40),x
                    sta 0xD800+(7*40),x
                    lda gfx_header_colram+(8*40),x
                    sta 0xD800+(8*40),x
                    lda gfx_header_colram+(9*40),x
                    sta 0xD800+(9*40),x
                    lda gfx_header_colram+(10*40),x
                    sta 0xD800+(10*40),x
                    dex
                    bpl +
                    jmp ++
+                   jmp -
++                  lda #<vidmem0+(40*DISPLAY_START_Y)+DISPLAY_START_X
                    sta marker_pos_lo
                    sta zp_temp_lo
                    lda #>vidmem0+(40*DISPLAY_START_Y)+DISPLAY_START_X
                    sta marker_pos_hi
                    sta zp_temp_hi
                    ldx #0x00
-                   stx num_songs
                    txa
                    jsr print_snum
                    inc zp_temp_lo
                    inc zp_temp_lo
                    lda songtable,x
                    jsr print_stitle
                    dec zp_temp_lo
                    dec zp_temp_lo
                    clc
                    lda zp_temp_lo
                    adc #40
                    sta zp_temp_lo
                    lda zp_temp_hi
                    adc #0
                    sta zp_temp_hi
                    inx
                    lda songtable,x
                    bne -
                    lda num_songs
                    sta cur_song
                    lda #<(vidmem0+(40*TDISPL_Y)+TDISPL_X)
                    sta zp_temp_lo
                    lda #>(vidmem0+(40*TDISPL_Y)+TDISPL_X)
                    sta zp_temp_hi
                    ldy #0x0C
-                   lda time_tmpl,y
                    sta ( zp_temp ),y
                    dey
                    bpl -
                    lda #<0xD800+(40*DISPLAY_START_Y)+DISPLAY_START_X
                    sta cur_colram_lo
                    lda #>0xD800+(40*DISPLAY_START_Y)+DISPLAY_START_X
                    sta cur_colram_hi
                    jsr mark_update
                    lda #COLORTIME
                    sta 0xD800+(40*TDISPL_Y)+TDISPL_X
                    sta 0xD800+(40*TDISPL_Y)+TDISPL_X+1
                    sta 0xD800+(40*TDISPL_Y)+TDISPL_X+3
                    sta 0xD800+(40*TDISPL_Y)+TDISPL_X+4
                    lda #COLORTEXT
                    sta 0xD800+(40*TDISPL_Y)+TDISPL_X+2
                    ldx #7
-                   sta 0xD800+(40*TDISPL_Y)+TDISPL_X+5,x
                    dex
                    bpl -

                    ldx #8

-
                    !for i, 0, 10 {
                        lda gfx_dude_screen+(i*9),x
                        sta vidmem0+((DISPLAY_START_Y+2+i)*40)+31,x
                    }

                    !for i, 0, 10 {
                        lda gfx_dude_colram+(i*9),x
                        sta 0xD800+((DISPLAY_START_Y+2+i)*40)+31,x
                    }

                    dex
                    bpl +
                    jmp ++
+                   jmp -
++
                    rts

time_tmpl:          !scr "00:00 / 00:00"

                    SPRITESY = 0x3A
                    SPRITESX1 = 0x18
                    SPRITESX2 = <SPRITESX1+(30*8)

init_sprites:       lda #sprite_base
                    sta vidmem0+0x3F8
                    sta vidmem0+0x3F9
                    sta vidmem0+0x3FA
                    sta vidmem0+0x3FB
                    sta vidmem0+0x3FC
                    sta vidmem0+0x3FD
                    sta vidmem0+0x3FE
                    sta vidmem0+0x3FF

                    lda #SPRITESY
                    sta 0xD001
                    sta 0xD003
                    sta 0xD005
                    sta 0xD007
                    sta 0xD009
                    sta 0xD00B
                    sta 0xD00D
                    sta 0xD00F

                    lda #SPRITESX1
                    sta 0xD000
                    lda #SPRITESX1+16
                    sta 0xD002

                    lda #SPRITESX2
                    sta 0xD004
                    lda #SPRITESX2+16
                    sta 0xD006
                    lda #SPRITESX2+(2*16)
                    sta 0xD008
                    lda #SPRITESX2+(3*16)
                    sta 0xD00A
                    lda #SPRITESX2+(4*16)
                    sta 0xD00C
                    lda #SPRITESX2+(5*16)
                    sta 0xD00E

                    lda #%11111100
                    sta 0xD010

                    lda #RED
                    ldx #7
-                   sta 0xD027,x
                    dex
                    bpl -

                    lda #PINK
                    sta 0xD025
                    lda #YELLOW
                    sta 0xD026

                    lda #0
                    sta 0xD017
                    sta 0xD01D

                    lda #0xFF
                    sta 0xD01B
                    sta 0xD01C
                    sta 0xD015
                    rts

init_next_tune:     lda #DISABLE
                    sta enable_music
                    sta enable_timer
                    sta enable_timer_check
                    lda #0
                    sta 0xD404
                    sta 0xD406
                    sta 0xD40B
                    sta 0xD40D
                    sta 0xD412
                    sta 0xD414
                    lda #COLORTEXT
                    ldx cur_song
                    jsr print_colored_line
                    lda cur_song
                    cmp num_songs
                    bne +
                    lda #0xFF
                    sta cur_song
+                   inc cur_song
                    jsr read_cur_data
                    jsr decrunch_song
                    lda #COLORHIGHLIGHT
                    ldx cur_song
                    jsr print_colored_line
                    lda #0
                    jsr music_init
                    jsr timer_init
                    jsr wait_irq
                    lda #ENABLE
                    sta enable_music
                    sta enable_timer
                    sta enable_timer_check
                    lda #50
                    sta framecounter
                    lda #0
                    sta new_song
                    rts
; ==============================================================================
                    !zone MAIN
main:
mainloop:           jsr wait_irq
                    !if DEBUG=1 {
                        lda cur_song
                        sta vidmem0
                        lda tune_end_flag
                        sta vidmem0+1
                    }
                    lda new_song
                    beq +
                    jsr init_next_tune
+
                    lda tune_end_flag
                    beq +
                    lda #0
                    sta tune_end_flag
                    lda cur_data+2
                    lsr
                    bcs init_fadeloop
                    lda #1
                    sta new_song
+                   jsr anim_discoball
                    jsr anim_discoshine
                    jsr anim_dancefloor
                    jsr anim_finger
                    jsr keyboard_get
                    jmp mainloop

init_fadeloop:
.gt:                lda #<VOLUME_GT
                    sta vol_addr+1
                    lda #>VOLUME_GT
                    sta vol_addr+2
                    jmp fadeloop

fadeloop:           jsr wait_irq
                    dec .fadetime
                    lda .fadetime
                    bne fadeloop
                    lda #32
                    sta .fadetime
cur_vol:            lda #0x0F
vol_addr:           sta 0x0000
                    beq +
                    dec cur_vol+1
                    jmp fadeloop
+                   lda #0x0F
                    sta cur_vol+1
                    lda #1
                    sta new_song
                    jmp mainloop
.fadetime:          !byte 32

read_cur_data:      ldx cur_song
                    ldy songtable,x
                    dey
                    lda sdata_pt_lo,y
                    sta zp_temp_lo
                    lda sdata_pt_hi,y
                    sta zp_temp_hi

                    ldy #7
-                   lda ( zp_temp ),y
                    sta cur_data,y
                    dey
                    bpl -
                    rts
; ==============================================================================
                    !zone NMI
nmi:                lda #0x37               ; restore 0x01 standard value
                    sta 0x01
                    lda #0                  ; if AR/RR present
                    sta 0xDE00              ; reset will lead to menu
                    jmp 0xFCE2              ; reset
; ==============================================================================
                    !zone WAIT
wait_irq:           +flag_clear flag_irq_ready
.wait_irq:          +flag_get flag_irq_ready
                    beq .wait_irq
                    rts
; ==============================================================================
                    !zone DECRUNCH
exod_addr:          !src "inc/wrap.asm"
                    !src "inc/exodecrunch.asm"
decrunch_song:      lda cur_data
                    sta opbase+1
                    lda cur_data+1
                    sta opbase+2
                    jsr exod_addr
                    rts
; ==============================================================================
                    !zone PRINT
print_snum:         stx .savex+1
                    tax
                    inx
                    ldy #0
                    lda #0x20
                    sta ( zp_temp ),y
                    iny
                    lda .dectab,x
                    jsr lib_hex2screen
                    sta ( zp_temp ),y
                    iny
                    txa
                    sta ( zp_temp ),y
                    iny
                    lda #0x20
                    sta ( zp_temp ),y
.savex:             ldx #0
                    rts
.dectab:            !byte 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07
                    !byte 0x08, 0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15

print_stitle:       stx .savex+1
                    tax
                    dex
                    lda sdata_pt_lo,x
                    sta .a0+1
                    lda sdata_pt_hi,x
                    sta .a0+2
                    clc
                    lda .a0+1
                    adc #(8-3)
                    sta .a0+1
                    lda .a0+2
                    adc #0
                    sta .a0+2
                    ldy #3
.a0:                lda 0x0000,y
                    beq +
                    sta ( zp_temp ),y
                    iny
                    jmp .a0
+                   jsr .savex
                    rts

print_colored_line: pha                         ; color in A
                    lda .d800ytab_lo,x          ; line num in X
                    sta .mod0+1
                    lda .d800ytab_hi,x
                    sta .mod0+2

                    ldx #30
                    pla
.mod0:              sta 0x0000,x
                    dex
                    bpl .mod0
                    rts
.d800ytab_lo:       !for i, 0, 12 {
                        !byte <0xD800+(40*(DISPLAY_START_Y+i))+DISPLAY_START_X
                    }
.d800ytab_hi:       !for i, 0, 12 {
                        !byte >0xD800+(40*(DISPLAY_START_Y+i))+DISPLAY_START_X
                    }
; ==============================================================================
; lib_hex2screen
; ------------+-----------------------------------------------------------------
; depends on: | -
; ------------+-----------------------------------------------------------------
; uses:       | A, X
; ------------+-----------------------------------------------------------------
; preserves:  | Y
; ------------+---+-------------------------------------------------------------
; input:      | A | hexvalue to be converted
; ------------+---+-------------------------------------------------------------
; output:     | A | petscii/screencode high nibble
;             | X | petscii/screencode low nibble
; ------------+---+-------------------------------------------------------------
                    !zone LIB_HEX2SCREEN
lib_hex2screen:     sta .savea+1
                    and #%00001111
                    tax
                    lda .hextab,x
                    sta .low_nibble+1
.savea              lda #0
                    lsr
                    lsr
                    lsr
                    lsr
                    tax
                    lda .hextab,x           ; high nibble
.low_nibble         ldx #0
                    rts
.hextab:            !scr "0123456789abcdef"
; ==============================================================================
                    !zone KEYBOARD
keyboard_get:       !if DEBUG=1 { dec 0xD020 }
                    lda #0x36
                    sta 0x01
                    jsr keyscan
                    jsr getin
                    bne +
                    jmp .key_exit
+                   !if DEBUG=1 { sta vidmem0+3 }
                    cmp #KEY_CRSRUP
                    bne +
                    jmp .mark_up
+                   cmp #KEY_CRSRDOWN
                    bne +
                    jmp .mark_down
+                   cmp #KEY_RETURN
                    bne +
                    jmp .tune_select
+                   cmp #KEY_STOP
                    bne +
                    jmp .pause_toggle
+
.key_exit:          lda #0x35
                    sta 0x01
                    lda #d018_val0
                    sta 0xD018
                    !if DEBUG=1 { inc 0xD020 }
                    rts

.mark_down:         lda marker_pos_lo
                    cmp #<vidmem0+(40*(DISPLAY_START_Y+12))+DISPLAY_START_X
                    bne +
                    lda marker_pos_hi
                    cmp #>vidmem0+(40*(DISPLAY_START_Y+12))+DISPLAY_START_X
                    bne +
                    jmp .key_exit
+                   jsr mark_update
                    inc marker_song
                    clc
                    lda marker_pos_lo
                    adc #40
                    sta marker_pos_lo
                    lda marker_pos_hi
                    adc #0
                    sta marker_pos_hi
                    jsr mark_update
                    jmp .key_exit
.mark_up:           lda marker_pos_lo
                    cmp #<vidmem0+(40*(DISPLAY_START_Y+0))+DISPLAY_START_X
                    bne +
                    lda marker_pos_hi
                    cmp #>vidmem0+(40*(DISPLAY_START_Y+0))+DISPLAY_START_X
                    bne +
                    jmp .key_exit
+                   jsr mark_update
                    dec marker_song
                    sec
                    lda marker_pos_lo
                    sbc #40
                    sta marker_pos_lo
                    lda marker_pos_hi
                    sbc #0
                    sta marker_pos_hi
                    jsr mark_update
                    jmp .key_exit
mark_update:        ldy #3
-                   lda ( marker_pos ),y
                    eor #MARKER_EORVAL
                    sta ( marker_pos ),y
                    dey
                    bpl -
                    rts

.tune_select:       ldx cur_song
                    lda #COLORTEXT
                    jsr print_colored_line
                    ldx marker_song
                    dex
                    stx cur_song
                    lda #1
                    sta new_song
                    jmp .key_exit

.pause_toggle:      lda #0
                    beq .pause
.unpause:           lda #ENABLE
                    sta enable_music
                    sta enable_timer
                    sta enable_timer_check
                    lda #0
                    sta .pause_toggle+1
                    jmp .key_exit
.pause:             lda #DISABLE
                    sta enable_music
                    sta enable_timer
                    sta enable_timer_check
                    lda #0
                    sta 0xD404
                    sta 0xD40B
                    sta 0xD412
                    lda #1
                    sta .pause_toggle+1
                    jmp .key_exit
; ==============================================================================
                    !zone TIMER
timer_init:         ldy #4
-                   lda cur_data+3,y
                    sta time_tmpl+8,y
                    dey
                    bpl -

                    lda #<(vidmem0+(40*TDISPL_Y)+TDISPL_X)
                    sta zp_temp_lo
                    lda #>(vidmem0+(40*TDISPL_Y)+TDISPL_X)
                    sta zp_temp_hi
                    ldy #0x0C
-                   lda time_tmpl,y
                    sta ( zp_temp ),y
                    dey
                    bpl -
                    rts
timer_increase:     min_cnt_hi = vidmem0+(40*TDISPL_Y)+TDISPL_X+0
                    min_cnt_lo = vidmem0+(40*TDISPL_Y)+TDISPL_X+1
                    sec_cnt_hi = vidmem0+(40*TDISPL_Y)+TDISPL_X+3
                    sec_cnt_lo = vidmem0+(40*TDISPL_Y)+TDISPL_X+4
                    dec framecounter
                    beq +
                    rts
+                   lda sec_cnt_lo
                    cmp #0x39
                    bne ++++
                    lda #0x2F
                    sta sec_cnt_lo
                    lda sec_cnt_hi
                    cmp #0x35
                    bne +++
                    lda #0x2F
                    sta sec_cnt_hi
                    lda min_cnt_lo
                    cmp #0x39
                    bne ++
                    lda #0x2F
                    sta min_cnt_lo
                    lda min_cnt_hi
                    cmp #0x35
                    bne +
                    lda #0x2F
                    sta min_cnt_hi
+                   inc min_cnt_hi
++                  inc min_cnt_lo
+++                 inc sec_cnt_hi
++++                inc sec_cnt_lo
                    lda #50
                    sta framecounter
                    rts
framecounter:       !byte 50
timer_check:        min_end_hi = vidmem0+(40*TDISPL_Y)+TDISPL_X+8
                    min_end_lo = vidmem0+(40*TDISPL_Y)+TDISPL_X+9
                    sec_end_hi = vidmem0+(40*TDISPL_Y)+TDISPL_X+11
                    sec_end_lo = vidmem0+(40*TDISPL_Y)+TDISPL_X+12
                    lda min_cnt_hi
                    cmp min_end_hi
                    beq +
                    rts
+                   lda min_cnt_lo
                    cmp min_end_lo
                    beq +
                    rts
+                   lda sec_cnt_hi
                    cmp sec_end_hi
                    beq +
                    rts
+                   lda sec_cnt_lo
                    cmp sec_end_lo
                    beq +
                    rts
+                   lda #1
                    sta tune_end_flag
                    lda #DISABLE
                    sta enable_timer_check
                    rts
; ==============================================================================
                    !zone ANIMATION

                    ANIM_DANCEFLOOR_SPEED = 4
anim_dancefloor:    lda #ANIM_DANCEFLOOR_SPEED
                    beq +
                    dec anim_dancefloor+1
                    rts
+                   lda #ANIM_DANCEFLOOR_SPEED
                    sta anim_dancefloor+1

                    ldx #15
                    lda 0xD800+(9*40)+2+1,x
                    sta .save_upper_l+1
                    lda 0xD800+(10*40)+1+1,x
                    sta .save_lower_l+1
-                   lda 0xD800+(9*40)+2,x
                    sta 0xD800+(9*40)+2+1,x
                    lda 0xD800+(10*40)+1,x
                    sta 0xD800+(10*40)+1+1,x
                    dex
                    bpl -
.save_upper_l:      lda #0
                    sta 0xD800+(9*40)+2
.save_lower_l:      lda #0
                    sta 0xD800+(10*40)+1

                    ldx #0
                    lda 0xD800+(9*40)+2+15+5,x
                    sta .save_upper_r+1
                    lda 0xD800+(10*40)+2+15+5,x
                    sta .save_lower_r+1
-                   lda 0xD800+(9*40)+2+15+5+1,x
                    sta 0xD800+(9*40)+2+15+5,x
                    lda 0xD800+(10*40)+2+15+5+1,x
                    sta 0xD800+(10*40)+2+15+5,x
                    inx
                    cpx #16
                    bne -

.save_upper_r:      lda #0
                    sta 0xD800+(9*40)+38
.save_lower_r:      lda #0
                    sta 0xD800+(10*40)+38

                    lda 0xD800+(9*40)+2+1+16
                    sta .save_inner+1
                    lda 0xD800+(9*40)+2+1+16+1
                    sta 0xD800+(9*40)+2+1+16
                    lda 0xD800+(9*40)+2+1+16+2
                    sta 0xD800+(9*40)+2+1+16+1
                    lda 0xD800+(10*40)+2+1+16+2
                    sta 0xD800+(9*40)+2+1+16+2
                    lda 0xD800+(10*40)+2+1+16+1
                    sta 0xD800+(10*40)+2+1+16+2
                    lda 0xD800+(10*40)+2+1+16
                    sta 0xD800+(10*40)+2+1+16+1
                    lda 0xD800+(10*40)+2+1+15
                    sta 0xD800+(10*40)+2+1+16

.save_inner:        lda #0
                    sta 0xD800+(10*40)+2+1+15
                    rts

                    ; discoball 17,1 - 22,6
                    ANIM_DISCOBALL_SPEED = 3
anim_discoball:     lda #ANIM_DISCOBALL_SPEED
                    beq +
                    dec anim_discoball+1
                    rts
+                   lda #ANIM_DISCOBALL_SPEED
                    sta anim_discoball+1

                    lda 0xD800+(1*40)+17
                    sta .disco_buf0+1
                    lda 0xD800+(2*40)+17
                    sta .disco_buf1+1
                    lda 0xD800+(3*40)+17
                    sta .disco_buf2+1
                    lda 0xD800+(4*40)+17
                    sta .disco_buf3+1
                    lda 0xD800+(5*40)+17
                    sta .disco_buf4+1
                    lda 0xD800+(6*40)+17
                    sta .disco_buf5+1

                    ldx #0
-                   lda 0xD800+(1*40)+17+1,x
                    sta 0xD800+(1*40)+17,x
                    lda 0xD800+(2*40)+17+1,x
                    sta 0xD800+(2*40)+17,x
                    lda 0xD800+(3*40)+17+1,x
                    sta 0xD800+(3*40)+17,x
                    lda 0xD800+(4*40)+17+1,x
                    sta 0xD800+(4*40)+17,x
                    lda 0xD800+(5*40)+17+1,x
                    sta 0xD800+(5*40)+17,x
                    lda 0xD800+(6*40)+17+1,x
                    sta 0xD800+(6*40)+17,x
                    inx
                    cpx #5
                    bne -

.disco_buf0:        lda #0
                    sta 0xD800+(1*40)+22
.disco_buf1:        lda #0
                    sta 0xD800+(2*40)+22
.disco_buf2:        lda #0
                    sta 0xD800+(3*40)+22
.disco_buf3:        lda #0
                    sta 0xD800+(4*40)+22
.disco_buf4:        lda #0
                    sta 0xD800+(5*40)+22
.disco_buf5:        lda #0
                    sta 0xD800+(6*40)+22
                    rts

                    ANIM_DISCOSHINE_SPEED = 3
                    ANIM_DISCOSHINE_TABMAX = 17
anim_discoshine:    lda #ANIM_DISCOSHINE_SPEED
                    beq +
                    dec anim_discoshine+1
                    rts
+                   lda #ANIM_DISCOSHINE_SPEED
                    sta anim_discoshine+1

                    ldx .shine_pt0
                    inx
                    cpx #ANIM_DISCOSHINE_TABMAX
                    bne +
                    ldx #0
+                   stx .shine_pt0
                    lda .shinetab,x
                    sta 0xD800+(1*40)+16
                    sta 0xD800+(6*40)+23

                    ldx .shine_pt1
                    inx
                    cpx #ANIM_DISCOSHINE_TABMAX
                    bne +
                    ldx #0
+                   stx .shine_pt1
                    lda .shinetab,x
                    sta 0xD800+(1*40)+23
                    sta 0xD800+(3*40)+15
                    sta 0xD800+(7*40)+20

                    ldx .shine_pt2
                    inx
                    cpx #ANIM_DISCOSHINE_TABMAX
                    bne +
                    ldx #0
+                   stx .shine_pt2
                    lda .shinetab,x
                    sta 0xD800+(3*40)+24
                    sta 0xD800+(6*40)+16

                    rts

.shinetab:          !byte BLACK, DARK_GREY, GREY, BROWN, ORANGE, YELLOW, WHITE, CYAN
                    !byte WHITE, YELLOW, ORANGE, BROWN, GREY, DARK_GREY, BLACK, BLACK
                    !byte BLACK

.shine_pt0:         !byte 6
.shine_pt1:         !byte 10
.shine_pt2:         !byte 14

                    ANIM_FINGER_EORVAL = 103 xor 77
                    ANIM_FINGER_WAIT = 82
                    ANIM_FINGER_SPEED = 5
                    ANIM_FINGER_TIMES = 6
anim_finger:        lda #ANIM_FINGER_WAIT
                    beq +
                    dec anim_finger+1
                    rts
+
.inanim:            lda #ANIM_FINGER_SPEED
                    beq +
                    dec .inanim+1
                    rts
+                   lda #ANIM_FINGER_SPEED
                    sta .inanim+1
.animct:            lda #ANIM_FINGER_TIMES
                    beq +
                    dec .animct+1
                    lda vidmem0+((DISPLAY_START_Y+2+1)*40)+31
                    eor #ANIM_FINGER_EORVAL
                    sta vidmem0+((DISPLAY_START_Y+2+1)*40)+31
                    rts

+                   lda #ANIM_FINGER_TIMES
                    sta .animct+1
                    ldx .af_tab_pt
                    inx
                    cpx #7
                    bne +
                    ldx #0
+                   stx .af_tab_pt
                    lda .anim_finger_waittab,x
                    sta anim_finger+1
                    rts
.anim_finger_waittab:
                    !byte 82, 108, 64, 180, 210, 73, 167
.af_tab_pt:         !byte 0
; ==============================================================================
                    !zone SONGS_EXOMIZED
                    !bin "exo/01-hudbalupinky.exo"
s01_end:            !bin "exo/02-thylyx.exo"
s02_end:            !bin "exo/03-ekp.exo"
s03_end:            !bin "exo/04-bigpack.exo"
s04_end:            !bin "exo/05-ppxt.exo"
s05_end:            !bin "exo/06-discomatic.exo"
s06_end:            !bin "exo/07-trainride2.exo"
s07_end:            !bin "exo/08-cloudywindow.exo"
s08_end:            !bin "exo/09-virtualhugs.exo"
s09_end:            !bin "exo/10-hyggeligekomponist.exo"
s10_end:            !bin "exo/11-loop54.exo"
s11_end:            !bin "exo/12-perblom.exo"
s12_end:            !bin "exo/13-traxcess.exo"
s13_end:
; ==============================================================================
                    !zone SONG_TABLES
songtable:          !byte 01 ; #01
                    !byte 02 ; #02
                    !byte 03 ; #03
                    !byte 04 ; #04
                    !byte 05 ; #05
                    !byte 06 ; #06
                    !byte 07 ; #07
                    !byte 08 ; #08
                    !byte 09 ; #09
                    !byte 10 ; #10
                    !byte 11 ; #11
                    !byte 12 ; #12
                    !byte 13 ; #13
                    !byte 0x00

sdata_pt_lo:        !byte <s01_data, <s02_data, <s03_data, <s04_data
                    !byte <s05_data, <s06_data, <s07_data, <s08_data
                    !byte <s09_data, <s10_data, <s11_data, <s12_data
                    !byte <s13_data

sdata_pt_hi:        !byte >s01_data, >s02_data, >s03_data, >s04_data
                    !byte >s05_data, >s06_data, >s07_data, >s08_data
                    !byte >s09_data, >s10_data, >s11_data, >s12_data
                    !byte >s13_data

s01_data:           !byte <s01_end, >s01_end
                    !byte FLAG_END
                    !scr "03:09"
                    !scr "hudba lupinky (lp version)"
                    !byte 0x00
s02_data:           !byte <s02_end, >s02_end
                    !byte FLAG_END
                    !scr "03:07"
                    !scr "thylyx (album version)"
                    !byte 0x00
s03_data:           !byte <s03_end, >s03_end
                    !byte FLAG_END
                    !scr "03:08"
                    !scr "eine kleine popmusik"
                    !byte 0x00
s04_data:           !byte <s04_end, >s04_end
                    !byte FLAG_END
                    !scr "03:16"
                    !scr "dancing on a bigpack"
                    !byte 0x00
s05_data:           !byte <s05_end, >s05_end
                    !byte FLAG_END
                    !scr "04:25"
                    !scr "partly pieces xtended"
                    !byte 0x00
s06_data:           !byte <s06_end, >s06_end
                    !byte FLAG_END
                    !scr "03:01"
                    !scr "discomatic"
                    !byte 0x00
s07_data:           !byte <s07_end, >s07_end
                    !byte FLAG_END
                    !scr "03:25"
                    !scr "trainride 2"
                    !byte 0x00
s08_data:           !byte <s08_end, >s08_end
                    !byte FLAG_END
                    !scr "03:30"
                    !scr "cloudy window"
                    !byte 0x00
s09_data:           !byte <s09_end, >s09_end
                    !byte FLAG_END
                    !scr "03:09"
                    !scr "virtual hugs"
                    !byte 0x00
s10_data:           !byte <s10_end, >s10_end
                    !byte FLAG_END
                    !scr "03:22"
                    !scr "den hyggelige komponist"
                    !byte 0x00
s11_data:           !byte <s11_end, >s11_end
                    !byte FLAG_LOOP
                    !scr "00:56"
                    !scr "loop 54"
                    !byte 0x00
s12_data:           !byte <s12_end, >s12_end
                    !byte FLAG_END
                    !scr "03:02"
                    !scr "pernilles blomster"
                    !byte 0x00
s13_data:           !byte <s13_end, >s13_end
                    !byte FLAG_END
                    !scr "02:52"
                    !scr "traxcess"
                    !byte 0x00
; ==============================================================================
                    !zone GFX_TABLES
gfx_header_screen:  !bin "gfx/header.scr"
gfx_header_colram:  !bin "gfx/header.col"

gfx_dude_screen:    !bin "gfx/dude.scr"
gfx_dude_colram:    !bin "gfx/dude.col"
; ==============================================================================
*= 0x1000
intro_part:         ldx #0
-                   lda #COLORTEXT
                    sta 0xD800+0x000,x
                    sta 0xD800+0x100,x
                    sta 0xD800+0x200,x
                    sta 0xD800+0x2E8,x
                    inx
                    bne -
                    lda #BLACK
                    sta 0xD020
                    sta 0xD021
                    lda #0x1B
                    sta 0xD011

.intro_timer0:      lda #0xC8
                    beq +
                    dec .intro_timer0+1
                    jsr wait_frame
                    jmp .intro_timer0
+                   lda #0xC8
                    sta .intro_timer0+1
.intro_timer1:      lda #0x02
                    beq +
                    dec .intro_timer1+1
                    jmp .intro_timer0
+
                    rts

wait_frame:         lda #0xFF
-                   cmp 0xD012
                    bne -
                    rts
