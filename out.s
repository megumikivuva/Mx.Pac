; Disassembly of ~\Downloads\ms_pac_man\Ms. Pac-Man.bin
; Disassembled 04/01/21 15:14:26
; Using Stella 6.5.2
;
; ROM properties name : Ms. Pac-Man (1983) (Atari)
; ROM properties MD5  : 87e79cd41ce136fd4f72cc6e2c161bee
; Bankswitch type     : F8* (8K) 
;
; Legend: *  = CODE not yet run (tentative code)
;         D  = DATA directive (referenced in some way)
;         G  = GFX directive, shown as '#' (stored in player, missile, ball)
;         P  = PGFX directive, shown as '*' (stored in playfield)
;         C  = COL directive, shown as color constants (stored in player color)
;         CP = PCOL directive, shown as color constants (stored in playfield color)
;         CB = BCOL directive, shown as color constants (stored in background color)
;         A  = AUD directive (stored in audio registers)
;         i  = indexed accessed only
;         c  = used by code executed in RAM
;         s  = used by stack
;         !  = page crossed, 1 cycle penalty

    processor 6502


;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

BLACK            = $00
YELLOW           = $10
BROWN            = $20
ORANGE           = $30
RED              = $40
MAUVE            = $50
VIOLET           = $60
PURPLE           = $70
BLUE             = $80
BLUE_CYAN        = $90
CYAN             = $a0
CYAN_GREEN       = $b0
GREEN            = $c0
GREEN_YELLOW     = $d0
GREEN_BEIGE      = $e0
BEIGE            = $f0


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

CXP0FB          = $02  ; (R)
;CXP1FB         = $03  ; (Ri)
;CXM0FB         = $04  ; (Ri)
;CXM1FB         = $05  ; (Ri)
;CXBLPF         = $06  ; (Ri)
;CXPPMM         = $07  ; (Ri)
;INPT0          = $08  ; (Ri)
;INPT1          = $09  ; (Ri)
;INPT2          = $0a  ; (Ri)
;INPT3          = $0b  ; (Ri)
INPT4           = $0c  ; (R)
;INPT5          = $0d  ; (Ri)

VSYNC           = $00  ; (W)
VBLANK          = $01  ; (W)
WSYNC           = $02  ; (W)
NUSIZ0          = $04  ; (W)
NUSIZ1          = $05  ; (W)
COLUP0          = $06  ; (W)
COLUP1          = $07  ; (W)
COLUPF          = $08  ; (W)
COLUBK          = $09  ; (W)
CTRLPF          = $0a  ; (W)
REFP1           = $0c  ; (W)
PF0             = $0d  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)
RESBL           = $14  ; (W)
AUDC0           = $15  ; (W)
AUDC1           = $16  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)
AUDV0           = $19  ; (W)
AUDV1           = $1a  ; (W)
GRP0            = $1b  ; (W)
GRP1            = $1c  ; (W)
ENABL           = $1f  ; (W)
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
HMBL            = $24  ; (W)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
HMOVE           = $2a  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM64T          = $0296


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80
ram_81          = $81
ram_82          = $82
ram_83          = $83
ram_84          = $84
ram_85          = $85
ram_86          = $86
ram_87          = $87
ram_88          = $88
ram_89          = $89
ram_8A          = $8a
ram_8B          = $8b
ram_8C          = $8c
ram_8D          = $8d
ram_8E          = $8e
ram_8F          = $8f
ram_90          = $90
ram_91          = $91
ram_92          = $92
ram_93          = $93
ram_94          = $94
ram_95          = $95
ram_96          = $96

ram_98          = $98
ram_99          = $99
ram_9A          = $9a
ram_9B          = $9b
ram_9C          = $9c
ram_9D          = $9d
ram_9E          = $9e
ram_9F          = $9f
ram_A0          = $a0
ram_A1          = $a1
ram_A2          = $a2
ram_A3          = $a3
;                 $a4  (i)

ram_A7          = $a7

ram_A9          = $a9
;                 $aa  (i)

ram_AD          = $ad
ram_AE          = $ae
ram_AF          = $af
;                 $b0  (i)

ram_B2          = $b2
ram_B3          = $b3
;                 $b4  (i)
ram_B5          = $b5

ram_B7          = $b7
ram_B8          = $b8
ram_B9          = $b9
ram_BA          = $ba
ram_BB          = $bb
ram_BC          = $bc
ram_BD          = $bd
;                 $be  (i)
;                 $bf  (i)
;                 $c0  (i)
;                 $c1  (i)
;                 $c2  (i)
;                 $c3  (i)
;                 $c4  (i)
;                 $c5  (i)
;                 $c6  (i)
;                 $c7  (i)
;                 $c8  (i)
;                 $c9  (i)
;                 $ca  (i)
;                 $cb  (i)
;                 $cc  (i)
;                 $cd  (i)
;                 $ce  (i)
;                 $cf  (i)
;                 $d0  (i)
;                 $d1  (i)
;                 $d2  (i)
;                 $d3  (i)
;                 $d4  (i)
;                 $d5  (i)
;                 $d6  (i)
;                 $d7  (i)
;                 $d8  (i)
;                 $d9  (i)
;                 $da  (i)
;                 $db  (i)
;                 $dc  (i)
;                 $dd  (i)
;                 $de  (i)
;                 $df  (i)
;                 $e0  (i)
;                 $e1  (i)
;                 $e2  (i)
;                 $e3  (i)
;                 $e4  (i)
ram_E5          = $e5
ram_E6          = $e6
ram_E7          = $e7
ram_E8          = $e8
ram_E9          = $e9
ram_EA          = $ea
ram_EB          = $eb
ram_EC          = $ec; (s)
ram_ED          = $ed; (s)
ram_EE          = $ee; (s)
ram_EF          = $ef
ram_F0          = $f0
ram_F1          = $f1
ram_F2          = $f2
ram_F3          = $f3
ram_F4          = $f4
ram_F5          = $f5
ram_F6          = $f6
ram_F7          = $f7
ram_F8          = $f8
ram_F9          = $f9
ram_FA          = $fa
ram_FB          = $fb
;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f000


;***********************************************************
;      Bank 0 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $0000
    RORG    $d000

    sta     $fff9                   ;4   =   4 *
Ld003
    SLO     ENABL                   ;5         *
    .byte   $3f ;RLA                ;7-7 =   5 *
    .byte   $7f ;RRA                ;7-3 =   4 *
Ld007
    NOP     RESP0                   ;3         *
    .byte   $20 ;jsr                ;6-6 =   3 *
    rti                             ;6   =   6 *
    
Ld00b
    jsr     Ldbc8                   ;6        
    lda     ram_9E                  ;3        
    lsr                             ;2        
    sta     REFP1                   ;3        
    lda     ram_94                  ;3        
    bmi     Ld05a                   ;2/3      
    lda     ram_F6                  ;3        
    tay                             ;2        
    bmi     Ld033                   ;2/3      
    lda     ram_9E                  ;3        
    cmp     #$80                    ;2        
    bne     Ld08b                   ;2/3      
    tya                             ;2         *
    and     #$f8                    ;2         *
    sta     ram_F6                  ;3         *
    tya                             ;2         *
    and     #$07                    ;2   =  44 *
Ld02a
    clc                             ;2         *
    adc     #$01                    ;2         *
    cmp     #$03                    ;2         *
    bne     Ld053                   ;2/3       *
    beq     Ld05a                   ;2/3 =  10 *
Ld033
    lda     ram_9E                  ;3         *
    ldx     ram_80                  ;3         *
    and     Ld003,x                 ;4         *
    cmp     Ld007,x                 ;4         *
    bne     Ld08b                   ;2/3       *
    lda     ram_F6                  ;3         *
    tax                             ;2         *
    bpl     Ld08b                   ;2/3       *
    txa                             ;2         *
    and     #$f8                    ;2         *
    sta     ram_F6                  ;3         *
    txa                             ;2         *
    and     #$07                    ;2         *
    clc                             ;2         *
    adc     #$ff                    ;2         *
    cmp     #$ff                    ;2         *
    beq     Ld05a                   ;2/3 =  42 *
Ld053
    ora     ram_F6                  ;3         *
    sta     ram_F6                  ;3         *
    jmp     Ld08b                   ;3   =   9 *
    
Ld05a
    ldx     #$03                    ;2   =   2 *
Ld05c
    lda     ram_94                  ;3         *
    bpl     Ld064                   ;2/3       *
    lda     #$03                    ;2         *
    bne     Ld066                   ;2/3 =   9 *
Ld064
    lda     #$83                    ;2   =   2 *
Ld066
    and     ram_81,x                ;4         *
    sta     ram_B7                  ;3         *
    lda     ram_8C,x                ;4         *
    eor     ram_86,x                ;4         *
    adc     ram_8A                  ;3         *
    eor     ram_90                  ;3         *
    and     #$03                    ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    ora     ram_B7                  ;3         *
    sta     ram_81,x                ;4         *
    dex                             ;2         *
    bpl     Ld05c                   ;2/3       *
    lda     ram_F6                  ;3         *
    and     #$78                    ;2         *
    sta     ram_F6                  ;3         *
    lda     ram_92                  ;3         *
    and     #$f0                    ;2         *
    sta     ram_92                  ;3   =  58 *
Ld08b
    lda     #$ff                    ;2        
    sta     PF0                     ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    sta     VDELP0                  ;3        
    lda     #$dd                    ;2        
    sta     ram_F3                  ;3        
    lda     #$0e                    ;2        
    sta     ram_99                  ;3        
    lda     #$00                    ;2        
    sta     ram_B7                  ;3        
    ldy     ram_80                  ;3        
    ldx     Ldca8,y                 ;4        
    lda     Ldc00,x                 ;4        
    sta     ram_B8                  ;3        
    inx                             ;2        
    lda     Ldc00,x                 ;4        
    sta     ram_B9                  ;3        
    inx                             ;2        
    lda     Ldc00,x                 ;4        
    sta     ram_BA                  ;3        
    inx                             ;2        
    stx     ram_A0                  ;3        
    lda     ram_BB                  ;3        
    and     #$40                    ;2        
    ora     ram_B8                  ;3        
    sta     ram_E5                  ;3        
    lda     ram_BC                  ;3        
    and     #$aa                    ;2        
    ora     ram_B9                  ;3        
    sta     ram_E6                  ;3        
    lda     ram_BD                  ;3        
    and     #$55                    ;2        
    ora     ram_BA                  ;3        
    sta     ram_E7                  ;3        
    sta     WSYNC                   ;3   = 102
;---------------------------------------
    lda     ram_9E                  ;3        
    and     #$08                    ;2        
    beq     Ld0df                   ;2/3      
    ldx     #$02                    ;2        
    jmp     Ld0e6                   ;3   =  12
    
Ld0df
    ldx     #$0b                    ;2        
    lda     #$70                    ;2        
    sta     HMBL|$100               ;4   =   8
Ld0e6
    dex                             ;2        
    bne     Ld0e6                   ;2/3      
    sta     RESBL                   ;3        
    lda     #$0f                    ;2        
    sta     ram_EB                  ;3        
    sta     ram_ED                  ;3        
    ldx     ram_A2                  ;3        
    bmi     Ld117                   ;2/3!     
    lda     ram_9C,x                ;4        
    sta     HMP0                    ;3        
    and     #$0f                    ;2        
    sta     ram_EC                  ;3        
    lda     ram_B3,x                ;4        
    sta     ram_F0                  ;3        
    lda     ram_B5,x                ;4        
    sta     ram_F1                  ;3        
    lda     ram_AD,x                ;4        
    and     #$0f                    ;2        
    sta     ram_EB                  ;3        
    lda     ram_AD,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Ld9e3,x                 ;4        
    sta     COLUP0                  ;3   =  76
Ld117
    ldx     ram_A1                  ;3        
    bmi     Ld139                   ;2/3      
    lda     ram_A3,x                ;4        
    sta     HMP1                    ;3        
    and     #$0f                    ;2        
    sta     ram_EE                  ;3        
    lda     ram_AF,x                ;4        
    sta     ram_F2                  ;3        
    lda     ram_A9,x                ;4        
    and     #$0f                    ;2        
    sta     ram_ED                  ;3        
    lda     ram_A9,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Ldb82,x                 ;4        
    sta     COLUP1                  ;3   =  54
Ld139
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_EB                  ;3        
    beq     Ld143                   ;2/3      
    dec     ram_EB                  ;5        
    bpl     Ld175                   ;2/3 =  12
Ld143
    lda     #$c0                    ;2        
    sta     ram_EB                  ;3        
    ldx     ram_EC|$100             ;4        
    beq     Ld173                   ;2/3      
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3       *
    dex                             ;2         *
    beq     Ld173                   ;2/3 =  63 *
Ld173
    sta     RESP0                   ;3   =   3
Ld175
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_ED                  ;3        
    beq     Ld17f                   ;2/3      
    dec     ram_ED                  ;5        
    bpl     Ld1b1                   ;2/3 =  12
Ld17f
    lda     #$c0                    ;2        
    sta     ram_ED                  ;3        
    ldx     ram_EE|$100             ;4        
    beq     Ld1af                   ;2/3      
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3       *
    dex                             ;2         *
    beq     Ld1af                   ;2/3 =  63 *
Ld1af
    sta     RESP1                   ;3   =   3
Ld1b1
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    bit     ram_EB                  ;3        
    bvc     Ld1bf                   ;2/3      
    lda     #$00                    ;2        
    sta     HMP0                    ;3   =  10
Ld1bf
    sta     WSYNC                   ;3   =   3
;---------------------------------------
Ld1c1
    ldx     INTIM                   ;4        
    bne     Ld1c1                   ;2/3      
    ldy     #$01                    ;2        
    lda     #$00                    ;2        
    sta     HMBL                    ;3        
    sta     WSYNC                   ;3   =  16
;---------------------------------------
    sta     VBLANK                  ;3        
    ldx     #$0e                    ;2   =   5
Ld1d2
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_B8                  ;3        
    sta     PF0                     ;3        
    bit     ram_ED                  ;3        
    bpl     Ld1f6                   ;2/3      
    lda     (ram_F2),y              ;5        
    sta     GRP1                    ;3        
    lda     ram_B9                  ;3        
    sta     PF1                     ;3        
    lda     ram_BA                  ;3        
    ora     Ldcac,x                 ;4        
    sta     PF2                     ;3        
    iny                             ;2        
    lda     (ram_F2),y              ;5        
    tax                             ;2        
    lda     #$00                    ;2        
    sta     HMP1                    ;3        
    jmp     Ld20d                   ;3   =  52
    
Ld1f6
    lda     #$00                    ;2        
    sta     GRP1                    ;3        
    lda     ram_B9                  ;3        
    sta     PF1                     ;3        
    lda     ram_BA                  ;3        
    ora     Ldcac,x                 ;4        
    sta     PF2|$100                ;4        
    iny                             ;2        
    ldx     #$00                    ;2        
    pha                             ;3        
    pla                             ;4        
    pha                             ;3        
    pla                             ;4   =  40
Ld20d
    cpy     #$0e                    ;2        
    bne     Ld21b                   ;2/3      
    lda     ram_F5                  ;3        
    sta     ENABL                   ;3        
    asl                             ;2        
    sta     ram_F5                  ;3        
    jmp     Ld22a                   ;3   =  18
    
Ld21b
    cpy     #$92                    ;2        
    bne     Ld226                   ;2/3      
    lda     ram_F5                  ;3        
    sta     ENABL                   ;3        
    jmp     Ld22a                   ;3   =  13
    
Ld226
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2   =   8
Ld22a
    bit     ram_EB                  ;3        
    bvs     Ld261                   ;2/3      
    bpl     Ld27b                   ;2/3      
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    ldx     ram_A2                  ;3        
    bmi     Ld26f                   ;2/3      
    lda     ram_9C,x                ;4         *
    sta     HMP0                    ;3         *
    and     #$0f                    ;2         *
    sta     ram_EC                  ;3         *
    lda     ram_B3,x                ;4         *
    sta     ram_F0                  ;3         *
    lda     ram_B5,x                ;4         *
    sta     ram_F1                  ;3         *
    lda     ram_AD,x                ;4         *
    and     #$0f                    ;2         *
    sta     ram_EB                  ;3         *
    lda     ram_AD,x                ;4         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tax                             ;2         *
    lda     Ld9e3,x                 ;4         *
    sta     COLUP0|$100             ;4         *
    iny                             ;2         *
    jmp     Ld284                   ;3   =  82 *
    
Ld261
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    iny                             ;2        
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    jmp     Ld282                   ;3   =  24
    
Ld26f
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    lda     #$0f                    ;2        
    sta     ram_EB                  ;3        
    iny                             ;2        
    jmp     Ld282                   ;3   =  15
    
Ld27b
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    iny                             ;2   =  10
Ld282
    sta     WSYNC                   ;3   =   3
;---------------------------------------
Ld284
    bit     ram_ED                  ;3        
    bmi     Ld2b0                   ;2/3      
    lda     #$00                    ;2        
    sta     GRP1                    ;3        
    ldx     ram_B7                  ;3        
    lda     ram_BD,x                ;4        
    and     #$55                    ;2        
    ora     ram_BA                  ;3        
    sta     ram_E7                  ;3        
    lda     ram_BC,x                ;4        
    and     #$55                    ;2        
    ora     ram_BA                  ;3        
    sta     ram_E8                  ;3        
    lda     ram_BD,x                ;4        
    and     #$aa                    ;2        
    ora     ram_B9                  ;3        
    sta     ram_E9                  ;3        
    ldx     #$00                    ;2        
    iny                             ;2        
    lda     ram_EB                  ;3        
    beq     Ld2fe                   ;2/3      
    jmp     Ld2d6                   ;3   =  61
    
Ld2b0
    lda     (ram_F2),y              ;5        
    sta     GRP1                    ;3        
    iny                             ;2        
    ldx     ram_B7                  ;3        
    lda     ram_BD,x                ;4        
    and     #$55                    ;2        
    ora     ram_BA                  ;3        
    sta     ram_E7                  ;3        
    lda     ram_BC,x                ;4        
    and     #$55                    ;2        
    ora     ram_BA                  ;3        
    sta     ram_E8                  ;3        
    lda     ram_BD,x                ;4        
    and     #$aa                    ;2        
    ora     ram_B9                  ;3        
    sta     ram_E9                  ;3        
    lda     (ram_F2),y              ;5        
    tax                             ;2        
    lda     ram_EB                  ;3        
    beq     Ld2fe                   ;2/3 =  61
Ld2d6
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    cmp     #$c0                    ;2        
    beq     Ld2e8                   ;2/3      
    dec     ram_EB                  ;5        
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    iny                             ;2        
    jmp     Ld335                   ;3   =  22
    
Ld2e8
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    iny                             ;2        
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    lda     #$80                    ;2        
    sta     ram_EB                  ;3        
    dec     ram_A2                  ;5        
    lda     #$00                    ;2        
    jmp     Ld335                   ;3   =  36
    
Ld2fe
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    stx     GRP1                    ;3        
    lda     #$c0                    ;2        
    sta     ram_EB                  ;3        
    iny                             ;2        
    nop                             ;2        
    ldx     ram_EC                  ;3        
    beq     Ld333                   ;2/3      
    dex                             ;2        
    beq     Ld333                   ;2/3      
    dex                             ;2        
    beq     Ld333                   ;2/3      
    dex                             ;2        
    beq     Ld333                   ;2/3      
    dex                             ;2        
    beq     Ld333                   ;2/3      
    dex                             ;2        
    beq     Ld333                   ;2/3      
    dex                             ;2        
    beq     Ld333                   ;2/3      
    dex                             ;2         *
    beq     Ld333                   ;2/3       *
    dex                             ;2         *
    beq     Ld333                   ;2/3       *
    dex                             ;2         *
    beq     Ld333                   ;2/3       *
    dex                             ;2         *
    beq     Ld333                   ;2/3       *
    dex                             ;2         *
    beq     Ld333                   ;2/3       *
    dex                             ;2         *
    beq     Ld333                   ;2/3       *
    dex                             ;2         *
    beq     Ld333                   ;2/3 =  69 *
Ld333
    sta     RESP0                   ;3   =   3
Ld335
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    bit     ram_ED                  ;3        
    bmi     Ld356                   ;2/3      
    lda     #$00                    ;2        
    sta     GRP1                    ;3        
    ldx     ram_B7                  ;3        
    lda     ram_BB,x                ;4        
    asl                             ;2        
    asl                             ;2        
    ora     ram_B8                  ;3        
    sta     ram_EA                  ;3        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    stx     ram_B7                  ;3        
    iny                             ;2        
    ldx     #$00                    ;2        
    jmp     Ld36d                   ;3   =  46
    
Ld356
    lda     (ram_F2),y              ;5        
    sta     GRP1                    ;3        
    iny                             ;2        
    ldx     ram_B7                  ;3        
    lda     ram_BB,x                ;4        
    asl                             ;2        
    asl                             ;2        
    ora     ram_B8                  ;3        
    sta     ram_EA                  ;3        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    stx     ram_B7                  ;3        
    lda     (ram_F2),y              ;5        
    tax                             ;2   =  43
Ld36d
    lda     #$00                    ;2        
    bit     ram_EB                  ;3        
    bpl     Ld375                   ;2/3      
    lda     (ram_F0),y              ;5   =  12
Ld375
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    lda     ram_E5                  ;3        
    sta     PF0                     ;3        
    lda     ram_E6                  ;3        
    sta     PF1                     ;3        
    lda     ram_E7                  ;3        
    sta     PF2                     ;3        
    iny                             ;2        
    bit     ram_EB                  ;3        
    bpl     Ld393                   ;2/3      
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    jmp     Ld399                   ;3   =  42
    
Ld393
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    lda     ram_E8                  ;3   =  11
Ld399
    lda     ram_E8                  ;3        
    sta     PF2                     ;3        
    lda     ram_E9                  ;3        
    sta     PF1                     ;3        
    lda     ram_EA                  ;3        
    sta     PF0                     ;3        
    bit     ram_ED                  ;3        
    bmi     Ld3ae                   ;2/3      
    lda     #$00                    ;2        
    jmp     Ld3b0                   ;3   =  28
    
Ld3ae
    lda     (ram_F2),y              ;5   =   5
Ld3b0
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     GRP1                    ;3        
    lda     ram_E5                  ;3        
    sta     PF0                     ;3        
    lda     ram_E6                  ;3        
    sta     PF1                     ;3        
    lda     ram_E7                  ;3        
    sta     PF2                     ;3        
    sta     PF2                     ;3        
    iny                             ;2        
    bit     ram_ED|$100             ;4        
    bpl     Ld3ce                   ;2/3      
    lda     (ram_F2),y              ;5        
    tax                             ;2        
    jmp     Ld3d4                   ;3   =  42
    
Ld3ce
    ldx     #$00                    ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2   =  10
Ld3d4
    lda     ram_E8                  ;3        
    sta     PF2                     ;3        
    lda     ram_E9                  ;3        
    sta     PF1                     ;3        
    lda     ram_EA                  ;3        
    sta     PF0                     ;3        
    lda     ram_B8                  ;3        
    bit     ram_EB                  ;3        
    bpl     Ld416                   ;2/3!     
    sta     WSYNC                   ;3   =  29
;---------------------------------------
    sta     PF0                     ;3        
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    lda     ram_B9                  ;3        
    sta     PF1                     ;3        
    ldx     ram_99                  ;3        
    lda     ram_BA                  ;3        
    ora     Ldcbb,x                 ;4        
    sta     PF2                     ;3        
    lda     #$00                    ;2        
    sta     ENABL                   ;3        
    sta     HMP0                    ;3        
    iny                             ;2        
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    ldx     ram_A0                  ;3        
    lda     Ldc00,x                 ;4        
    sta     ram_B8                  ;3        
    bit     ram_ED                  ;3        
    bmi     Ld453                   ;2/3      
    jmp     Ld439                   ;3   =  69
    
Ld416
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    stx     GRP1                    ;3        
    sta     PF0                     ;3        
    lda     ram_B9                  ;3        
    sta     PF1                     ;3        
    ldx     ram_99                  ;3        
    lda     ram_BA                  ;3        
    ora     Ldcbb,x                 ;4        
    sta     PF2                     ;3        
    lda     #$00                    ;2        
    sta     ENABL                   ;3        
    iny                             ;2        
    ldx     ram_A0                  ;3        
    lda     Ldc00,x                 ;4        
    sta     ram_B8                  ;3        
    bit     ram_ED                  ;3        
    bmi     Ld453                   ;2/3 =  47
Ld439
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2        
    sta     GRP1                    ;3        
    iny                             ;2        
    inx                             ;2        
    lda     Ldc00,x                 ;4        
    sta     ram_B9                  ;3        
    inx                             ;2        
    lda     Ldc00,x                 ;4        
    sta     ram_BA                  ;3        
    inx                             ;2        
    stx     ram_A0                  ;3        
    ldx     #$00                    ;2        
    beq     Ld480                   ;2/3 =  34
Ld453
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     (ram_F2),y              ;5        
    sta     GRP1                    ;3        
    iny                             ;2        
    inx                             ;2        
    lda     Ldc00,x                 ;4        
    sta     ram_B9                  ;3        
    inx                             ;2        
    lda     Ldc00,x                 ;4        
    sta     ram_BA                  ;3        
    inx                             ;2        
    stx     ram_A0                  ;3        
    bit     ram_ED                  ;3        
    bvs     Ld47d                   ;2/3      
    ldx     #$00                    ;2        
    bit     ram_EB                  ;3        
    bpl     Ld49f                   ;2/3      
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    iny                             ;2        
    lda     (ram_F0),y              ;5        
    jmp     Ld4a2                   ;3   =  63
    
Ld47d
    lda     (ram_F2),y              ;5        
    tax                             ;2   =   7
Ld480
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    bit     ram_EB                  ;3        
    bpl     Ld494                   ;2/3      
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    iny                             ;2        
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    jmp     Ld497                   ;3   =  29
    
Ld494
    stx     GRP1                    ;3        
    iny                             ;2   =   5
Ld497
    lda     ram_ED                  ;3        
    beq     Ld502                   ;2/3!     
    bmi     Ld4d1                   ;2/3      
    bpl     Ld4f4                   ;2/3 =   9
Ld49f
    lda     #$00                    ;2        
    iny                             ;2   =   4
Ld4a2
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    ldx     ram_A1                  ;3        
    bmi     Ld4e4                   ;2/3      
    lda     ram_A3,x                ;4        
    sta     HMP1                    ;3        
    and     #$0f                    ;2        
    sta     ram_EE                  ;3        
    lda     ram_AF,x                ;4        
    sta     ram_F2                  ;3        
    lda     ram_A9,x                ;4        
    and     #$0f                    ;2        
    sta     ram_ED                  ;3        
    lda     ram_A9,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Ldb82,x                 ;4        
    sta     COLUP1                  ;3        
    lda     ram_ED                  ;3        
    beq     Ld502                   ;2/3!     
    jmp     Ld4f4                   ;3   =  68 *
    
Ld4d1
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     (ram_F2),y              ;5        
    sta     GRP1                    ;3        
    iny                             ;2        
    lda     (ram_F2),y              ;5        
    tax                             ;2        
    dec     ram_A1                  ;5        
    lda     #$80                    ;2        
    sta     ram_ED                  ;3        
    jmp     Ld539                   ;3   =  30
    
Ld4e4
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2        
    sta     GRP1                    ;3        
    lda     #$0f                    ;2        
    sta     ram_ED                  ;3        
    ldx     #$00                    ;2        
    iny                             ;2        
    jmp     Ld539                   ;3   =  17
    
Ld4f4
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2        
    sta     GRP1                    ;3        
    ldx     #$00                    ;2        
    dec     ram_ED                  ;5        
    iny                             ;2        
    jmp     Ld539                   ;3   =  17
    
Ld502
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     GRP1                    ;3        
    lda     #$c0                    ;2        
    sta     ram_ED                  ;3        
    iny                             ;2        
    nop                             ;2        
    ldx     ram_EE                  ;3        
    beq     Ld537                   ;2/3      
    dex                             ;2        
    beq     Ld537                   ;2/3      
    dex                             ;2        
    beq     Ld537                   ;2/3      
    dex                             ;2        
    beq     Ld537                   ;2/3      
    dex                             ;2        
    beq     Ld537                   ;2/3      
    dex                             ;2        
    beq     Ld537                   ;2/3      
    dex                             ;2        
    beq     Ld537                   ;2/3      
    dex                             ;2         *
    beq     Ld537                   ;2/3       *
    dex                             ;2         *
    beq     Ld537                   ;2/3       *
    dex                             ;2         *
    beq     Ld537                   ;2/3       *
    dex                             ;2         *
    beq     Ld537                   ;2/3       *
    dex                             ;2         *
    beq     Ld537                   ;2/3       *
    dex                             ;2         *
    beq     Ld537                   ;2/3       *
    dex                             ;2         *
    beq     Ld537                   ;2/3 =  69 *
Ld537
    sta     RESP1                   ;3   =   3
Ld539
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    bit     ram_EB                  ;3        
    bpl     Ld567                   ;2/3      
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    iny                             ;2        
    lda     (ram_F0),y              ;5        
    sta     GRP0                    ;3        
    ldx     ram_B7                  ;3        
    lda     ram_BB,x                ;4        
    and     #$40                    ;2        
    ora     ram_B8                  ;3        
    sta     ram_E5                  ;3        
    lda     ram_BC,x                ;4        
    and     #$aa                    ;2        
    ora     ram_B9                  ;3        
    sta     ram_E6                  ;3        
    dec     ram_99                  ;5        
    ldx     ram_99                  ;3        
    beq     Ld585                   ;2/3      
    jmp     Ld1d2                   ;3   =  69
    
Ld567
    stx     GRP1                    ;3        
    iny                             ;2        
    ldx     ram_B7                  ;3        
    lda     ram_BB,x                ;4        
    and     #$40                    ;2        
    ora     ram_B8                  ;3        
    sta     ram_E5                  ;3        
    lda     ram_BC,x                ;4        
    and     #$aa                    ;2        
    ora     ram_B9                  ;3        
    sta     ram_E6                  ;3        
    dec     ram_99                  ;5        
    ldx     ram_99                  ;3        
    beq     Ld585                   ;2/3      
    jmp     Ld1d2                   ;3   =  45
    
Ld585
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$ff                    ;2        
    sta     PF0                     ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    lda     ram_FB                  ;3        
    and     #$03                    ;2        
    tay                             ;2        
    lda     Ld5b9,y                 ;4        
    sta     RESP0                   ;3        
    sta     NUSIZ0                  ;3        
    lda     Ld5be,y                 ;4        
    sta     COLUP0                  ;3        
    lda     ram_93                  ;3        
    cmp     #$03                    ;2        
    beq     Ld5c2                   ;2/3      
    eor     #$03                    ;2         *
    tax                             ;2         *
    lda     Ld5bb,x                 ;4   =  50 *
Ld5ac
    dex                             ;2         *
    bne     Ld5ac                   ;2/3       *
    sta     RESP1                   ;3         *
    ldy     #$07                    ;2         *
    sta     NUSIZ1                  ;3         *
    lda     #$26                    ;2         *
    bne     Ld5ce                   ;2/3 =  16 *
    
Ld5b9
    .byte   $00                             ; $d5b9 (D)
    .byte   $00                             ; $d5ba (*)
Ld5bb
    .byte   $01,$03,$01                     ; $d5bb (*)
    
Ld5be
    .byte   BLACK|$0                        ; $d5be (C)
    
    .byte   $18,$18,$18                     ; $d5bf (*)
    
Ld5c2
    lda     ram_FB                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Ld9e3,y                 ;4        
    sta     RESP1                   ;3   =  20
Ld5ce
    sta     COLUP1                  ;3        
    lda     Ld9eb,y                 ;4        
    sta     ram_E5                  ;3        
    lda     Ld9f3,y                 ;4        
    sta     ram_E6                  ;3        
    lsr     ram_F5                  ;5        
    lda     #$d9                    ;2        
    sta     ram_E8                  ;3        
    sta     ram_EA                  ;3        
    sta     ram_EC                  ;3        
    sta     ram_EE                  ;3        
    sta     ram_F0                  ;3        
    sta     ram_F2                  ;3        
    ldy     #$02                    ;2        
    sty     ram_B9                  ;3        
    ldx     #$00                    ;2        
    stx     ram_B8                  ;3        
    ldx     #$0a                    ;2        
    stx     ram_B7                  ;3        
    sta     WSYNC                   ;3   =  60
;---------------------------------------
    ldy     #$00                    ;2        
    sty     PF0                     ;3        
    sty     PF1                     ;3        
    sty     PF2                     ;3        
    sty     REFP1                   ;3        
    sty     COLUBK                  ;3        
    bit     ram_94                  ;3        
    bpl     Ld60c                   ;2/3      
    sty     COLUP0                  ;3         *
    sty     COLUP1                  ;3   =  28 *
Ld60c
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     SWCHA                   ;4        
    cmp     #$ff                    ;2        
    beq     Ld617                   ;2/3      
    sty     ram_98                  ;3   =  11 *
Ld617
    lda     Lde64,y                 ;4        
    sta     GRP0                    ;3        
    lda     (ram_E5),y              ;5        
    sta     GRP1                    ;3        
    inc     ram_B8                  ;5        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
Ld624
    ldy     ram_B8                  ;3        
    lda     Lde64,y                 ;4        
    sta     GRP0                    ;3        
    lda     (ram_E5),y              ;5        
    sta     GRP1                    ;3        
    ldy     ram_B9                  ;3        
    lda.wy  ram_F8,y                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    bne     Ld642                   ;2/3      
    lda     #$0a                    ;2        
    cmp     ram_B7                  ;3        
    beq     Ld642                   ;2/3      
    lda     #$00                    ;2   =  44
Ld642
    sta     ram_B7                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$2b                    ;2        
    sta     ram_E7,x                ;4        
    dex                             ;2        
    dex                             ;2        
    inc     ram_B8                  ;5        
    sta     WSYNC                   ;3   =  27
;---------------------------------------
    ldy     ram_B8                  ;3        
    lda     Lde64,y                 ;4        
    sta     GRP0                    ;3        
    lda     (ram_E5),y              ;5        
    sta     GRP1                    ;3        
    ldy     ram_B9                  ;3        
    lda.wy  ram_F8,y                ;4        
    and     #$0f                    ;2        
    bne     Ld66d                   ;2/3      
    lda     #$0a                    ;2        
    cmp     ram_B7                  ;3        
    beq     Ld66d                   ;2/3      
    lda     #$00                    ;2   =  38
Ld66d
    sta     ram_B7                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$2b                    ;2        
    sta     ram_E7,x                ;4        
    dex                             ;2        
    dex                             ;2        
    inc     ram_B8                  ;5        
    dec     ram_B9                  ;5        
    sta     WSYNC                   ;3   =  32
;---------------------------------------
    bpl     Ld624                   ;2/3      
    ldy     ram_B8                  ;3        
    lda     Lde64,y                 ;4        
    sta     GRP0                    ;3        
    lda     (ram_E5),y              ;5        
    sta     GRP1                    ;3        
    iny                             ;2        
    lda     #$10                    ;2        
    and     ram_94                  ;3        
    beq     Ld6ad                   ;2/3      
    lda     #$b3                    ;2   =  31 *
Ld694
    sta     ram_F1                  ;3        
    clc                             ;2        
    adc     #$08                    ;2        
    sta     ram_EF                  ;3        
    adc     #$08                    ;2        
    sta     ram_ED                  ;3        
    adc     #$08                    ;2        
    sta     ram_EB                  ;3        
    adc     #$08                    ;2        
    sta     ram_E9                  ;3        
    adc     #$08                    ;2        
    sta     ram_E7                  ;3        
    bne     Ld6b9                   ;2/3 =  32
Ld6ad
    lda     #$83                    ;2        
    ldx     ram_FA                  ;3        
    cpx     #$ff                    ;2        
    beq     Ld694                   ;2/3      
    lda     #$2b                    ;2         *
    sta     ram_E7                  ;3   =  14 *
Ld6b9
    sta     WSYNC                   ;3   =   3
;---------------------------------------
Ld6bb
    lda     Lde64,y                 ;4        
    sta     GRP0                    ;3        
    lda     (ram_E5),y              ;5        
    sta     GRP1                    ;3        
    iny                             ;2        
    sta     WSYNC                   ;3   =  20
;---------------------------------------
    cpy     #$0a                    ;2        
    bne     Ld6bb                   ;2/3      
    ldx     #$00                    ;2        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     GRP0                    ;3        
    stx     COLUP1                  ;3        
    lda     #$10                    ;2        
    and     ram_94                  ;3        
    beq     Ld6e3                   ;2/3      
    lda     #$54                    ;2         *
    sta     COLUPF                  ;3         *
    lda     #$ff                    ;2         *
    sta     PF2                     ;3   =  35 *
Ld6e3
    stx     HMP1                    ;3        
    inx                             ;2        
    lda     #$03                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    ldy     #$07                    ;2        
    stx     VDELP0                  ;3        
    stx     VDELP1                  ;3        
    sty     ram_B8                  ;3        
    dey                             ;2        
    sty     WSYNC                   ;3   =  29
;---------------------------------------
Ld6f7
    dey                             ;2        
    bpl     Ld6f7                   ;2/3      
    nop                             ;2        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    lda     #$f0                    ;2        
    sta     HMP0                    ;3        
    sta     WSYNC                   ;3   =  20
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_98                  ;3        
    and     #$10                    ;2        
    beq     Ld713                   ;2/3      
    lda     ram_94                  ;3         *
    and     #$10                    ;2         *
    beq     Ld719                   ;2/3 =  17 *
Ld713
    ldx     #$28                    ;2        
    stx     COLUP0                  ;3        
    stx     COLUP1                  ;3   =   8
Ld719
    ldy     ram_B8                  ;3        
    lda     (ram_F1),y              ;5        
    sta     GRP0                    ;3        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    lda     (ram_EF),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_ED),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_EB),y              ;5        
    sta     ram_B7                  ;3        
    lda     (ram_E9),y              ;5        
    tax                             ;2        
    lda     (ram_E7),y              ;5        
    tay                             ;2        
    lda     ram_B7                  ;3        
    sta     GRP1                    ;3        
    stx     GRP0                    ;3        
    sty     GRP1                    ;3        
    sta     GRP0                    ;3        
    dec     ram_B8                  ;5        
    bpl     Ld719                   ;2/3      
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     GRP0                    ;3        
    sta     HMP0                    ;3        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    sta     PF2                     ;3        
    lda     #$24                    ;2        
    sta     TIM64T                  ;4        
    lda     ram_A7                  ;3        
    cmp     #$ff                    ;2        
    beq     Ld78d                   ;2/3      
    cmp     #$4e                    ;2        
    bmi     Ld78d                   ;2/3      
    lda     #$ff                    ;2         *
    sta     ram_A1                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_8B                  ;3         *
    jsr     Ld79d                   ;6         *
    lda     ram_9E                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    and     #$03                    ;2         *
    sta     ram_B8                  ;3         *
    asl                             ;2         *
    sec                             ;2         *
    adc     ram_B8                  ;3         *
    tax                             ;2         *
    lda     Lda02,x                 ;4         *
    sec                             ;2         *
    sbc     ram_90                  ;3         *
    sta     ram_B3                  ;3         *
    lda     #$de                    ;2         *
    sbc     #$00                    ;2         *
    sta     ram_B5                  ;3         *
    bne     Ld793                   ;2/3 = 166 *
Ld78d
    jsr     Lda42                   ;6        
    jsr     Ld79d                   ;6   =  12
Ld793
    lda     INTIM                   ;4        
    bne     Ld793                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    jmp     Ldff2                   ;3   =   3
    
Ld79d
    lda     ram_8B                  ;3        
    bne     Ld7b7                   ;2/3      
    sta     ram_A2                  ;3        
    sta     ram_B9                  ;3        
    lda     ram_90                  ;3        
    clc                             ;2        
    adc     #$06                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lda15,y                 ;4        
    ora     #$70                    ;2        
    sta     ram_AD                  ;3        
    jmp     Ld891                   ;3   =  36
    
Ld7b7
    ldx     #$01                    ;2   =   2 *
Ld7b9
    lda     ram_90,x                ;4         *
    clc                             ;2         *
    adc     #$06                    ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    lda     Lda15,y                 ;4         *
    sta     ram_9A,x                ;4         *
    dex                             ;2         *
    bpl     Ld7b9                   ;2/3       *
    lda     ram_9A                  ;3         *
    sec                             ;2         *
    sbc     ram_9B                  ;3         *
    beq     Ld806                   ;2/3!      *
    cmp     #$01                    ;2         *
    beq     Ld806                   ;2/3!      *
    eor     #$ff                    ;2         *
    beq     Ld806                   ;2/3!      *
    lda     #$01                    ;2         *
    sta     ram_A2                  ;3         *
    lda     ram_9A                  ;3         *
    cmp     ram_9B                  ;3         *
    bmi     Ld7e5                   ;2/3       *
    jmp     Ld875                   ;3   =  60 *
    
Ld7e5
    ldx     #$00                    ;2         *
    stx     ram_BA                  ;3         *
    ora     #$70                    ;2         *
    sta     ram_AE                  ;3         *
    lda     ram_FB                  ;3         *
    and     #$f0                    ;2         *
    sta     ram_B9                  ;3         *
    lda     ram_9B                  ;3         *
    clc                             ;2         *
    sbc     ram_9A                  ;3         *
    sec                             ;2         *
    sbc     #$01                    ;2         *
    ora     ram_B9                  ;3         *
    sta     ram_AD                  ;3         *
    ldx     #$01                    ;2         *
    stx     ram_B9                  ;3         *
    jmp     Ld891                   ;3   =  44 *
    
Ld806
    lda     ram_8A                  ;3         *
    sec                             ;2         *
    sbc     ram_8B                  ;3         *
    bpl     Ld80f                   ;2/3       *
    eor     #$ff                    ;2   =  12 *
Ld80f
    cmp     #$05                    ;2         *
    bpl     Ld854                   ;2/3       *
    lda     ram_90                  ;3         *
    sec                             ;2         *
    sbc     ram_91                  ;3         *
    bpl     Ld81c                   ;2/3       *
    eor     #$ff                    ;2   =  16 *
Ld81c
    cmp     #$05                    ;2         *
    bpl     Ld854                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_8B                  ;3         *
    lda     ram_FB                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    bit     ram_94                  ;3         *
    bvs     Ld84f                   ;2/3       *
    sed                             ;2         *
    clc                             ;2         *
    lda     Ld9fb,y                 ;4         *
    adc     ram_F9                  ;3         *
    sta     ram_F9                  ;3         *
    lda     #$00                    ;2         *
    adc     ram_FA                  ;3         *
    sta     ram_FA                  ;3         *
    lda     ram_F8                  ;3         *
    lsr                             ;2         *
    bcs     Ld84f                   ;2/3       *
    lda     ram_FA                  ;3         *
    beq     Ld84f                   ;2/3       *
    lda     ram_F8                  ;3         *
    ora     #$01                    ;2         *
    sta     ram_F8                  ;3         *
    inc     ram_FB                  ;5   =  74 *
Ld84f
    cld                             ;2         *
    lda     #$33                    ;2         *
    sta     ram_A7                  ;3   =   7 *
Ld854
    ldy     #$00                    ;2         *
    sty     ram_A2                  ;3         *
    lda     ram_9E                  ;3         *
    and     #$01                    ;2         *
    sta     ram_B9                  ;3         *
    tax                             ;2         *
    cpx     #$01                    ;2         *
    beq     Ld86b                   ;2/3       *
    lda     ram_9A                  ;3         *
    ora     #$70                    ;2         *
    sta     ram_AD                  ;3         *
    bpl     Ld891                   ;2/3 =  29 *
Ld86b
    lda     ram_FB                  ;3         *
    and     #$f0                    ;2         *
    ora     ram_9B                  ;3         *
    sta     ram_AD                  ;3         *
    bpl     Ld891                   ;2/3 =  13 *
Ld875
    ldx     #$00                    ;2         *
    stx     ram_B9                  ;3         *
    lda     ram_FB                  ;3         *
    and     #$f0                    ;2         *
    ora     ram_9B                  ;3         *
    sta     ram_AE                  ;3         *
    lda     ram_9A                  ;3         *
    clc                             ;2         *
    sbc     ram_9B                  ;3         *
    sec                             ;2         *
    sbc     #$01                    ;2         *
    ora     #$70                    ;2         *
    sta     ram_AD                  ;3         *
    ldx     #$01                    ;2         *
    stx     ram_BA                  ;3   =  38 *
Ld891
    ldy     ram_A2                  ;3   =   3
Ld893
    ldx     ram_B9,y                ;4        
    lda     ram_8A,x                ;4        
    sec                             ;2        
    sbc     #$0c                    ;2        
    cmp     #$0c                    ;2        
    bcc     Ld8a0                   ;2/3      
    adc     #$02                    ;2   =  18
Ld8a0
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Lda15,x                 ;4        
    sta     ram_B7                  ;3        
    ldx     ram_B9,y                ;4        
    lda     ram_8A,x                ;4        
    cmp     #$18                    ;2        
    bcc     Ld8b2                   ;2/3      
    adc     #$02                    ;2   =  27
Ld8b2
    cmp     #$54                    ;2        
    bmi     Ld8b8                   ;2/3      
    sbc     #$54                    ;2   =   6
Ld8b8
    cmp     #$30                    ;2        
    bmi     Ld8be                   ;2/3      
    sbc     #$30                    ;2   =   6 *
Ld8be
    cmp     #$18                    ;2        
    bmi     Ld8c4                   ;2/3      
    sbc     #$18                    ;2   =   6 *
Ld8c4
    cmp     #$0c                    ;2        
    bmi     Ld8ca                   ;2/3      
    sbc     #$0c                    ;2   =   6 *
Ld8ca
    sec                             ;2        
    sbc     #$04                    ;2        
    eor     #$ff                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    ora     ram_B7                  ;3        
    sta.wy  ram_9C,y                ;5        
    cpx     #$01                    ;2        
    bne     Ld8f5                   ;2/3      
    lda     ram_FB                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    clc                             ;2         *
    adc     #$0c                    ;2         *
    tax                             ;2         *
    lda     Lda02,x                 ;4         *
    ldx     ram_B9,y                ;4         *
    sec                             ;2         *
    sbc     ram_90,x                ;4         *
    sta.wy  ram_B3,y                ;5         *
    lda     #$df                    ;2         *
    bne     Ld91c                   ;2/3!=  66 *
Ld8f5
    lda     ram_9E                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    and     #$03                    ;2        
    cmp     #$03                    ;2        
    bne     Ld901                   ;2/3!     
    lda     #$01                    ;2   =  15
Ld901
    sta     ram_B7                  ;3        
    lda     ram_85,x                ;4        
    and     #$03                    ;2        
    sta     ram_B8                  ;3        
    asl                             ;2        
    adc     ram_B8                  ;3        
    adc     ram_B7                  ;3        
    tax                             ;2        
    lda     Lda02,x                 ;4        
    ldx     ram_B9,y                ;4        
    sec                             ;2        
    sbc     ram_90,x                ;4        
    sta.wy  ram_B3,y                ;5        
    lda     #$de                    ;2   =  43
Ld91c
    sbc     #$00                    ;2        
    sta.wy  ram_B5,y                ;5        
    dey                             ;2        
    bmi     Ld927                   ;2/3      
    jmp     Ld893                   ;3   =  14 *
    
Ld927
    rts                             ;6   =   6
    
    .byte   $00,$00,$00,$38,$6c,$c6,$c6,$c6 ; $d928 (*)
    .byte   $6c,$38,$00,$7e,$18,$18,$18,$18 ; $d930 (*)
    .byte   $38,$18,$00,$fe,$c0,$e0,$3c,$06 ; $d938 (*)
    .byte   $c6,$7c,$00,$fc,$06,$06,$7c,$06 ; $d940 (*)
    .byte   $06,$fc,$00,$0c,$0c,$0c,$fe,$cc ; $d948 (*)
    .byte   $cc,$cc,$00,$fc,$06,$06,$fc,$c0 ; $d950 (*)
    .byte   $c0,$fc,$00,$7c,$c6,$c6,$fc,$c0 ; $d958 (*)
    .byte   $c0,$7c,$00,$30,$30,$18,$18,$0c ; $d960 (*)
    .byte   $06,$fe,$00,$7c,$c6,$c6,$7c,$c6 ; $d968 (*)
    .byte   $c6,$7c,$00,$7c,$06,$06,$7e,$c6 ; $d970 (*)
    .byte   $c6,$7c,$00,$00,$00,$00,$00,$00 ; $d978 (*)
    .byte   $00,$00,$00                     ; $d980 (*)
    
    .byte   %01111100 ; | #####  |            $d983 (G)
    .byte   %11000110 ; |##   ## |            $d984 (G)
    .byte   %10111010 ; |# ### # |            $d985 (G)
    .byte   %10110010 ; |# ##  # |            $d986 (G)
    .byte   %10111010 ; |# ### # |            $d987 (G)
    .byte   %11000110 ; |##   ## |            $d988 (G)
    .byte   %01111100 ; | #####  |            $d989 (G)
    .byte   %00000000 ; |        |            $d98a (G)
    .byte   %00011101 ; |   ### #|            $d98b (G)
    .byte   %00001000 ; |    #   |            $d98c (G)
    .byte   %00001000 ; |    #   |            $d98d (G)
    .byte   %00001001 ; |    #  #|            $d98e (G)
    .byte   %00001001 ; |    #  #|            $d98f (G)
    .byte   %00011001 ; |   ##  #|            $d990 (G)
    .byte   %00001001 ; |    #  #|            $d991 (G)
    .byte   %00000000 ; |        |            $d992 (G)
    .byte   %10001001 ; |#   #  #|            $d993 (G)
    .byte   %01010101 ; | # # # #|            $d994 (G)
    .byte   %01010101 ; | # # # #|            $d995 (G)
    .byte   %11001001 ; |##  #  #|            $d996 (G)
    .byte   %01010100 ; | # # #  |            $d997 (G)
    .byte   %01010101 ; | # # # #|            $d998 (G)
    .byte   %10001001 ; |#   #  #|            $d999 (G)
    .byte   %00000000 ; |        |            $d99a (G)
    
    .byte   $c2,$02,$02,$83,$42,$42,$81,$00 ; $d99b (D)
    
    .byte   %10011011 ; |#  ## ##|            $d9a3 (G)
    .byte   %10010110 ; |#  # ## |            $d9a4 (G)
    .byte   %10010011 ; |#  #  ##|            $d9a5 (G)
    .byte   %10010000 ; |#  #    |            $d9a6 (G)
    .byte   %10111011 ; |# ### ##|            $d9a7 (G)
    .byte   %10010000 ; |#  #    |            $d9a8 (G)
    .byte   %00010000 ; |   #    |            $d9a9 (G)
    .byte   %00000000 ; |        |            $d9aa (G)
    .byte   %11100011 ; |###   ##|            $d9ab (G)
    .byte   %10100010 ; |# #   # |            $d9ac (G)
    .byte   %10100010 ; |# #   # |            $d9ad (G)
    .byte   %10100010 ; |# #   # |            $d9ae (G)
    .byte   %00111010 ; |  ### # |            $d9af (G)
    .byte   %00000000 ; |        |            $d9b0 (G)
    .byte   %00000010 ; |      # |            $d9b1 (G)
    .byte   %00000000 ; |        |            $d9b2 (G)
    
    .byte   $fb,$fb,$f9,$fb,$fb,$f9,$d8,$88 ; $d9b3 (*)
    .byte   $96,$d6,$c6,$87,$c7,$c6,$07,$07 ; $d9bb (*)
    .byte   $3e,$3e,$3e,$36,$9c,$9c,$9c,$08 ; $d9c3 (*)
    .byte   $39,$7d,$f1,$c1,$c1,$f1,$7d,$39 ; $d9cb (*)
    .byte   $f7,$f7,$f7,$f6,$f3,$f3,$b3,$11 ; $d9d3 (*)
    .byte   $df,$df,$df,$df,$9f,$9d,$99,$11 ; $d9db (*)
    
Ld9e3
    .byte   PURPLE|$4                          ; $d9e3 (C)
    
    .byte   $44,$36,$16,$44,$d6,$28         ; $d9e4 (*)
    
    .byte   YELLOW|$a                        ; $d9ea (C)
    
Ld9eb
    .byte   $14                             ; $d9eb (D)
    .byte   $2a,$40,$56,$6c,$82,$98,$ae     ; $d9ec (*)
Ld9f3
    .byte   $df                             ; $d9f3 (D)
    .byte   $df,$df,$df,$df,$df,$df,$df     ; $d9f4 (*)
Ld9fb
    .byte   $01,$02,$05,$07,$10,$20,$50     ; $d9fb (*)
Lda02
    .byte   $0c,$22,$38,$4e,$64,$7a,$90,$a6 ; $da02 (*)
    .byte   $bc                             ; $da0a (*)
    .byte   $d2,$e8,$fe                     ; $da0b (D)
    .byte   $14,$2a,$40,$56,$6c,$82,$98     ; $da0e (*)
Lda15
    .byte   $00,$00,$00,$01,$01,$01,$02,$02 ; $da15 (*)
    .byte   $02,$03,$03,$03                 ; $da1d (*)
    .byte   $04                             ; $da21 (D)
    .byte   $04,$04,$05,$05,$05,$06         ; $da22 (*)
    .byte   $06,$06                         ; $da28 (D)
    .byte   $07,$07,$07,$08,$08             ; $da2a (*)
    .byte   $08                             ; $da2f (D)
    .byte   $09,$09,$09,$0a,$0a,$0a,$0b,$0b ; $da30 (*)
    .byte   $0b,$0c,$0c,$0c,$0d,$0d,$0d,$0e ; $da38 (*)
    .byte   $0e,$0e                         ; $da40 (*)
    
Lda42
    ldx     ram_93                  ;3   =   3
Lda44
    lda     ram_8C,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lda15,y                 ;4        
    sta     ram_9A,x                ;4        
    dex                             ;2        
    bpl     Lda44                   ;2/3      
    stx     ram_A1                  ;3        
    ldx     #$0d                    ;2        
    stx     ram_B8                  ;3   =  30
Lda57
    lda     ram_93                  ;3        
    cmp     #$03                    ;2        
    beq     Lda6e                   ;2/3 =   7
Lda5d
    lda     ram_B8                  ;3         *
    ldx     ram_93                  ;3   =   6 *
Lda61
    cmp     ram_9A,x                ;4         *
    beq     Ldab4                   ;2/3       *
    dex                             ;2         *
    bpl     Lda61                   ;2/3       *
    dec     ram_B8                  ;5         *
    bpl     Lda5d                   ;2/3       *
    bmi     Lda84                   ;2/3 =  19 *
Lda6e
    lda     ram_B8                  ;3        
    cmp     ram_9D                  ;3        
    beq     Ldab2                   ;2/3      
    cmp     ram_9C                  ;3        
    beq     Ldaae                   ;2/3      
    cmp     ram_9B                  ;3        
    beq     Ldaaa                   ;2/3      
    cmp     ram_9A                  ;3        
    beq     Ldaa6                   ;2/3      
    dec     ram_B8                  ;5        
    bpl     Lda6e                   ;2/3 =  30
Lda84
    ldy     #$01                    ;2        
    ldx     #$00                    ;2   =   4
Lda88
    cpx     ram_A1                  ;3        
    beq     Ldaa5                   ;2/3      
    lda     ram_A9,x                ;4        
    clc                             ;2        
    sbc.wy  ram_A9,y                ;4        
    sec                             ;2        
    sbc     #$01                    ;2        
    and     #$0f                    ;2        
    sta     ram_B7                  ;3        
    lda     ram_A9,x                ;4        
    and     #$f0                    ;2        
    ora     ram_B7                  ;3        
    sta     ram_A9,x                ;4        
    iny                             ;2        
    inx                             ;2        
    bne     Lda88                   ;2/3 =  43
Ldaa5
    rts                             ;6   =   6
    
Ldaa6
    ldx     #$00                    ;2         *
    beq     Ldab4                   ;2/3 =   4 *
Ldaaa
    ldx     #$01                    ;2         *
    bne     Ldab4                   ;2/3 =   4 *
Ldaae
    ldx     #$02                    ;2        
    bne     Ldab4                   ;2/3 =   4
Ldab2
    ldx     #$03                    ;2   =   2
Ldab4
    ldy     #$01                    ;2        
    stx     ram_B3                  ;3   =   5
Ldab8
    dex                             ;2        
    bmi     Ldac4                   ;2/3      
    cmp     ram_9A,x                ;4        
    bne     Ldab8                   ;2/3      
    iny                             ;2        
    stx     ram_B2,y                ;4        
    bpl     Ldab8                   ;2/3 =  18
Ldac4
    dec     ram_B8                  ;5        
    lda     ram_B8                  ;3        
    ldx     ram_93                  ;3        
    sty     ram_99                  ;3   =  14
Ldacc
    cmp     ram_9A,x                ;4        
    bne     Ldad3                   ;2/3      
    iny                             ;2         *
    stx     ram_B2,y                ;4   =  12 *
Ldad3
    dex                             ;2        
    bpl     Ldacc                   ;2/3      
    dey                             ;2        
    beq     Ldaf3                   ;2/3      
    lda     ram_9E                  ;3        
    dey                             ;2        
    beq     Ldaef                   ;2/3      
    dey                             ;2        
    beq     Ldae5                   ;2/3      
    and     #$03                    ;2         *
    bpl     Ldaf5                   ;2/3 =  23 *
Ldae5
    and     #$3f                    ;2        
    tax                             ;2        
    lda     Ldb88,x                 ;4        
    lsr                             ;2        
    lsr                             ;2        
    bpl     Ldaf5                   ;2/3 =  14
Ldaef
    and     #$01                    ;2         *
    bpl     Ldaf5                   ;2/3 =   4 *
Ldaf3
    lda     #$00                    ;2   =   2
Ldaf5
    cmp     ram_99                  ;3        
    bmi     Ldafb                   ;2/3      
    dec     ram_B8                  ;5   =  10 *
Ldafb
    dec     ram_B8                  ;5        
    tay                             ;2        
    ldx     ram_B3,y                ;4        
    lda     ram_86,x                ;4        
    sec                             ;2        
    sbc     #$0c                    ;2        
    cmp     #$0c                    ;2        
    bcc     Ldb0b                   ;2/3      
    adc     #$02                    ;2   =  25
Ldb0b
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lda15,y                 ;4        
    sta     ram_BA                  ;3        
    lda     ram_86,x                ;4        
    cmp     #$18                    ;2        
    bcc     Ldb1b                   ;2/3      
    adc     #$02                    ;2   =  23
Ldb1b
    cmp     #$54                    ;2        
    bmi     Ldb21                   ;2/3      
    sbc     #$54                    ;2   =   6
Ldb21
    cmp     #$30                    ;2        
    bmi     Ldb27                   ;2/3      
    sbc     #$30                    ;2   =   6 *
Ldb27
    cmp     #$18                    ;2        
    bmi     Ldb2d                   ;2/3      
    sbc     #$18                    ;2   =   6 *
Ldb2d
    cmp     #$0c                    ;2        
    bmi     Ldb33                   ;2/3      
    sbc     #$0c                    ;2   =   6 *
Ldb33
    sec                             ;2        
    sbc     #$04                    ;2        
    eor     #$ff                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    ora     ram_BA                  ;3        
    inc     ram_A1                  ;5        
    ldy     ram_A1                  ;3        
    sta.wy  ram_A3,y                ;5        
    lda     ram_81,x                ;4        
    bmi     Ldb57                   ;2/3      
    and     #$70                    ;2        
    cmp     #$40                    ;2        
    beq     Ldb5b                   ;2/3      
    cmp     #$60                    ;2        
    beq     Ldb5b                   ;2/3      
    lda     #$b4                    ;2        
    bne     Ldb5d                   ;2/3 =  50
Ldb57
    lda     #$cb                    ;2         *
    bne     Ldb5d                   ;2/3 =   4 *
Ldb5b
    lda     #$e2                    ;2   =   2 *
Ldb5d
    sec                             ;2        
    sbc     ram_8C,x                ;4        
    sta.wy  ram_AF,y                ;5        
    lda     ram_81,x                ;4        
    bpl     Ldb75                   ;2/3      
    lda     ram_F4                  ;3         *
    and     #$32                    ;2         *
    bne     Ldb71                   ;2/3       *
    lda     #$40                    ;2         *
    bne     Ldb7a                   ;2/3 =  28 *
Ldb71
    lda     #$50                    ;2         *
    bne     Ldb7a                   ;2/3 =   4 *
Ldb75
    txa                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2   =  10
Ldb7a
    ora     ram_9A,x                ;4        
    sta.wy  ram_A9,y                ;5        
    jmp     Lda57                   ;3   =  12
    
Ldb82
    .byte   BLUE_CYAN|$6                        ; $db82 (C)
    .byte   CYAN_GREEN|$8                       ; $db83 (C)
    .byte   ORANGE|$8                           ; $db84 (C)
    .byte   VIOLET|$6                           ; $db85 (C)
    
    .byte   $0c,$88                             ; $db86 (*)
Ldb88
    .byte   $00,$04,$08,$00,$04,$08,$00,$04 ; $db88 (D)
    .byte   $08,$00,$04,$08,$00,$04,$08,$00 ; $db90 (D)
    .byte   $04,$08,$00,$04,$08,$00,$04,$08 ; $db98 (D)
    .byte   $00,$04,$08,$00,$04,$08,$00,$04 ; $dba0 (D)
    .byte   $08,$00,$04,$08,$00,$04,$08,$00 ; $dba8 (D)
    .byte   $04,$08,$00,$04,$08,$00,$04,$08 ; $dbb0 (D)
    .byte   $00,$04,$08,$00,$04,$08,$00,$04 ; $dbb8 (D)
    .byte   $08,$00,$04,$08,$00,$04,$08,$00 ; $dbc0 (D)
    
Ldbc8
    lda     ram_A7                  ;3        
    cmp     #$ff                    ;2        
    bne     Ldbff                   ;2/3      
    lda     ram_9E                  ;3         *
    and     #$07                    ;2         *
    cmp     #$02                    ;2         *
    bne     Ldbff                   ;2/3       *
    lda     ram_F4                  ;3         *
    beq     Ldbff                   ;2/3       *
    dec     ram_F4                  ;5         *
    lda     ram_F4                  ;3         *
    and     #$3f                    ;2         *
    beq     Ldbf2                   ;2/3       *
    bit     ram_94                  ;3         *
    bvs     Ldbff                   ;2/3       *
    lsr                             ;2         *
    sta     AUDF0                   ;3         *
    lda     #$0f                    ;2         *
    sta     AUDV0                   ;3         *
    lda     #$0d                    ;2         *
    sta     AUDC0                   ;3         *
    rts                             ;6   =  59 *
    
Ldbf2
    sta     ram_F4                  ;3         *
    ldx     #$03                    ;2   =   5 *
Ldbf6
    lda     ram_81,x                ;4         *
    and     #$7f                    ;2         *
    sta     ram_81,x                ;4         *
    dex                             ;2         *
    bpl     Ldbf6                   ;2/3 =  14 *
Ldbff
    rts                             ;6   =   6
    
Ldc00
    .byte   $10,$02,$00,$10,$e2,$fc,$10,$00 ; $dc00 (D)
    .byte   $00,$70,$23,$f1,$00,$20,$00,$70 ; $dc08 (D)
    .byte   $3f,$11,$40,$00,$10,$70,$3f,$11 ; $dc10 (D)
    .byte   $00,$20,$00,$70,$22,$c4,$10,$02 ; $dc18 (D)
    .byte   $04,$10,$e2,$c0,$10,$e3,$c7,$10 ; $dc20 (D)
    .byte   $00,$00,$10,$00,$00             ; $dc28 (D)
    .byte   $10,$fe,$7c,$10,$20,$04,$70,$22 ; $dc2d (*)
    .byte   $c4,$70,$22,$00,$00,$03,$11,$10 ; $dc35 (*)
    .byte   $e0,$10,$10,$e3,$11,$10,$e2,$00 ; $dc3d (*)
    .byte   $10,$00,$fc,$f0,$8e,$04,$10,$02 ; $dc45 (*)
    .byte   $40,$10,$e2,$7c,$00,$02,$00,$10 ; $dc4d (*)
    .byte   $00,$04,$10,$e2,$c0,$10,$02,$04 ; $dc55 (*)
    .byte   $f0,$8e,$7c,$10,$00,$00,$10,$e3 ; $dc5d (*)
    .byte   $11,$10,$80,$11,$10,$88,$11,$00 ; $dc65 (*)
    .byte   $08,$00,$f0,$8e,$c4,$10,$00,$04 ; $dc6d (*)
    .byte   $10,$8e,$c0,$10,$8e,$c4,$10,$00 ; $dc75 (*)
    .byte   $04,$10,$00,$00,$10,$8e,$fc,$10 ; $dc7d (*)
    .byte   $80,$04,$10,$e2,$c4,$10,$02,$00 ; $dc85 (*)
    .byte   $70,$23,$11,$00,$20,$10,$f0,$e3 ; $dc8d (*)
    .byte   $11,$00,$22,$00,$70,$22,$c4,$10 ; $dc95 (*)
    .byte   $00,$04,$10,$8e,$7c,$10,$8e,$04 ; $dc9d (*)
    .byte   $10,$00,$c0                     ; $dca5 (*)
Ldca8
    .byte   $00                             ; $dca8 (D)
    .byte   $2a,$54,$7e                     ; $dca9 (*)
Ldcac
    .byte   $00                             ; $dcac (*)
    
    .byte   %00000000 ; |        |            $dcad (P)
    .byte   %00000000 ; |        |            $dcae (P)
    .byte   %00000000 ; |        |            $dcaf (P)
    .byte   %00000000 ; |        |            $dcb0 (P)
    .byte   %00000000 ; |        |            $dcb1 (P)
    .byte   %00000000 ; |        |            $dcb2 (P)
    .byte   %00000000 ; |        |            $dcb3 (P)
    .byte   %00000000 ; |        |            $dcb4 (P)
    .byte   %11100000 ; |***     |            $dcb5 (P)
    .byte   %00000000 ; |        |            $dcb6 (P)
    .byte   %00000000 ; |        |            $dcb7 (P)
    .byte   %00000000 ; |        |            $dcb8 (P)
    .byte   %00000000 ; |        |            $dcb9 (P)
    .byte   %00000000 ; |        |            $dcba (P)
    
Ldcbb
    .byte   $00                             ; $dcbb (*)
    
    .byte   %00000000 ; |        |            $dcbc (P)
    .byte   %00000000 ; |        |            $dcbd (P)
    .byte   %00000000 ; |        |            $dcbe (P)
    .byte   %00000000 ; |        |            $dcbf (P)
    .byte   %00000000 ; |        |            $dcc0 (P)
    .byte   %00000000 ; |        |            $dcc1 (P)
    .byte   %11100000 ; |***     |            $dcc2 (P)
    .byte   %00000000 ; |        |            $dcc3 (P)
    .byte   %00000000 ; |        |            $dcc4 (P)
    .byte   %00000000 ; |        |            $dcc5 (P)
    .byte   %00000000 ; |        |            $dcc6 (P)
    .byte   %00000000 ; |        |            $dcc7 (P)
    .byte   %00000000 ; |        |            $dcc8 (P)
    .byte   %00000000 ; |        |            $dcc9 (P)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dcca (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dcd2 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dcda (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dce2 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$36 ; $dcea (*)
    .byte   $47,$a9,$83,$b5,$5a,$18,$18,$18 ; $dcf2 (*)
    .byte   $18,$00,$00,$00,$00,$00,$00     ; $dcfa (*)
    
    .byte   %00000000 ; |        |            $dd01 (G)
    .byte   %00000000 ; |        |            $dd02 (G)
    .byte   %00000000 ; |        |            $dd03 (G)
    .byte   %00000000 ; |        |            $dd04 (G)
    .byte   %00000000 ; |        |            $dd05 (G)
    .byte   %00000000 ; |        |            $dd06 (G)
    .byte   %00000000 ; |        |            $dd07 (G)
    .byte   %00000000 ; |        |            $dd08 (G)
    .byte   %00000000 ; |        |            $dd09 (G)
    .byte   %00000000 ; |        |            $dd0a (G)
    .byte   %00000000 ; |        |            $dd0b (G)
    .byte   %00000000 ; |        |            $dd0c (G)
    .byte   %00000000 ; |        |            $dd0d (G)
    .byte   %00000000 ; |        |            $dd0e (G)
    .byte   %00000000 ; |        |            $dd0f (G)
    .byte   %00000000 ; |        |            $dd10 (G)
    .byte   %00000000 ; |        |            $dd11 (G)
    .byte   %00000000 ; |        |            $dd12 (G)
    .byte   %00000000 ; |        |            $dd13 (G)
    .byte   %00000000 ; |        |            $dd14 (G)
    .byte   %00000000 ; |        |            $dd15 (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd16 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd1e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd26 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd2e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd36 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd3e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd46 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd4e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd56 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd5e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd66 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd6e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd76 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd7e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd86 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd8e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dd96 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$02 ; $dd9e (*)
    .byte   $02,$00,$00,$00,$00,$00,$00     ; $dda6 (*)
    
    .byte   %00000000 ; |        |            $ddad (G)
    .byte   %00000000 ; |        |            $ddae (G)
    .byte   %00000000 ; |        |            $ddaf (G)
    .byte   %00000000 ; |        |            $ddb0 (G)
    .byte   %00000000 ; |        |            $ddb1 (G)
    .byte   %00000000 ; |        |            $ddb2 (G)
    .byte   %00000000 ; |        |            $ddb3 (G)
    .byte   %00111100 ; |  ####  |            $ddb4 (G)
    .byte   %01111110 ; | ###### |            $ddb5 (G)
    .byte   %11001001 ; |##  #  #|            $ddb6 (G)
    .byte   %11011011 ; |## ## ##|            $ddb7 (G)
    .byte   %11001001 ; |##  #  #|            $ddb8 (G)
    .byte   %11111111 ; |########|            $ddb9 (G)
    .byte   %11111111 ; |########|            $ddba (G)
    .byte   %11111111 ; |########|            $ddbb (G)
    .byte   %11111111 ; |########|            $ddbc (G)
    .byte   %10101010 ; |# # # # |            $ddbd (G)
    .byte   %00000000 ; |        |            $ddbe (G)
    .byte   %00000000 ; |        |            $ddbf (G)
    .byte   %00000000 ; |        |            $ddc0 (G)
    .byte   %00000000 ; |        |            $ddc1 (G)
    .byte   %00000000 ; |        |            $ddc2 (G)
    .byte   %00000000 ; |        |            $ddc3 (G)
    .byte   %00000000 ; |        |            $ddc4 (G)
    .byte   %00000000 ; |        |            $ddc5 (G)
    .byte   %00000000 ; |        |            $ddc6 (G)
    .byte   %00000000 ; |        |            $ddc7 (G)
    
    .byte   $00,$00,$00,$3c,$7e,$db,$c9,$ff ; $ddc8 (*)
    .byte   $ff,$db,$a5,$ff,$aa,$00,$00,$00 ; $ddd0 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ddd8 (*)
    .byte   $00,$00,$00,$42,$a5,$a5,$a5,$42 ; $dde0 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dde8 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ddf0 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ddf8 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $de00 (*)
    .byte   $00,$00,$00,$00,$00,$00,$41,$63 ; $de08 (*)
    .byte   $63,$77,$57,$77,$be,$5c,$00,$00 ; $de10 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $de18 (*)
    .byte   $00,$00,$00,$22,$63,$63,$77,$77 ; $de20 (*)
    .byte   $57,$77,$be,$5c,$00,$00,$00,$00 ; $de28 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $de30 (*)
    .byte   $1c,$3e,$7f,$7f,$7f,$7f,$5f,$7f ; $de38 (*)
    .byte   $be,$5c,$00,$00,$00,$00,$00,$00 ; $de40 (*)
    .byte   $00,$00,$00,$00,$00,$00,$5c,$b8 ; $de48 (*)
    .byte   $68,$70,$60,$60,$70,$78,$38,$1c ; $de50 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $de58 (*)
    .byte   $00,$00,$00,$00                 ; $de60 (*)
    
Lde64
    .byte   %00011100 ; |   ###  |            $de64 (G)
    .byte   %00111110 ; |  ##### |            $de65 (G)
    .byte   %01101110 ; | ## ### |            $de66 (G)
    .byte   %01111000 ; | ####   |            $de67 (G)
    .byte   %01100000 ; | ##     |            $de68 (G)
    .byte   %01100000 ; | ##     |            $de69 (G)
    .byte   %01111000 ; | ####   |            $de6a (G)
    .byte   %01111110 ; | ###### |            $de6b (G)
    .byte   %00111110 ; |  ##### |            $de6c (G)
    .byte   %00011100 ; |   ###  |            $de6d (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $de6e (*)
    .byte   $00,$00,$00,$00,$5c,$be,$6f,$7f ; $de76 (*)
    .byte   $7f,$7f,$7f,$7f,$3e,$1c,$00,$00 ; $de7e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $de86 (*)
    .byte   $00,$00,$3a,$7d,$ee,$ea,$ee,$c6 ; $de8e (*)
    .byte   $c6,$82,$00,$00,$00,$00,$00,$00 ; $de96 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $de9e (*)
    .byte   $3a,$7d,$ee,$ea,$ee,$ee,$c6,$c6 ; $dea6 (*)
    .byte   $44,$00,$00,$00,$00,$00,$00,$00 ; $deae (*)
    .byte   $00,$00,$00,$00,$00,$00,$3a,$7d ; $deb6 (*)
    .byte   $fe,$fa,$fe,$fe,$fe,$fe,$7c,$38 ; $debe (*)
    .byte   $00,$00,$00,$00                 ; $dec6 (*)
    
    .byte   %00000000 ; |        |            $deca (G)
    .byte   %00000000 ; |        |            $decb (G)
    .byte   %00000000 ; |        |            $decc (G)
    .byte   %00000000 ; |        |            $decd (G)
    .byte   %00000000 ; |        |            $dece (G)
    .byte   %00000000 ; |        |            $decf (G)
    .byte   %00000000 ; |        |            $ded0 (G)
    .byte   %00000000 ; |        |            $ded1 (G)
    .byte   %00111000 ; |  ###   |            $ded2 (G)
    .byte   %00011100 ; |   ###  |            $ded3 (G)
    .byte   %00010110 ; |   # ## |            $ded4 (G)
    .byte   %00001110 ; |    ### |            $ded5 (G)
    .byte   %00000110 ; |     ## |            $ded6 (G)
    .byte   %00000110 ; |     ## |            $ded7 (G)
    .byte   %00001110 ; |    ### |            $ded8 (G)
    .byte   %00011110 ; |   #### |            $ded9 (G)
    .byte   %00011100 ; |   ###  |            $deda (G)
    .byte   %00111000 ; |  ###   |            $dedb (G)
    .byte   %00000000 ; |        |            $dedc (G)
    .byte   %00000000 ; |        |            $dedd (G)
    
    .byte   $00,$00                         ; $dede (*)
    
    .byte   %00000000 ; |        |            $dee0 (G)
    .byte   %00000000 ; |        |            $dee1 (G)
    .byte   %00000000 ; |        |            $dee2 (G)
    .byte   %00000000 ; |        |            $dee3 (G)
    .byte   %00000000 ; |        |            $dee4 (G)
    .byte   %00000000 ; |        |            $dee5 (G)
    .byte   %00000000 ; |        |            $dee6 (G)
    .byte   %00000000 ; |        |            $dee7 (G)
    .byte   %00111000 ; |  ###   |            $dee8 (G)
    .byte   %01111100 ; | #####  |            $dee9 (G)
    .byte   %01110110 ; | ### ## |            $deea (G)
    .byte   %00011110 ; |   #### |            $deeb (G)
    .byte   %00000110 ; |     ## |            $deec (G)
    .byte   %00000110 ; |     ## |            $deed (G)
    .byte   %00011110 ; |   #### |            $deee (G)
    .byte   %01111110 ; | ###### |            $deef (G)
    .byte   %01111100 ; | #####  |            $def0 (G)
    .byte   %00111000 ; |  ###   |            $def1 (G)
    .byte   %00000000 ; |        |            $def2 (G)
    .byte   %00000000 ; |        |            $def3 (G)
    
    .byte   $00,$00                         ; $def4 (*)
    
    .byte   %00000000 ; |        |            $def6 (G)
    .byte   %00000000 ; |        |            $def7 (G)
    .byte   %00000000 ; |        |            $def8 (G)
    .byte   %00000000 ; |        |            $def9 (G)
    .byte   %00000000 ; |        |            $defa (G)
    .byte   %00000000 ; |        |            $defb (G)
    .byte   %00000000 ; |        |            $defc (G)
    .byte   %00000000 ; |        |            $defd (G)
    .byte   %00111000 ; |  ###   |            $defe (G)
    .byte   %01111100 ; | #####  |            $deff (G)
    .byte   %11110110 ; |#### ## |            $df00 (G)
    .byte   %11111110 ; |####### |            $df01 (G)
    .byte   %11111110 ; |####### |            $df02 (G)
    .byte   %11111110 ; |####### |            $df03 (G)
    .byte   %11111110 ; |####### |            $df04 (G)
    .byte   %11111110 ; |####### |            $df05 (G)
    .byte   %01111100 ; | #####  |            $df06 (G)
    .byte   %00111000 ; |  ###   |            $df07 (G)
    .byte   %00000000 ; |        |            $df08 (G)
    .byte   %00000000 ; |        |            $df09 (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $df0a (*)
    .byte   $00,$00                         ; $df12 (*)
    
    .byte   %01100000 ; | ##     |            $df14 (G)
    .byte   %10011000 ; |#  ##   |            $df15 (G)
    .byte   %00010100 ; |   # #  |            $df16 (G)
    .byte   %00010010 ; |   #  # |            $df17 (G)
    .byte   %00100110 ; |  #  ## |            $df18 (G)
    .byte   %01001111 ; | #  ####|            $df19 (G)
    .byte   %01101111 ; | ## ####|            $df1a (G)
    .byte   %11110110 ; |#### ## |            $df1b (G)
    .byte   %11110000 ; |####    |            $df1c (G)
    .byte   %01100000 ; | ##     |            $df1d (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $df1e (*)
    .byte   $00,$00,$00,$00,$00,$00,$3c,$76 ; $df26 (*)
    .byte   $5e,$7a,$6e,$3c,$34,$18,$00,$00 ; $df2e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $df36 (*)
    .byte   $00,$00,$3c,$7e,$7e,$ff,$ff,$bf ; $df3e (*)
    .byte   $bf,$5e,$7e,$3c,$00,$00,$00,$00 ; $df46 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $df4e (*)
    .byte   $66,$bd,$99,$99,$a5,$a5,$42,$42 ; $df56 (*)
    .byte   $a5,$99,$00,$00,$00,$00,$00,$00 ; $df5e (*)
    .byte   $00,$00,$00,$00,$00,$00,$0c,$10 ; $df66 (*)
    .byte   $54,$fe,$fe,$fe,$be,$be,$5c,$38 ; $df6e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $df76 (*)
    .byte   $00,$00,$00,$00,$06,$08,$18,$3c ; $df7e (*)
    .byte   $3c,$3c,$5e,$5e,$6e,$3c,$00,$00 ; $df86 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $df8e (*)
    .byte   $00,$00,$01,$01,$01,$03,$07,$0f ; $df96 (*)
    .byte   $1e,$3e,$7c,$f0,$00,$00,$00,$00 ; $df9e (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $dfa6 (*)
    .byte   $66,$99,$ff,$5a,$db,$ff,$e7,$e7 ; $dfae (*)
    .byte   $7e,$3c,$24,$24,$24,$24,$24,$24 ; $dfb6 (*)
    .byte   $24,$24,$55,$aa,$3e,$50,$aa,$15 ; $dfbe (*)
    .byte   $88,$02,$55,$80,$48,$c5,$a0,$a2 ; $dfc6 (*)
    .byte   $a9,$28,$06,$b6,$49,$cf,$69,$aa ; $dfce (*)
    .byte   $a1,$28,$89,$a2,$00,$c0,$89,$22 ; $dfd6 (*)
    .byte   $e8,$1d,$86,$80,$aa,$49,$70,$55 ; $dfde (*)
    .byte   $e5,$a8,$00,$00,$01,$03,$81,$54 ; $dfe6 (*)
    .byte   $89,$84,$4c,$f3                 ; $dfee (*)
    
Ldff2
    sta     $fff9                   ;4        
    jmp     Ld00b                   ;3   =   7
    
    .byte   $12                             ; $dff8 (*)
Ldff9
    .byte   $22                             ; $dff9 (D)
    .byte   $1a,$8f,$00,$d0,$e6             ; $dffa (*)
    .byte   $01                             ; $dfff (*)


;***********************************************************
;      Bank 1 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $1000
    RORG    $f000

Start
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sei                             ;2        
    cld                             ;2        
    ldx     #$ff                    ;2        
    txs                             ;2        
    lda     #$00                    ;2   =  16
Lf00a
    sta     WSYNC,x                 ;4   =   4
;---------------------------------------
    dex                             ;2        
    bne     Lf00a                   ;2/3      
    lda     #$21                    ;2        
    sta     CTRLPF                  ;3        
    lda     #$03                    ;2        
    sta     ram_93                  ;3        
    lda     #$40                    ;2        
    sta     ram_94                  ;3        
    jsr     Lf025                   ;6        
    dec     ram_FA                  ;5        
    jmp     Lf092                   ;3   =  33
    
Lf023
    stx     ram_FB                  ;3   =   3 *
Lf025
    lda     #$00                    ;2        
    sta     ram_F8                  ;3        
    sta     ram_F9                  ;3        
    sta     ram_FA                  ;3        
    sta     ram_F6                  ;3   =  14
Lf02f
    inc     ram_FB                  ;5        
    bit     ram_F6                  ;3        
    bvs     Lf040                   ;2/3      
    lda     ram_FB                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    and     #$03                    ;2        
    sta     ram_80                  ;3   =  28
Lf040
    jsr     Lfd04                   ;6        
    lda     #$00                    ;2        
    sta     ram_A7                  ;3        
    sta     ram_9F                  ;3        
    sta     ram_F7                  ;3        
    lda     #$3c                    ;2        
    sta     ram_F5                  ;3   =  22
Lf04f
    lda     #$58                    ;2        
    sta     ram_86                  ;3        
    sta     ram_87                  ;3        
    sta     ram_88                  ;3        
    sta     ram_8A                  ;3        
    sta     ram_89                  ;3        
    lda     #$50                    ;2        
    sta     ram_8C                  ;3        
    sta     ram_8D                  ;3        
    sta     ram_8E                  ;3        
    lda     #$32                    ;2        
    sta     ram_8F                  ;3        
    lda     #$62                    ;2        
    sta     ram_90                  ;3        
    lda     #$03                    ;2        
    sta     ram_85                  ;3        
    lda     #$70                    ;2        
    sta     ram_81                  ;3        
    lda     #$72                    ;2        
    sta     ram_82                  ;3        
    lda     #$73                    ;2        
    sta     ram_83                  ;3        
    lda     ram_9E                  ;3        
    and     #$30                    ;2        
    sta     ram_84                  ;3        
    lda     #$00                    ;2        
    sta     ram_92                  ;3        
    sta     ram_F4                  ;3        
    sta     ram_8B                  ;3        
    lda     ram_F6                  ;3        
    and     #$40                    ;2        
    sta     ram_F6                  ;3        
    dec     ram_FB                  ;5        
    rts                             ;6   =  96
    
Lf092
    lda     #$02                    ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    sta     VSYNC                   ;3        
    sta     VBLANK                  ;3        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    lda     ram_A7                  ;3        
    cmp     #$ff                    ;2        
    bne     Lf0cf                   ;2/3      
    bit     ram_85                  ;3         *
    bpl     Lf0c9                   ;2/3       *
    lda     ram_85                  ;3         *
    tay                             ;2         *
    and     #$30                    ;2         *
    tax                             ;2         *
    tya                             ;2         *
    and     #$cf                    ;2         *
    sta     ram_85                  ;3         *
    txa                             ;2         *
    adc     #$0f                    ;2         *
    and     #$30                    ;2         *
    ora     ram_85                  ;3         *
    sta     ram_85                  ;3         *
    txa                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tax                             ;2         *
    ldy     Lf319,x                 ;4         *
    lda     #$0f                    ;2         *
    ldx     #$05                    ;2         *
    bne     Lf0cb                   ;2/3 =  62 *
Lf0c9
    lda     #$00                    ;2   =   2 *
Lf0cb
    stx     AUDC0                   ;3         *
    sty     AUDF0                   ;3   =   6 *
Lf0cf
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldx     ram_A7                  ;3        
    cpx     #$ff                    ;2        
    bne     Lf0dd                   ;2/3      
    bit     ram_94                  ;3         *
    bvs     Lf0dd                   ;2/3       *
    sta     AUDV0                   ;3   =  15 *
Lf0dd
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2        
    sta     VSYNC                   ;3        
    lda     #$2a                    ;2        
    sta     TIM64T                  ;4        
    lda     #$90                    ;2        
    bit     ram_94                  ;3        
    bpl     Lf0f0                   ;2/3      
    lda     #$00                    ;2   =  20 *
Lf0f0
    sta     COLUBK                  ;3        
    lda     ram_A7                  ;3        
    cmp     #$53                    ;2        
    bne     Lf134                   ;2/3!     
    lda     ram_FB                  ;3         *
    and     #$0f                    ;2         *
    bne     Lf12d                   ;2/3!      *
    ldx     ram_93                  ;3         *
    stx     ram_F7                  ;3         *
    ldx     ram_94                  ;3         *
    stx     ram_91                  ;3         *
    sta     ram_F6                  ;3         *
    sta     AUDV0                   ;3         *
    sta     AUDV1                   ;3         *
    sta     ram_FB                  ;3         *
    sta     ram_80                  ;3         *
    tax                             ;2         *
    lda     #$c0                    ;2         *
    sta     ram_94                  ;3         *
    lda     #$98                    ;2         *
    sta     ram_8C                  ;3         *
    stx     ram_9F                  ;3         *
    stx     ram_93                  ;3         *
    dex                             ;2         *
    stx     ram_A7                  ;3         *
    lda     #$aa                    ;2         *
    sta     ram_90                  ;3         *
    lda     #$a2                    ;2         *
    sta     ram_86                  ;3         *
    inc     ram_98                  ;5         *
    jmp     Lf134                   ;3   =  85 *
    
Lf12d
    lda     #$23                    ;2         *
    sta     ram_A7                  ;3         *
    jsr     Lf04f                   ;6   =  11 *
Lf134
    lda     SWCHB                   ;4        
    lsr                             ;2        
    bcc     Lf179                   ;2/3      
    lsr                             ;2        
    lda     ram_95                  ;3        
    beq     Lf143                   ;2/3      
    dec     ram_95                  ;5         *
    bpl     Lf171                   ;2/3 =  22 *
Lf143
    bcs     Lf171                   ;2/3      
    sta     AUDV0                   ;3         *
    sta     AUDV1                   ;3         *
    sta     ram_F8                  ;3         *
    sta     ram_F9                  ;3         *
    sta     ram_FA                  ;3         *
    bit     ram_94                  ;3         *
    bpl     Lf157                   ;2/3       *
    lda     ram_F7                  ;3         *
    sta     ram_93                  ;3   =  28 *
Lf157
    inc     ram_93                  ;5         *
    lda     ram_93                  ;3         *
    and     #$03                    ;2         *
    sta     ram_93                  ;3         *
    ldx     #$0f                    ;2         *
    stx     ram_95                  ;3         *
    bit     ram_94                  ;3         *
    bmi     Lf169                   ;2/3       *
    bvs     Lf190                   ;2/3 =  25 *
Lf169
    lda     ram_94                  ;3         *
    and     #$6f                    ;2         *
    ora     #$40                    ;2         *
    bne     Lf189                   ;2/3 =   9 *
Lf171
    bit     ram_94                  ;3        
    bvc     Lf190                   ;2/3      
    bit     INPT4|$30               ;3        
    bmi     Lf190                   ;2/3 =  10
Lf179
    bit     ram_94                  ;3         *
    bpl     Lf181                   ;2/3       *
    lda     ram_F7                  ;3         *
    sta     ram_93                  ;3   =  11 *
Lf181
    lda     ram_94                  ;3         *
    and     #$0f                    ;2         *
    ldx     #$02                    ;2         *
    bne     Lf18b                   ;2/3 =   9 *
Lf189
    ldx     #$00                    ;2   =   2 *
Lf18b
    sta     ram_94                  ;3         *
    jsr     Lf023                   ;6   =   9 *
Lf190
    inc     ram_9E                  ;5        
    lda     #$01                    ;2        
    sta     ram_96                  ;3        
    bit     ram_94                  ;3        
    bpl     Lf1a5                   ;2/3      
    ldy     #$04                    ;2   =  17 *
Lf19c
    jsr     Lf345                   ;6         *
    dey                             ;2         *
    bpl     Lf19c                   ;2/3       *
    jmp     Lfff2                   ;3   =  13 *
    
Lf1a5
    jsr     Lf44d                   ;6        
    lda     ram_A7                  ;3        
    cmp     #$ff                    ;2        
    beq     Lf1e6                   ;2/3      
    cmp     #$39                    ;2        
    beq     Lf1df                   ;2/3      
    cmp     #$3d                    ;2        
    beq     Lf1df                   ;2/3      
    cmp     #$41                    ;2        
    beq     Lf1df                   ;2/3      
    cmp     #$45                    ;2        
    beq     Lf1df                   ;2/3      
    cmp     #$3b                    ;2        
    beq     Lf1d5                   ;2/3      
    cmp     #$3f                    ;2        
    beq     Lf1d5                   ;2/3      
    cmp     #$43                    ;2        
    beq     Lf1d5                   ;2/3      
    cmp     #$47                    ;2        
    beq     Lf1d5                   ;2/3      
    cmp     #$48                    ;2        
    bne     Lf1e3                   ;2/3      
    jmp     Lf2b7                   ;3   =  52 *
    
Lf1d5
    ldy     ram_80                  ;3         *
    lda     Lfd16,y                 ;4         *
    sta     COLUPF                  ;3         *
    jmp     Lf2e9                   ;3   =  13 *
    
Lf1df
    lda     #$0d                    ;2         *
    sta     COLUPF                  ;3   =   5 *
Lf1e3
    jmp     Lf2e9                   ;3   =   3
    
Lf1e6
    lda     ram_9E                  ;3         *
    lsr                             ;2         *
    bcs     Lf1ee                   ;2/3       *
    jmp     Lf261                   ;3   =  10 *
    
Lf1ee
    ldy     #$03                    ;2         *
    sty     ram_B8                  ;3   =   5 *
Lf1f2
    ldy     ram_B8                  ;3         *
    lda     ram_FB                  ;3         *
    and     #$f0                    ;2         *
    bne     Lf202                   ;2/3!      *
    lda     ram_9E                  ;3         *
    and     #$1e                    ;2         *
    cmp     #$1e                    ;2         *
    beq     Lf25b                   ;2/3 =  19 *
Lf202
    lda.wy  ram_81,y                ;4         *
    bpl     Lf210                   ;2/3       *
    lsr                             ;2         *
    bcc     Lf210                   ;2/3       *
    lda     ram_9E                  ;3         *
    and     #$06                    ;2         *
    beq     Lf25b                   ;2/3 =  17 *
Lf210
    jsr     Lf9ce                   ;6         *
    lda.wy  ram_81,y                ;4         *
    bmi     Lf25b                   ;2/3       *
    and     #$70                    ;2         *
    cmp     #$40                    ;2         *
    beq     Lf258                   ;2/3       *
    lda.wy  ram_81,y                ;4         *
    lsr                             ;2         *
    bcs     Lf22c                   ;2/3       *
    lda     ram_9E                  ;3         *
    adc     ram_B8                  ;3         *
    and     #$02                    ;2         *
    bne     Lf258                   ;2/3 =  36 *
Lf22c
    cpy     #$00                    ;2         *
    beq     Lf25b                   ;2/3       *
    lda     ram_9E                  ;3         *
    and     #$0e                    ;2         *
    cpy     #$01                    ;2         *
    beq     Lf254                   ;2/3       *
    cpy     #$02                    ;2         *
    beq     Lf24a                   ;2/3       *
    cmp     #$00                    ;2         *
    beq     Lf258                   ;2/3       *
    cmp     #$08                    ;2         *
    beq     Lf258                   ;2/3       *
    cmp     #$0c                    ;2         *
    beq     Lf258                   ;2/3       *
    bne     Lf25b                   ;2/3 =  31 *
Lf24a
    cmp     #$02                    ;2         *
    beq     Lf258                   ;2/3       *
    cmp     #$0a                    ;2         *
    beq     Lf258                   ;2/3       *
    bne     Lf25b                   ;2/3 =  10 *
Lf254
    cmp     #$06                    ;2         *
    bne     Lf25b                   ;2/3 =   4 *
Lf258
    jsr     Lf9ce                   ;6   =   6 *
Lf25b
    dec     ram_B8                  ;5         *
    bpl     Lf1f2                   ;2/3!      *
    bmi     Lf2a8                   ;2/3 =   9 *
Lf261
    ldy     #$04                    ;2         *
    sty     ram_B8                  ;3         *
    jsr     Lf9ce                   ;6         *
    lda     ram_85                  ;3         *
    lsr                             ;2         *
    bcs     Lf273                   ;2/3       *
    lda     ram_9E                  ;3         *
    and     #$02                    ;2         *
    bne     Lf28c                   ;2/3 =  25 *
Lf273
    lda     ram_9E                  ;3         *
    and     #$02                    ;2         *
    bne     Lf28f                   ;2/3       *
    bit     ram_85                  ;3         *
    bpl     Lf28c                   ;2/3       *
    bvc     Lf283                   ;2/3       *
    lda     #$bf                    ;2         *
    bmi     Lf285                   ;2/3 =  18 *
Lf283
    lda     #$3f                    ;2   =   2 *
Lf285
    and     ram_85                  ;3         *
    sta     ram_85                  ;3         *
    jmp     Lf28f                   ;3   =   9 *
    
Lf28c
    jsr     Lf9ce                   ;6   =   6 *
Lf28f
    lda     ram_9E                  ;3         *
    and     #$02                    ;2         *
    bne     Lf298                   ;2/3       *
    jsr     Lf657                   ;6   =  13 *
Lf298
    bit     CXP0FB|$30              ;3         *
    bvc     Lf2a2                   ;2/3       *
    jsr     Lf52f                   ;6         *
    jmp     Lf2a5                   ;3   =  14 *
    
Lf2a2
    jsr     Lf5b1                   ;6   =   6 *
Lf2a5
    jsr     Lf3e9                   ;6   =   6 *
Lf2a8
    lda     ram_F7                  ;3         *
    ldx     ram_80                  ;3         *
    cmp     Lf315,x                 ;4         *
    bne     Lf2e9                   ;2/3       *
    lda     #$37                    ;2         *
    sta     ram_A7                  ;3         *
    bpl     Lf2e9                   ;2/3 =  19 *
Lf2b7
    bit     ram_F6                  ;3         *
    bvs     Lf2c9                   ;2/3       *
    lda     ram_FB                  ;3         *
    cmp     #$60                    ;2         *
    bmi     Lf2dd                   ;2/3       *
    lda     ram_F6                  ;3         *
    ora     #$40                    ;2         *
    sta     ram_F6                  ;3         *
    bne     Lf2cf                   ;2/3 =  22 *
Lf2c9
    lda     ram_80                  ;3         *
    eor     #$01                    ;2         *
    sta     ram_80                  ;3   =   8 *
Lf2cf
    lda     ram_9E                  ;3         *
    and     #$70                    ;2         *
    eor     ram_FB                  ;3         *
    cmp     #$70                    ;2         *
    bmi     Lf2e0                   ;2/3       *
    and     #$6f                    ;2         *
    bpl     Lf2e0                   ;2/3 =  16 *
Lf2dd
    clc                             ;2         *
    adc     #$10                    ;2   =   4 *
Lf2e0
    sta     ram_FB                  ;3         *
    jsr     Lf02f                   ;6         *
    lda     #$23                    ;2         *
    sta     ram_A7                  ;3   =  14 *
Lf2e9
    sta     CXCLR                   ;3        
    lda     ram_9E                  ;3        
    and     #$08                    ;2        
    beq     Lf304                   ;2/3!     
    lda     ram_F5                  ;3        
    and     #$7c                    ;2        
    sta     ram_F5                  ;3        
    and     #$30                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    ora     ram_F5                  ;3        
    sta     ram_F5                  ;3        
    jmp     Lfff2                   ;3   =  37
    
Lf304
    lda     ram_F5                  ;3        
    and     #$7c                    ;2        
    sta     ram_F5                  ;3        
    and     #$0c                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    ora     ram_F5                  ;3        
    sta     ram_F5                  ;3        
    jmp     Lfff2                   ;3   =  23
    
Lf315
    .byte   $9a,$96,$9e,$9a                 ; $f315 (*)
Lf319
    .byte   $1e,$1a,$17,$00                 ; $f319 (*)
    
Lf31d
    bit     ram_94                  ;3         *
    bvs     Lf343                   ;2/3       *
    sed                             ;2         *
    clc                             ;2         *
    adc     ram_F8                  ;3         *
    sta     ram_F8                  ;3         *
    txa                             ;2         *
    adc     ram_F9                  ;3         *
    sta     ram_F9                  ;3         *
    lda     #$00                    ;2         *
    adc     ram_FA                  ;3         *
    sta     ram_FA                  ;3         *
    lda     ram_F8                  ;3         *
    lsr                             ;2         *
    bcs     Lf343                   ;2/3       *
    lda     ram_FA                  ;3         *
    beq     Lf343                   ;2/3       *
    lda     ram_F8                  ;3         *
    ora     #$01                    ;2         *
    sta     ram_F8                  ;3         *
    inc     ram_FB                  ;5   =  56 *
Lf343
    cld                             ;2         *
    rts                             ;6   =   8 *
    
Lf345
    lda     #$00                    ;2         *
    sta     COLUPF                  ;3         *
    lda     #$10                    ;2         *
    bit     ram_94                  ;3         *
    bne     Lf39c                   ;2/3       *
    cpy     ram_9F                  ;3         *
    bne     Lf396                   ;2/3       *
    lda.wy  ram_86,y                ;4         *
    cpy     #$04                    ;2         *
    beq     Lf360                   ;2/3       *
    cmp     #$3f                    ;2         *
    beq     Lf367                   ;2/3       *
    bne     Lf364                   ;2/3 =  31 *
Lf360
    cmp     #$58                    ;2         *
    beq     Lf397                   ;2/3 =   4 *
Lf364
    jmp     Lfbbc                   ;3   =   3 *
    
Lf367
    lda.wy  ram_8C,y                ;4         *
    cmp     Lf3e5,y                 ;4         *
    beq     Lf372                   ;2/3       *
    jmp     Lfba3                   ;3   =  13 *
    
Lf372
    ldx     ram_9F                  ;3         *
    inx                             ;2         *
    cpx     #$04                    ;2         *
    beq     Lf37b                   ;2/3       *
    stx     ram_93                  ;3   =  12 *
Lf37b
    stx     ram_9F                  ;3         *
    lda     #$a2                    ;2         *
    sta     ram_86,x                ;4         *
    cpx     #$04                    ;2         *
    beq     Lf38a                   ;2/3       *
    lda     #$98                    ;2         *
    sta     ram_8C,x                ;4         *
    rts                             ;6   =  25 *
    
Lf38a
    lda     #$7f                    ;2         *
    sta     ram_90                  ;3         *
    lda     ram_85                  ;3         *
    and     #$fc                    ;2         *
    ora     #$03                    ;2         *
    sta     ram_85                  ;3   =  15 *
Lf396
    rts                             ;6   =   6 *
    
Lf397
    lda     #$d0                    ;2         *
    sta     ram_94                  ;3         *
    rts                             ;6   =  11 *
    
Lf39c
    cpy     #$04                    ;2         *
    beq     Lf396                   ;2/3       *
    lda.wy  ram_86,y                ;4         *
    cmp     #$3f                    ;2         *
    beq     Lf3b5                   ;2/3       *
    cmp     #$71                    ;2         *
    beq     Lf3bf                   ;2/3       *
    lda.wy  ram_8C,y                ;4         *
    cmp     #$66                    ;2         *
    beq     Lf3e2                   ;2/3 =  24 *
Lf3b2
    jmp     Lfbc7                   ;3   =   3 *
    
Lf3b5
    lda.wy  ram_8C,y                ;4         *
    cmp     #$98                    ;2         *
    beq     Lf3b2                   ;2/3       *
    jmp     Lfba7                   ;3   =  11 *
    
Lf3bf
    lda.wy  ram_8C,y                ;4         *
    cmp     #$66                    ;2         *
    beq     Lf3c9                   ;2/3       *
    jmp     Lfba3                   ;3   =  11 *
    
Lf3c9
    ldx     ram_94                  ;3         *
    inx                             ;2         *
    stx     ram_94                  ;3         *
    txa                             ;2         *
    and     #$0f                    ;2         *
    bne     Lf3e2                   ;2/3       *
    lda     ram_F7                  ;3         *
    sta     ram_93                  ;3         *
    lda     ram_91                  ;3         *
    ora     #$40                    ;2         *
    sta     ram_94                  ;3         *
    ldy     #$00                    ;2         *
    jmp     Lf02f                   ;3   =  33 *
    
Lf3e2
    jmp     Lfbbc                   ;3   =   3 *
    
Lf3e5
    .byte   $02,$34,$66,$98                 ; $f3e5 (*)
    
Lf3e9
    ldx     ram_93                  ;3   =   3 *
Lf3eb
    lda     ram_8A                  ;3         *
    sec                             ;2         *
    sbc     ram_86,x                ;4         *
    bpl     Lf3f7                   ;2/3       *
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$01                    ;2   =  17 *
Lf3f7
    sta     ram_B7                  ;3         *
    lda     ram_90                  ;3         *
    sec                             ;2         *
    sbc     ram_8C,x                ;4         *
    bpl     Lf405                   ;2/3       *
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$01                    ;2   =  20 *
Lf405
    clc                             ;2         *
    adc     ram_B7                  ;3         *
    bcs     Lf449                   ;2/3       *
    cmp     #$05                    ;2         *
    bcs     Lf449                   ;2/3       *
    lda     ram_81,x                ;4         *
    bpl     Lf43a                   ;2/3       *
    and     #$03                    ;2         *
    ora     #$40                    ;2         *
    sta     ram_81,x                ;4         *
    lda     #$40                    ;2         *
    clc                             ;2         *
    adc     ram_F4                  ;3         *
    bcc     Lf421                   ;2/3       *
    lda     #$00                    ;2   =  36 *
Lf421
    sta     ram_F4                  ;3         *
    and     #$c0                    ;2         *
    asl                             ;2         *
    rol                             ;2         *
    rol                             ;2         *
    tay                             ;2         *
    ldx     Lf436,y                 ;4         *
    lda     #$00                    ;2         *
    jsr     Lf31d                   ;6         *
    lda     #$33                    ;2         *
    sta     ram_A7                  ;3         *
    rts                             ;6   =  36 *
    
Lf436
    .byte   $16,$02,$04,$08                 ; $f436 (*)
    
Lf43a
    and     #$f0                    ;2         *
    cmp     #$40                    ;2         *
    beq     Lf449                   ;2/3       *
    cmp     #$60                    ;2         *
    beq     Lf449                   ;2/3       *
    lda     #$49                    ;2         *
    sta     ram_A7                  ;3         *
    rts                             ;6   =  21 *
    
Lf449
    dex                             ;2         *
    bpl     Lf3eb                   ;2/3!      *
    rts                             ;6   =  10 *
    
Lf44d
    ldy     ram_A7                  ;3        
    cpy     #$ff                    ;2        
    beq     Lf484                   ;2/3      
    lda     ram_9E                  ;3        
    and     #$07                    ;2        
    bne     Lf484                   ;2/3      
    iny                             ;2        
    sty     ram_A7                  ;3        
    bit     ram_94                  ;3        
    bvs     Lf46c                   ;2/3      
    lda     #$04                    ;2         *
    sta     AUDC0                   ;3         *
    sta     AUDC1                   ;3         *
    lda     #$0f                    ;2         *
    sta     AUDV0                   ;3         *
    sta     AUDV1                   ;3   =  40 *
Lf46c
    lda     Lf485,y                 ;4        
    cmp     #$ff                    ;2        
    bne     Lf47d                   ;2/3      
    sta     ram_A7                  ;3         *
    lda     #$00                    ;2         *
    sta     AUDV0                   ;3         *
    sta     AUDV1                   ;3         *
    beq     Lf484                   ;2/3 =  21 *
Lf47d
    sta     AUDF0                   ;3        
    lda     Lf4da,y                 ;4        
    sta     AUDF1                   ;3   =  10
Lf484
    rts                             ;6   =   6
    
Lf485
    .byte   $00                             ; $f485 (*)
    .byte   $17                             ; $f486 (A)
    .byte   $18                             ; $f487 (A)
    .byte   $1b                             ; $f488 (A)
    .byte   $00                             ; $f489 (A)
    .byte   $00                             ; $f48a (A)
    .byte   $18                             ; $f48b (A)
    .byte   $18                             ; $f48c (A)
    .byte   $1b                             ; $f48d (A)
    .byte   $1b                             ; $f48e (A)
    .byte   $17                             ; $f48f (A)
    .byte   $17                             ; $f490 (A)
    .byte   $18                             ; $f491 (A)
    .byte   $17                             ; $f492 (A)
    .byte   $14                             ; $f493 (A)
    .byte   $18,$1b,$1b,$17,$17,$18,$17,$14 ; $f494 (*)
    .byte   $18,$17,$14,$12,$10,$0f,$0f,$10 ; $f49c (*)
    .byte   $10,$0f,$0f,$ff,$00,$00,$00,$00 ; $f4a4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f4ac (*)
    .byte   $00,$00,$00,$ff,$00,$18,$10,$ff ; $f4b4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f4bc (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f4c4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$0b ; $f4cc (*)
    .byte   $0c,$0d,$0e,$0f,$0f,$ff         ; $f4d4 (*)
Lf4da
    .byte   $00                             ; $f4da (*)
    .byte   $00                             ; $f4db (A)
    .byte   $00                             ; $f4dc (A)
    .byte   $00                             ; $f4dd (A)
    .byte   $1f                             ; $f4de (A)
    .byte   $1f                             ; $f4df (A)
    .byte   $00                             ; $f4e0 (A)
    .byte   $00                             ; $f4e1 (A)
    .byte   $1b                             ; $f4e2 (A)
    .byte   $1b                             ; $f4e3 (A)
    .byte   $00                             ; $f4e4 (A)
    .byte   $00                             ; $f4e5 (A)
    .byte   $1f                             ; $f4e6 (A)
    .byte   $1f                             ; $f4e7 (A)
    .byte   $00                             ; $f4e8 (A)
    .byte   $00,$1b,$18,$17,$1b,$18,$1b,$1f ; $f4e9 (*)
    .byte   $18,$1b,$1f,$1b,$18,$1f,$1f,$1b ; $f4f1 (*)
    .byte   $1b,$1f,$1f,$ff,$00,$00,$00,$00 ; $f4f9 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f501 (*)
    .byte   $00,$00,$00,$ff,$00,$00,$00,$ff ; $f509 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f511 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f519 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$0f ; $f521 (*)
    .byte   $11,$12,$14,$12,$12,$ff         ; $f529 (*)
    
Lf52f
    lda     ram_8A                  ;3         *
    ldy     ram_90                  ;3         *
    cmp     #$55                    ;2         *
    bmi     Lf543                   ;2/3       *
    cpy     #$4f                    ;2         *
    bmi     Lf53f                   ;2/3       *
    lda     #$fb                    ;2         *
    bmi     Lf54d                   ;2/3 =  18 *
Lf53f
    lda     #$f7                    ;2         *
    bmi     Lf54d                   ;2/3 =   4 *
Lf543
    cpy     #$4f                    ;2         *
    bmi     Lf54b                   ;2/3       *
    lda     #$ef                    ;2         *
    bmi     Lf54d                   ;2/3 =   8 *
Lf54b
    lda     #$df                    ;2   =   2 *
Lf54d
    and     ram_F5                  ;3         *
    sta     ram_F5                  ;3         *
    lda     #$50                    ;2         *
    ldx     #$00                    ;2         *
    jsr     Lf31d                   ;6         *
    ldx     #$03                    ;2   =  18 *
Lf55a
    lda     ram_81,x                ;4         *
    and     #$70                    ;2         *
    cmp     #$40                    ;2         *
    beq     Lf580                   ;2/3       *
    cmp     #$60                    ;2         *
    beq     Lf580                   ;2/3       *
    cmp     #$70                    ;2         *
    beq     Lf5ac                   ;2/3       *
    lda     ram_81,x                ;4         *
    and     #$03                    ;2         *
    eor     #$02                    ;2         *
    sta     ram_81,x                ;4         *
    lda     ram_86,x                ;4         *
    adc     ram_8C,x                ;4         *
    eor     ram_9E                  ;3         *
    and     #$30                    ;2         *
    ora     ram_81,x                ;4   =  47 *
Lf57c
    ora     #$80                    ;2         *
    sta     ram_81,x                ;4   =   6 *
Lf580
    dex                             ;2         *
    bpl     Lf55a                   ;2/3       *
    lda     ram_F6                  ;3         *
    and     #$78                    ;2         *
    sta     ram_F6                  ;3         *
    lda     ram_92                  ;3         *
    and     #$f0                    ;2         *
    sta     ram_92                  ;3         *
    bit     ram_F6                  ;3         *
    bvc     Lf597                   ;2/3       *
    lda     #$08                    ;2         *
    bne     Lf59d                   ;2/3 =  29 *
Lf597
    lda     #$7f                    ;2         *
    sec                             ;2         *
    sbc     ram_FB                  ;3         *
    lsr                             ;2   =   9 *
Lf59d
    ldx     ram_93                  ;3         *
    clc                             ;2         *
    adc     Lf5a8,x                 ;4         *
    sta     ram_F4                  ;3         *
    inc     ram_F7                  ;5         *
    rts                             ;6   =  23 *
    
Lf5a8
    .byte   $c0,$80,$40,$00                 ; $f5a8 (*)
    
Lf5ac
    lda     ram_81,x                ;4         *
    jmp     Lf57c                   ;3   =   7 *
    
Lf5b1
    lda     ram_85                  ;3         *
    and     #$01                    ;2         *
    bne     Lf5e1                   ;2/3       *
    lda     ram_8A                  ;3         *
    cmp     #$0c                    ;2         *
    bcc     Lf5f1                   ;2/3       *
    sbc     #$0c                    ;2         *
    cmp     #$4e                    ;2         *
    bpl     Lf5c9                   ;2/3       *
    cmp     #$4a                    ;2         *
    bpl     Lf5f1                   ;2/3       *
    adc     #$05                    ;2   =  26 *
Lf5c9
    adc     #$01                    ;2         *
    tax                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    lda     ram_90                  ;3         *
    clc                             ;2         *
    adc     #$05                    ;2         *
    jsr     Lfcf4                   ;6         *
    cmp     #$03                    ;2         *
    bcc     Lf5f1                   ;2/3       *
    cmp     #$08                    ;2         *
    bcc     Lf60c                   ;2/3!      *
    rts                             ;6   =  39 *
    
Lf5e1
    lda     ram_8A                  ;3         *
    cmp     #$0c                    ;2         *
    bcc     Lf5f1                   ;2/3       *
    sbc     #$0c                    ;2         *
    cmp     #$4e                    ;2         *
    bpl     Lf5f4                   ;2/3       *
    cmp     #$4a                    ;2         *
    bmi     Lf5f2                   ;2/3 =  17 *
Lf5f1
    rts                             ;6   =   6 *
    
Lf5f2
    adc     #$05                    ;2   =   2 *
Lf5f4
    adc     #$01                    ;2         *
    tax                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    beq     Lf5f1                   ;2/3       *
    cmp     #$13                    ;2         *
    beq     Lf5f1                   ;2/3!      *
    tay                             ;2         *
    txa                             ;2         *
    and     #$07                    ;2         *
    cmp     #$02                    ;2         *
    bcc     Lf5f1                   ;2/3!      *
    cmp     #$06                    ;2         *
    bcs     Lf5f1                   ;2/3!=  30 *
Lf60c
    lda     ram_90                  ;3         *
    clc                             ;2         *
    adc     #$05                    ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tax                             ;2         *
    lda     Lfde5,x                 ;4         *
    sta     ram_B7                  ;3         *
    asl                             ;2         *
    adc     ram_B7                  ;3         *
    tax                             ;2         *
    tya                             ;2         *
    cpy     #$02                    ;2         *
    bmi     Lf636                   ;2/3       *
    cpy     #$06                    ;2         *
    bmi     Lf635                   ;2/3       *
    cpy     #$0a                    ;2         *
    bmi     Lf634                   ;2/3       *
    cpy     #$0e                    ;2         *
    bmi     Lf635                   ;2/3       *
    cpy     #$12                    ;2         *
    bmi     Lf634                   ;2/3       *
    bpl     Lf636                   ;2/3 =  51 *
Lf634
    inx                             ;2   =   2 *
Lf635
    inx                             ;2   =   2 *
Lf636
    lda     ram_BB,x                ;4         *
    dey                             ;2         *
    and     Lffb3,y                 ;4         *
    beq     Lf656                   ;2/3       *
    lda     ram_BB,x                ;4         *
    eor     Lffb3,y                 ;4         *
    sta     ram_BB,x                ;4         *
    lda     #$10                    ;2         *
    ldx     #$00                    ;2         *
    jsr     Lf31d                   ;6         *
    lda     #$c0                    ;2         *
    ora     ram_85                  ;3         *
    and     #$cf                    ;2         *
    sta     ram_85                  ;3         *
    inc     ram_F7                  ;5   =  49 *
Lf656
    rts                             ;6   =   6 *
    
Lf657
    lda     ram_8B                  ;3         *
    beq     Lf69e                   ;2/3       *
    lda     ram_9F                  ;3         *
    and     #$1f                    ;2         *
    sta     ram_B9                  ;3         *
    lda     ram_85                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    and     #$03                    ;2         *
    sta     ram_BA                  ;3         *
    lda     ram_F6                  ;3         *
    tax                             ;2         *
    and     #$c7                    ;2         *
    sta     ram_F6                  ;3         *
    txa                             ;2         *
    clc                             ;2         *
    adc     #$08                    ;2         *
    and     #$38                    ;2         *
    ora     ram_F6                  ;3         *
    sta     ram_F6                  ;3         *
    and     #$38                    ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    sta     ram_B8                  ;3         *
    cmp     #$05                    ;2         *
    bne     Lf695                   ;2/3       *
    bit     ram_94                  ;3         *
    bvs     Lf695                   ;2/3       *
    lda     #$0f                    ;2         *
    sta     AUDV0                   ;3         *
    lda     #$05                    ;2         *
    sta     AUDC0                   ;3         *
    lda     #$10                    ;2         *
    sta     AUDF0                   ;3   =  84 *
Lf695
    lda     #$20                    ;2         *
    bit     ram_9F                  ;3         *
    bpl     Lf708                   ;2/3!      *
    jmp     Lf751                   ;3   =  10 *
    
Lf69e
    ldx     ram_F7                  ;3         *
    lda     #$20                    ;2         *
    bit     ram_9F                  ;3         *
    bne     Lf6ab                   ;2/3       *
    cpx     #$32                    ;2         *
    beq     Lf6b0                   ;2/3       *
    rts                             ;6   =  20 *
    
Lf6ab
    cpx     #$64                    ;2         *
    bpl     Lf6b0                   ;2/3       *
    rts                             ;6   =  10 *
    
Lf6b0
    lda     ram_9E                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    adc     ram_90                  ;3         *
    eor     ram_8A                  ;3         *
    eor     ram_80                  ;3         *
    and     #$03                    ;2         *
    sta     ram_BA                  ;3         *
    beq     Lf6d3                   ;2/3       *
    cmp     #$03                    ;2         *
    beq     Lf6d3                   ;2/3       *
    lda     #$01                    ;2         *
    sta     ram_B8                  ;3         *
    lda     ram_F6                  ;3         *
    and     #$c7                    ;2         *
    ora     #$08                    ;2         *
    sta     ram_F6                  ;3         *
    jmp     Lf6dd                   ;3   =  45 *
    
Lf6d3
    lda     #$02                    ;2         *
    sta     ram_B8                  ;3         *
    lda     ram_F6                  ;3         *
    and     #$c7                    ;2         *
    ora     #$10                    ;2   =  12 *
Lf6dd
    sta     ram_F6                  ;3         *
    lda     ram_9F                  ;3         *
    eor     #$ff                    ;2         *
    sta     ram_9F                  ;3         *
    lda     ram_80                  ;3         *
    asl                             ;2         *
    asl                             ;2         *
    clc                             ;2         *
    adc     ram_BA                  ;3         *
    tay                             ;2         *
    lda     Lfd52,y                 ;4         *
    sta     ram_91                  ;3         *
    ldy     ram_BA                  ;3         *
    lda     Lfd4e,y                 ;4         *
    sta     ram_8B                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_B9                  ;3         *
    lda     ram_9F                  ;3         *
    and     #$20                    ;2         *
    ora     #$40                    ;2         *
    sta     ram_9F                  ;3         *
    jmp     Lf7d1                   ;3   =  60 *
    
Lf708
    lda     ram_8B                  ;3         *
    cmp     #$60                    ;2         *
    bpl     Lf716                   ;2/3       *
    lda     ram_B8                  ;3         *
    cmp     #$02                    ;2         *
    bne     Lf74e                   ;2/3       *
    beq     Lf71c                   ;2/3 =  16 *
Lf716
    lda     ram_B8                  ;3         *
    cmp     #$06                    ;2         *
    bne     Lf74e                   ;2/3 =   7 *
Lf71c
    lda     #$20                    ;2         *
    bit     ram_9F                  ;3         *
    bvs     Lf72f                   ;2/3       *
    dec     ram_B9                  ;5         *
    lda     ram_B9                  ;3         *
    cmp     #$ff                    ;2         *
    bne     Lf74e                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_8B                  ;3         *
    rts                             ;6   =  30 *
    
Lf72f
    inc     ram_B9                  ;5         *
    jsr     Lf7b2                   ;6         *
    cmp     ram_B9                  ;3         *
    bcs     Lf74e                   ;2/3       *
    lda     ram_9F                  ;3         *
    ora     #$80                    ;2         *
    sta     ram_9F                  ;3         *
    lda     ram_80                  ;3         *
    asl                             ;2         *
    asl                             ;2         *
    clc                             ;2         *
    adc     ram_BA                  ;3         *
    tax                             ;2         *
    lda     Lfd3e,x                 ;4         *
    sta     ram_B9                  ;3         *
    jmp     Lf81a                   ;3   =  48 *
    
Lf74e
    jmp     Lf7d1                   ;3   =   3 *
    
Lf751
    lda     ram_8B                  ;3         *
    cmp     #$60                    ;2         *
    bpl     Lf75f                   ;2/3       *
    lda     ram_B8                  ;3         *
    cmp     #$02                    ;2         *
    bne     Lf7af                   ;2/3       *
    beq     Lf765                   ;2/3 =  16 *
Lf75f
    lda     ram_B8                  ;3         *
    cmp     #$06                    ;2         *
    bne     Lf7af                   ;2/3 =   7 *
Lf765
    ldx     ram_B9                  ;3         *
    inx                             ;2         *
    cpx     #$12                    ;2         *
    bne     Lf76e                   ;2/3       *
    ldx     #$00                    ;2   =  11 *
Lf76e
    stx     ram_B9                  ;3         *
    lda     ram_80                  ;3         *
    asl                             ;2         *
    asl                             ;2         *
    clc                             ;2         *
    adc     ram_BA                  ;3         *
    tax                             ;2         *
    lda     #$20                    ;2         *
    bit     ram_9F                  ;3         *
    bvc     Lf79a                   ;2/3       *
    lda     Lfd2e,x                 ;4         *
    cmp     ram_B9                  ;3         *
    bne     Lf7af                   ;2/3       *
    lda     ram_90                  ;3         *
    adc     ram_8A                  ;3         *
    eor     ram_86                  ;3         *
    eor     ram_8F                  ;3         *
    and     #$03                    ;2         *
    sta     ram_BA                  ;3         *
    lda     ram_9F                  ;3         *
    and     #$bf                    ;2         *
    sta     ram_9F                  ;3         *
    jmp     Lf7af                   ;3   =  61 *
    
Lf79a
    lda     Lfd3e,x                 ;4         *
    cmp     ram_B9                  ;3         *
    bne     Lf7af                   ;2/3       *
    lda     ram_9F                  ;3         *
    and     #$7f                    ;2         *
    sta     ram_9F                  ;3         *
    jsr     Lf7b2                   ;6         *
    sta     ram_B9                  ;3         *
    jmp     Lf7d1                   ;3   =  29 *
    
Lf7af
    jmp     Lf81a                   ;3   =   3 *
    
Lf7b2
    lda     ram_80                  ;3         *
    asl                             ;2         *
    sta     ram_B7                  ;3         *
    lda     ram_BA                  ;3         *
    lsr                             ;2         *
    clc                             ;2         *
    adc     ram_B7                  ;3         *
    tax                             ;2         *
    lda     ram_BA                  ;3         *
    lsr                             ;2         *
    bcc     Lf7c9                   ;2/3       *
    lda     Lfd62,x                 ;4         *
    and     #$0f                    ;2         *
    rts                             ;6   =  39 *
    
Lf7c9
    lda     Lfd62,x                 ;4         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    rts                             ;6   =  18 *
    
Lf7d1
    lda     ram_B9                  ;3         *
    and     #$03                    ;2         *
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$04                    ;2         *
    tay                             ;2         *
    lda     ram_B9                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    sta     ram_B7                  ;3         *
    lda     ram_80                  ;3         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    clc                             ;2         *
    adc     ram_B7                  ;3         *
    sta     ram_B7                  ;3         *
    lda     ram_BA                  ;3         *
    asl                             ;2         *
    asl                             ;2         *
    clc                             ;2         *
    adc     ram_B7                  ;3         *
    tax                             ;2         *
    lda     Lfd6a,x                 ;4         *
    cpy     #$00                    ;2         *
    beq     Lf800                   ;2/3!=  64 *
Lf7fb
    lsr                             ;2         *
    lsr                             ;2         *
    dey                             ;2         *
    bne     Lf7fb                   ;2/3!=   8 *
Lf800
    sta     ram_99                  ;3         *
    lda     #$20                    ;2         *
    bit     ram_9F                  ;3         *
    bvs     Lf817                   ;2/3       *
    lda     ram_99                  ;3         *
    eor     #$02                    ;2         *
    sta     ram_99                  ;3         *
    lda     ram_B8                  ;3         *
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$08                    ;2         *
    sta     ram_B8                  ;3   =  30 *
Lf817
    jmp     Lf837                   ;3   =   3 *
    
Lf81a
    lda     ram_B9                  ;3         *
    and     #$03                    ;2         *
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$04                    ;2         *
    tay                             ;2         *
    lda     ram_B9                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    tax                             ;2         *
    lda     Lfdaa,x                 ;4         *
    cpy     #$00                    ;2         *
    beq     Lf835                   ;2/3 =  30 *
Lf830
    lsr                             ;2         *
    lsr                             ;2         *
    dey                             ;2         *
    bne     Lf830                   ;2/3 =   8 *
Lf835
    sta     ram_99                  ;3   =   3 *
Lf837
    ldy     ram_B8                  ;3         *
    lda     ram_99                  ;3         *
    lsr                             ;2         *
    bcc     Lf866                   ;2/3       *
    lsr                             ;2         *
    bcc     Lf854                   ;2/3       *
    lda     #$ff                    ;2         *
    clc                             ;2         *
    adc     ram_8B                  ;3         *
    cmp     #$0b                    ;2         *
    bne     Lf84c                   ;2/3       *
    lda     #$00                    ;2   =  27 *
Lf84c
    sta     ram_8B                  ;3         *
    lda     Lfdb2,y                 ;4         *
    jmp     Lf872                   ;3   =  10 *
    
Lf854
    lda     #$01                    ;2         *
    adc     ram_8B                  ;3         *
    cmp     #$a4                    ;2         *
    bne     Lf85e                   ;2/3       *
    lda     #$00                    ;2   =  11 *
Lf85e
    sta     ram_8B                  ;3         *
    lda     Lfdb2,y                 ;4         *
    jmp     Lf872                   ;3   =  10 *
    
Lf866
    lsr                             ;2         *
    bcc     Lf86f                   ;2/3       *
    lda     Lfdba,y                 ;4         *
    jmp     Lf872                   ;3   =  11 *
    
Lf86f
    lda     Lfdc2,y                 ;4   =   4 *
Lf872
    ldx     ram_9F                  ;3         *
    cpx     #$00                    ;2         *
    bmi     Lf87e                   ;2/3       *
    ldx     ram_B9                  ;3         *
    cpx     #$00                    ;2         *
    beq     Lf883                   ;2/3 =  14 *
Lf87e
    clc                             ;2         *
    adc     ram_91                  ;3         *
    sta     ram_91                  ;3   =   8 *
Lf883
    lda     ram_85                  ;3         *
    and     #$f3                    ;2         *
    sta     ram_85                  ;3         *
    lda     ram_BA                  ;3         *
    asl                             ;2         *
    asl                             ;2         *
    ora     ram_85                  ;3         *
    sta     ram_85                  ;3         *
    lda     ram_9F                  ;3         *
    and     #$e0                    ;2         *
    ora     ram_B9                  ;3         *
    sta     ram_9F                  ;3         *
    rts                             ;6   =  38 *
    
Lf89a
    cpy     #$04                    ;2         *
    beq     Lf8b3                   ;2/3       *
    lda.wy  ram_81,y                ;4         *
    tax                             ;2         *
    and     #$04                    ;2         *
    beq     Lf8a9                   ;2/3       *
    jmp     Lf94b                   ;3   =  17 *
    
Lf8a9
    txa                             ;2         *
    and     #$7f                    ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    jmp     Lf8c0                   ;3   =  15 *
    
Lf8b3
    lda     #$20                    ;2         *
    bit     ram_94                  ;3         *
    beq     Lf8bc                   ;2/3       *
    jmp     Lf99e                   ;3   =  10 *
    
Lf8bc
    lda     ram_94                  ;3         *
    and     #$03                    ;2   =   5 *
Lf8c0
    tax                             ;2         *
    cmp     #$05                    ;2         *
    beq     Lf8ca                   ;2/3       *
    lda     Lfdca,x                 ;4         *
    bne     Lf8cc                   ;2/3 =  12 *
Lf8ca
    lda     ram_8A                  ;3   =   3 *
Lf8cc
    sec                             ;2         *
    sbc.wy  ram_86,y                ;4         *
    sta     ram_BA                  ;3         *
    bcc     Lf8d8                   ;2/3       *
    lda     #$01                    ;2         *
    bne     Lf8e1                   ;2/3 =  15 *
Lf8d8
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$01                    ;2         *
    sta     ram_BA                  ;3         *
    lda     #$03                    ;2   =  11 *
Lf8e1
    sta     ram_B9                  ;3         *
    lda     ram_BA                  ;3         *
    lsr                             ;2         *
    clc                             ;2         *
    adc     ram_BA                  ;3         *
    sta     ram_BA                  ;3         *
    cpx     #$05                    ;2         *
    beq     Lf8f4                   ;2/3       *
    lda     Lfdcf,x                 ;4         *
    bne     Lf8f6                   ;2/3 =  26 *
Lf8f4
    lda     ram_90                  ;3   =   3 *
Lf8f6
    sec                             ;2         *
    sbc.wy  ram_8C,y                ;4         *
    sta     ram_99                  ;3         *
    beq     Lf917                   ;2/3!      *
    bcc     Lf908                   ;2/3       *
    lda     ram_B9                  ;3         *
    ora     #$08                    ;2         *
    sta     ram_B9                  ;3         *
    bne     Lf90f                   ;2/3 =  23 *
Lf908
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$01                    ;2         *
    sta     ram_99                  ;3   =   9 *
Lf90f
    lda     ram_99                  ;3         *
    cmp     ram_BA                  ;3         *
    bcc     Lf92f                   ;2/3       *
    bcs     Lf929                   ;2/3 =  10 *
Lf917
    lda     ram_BA                  ;3         *
    beq     Lf930                   ;2/3       *
    lda     ram_9E                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    bcs     Lf92f                   ;2/3       *
    lda     ram_B9                  ;3         *
    ora     #$08                    ;2         *
    sta     ram_B9                  ;3         *
    bne     Lf92f                   ;2/3 =  24 *
Lf929
    lda     ram_B9                  ;3         *
    ora     #$80                    ;2         *
    sta     ram_B9                  ;3   =   8 *
Lf92f
    rts                             ;6   =   6 *
    
Lf930
    cpy     #$04                    ;2         *
    bne     Lf937                   ;2/3       *
    jmp     Lf996                   ;3   =   7 *
    
Lf937
    lda.wy  ram_81,y                ;4         *
    and     #$83                    ;2         *
    ora     #$04                    ;2         *
    sta     ram_B9                  ;3         *
    lda     ram_9E                  ;3         *
    and     #$38                    ;2         *
    ora     ram_B9                  ;3         *
    sta.wy  ram_81,y                ;5         *
    bne     Lf98b                   ;2/3 =  26 *
Lf94b
    lda.wy  ram_81,y                ;4         *
    tax                             ;2         *
    and     #$38                    ;2         *
    beq     Lf95e                   ;2/3       *
    lda.wy  ram_81,y                ;4         *
    sec                             ;2         *
    sbc     #$08                    ;2         *
    sta.wy  ram_81,y                ;5         *
    bne     Lf98b                   ;2/3 =  25 *
Lf95e
    lda.wy  ram_81,y                ;4         *
    and     #$83                    ;2         *
    ora     #$50                    ;2         *
    sta.wy  ram_81,y                ;5         *
    lda     ram_F6                  ;3         *
    and     #$78                    ;2         *
    sta     ram_F6                  ;3         *
    lda     Lfdd4,y                 ;4         *
    ora     ram_92                  ;3         *
    sta     ram_92                  ;3         *
    and     #$0f                    ;2         *
    cmp     #$0f                    ;2         *
    bne     Lf98b                   ;2/3       *
    lda     ram_90                  ;3         *
    adc     ram_9E                  ;3         *
    eor     ram_F7                  ;3         *
    eor     ram_8A                  ;3         *
    and     #$07                    ;2         *
    ora     ram_F6                  ;3         *
    ora     #$80                    ;2         *
    sta     ram_F6                  ;3   =  59 *
Lf98b
    lda     ram_9E                  ;3         *
    and     #$0f                    ;2         *
    tax                             ;2         *
    lda     Lfd1e,x                 ;4         *
    sta     ram_B9                  ;3         *
    rts                             ;6   =  20 *
    
Lf996
    lda     ram_94                  ;3         *
    and     #$c0                    ;2         *
    ora     #$2f                    ;2         *
    sta     ram_94                  ;3   =  10 *
Lf99e
    lda     ram_94                  ;3         *
    sec                             ;2         *
    sbc     #$01                    ;2         *
    sta     ram_94                  ;3         *
    and     #$20                    ;2         *
    beq     Lf9b8                   ;2/3       *
    lda     ram_9E                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    and     #$0f                    ;2         *
    tax                             ;2         *
    lda     Lfd1e,x                 ;4         *
    sta     ram_B9                  ;3         *
    rts                             ;6   =  42 *
    
Lf9b8
    lda     ram_94                  ;3         *
    and     #$c0                    ;2         *
    sta     ram_94                  ;3         *
    lda     ram_9E                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    and     #$03                    ;2         *
    ora     ram_94                  ;3         *
    sta     ram_94                  ;3         *
    lda     #$81                    ;2         *
    sta     ram_B9                  ;3         *
    rts                             ;6   =  36 *
    
Lf9ce
    cpy     #$04                    ;2         *
    beq     Lfa1f                   ;2/3!      *
    lda.wy  ram_86,y                ;4         *
    cmp     #$12                    ;2         *
    bcc     Lf9dd                   ;2/3       *
    cmp     #$ab                    ;2         *
    bcc     Lf9ea                   ;2/3 =  16 *
Lf9dd
    lda     Lfd1a,y                 ;4         *
    eor     ram_92                  ;3         *
    sta     ram_92                  ;3         *
    and     Lfd1a,y                 ;4         *
    beq     Lf9ea                   ;2/3       *
    rts                             ;6   =  22 *
    
Lf9ea
    lda.wy  ram_81,y                ;4         *
    and     #$70                    ;2         *
    cmp     #$40                    ;2         *
    beq     Lf9fe                   ;2/3       *
    cmp     #$60                    ;2         *
    beq     Lfa1c                   ;2/3!      *
    cmp     #$70                    ;2         *
    bne     Lfa1f                   ;2/3!      *
    jmp     Lfb84                   ;3   =  21 *
    
Lf9fe
    lda     ram_F6                  ;3         *
    and     #$78                    ;2         *
    sta     ram_F6                  ;3         *
    lda.wy  ram_86,y                ;4         *
    cmp     #$58                    ;2         *
    bne     Lfa1f                   ;2/3       *
    lda.wy  ram_8C,y                ;4         *
    cmp     #$32                    ;2         *
    bne     Lfa1f                   ;2/3       *
    lda.wy  ram_81,y                ;4         *
    and     #$8f                    ;2         *
    ora     #$60                    ;2         *
    sta.wy  ram_81,y                ;5   =  37 *
Lfa1c
    jmp     Lfb64                   ;3   =   3 *
    
Lfa1f
    lda.wy  ram_81,y                ;4         *
    lsr                             ;2         *
    bcc     Lfa48                   ;2/3       *
    lda.wy  ram_86,y                ;4         *
    cmp     #$58                    ;2         *
    bcc     Lfa38                   ;2/3       *
    cmp     #$60                    ;2         *
    bcs     Lfa40                   ;2/3       *
    and     #$07                    ;2         *
    cmp     #$00                    ;2         *
    bne     Lfa52                   ;2/3       *
    beq     Lfa73                   ;2/3 =  28 *
Lfa38
    and     #$07                    ;2         *
    cmp     #$02                    ;2         *
    bne     Lfa52                   ;2/3       *
    beq     Lfa73                   ;2/3 =   8 *
Lfa40
    and     #$07                    ;2         *
    cmp     #$06                    ;2         *
    bne     Lfa52                   ;2/3       *
    beq     Lfa73                   ;2/3 =   8 *
Lfa48
    lda.wy  ram_8C,y                ;4         *
    jsr     Lfcf4                   ;6         *
    cmp     #$02                    ;2         *
    beq     Lfa73                   ;2/3 =  14 *
Lfa52
    cpy     #$04                    ;2         *
    beq     Lfa59                   ;2/3       *
    jmp     Lfb35                   ;3   =   7 *
    
Lfa59
    bit     ram_94                  ;3         *
    bvc     Lfa60                   ;2/3       *
    jmp     Lfb35                   ;3   =   8 *
    
Lfa60
    jsr     Lfc22                   ;6         *
    ldy     ram_B8                  ;3         *
    lda     ram_99                  ;3         *
    eor     ram_BA                  ;3         *
    cmp     #$02                    ;2         *
    beq     Lfa70                   ;2/3       *
    jmp     Lfb35                   ;3   =  22 *
    
Lfa70
    jmp     Lfb2b                   ;3   =   3 *
    
Lfa73
    dec     ram_96                  ;5         *
    bpl     Lfa78                   ;2/3       *
    rts                             ;6   =  13 *
    
Lfa78
    ldx     ram_86,y                ;4         *
    lda.wy  ram_8C,y                ;4         *
    jsr     Lfc72                   ;6         *
    sta     ram_B7                  ;3         *
    ldy     ram_B8                  ;3         *
    cpy     #$04                    ;2         *
    bne     Lfa8f                   ;2/3       *
    bit     ram_94                  ;3         *
    bvs     Lfa8f                   ;2/3       *
    jmp     Lfb13                   ;3   =  32 *
    
Lfa8f
    jsr     Lf89a                   ;6         *
    lda.wy  ram_81,y                ;4         *
    and     #$03                    ;2         *
    sta     ram_BA                  ;3         *
    lda     ram_B9                  ;3         *
    bpl     Lfa9f                   ;2/3       *
    lsr                             ;2         *
    lsr                             ;2   =  24 *
Lfa9f
    and     #$03                    ;2         *
    sta     ram_99                  ;3         *
    eor     ram_BA                  ;3         *
    beq     Lfacc                   ;2/3       *
    cmp     #$02                    ;2         *
    beq     Lfaf0                   ;2/3       *
    ldx     ram_99                  ;3         *
    lda     Lfdd4,x                 ;4         *
    and     ram_B7                  ;3         *
    beq     Lfab7                   ;2/3       *
    jmp     Lfb2b                   ;3   =  29 *
    
Lfab7
    ldx     ram_BA                  ;3         *
    lda     Lfdd4,x                 ;4         *
    and     ram_B7                  ;3         *
    beq     Lfac3                   ;2/3       *
    jmp     Lfb35                   ;3   =  15 *
    
Lfac3
    lda     ram_99                  ;3         *
    eor     #$02                    ;2         *
    sta     ram_99                  ;3         *
    jmp     Lfb2b                   ;3   =  11 *
    
Lfacc
    ldx     ram_BA                  ;3         *
    lda     Lfdd4,x                 ;4         *
    and     ram_B7                  ;3         *
    bne     Lfb35                   ;2/3!      *
    lda     ram_B9                  ;3         *
    bmi     Lfadb                   ;2/3       *
    lsr                             ;2         *
    lsr                             ;2   =  21 *
Lfadb
    and     #$03                    ;2         *
    sta     ram_99                  ;3         *
    tax                             ;2         *
    lda     Lfdd4,x                 ;4         *
    and     ram_B7                  ;3         *
    bne     Lfb2b                   ;2/3!      *
    lda     ram_99                  ;3         *
    eor     #$02                    ;2         *
    sta     ram_99                  ;3         *
    jmp     Lfb2b                   ;3   =  27 *
    
Lfaf0
    lda     ram_B9                  ;3         *
    bmi     Lfaf6                   ;2/3       *
    lsr                             ;2         *
    lsr                             ;2   =   9 *
Lfaf6
    and     #$03                    ;2         *
    sta     ram_99                  ;3         *
    tax                             ;2         *
    lda     Lfdd4,x                 ;4         *
    and     ram_B7                  ;3         *
    bne     Lfb2b                   ;2/3       *
    lda     ram_99                  ;3         *
    eor     #$02                    ;2         *
    sta     ram_99                  ;3         *
    tax                             ;2         *
    lda     Lfdd4,x                 ;4         *
    and     ram_B7                  ;3         *
    bne     Lfb2b                   ;2/3       *
    jmp     Lfb35                   ;3   =  38 *
    
Lfb13
    jsr     Lfc22                   ;6         *
    ldy     ram_B8                  ;3         *
    ldx     ram_99                  ;3         *
    lda     Lfdd4,x                 ;4         *
    and     ram_B7                  ;3         *
    bne     Lfb2b                   ;2/3       *
    ldx     ram_BA                  ;3         *
    lda     Lfdd4,x                 ;4         *
    and     ram_B7                  ;3         *
    bne     Lfb35                   ;2/3       *
    rts                             ;6   =  39 *
    
Lfb2b
    lda.wy  ram_81,y                ;4         *
    and     #$fc                    ;2         *
    ora     ram_99                  ;3         *
    sta.wy  ram_81,y                ;5   =  14 *
Lfb35
    lda.wy  ram_81,y                ;4         *
    and     #$03                    ;2         *
    lsr                             ;2         *
    bcc     Lfb5d                   ;2/3       *
    tax                             ;2         *
    lda     Lff83,x                 ;4         *
    clc                             ;2         *
    adc.wy  ram_86,y                ;4         *
    sta.wy  ram_86,y                ;5         *
    cmp     #$0c                    ;2         *
    bcs     Lfb52                   ;2/3       *
    lda     #$ab                    ;2         *
    sta.wy  ram_86,y                ;5         *
    rts                             ;6   =  44 *
    
Lfb52
    cmp     #$ac                    ;2         *
    bcs     Lfb57                   ;2/3       *
    rts                             ;6   =  10 *
    
Lfb57
    lda     #$0c                    ;2         *
    sta.wy  ram_86,y                ;5         *
    rts                             ;6   =  13 *
    
Lfb5d
    tax                             ;2         *
    lda     Lff84,x                 ;4         *
    jmp     Lfba9                   ;3   =   9 *
    
Lfb64
    lda.wy  ram_8C,y                ;4         *
    cmp     #$42                    ;2         *
    bne     Lfba7                   ;2/3       *
    lda.wy  ram_81,y                ;4         *
    and     #$10                    ;2         *
    ora     #$70                    ;2         *
    sta     ram_B9                  ;3         *
    lda     ram_F7                  ;3         *
    sbc     ram_9E                  ;3         *
    eor     ram_8A                  ;3         *
    and     #$03                    ;2         *
    ora     ram_B9                  ;3         *
    sta.wy  ram_81,y                ;5         *
    jmp     Lfbbc                   ;3   =  41 *
    
Lfb84
    lda     ram_F6                  ;3         *
    and     #$78                    ;2         *
    sta     ram_F6                  ;3         *
    lda.wy  ram_81,y                ;4         *
    and     #$08                    ;2         *
    bne     Lfbf6                   ;2/3       *
    lda.wy  ram_8C,y                ;4         *
    cmp     #$42                    ;2         *
    beq     Lfbb1                   ;2/3       *
    cmp     #$50                    ;2         *
    beq     Lfbc0                   ;2/3       *
    lda.wy  ram_86,y                ;4         *
    cmp     #$52                    ;2         *
    beq     Lfba7                   ;2/3 =  36 *
Lfba3
    lda     #$ff                    ;2         *
    bmi     Lfba9                   ;2/3 =   4 *
Lfba7
    lda     #$01                    ;2   =   2 *
Lfba9
    clc                             ;2         *
    adc.wy  ram_8C,y                ;4         *
    sta.wy  ram_8C,y                ;5         *
    rts                             ;6   =  17 *
    
Lfbb1
    lda.wy  ram_86,y                ;4         *
    cmp     #$58                    ;2         *
    beq     Lfbd1                   ;2/3       *
    cmp     #$52                    ;2         *
    beq     Lfba7                   ;2/3 =  12 *
Lfbbc
    lda     #$ff                    ;2         *
    bmi     Lfbc9                   ;2/3 =   4 *
Lfbc0
    lda.wy  ram_86,y                ;4         *
    cmp     #$5e                    ;2         *
    beq     Lfba3                   ;2/3 =   8 *
Lfbc7
    lda     #$01                    ;2   =   2 *
Lfbc9
    clc                             ;2         *
    adc.wy  ram_86,y                ;4         *
    sta.wy  ram_86,y                ;5         *
    rts                             ;6   =  17 *
    
Lfbd1
    lda.wy  ram_81,y                ;4         *
    and     #$f0                    ;2         *
    sta     ram_B9                  ;3         *
    lda.wy  ram_81,y                ;4         *
    and     #$03                    ;2         *
    clc                             ;2         *
    adc     #$ff                    ;2         *
    cmp     #$ff                    ;2         *
    beq     Lfbeb                   ;2/3       *
    ora     ram_B9                  ;3         *
    sta.wy  ram_81,y                ;5         *
    bne     Lfbbc                   ;2/3 =  33 *
Lfbeb
    lda.wy  ram_81,y                ;4         *
    ora     #$08                    ;2         *
    sta.wy  ram_81,y                ;5         *
    jmp     Lfba3                   ;3   =  14 *
    
Lfbf6
    lda.wy  ram_8C,y                ;4         *
    cmp     #$32                    ;2         *
    bne     Lfba3                   ;2/3       *
    lda.wy  ram_81,y                ;4         *
    and     #$80                    ;2         *
    sta     ram_B9                  ;3         *
    lda     ram_8A                  ;3         *
    adc     ram_90                  ;3         *
    eor     ram_9E                  ;3         *
    eor     ram_F7                  ;3         *
    and     #$03                    ;2         *
    tax                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    ora     ram_B9                  ;3         *
    sta     ram_B9                  ;3         *
    lda     Lff7f,x                 ;4         *
    ora     ram_B9                  ;3         *
    sta.wy  ram_81,y                ;5         *
    jmp     Lfb35                   ;3   =  62 *
    
Lfc22
    lda     SWCHA                   ;4         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    eor     #$0f                    ;2         *
    beq     Lfc69                   ;2/3       *
    cmp     #$05                    ;2         *
    bmi     Lfc5c                   ;2/3       *
    cmp     #$08                    ;2         *
    beq     Lfc5c                   ;2/3       *
    sta     ram_B9                  ;3         *
    lda     ram_85                  ;3         *
    and     #$03                    ;2         *
    tay                             ;2         *
    lda     Lfde1,y                 ;4         *
    tay                             ;2         *
    and     ram_B9                  ;3         *
    bne     Lfc59                   ;2/3       *
    tya                             ;2         *
    ora     ram_B9                  ;3         *
    eor     #$03                    ;2         *
    beq     Lfc52                   ;2/3       *
    lda     ram_B9                  ;3         *
    and     #$0c                    ;2         *
    jmp     Lfc5c                   ;3   =  62 *
    
Lfc52
    lda     ram_B9                  ;3         *
    and     #$03                    ;2         *
    jmp     Lfc5c                   ;3   =   8 *
    
Lfc59
    tay                             ;2         *
    eor     ram_B9                  ;3   =   5 *
Lfc5c
    tax                             ;2         *
    lda     Lfdd8,x                 ;4         *
    sta     ram_99                  ;3         *
    lda     ram_85                  ;3         *
    and     #$03                    ;2         *
    sta     ram_BA                  ;3         *
    rts                             ;6   =  23 *
    
Lfc69
    lda     ram_85                  ;3         *
    and     #$03                    ;2         *
    sta     ram_BA                  ;3         *
    sta     ram_99                  ;3         *
    rts                             ;6   =  17 *
    
Lfc72
    lsr                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    lda     Lfde5,y                 ;4         *
    sta     ram_B7                  ;3         *
    asl                             ;2         *
    adc     ram_B7                  ;3         *
    asl                             ;2         *
    sta     ram_B7                  ;3         *
    txa                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    ldx     #$00                    ;2         *
    cmp     #$0c                    ;2         *
    bmi     Lfc91                   ;2/3       *
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$16                    ;2         *
    ldx     #$01                    ;2   =  45 *
Lfc91
    lsr                             ;2         *
    bcc     Lfcb6                   ;2/3       *
    clc                             ;2         *
    adc     ram_B7                  ;3         *
    sta     ram_B7                  ;3         *
    lda     ram_80                  ;3         *
    asl                             ;2         *
    tay                             ;2         *
    lda     Lff77,y                 ;4         *
    clc                             ;2         *
    adc     ram_B7                  ;3         *
    sta     ram_B9                  ;3         *
    iny                             ;2         *
    lda     Lff77,y                 ;4         *
    adc     #$00                    ;2         *
    sta     ram_BA                  ;3         *
    ldy     #$00                    ;2         *
    lda     (ram_B9),y              ;5         *
    cpx     #$00                    ;2         *
    bne     Lfcdb                   ;2/3       *
    rts                             ;6   =  59 *
    
Lfcb6
    adc     ram_B7                  ;3         *
    sta     ram_B7                  ;3         *
    lda     ram_80                  ;3         *
    asl                             ;2         *
    tay                             ;2         *
    lda     Lff77,y                 ;4         *
    clc                             ;2         *
    adc     ram_B7                  ;3         *
    sta     ram_B9                  ;3         *
    iny                             ;2         *
    lda     Lff77,y                 ;4         *
    adc     #$00                    ;2         *
    sta     ram_BA                  ;3         *
    ldy     #$00                    ;2         *
    lda     (ram_B9),y              ;5         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    cpx     #$00                    ;2         *
    bne     Lfcdb                   ;2/3       *
    rts                             ;6   =  61 *
    
Lfcdb
    tax                             ;2         *
    and     #$0a                    ;2         *
    sta     ram_B7                  ;3         *
    txa                             ;2         *
    and     #$05                    ;2         *
    tax                             ;2         *
    and     #$04                    ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    ora     ram_B7                  ;3         *
    sta     ram_B7                  ;3         *
    txa                             ;2         *
    and     #$01                    ;2         *
    asl                             ;2         *
    asl                             ;2         *
    ora     ram_B7                  ;3         *
    rts                             ;6   =  42 *
    
Lfcf4
    tax                             ;2         *
    and     #$03                    ;2         *
    sta     ram_B9                  ;3         *
    txa                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tax                             ;2         *
    lda     Lff86,x                 ;4         *
    clc                             ;2         *
    adc     ram_B9                  ;3         *
    rts                             ;6   =  30 *
    
Lfd04
    ldx     ram_80                  ;3        
    lda     Lfd16,x                 ;4        
    sta     COLUPF                  ;3        
    ldx     #$29                    ;2   =  12
Lfd0d
    lda     Lffc5,x                 ;4        
    sta     ram_BB,x                ;4        
    dex                             ;2        
    bpl     Lfd0d                   ;2/3      
    rts                             ;6   =  18
    
Lfd16
    .byte   GREEN|$a                          ; $fd16 (CP)
    
    .byte   $8a,$08,$d6                     ; $fd17 (*)
Lfd1a
    .byte   $80,$40,$20,$10                 ; $fd1a (*)
Lfd1e
    .byte   $1b,$09,$03,$01,$01,$0b,$0b,$03 ; $fd1e (*)
    .byte   $01,$09,$03,$09,$0b,$03,$03,$19 ; $fd26 (*)
Lfd2e
    .byte   $09,$0e,$00,$07,$07,$0e,$10,$05 ; $fd2e (*)
    .byte   $05,$0e,$00,$04,$09,$10,$00,$07 ; $fd36 (*)
Lfd3e
    .byte   $00,$05,$09,$10,$10,$05,$07,$0e ; $fd3e (*)
    .byte   $0e,$05,$09,$0d,$00,$07,$09,$10 ; $fd46 (*)
Lfd4e
    .byte   $0b,$a4,$a4,$0b                 ; $fd4e (*)
Lfd52
    .byte   $31,$31,$61,$61,$3d,$3d,$9d,$9d ; $fd52 (*)
    .byte   $61,$61,$61,$61,$47,$47,$61,$61 ; $fd5a (*)
Lfd62
    .byte   $aa,$a8,$7b,$fd,$ea,$ad,$ca,$ac ; $fd62 (*)
Lfd6a
    .byte   $50,$5a,$55,$00,$f0,$ff,$e8,$00 ; $fd6a (*)
    .byte   $fa,$f0,$fc,$00,$50,$55,$40,$00 ; $fd72 (*)
    .byte   $55,$95,$00,$00,$ff,$03,$eb,$00 ; $fd7a (*)
    .byte   $ff,$04,$30,$3f,$55,$0c,$15,$10 ; $fd82 (*)
    .byte   $40,$15,$a6,$94,$c0,$3f,$fc,$00 ; $fd8a (*)
    .byte   $fc,$3e,$bc,$00,$56,$95,$94,$00 ; $fd92 (*)
    .byte   $50,$50,$5a,$40,$f0,$fa,$fc,$00 ; $fd9a (*)
    .byte   $fa,$ff,$0c,$00,$5a,$50,$05,$40 ; $fda2 (*)
Lfdaa
    .byte   $55,$6a,$bf,$f0,$00,$00,$00,$00 ; $fdaa (*)
Lfdb2
    .byte   $00,$01,$01,$02,$00,$fe,$ff,$ff ; $fdb2 (*)
Lfdba
    .byte   $01,$04,$03,$04,$01,$00,$ff,$00 ; $fdba (*)
Lfdc2
    .byte   $ff,$00,$01,$00,$ff,$fc,$fd,$fc ; $fdc2 (*)
Lfdca
    .byte   $12,$9e,$9e,$12,$58             ; $fdca (*)
Lfdcf
    .byte   $02,$02,$92,$92,$32             ; $fdcf (*)
Lfdd4
    .byte   $08,$04,$02,$01                 ; $fdd4 (*)
Lfdd8
    .byte   $ff,$00,$02,$ff,$03,$ff,$ff,$ff ; $fdd8 (*)
    .byte   $01                             ; $fde0 (*)
Lfde1
    .byte   $01,$08,$02,$04                 ; $fde1 (*)
Lfde5
    .byte   $00,$00,$00,$01,$01,$01,$02,$02 ; $fde5 (*)
    .byte   $02,$03,$03,$03,$04,$04,$04,$05 ; $fded (*)
    .byte   $05,$05,$06,$06,$06,$07,$07,$07 ; $fdf5 (*)
    .byte   $08,$08,$08,$09,$09,$09,$0a,$0a ; $fdfd (*)
    .byte   $0a,$0b,$0b,$0b,$0c,$0c,$0c,$0d ; $fe05 (*)
    .byte   $0d,$0d,$0e,$0e,$0e,$0f,$0f,$0f ; $fe0d (*)
    .byte   $10,$10,$10,$11,$11,$11,$12,$12 ; $fe15 (*)
    .byte   $12,$13,$13,$13,$14,$14,$14,$15 ; $fe1d (*)
    .byte   $15,$15,$00,$65,$53,$06,$55,$55 ; $fe25 (*)
    .byte   $00,$a0,$0a,$0a,$00,$00,$00,$c7 ; $fe2d (*)
    .byte   $5f,$5d,$75,$55,$00,$0a,$0a,$00 ; $fe35 (*)
    .byte   $a0,$00,$55,$5b,$0c,$55,$f5,$55 ; $fe3d (*)
    .byte   $00,$0a,$00,$00,$a0,$6f,$00,$0e ; $fe45 (*)
    .byte   $55,$55,$b0,$aa,$00,$0a,$00,$00 ; $fe4d (*)
    .byte   $a0,$cf,$55,$5b,$06,$57,$d7,$55 ; $fe55 (*)
    .byte   $00,$0a,$0a,$0a,$0a,$00,$00,$6d ; $fe5d (*)
    .byte   $5b,$0a,$0e,$55,$00,$a0,$0a,$0c ; $fe65 (*)
    .byte   $5b,$00,$00,$a0,$0a,$00,$0a,$00 ; $fe6d (*)
    .byte   $00,$c5,$5d,$55,$5d,$55,$00,$65 ; $fe75 (*)
    .byte   $55,$57,$55,$57,$00,$a0,$00,$0a ; $fe7d (*)
    .byte   $00,$0a,$00,$c3,$06,$5b,$06,$5d ; $fe85 (*)
    .byte   $00,$0a,$0a,$0a,$0a,$00,$00,$0a ; $fe8d (*)
    .byte   $0a,$0c,$7d,$55,$05,$7d,$5b,$00 ; $fe95 (*)
    .byte   $a0,$6f,$00,$a0,$0e,$55,$b0,$aa ; $fe9d (*)
    .byte   $00,$a0,$0a,$00,$a0,$cf,$00,$a0 ; $fea5 (*)
    .byte   $0a,$06,$d5,$55,$00,$c5,$7d,$5b ; $fead (*)
    .byte   $00,$00,$00,$00,$a0,$0a,$06,$57 ; $feb5 (*)
    .byte   $00,$65,$d3,$0e,$59,$0a,$00,$a0 ; $febd (*)
    .byte   $0a,$0a,$00,$0a,$05,$d5,$59,$0c ; $fec5 (*)
    .byte   $55,$5d,$00,$65,$57,$53,$06,$55 ; $fecd (*)
    .byte   $00,$a0,$0a,$0e,$5b,$00,$00,$c5 ; $fed5 (*)
    .byte   $79,$0a,$0c,$57,$00,$00,$a0,$0a ; $fedd (*)
    .byte   $00,$0a,$00,$65,$d7,$5d,$75,$5d ; $fee5 (*)
    .byte   $00,$a0,$0a,$00,$a0,$6f,$00,$a0 ; $feed (*)
    .byte   $6d,$30,$a0,$aa,$00,$a0,$a0,$a0 ; $fef5 (*)
    .byte   $a0,$cf,$05,$d5,$b0,$c7,$d7,$55 ; $fefd (*)
    .byte   $00,$00,$a0,$0a,$0a,$00,$00,$65 ; $ff05 (*)
    .byte   $f5,$5b,$0e,$55,$00,$a0,$a0,$0e ; $ff0d (*)
    .byte   $5b,$00,$00,$a0,$a0,$0a,$0a,$00 ; $ff15 (*)
    .byte   $00,$c5,$d5,$59,$0c,$55,$00,$65 ; $ff1d (*)
    .byte   $75,$57,$55,$55,$00,$a0,$a0,$0a ; $ff25 (*)
    .byte   $00,$00,$00,$a0,$c7,$5b,$06,$55 ; $ff2d (*)
    .byte   $00,$a0,$0a,$0a,$0a,$00,$00,$c7 ; $ff35 (*)
    .byte   $5b,$0c,$7d,$55,$00,$0a,$0a,$00 ; $ff3d (*)
    .byte   $a0,$6f,$05,$59,$0e,$55,$b0,$aa ; $ff45 (*)
    .byte   $00,$00,$0a,$00,$a0,$cf,$05,$53 ; $ff4d (*)
    .byte   $0a,$06,$d7,$55,$00,$0a,$0a,$0a ; $ff55 (*)
    .byte   $0a,$00,$00,$6d,$7d,$5b,$0c,$57 ; $ff5d (*)
    .byte   $00,$a0,$a0,$0a,$00,$0a,$00,$a0 ; $ff65 (*)
    .byte   $a0,$0a,$06,$5d,$00,$c5,$d5,$5d ; $ff6d (*)
    .byte   $59,$00                         ; $ff75 (*)
Lff77
    .byte   $27,$fe,$7b,$fe,$cf,$fe,$23,$ff ; $ff77 (*)
Lff7f
    .byte   $03,$01,$01,$03                 ; $ff7f (*)
Lff83
    .byte   $01                             ; $ff83 (*)
Lff84
    .byte   $ff,$01                         ; $ff84 (*)
Lff86
    .byte   $00,$04,$08,$00,$04,$08,$00,$04 ; $ff86 (*)
    .byte   $08,$00,$04,$08,$00,$04,$08,$00 ; $ff8e (*)
    .byte   $04,$08,$00,$04,$08,$00,$04,$08 ; $ff96 (*)
    .byte   $00,$04,$08,$00,$04,$08,$00,$04 ; $ff9e (*)
    .byte   $08,$00,$04,$08,$00,$04,$08,$00 ; $ffa6 (*)
    .byte   $04,$08,$00,$04,$08             ; $ffae (*)
Lffb3
    .byte   $40,$80,$20,$08,$02,$01,$04,$10 ; $ffb3 (*)
    .byte   $40,$40,$10,$04,$01,$02,$08,$20 ; $ffbb (*)
    .byte   $80,$10                         ; $ffc3 (*)
Lffc5
    .byte   $50,$ff,$ff,$00,$ff,$ff,$50,$ff ; $ffc5 (D)
    .byte   $ff,$50,$ff,$ff,$50,$ff,$ff,$50 ; $ffcd (D)
    .byte   $bf,$bf,$50,$bf,$bf,$50,$bf,$bf ; $ffd5 (D)
    .byte   $50,$ff,$ff,$50,$ff,$ff,$50,$ff ; $ffdd (D)
    .byte   $ff,$50,$ff,$ff,$00,$ff,$ff,$50 ; $ffe5 (D)
    .byte   $ff,$ff                         ; $ffed (D)
    .byte   $22,$4c,$f3                     ; $ffef (*)
    
Lfff2
    sta     Lfff8                   ;4        
    jmp     Lf092                   ;3   =   7
    
Lfff8
    .byte   $12 ;.JAM               ;0         *
    
    .byte   $22,$09,$82                     ; $fff9 (*)
    .byte   $00,$f0                         ; $fffc (D)
    .byte   $1e                             ; $fffe (*)
    .byte   $01                             ; $ffff (*)