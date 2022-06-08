IF	!DEF(GBMACROS_INC)
GBMACROS_INC	SET	1

;*******************************************************************************
;*
;* Commonly used code macros
;*
;* To Do: need to fix ROM bank switch for >256 bank usage. 
;*
;* Breakpoint - Inserts a no$gmb-style breakpoint of LD B,B ($40)
;* RGBSet 
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*******************************************************************************

INCLUDE	"Hardware.inc"

NINTENDO_LOGO: MACRO
  DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
  ENDM


Breakpoint: MACRO
  DB $40        ;a ld b,b instruction is a breakpoint for use in No$gmb, no 
  ENDM          ;effect on real hardware, other than wasting a cycle.

RGBSet: MACRO
  DW    ((\3 >> 3) << 10) + ((\2 >> 3) << 5) + (\1 >> 3)
  ENDM

PushAll:    MACRO
  push  af
  push  bc
  push  de
  push  hl
  ENDM

PopAll:     MACRO
  pop   hl
  pop   de
  pop   bc
  pop   af
  ENDM

Wait_VBlank_End:	MACRO
.waitvblankend\@:	; wait for the VBlank code to finish
	ldh	a,[G_NUSERFLAGS]
	and	USER_F_VBLANKEND
	jr	z,.waitvblankend\@
	ENDM

Clear_VBlank_Flag:	MACRO
	; clear the VBlank flag
	ldh	a,[G_NUSERFLAGS]
	and	~(USER_F_VBLANKEND)
	ldh	[G_NUSERFLAGS],a
	ENDM

Set_VBlank_Flag:	MACRO
	; set the VBlank end flag
	ldh	a,[G_NUSERFLAGS]
	or	USER_F_VBLANKEND
	ldh	[G_NUSERFLAGS],a
	ENDM

Wait_For_Next_HBlank:	MACRO
.waithblankstart\@:	; wait for the next HBlank
	ldh	a,[REG_STAT]
	and	REG_STAT_F_LCD	; bits 0 and 1
	cp	REG_STAT_F_HB	; check if zero
	jr	nz,.waithblankstart\@	; no (not in HBlank)
	ENDM

Wait_HBlank_Complete:	MACRO
.waithblankend\@:	; wait for the hblank to finish
	ldh	a,[REG_STAT]
	and	REG_STAT_F_LCD	; bits 0 and 1
	cp	REG_STAT_F_HB	; check if zero
	jr	z,.waithblankend\@	; yes (still in HBlank)
	ENDM

Wait_HDMA_Complete:	MACRO
.waithdmacomplete\@:	; wait for the DMA transfer to finish
	ldh	a,[REG_HDMA5]
	bit	7,a
	jr	z,.waithdmacomplete\@
	ENDM


;Destroys A, but faster than WaitVRAMPushAF
WaitVRAM:       MACRO
  ldh   a,[REG_STAT]    ; <---+
  and   \1              ;     |
  jr    nz,@-4          ; ----+ Jump back and reload
  ENDM

WaitVRAMPushAF: MACRO
  push  af
  ldh   a,[REG_STAT]    ; <---+
  and   \1              ;     |
  jr    nz,@-4          ; ----+ Jump back and reload
  pop   af
  ENDM

EnableCartSRAM: MACRO
  ld    a,$0A
  ldh   [BASE_RAMG],a
  ENDM

DisableCartRAM: MACRO
  xor   a
  ldh   [BASE_RAMG],a
  ENDM

SetROMBank:     MACRO   ; Bank #0-255 <=32Mbit
        ld    a,\1
        ld    [BASE_ROMB0],a
  ENDM
	
SetWRAMBank: MACRO	;Bank
	ld	a,\1
	ldh	[REG_SVBK],a
	ENDM

SetVideoBank: MACRO ;Bank 0,1
	IF  \1==0
		xor  a
		ldh  [REG_VBK],a
	ELSE
		ld   a,$01
		ldh  [REG_VBK],a
	ENDC
		
	ENDM
		
;Uncomment the following section if you wish to use a 64Mbit ROM
;Note: This is untested.
;
;IF    DEF MBC5_64MBIT
;SetROMBank512:  MACRO   ; Bank #0-511 =64Mbit
;        IF    \1>255
;              ld    a,\1-256
;              ld    [BASE_ROMB0],a
;              ld    a,1
;              ld    [BASE_ROMB1],a
;        ELSE
;              ld    a,\1
;              ld    [BASE_ROMB0],a
;              ld    a,0
;              ld    [BASE_ROMB1],a
;        ENDC  ;If bank #
;ENDM          ;End of Macro
;ENDC          ;End of 64Mbit condition




ENDC	; DEF GBMACROS_INC
