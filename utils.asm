	; system includes
	INCLUDE	"GBMacros.inc"
	INCLUDE	"Hardware.inc"

	; project includes
	IMPORT  InitReturn
	INCLUDE	"main.inc"
	INCLUDE	"utils.inc"
	INCLUDE	"vars.inc"



SECTION "Initialize", HOME   ; was in BANK[1]
;******************************************************************************
; The Init routines go into Bank 1, to save Home Bank (0) space
;******************************************************************************


;******************************************************************************
;InitializeGB
;
;Input: A=GB Type
;******************************************************************************
InitializeGB::
	di
	cp	CPU_GBC
	ld	a,$00
	jr	nz,.not_gbc
	ld	a,$01
.not_gbc:
	ldh	[g_nUserFlags],a              
	call	WaitForVblank

	xor	a
	ldh	[REG_IE],a    ;int enabl
	ldh	[REG_IF],a    ;int flag 
	ldh	[REG_SCX],a    ;lcd scrol
	ldh	[REG_SCY],a    ;lcd scrol
	ldh	[REG_STAT],a    ;lcd stat 

	ld	hl,BASE_MAPRAM9800
	call	ClearAllBGAttrMap

	ld	hl,BASE_MAPRAM9800
	call	ClearAllBGCharMap

	ld	hl,BASE_MAPRAM9C00
	call	ClearAllBGAttrMap

	ld	hl,BASE_MAPRAM9C00
	call	ClearAllBGCharMap

	ld	a,$E4
	ldh	[REG_BGP],a    ;lcd back 
	ldh	[REG_OBP0],a    ;lcd spr0 
	ldh	[REG_OBP1],a    ;lcd spr1 
	xor	a
	ldh	[REG_AUDENA],a    ;snd stat 
	ldh	[REG_WY],a    ;lcd win y
	ld	a,$07
	ldh	[REG_WX],a    ;lcd win x
	ldh	a,[g_nUserFlags]              
	and	USER_F_ISCGB
	jr	nz,GBC_Start
	

	ld	bc,DMG_Text_Tiles
	ld	hl,$8400 			; $8000+(tile# * $10]
	ld	de,(31*16)					;$01F0   (# of tile * 16 bytes each tile
	call	CopyToVRAM
	xor a
	ld	bc,DMG_Text_1
	ld	hl,BASE_MAPRAM9800
	ld	de,$0107			;is line 7, tile 1 (location]
	call	PrintText
	ld	bc,DMG_Text_2
	ld	hl,BASE_MAPRAM9800
	ld	de,$0309			;is line 9, tile 3
	call	PrintText
	ld	bc,DMG_Text_3
	ld	hl,BASE_MAPRAM9800
	ld	de,$030B		;is line B, tile 3
	call	PrintText
	ld	a,$91
	ldh	[REG_LCDC],a    ;lcd on, etc
	xor	a
	ldh	[REG_IF],a      ;int flag    Kill all interrupts to lock the GB into doing NOTHING after the HALT
	ldh	[REG_IE],a      ;int enabl
	ei  

.DMG_Loop:
	halt
	nop 
	nop 
	jr	.DMG_Loop

GBC_Start::
	call	ToggleCPUSpeed
	xor	a
	ldh	[REG_VBK],a		;vram bank
	ldh	[REG_SVBK],a	;ram bank 
	ldh	[REG_RP],a		;ir port  

DMG_Start::
	ld	hl,$DFFF		;Clear Ram from top to bottom (DFFF-C000]
	ld	c,$20			;16-bit loop of $20*$100 
	ld	b,$00					;      /
.RAM_Clear_Loop:				;     /
	ldd	[hl],a					;    /
	dec	b						;   /
	jr	nz,.RAM_Clear_Loop		;  /
	dec	c						; /
	jr	nz,.RAM_Clear_Loop		;/


	pop	de				;\
	ld	de,InitReturn	;This gets the address to return to.	
	push	de			;/


	ld	hl,$FE9F		;Clear OAM memory top-down
	ld	b,4 * 40		;40 sprites * 4 bytes each
.OAM_Clear_Loop:		;
	ldd	[hl],a			;
	dec	b				;
	jr	nz,.OAM_Clear_Loop		;
	
	
	ld	hl,$FFFE		;Clear HRAM from $FFFE to $FF80
	ld	b,$7E			;
.HRAM_Clear_Loop:		;
	ldd	[hl],a			;
	dec	b				;
	jr	nz,.HRAM_Clear_Loop		;
	
	
	ld	hl,HRAM_OAM_Routine	;Put V-Blank OAM DMA routine in HRAM
	ld	de,SPRITEDMA_ADDR		;$FFF0
	ld	b,HRAM_OAM_Routine_END - HRAM_OAM_Routine ;length of routine ;-]
.HRAM_OAM_Loader_Loop:
	ld	a,[hl+]
	ld	[de],a
	inc	de
	dec	b
	jr	nz,.HRAM_OAM_Loader_Loop
	ret

HRAM_OAM_Routine::
	push	af
	ld	a,$c0
	ldh	[REG_DMA],a
	ld	a,$28
.loop:
	dec	a
	jr	nz,.loop
	pop	af
	ret
HRAM_OAM_Routine_END::

	
;******************************************************************************
;ToggleCPUSpeed
;******************************************************************************
ToggleCPUSpeed::
	di
	ld	hl,REG_IE_W
	ld	a,[hl]
	push	af
	xor	a
	ld	[hl],a
	ldh	[REG_IF],a
	ld	a,$30
	ldh	[REG_P1],a
	ld	a,$1
	ldh	[REG_KEY1],a
	stop
	pop	af
	ld	[hl],a
	ei
	ret



SECTION "TextTileData", HOME  ; was in BANK[1]
DMG_Text_Tiles::
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;Blank
	db $00,$00,$3C,$00,$66,$18,$7E,$01,$66,$19,$66,$11,$66,$11,$00,$33 ;A
	db $00,$00,$7C,$00,$66,$18,$7C,$03,$66,$18,$66,$11,$7C,$03,$00,$3E ;B
	db $00,$00,$3C,$00,$66,$18,$60,$13,$60,$10,$66,$10,$3C,$03,$00,$1E ;C
	db $00,$00,$7C,$00,$66,$18,$66,$11,$66,$11,$66,$11,$7C,$03,$00,$3E ;D
	db $00,$00,$7E,$00,$60,$1F,$7C,$00,$60,$1E,$60,$10,$7E,$00,$00,$3F ;E
	db $00,$00,$7E,$00,$60,$1F,$7C,$00,$60,$1E,$60,$10,$60,$10,$00,$30 ;F
	db $00,$00,$3C,$00,$66,$18,$60,$13,$66,$10,$66,$11,$3E,$01,$00,$1F ;G
	db $00,$00,$66,$00,$66,$11,$7E,$01,$66,$19,$66,$11,$66,$11,$00,$33 ;H
	db $00,$00,$18,$00,$18,$04,$18,$04,$18,$04,$18,$04,$18,$04,$00,$0C ;I
	db $00,$00,$06,$00,$06,$01,$06,$01,$66,$01,$66,$11,$3C,$03,$00,$1E ;J
	db $00,$00,$66,$00,$6C,$13,$78,$06,$6C,$10,$66,$10,$66,$11,$00,$33 ;K
	db $00,$00,$60,$00,$60,$10,$60,$10,$60,$10,$60,$10,$7E,$00,$00,$3F ;L
	db $00,$00,$66,$00,$7E,$01,$5A,$25,$66,$09,$66,$11,$66,$11,$00,$33 ;M
	db $00,$00,$66,$00,$76,$01,$7E,$01,$6E,$11,$66,$11,$66,$11,$00,$33 ;N
	db $00,$00,$3C,$00,$66,$18,$66,$11,$66,$11,$66,$11,$3C,$03,$00,$1E ;O
	db $00,$00,$7C,$00,$66,$18,$7C,$03,$60,$1E,$60,$10,$60,$10,$00,$30 ;P
	db $00,$00,$3C,$00,$66,$18,$66,$11,$66,$11,$6E,$11,$3E,$01,$00,$1F ;Q
	db $00,$00,$7C,$00,$66,$18,$7C,$03,$66,$18,$66,$11,$66,$11,$00,$33 ;R
	db $00,$00,$3C,$00,$60,$1E,$3C,$00,$06,$18,$66,$01,$3C,$03,$00,$1E ;S
	db $00,$00,$7E,$00,$18,$27,$18,$04,$18,$04,$18,$04,$18,$04,$00,$0C ;T
	db $00,$00,$66,$00,$66,$11,$66,$11,$66,$11,$66,$11,$3C,$03,$00,$1E ;U
	db $00,$00,$66,$00,$66,$11,$66,$11,$66,$11,$3C,$03,$18,$06,$00,$0C ;V
	db $00,$00,$66,$00,$66,$11,$66,$11,$5A,$21,$7E,$01,$66,$19,$00,$33 ;W
	db $00,$00,$66,$00,$3C,$03,$18,$06,$18,$04,$3C,$00,$66,$18,$00,$33 ;X
	db $00,$00,$66,$00,$66,$11,$3C,$03,$18,$06,$18,$04,$18,$04,$00,$0C ;Y
	db $00,$00,$7E,$00,$06,$39,$0C,$03,$30,$06,$60,$18,$7E,$00,$00,$3F ;Z
	db $00,$00,$00,$00,$18,$00,$00,$0C,$00,$00,$18,$00,$00,$0C,$00,$00 ;:
	db $00,$00,$18,$00,$18,$04,$18,$04,$18,$04,$00,$0C,$18,$00,$00,$0C ;!
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$18,$00,$00,$0C ;.
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$00,$10,$04 ;,
	db $00,$00,$00,$00,$00,$00,$00,$00,$3C,$00,$00,$1E,$00,$00,$00,$00 ;-

DMG_Text_1::
	db "THIS@DEMO@CAN@ONLY",$00
DMG_Text_2::
	db "BE@VIEWED@ON@A@",$00
DMG_Text_3::
	db "GAME@BOY@COLOR",$00
	




SECTION "Utilities Home",HOME

;******************************************************************************
;*
;* SetMap 
;*
;* HL=source map
;* DE=dest map
;* B=X tiles wide
;* C=Y tiles tall
;* g_nSourceMapWidth=width of source map (tiles)
;* 
;******************************************************************************
SetMap::
	ld   a,[g_nSourceMapWidth]	;source map width-dest map width
	sub  b						;
	ld   [g_nSourceMapWidth],a	;
	ld   a,b					;
	ldh  [g_nTempVar],a	;Store dest width for later
.loop2:
	ldh  a,[g_nTempVar]	;Get dest width
	ld   b,a	
	push de						;get map address
.loop1:
	WaitVRAM 2
	ld   a,[hl+]				;
	ld   [de],a
	inc  de
	dec  b
	jr   nz,.loop1

	ld   a,[g_nSourceMapWidth]
	ld   e,a
	ld   d,0
	add  hl,de
	
	pop  de
	ld   a,$20
	add  e
	ld   e,a
	jr   nc,.skip
	ld   a,d
	inc  a
	ld   d,a
.skip:
	dec  c
	jr   nz,.loop2

	ret


;******************************************************************************
; CopyToVRAM
;
; Home Bank
; Copy DE bytes from BC to HL (Vram] during H-BLANK ONLY.
;******************************************************************************
CopyToVRAM::
	WaitVRAM 3	; Wait for VRAM
	ld   a,[bc]
	ldi  [hl],a
	inc  bc
	dec  de
	ld   a,d
	or   e
	jr   nz,CopyToVRAM
	ret

;******************************************************************************
; CopyMemory
;
; Home Bank
; Copy DE bytes from BC to HL
;******************************************************************************
CopyMemory::
	ld   a,[bc]
	ldi  [hl],a
	inc  bc
	dec  de
	ld   a,d
	or   e
	jr   nz,CopyMemory
	ret 


;******************************************************************************
;
;WaitForVblank
;
;******************************************************************************
WaitForVblank::
	ldh  a,[REG_IE]		;save IE
	push af				;/
	res  0,a				;turn off vblank interrupt;
	ldh  [REG_IE],a		;/
.loop:
	ldh  a,[REG_LY]
	cp   $91			;Are we in v-blank yet?
	jr	 nz,.loop
	pop  af				;Restore IE register
	ldh  [REG_IE],a		;/
	ret 				;done

	
	
;******************************************************************************
;*
;* Wait
;* B=frames to wait
;* Have to fix this sometime... =/
;* 
;******************************************************************************
;Wait::
;	ld   b,$FF
;.loop1:
;	push bc
;.loop2:
;	ld   a,[g_nUserFlags]
;	and  SCREENID_VERSION
;	jr   z,.loop2
;	ld   a,[g_nUserFlags]
;	and  $FD
;	ld   [g_nUserFlags],a
;	pop  bc
;	dec  b
;	jr   nz,.loop1
;	ret

	
;******************************************************************************
;* PrintText
;*
;* Home Bank
;* Print ZT-string BC on map HL (9800 or 9C00] at D,E (column,row]
;* A=(bit 7 - tile bank to print from] || (palette # to use 0-7]
;******************************************************************************
PrintText::
	push af
	push hl
	push bc
	ld   bc,$0020
	and  $7f
	ld   a,e
.loop1:
	or   a
	jr   z, .done1
	add  hl,bc
	dec  a
	jr   .loop1
.done1:
	ld   b,$00
	ld   c,d
	add  hl,bc
	ld   d,h
	ld   e,l
	pop  bc
	push bc
.wait_blank:
	WaitVRAM 2
	ld   a,[bc]
	or   a
	jr   z,.done2
	ld   a,[bc]
	ldi  [hl],a
	inc  bc
	jr   .wait_blank
.done2:
	pop  bc
	pop  hl
	pop  af
	ld   h,d
	ld   l,e
	ld   d,a
	and  $80
	srl  a
	srl  a
	srl  a
	srl  a
	ld   e,a
	ld   a,d
	and  $7F
	or   a
	ret  z
	
	ld   d,a
	SetVideoBank 1
.loop2:
	ld   a,[bc]
	or   a
	jr   z,.done3
.loop3:
	WaitVRAM 2
	ld   a,d
	or   e
	ldi  [hl],a
	inc  bc
	jr   .loop2
.done3:
	SetVideoBank 0 
 	ret

;******************************************************************************
; ClearAllBGAttrMap
;
; Home Bank
; HL contains map to clear atts for (9800,9C00]
;******************************************************************************
ClearAllBGAttrMap::
	SetVideoBank 1
	ld   d,0
	ld   c,$20
.loop1:
	ld   b,$20
.loop2:
	WaitVRAM 3
	ld   a,d
	ldi  [hl],a
	dec  b
	jr   nz,.loop2
	dec  c
	jr   nz,.loop1
	SetVideoBank 0
	ret


;******************************************************************************
; ClearAllBGCharMap
;
; Home Bank
; HL contains BG map to clear (9800, 9C00]
;******************************************************************************
ClearAllBGCharMap::
	xor  a
	ld   c,$20 ;rows of BG map to clear
.loop1:
	ld   b,$20 ;columns in each row (32]
.loop2:
	ldi  [hl],a
.loop3:
	WaitVRAM 3
	dec  b
	jr   nz,.loop2
	dec  c
	jr   nz,.loop1
	ret

;******************************************************************************
;
; SetBGAttrMap/
; SetBGCharMap
; HL=pointer to source
; DE=BG to set (9800 or 9C00)
; C=# of rows to set
;
;******************************************************************************
SetBGAttrMap:: 
	SetVideoBank 1
SetBGCharMap::
	ld   b,$14
.loop:
	WaitVRAM 3
	ld   a,[hl+]
	ld   [de],a
	inc  de
	dec  b
	jr   nz,.loop
	push hl
	ld   hl,$000C
	add  hl,de
	ld   d,h
	ld   e,l
	pop  hl
	dec  c
	jr   nz,SetBGCharMap
	SetVideoBank 0
	ret 
	
;******************************************************************************
; TurnOffDisplay
;
; Home Bank
; waits for vblank with vblint turned OFF (restores previous state after]
;******************************************************************************
TurnOffDisplay::			;Waits for V-Blank with vblint OFF
	ld   hl,REG_LCDC_W	;$FF40
	bit  7,[hl]
	ret  z				;ret if screen already off
	ldh  a,[REG_IE]		;save IE
	push af				;/
	res  0,a				;turn off vblank interrupt;
	ldh  [REG_IE],a		;/
.loop:
	ldh  a,[REG_LY]
	cp   $91				;Are we in v-blank yet?
	jr	 nz,.loop
	res  7,[hl]			;Turn off screen, we're in vblank.
	pop  af				;Restore IE register
	ldh  [REG_IE],a		;/
	ret 				;done


;******************************************************************************
;
;SetOneBGPalette
;HL=pointer to palette data
;A=Palette to change [0-7]
;
;******************************************************************************
SetOneBGPalette::
	add  a,$80			;set bit 7 to auto-increment palette while loading
	ldh  [REG_BCPS],a   ;bgp index
	ld   bc,$0869
.loop:
	WaitVRAM 2
	ld   a,[hl+]
	ld   [c],a
	dec  b
	jr   nz,.loop
	ret 

;******************************************************************************
;
;SetOneBGColor
;
;
;******************************************************************************


;******************************************************************************
;
;SetAllBGPalettes
;HL=pointer to palette data
;
;******************************************************************************
SetAllBGPalettes:: 
	ld   a,$80
	ldh  [REG_BCPS],a
	ld   bc,$4069
.loop:
	WaitVRAM 2
	ld   a,[hl+]
	ld   [c],a
	dec  b
	jr   nz,.loop
	ret 


;******************************************************************************
;
;SetAllOBJPalettes
;HL=pointer to palette data
;
;******************************************************************************
SetAllOBJPalettes::
	ld   a,$80
	ldh  [REG_OCPS],a    ;obp index
	ld   bc,$406B
.loop:
	WaitVRAM 2
	ld  a,[hl+]
	ld   [c],a
	dec  b
	jr   nz,.loop
	ret 


;******************************************************************************
;
;SetOneOBJPalette
;HL=pointer to palette data
;A=Palette to change [0-7]
;
;******************************************************************************
SetOneOBJPalette::
	add  a,$80
	ldh  [REG_OCPS],a 
	ld   bc,$086B		;B=#bytes in 1 palette, C=REG_OCPD 
.loop:
	WaitVRAM 2
	ld   a,[hl+]
	ld   [c],a
	dec  b
	jr   nz,.loop
	ret 


;******************************************************************************
;
;WaitForClearPad
;
;******************************************************************************
WaitForClearPad::
 call ReadJoypad
 ld   a,[g_nPadData]
 and  a
 jr   nz,WaitForClearPad
 ret 


;******************************************************************************
;
;ReadJoypad
;InitJoypad
;
;******************************************************************************

InitJoypad::
	ld   hl,g_anButtons
	xor  a
	ld   [hl+],a
	ld   [hl+],a
	ld   [hl+],a
	ld   [hl+],a
	ld   [hl+],a
	ld   [hl+],a
	ld   [hl+],a
	ld   [hl+],a
	ld   [g_nPadData],a
	ret 


ReadJoypad::
	ld   a,$20
	ldh [REG_P1],a    ;joypad   
	ldh a,[REG_P1]    ;joypad   
	ldh a,[REG_P1]    ;joypad   
	cpl 
	and  $0F
	swap a
	ld   b,a
	ld   a,$10
	ldh [REG_P1],a    ;joypad   
	ldh a,[REG_P1]    ;joypad   
	ldh a,[REG_P1]    ;joypad   
	ldh a,[REG_P1]    ;joypad   
	ldh a,[REG_P1]    ;joypad   
	ldh a,[REG_P1]    ;joypad   
	ldh a,[REG_P1]    ;joypad   
	cpl 
	and  $0F
	or   b
	ld   [g_nPadData],a
	ldh a,[$80]              
	bit  2,a
	jr   nz,.skip1
	ld   a,[g_nPadData]
	and  $0F
	cp   $0F
	ld   a,$11
	jp   z,$0150 ;All 4 buttons pressed: SoftReset
.skip1:
	ld   b,$01
	ld   hl,g_anButtons  ;[8byte array]
.loop1:
	ld   a,[g_nPadData]
	and  b
	jr   z,.skip3
	ld   a,[hl]
	cp   $00
	jr   z,.skip2
	ld   a,$01
	ld   [hl+],a
	jr   .skip4
.skip2:
	ld   a,$02
	ld   [hl+],a
	jr   .skip4
.skip3:
	ld   a,$00
	ld   [hl+],a
.skip4:
	rl   b
	jr   nc,.loop1
	ret 


;******************************************************************************
;
;SGBVramTransfer
;
;BC=Pointer to data to transfer
;DE=Pointer to SGB command string
;
;******************************************************************************
IF DEF(CPU_SGB)

SGBVramTransfer::
	di
	push de
	call TurnOffDisplay
	ld   a,$E4
	ldh  [REG_BGP],a
	ld   hl,BASE_VRAM8800
	ld	 de,$1000
	call CopyMemory
	ld   hl,BASE_MAPRAM9800
	ld   de,$000C
	ld   a,$80
	ld   c,$0D
.SGB_loop1:
	ld   b,$14
.SGB_loop2:
	ld   [hl+],a
	inc  a
	dec  b
	jr   nz,.SGB_loop2
	add  hl,de
	dec  c
	jr   nz,.SGB_loop1
	ld   a,$81
	ldh  [REG_LCDC],a
	call SGB_wait
	pop  hl
	call SGBDoTransfer
	call SGB_wait
	ei
	ret

SGB_wait:
	ld   de,$1C00
.SGB_wait_loop:
	nop
	nop
	nop
	dec  de
	ld   a,d
	or   e
	jr   nz,.SGB_wait_loop
	ret
	
SGBDoTransfer::
	ret  z
	ld   a,[hl]
	and  $07
	ret  z
	ld   b,a
	ld   c,$00
.SGBDo_loop1:
	push bc
	xor  a
	ld   [c],a
	ld   b,$10
.SGBDo_loop2:
	ld   e,$08
	ld   a,[hl+]
	ld   d,a
.SGBDo_loop3:
	bit  0,d
	ld   a,$10
	jr   nz,.SGBDo_skip
	ld   a,$20
.SGBDo_skip:
	ld   [c],a
	ld   a,$30
	ld   [c],a
	rr   d
	dec  e
	jr   nz,.SGBDo_loop3
	dec  b
	jr   nz,.SGBDo_loop2
	ld   a,$20
	ld   [c],a
	ld   a,$30
	ld   [c],a
	
	pop  bc
	dec  b
	ret  z
	call SGB_wait
	jr   .SGBDo_loop1


SGB_Commands::

SGB_Command_RQ_1Play::
	DB	$89,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
SGB_Command_RQ_2Play::
	DB	$89,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
SGB_Command_Mask_Off::
	DB	$B9,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
SGB_Command_FreezeSGB::
	DB	$B9,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
SGB_Command_Mask_Black::
	DB	$B9,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
SGB_Command_Mask_White::
	DB	$B9,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
SGB_Command_PCT_TRN::
	DB	$A1
SGB_Command_CHR_TRN1::
	DB  $99,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
SGB_Command_CHR_TRN2::
	DB  $99,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ENDC


