;23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
;***********************************************************************************************************************
;*
;*   Program	: PDROMS.COM Re-Opening Intro
;*
;*   Date		: June 20th, 2002
;*
;*   Function	: Main program rountines
;*
;***********************************************************************************************************************


;***********************************************************************************************************************
;*  Target System Definition
;*
;*  Uncomment one of these lines to set the target machine spec.
;*  For Mono-GB-only programs, use SYS_DMG only.
;*  For SGB-Compatible DMG or GBC games, uncomment SYS_SGB WITH either SYS_DMG or SYS_GBC.
;*  For backwards-compatible DMG/GBC games (Nintendo black cartridges) uncomment SYS_GBC only.
;*  For GBC-only games (SGB not possible), uncomment only SYS_GBC_ONLY
;*
;*
;***********************************************************************************************************************

; SYS_DMG SET 1		; 
; SYS_SGB SET 1		; 
; SYS_GBC SET 1		;
SYS_GBC_ONLY SET 1	; For our uses, we will be operating on the GBC.


;***********************************************************************************************************************
;*	includes
;***********************************************************************************************************************
	INCLUDE	"GBMacros.inc"
	INCLUDE	"Hardware.inc"

	INCLUDE	"main.inc"
	INCLUDE	"utils.inc"
	INCLUDE	"vars.inc"

;***********************************************************************************************************************
;*	equates
;***********************************************************************************************************************


;***********************************************************************************************************************
;*	cartridge header
;***********************************************************************************************************************

	SECTION	"Org $00",HOME[$00]
	; $0000 - Restart $00 address
RST_00:	jp	$100

	SECTION	"Org $08",HOME[$08]
	; $0008 - Restart $08 address
RST_08:	jp	$100

	SECTION	"Org $10",HOME[$10]
	; $0010 - Restart $10 address
RST_10:	jp	$100

	SECTION	"Org $18",HOME[$18]
	; $0018 - Restart $18 address
RST_18:	jp	$100

	SECTION	"Org $20",HOME[$20]
	; $0020 - Restart $20 address
RST_20:	jp	$100

	SECTION	"Org $28",HOME[$28]
	; $0028 - Restart $28 address
RST_28:	jp	$100

	SECTION	"Org $30",HOME[$30]
	; $0030 - Restart $30 address
RST_30:	jp	$100

	SECTION	"Org $38",HOME[$38]
	; $0038 - Restart $38 address
RST_38:	jp	$100

	SECTION	"Org $40",HOME[$40]
	; $0040 - V-Blank interrupt start address
	jp	IRQ_VBlank

	SECTION	"Org $48",HOME[$48]
	; $0048 - LCDC Status interrupt start address
	jp  IRQ_LCDC

	SECTION	"Org $50",HOME[$50]
	; $0050 - Timer Overflow interrupt start address
	reti

	SECTION	"Org $58",HOME[$58]
	; $0058 - Serial Transfer Completion interrupt start address
	reti

	SECTION	"Org $60",HOME[$60]
	; $0060 - Joypad interrupt start address
	reti

	SECTION	"Extra Space",HOME[$61]
	DB  "Written by DuoDreamer (rownik@hotmail.com)",$00
	SECTION	"Start",HOME[$100]
	; $0100-$0103 (Game code start)
ColdReboot::
	nop
	jp	Start

	; $0104-$0133 (Nintendo logo - do _not_ modify the logo data here or the CGB will freeze)
    NINTENDO_LOGO
	; $0134-$013E (Game title - up to 11 upper case ASCII characters; pad with $00)
	DB	"PDROMS.COM "
		;0123456789A

	; $013F-$0142 (Product code - 4 ASCII characters)
	DB	"    "
		;0123

	; $0143 (Color GameBoy compatibility code)
	DB	$C0	; $C0 - Dedicated Color GameBoy cartridge

	; $0144 (High-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0145 (Low-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0146 (GameBoy/Super GameBoy indicator)
	DB	$00	; $00 - GameBoy

	; $0147 (Cartridge type - all Color GameBoy cartridges are at least $19)
	DB	$19	; $19 - ROM + MBC5

	; $0148 (ROM size)
	DB	$00	; $00 - 256Kbit = 32Kbyte = 2 banks

	; $0149 (RAM size)
	DB	$00	; $00 - None

	; $014A (Destination code)
	DB	$01	; $01 - All others

	; $014B (Licensee code - this _must_ be $33)
	DB	$33	; $33 - Check $0144/$0145 for Licensee code.

	; $014C (Mask ROM version - handled by a post-linking tool)
	DB	$00

	; $014D (Complement check - handled by a post-linking tool)
	DB	$00 

	; $014E-$014F (Cartridge checksum - handled by a post-linking tool)
	DW	$00


;***********************************************************************************************************************
;* Program Start
;***********************************************************************************************************************

WarmReboot::
Start::
	ld	sp,$CF80	; set up a stack
	call InitializeGB	;
InitReturn::			; <-- this line must be here, the initialization routine clears all memory including the
						; stack, and needs this label to know where to jump back to when done with the INIT.
						

;***********************************************************************************************************************
	di
	call TurnOffDisplay
	
	ld      hl,MusicPlayer       ; this routine will upload the $0421
	ld      de,$d000        ; large musicplayer into the WRAM
	ld      bc,$0421        ; from $d000
.uploadloop:
	ld      a,[hl+]         ; note: you can use several WRAM
	ld      [de],a          ; banks on GBC in this area, so
	inc     de              ; theorically this will not waste
	dec     bc              ; memory, but always switch on the
	ld      a,b             ; player's WRAM bank before calling
	or      c               ; player functions
	jr      nz,.uploadloop
	
	ld      hl,Music        ; load music address to hl
	xor     a               ; load beginning pattern pos to a (0)
	ld      b,a             ; load restart pattern pos to b (0)
	call    $d000           ; initialize the music

	ld hl, BASE_VRAM8000
	ld bc, BGTiles
	ld de, (16*95)
	call CopyToVRAM
	ld hl, BASE_VRAM8800
	ld bc, TextTiles
	ld de, (32*45)
	call CopyToVRAM

SetupBGDesign::

	ld hl,Palette0
	call SetAllBGPalettes

	SetVideoBank 1
	ld bc,BGAtt1
	ld hl,BASE_MAPRAM9800
	ld de,(32*30)
	call CopyToVRAM	
	
	SetVideoBank 0
	ld  a,1
	ld	hl,BASE_MAPRAM9800		;Fill mem at 9800
	ld  bc,$02C0
.Set_Loop:						;     /
	WaitVRAMPushAF 2
	ldi	[hl],a					;    /
	dec	c						;   /
	jr	nz,.Set_Loop			;  /
	dec	b						; /
	jr	nz,.Set_Loop			;/

	xor a
	ld  bc,(16*32)
.Set_Loop2:						;     /
	WaitVRAMPushAF 2
	ldi	[hl],a					;    /
	dec	c						;   /
	jr	nz,.Set_Loop2			;  /
	dec	b						; /
	jr	nz,.Set_Loop2			;/


	SetVideoBank 1
	ld  a,2
	ld  b,2*32
.Set_Loop3:						;     /
	WaitVRAMPushAF 2
	ldi	[hl],a					;    /
	dec	b						; /
	jr	nz,.Set_Loop3			;/
	
	


	SetVideoBank 1
	ld  a,17
	ld  [g_nSourceMapWidth],a
	ld  hl,KojoteAtt
	ld  de,$98C8
	ld  bc,$1103
	call SetMap

	SetVideoBank 0
	ld  a,17
	ld  [g_nSourceMapWidth],a
	ld  hl,KojoteMap
	ld  de,$98C8
	ld  bc,$1103
	call SetMap

	SetVideoBank 1
	ld  a,14
	ld  [g_nSourceMapWidth],a
	ld  hl,PDROMSAtt
	ld  de,$9A8A
	ld  bc,$0E03
	call SetMap

	SetVideoBank 0
	ld  a,14
	ld  [g_nSourceMapWidth],a
	ld  hl,PDROMSMap
	ld  de,$9A8A
	ld  bc,$0E03
	call SetMap

	xor a
	ldh [G_NLCDSECTION],a
	ldh [REG_LYC],a
	ldh [REG_SCX],a
	ldh [REG_SCY],a
	ldh [G_NTOPOFFSETX],a
	ldh [G_NTEXTOFFSETX],a
	ldh [G_NBOTTOMOFFSETX],a
	ldh [G_NSCROLLCOUNTER],a
	ldh [G_NTEXTCOUNT],a

	ld  a,22
	ldh [G_PMAPPOSITION],a
	
	ld  hl,ScrollerText
	ld  a,l
	ldh [G_PLOWPOINTER],a
	ld  a,h
	ldh [G_PHIGHPOINTER],a

	ld  a,24
	ldh [G_NTOPOFFSETY],a
	ld  a,176
	ldh [G_NTEXTOFFSETY],a
	ld  a,64
	ldh [G_NBOTTOMOFFSETY],a

	ld  a,0
	ldh [G_NSECTION0LINE],a
	ld  a,64
	ldh [G_NSECTION1LINE],a
	ld  a,80
	ldh [G_NSECTION2LINE],a
	
	ld  a, REG_STAT_F_LYC
	ldh [REG_STAT],a
	
	ld  a, REG_IE_F_VBLANK | REG_IE_F_LCDC	
	ldh [REG_IE],a
	
	ld  a, REG_LCDC_F_ON | REG_LCDC_F_BG8000 | REG_LCDC_F_WIN9800 | REG_LCDC_F_BGON
	ldh [REG_LCDC],a
	

	ei
.mainloop:
	jp	.mainloop





;***********************************************************************************************************************
;*
;*  IRQ_VBlank -- VBlank interrupt routine
;*
;*  When the scanline reaches line 144, the GB is in VBlank mode and this routine is called.
;*
;*  INPUTS:
;*  None
;*
;*  RETURNS:
;*  None
;*
;***********************************************************************************************************************
IRQ_VBlank::

	PushAll
	
	call $D006
	
	
	ldh a,[G_NFRAMECOUNTER]
	inc a
	ldh [G_NFRAMECOUNTER],a
	bit 0, a
	jp  z, EvenFrameCalculations

OddFrameCalculations::
	ldh a,[G_NTOPOFFSETX]
	inc a
	ldh [G_NTOPOFFSETX],a

UpdateBottomSine::
	ld	hl,BottomSinX
	xor a
	ld  d,a
	ldh a,[G_NBOTTOMCOUNTX]
	inc a
	cp  64
	jr  nz,.skip
	xor a
.skip:
	ldh [G_NBOTTOMCOUNTX],a
	ld  e,a
	add hl,de
	ld  a,[hl]
	ldh [G_NBOTTOMOFFSETX],a

	ld	hl,BottomSinY
	xor a
	ld  d,a
	ldh a,[G_NBOTTOMCOUNTY]
	inc a
	cp  39
	jr  nz,.skip2
	xor a
.skip2:
	ldh [G_NBOTTOMCOUNTY],a
	ld  e,a
	add hl,de
	ld  a,[hl]
	ldh [G_NBOTTOMOFFSETY],a

	ld	hl,TopSinY
	xor a
	ld  d,a
	ldh a,[G_NTOPCOUNTY]
	inc a
	cp  32
	jr  nz,.skip3
	xor a
.skip3:
	ldh [G_NTOPCOUNTY],a
	ld  e,a
	add hl,de
	ld  a,[hl]
	ldh [G_NTOPOFFSETY],a
End_OddFrameCalculations::
	PopAll
	reti


EvenFrameCalculations::
	ldh a,[G_NTEXTOFFSETX]
	inc a
	inc a
	ldh [G_NTEXTOFFSETX],a
	and $07	
	jr  z,.updatescroll
	PopAll
	reti
	
.updatescroll:
; If 8 pixels has passed, then perform this stuff 

	ldh a,[G_PLOWPOINTER]
	ld  l,a
	ldh a,[G_PHIGHPOINTER]
	ld  h,a
	ld  a,[hl]
	ld  b,64			;
	sub b
	sla a				;calculate (x*2)+64 tile number
	ld  b,128
	add b				;
	push af
	inc hl
.test_addr:  ;DE <= HL?
	ld  de,TextEndLength
	ld  a,d
	cp  h
	jr  nz,.is_less_than
	ld  a,e
	cp  l
.is_less_than:
	jr  nc,.not_true
	ld  hl,ScrollerText
.not_true:
	ld  a,l
	ldh [G_PLOWPOINTER],a
	ld  a,h
	ldh [G_PHIGHPOINTER],a


	ldh a,[G_PMAPPOSITION]		; Get screen map position to update.
	inc a						; increment it
	cp  32						; compare it to 32
	jr  nz,.skip_reset			; if = 32 then tileposition = 0
	xor a						;
.skip_reset:					;
	ldh [G_PMAPPOSITION],a     ;
	
	ld  hl,$9BC0				; add map position to base map position
	ld  de,$0000				;
	ld  e,a						;
	add hl,de					;
	
	pop af						; retrieve character from stack
	ld  [hl],a					; load it to map position
	ld  de,$0020				;
	add hl,de					; and then load character + 1 to
	inc a						; map position + 1 line ($20 positions higher)
	ld  [hl],a					;
	
;	ld  b,8
;	ldh a,[G_NTEXTCOUNT]
;	add b
;	ldh [G_NTEXTCOUNT],a
	
; end if

End_EvenFrameCalculations::
	PopAll
	reti

;***********************************************************************************************************************
;*
;*  IRQ_LCDC -- LCDC interrupt routine
;*
;*  This routine is used to manage each scrolling section of the screen
;*
;*  INPUTS:
;*  None
;*
;*  RETURNS:
;*  None
;*
;***********************************************************************************************************************
IRQ_LCDC::

	push af
	ldh a,[G_NLCDSECTION]
	cp  0
	jr  z,.Section_0
	dec a
	jr  z,.Section_1
	jr  .Section_2

.Section_0:					;X Sinus  , Y Sinus
	ldh a,[G_NTOPOFFSETY]
	ldh [REG_SCY],a
	ldh a,[G_NTOPOFFSETX]
	ldh [REG_SCX],a
	ldh a,[G_NSECTION1LINE]
	ldh [REG_LYC],a
	ld  a,1
	ldh [G_NLCDSECTION],a
	pop af
	reti
	
.Section_1:
	ldh a,[G_NTEXTOFFSETY]
	ldh [REG_SCY],a
	ldh a,[G_NTEXTOFFSETX]
	ldh [REG_SCX],a
	ldh a,[G_NSECTION2LINE]
	ldh [REG_LYC],a
	ld  a,2
	ldh [G_NLCDSECTION],a
	pop af
	reti

.Section_2:						;X Sinus 32 to 64, Y Sinus 
	ldh a,[G_NBOTTOMOFFSETY]
	ldh [REG_SCY],a
	ldh a,[G_NBOTTOMOFFSETX]
	ldh [REG_SCX],a
	ldh a,[G_NSECTION0LINE]
	ldh [REG_LYC],a
	ld  a,0
	ldh [G_NLCDSECTION],a

	pop af
	reti


;***********************************************************************************************************************
;*	Support Routines
;*  
;*  Place all of your little generic support subroutines here
;*  
;***********************************************************************************************************************

ScrollerText::
;@=' '
;[='.'
;\='!'
;]='-'
;^='#'
;_='@'
;`='*'
;a-j='0-9'
	DB "@@@@@@@@@@WELCOME@TO@THE@PDROMS[COM@GRAND@RE]OPENING@INTRO"
	DB $5C,"@@@"
	DB "ON@JUNE@ca@caac@KOJOTE@RE]OPENED@HIS@PUBLIC@DOMAIN@FREE@ROM@"
	DB "WEBSITE[@@@THIS@LITTLE@INTRO@WAS@CODED@AS@A@GIFT@FOR@KOJOTE@"
	DB "BY@DUO@A@FEW@DAYS@BEFORE@THE@NEW@SITE@OPENED[@@@@CODE@BY"
	DB "@DUO@@@GRAPHICS@BY@KOJOTE[@@@MUSIC@AND@PLAYER@CODE@BY"
	DB "@BLACKBOX[@HOPE@YOU@DONT@MIND",$5C, "@@@@@@"
	DB "KOJOTE@AND@DUO@WOULD@LIKE@TO@GREET[[[@@@AMINA@@@LAI@@@ONEMANBAND@@@"
	DB "PANCID@@@PEITSCHI@@@OTAKU@@@LORDGOAT@@@WISEASS@@@VODKA@@@"
	DB "REMII@@@JOAT@@@AELIUS@@@SIMONB@@@SLASHERX@@@SUBICE@@@MADMAN@@@"
	DB "NEIMOD@@@JARVIKh@@@JEFF@F@@@ANGEL]LEIGH@@@PINK@@@NIGHTMARE@@@"
	DB "CITADEL@@@TBSP@@@EXOTICORN@@@WHATZDAT@@@GUYFAWKES@@@DOX@@@FIREFLY"
	DB "@@@GOLLUM@@@PHaX@@@EMUMANIAC@@@SOMNIUM@@@OPER_TOR@@@SPACE@COMMANDER"
	DB "@@@MKcK@@@SCIFISH@@@LYONHRT@@@FReNZ@@@NOKTURN@@@SJAAK@@@DIVER@@@ANDERER"
	DB "@@@KRAYZ@@@LAYZ@@@SULPH@@@NEh@@@TOMCAT@@@NAGZ@@@CHN@@@FRANKY@@@TMB@@@"
	DB "URLge@@@SACK@@@ARTHUS@@@MYTH@@@@@@@[[[AND@EVERYONE@IN@"
	DB "^GAMEBOY@AND@^PDROMS@ON@EFNET@AND@ALL@THE@PEOPLE@WE@FORGOT@TO@"
	DB "GREET[@@MAKE@SURE@TO@VISIT@PDROMS[COM@FOR@ALL@OF@YOUR@HOMEBREW@"
	DB "FREEWARE@ROM@NEEDS[@@AND@DONT@FORGET@TO@VISIT@DUO[GBADEV[ORG\@@@@@@@TEXT@REPEATS[[[@@@@@@@@@"

;guyfawkes, dox, amina, firefly, gollum, ph0x, emumaniac, somnium, oper@tor, space commander, mk2k
;scifish, lyonhrt, fr4nz, nokturn, sjaak, diver, anderer, krayz, layz, sulph, ne7, tomcat, nagz,
;chn, franky, tmb, url64, sack, arthus, myth
TextEndLength::


BottomSinX::
DB  56,  58,  60,  62,  65,  67,  69,  71
DB  72,  74,  75,  77,  78,  78,  79,  79
DB  79,  79,  79,  79,  78,  77,  76,  74
DB  73,  71,  69,  67,  65,  63,  60,  58
DB  56,  53,  51,  49,  47,  44,  42,  40
DB  39,  37,  36,  34,  33,  33,  32,  32
DB  32,  32,  32,  32,  33,  34,  35,  37
DB  38,  40,  42,  44,  46,  48,  50,  53

BottomSinY::
DB  64,  66,  69,  71,  73,  75,  77,  78
DB  79,  79,  79,  79,  78,  77,  76,  74
DB  72,  70,  67,  65,  62,  60,  57,  55
DB  53,  51,  50,  49,  48,  48,  48,  48
DB  49,  50,  52,  54,  56,  58,  61

TopSinY::
DB  12,  15,  18,  22,  25,  28,  31,  34
DB  37,  39,  41,  43,  45,  46,  47,  47
DB  47,  47,  47,  46,  45,  43,  42,  40
DB  37,  35,  32,  29,  26,  22,  19,  16

Palette0::
	RGBSet 0,0,0
	RGBSet 255,150,150
	RGBSet 255,200,200
	RGBSet 255,255,255
Palette1::
	RGBSet 32,32,32
	RGBSet 50,100,50
	RGBSet 90,164,90
	RGBSet 120,255,120
Palette2::
	RGBSet 0,0,0
	RGBSet 92,92,92
	RGBSet 148,148,148
	RGBSet 255,255,255
Palette3::
	RGBSet 0,0,0
	RGBSet 92,92,92
	RGBSet 148,148,148
	RGBSet 255,255,255
Palette4::
	RGBSet 0,0,0
	RGBSet 92,92,92
	RGBSet 148,148,148
	RGBSet 255,255,255
Palette5::
	RGBSet 0,0,0
	RGBSet 92,92,92
	RGBSet 148,148,148
	RGBSet 255,255,255
Palette6::
	RGBSet 0,0,0
	RGBSet 92,92,92
	RGBSet 148,148,148
	RGBSet 255,255,255
Palette7::
	RGBSet 0,0,0
	RGBSet 92,92,92
	RGBSet 148,148,148
	RGBSet 255,255,255
	
BGAtt1::
	REPT 7
	DB $00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20,$00,$20
	DB $40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$60
	ENDR
	REPT 16
	DB $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	ENDR

PDROMSMap::
	DB 00,00,02,03,04,05,06,07,08,00,09,10,11,12
	DB 13,14,15,16,17,18,19,20,21,22,23,24,25,26
	DB 27,28,29,29,30,31,32,33,34,28,35,36,37,29
PDROMSAtt::
	DB 01,01,01,01,01,01,01,01,01,01,01,01,01,01
	DB 01,01,01,01,01,01,01,01,01,01,01,01,01,01
	DB 01,01,01,01,01,01,01,01,01,01,01,01,01,01
KojoteMap::
	DB 44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60
	DB 61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77
	DB 78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94
KojoteAtt::
	DB 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00

BGTiles::
INCBIN "bgtiles.bin" ;16 bytes * 95 tiles

TextTiles::
INCBIN "texttiles.bin" ;32 bytes * 45 tiles

MusicPlayer::
INCBIN "player.bin"

SECTION "Music Data", DATA, BANK[1]
Music::
INCBIN "melodee.sav"

;***********************************************************************************************************************
;*	constants
;***********************************************************************************************************************

