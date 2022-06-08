	IF	!DEF(USERVARS_INC)
USERVARS_INC	SET	1

;***********************************************************************************************************************
;*  User EQUates
;***********************************************************************************************************************

SPRITEDMA_ADDR				EQU	-16	; $FFF0 - For InitializeGB use

USER_F_ISCGB				EQU	%00000001	; set when running on a CGB
USER_F_VBLANKEND			EQU	%00000010	; set when the vblank code is done
USER_F_DISABLEWARMREBOOT	EQU	%00000100	; set when the user isn't allowed to warm reboot the game
USER_F_UNUSED3				EQU	%00001000
USER_F_UNUSED4				EQU	%00010000
USER_F_UNUSED5				EQU	%00100000
USER_F_UNUSED6				EQU	%01000000
USER_F_UNUSED7				EQU	%10000000

; bit versions of the flags
USER_B_ISCGB				EQU	0
USER_B_VBLANKEND			EQU	1
USER_B_DISABLEWARMREBOOT	EQU	2
USER_B_UNUSED3				EQU	3
USER_B_UNUSED4				EQU	4
USER_B_UNUSED5				EQU	5
USER_B_UNUSED6				EQU	6
USER_B_UNUSED7				EQU	7


;****************************************************************************************************************************************************
;*	BSS section variables
;****************************************************************************************************************************************************

; For Example: GLOBAL g_nSystemID

	GLOBAL g_nSystemID
	GLOBAL g_nSourceMapWidth
	GLOBAL g_anButtons
	GLOBAL g_nPadData



;****************************************************************************************************************************************************
;*	High RAM ($FF80-$FFFD) variables
;****************************************************************************************************************************************************

; For Example: GLOBAL g_nTempVar

	GLOBAL g_nUserFlags
	GLOBAL g_nFrameCounter
	GLOBAL g_nLastROMBank
	GLOBAL g_nLastRAMBank
	GLOBAL g_nTempVar
	GLOBAL g_nTopOffsetX
	GLOBAL g_nTopOffsetY
	GLOBAL g_nTextOffsetX
	GLOBAL g_nTextOffsetY
	GLOBAL g_nBottomOffsetX
	GLOBAL g_nBottomOffsetY
	GLOBAL g_nLCDSection
	GLOBAL g_nSection0Line
	GLOBAL g_nSection1Line
	GLOBAL g_nSection2Line
	GLOBAL g_nTopCountX
	GLOBAL g_nTopCountY
	GLOBAL g_nTextCount
	GLOBAL g_nBottomCountX
	GLOBAL g_nBottomCountY
	GLOBAL g_pMapPosition
	GLOBAL g_pLowPointer
	GLOBAL g_pHighPointer
	GLOBAL g_nScrollCounter


; offsets for 'ldh a,[xxx]' and 'ldh [xxx],a'
G_NUSERFLAGS			EQU	$80	; DS 1
G_NFRAMECOUNTER			EQU $81 ; DS 1 
G_NLASTROMBANK			EQU $82 ; DS 1
G_NLASTRAMBANK			EQU $83 ; DS 1
G_NTEMPVAR				EQU $84 ; DS 1
G_NTOPOFFSETX			EQU $85 ; DS 1 Top section X scroll offset
G_NTOPOFFSETY			EQU $86 ; DS 1 Top section Y scroll offset
G_NTEXTOFFSETX			EQU $87 ; DS 1 Text section X scroll offset
G_NTEXTOFFSETY			EQU $88 ; DS 1 Text section Y scroll offset
G_NBOTTOMOFFSETX		EQU $89 ; DS 1 Top section X scroll offset
G_NBOTTOMOFFSETY		EQU $8A ; DS 1 Top section Y scroll offset
G_NLCDSECTION			EQU $8B ; DS 1 Next LCD Section to perform
G_NSECTION0LINE			EQU $8C ; DS 1 Section 0 scanline
G_NSECTION1LINE			EQU $8D ; DS 1 Section 1 scanline
G_NSECTION2LINE			EQU $8E ; DS 1 Section 2 scanline
G_NTOPCOUNTX			EQU $8F ; DS 1 Top Seciton X Sinus Table counter
G_NTOPCOUNTY			EQU $90 ; DS 1 Top Seciton Y Sinus Table counter
G_NTEXTCOUNT			EQU $91 ; DS 1 Text Section counter
G_NBOTTOMCOUNTX			EQU $92 ; DS 1 Bottom Seciton X Sinus Table counter
G_NBOTTOMCOUNTY			EQU $93 ; DS 1 Bottom Seciton Y Sinus Table counter
G_PMAPPOSITION			EQU $94 ; DS 1 Map offset for text scroller
G_PLOWPOINTER			EQU $95 ; DS 1
G_PHIGHPOINTER			EQU $96 ; DS 1 
G_NSCROLLCOUNTER		EQU $97 ; DS 1


ENDC	; VARS_INC
