	IF	!DEF(UTILS_INC)
UTILS_INC	SET	1
;****************************************************************************************************************************************************
;*
;*  Support and Basic Utilities
;*
;****************************************************************************************************************************************************

;****************************************************************************************************************************************************
;*	externally referenced symbols
;****************************************************************************************************************************************************

	GLOBAL  SetMap
	GLOBAL	TurnOffDisplay
	GLOBAL	ClearAllBGAttrMap
	GLOBAL	SetBGAttrMap
	GLOBAL	ClearAllBGCharMap
	GLOBAL	SetBGCharMap
	GLOBAL	CopyToVRAM
	GLOBAL	CopyMemory
	GLOBAL	SetAllBGPalettes
	GLOBAL	SetOneBGPalette
	GLOBAL	SetAllOBJPalettes
	GLOBAL	SetOneOBJPalette
	GLOBAL	InitJoypad
	GLOBAL	ReadJoypad
	GLOBAL	WaitForClearPad
	GLOBAL	PrintText
	GLOBAL	Divide16

	GLOBAL	InitializeGB

	ENDC	; UTILS_INC
