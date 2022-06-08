;***********************************************************************************************************************
;*
;*   Program	: Blank GBC User Variables and Flags
;*
;*   Date		: Jan 01, 1980
;*
;*   Function	: Support file that declares the user (e.g. global) variables.
;*
;*
;***********************************************************************************************************************

	INCLUDE "vars.inc"



;***********************************************************************************************************************
;*	BSS section variables
;*	Data "types":
;*      g_n : general data
;*      g_p : general pointer
;*      g_a : general array
;*      
;***********************************************************************************************************************

	SECTION	"BSSVars",BSS
g_nSystemID					DS	1	; Hardware ID byte (contents of A register on startup get loaded here by the Initialization routine)
g_nSourceMapWidth			DS	1	; Variable for BG map copy routines
g_anButtons					DS	8	; array to hold button activity
g_nPadData					DS	1	; raw button status bits



;***********************************************************************************************************************
;*	variables located in high memory
;***********************************************************************************************************************

	SECTION	"HRAMVars",HRAM
g_nUserFlags				DS	1	; various user/state flags (see UserFlags.inc for list)
g_nFrameCounter				DS	1	; frame counter
g_nLastROMBank				DS	1	; last ROM bank indicator
g_nLastRAMBank				DS	1	; last RAM bank indicator
g_nTempVar					DS	1	; general purpose variable
g_nTopOffsetX				DS	1	; Top section X scroll offset
g_nTopOffsetY				DS	1	; Top section Y scroll offset
g_nTextOffsetX				DS	1	; Text Scroller X offset
g_nTextOffsetY				DS	1	; Text Scroller Y offset
g_nBottomOffsetX			DS	1	; Top section X scroll offset
g_nBottomOffsetY			DS	1	; Top section Y scroll offset
g_nLCDSection				DS	1	; Next LCD Section to perform
g_nSection0Line				DS	1	; Scanline where Section 0 Starts
g_nSection1Line				DS	1	; Scanline where Section 1 Starts
g_nSection2Line				DS	1	; Scanline where Section 2 Starts
g_nTopCountX				DS	1	; Top Seciton X Sinus Table counter
g_nTopCountY				DS	1	; Top Seciton Y Sinus Table counter
g_nTextCount				DS	1	; Text Section counter
g_nBottomCountX				DS	1	; Bottom Seciton X Sinus Table counter
g_nBottomCountY				DS	1	; Bottom Seciton Y Sinus Table counter
g_pMapPosition				DS	1	; Map offset for Text Scroller
g_pLowPointer				DS	1	; Low byte of pointer to text data
g_pHighPointer				DS	1	; High byte of pointer to text data
g_nScrollCounter			DS	1	; Scroll update frame counter


