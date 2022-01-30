	DIGIT_PREC = 1000
	WORD_PREC = DIGIT_PREC * 104 / 1000 + 2 ; 106
	COMP_COUNT = (32 * WORD_PREC + 11) / 10 ; (32*106+11)/10 = (3392+11)/10 = 3403/10 = 340

	* = $8000		; $8000 is the bottom of our ROM

	COMPCNT = $0200		; uint32

	PI_PART = $1000		; Start of a series of 32-bit values, WORD_PREC long
				; Lower-order byte of this must be zero for our clear loop to work

	P2 	= $80		; int32
	FIRSTNZ = $84		; int32
	DIG	= $88		; uint32
	ZERODIG = $8C		; Allows us to treat dig as 64-bit when convenient, set to zeroes
	DIG0 	= $90		; uint32
	ZERODIG0 = $94		; Allows us to treat dig0 as 64-bit when convenient, set to zeroes
	REMAINDERS = $98	; uint32[7], 7*4=28=$1C long
	DIV	= $B4		; uint32[7], 7*4=28=$1C long
	DIVIDEND = $D0		; uint64
	RESSUM	= $D8		; uint64
	SUMOVL	= $E0		; int32
	ZEROSUMOVL = $E4	; Allows us to treat sumOvl as 64-bit when convenient
	PITEMP0	= $E8		; uint32
	PITEMP1	= $EC		; uint32
	PITEMP2	= $F0		; uint64
	PITEMP3	= $F8		; uint64

#include "call.s"		; Gives us A/B/R definitions

reset:
	ldx #$ff		; Point at top of stack
	txs

	jsr lcd_init


	lda #0
	sta COMPCNT
	sta COMPCNT+1
	sta COMPCNT+2
	sta COMPCNT+3
	sta ZERODIG
	sta ZERODIG+1
	sta ZERODIG+2
	sta ZERODIG+3
	sta ZERODIG0
	sta ZERODIG0+1
	sta ZERODIG0+2
	sta ZERODIG0+3
	sta ZEROSUMOVL
	sta ZEROSUMOVL+1
	sta ZEROSUMOVL+2
	sta ZEROSUMOVL+3

	;; Zero PI_PART. This is almost certainly >256 bytes long, so we loop using both X and Y
	lda #0
	sta PITEMP0
	lda #(PI_PART >> 8)
	adc #((WORD_PREC * 4) >> 8)
	sta PITEMP0+1

	lda #0
	ldx #((WORD_PREC * 4) >> 8)
	ldy #((WORD_PREC * 4) & $ff)
pi_partz:
	sta (PITEMP0),y
	dey
	bne pi_partz
	sta (PITEMP0),y		; Zero byte must be zeroed, but we can't use BPL, as so do bytes 128+
	dey
	dec PITEMP0+1
	dex
	bpl pi_partz

calc_component:
	;; Multiply component counter by 10
	lda #(COMPCNT & $ff)
	sta A
	lda #(COMPCNT >> 8)
	sta A_
	lda #(const_10 & $ff)
	sta B
	lda #(const_10 >> 8)
	sta B_
	lda #(P2)
	sta R
	lda #0
	sta R_
	ldy #4
	jsr mul

	;; Add 29
	lda #(P2)
	sta A
	lda #0
	sta A_
	lda #(const_29 & $ff)
	sta B
	lda #(const_29 >> 8)
	sta B_
	;; Don't fill R - it still points at P2, which is where we want the result
	jsr add

	;; Copy P2 to FIRSTNZ.
	lda #(P2)
	sta A
	lda #0
	sta A_
	lda #(FIRSTNZ)
	sta R
	jsr copy

	;; FIRSTNZ >>= 5
	lda #(FIRSTNZ)
	sta A
	jsr rsh
	jsr rsh
	jsr rsh
	jsr rsh
	jsr rsh

	;; dig0 = 0x80000000U >> (p2 & 0x1f);
	lda P2
	and #$1f
	beq dig0_shifted
	tax
	lda #0
	sta DIG0
	sta DIG0+1
	sta DIG0+2
	lda #$80
	sta DIG0+3
	lda #(DIG0)
	sta A
	sta R
dig0_shift_l:
	jsr rsh
	dex
	bne dig0_shift_l
dig0_shifted:

	;; div[0] = 10 * i + 1;
	lda #(const_10 & $ff)
	sta A
	lda #(const_10 >> 8)
	sta A_
	lda #(COMPCNT & $ff)
	sta B
	lda #(COMPCNT >> 8)
	sta B_
	lda #(DIV + 0 * 4)
	sta R
	jsr mul

	lda #(DIV + 0 * 4)
	sta A
	lda #0
	sta A_
	lda #(const_1 & $ff)
	sta B
	lda #(const_1 >> 8)
	sta B_
	jsr add

	;; div[1] = 2560 * i + 2304;
	lda #(const_2560 & $ff)
	sta A
	lda #(const_2560 >> 8)
	sta A_
	lda #(COMPCNT & $ff)
	sta B
	lda #(COMPCNT >> 8)
	sta B_
	lda #(DIV + 1 * 4)
	sta R
	jsr mul

	lda #(DIV + 1 * 4)
	sta A
	lda #0
	sta A_
	lda #(const_2304 & $ff)
	sta B
	lda #(const_2304 >> 8)
	sta B_
	jsr add

	;; div[2] = 32 * i + 8;
	lda #(const_32 & $ff)
	sta A
	lda #(const_32 >> 8)
	sta A_
	lda #(COMPCNT & $ff)
	sta B
	lda #(COMPCNT >> 8)
	sta B_
	lda #(DIV + 2 * 4)
	sta R
	jsr mul

	lda #(DIV + 2 * 4)
	sta A
	lda #0
	sta A_
	lda #(const_8 & $ff)
	sta B
	lda #(const_8 >> 8)
	sta B_
	jsr add

	;; div[3] = 1024 * i + 768;
	lda #(const_1024 & $ff)
	sta A
	lda #(const_1024 >> 8)
	sta A_
	lda #(COMPCNT & $ff)
	sta B
	lda #(COMPCNT >> 8)
	sta B_
	lda #(DIV + 3 * 4)
	sta R
	jsr mul

	lda #(DIV + 3 * 4)
	sta A
	lda #0
	sta A_
	lda #(const_768 & $ff)
	sta B
	lda #(const_768 >> 8)
	sta B_
	jsr add

	;; div[4] = 40 * i + 12;
	lda #(const_40 & $ff)
	sta A
	lda #(const_40 >> 8)
	sta A_
	lda #(COMPCNT & $ff)
	sta B
	lda #(COMPCNT >> 8)
	sta B_
	lda #(DIV + 4 * 4)
	sta R
	jsr mul

	lda #(DIV + 4 * 4)
	sta A
	lda #0
	sta A_
	lda #(const_12 & $ff)
	sta B
	lda #(const_12 >> 8)
	sta B_
	jsr add

	;; div[5] = 640 * i + 320;
	lda #(const_640 & $ff)
	sta A
	lda #(const_640 >> 8)
	sta A_
	lda #(COMPCNT & $ff)
	sta B
	lda #(COMPCNT >> 8)
	sta B_
	lda #(DIV + 5 * 4)
	sta R
	jsr mul

	lda #(DIV + 5 * 4)
	sta A
	lda #0
	sta A_
	lda #(const_320 & $ff)
	sta B
	lda #(const_320 >> 8)
	sta B_
	jsr add

	;; div[6] = 640 * i + 448;
	lda #(const_640 & $ff)
	sta A
	lda #(const_640 >> 8)
	sta A_
	lda #(COMPCNT & $ff)
	sta B
	lda #(COMPCNT >> 8)
	sta B_
	lda #(DIV + 6 * 4)
	sta R
	jsr mul

	lda #(DIV + 6 * 4)
	sta A
	lda #0
	sta A_
	lda #(const_448 & $ff)
	sta B
	lda #(const_448 >> 8)
	sta B_
	jsr add

	;; resSum = sum[firstnz];
	ldx FIRSTNZ		; Implies FIRSTNZ is <=$ff, should be fine for our purposes
	jsr calc_sum_addr	; Gets the right PI_PART address into PITEMP0
	lda PITEMP0
	sta A
	lda PITEMP0+1
	sta A_
	lda #(RESSUM & $ff)
	sta R
	ldy #4
	jsr copy
	lda #0
	sta RESSUM+4
	sta RESSUM+5
	sta RESSUM+6
	sta RESSUM+7

	;; for(j = 0; j < 7; ++j) {
	;; We'll store j in x
	ldx #0

divloop:
	;; if( (i&1) == (j<2) ) {  // dividend is negative
	lda COMPCNT
	and #1
	cpx #2			; Clear carry if <2, set otherwise
	rol			; Rotate carry into A
	;; A can now have one of four values
	;; 00 : bottom bit of COMPCNT is 0, x<2, if not satisfied
	;; 01 : bottom bit of COMPCNT is 0, x>=2, if satisfied
	;; 10 : bottom bit of COMPCNT is 1, x<2, if satisfied
	;; 11 : bottom bit of COMPCNT is 1, x>=2, if not satisfied
	;; So, if A is 1 or 2, dividend is negative, if 0 or 3, dividend is positive
	;; ROL sets the zero flag, so that's easy to check
	beq div_positive
	;; And this isn't _expensive_
	cmp #3
	beq div_positive
	;; OK, dividend is negative

	;; dividend = ((uint64_t)div[j] << 32) - dig0
	txa
	asl
	asl			; A now has the right index into DIV
	phx
	tax
	lda DIV,x
	sta DIVIDEND+4		; This is shifted left 32 bits, same below
	inx
	lda DIV,x
	sta DIVIDEND+5
	inx
	lda DIV,x
	sta DIVIDEND+6
	inx
	lda DIV,x
	sta DIVIDEND+7
	plx
	lda #0
	sta DIVIDEND
	sta DIVIDEND+1
	sta DIVIDEND+2
	sta DIVIDEND+3
	;; Dividend now contains div[j]<<32, just need to subtract dig0
	sta A_
	sta B_
	lda #DIVIDEND
	sta A
	sta R
	lda #DIG0		; DIG0 has padding letting us to treat it as 64-bit even though it's not
	sta B
	ldy #8
	jsr sub

	;; resSum -= 1LL << 32;
	lda #RESSUM
	sta A
	sta R
	lda #(const_1lsh32 & $ff)
	sta B
	lda #(const_1lsh32 >> 8)
	sta B_
	jsr sub
	bra div_setup_done

div_positive:
	;; dividend = dig0; (casting a uint32 up to a uint64)
	lda DIG0
	sta DIVIDEND
	lda DIG0+1
	sta DIVIDEND+1
	lda DIG0+2
	sta DIVIDEND+2
	lda DIG0+3
	sta DIVIDEND+3
	lda #0
	sta DIVIDEND+4
	sta DIVIDEND+5
	sta DIVIDEND+6
	sta DIVIDEND+7

div_setup_done:
	;; dig = dividend / div[j];
	;; We'll start by casting div[j] up to 64-bit in PITEMP2
	txa
	asl
	asl			; A now has the right index into DIV
	phx
	tax
	lda DIV,x
	sta PITEMP2
	inx
	lda DIV,x
	sta PITEMP2+1
	inx
	lda DIV,x
	sta PITEMP2+2
	inx
	lda DIV,x
	sta PITEMP2+3
	plx
	lda #0
	sta PITEMP2+4
	sta PITEMP2+5
	sta PITEMP2+6
	sta PITEMP2+7
	;; ...then divide
	sta A_
	sta B_
	lda #DIVIDEND
	sta A
	lda #PITEMP2
	sta B
	lda #DIG
	sta R
	ldy #8
	jsr div

	;; remainders[j] = dividend - dig * div[j];
	;; ...actually, div leaves the remainder in TEMP1, so let's just use that. If we just use
	;; TEMP1, the assembler complains because it's forward-defined, so we'll use our own copy.
	;; We should probably come up with something a bit more formal for this, an R2 in call.s or so.
	DIVREM = $10
	txa
	asl
	asl			; A now has the right index into REMAINDERS
	phx
	tax
	lda DIVREM
	sta REMAINDERS,x
	lda DIVREM+1
	sta REMAINDERS+1,x
	lda DIVREM+2
	sta REMAINDERS+2,x
	lda DIVREM+3
	sta REMAINDERS+3,x
	plx

	;; resSum += dig;
	lda #RESSUM
	sta A
	sta R
	lda #DIG
	sta B
	jsr add

	jsr pause

	inx
	cpx #7
	beq divloopdone
	jmp divloop
divloopdone:

	;; sum[firstnz] = resSum;
	;; A already points at RESSUM from the last add in the loop
	;; The correct sum address is still in PITEMP0 from before the loop
	;; NB: resSum is uint64, sum[] is uint32, so this is copying the bottom 4 bytes
	lda PITEMP0
	sta R
	lda PITEMP0+1
	sta R_
	ldy #4
	jsr copy

	;; sumOvl = (int32_t)(resSum>>32);
	;; The other four bytes
	lda A			;This is quicker than four incs directly on A
	adc #4
	sta A
	lda #SUMOVL
	sta R
	lda #0
	sta R_
	jsr copy
	jsr pause

	;; for(loc = firstnz - 1; sumOvl && loc >= 0; --loc ) {
	jsr ovlloop		; Implements that whole loop

	;; while(++firstnz < gWordPrec) {
fnzloop:
	inc FIRSTNZ
	lda FIRSTNZ
	cmp #WORD_PREC
	bcc fnzloopcont
	jmp fnzloopdone		; too far away for a bcs

fnzloopcont:
	;; resSum = sum[firstnz];
	ldx FIRSTNZ
	jsr calc_sum_addr
	;; cast/copy from *PITEMP0 to RESSUM
	ldy #0
	lda (PITEMP0),y
	sta RESSUM
	lda (PITEMP0+1),y
	sta RESSUM+1
	lda (PITEMP0+2),y
	sta RESSUM+2
	lda (PITEMP0+3),y
	sta RESSUM+3
	sty RESSUM+4
	sty RESSUM+5
	sty RESSUM+6
	sty RESSUM+7

	;; for(j = 0; j < 7; ++j) {
	ldx #0
fnzdivloop:

	;; dividend = (uint64_t)remainders[j] << 32;
	phx
	txa
	asl
	asl
	tax
	lda REMAINDERS,x
	sta DIVIDEND+4
	lda REMAINDERS+1,x
	sta DIVIDEND+5
	lda REMAINDERS+2,x
	sta DIVIDEND+6
	lda REMAINDERS+3,x
	sta DIVIDEND+7
	lda #0
	sta DIVIDEND
	sta DIVIDEND+1
	sta DIVIDEND+2
	sta DIVIDEND+3
	stx PITEMP1+1		; Store remainder offset for later

	;; dig = dividend / div[j];
	;; First, cast div[j] up to 64-bit in PITEMP2
	sta PITEMP2+4
	sta PITEMP2+5
	sta PITEMP2+6
	sta PITEMP2+7
	lda DIV,x
	sta PITEMP2
	lda DIV+1,x
	sta PITEMP2+1
	lda DIV+2,x
	sta PITEMP2+2
	lda DIV+3,x
	sta PITEMP2+3
	stx PITEMP1		; Store div offset for later
	plx

	lda #DIVIDEND
	sta A
	lda #DIV
	adc PITEMP1		; div offset from above
	sta B
	lda #DIG
	sta R
	ldy #8
	jsr div

	;; remainders[j] = dividend - dig * div[j];
	;; Search for TEMP1 above to see what's going on here
	phx
	ldx PITEMP1+1		; remainder offset from above
	lda DIVREM
	sta REMAINDERS,x
	lda DIVREM+1
	sta REMAINDERS+1,x
	lda DIVREM+2
	sta REMAINDERS+2,x
	lda DIVREM+3
	sta REMAINDERS+3,x
	plx
	jsr pause

	;; resSum += dig;
	lda #RESSUM
	sta A
	sta R
	lda #DIG
	sta B
	jsr add			; This'll be an 8-byte add. That's fine, DIG is zero-padded.
	jsr pause

	inx
	cpx #7
	bne fnzdivloop

	;; sum[firstnz] = resSum;
	;; A already points at RESSUM
	;; PITEMP0 still has the right address from before the loop
	lda PITEMP0
	sta R
	lda PITEMP0+1
	sta R_
	;; This is a shortening assignment copying the bottom 32 bits
	ldy #4
	jsr copy

	;; sumOvl = (int32_t)(resSum>>32);
	lda A
	adc #4
	sta A
	lda #SUMOVL
	sta R
	lda #0
	sta R_
	jsr copy

	;; for(loc = firstnz - 1; sumOvl && loc >= 0; --loc ) {
	jsr ovlloop		; Implements that whole loop

	jmp fnzloop
fnzloopdone:
	;; ...


end:
	nop
	bra end



ovlloop:
	;; This loop is repeated twice in the original code.
	;; This routine requires that A_ and B_ are zero before calling
	;; It messes with A, X and Y, they're not saved.

	;; for(loc = firstnz - 1; sumOvl && loc >= 0; --loc ) {
	ldx FIRSTNZ
ovll:
	dex			; Both the initial -1 and the loop's --loc
	beq ovlloopdone
	lda SUMOVL
	ora SUMOVL+1
	ora SUMOVL+2
	ora SUMOVL+3
	beq ovlloopdone

	;; resSum = (int64_t)sum[loc] + sumOvl;
	jsr calc_sum_addr	; loc is already in X. Get sum[loc] addr into PITEMP0.
	;; cast/copy from *PITEMP0 to PITEMP2 (which is 64-bit)
	ldy #0
	lda (PITEMP0),y
	sta PITEMP2
	lda (PITEMP0+1),y
	sta PITEMP2+1
	lda (PITEMP0+2),y
	sta PITEMP2+2
	lda (PITEMP0+3),y
	sta PITEMP2+3
	sty PITEMP2+4
	sty PITEMP2+5
	sty PITEMP2+6
	sty PITEMP2+7
	lda #PITEMP2
	sta A
	;; A_ is still zero from above
	lda #SUMOVL
	sta B
	;; B_ is still zero from above
	;; The below uses a dirty trick. The math in lines below wants to add sum[loc] to sumOvl
	;; and for the bottom 32 bits to go back in sum[loc], but the top 32 bits to go into sumOvl
	;; (sumOvl effectively acting as carry bits).  By pointing halfway through RESSUM, we'll put
	;; the bottom 32 bits into the top half of ressum and the top 32 bits into sumovl, saving us
	;; a bunch of copying
	lda #(RESSUM + 4)
	sta R
	jsr add			;TODO: Not sure this does the right thing if sumOvl is negative

	;; sum[loc] = resSum;
	;; Top 32 bits of resSum into sum[loc]
	lda RESSUM+4
	sta (PITEMP0),y
	lda RESSUM+5
	sta (PITEMP0+1),y
	lda RESSUM+6
	sta (PITEMP0+2),y
	lda RESSUM+7
	sta (PITEMP0+3),y

	;; sumOvl = (int32_t)(resSum>>32);
	;; Nothing to do, see longer comment above.

	bra ovll
ovlloopdone:
	rts


pause:
	;; Spend a while just doing nops, enabling us to run the emulator fast, but stop at certain
	;; points with some degree of reliability.
	phy
	ldy #0
pausel:
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	iny
	bne pausel
	ply
	rts


calc_sum_addr:
	;; Calculates the address for sum[x] aka PI_PART+4*x and leaves it in PITEMP0. Note that this
	;; implies that PI_PART has a maximum length of 256 uint32s (but it'd probably be easy to switch
	;; if that proves useful).
	pha
	phx
	phy

	;; Shift X left by two bits, taking any bits that come out of that into Y
	ldy #0
	txa
	asl
	bcc calc_sum_noc1
	ldy #2
calc_sum_noc1:
	asl
	bcc calc_sum_noc2
	iny
calc_sum_noc2:
	tax
	tya
	adc #(PI_PART >> 8)
	sta PITEMP0+1
	stx PITEMP0		; Why no add? Because PI_PART is guaranteed to start at start-of-page,
				; so we'd just be adding zero. (If this weren't the case though, we'd
				; also need to check for carries and increment Y.)

	ply
	plx
	pla
	rts


#include "lcd.s"
#include "math.s"

const_1:
	.byte $01
	.byte $00
	.byte $00
	.byte $00

const_8:
	.byte $08
	.byte $00
	.byte $00
	.byte $00

const_10:
	.byte $0a
	.byte $00
	.byte $00
	.byte $00

const_12:
	.byte $0c
	.byte $00
	.byte $00
	.byte $00

const_29:
	.byte $1d
	.byte $00
	.byte $00
	.byte $00

const_32:
	.byte $20
	.byte $00
	.byte $00
	.byte $00

const_40:
	.byte $28
	.byte $00
	.byte $00
	.byte $00

const_320:
	.byte $40
	.byte $01
	.byte $00
	.byte $00

const_448:
	.byte $c0
	.byte $01
	.byte $00
	.byte $00

const_640:
	.byte $80
	.byte $02
	.byte $00
	.byte $00

const_768:
	.byte $00
	.byte $03
	.byte $00
	.byte $00

const_1024:
	.byte $00
	.byte $04
	.byte $00
	.byte $00

const_2304:
	.byte $00
	.byte $09
	.byte $00
	.byte $00

const_2560:
	.byte $00
	.byte $0a
	.byte $00
	.byte $00

const_1lsh32:			;1 << 32
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $01
	.byte $00
	.byte $00
	.byte $00

; The section below will pad out the file to 32k and also insert the interrupt and reset vectors.
; The reset vector will point at address $8000, which is where the ROM is mapped to for the 6502
; (and what we set as start address above).
codeend:
* = $fffa
.dsb (*-codeend), $ea    ; Fill everything from end of code to vectors with NOPs
* = $fffa

.word $0000  ; NMI Vector
.word reset  ; Reset Vector
.word $0000  ; BRK/IRQB Vector
