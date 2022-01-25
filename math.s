; Calling convention for routines here
; $0000 is two-byte LE address of operand A. The address may be stomped, the destination won't.
; $0002 is two-byte LE address of operand B. The address may be stomped, the destination won't.
; $0004 is two-byte LE address of result. The address won't be stomped, result will be stored at dest.
; Y contains the number of bytes in the number and won't be stomped.
; $0006 through $002f are reserved for temp values and may be stomped

	TEMP0 = $08
	TEMP1 = $10
	TEMP2 = $18
	TEMP3 = $20
	
mul:
	;; Multiply A and B, each of which are little-endian numbers.
	pha
	phx
	phy

	jsr cpy_at0		; Move A to temp0 - we'll need to shift it in place
	;; Repoint A at temp0
	lda #TEMP0
	sta A
	lda #$00
	sta A_

	jsr cpy_bt1 		; Copy B to temp1.
	;; Repoint B at temp2 (NOT temp1) - we need it to point at something stompable
	;; as an intermediate operand 
	lda #TEMP2
	sta B
	lda #$00
	sta B_
	
	;; Zero B (because we'll be adding A to it)
	lda #0
	dey 			; If we're given e.g. length 8, we want to start at byte 7.
mulz:
	sta (B),y
	dey
	bpl mulz

	;; Save the real result address in TEMP3[0,1]. We don't need it until the end and will put our
	;; results in (our repointed) A or B. Zero the second byte (only) of R, since it won't change.
	ldy R
	sty TEMP3
	ldy R_
	sty TEMP3+1
	ldy #00
	sta R_

	ply			; Restore Y from stack
	phy

	;; As long as the top-end bytes are zero, reduce Y (this won't affect the _stack_ Y, which
	;; we use for shift/add calls, but will affect the _loop_ Y, which we use to know when we're
	;; done).
	dey 			; Look at last byte (e.g. 7 not 8 for 64-bit). We'll reincrement below.
mulrechecky:
	lda TEMP1,y
	bne mulyisgood
	dey
	bra mulrechecky
mulyisgood:
	iny			; Reincrement the decrement above
	lda #1			; Start at bit 0
	ldx #0			; Start at byte 0 (lower-order because LE)

mull:
	bit TEMP1,x
	sty TEMP3+2	       ; Dump loop Y in TEMP3[2] - we need Y for calls (this affects no flags)
	beq mulnext	       ; Bit not set, no add, move to next bit

	;; Point R at TEMP2 (where B points), get number-of-bytes Y back for the call.	
	ldy #TEMP2
	sty R
	ply
	phy
	jsr add			; Bit is set, add A to intermediate (in B), store in B

mulnext:
	;; Point R at TEMP0 (where A points), get number-of-bytes Y back for the call.
	ldy #TEMP0
	sty R
	ply
	phy
	jsr lsh		 	; Shift A one bit left
	ldy TEMP3+2		; Get loop Y back
	clc                     ; Don't want to rotate any bits into A
	rol			; Move test bit one bit up
	bne mull		; We didn't move it out of the byte, proceed
	inx			; Finished bits of that byte, go to next byte
	dey			; Reduce bytes to do
	beq muldone		; If bytes-to-do is zero, we're done
	lda #1			; Otherwise, start next byte at bit 0 again
	bra mull

muldone:
	;; We need to copy from our intermediate (B) to result.
	ldy TEMP3
	sty R
	ldy TEMP3+1
	sty R_

	ply
	jsr cpy_br
	
	plx
	pla
	rts

cpy_rb:
	;; Copy result to B
	pha
	phy

	dey 			; Param will e.g. be 8 for 64-bit, but we want to start at byte 7
cpy_rbl:
	lda (R),y
	sta (B),y
	dey
	bpl cpy_rbl

	ply
	pla
	rts

cpy_br:
	;; Copy B to result
	pha
	phy

	dey 			; Param will e.g. be 8 for 64-bit, but we want to start at byte 7
cpy_brl:
	lda (B),y
	sta (R),y
	dey
	bpl cpy_brl

	ply
	pla
	rts

cpy_at0:
	;; Copy A to temp0
	pha
	phx
	phy

	;; Start with byte (e.g.) 7, not 8. Copy number of bytes Y->X
	dey
	tya
	tax
cpy_at0l:
	lda (A),y
	sta TEMP0,x
	dex
	dey
	bpl cpy_at0l

	ply
	plx
	pla
	rts

cpy_bt1:
	;; Copy B to temp1
	pha
	phx
	phy

	;; Start with byte (e.g.) 7, not 8. Copy number of bytes Y->X
	dey
	tya
	tax
cpy_bt1l:
	lda (B),y
	sta TEMP1,x
	dex
	dey
	bpl cpy_bt1l

	ply
	plx
	pla
	rts

copy:
	;; Generic copy from A to R
	pha
	phy

	dey 			; Param will e.g. be 8 for 64-bit, but we want to start at byte 7
copyl:
	lda (A),y
	sta (R),y
	dey
	bpl copyl

	ply
	pla
	rts
	
rsh:
	;; Shift A right by one bit (stored in R, A is not changed)
	;;
	;; We start at the big end and loop down. ROR shifts out of carry and into carry, we just
	;; need to clear carry before our first ROR.
	pha
	phy

	dey 			; If we got e.g. 8, we want to start at 7
	clc
rshl:
	lda (A),y
	ror
	sta (R),y
	dey
	bpl rshl

	ply
	pla
	rts

lsh:
	;; Shift A left by one bit (stored in R, A is not changed)
	;;
	;; We start at the little end and loop up. ROL shifts out of carry and into carry, we just
	;; need to clear carry before our first ROR. We also need to count with X.
	pha
	phx
	phy

	tya
	tax
	ldy #0
	clc
lshl:
	lda (A),y
	rol
	sta (R),y
	iny
	dex
	bne lshl

	ply
	plx
	pla
	rts
	
	
add:
	;; Add A and B, both unsigned LE.
	;; 
	;; We need to preserve the carry bit between adds. We therefore have three options (afaict).
	;; These calculations assume 64-bit numbers.
	;; 
	;; a) 8 lda/adc/sta/iny repetitions, unrolled.
	;;    Cycles: LDA, ADC, STA are 5 cycles afaict. INY is 2. We'd skip the last INY.
	;;    Total cycles: 17*8-2=134 cycles.
	;;    Bytes: LDA, ADC, STA are 2 bytes. INY is 1.
	;;    Total bytes: 7*8-1=55 bytes.
	;; 
	;; b) No unrolling, using CPY to compare Y with 8. CPY will (in our case) clear carry.
	;;    We'd therefore, after INY, do a BCS to different branches, one doing a CPY, BNE and the
	;;    other doing a CPY, SEC, BNE.
	;;    Cycles when carry unset, LDA=5, ADC=5, STA=5, INY=2, BCS=2, CPY=2, BNE=3, Total 24
	;;    Cycles when carry set, LDA=5, ADC=5, STA=5, INY=2, BCS=3, CPY=2, SEC=2 BNE=3, Total 27
	;;    The last BNE is only 2 as it's not taken.
	;;    Total cycles: 24*4+27*4-1=96+108-1=203 cycles. (Assuming 50% carries.)
	;;    Bytes: LDA, ADC, STA are 2, INY 1, BCS 2, [CPY 2, BNE 2]*2, SEC 1.
	;;    Total bytes: 2+2+2+1+2+(2+2)*2+1=18 bytes.
	;;
	;; c) No unrolling but count up with Y (needed for our little-endian adds), but count down
	;;    with X (from 8). DEX sets the zero flag, then we use BNE for the looping. Carry remains
	;;    untouched, because we don't have a cPY.
	;;    Cycles: LDA, ADC, STA are 5, INY is 2, DEX is 2, BNE is 3 (except for the last, two)
	;;    But also an extra PHX/LDX/PLX, 3+2+4=9.
	;;    Total cycles: 22*8-1+13=184 cycles.
	;;    Bytes: LDA, ADC, STA are 2, INY 1, DEX 1, BNE 2. Plus the LDX, 2.
	;;    Total bytes: 2+2+2+1+1+2+2=12 bytes.
	;;
	;; (b) is a clear loser. (c) is 37% worse performance, but less than a quarter of the size.
	;; (c) seems like the best compromise, but given we have 32kiB of ROM to play with, there's
	;; an argument to be made for (a).
	;; We'll do (c).

	pha
	phx
	phy

	tya			; Move number-of-bytes to X
	tax
	ldy #$00
	clc 			; No carry coming in
addl:
	lda (A),y
	adc (B),y
	sta (R),y
	iny
	dex
	bne addl

	ply
	plx
	pla
	
	rts

compare:
	;; Compare A and B. C flag will be set on return if unequal.
	pha
	phy

	sty TEMP3
	ldy #$00
comparel:
	lda (A),y
	cmp (B),y
	bne comparene
	iny
	tya
	sbc TEMP3
	bne comparel
	clc 			; Completed loop with no inequality, clear carry
	bra compareeq
comparene:
	sec
compareeq:
	ply			; These don't affect carry
	pla
	
	rts
