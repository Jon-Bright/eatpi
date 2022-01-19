; Calling convention for routines here
; $0000 is two-byte LE address of operand A. The address may be stomped, the destination won't.
; $0002 is two-byte LE address of operand B. The address may be stomped, the destination won't.
; $0004 is two-byte LE address of result. The address won't be stomped, result will be stored at dest.
; $0010 through $002f are reserved for temp values and may be stomped

	A = $00
	A_ = $01
	B = $02
	B_ = $03
	R = $04
	R_ = $05
	TEMP0 = $10
	TEMP1 = $18
	TEMP2 = $20
	TEMP3 = $28
	
mul_64:
	;; Multiply A and B, each of which are 64-bit LE.
	pha
	phx
	phy

	jsr cpy_at0_64		; Move A to temp0 - we'll need to shift it in place
	;; Repoint A at temp0
	lda #TEMP0
	sta A
	lda #$00
	sta A_

	jsr cpy_bt1_64 		; Copy B to temp1.
	;; Repoint B at temp2 (NOT temp1) - we need it to point at something stompable
	;; as an intermediate operand 
	lda #TEMP2
	sta B
	lda #$00
	sta B_
	
	;; Zero B (because we'll be adding A to it)
	ldy #7
	lda #0
mul_64z:
	sta (B),y
	dey
	bpl mul_64z


	lda #1			; Start at bit 0
	ldx #0			; Start at byte 0 (lower-order because LE)

mul_64l:
	bit TEMP1,x
	beq mul_64next		; Bit not set, no add, move to next bit

	jsr add_64		; Bit is set, add A to intermediate (in B)
	jsr cpy_rb_64		; Copy the result back to B. TODO: probably just make a new add variant?

mul_64next:
	jsr lsh_64		; Shift A one bit left
	jsr cpy_ra_64		; Copy the result back to A
	rol			; Move test bit one bit up
	bne mul_64l		; We didn't move it out of the byte, proceed
	inx			; Finished bits of that byte, go to next byte
	cpx 8			; Did 8 bytes?
	beq mul_64done		; If yes, we're done
	lda #1			; Otherwise, start next byte at bit 0 again

mul_64done:
	;; We don't need to copy anything around - the most recent add will have ended up in result
	ply
	plx
	pla
	rts

cpy_ra_64:
	;; Copy result to A
	pha
	phy

	ldy #7
cpy_ral:
	lda (R),y
	sta (A),y
	dey
	bpl cpy_ral

	ply
	pla
	rts

cpy_rb_64:
	;; Copy result to B
	pha
	phy

	ldy #7
cpy_rbl:
	lda (R),y
	sta (B),y
	dey
	bpl cpy_rbl

	ply
	pla
	rts

cpy_at0_64:
	;; Copy A to temp0
	pha
	phx
	phy

	ldy #7
	ldx #TEMP0
cpy_at0l:
	lda (A),y
	sta $07,x
	dex
	dey
	bpl cpy_at0l

	ply
	plx
	pla
	rts

cpy_bt1_64:
	;; Copy B to temp1
	pha
	phx
	phy

	ldy #7
	ldx #TEMP1
cpy_bt1l:
	lda (B),y
	sta $07,x
	dex
	dey
	bpl cpy_bt1l

	ply
	plx
	pla
	rts
	
rsh_64:
	;; Shift A (64-bit LE) right by one bit (still store in result, A is not changed)
	;;
	;; We start at the big end and loop down. ROR shifts out of carry and into carry, we just
	;; need to clear carry before our first ROR.
	pha
	phy
	
	ldy #7
	clc
rsh_64l:
	lda ($00),y
	ror
	sta ($04),y
	dey
	bpl rsh_64l

	ply
	pla
	rts

lsh_64:
	;; Shift A (64-bit LE) left by one bit (still store in result, A is not changed)
	;;
	;; We start at the little end and loop up. ROL shifts out of carry and into carry, we just
	;; need to clear carry before our first ROR. We also need to count with X.
	pha
	phx
	phy

	ldx #8
	ldy #0
	clc
lsh_64l:
	lda ($00),y
	rol
	sta ($04),y
	iny
	dex
	bne lsh_64l

	ply
	plx
	pla
	rts
	
	
add_64:
	;; Add A and B, both unsigned 64-bit LE.
	;; 
	;; We need to preserve the carry bit between adds. We therefore have three options (afaict):
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
	
	ldy #$00
	ldx #$08
	clc 			; No carry coming in
a64l:
	lda ($00),y
	adc ($02),y
	sta ($04),y
	iny
	dex
	bne a64l

	ply
	plx
	pla
	
	rts

cmp_64:
	;; Compare A and B, each of which are 64-bit. C flag will be set on return if unequal.
	pha
	phy
	
	ldy #$00
c64l:
	lda ($00),y
	cmp ($02),y
	bne c64ne
	iny
	cpy #$08
	bne c64l
	clc 			; Completed loop with no inequality, clear carry
	bra c64eq
c64ne:
	sec
c64eq:
	ply			; These don't affect carry
	pla
	
	rts
