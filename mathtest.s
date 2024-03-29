RES = $3000

#include "call.s"

* = $8000 			; $8000 is the bottom of our ROM
reset:
	ldx #$ff		; Point at top of stack
	txs

	jsr lcd_init


	;; Test 1, add num1 and num2, compare
test1:
	jsr lcd_home
	lda #'1'
	jsr lcd_char_out

	lda #(num1 & $ff)
	sta A
	lda #(num1 >> 8)
	sta A_

	lda #(num2 & $ff)
	sta B
	lda #(num2 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr add

	lda #(res_1_2_add & $ff)
	sta A
	lda #(res_1_2_add >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test2
	jmp err_num_out

	;; Test 2, add num1 and num3, compare
test2:
	jsr lcd_home
	lda #'2'
	jsr lcd_char_out

	lda #(num1 & $ff)
	sta A
	lda #(num1 >> 8)
	sta A_

	lda #(num3 & $ff)
	sta B
	lda #(num3 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr add

	lda #(res_1_3_add & $ff)
	sta A
	lda #(res_1_3_add >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test3
	jmp err_num_out

	;; Test 3, add num4 and num5, compare
test3:
	jsr lcd_home
	lda #'3'
	jsr lcd_char_out

	lda #(num4 & $ff)
	sta A
	lda #(num4 >> 8)
	sta A_

	lda #(num5 & $ff)
	sta B
	lda #(num5 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr add

	lda #(res_4_5_add & $ff)
	sta A
	lda #(res_4_5_add >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test4
	jmp err_num_out

	;; Test 4, right-shift num1, compare
test4:
	jsr lcd_home
	lda #'4'
	jsr lcd_char_out

	lda #(num1 & $ff)
	sta A
	lda #(num1 >> 8)
	sta A_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr rsh

	lda #(res_num1_rsh & $ff)
	sta A
	lda #(res_num1_rsh >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test5
	jmp err_num_out

	;; Test 5, right-shift num5, compare
test5:
	jsr lcd_home
	lda #'5'
	jsr lcd_char_out

	lda #(num5 & $ff)
	sta A
	lda #(num5 >> 8)
	sta A_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr rsh

	lda #(res_num5_rsh & $ff)
	sta A
	lda #(res_num5_rsh >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test6
	jmp err_num_out

	;; Test 6, left-shift num1, compare
test6:
	jsr lcd_home
	lda #'6'
	jsr lcd_char_out

	lda #(num1 & $ff)
	sta A
	lda #(num1 >> 8)
	sta A_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr lsh

	lda #(res_num1_lsh & $ff)
	sta A
	lda #(res_num1_lsh >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test7
	jmp err_num_out

	;; Test 7, left-shift num5, compare
test7:
	jsr lcd_home
	lda #'7'
	jsr lcd_char_out

	lda #(num5 & $ff)
	sta A
	lda #(num5 >> 8)
	sta A_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr lsh

	lda #(res_num5_lsh & $ff)
	sta A
	lda #(res_num5_lsh >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test8
	jmp err_num_out

	;; Test 8, multiply num1 and num2, compare
test8:
	jsr lcd_home
	lda #'8'
	jsr lcd_char_out

	lda #(num1 & $ff)
	sta A
	lda #(num1 >> 8)
	sta A_

	lda #(num2 & $ff)
	sta B
	lda #(num2 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr mul

	lda #(res_1_2_mul & $ff)
	sta A
	lda #(res_1_2_mul >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test9
	jmp err_num_out

	;; Test 9, multiply num2 and num1, compare
	;; This _should_ be exactly the same result as test 8.
test9:
	jsr lcd_home
	lda #'9'
	jsr lcd_char_out

	lda #(num2 & $ff)
	sta A
	lda #(num2 >> 8)
	sta A_

	lda #(num1 & $ff)
	sta B
	lda #(num1 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr mul

	lda #(res_1_2_mul & $ff)
	sta A
	lda #(res_1_2_mul >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc testA
	jmp err_num_out

	;; Test A, multiply (now 32-bit!) num6 and num7, compare
testA:
	jsr lcd_home
	lda #'A'
	jsr lcd_char_out

	lda #(num6 & $ff)
	sta A
	lda #(num6 >> 8)
	sta A_

	lda #(num7 & $ff)
	sta B
	lda #(num7 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr mul

	lda #(res_6_7_mul & $ff)
	sta A
	lda #(res_6_7_mul >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc testB
	jmp err_num_out

	;; Test B, subtract num1 from num3, compare
testB:
	jsr lcd_home
	lda #'B'
	jsr lcd_char_out

	lda #(num3 & $ff)
	sta A
	lda #(num3 >> 8)
	sta A_

	lda #(num1 & $ff)
	sta B
	lda #(num1 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr sub

	lda #(res_3_1_sub & $ff)
	sta A
	lda #(res_3_1_sub >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc testC
	jmp err_num_out

	;; Test C, subtract num7 from num7, compare
testC:
	jsr lcd_home
	lda #'C'
	jsr lcd_char_out

	lda #(num7 & $ff)
	sta A
	lda #(num7 >> 8)
	sta A_

	lda #(num7 & $ff)
	sta B
	lda #(num7 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr sub

	lda #(res_7_7_sub & $ff)
	sta A
	lda #(res_7_7_sub >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc testD
	jmp err_num_out


	;; Test D, subtract num8 from num7, compare
testD:
	jsr lcd_home
	lda #'D'
	jsr lcd_char_out

	lda #(num7 & $ff)
	sta A
	lda #(num7 >> 8)
	sta A_

	lda #(num8 & $ff)
	sta B
	lda #(num8 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr sub

	lda #(res_7_8_sub & $ff)
	sta A
	lda #(res_7_8_sub >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc testE
	jmp err_num_out


	;; Test E, compare num7 >= num7, expect carry set
testE:
	jsr lcd_home
	lda #'E'
	jsr lcd_char_out

	lda #(num7 & $ff)
	sta A
	lda #(num7 >> 8)
	sta A_

	lda #(num7 & $ff)
	sta B
	lda #(num7 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr cmpge

	bcs testF
	jmp err_num_out


	;; Test F, compare num7 >= num8, expect carry clear
testF:
	jsr lcd_home
	lda #'F'
	jsr lcd_char_out

	lda #(num7 & $ff)
	sta A
	lda #(num7 >> 8)
	sta A_

	lda #(num8 & $ff)
	sta B
	lda #(num8 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr cmpge

	bcc test10
	jmp err_num_out


	;; Test 10, compare num8 >= num7, expect carry set
test10:
	jsr lcd_home
	lda #'1'
	jsr lcd_char_out
	lda #'0'
	jsr lcd_char_out

	lda #(num8 & $ff)
	sta A
	lda #(num8 >> 8)
	sta A_

	lda #(num7 & $ff)
	sta B
	lda #(num7 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr cmpge

	bcs test11
	jmp err_num_out


	;; Test 11, divide num9 by numA, compare
test11:
	jsr lcd_home
	lda #'1'
	jsr lcd_char_out
	lda #'1'
	jsr lcd_char_out

	lda #(num9 & $ff)
	sta A
	lda #(num9 >> 8)
	sta A_

	lda #(numA & $ff)
	sta B
	lda #(numA >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr div

	lda #(res_9_A_div & $ff)
	sta A
	lda #(res_9_A_div >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test12
	jmp err_num_out


	;; Test 12, divide num3 by num1, compare
test12:
	jsr lcd_home
	lda #'1'
	jsr lcd_char_out
	lda #'2'
	jsr lcd_char_out

	lda #(num3 & $ff)
	sta A
	lda #(num3 >> 8)
	sta A_

	lda #(num1 & $ff)
	sta B
	lda #(num1 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #8

	jsr div

	lda #(res_3_1_div & $ff)
	sta A
	lda #(res_3_1_div >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test13
	jmp err_num_out


	;; Test 13, multiply num9 by numB (zero), compare
test13:
	jsr lcd_home
	lda #'1'
	jsr lcd_char_out
	lda #'3'
	jsr lcd_char_out

	lda #(num9 & $ff)
	sta A
	lda #(num9 >> 8)
	sta A_

	lda #(numB & $ff)
	sta B
	lda #(numB >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr mul

	lda #(res_9_B_mul & $ff)
	sta A
	lda #(res_9_B_mul >> 8)
	sta A_

	lda #(RES & $ff)
	sta B
	lda #(RES >> 8)
	sta B_

	jsr cmpeq
	bcc test14
	jmp err_num_out


	;; Test 14, compare num7 > num7, expect carry clear
test14:
	jsr lcd_home
	lda #'1'
	jsr lcd_char_out
	lda #'4'
	jsr lcd_char_out

	lda #(num7 & $ff)
	sta A
	lda #(num7 >> 8)
	sta A_

	lda #(num7 & $ff)
	sta B
	lda #(num7 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr cmpgt

	bcc test15
	jmp err_num_out


	;; Test 15, compare num7 > num8, expect carry clear
test15:
	jsr lcd_home
	lda #'1'
	jsr lcd_char_out
	lda #'5'
	jsr lcd_char_out

	lda #(num7 & $ff)
	sta A
	lda #(num7 >> 8)
	sta A_

	lda #(num8 & $ff)
	sta B
	lda #(num8 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr cmpgt

	bcc test16
	jmp err_num_out


	;; Test 16, compare num8 > num7, expect carry set
test16:
	jsr lcd_home
	lda #'1'
	jsr lcd_char_out
	lda #'6'
	jsr lcd_char_out

	lda #(num8 & $ff)
	sta A
	lda #(num8 >> 8)
	sta A_

	lda #(num7 & $ff)
	sta B
	lda #(num7 >> 8)
	sta B_

	lda #(RES & $ff)
	sta R
	lda #(RES >> 8)
	sta R_

	ldy #4

	jsr cmpgt

	bcs test17
	jmp err_num_out



	;; Our work here is done
test17:
	lda #(msg_ok & $ff)
	sta $20
	lda #(msg_ok >> 8)
	sta $21
	jsr lcd_print


loop:
	jmp loop

err_num_out:
	lda #(RES & $ff)
	sta $20
	lda #(RES >> 8)
	sta $21
	jsr lcd_hex
	;; Fallthrough
error:
	lda #(msg_err & $ff)
	sta $20
	lda #(msg_err >> 8)
	sta $21
	jsr lcd_print
	jmp loop

#include "lcd.s"
#include "math.s"

msg_err:
	.asc "ERROR!",0

msg_ok:
	.asc "All tests pass!",0

num1:
	;; 12345678901 decimal as a little-endian 64-bit number
	.byte $35
	.byte $1c
	.byte $dc
	.byte $df
	.byte $02
	.byte $00
	.byte $00
	.byte $00


num2:
	;; 3 decimal as a little-endian 64-bit number
	.byte $03
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00

num3:
	;; A number big enough to cause a few overflows on addition with num1
	.byte $d2
	.byte $34
	.byte $44
	.byte $55
	.byte $ff
	.byte $00
	.byte $01
	.byte $01

num4:
	;; Pattern number
	.byte $55
	.byte $aa
	.byte $55
	.byte $aa
	.byte $55
	.byte $aa
	.byte $55
	.byte $aa

num5:
	;; Pattern number
	.byte $aa
	.byte $55
	.byte $aa
	.byte $55
	.byte $aa
	.byte $55
	.byte $aa
	.byte $55

num6:
	;; 32-bit ~sqrt of FFFFFFFF
	.byte $ff
	.byte $ff
	.byte $00
	.byte $00

num7:
	;; 32-bit ~sqrt of FFFFFFFF (other half)
	.byte $01
	.byte $00
	.byte $01
	.byte $00

num8:
	;; 1 more than num7, for purpose of subtraction test
	.byte $02
	.byte $00
	.byte $01
	.byte $00

num9:
	;; $12345 * $1abc
	.byte $ac
	.byte $e8
	.byte $6a
	.byte $1e

numA:
	;; Divisor for num9
	.byte $bc
	.byte $1a
	.byte $00
	.byte $00

numB:
	;; Zero, to test multiplying by zero
	.byte $00
	.byte $00
	.byte $00
	.byte $00

res_1_2_mul:
	;; 37037036703 decimal as a little-endian 64-bit number. Result of multiplying num1 and num2.
	.byte $9f
	.byte $54
	.byte $94
	.byte $9f
	.byte $08
	.byte $00
	.byte $00
	.byte $00

res_1_2_add:
	;; 12345678904 decimal as a little-endian 64-bit number. Result of adding num1 and num2.
	.byte $38
	.byte $1c
	.byte $dc
	.byte $df
	.byte $02
	.byte $00
	.byte $00
	.byte $00

res_1_3_add:
	;; Result of adding num1 and num3.
	.byte $07
	.byte $51
	.byte $20
	.byte $35
	.byte $02
	.byte $01
	.byte $01
	.byte $01

res_4_5_add:
	;; Result of adding num4 and num5.
	.byte $ff
	.byte $ff
	.byte $ff
	.byte $ff
	.byte $ff
	.byte $ff
	.byte $ff
	.byte $ff

res_num1_rsh:
	;; num1 shifted right by one bit
	.byte $1a
	.byte $0e
	.byte $ee
	.byte $6f
	.byte $01
	.byte $00
	.byte $00
	.byte $00

res_num5_rsh:
	;; num5 shifted right by one bit
	.byte $d5
	.byte $2a
	.byte $d5
	.byte $2a
	.byte $d5
	.byte $2a
	.byte $d5
	.byte $2a

res_num1_lsh:
	;; num1 shifted left by one bit
	.byte $6a
	.byte $38
	.byte $b8
	.byte $bf
	.byte $05
	.byte $00
	.byte $00
	.byte $00

res_num5_lsh:
	;; num5 shifted right by one bit
	.byte $54
	.byte $ab
	.byte $54
	.byte $ab
	.byte $54
	.byte $ab
	.byte $54
	.byte $ab

res_6_7_mul:
	;; num6 and num7 multiplied
	.byte $ff
	.byte $ff
	.byte $ff
	.byte $ff

res_3_1_sub:
	;; num3 minus num1
	.byte $9d
	.byte $18
	.byte $68
	.byte $75
	.byte $fc
	.byte $00
	.byte $01
	.byte $01

res_7_7_sub:
	;; num7 minus num7 - zero
	.byte $00
	.byte $00
	.byte $00
	.byte $00

res_7_8_sub:
	;; num7 minus num8 - minus 1
	.byte $ff
	.byte $ff
	.byte $ff
	.byte $ff

res_9_A_div:
	;; num9 divided by numA
	.byte $45
	.byte $23
	.byte $01
	.byte $00

res_3_1_div:
	;; num3 divided by num1. There's a remainder too, not represented here.
	.byte $e1
	.byte $68
	.byte $59
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00

res_9_B_mul:
	;; num9 (or indeed anything else) multiplied by numB.
	.byte $00
	.byte $00
	.byte $00
	.byte $00

codeend:
	;; This section will pad out the file to 32k and also insert the interrupt and reset vectors.
	;; The reset vector will point at address $8000, which is where the ROM is mapped to for our
	;; 6502 (and what we set as start address above).

* = $fffa
	.dsb (*-codeend), $ea	; Fill everything from end of code to vectors with NOPs
* = $fffa
	.word $0000		; NMI Vector
	.word reset		; Reset Vector
	.word $0000		; BRK/IRQB Vector
