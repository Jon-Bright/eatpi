; 6522 registers as defined by logic on the board
IORB = $6000
IORA = $6001
DDRB = $6002
DDRA = $6003

; Enable, read/write and register select for the LCD, on IORA
E  = %10000000
RW = %01000000
RS = %00100000

lcd_init:
	;; Initialize the LCD, including setting up data direction
	lda #%11111111
	sta DDRB       ; Set all of DDRB to output
	lda #%11100000
	sta DDRA       ; Set top 3 of DDRA to output

	lda #%00000001 ; Clear display, reset position to 0
	jsr lcd_setup_cmd

	lda #%00111000 ; 8-bit, 2-line, 5x8 font
	jsr lcd_setup_cmd

	lda #%00001110 ; Display on, cursor on, blink off
	jsr lcd_setup_cmd

	lda #%00000110 ; Cursor increment, no display shift
	jsr lcd_setup_cmd

	rts

lcd_home:
	lda #%00000010
	jsr lcd_setup_cmd
	rts

lcd_print:
	;; Print the message pointed to at $0020/1 to the LCD
	ldy #0
lcd_print_loop:
	lda ($20),y
	beq lcd_print_ret
	jsr lcd_char_out
	iny
	jmp lcd_print_loop
lcd_print_ret:
	rts
	
lcd_hex:
	;; Print the LE number pointed to at $0020/1 to the LCD
	;; y should be set to the number of bytes in the number
	dey 			; If the number's 8 bytes, we want to start at byte 7
lcd_hex_loop:
	;; Top nybble
	lda ($20),y
	lsr
	lsr
	lsr
	lsr
	jsr lcd_hex_digit

	;; Bottom nybble
	lda ($20),y
	and #$0f
	jsr lcd_hex_digit

	dey
	bpl lcd_hex_loop
	rts

lcd_hex_digit:
	;; Output the single hex digit in A
	cmp #$0a		; Carry set if we're A or higher.
	bcc lcd_hex_1add
	adc #('A'-('0'+11))
lcd_hex_1add:
	adc #'0'
	jsr lcd_char_out
	rts
	
lcd_wait:
	;; Wait for the LCD to be ready for a new command
	pha            ; Save A for later
	lda #%00000000
	sta DDRB       ; Port B should be input for this
lcd_wait_loop:
	lda #RW        ; We want to read
	sta IORA
	lda #(RW | E)  ; Enable
	sta IORA
	lda IORB
	; If top bit of A is set, LCD is busy
	and #$80
	bne lcd_wait_loop

	lda #%11111111
	sta DDRB       ; Reset Port B to output
	pla
	rts

lcd_setup_cmd:
	;; Send the setup command in A to the LCD
	jsr lcd_wait

	sta IORB       ; Setup command to LCD data

	lda #0         ; Clear RS/RW/E
	sta IORA

	lda #E         ; Set E
	sta IORA

	lda #0         ; Clear E
	sta IORA
	rts

lcd_char_out:
	;; Emit the char in A to the LCD
	jsr lcd_wait

	sta IORB       ; Character to LCD data

	lda #RS        ; Set RS, clear RW/E
	sta IORA

	lda #(RS | E)  ; Set E
	sta IORA

	lda #RS        ; Clear E
	sta IORA
	rts
