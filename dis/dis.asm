; Atliko: Tomas Kozakas

.model small

.stack
	MAX_BUFF 	EQU 10
	JUMPS
.data
	about               db "Tomas Kozakas, 2 kursas, 1 grupe.", 10, 13
						db 'Programa disasembleris vercia masinini koda i assemblerio kalba.', 10, 13, 9
						db 'dis.exe mano.source kodas.destination$'

	err_op 			    db ' - nepavyko atidaryti', 13, 10, '$'
	err_re 				db 'nepavyko skaityti', 13, 10, '$'

	source     			db MAX_BUFF, ?, MAX_BUFF dup(0)
	sourceHandle    	dw ?
	destination     	db MAX_BUFF, ?, MAX_BUFF dup(0)
	destHandle			dw ?
	buffer 				db MAX_BUFF dup(?)
	
	address				dw 100h
	hex					dw 0
	operand				dw 0
	prev_byte			db 0
	mod_byte			db 0
	
	;;; COMMANDS
	__MOV				db 'mov $'
	__RCR				db 'rcr $'
	__NOT				db 'not $'
	__OUT				db 'out $'
	__XLAT				db 'xlat $'

	__MOVSB				db 'movsb $'
	__MOVSW				db 'movsw $'
	;;; REGISTERS
	;; registers 
	__AX				db 'ax$'
	__CX				db 'cx$'
	__DX				db 'dx$'
	__BX				db 'bx$'
	; data low
	__AL				db 'al$'
	__CL				db 'cl$'
	__DL				db 'dl$'
	__BL				db 'bl$'
	; data high
	__AH				db 'ah$'
	__CH				db 'ch$'
	__DH				db 'dh$'
	__BH				db 'bh$'
	;; pointer
	__BP				db 'bp$'
	__SP				db 'sp$'
	__BYTE_PTR			db 'byte ptr $'
	__WORD_PTR			db 'word ptr $'
	;; index
	__SI				db 'si$'
	__DI				db 'di$'
	;; segment
	__ES				db 'es$'
	__SS				db 'ss$'
	__DS				db 'ds$'
	__CS				db 'cs$'
	;;; OPERATORS
	__PLUS				db ' + $'
	__MINUS				db ' - $'
	__OPEN_BR			db '[$'
	__CLOSE_BR			db ']$'
	__COMMA				db ', $'
	__COLON				db ':$'

	;;; invalid COMMAND
	__INVALID			db 'invalid $'

	;__WORD				db '(word) $'
	;__BYTE				db '(byte) $'

	alignment9			db 9, '$'
	alignment99			db 9, 9, '$'
	string 				db 10 dup(0)
	endline          	db 13, '$'


.code

@@beginning:	
	jmp near ptr main

main proc near
    ;; open files
    call near ptr read_terminal
    
	;; read source
	call near ptr file_read

@@main_end:
	;; return to dos
    mov ax, 4c00h
    int 21h
main endp


file_read proc near
@@new_buffer:
	mov ah, 3Fh
	mov dx, offset buffer
	mov bx, sourceHandle
	mov cx, MAX_BUFF
	int 21h
	jc @@error_read
	
	;; sourcepare if nothing was read
	mov cx, ax
	cmp ax, 0                   
	je @@read_end        

	;; point si to dx
	mov si, dx
@@repeat:
	;; get one byte, store it in al
	call getByte
	call dissasemble
	loop @@repeat

	jmp @@new_buffer
@@error_read:
    mov ah, 9h
	mov dx, offset err_re
	int 21h
@@read_end:
	;; close file
	mov ah, 3eh
	int 21h
    ret
file_read endp


convertHex proc
	mov cl, 2
@@convert:
	mov dl, bl
	and dl, 00001111b
	cmp dl, 9
	ja 	@@toAscii
	jbe @@putChar
@@toAscii:
	add dl, '0'
@@putChar:
	mov [si], dl

	shr bl, 4
	dec si
	dec cl
	jnz @@convert

	ret
convertHex endp


setupHex proc
	push ax cx si

	mov ax, address

	mov si, offset string + 9    	
	mov byte ptr [si], '$'
	
	mov bl, al
	call convertHex
	mov bl, ah
	call convertHex

	mov ax, si 
	call setupOperand

	pop si cx ax
	ret
setupHex endp


printAddress proc
	call setupHex
	
	mov dx, offset __COLON
	call file_write

	mov dx, offset alignment9
	call file_write

	RET
printAddress endp


;; write stuff to file
file_write proc near
	push ax cx

	;; find length of string (store length in cx)
	call strlen
	mov ah, 40h
	mov bx, destHandle
	int  21h

	pop cx ax
	ret
file_write ENDP



segm proc
	test al, 00000010b
	jnz @@s00000010s
	jz	@@s00000000s

@@s00000010s:
	test al, 00000001b
	jnz	@@s00000011sf
	jz	@@s00000010sf

	@@s00000011sf:
		mov dx, offset __DS
		call file_write
		jmp @@returnseg
	@@s00000010sf:
		mov dx, offset __SS
		call file_write
		jmp @@returnseg
@@s00000000s:
	test al, 00000001b
	jnz	@@s00000001sf
	jz	@@s00000000sf
	
	@@s00000001sf:
		mov dx, offset __CS
		call file_write
		jmp @@returnseg
	@@s00000000sf:
		mov dx, offset __ES
		call file_write
		jmp @@returnseg
@@returnseg:
	ret
segm endp



rm proc
	test prev_byte, 00000001b
	jnz	@@word_ptr
	jz	@@byte_ptr

@@word_ptr:
	mov dx, offset __WORD_PTR
	call file_write
	jmp @@r00000000m
@@byte_ptr:
	mov dx, offset __BYTE_PTR
	call file_write
	jmp @@r00000000m
	
@@r00000000m:
	mov dx, offset __open_br
	call file_write

	test al, 00000100b
	jnz	@@r00000100mf
	jz 	@@r00000000mf

	@@r00000100mf:
		test al, 00000010b
		jnz @@r00000110mff
		jz	@@r00000100mff

		@@r00000110mff:
			test al, 00000001b
			jnz @@r00000111mfff
			jz 	@@r00000110mfff
			
			@@r00000111mfff:
				mov dx, offset __BX
				call file_write
				jmp @@returnc
			@@r00000110mfff:
				mov dx, offset __BP
				call file_write
				jmp @@returnc

		@@r00000100mff:
			test al, 00000001b
			jnz @@r00000101mfff
			jz 	@@r00000100mfff

			@@r00000101mfff:
				mov dx, offset __DI
				call file_write
				jmp @@returnc
			@@r00000100mfff:
				mov dx, offset __SI
				call file_write
				jmp @@returnc
	@@r00000000mf:
		test al, 00000010b 
		jnz @@r00000010mff
		jz	@@r00000000mff

		@@r00000010mff:
			test al, 00000001b
			jnz @@r00000011mfff
			jz	@@r00000010mfff

			@@r00000011mfff:
				mov dx, offset __BP
				call file_write
				mov dx, offset __plus
				call file_write
				mov dx, offset __DI
				call file_write
				jmp @@continue
			@@r00000010mfff:
				mov dx, offset __BP
				call file_write
				mov dx, offset __plus
				call file_write
				mov dx, offset __SI
				call file_write
				jmp @@continue
		@@r00000000mff:
			test al, 00000001b
			jnz @@r00000001mfff
			jz	@@r00000000mfff

			@@r00000001mfff:
				mov dx, offset __BX
				call file_write
				mov dx, offset __plus
				call file_write
				mov dx, offset __DI
				call file_write
				jmp @@continue
			@@r00000000mfff:
				mov dx, offset __BP
				call file_write
				mov dx, offset __plus
				call file_write
				mov dx, offset __SI
				call file_write
				jmp @@continue
@@continue:
	;; shift flag (poslinkis)
	cmp di, 1
	jne @@putbr

	mov dx, offset __PLUS
	call file_write

	push ax
	call getByte
	call setupOperand

	pop ax
@@putbr:
	mov dx, offset __close_br
	call file_write
@@returnrm:
	ret
rm endp


regw proc
	test al, 00000100b
	jnz @@w00000100w
	jz	@@w00000000w
	
@@w00000100w:
	test al, 00000010b
	jnz @@w00000110wf
	jz	@@w00000100wf

	@@w00000110wf:
		test al, 00000001b
		jnz	@@w00000111wff
		jz 	@@w00000110wff

		@@w00000111wff:
			mov dx, offset __DI
			call file_write
			jmp @@returnw

		@@w00000110wff:
			mov dx, offset __SI
			call file_write
			jmp @@returnw

	@@w00000100wf:
		test al, 00000001b
		jnz @@w00000101wff
		jz 	@@w00000100wff

		@@w00000101wff:
			mov dx, offset __BP
			call file_write
			jmp @@returnw

		@@w00000100wff:
			mov dx, offset __SP
			call file_write
			jmp @@returnw

@@w00000000w:
	test al, 00000010b
	jnz @@w00000010wf
	jz	@@w00000000wf

	@@w00000000wf:
		test al, 00000001b
		jnz	@@w00000001wff
		jz	@@w00000000wff

		@@w00000001wff:
			mov dx, offset __CX
			call file_write
			jmp @@returnw
		@@w00000000wff:
			mov dx, offset __AX
			call file_write
			jmp @@returnw
	@@w00000010wf:
		test al, 00000001b
		jnz @@w00000011wff
		jz 	@@w00000010wff

		@@w00000011wff:
			mov dx, offset __BX
			call file_write
			jmp @@returnw
		@@w00000010wff:
			mov dx, offset __DX
			call file_write
			jmp @@returnw
@@returnw:
	ret
regw endp

regb proc
	test al, 00000100b
	jnz @@b00000100b
	jz	@@b00000000b
	
@@b00000100b:
	test al, 00000010b
	jnz @@b00000110bf
	jz	@@b00000100bf

	@@b00000110bf:
		test al, 00000001b
		jnz	@@b00000111bff
		jz 	@@b00000110bff

		@@b00000111bff:
			mov dx, offset __BH
			call file_write
			jmp @@returnb

		@@b00000110bff:
			mov dx, offset __DH
			call file_write
			jmp @@returnb

	@@b00000100bf:
		test al, 00000001b
		jnz @@b00000101bff
		jz 	@@b00000100bff

		@@b00000101bff:
			mov dx, offset __CH
			call file_write
			jmp @@returnb

		@@b00000100bff:
			mov dx, offset __AH
			call file_write
			jmp @@returnb

@@b00000000b:
	test al, 00000010b
	jnz @@b00000010bf
	jz	@@b00000000bf

	@@b00000000bf:
		test al, 00000001b
		jnz	@@b00000001bff
		jz	@@b00000000bff

		@@b00000001bff:
			mov dx, offset __CL
			call file_write
			jmp @@returnb
		@@b00000000bff:
			mov dx, offset __AL
			call file_write
			jmp @@returnb

	@@b00000010bf:
		test al, 00000001b
		jnz @@b00000011bff
		jz 	@@b00000010bff

		@@b00000011bff:
			mov dx, offset __BL
			call file_write
			jmp @@returnb
		@@b00000010bff:
			mov dx, offset __DL
			call file_write
			jmp @@returnb
@@returnb:
	ret
regb endp



setupComm proc
	test al, 10000000b
	jnz @@c10000000c
	jz 	@@c00000000c
	
@@c10000000c:
	test al, 01000000b
	jnz @@c11000000cf
	jz  @@c10000000cf

	@@c11000000cf:
		test al, 00100000b
		jnz @@c11100000cff
		jz	@@c11000000cff

		@@c11100000cff:
			test al, 00010000b
			jnz @@c11110000cfff
			jz 	@@c11100000cfff

			@@c11110000cfff: 
				test al, 00001000b
				jnz	@@c11111000cffff
				jz	@@c11110000cffff

				@@c11111000cffff:
					jmp @@invalid
				@@c11110000cffff:
					test al, 00000100b
					jnz	@@c11110100cfffff
					jz	@@c11110000cfffff

					@@c11110100cfffff:
						test al, 00000010b
						jnz @@c11110110cffffff
						jz 	@@c11110100cffffff

						@@c11110110cffffff:
							call getByte
							test al, 00100000b
							jnz @@c11110110c00100000cff
							jz	@@c11110110c00000000cff

							@@c11110110c00100000cff:
								jmp @@invalid
							@@c11110110c00000000cff:
								test al, 00010000b
								jnz @@c11110110c00010000cfff
								jz	@@c11110110c00000000cfff
							
								@@c11110110c00010000cfff:
									test al, 00001000b
									jnz @@c11110110c00011000cffff
									jz	@@c11110110c00010000cffff

									@@c11110110c00011000cffff:
										jmp @@invalid
									@@c11110110c00010000cffff:
										jmp @@not
									
								@@c11110110c00000000cfff:
									jmp @@invalid

						@@c11110100cffffff:
							jmp @@invalid

					@@c11110000cfffff:
						jmp @@invalid

			@@c11100000cfff:
				test al, 00001000b
				jnz	@@c11101000cffff
				jz	@@c11100000cffff

				@@c11101000cffff:
					test al, 00000100b
					jnz	@@c11101100cfffff
					jz	@@c11101000cfffff
					
					@@c11101100cfffff:
						test al, 00000010b
						jnz	@@c11101110cffffff
						jz	@@c11101100cffffff
						
						@@c11101110cffffff:
							jmp @@out
						@@c11101100cffffff:
							jmp @@invalid

					@@c11101000cfffff:
						jmp @@invalid
				@@c11100000cffff:
					test al, 00000100b
					jnz	@@c11100100cfffff
					jz	@@c11100000cfffff

					@@c11100100cfffff:
						test al, 00000010b
						jnz	@@c11100110cffffff
						jz	@@c11100000cffffff

						@@c11100110cffffff:
							jmp @@out
						@@c11100000cffffff:
							jmp @@invalid

					@@c11100000cfffff:
						jmp @@invalid
		@@c11000000cff:
			test al, 00010000b
			jnz @@c11010000cfff
			jz 	@@c11000000cfff

			@@c11010000cfff:
				test al, 00001000b
				jnz	@@c11011000cffff
				jz	@@c11010000cffff

				@@c11011000cffff:
					jmp @@invalid
				@@c11010000cffff:
					test al, 00000100b
					jnz	@@c11010100cfffff
					jz	@@c11010000cfffff

					@@c11010100cfffff:
						jmp @@invalid
					@@c11010000cfffff:
						call getByte
						test al, 00100000b
						jnz	@@c11010000c00100000cff
						jz	@@c11010000c00000000cff

						@@c11010000c00100000cff:
							jmp @@invalid
						@@c11010000c00000000cff:
							test al, 00010000b
							jnz	@@c11010000c00010000cfff
							jz	@@c11010000c00000000cfff

							@@c11010000c00010000cfff:
								test al, 00001000b
								jnz	@@c11010000c00011000cffff
								jz	@@c11010000c00010000cffff

								@@c11010000c00011000cffff:
									jmp @@rcr
								@@c11010000c00010000cffff:
									jmp @@invalid
							@@c11010000c00000000cfff:
								jmp @@invalid
			@@c11000000cfff:
				test al, 00001000b
				jnz	@@c11001000cffff
				jz	@@c11000000cffff

				@@c11000000cffff:
					test al, 00000100b
					jnz	@@c11000100cfffff
					jz	@@c11000000cfffff

					@@c11000100cfffff:
						test al, 00000010b
						jnz	@@c11000110cffffff
						jz	@@c11000100cffffff

						@@c11000110cffffff:
							mov prev_byte, al
							lodsb
							test al, 001000000b
							jnz	@@c11000110c00100000cff
							jz	@@c11000110c00000000cff

							@@c11000110c00100000cff:
								jmp @@invalid
							@@c11000110c00000000cff:
								test al, 00010000b
								jnz	@@c11000110c00010000cfff 
								jz	@@c11000110c00000000cfff

								@@c11000110c00000000cfff:
									test al, 00001000b
									jnz	@@c11000110c00001000cffff 
									jz	@@c11000110c00000000cffff

									@@c11000110c00001000cffff:
										jmp @@invalid
									@@c11000110c00000000cffff:
										jmp @@mov_rm_imm
								@@c11000110c00010000cfff:
									jmp @@invalid
						@@c11000100cffffff:
							jmp @@invalid
					@@c11000000cfffff:
						jmp @@invalid	
				@@c11001000cffff:
					jmp @@invalid

	@@c10000000cf:
		test al, 00100000b
		jnz @@c10100000cff
		jz	@@c10000000cff

		@@c10100000cff:
			test al, 00010000b
			jnz @@c10110000cfff
			jz 	@@c10100000cfff

			@@c10110000cfff:
				jmp @@mov_reg_imm
		
			@@c10100000cfff:
				test al, 00000100b
				jnz	@@c10100100cffff
				jz	@@c10100000cffff

				@@c10100100cffff:
					test al, 0000010b
					jnz	@@c10100110cfffff
					jz	@@c10100100cfffff

					@@c10100110cfffff:
						jmp @@invalid

					@@c10100100cfffff:
						jmp @@movs

				@@c10100000cffff:
					test al, 00000010b
					jnz @@c10100010cfffff
					jz	@@c10100000cfffff

					@@c10100010cfffff:
						jmp @@mov_mem_acc
					@@c10100000cfffff:
						jmp @@mov_acc_mem

		@@c10000000cff:
			test al, 00010000b
			jnz @@c10010000cfff
			jz 	@@c10000000cfff

			@@c10010000cfff:
				jmp @@invalid
			@@c10000000cfff:
				test al, 00001000b
				jnz @@c10001000cffff
				jz	@@c10000000cffff

				@@c10001000cffff:
					test al, 00000100b
					jnz	@@c10001100cfffff
					jz	@@c10001000cfffff

					@@c10001100cfffff:
						test al, 00000010b
						jnz	@@c10001110cffffff
						jz	@@c10001100cffffff

						@@c10001110cffffff:
							jmp @@mov_seg_rm
						@@c10001100cffffff:
							jmp @@mov_rm_seg

					@@c10001000cfffff:
						test al, 00000010b
						jnz @@c10001010cffffff
						jz	@@c10001000cffffff

						@@c10001010cffffff:
							jmp @@mov_reg_rm

						@@c10001000cffffff:
							test al, 00000001b
							jnz	@@c10001001cfffffff
							jz	@@c10001000cfffffff

							@@c10001001cfffffff:
								jmp @@invalid
							@@c10001000cfffffff:
								jmp @@mov_rm_reg

				@@c10000000cffff:
					jmp @@invalid
@@c00000000c:
	test al, 01000000b
	jnz	@@c01000000cf
	jz	@@c00000000cf

	@@c01000000cf:
		jmp @@invalid
	@@c00000000cf:
		test al, 00100000b
		jnz @@c00100000cff
		jz 	@@c00000000cff

		@@c00100000cff:
			test al, 00000100b
			jnz @@c00100100cfff
			jz 	@@c00100000cfff
			@@c00100100cfff:
				test al, 00000010b
				jnz	@@c00100110cffff
				jz	@@c00100100cffff

				@@c00100110cffff:
					jmp @@segm_change
				@@c00100100cffff:
					jmp @@invalid
			@@c00100000cfff:
				jmp @@invalid
		@@c00000000cff:
			test al, 00010000b
			jnz @@c00010000cfff
			jz	@@c00000000cfff

			@@c00010000cfff:
			 	jmp @@invalid
			@@c00000000cfff:
				test al, 00001000b
				jnz @@c00001000cffff
				jz	@@c00000000cffff

				@@c00001000cffff:
					jmp @@invalid
				@@c00000000cffff:
					test al, 00000100b
					jnz	@@c00000100cfffff
					jz	@@c00000000cfffff

					@@c00000100cfffff:
						jmp @@invalid
					@@c00000000cfffff:
						test al, 00000010b
						jnz @@c00000010cffffff
						jz	@@c00000000cffffff

						@@c00000010cffffff:
							jmp @@invalid
						@@c00000000cffffff:
							test al, 00000001b
							jnz @@c00000001cfffffff
							jz 	@@c00000000cfffffff

							@@c00000001cfffffff:
								jmp @@invalid
							@@c00000000cfffffff:
								jmp @@invalid 
@@segm_change:
	
	jmp @@invalid
@@mov_rm_imm:
	mov dx, offset __MOV
	call file_write

	jmp @@invalid
@@mov_reg_imm:
	;1011 wreg bojb [bovb] – MOV registras <- betarpiškas operandas
	mov dx, offset __MOV
	call file_write

	mov prev_byte, al
	mov mod_byte, 11000000b
	shr prev_byte, 3
	call setupArg

	mov dx, offset __COMMA
	call file_write

	;; TODO 2 byte numbers
	call getByte
	call setupOperand

	jmp @@returnc
@@mov_acc_mem:
	
	jmp @@invalid
@@mov_mem_acc:
	
	jmp @@invalid
@@movs:
	; 1010 010w – movsb; movsw
	test al, 00000001b
	jnz @@movsw
	jz	@@movsb
	@@movsw:
		mov dx, offset __MOVSW
		call file_write	
		jmp @@returnc
	@@movsb:
		mov dx, offset __MOVSB
		call file_write	
		jmp @@returnc
@@mov_seg_rm:
	mov dx, offset __MOV
	call file_write

	call getByte

	push ax
	;; shift middle bits to right
	shr al, 3
	call segm

	mov dx, offset __COMMA
	call file_write

	pop ax
	;; register is always 2 bytes type
	; so make sure previous last bit is 1
	or prev_byte, 00000001b 
	call setupArg

	jmp @@returnc
@@mov_rm_seg:
	mov dx, offset __MOV
	call file_write

	call getByte

	;; register is always 2 bytes type
	; so make sure previous last bit is 1
	or prev_byte, 00000001b 
	call setupArg

	mov dx, offset __COMMA
	call file_write

	;; shift middle bits to right
	shr al, 3
	call segm
	

	jmp @@returnc
@@mov_reg_rm:
	mov dx, offset __MOV
	call file_write

	call getByte

	push ax
	shr al, 3
	call setupArg

	mov dx, offset __COMMA
	call file_write
	
	pop ax
	call setupArg

	jmp @@returnc
@@mov_rm_reg:
	mov dx, offset __MOV
	call file_write

	call getByte
	call setupArg

	mov dx, offset __COMMA
	call file_write

	mov mod_byte, 11000000b
	shr al, 3
	call setupArg

	jmp @@returnc
@@rcr:
	; 1101 00vw mod 011 r/m [poslinkis] – RCR registras/atmintis, {1; CL}
	mov dx, offset __RCR
	call file_write

	mov mod_byte, al
	call setupArg

	mov dx, offset __COMMA
	call file_write

	test prev_byte, 00000010b
	jnz	@@shiftcl
	jz	@@shift1

	@@shiftcl:
		mov dx, offset __CL
		call file_write
		jmp @@returnc
	@@shift1:
		mov ax, 1
		call setupOperand
		jmp @@returnc
@@out:
	; 1110 011w portas – out portas
	; 1110 111w – out

	mov dx, offset __OUT
	call file_write

	mov dx, offset __DX
	call file_write

	mov dx, offset __COMMA
	call file_write

	mov dx, offset __AX
	call file_write

	jmp @@returnc
@@not:
	; 1111 011w mod 010 r/m [poslinkis] – NOT registras/atmintis
	mov dx, offset __NOT
	call file_write

	mov mod_byte, al
	call setupArg

	jmp @@returnc
@@invalid:
	mov dx, offset __INVALID
	call file_write
@@returnc:
	mov dx, offset endline
	call file_write
	
	ret
setupComm endp


getByte proc
	inc address
	mov prev_byte, al
	lodsb
	mov mod_byte, al
	ret
getByte endp


setupArg proc
	test mod_byte, 10000000b
	jnz	@@mod10b
	jz	@@mod00b

	@@mod10b:	
		test mod_byte, 01000000b
		jnz	@@mod11bf
		jz	@@mod10bf

		@@mod11bf:
			test prev_byte, 00000001b
			jnz	@@words
			jz	@@bytes

			@@words:	
				call regw
				jmp @@ret
			@@bytes:
				call regb
				jmp @@ret
		@@mod10bf:
			;; shift_flag 1
			mov di, 1 
			call rm
			jmp @@ret

	@@mod00b:
		test mod_byte, 01000000b
		jnz	@@mod01bf
		jz	@@mod00bf

		@@mod01bf:
			;; shift_flag 1
			mov di, 1
			call rm
			jmp @@ret
		@@mod00bf:
			;; shift_flag 0
			mov di, 0
			call rm
			jmp @@ret
@@ret:
	ret
setupArg endp


setupOperand proc
	push si
	mov si, offset string + 9    	; points to the last symbol
	mov byte ptr [si], '$'       	; puts dollar sign at the end
	
	mov bx, 10                   	; divides bx by 10
@@asc2:
	xor dx, dx                   	; clears dx
	div bx                       	; dx is remainder of ax / bx
	add dx, '0'                  	; add 48
	dec si                       	; take another char
	mov [si], dl                 	; put number in si
	
	cmp ax, 0                    	; if ax is 0 (divided)
	jz @@print                   	; end
	jmp @@asc2                   	; take another char
@@print:
	mov dx, si
	call file_write
	pop si
	ret
setupOperand ENDP


dissasemble proc near
	call printAddress
	; call printHex
	call setupComm
	
@@dissasemble_end:
	ret
dissasemble endp



read_terminal proc near
	mov ax, @data
	mov es, ax 

	;; terminal parameters start at ds:81h 
    mov si, 81h                          
 
    ;; read source file name
    lea	di, source
	call	file_readname	

    ;; read destination file name
    lea	di, destination
	call	file_readname		

	mov ax, @data
	mov ds, ax

	;; check if file names were read
    cmp byte ptr ds:[source], '$'   
	je @@about
    cmp byte ptr ds:[destination], '$'   
    je @@about

	;; open source file for reading
    mov	dx, offset source		        
	call near ptr open_reading
	mov sourceHandle, ax        

	;; open destination file for reading
	mov dx, offset destination
    call near ptr open_writing
	mov destHandle, ax      	

    jmp @@input_end
@@about:
	mov dx, offset about
	mov ah, 09h
	int 21h
    jmp @@main_end
@@err_open:
    mov ah, 09h
	int 21h
	mov dx, offset err_op
	int 21h
    jmp @@main_end
@@input_end:
    ret
read_terminal endp



open_reading PROC near
	mov ah, 3Dh                     
	mov al, 0                      
	int 21h                         
	jc @@err_open                  
    ret
open_reading endp



open_writing PROC near 
	mov	ah, 3ch				        
	mov	cx, 0				        
	int	21h					        
	jc	@@err_open
	mov	ah, 3dh				        
	mov	al, 1				        
	int	21h					        
	jc	@@err_open
    ret
open_writing endp


skip_spaces PROC near
@@skip_spaces_loop:
	cmp byte ptr ds:[si], ' '
	jne @@skip_spaces_end
	inc si
	jmp @@skip_spaces_loop

@@skip_spaces_end:
	ret
skip_spaces ENDP


file_readname PROC near
	push ax
	call skip_spaces

@@file_readname_start:
	cmp byte ptr ds:[si], 13      
	je @@file_readname_end         
	cmp byte ptr ds:[si], ' '     
	jne @@file_readname_next       
@@file_readname_end:
	mov al, '$'                  
	stosb                        ; store AL at address ES:(E)DI, di = di + 1
	pop ax
	ret
@@file_readname_next:
	lodsb                        
	stosb                        ; store AL at address ES:(E)DI, di = di + 1
	jmp @@file_readname_start
file_readname ENDP


;; get length of the string
strlen proc
	mov di, dx 						; in the di
	mov al, '$' 					; search for endline
	mov cx, 30 						; maximum word size
	repnz scasb 					; search
	sub di, dx 						; substract to get the distance 
	sub di, 1 						; substract 1 to remove endline sign
	mov cx, di
	ret
strlen ENDP


end @@beginning 

