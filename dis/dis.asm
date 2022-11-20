; Atliko: Tomas Kozakas

.model small

.stack
	MAX_BUFF 	EQU 10
	JUMPS
.data
	about               db "Tomas Kozakas, 2 kursas, 1 grupe.", 10, 13, 'Programa disasembleris vercia masinini koda i assemblerio kalba.',  10, 13, 9, 'dis.exe mano.source kodas.destination$'	
	err_op 			    db ' - nepavyko atidaryti', 13, 10, '$'
	err_re 				db 'nepavyko skaityti', 13, 10, '$'
	
	succ_op 			db ' - sekmingai atidarytas skaitymui', 13, 10, 13, 10, '$'

	source     			db MAX_BUFF, ?, MAX_BUFF dup(0)
	sourceHandle    	dw ?
	destination     	db MAX_BUFF, ?, MAX_BUFF dup(0)
	destHandle			dw ?
	buffer 				db MAX_BUFF dup(?)
	
	address				dw 100H
	hex 				db "0123456789ABCDEF$"
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
	;; index
	__SI				db 'si$'
	__DI				db 'di$'
	;; segment
	__ES				db 'es$'
	__SS				db 'ss$'
	__DS				db 'ds$'
	;;; OPERATORS
	__PLUS				db ' + $'
	__MINUS				db ' - $'
	__OPEN_BR			db '[$'
	__CLOSE_BR			db ']$'
	__COMMA				db ', $'
	__COLON				db ':$'

	;;; invalid COMMAND
	__INVALID			db 'invalid $'

	__WORD				db '(word) $'
	__BYTE				db '(byte) $'

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
	lodsb
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



; printHex proc
; 	push si ax bx cx dx
; 	mov bx, 10h
; 	add di, cx
; 	dec di
; @@loop:
; 	mov dx, 0000h
; 	div bx
; 	lea si, hex
; 	add si, dx
; 	mov dl, [si]
; 	mov [di], dl
; 	dec di
; 	loop @@loop

; 	mov dx, di
; 	call file_write

; 	pop si ax bx cx dx
; 	ret
; printHex endp



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
	mov al, 00000010b
	jnz @@s00000010s
	jz	@@s00000000s

@@s00000010s:
	test al, 00000001b
	jnz	@@s00000011sf
	jz	@@s00000010sf

	@@s00000011sf:
		mov dx, offset __ES
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
		mov dx, offset __SS
		call file_write
		jmp @@returnseg
	@@s00000000sf:
		mov dx, offset __DS
		call file_write
		jmp @@returnseg
@@returnseg:
	ret
segm endp



rm proc
	test al, 00000010b
	jnz @@r00000000m

	mov dx, offset __BYTE_PTR
	call file_write
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
	; 1000 10dw mod reg r/m [poslinkis] – MOV registras <=> registras/atmintis
	; 1000 11d0 mod 0sr r/m [poslinkis] – MOV segmento registras <=> registras/atmintis
	; 1010 000w adrjb adrvb – MOV akumuliatorius <= atmintis
	; 1010 001w adrjb adrvb – MOV atmintis <= akumuliatorius
	; 1010 010w – movsb; movsw
	; 1011 wreg bojb [bovb] – MOV registras <= betarpiškas operandas
	; 1100 011w mod 000 r/m [poslinkis] bojb [bovb] – MOV registras/atmintis <= betarpiškas
	; 1101 00vw mod 011 r/m [poslinkis] – rcr registras/atmintis, {1; CL}
	; 1111 011w mod 010 r/m [poslinkis] – not registras/atmintis
	; 1110 011w portas – out portas
	; 1110 111w – out

	test al, 10000000b
	jnz @@c10000000c
	jz 	@@returnc
	
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
							mov prev_byte, al
							lodsb 	; get one more byte
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
				jmp @@out

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
						mov prev_byte, al
						lodsb
						test al, 00100000b
						jnz	@@c11010000c00100000c
						jz	@@c11010000c00000000c

						@@c11010000c00100000c:
							jmp @@invalid
						@@c11010000c00000000c:
							test al, 00010000b
							jnz	@@c11010000c00010000cf
							jz	@@c11010000c00000000cf

							@@c11010000c00010000cf:
								test al, 00001000b
								jnz	@@c11010000c00011000cff
								jz	@@c11010000c00010000cff

								@@c11010000c00011000cff:
									jmp @@rcr
								@@c11010000c00010000cff:
									jmp @@invalid
							@@c11010000c00000000cf:
								jmp @@invalid
			@@c11000000cfff:
				test al, 00001000b
				jnz	@@c11001000cffff
				jz	@@c11000000cffff

				@@c11000000cffff:
					jmp @@mov_mem_imm
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
						jmp @@mov_seg_reg
					@@c10001000cfffff:
						test al, 00000010b
						jnz @@c10001010cffffff
						jz	@@c10001000cffffff

						@@c10001010cffffff:
							jmp @@mov_mem_reg
						@@c10001000cffffff:
							jmp @@mov_reg_mem

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
			jmp @@invalid

@@mov_reg_mem:
	test al, 00000001b
	jnz	@@mov_reg_mem_w 
	jz	@@mov_reg_mem_b	

	@@mov_reg_mem_w:

	@@mov_reg_mem_b:

	jmp @@returnc
@@segm_change:
	
	jmp @@returnc
@@mov_mem_imm:
	
	jmp @@returnc
@@mov_reg_imm:
	;1011 wreg bojb [bovb] – MOV registras <- betarpiškas operandas
	mov dx, offset __MOV
	call file_write

	test al, 00001000b
	jnz @@mov_reg_imm_w 
	jz	@@mov_reg_imm_b 

	@@mov_reg_imm_w:
		call regw
		jmp @@comma
	@@mov_reg_imm_b:
		call regb
	@@comma:
		mov dx, offset __COMMA
		call file_write
	
	lodsb
	call setupOperand
	
	jmp @@returnc
@@mov_acc_mem:
	

	jmp @@returnc
@@mov_mem_acc:
	
	jmp @@returnc
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
@@mov_seg_reg:

	jmp @@returnc
@@mov_mem_reg:
	mov dx, offset __MOV
	call file_write

	mov prev_byte, al
	lodsb
	mov mod_byte, al

	push ax
	shr al, 3
	call setupArg

	mov dx, offset __COMMA
	call file_write
	
	pop ax
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
		call print_number
		jmp @@returnc
@@out:
	mov dx, offset __OUT
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
	mov dx, offset endline
	call file_write
@@returnc:
	mov dx, offset endline
	call file_write
	ret
setupComm endp


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
			jmp @@ret

	@@mod00b:
		test mod_byte, 01000000b
		jnz	@@mod01bf
		jz	@@mod00bf

		@@mod01bf:
			jmp @@ret
		@@mod00bf:
			call rm
			jmp @@ret
@@ret:
	ret
setupArg endp


dissasemble proc near
	; inc	address
	; mov dx, address
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
    call near ptr open_printing
	mov destHandle, ax      	

    jmp @@input_end
@@about:
	mov dx, offset about
	mov ah, 09h
	int 21h
    jmp @@main_end
@@succ_open:
    mov ah, 9h
    int 21h
	mov dx, offset succ_op
	int 21h
    ret
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



open_printing PROC near 
	mov	ah, 3ch				        
	mov	cx, 0				        
	int	21h					        
	jc	@@err_open
	mov	ah, 3dh				        
	mov	al, 1				        
	int	21h					        
	jc	@@err_open
    ret
open_printing endp



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


;; convert binary to dec
setupOperand proc
	push ax bx cx
	mov ax, 1
	mov bx, 2

	mov     cx, 10
    cwd               ;This puts zero in DX because AX holds a positive number
@@mul:
	mul bx

    dec     cx
    jnz     @@mul	
	pop cx bx ax

	call print_number
	ret
setupOperand endp


;; get length of the string
strlen proc
	mov di, dx 					; in the di
	mov al, '$' 				; search for endline
	mov cx, 30 					; maximum word size
	repnz scasb 				; search
	sub di, dx 					; substract to get the distance 
	sub di, 1 					; substract 1 to remove endline sign
	mov cx, di
	ret
strlen ENDP

print_number proc
	push si
	mov si, offset string + 9    ; nurodyti i paskutini simboli
	mov byte ptr [si], '$'       ; kelti pabaigos simboli
	
	mov bx, 10                   ; dalinti reiksmes is 10
@@asc2:
	xor dx, dx                   ; isvalyti dx
	div bx                       ; dx bus liekana is ax / bx
	add dx, '0'                  ; prideti 48, kad paruosti simboli isvedimui
	dec si                       ; imam kita simboli (decrementinam pointeri)
	mov [si], dl                 ; padedam skaitmeni
	
	cmp ax, 0                    ; jei skaicius isdalintas
	jz @@print                    ; tai einam i pabaiga
	jmp @@asc2                     ; kitu atveju imam kita skaitmeni
@@print:
	mov dx, si
	call file_write
	pop si
	ret
print_number ENDP

end @@beginning 

