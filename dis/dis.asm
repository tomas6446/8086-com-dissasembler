; Atliko: Tomas Kozakas

.model small

.stack
	MAX_BUFF 	EQU 100
	JUMPS
.data
	about               db "Tomas Kozakas, 2 kursas, 1 grupe.", 10, 13
						db 'Programa disasembleris vercia masinini koda i assemblerio kalba.', 10, 13, 9
						db 'dis.exe mano.source kodas.txt$'

	err_op 			    db ' - nepavyko atidaryti', 13, 10, '$'
	err_re 				db 'nepavyko skaityti', 13, 10, '$'

	source     			db MAX_BUFF, ?, MAX_BUFF dup(0)
	sourceHandle    	dw ?
	destination     	db MAX_BUFF, ?, MAX_BUFF dup(0)
	destHandle			dw ?
	buffer 				db MAX_BUFF dup(?)
	
	;;; VARIABLES
	address				dw 100h
	operand				dw 0
	first_byte			db 0
	prev_byte			db 0
	mod_byte			db 0
	sr_byte				db 0

	;;; FLAGS
	shift_flag			db 0
	sr_flag				db 0
	memory_flag			db 1
	
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

	;;; INVALID 
	__INVALID			db 'invalid $'

	
	alignment9			db 9, '$'
	alignment99			db 9, 9, '$'
	string 				db 10 dup(0)
	endline          	db 13, '$'
	char				db 1, '$'

.code
@@beginning:	
	jmp main

main proc
    ;; open files 
    call read_terminal
    
	;; read source
	call readFile

@@main_end:
	;; return to dos
    mov ax, 4c00h
    int 21h
main endp

;; WRITING & READING
;##################
readFile proc 
@@new_buffer:
	mov ah, 3Fh
	mov dx, offset buffer
	mov bx, sourceHandle
	mov cx, MAX_BUFF
	int 21h
	jc @@error_read
	
	;; detect if nothing was read
	mov cx, ax
	cmp ax, 0                   
	je @@read_end        

	;; point si to dx
	mov si, dx
@@repeat:
	;; get one byte from si, store it in al
	call getByte
	mov first_byte, al
	call defineInstruction
	loop @@repeat

	;; jmp to new buffer
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
readFile endp


read_terminal proc
	mov ax, @data
	mov es, ax 

	;; terminal parameters start at ds:81h 
    mov si, 81h                          
 
    ;; read source file name
    lea	di, source
	call	read_filename	

    ;; read destination file name
    lea	di, destination
	call	read_filename		

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

;; opens file for reading
open_reading proc
	mov ah, 3Dh                     
	mov al, 0                      
	int 21h                         
	jc @@err_open                  
    ret
open_reading endp

;; opens file for writing
open_writing proc 
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
	;; if space, then skip, inc pointer 'si'
	cmp byte ptr ds:[si], ' '
	jne @@skip_spaces_end
	inc si
	jmp @@skip_spaces_loop

@@skip_spaces_end:
	ret
skip_spaces ENDP

;; read file name from termnial
read_filename PROC near
	push ax di
	call skip_spaces
@@read_name_start:
	cmp byte ptr ds:[si], 13      
	je @@read_name_end         
	cmp byte ptr ds:[si], ' '     
	jne @@read_name_next       
@@read_name_end:
	mov al, '$'                   
	;; store AL at address ES:(E)DI, di = di + 1
	stosb                       
	pop di ax
	ret
@@read_name_next:
	;; store next char in AL
	lodsb                        
	;; store AL at address ES:(E)DI, di = di + 1
	stosb                        
	jmp @@read_name_start
read_filename ENDP


writeHex proc
	push cx bx si					; save previous registers

	xor dx, dx
	mov cx,	4        				; print 4 hex digits (= 16 bits)
@@move_bits:
	rol ax, 4   					; more first 4 bits to the end
	mov dl, al
	and dl, 00001111b  				; isolate the hex digit we want to print
	cmp dl, 9
	jbe @@print_digit
	add dl, 7    					; ... (for 'A'..'F')
	add dl, '0'  					; and convert it into a character
@@print_letter:
	push ax

	mov si, offset char				; point si to char
	mov [si], dl					; put at the start of char with dollar at the end
	mov dx, si	
	call writeFile					; print character		

	pop ax
	jmp @@next
@@print_digit:   
	push ax

	xor ah, ah						; clear ah
	mov al, dl		 
	call writeNumber				; print digits
	
	pop ax
@@next:
	loop @@move_bits

	pop si bx cx
	ret
writeHex endp


writeNumber proc
	push si bx
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
	call writeFile
	pop bx si
	ret
writeNumber ENDP


;; moves address in ax and converts it into hex
writeAddress proc
	push ax
	mov ax, address				
	call writeHex					
	
	mov dx, offset __COLON
	call writeFile

	mov dx, offset alignment9
	call writeFile

	pop ax
	ret
writeAddress endp


;; write stuff to file
writeFile proc
	push ax cx bx

	;; find length of string (store length in cx)
	call strlen

	;; print in file, what is stored in dx
	mov ah, 40h
	mov bx, destHandle
	int  21h

	pop bx cx ax
	ret
writeFile ENDP
;#######################


;; saves previous byte
getByte proc
	inc address
	mov prev_byte, al
	lodsb
	mov mod_byte, al
	ret
getByte endp

;; used to get 2 bytes
getWord proc
	inc address
	inc address
	lodsw
	ret
getWord endp

;; get length of the string
strlen proc
	push di
	mov di, dx 						; in the di
	mov al, '$' 					; search for endline
	mov cx, 30 						; maximum word size
	repnz scasb 					; search
	sub di, dx 						; substract to get the distance 
	sub di, 1 						; substract 1 to remove endline sign
	mov cx, di
	pop di
	ret
strlen ENDP




;###############################
mods proc
	test mod_byte, 10000000b
	jnz	@@mod10b
	jz	@@mod00b

	@@mod10b:	
		test mod_byte, 01000000b
		jnz	@@mod11bf
		jz	@@mod10bf

		@@mod11bf:
			call d_regreg
			jmp @@ret 
		@@mod10bf:
			mov shift_flag, 2 
			mov memory_flag, 1
			call d_rmreg
			jmp @@ret

	@@mod00b:
		test mod_byte, 01000000b
		jnz	@@mod01bf
		jz	@@mod00bf

		@@mod01bf:
			mov shift_flag, 1
			mov memory_flag, 1
			call d_rmreg
			jmp @@ret
		@@mod00bf:
			mov shift_flag, 0
			mov memory_flag, 1
			call d_rmreg
			jmp @@ret
@@ret:
	ret
mods endp



memory proc
	mov dx, offset __open_br
	call writeFile

	call w
	call writeHex

	mov dx, offset __close_br
	call writeFile

	ret
memory endp


acc proc
	test first_byte, 00000001b
	jnz	@@w
	jz	@@b

	@@w:
		mov dx, offset __AX
		jmp @@returnacc
	@@b:
		mov dx, offset __AL
@@returnacc:
	call writeFile
	ret
acc endp


port proc
	test first_byte, 00001000b
	jnz	@@nport
	jz	@@port

	@@nport:
		mov dx, offset __DX
		call writeFile
		jmp @@size
	@@port:
		call getByte
		call writeNumber

		mov al, prev_byte
	@@size:
		mov dx, offset __COMMA
		call writeFile

		call acc

	ret
port endp


reg proc
	test first_byte, 00000001b
	jz	@@bytes
	
	call regw
	jmp @@returnreg
@@bytes:
	call regb
@@returnreg:
	ret
reg endp


w proc
	test first_byte, 00000001b
	jz 	@@getByte

	call getWord
	jmp @@returnw
@@getByte:
	call getByte
@@returnwb:
	ret
w endp


d_rmreg proc
	test first_byte, 00000010b
	jnz	@@reg
	jz	@@rrm

	;; first 3 bits after mod is register
	;; shift these 3 bits to right
	@@reg:
		push ax
		shr al, 3
		call reg
		pop ax
		jmp @@returnd
	@@rrm:
		call rm
@@returnd:
	ret
d_rmreg endp



d_segregw proc
	test first_byte, 00000010b
	jnz	@@seg
	jz	@@srm

	;; first 3 bits after mod is segment register
	;; shift these 3 bits to right
	@@seg:
		push ax
		shr al, 3
		call sr
		pop ax
		jmp @@returns
	@@srm:
		call regw
@@returns:
	ret
d_segregw endp



d_regreg proc
	test first_byte, 00000010b
	jnz	@@regl
	jz	@@regr

	;; first 3 bits after mod is left register
	;; shift these 3 bits to right
	@@regl:
		push ax
		shr al, 3
		call reg
		pop ax
		jmp @@returnr
	@@regr:
		call reg
@@returnr:
	ret
d_regreg endp


d_accmem proc
	test first_byte, 00000010b
	jnz	@@mem
	jz	@@acc
	
	@@acc:	
		call acc
		jmp @@returnaccmem
	@@mem:
		call memory
@@returnaccmem:
	ret
d_accmem endp



v proc
	test first_byte, 00000010b
	jnz	@@shiftcl
	jz	@@shift1

@@shiftcl:
	mov dx, offset __CL
	call writeFile
	jmp @@returnv
@@shift1:
	mov ax, 1
	call writeNumber
@@returnv:
	ret
v endp


sr proc
	cmp sr_flag, 1
	jne	@@returnseg

	test sr_byte, 00000010b
	jnz @@s1s
	jz	@@s0s

@@s1s:
	test sr_byte, 00000001b
	jnz	@@s11s
	jz	@@s10s

	@@s11s:
		mov dx, offset __DS
		call writeFile
		jmp @@continueseg
	@@s10s:
		mov dx, offset __SS
		call writeFile
		jmp @@continueseg
@@s0s:
	test sr_byte, 00000001b
	jnz	@@s01s
	jz	@@s00s
	
	@@s01s:
		mov dx, offset __CS
		call writeFile
		jmp @@continueseg
	@@s00s:
		mov dx, offset __ES
		call writeFile
		jmp @@continueseg
	
@@continueseg:
	mov dx, offset __COLON
	call writeFile
	mov sr_flag, 0
@@returnseg:
	ret
sr endp


rm proc
@@ptr:
	test first_byte, 00000001b
	jnz	@@word_ptr
	jz	@@byte_ptr
@@word_ptr:
	mov dx, offset __WORD_PTR
	call writeFile
	call sr
	jmp @@rm_start
@@byte_ptr:
	mov dx, offset __BYTE_PTR
	call writeFile
	call sr
	jmp @@rm_start

	
@@rm_start:
	mov dx, offset __open_br
	call writeFile

	test al, 00000100b
	jnz	@@r1m
	jz 	@@r0m

	@@r1m:
		test al, 00000010b
		jnz @@r11m
		jz	@@r10m

		@@r11m:
			test al, 00000001b
			jnz @@r111m
			jz 	@@r110m
			
			@@r111m:
				mov dx, offset __BX
				call writeFile
				jmp @@continue
			@@r110m:
				cmp memory_flag, 1
				je @@memory

				mov dx, offset __BP
				call writeFile
				jmp @@continue
				
				@@memory:
					call writeHex
					call w

					;sub al, prev_byte

					; mov dx, offset __PLUS
					; call writeFile

					call getByte
					; call writeNumber

					jmp @@continue

		@@r10m:
			test al, 00000001b
			jnz @@r101m
			jz 	@@r100m

			@@r101m:
				mov dx, offset __DI
				call writeFile
				jmp @@continue
			@@r100m:
				mov dx, offset __SI
				call writeFile
				jmp @@continue
	@@r0m:
		test al, 00000010b 
		jnz @@r01m
		jz	@@r00m

		@@r01m:
			test al, 00000001b
			jnz @@r011m
			jz	@@r000m

			@@r011m:
				mov dx, offset __BP
				call writeFile
				mov dx, offset __plus
				call writeFile
				mov dx, offset __DI
				call writeFile
				jmp @@continue
			@@r010m:
				mov dx, offset __BP
				call writeFile
				mov dx, offset __plus
				call writeFile
				mov dx, offset __SI
				call writeFile
				jmp @@continue
		@@r00m:
			test al, 00000001b
			jnz @@r001m
			jz	@@r000m

			@@r001m:
				mov dx, offset __BX
				call writeFile
				mov dx, offset __plus
				call writeFile
				mov dx, offset __DI
				call writeFile
				jmp @@continue
			@@r000m:
				mov dx, offset __BP
				call writeFile
				mov dx, offset __plus
				call writeFile
				mov dx, offset __SI
				call writeFile
				jmp @@continue
@@continue:
	cmp shift_flag, 0
	je @@putbr

@@shift:
	call shift
@@putbr:
	mov dx, offset __close_br
	call writeFile
	mov shift_flag, 0
@@returnrm:
	ret
rm endp


shift proc
	mov dx, offset __PLUS
	call writeFile

	push ax
	xor ax, ax
	cmp shift_flag, 2
	je	@@gWord
	cmp shift_flag, 1
	je @@gByte
@@gByte:
	call getByte
	jmp @@write
@@gWord:
	call getWord	
@@write:
	call writeNumber
	pop ax
	ret
shift endp


regw proc
	test al, 00000100b
	jnz @@w1w
	jz	@@w0w
	
@@w1w:
	test al, 00000010b
	jnz @@w11w
	jz	@@w10w

	@@w11w:
		test al, 00000001b
		jnz	@@w111w
		jz 	@@w110w

		@@w111w:
			mov dx, offset __DI
			call writeFile
			jmp @@returnw
		@@w110w:
			mov dx, offset __SI
			call writeFile
			jmp @@returnw

	@@w10w:
		test al, 00000001b
		jnz @@w101w
		jz 	@@w100w

		@@w101w:
			mov dx, offset __BP
			call writeFile
			jmp @@returnw
		@@w100w:
			mov dx, offset __SP
			call writeFile
			jmp @@returnw

@@w0w:
	test al, 00000010b
	jnz @@w01w
	jz	@@w00w

	@@w01w:
		test al, 00000001b
		jnz @@w011w
		jz 	@@w010w

		@@w011w:
			mov dx, offset __BX
			call writeFile
			jmp @@returnw
		@@w010w:
			mov dx, offset __DX
			call writeFile
			jmp @@returnw

	@@w00w:
		test al, 00000001b
		jnz	@@w001w
		jz	@@w000w

		@@w001w:
			mov dx, offset __CX
			call writeFile
			jmp @@returnw
		@@w000w:
			mov dx, offset __AX
			call writeFile
			jmp @@returnw
	
@@returnw:
	ret
regw endp


regb proc
	test al, 00000100b
	jnz @@b1b
	jz	@@b0b
	
@@b1b:
	test al, 00000010b
	jnz @@b11b
	jz	@@b10b

	@@b11b:
		test al, 00000001b
		jnz	@@b111b
		jz 	@@b110b

		@@b111b:
			mov dx, offset __BH
			call writeFile
			jmp @@returnb
		@@b110b:
			mov dx, offset __DH
			call writeFile
			jmp @@returnb
	@@b10b:
		test al, 00000001b
		jnz @@b101b
		jz 	@@b100b

		@@b101b:
			mov dx, offset __CH
			call writeFile
			jmp @@returnb
		@@b100b:
			mov dx, offset __AH
			call writeFile
			jmp @@returnb
@@b0b:
	test al, 00000010b
	jnz @@b01b
	jz	@@b00b

	@@b01b:
		test al, 00000001b
		jnz @@b011b
		jz 	@@b010b

		@@b011b:
			mov dx, offset __BL
			call writeFile
			jmp @@returnb
		@@b010b:
			mov dx, offset __DL
			call writeFile
			jmp @@returnb
	@@b00b:
		test al, 00000001b
		jnz	@@b001b
		jz	@@b000b
		@@b001b:
			mov dx, offset __CL
			call writeFile
			jmp @@returnb
		@@b000b:
			mov dx, offset __AL
			call writeFile
			jmp @@returnb
	
@@returnb:
	ret
regb endp



defineInstruction proc
	test al, 10000000b
	jnz @@c1c
	jz 	@@c0c
	
@@c1c:
	test al, 01000000b
	jnz @@c11c
	jz  @@c10c

	@@c11c:
		test al, 00100000b
		jnz @@c111c
		jz	@@c110c

		@@c111c:
			test al, 00010000b
			jnz @@c1111c
			jz 	@@c1110c

			@@c1111c: 
				test al, 00001000b
				jnz	@@c11111c
				jz	@@c11110c

				@@c11111c:
					jmp @@invalid
				@@c11110c:
					test al, 00000100b
					jnz	@@c111101c
					jz	@@c111100c

					@@c111101c:
						test al, 00000010b
						jnz @@c1111011c
						jz 	@@c1111010c

						@@c1111011c:
							call getByte
							test al, 00100000b
							jnz @@c1111011?c??1c
							jz	@@c1111011?c??0c

							@@c1111011?c??1c:
								jmp @@invalid
							@@c1111011?c??0c:
								test al, 00010000b
								jnz @@c1111011?c??01c
								jz	@@c1111011?c??00c
							
								@@c1111011?c??01c:
									test al, 00001000b
									jnz @@c1111011?c??011c
									jz	@@c1111011?c??010c

									@@c1111011?c??011c:
										jmp @@invalid
									@@c1111011?c??010c:
										call notc
										jmp @@endline
									
								@@c1111011?c??00c:
									jmp @@invalid
						@@c1111010c:
							jmp @@invalid
					@@c111100c:
						jmp @@invalid

			@@c1110c:
				test al, 00001000b
				jnz	@@c11101c
				jz	@@c11100c

				@@c11101c:
					test al, 00000100b
					jnz	@@c111011c
					jz	@@c111010c
					
					@@c111011c:
						test al, 00000010b
						jnz	@@c1110111c
						jz	@@c1110110c
						
						@@c1110111c:
							call outc
							jmp @@endline
						@@c1110110c:
							jmp @@invalid

					@@c111010c:
						jmp @@invalid
				@@c11100c:
					test al, 00000100b
					jnz	@@c111001c
					jz	@@c111000c

					@@c111001c:
						test al, 00000010b
						jnz	@@c1110011c
						jz	@@c1110000c

						@@c1110011c:
							call outc
							jmp @@endline
						@@c1110000c:
							jmp @@invalid
					@@c111000c:
						jmp @@invalid
		@@c110c:
			test al, 00010000b
			jnz @@c1101c
			jz 	@@c1100c

			@@c1101c:
				test al, 00001000b
				jnz	@@c11011c
				jz	@@c11010c

				@@c11011c:
					jmp @@invalid
				@@c11010c:
					test al, 00000100b
					jnz	@@c110101c
					jz	@@c110100c

					@@c110101c:
						test al, 00000010b
						jnz	@@c1101011c
						jz 	@@c1101010c

						@@c1101011c:
							test al, 00000001b
							jnz	@@c11010111c
							jz 	@@c11010100c

							@@c11010111c:
								call xlatc
								jmp @@endline
							@@c11010100c:
								jmp @@invalid

						@@c1101010c:
							jmp @@invalid
					@@c110100c:
						call getByte

						test al, 00100000b
						jnz	@@c110100??c??1c
						jz	@@c110100??c??0c

						@@c110100??c??1c:
							jmp @@invalid
						@@c110100??c??0c:
							test al, 00010000b
							jnz	@@c110100??c??01c
							jz	@@c110100??c??00c

							@@c110100??c??01c:
								test al, 00001000b
								jnz	@@c110100??c??011c
								jz	@@c110100??c??010c

								@@c110100??c??011c:
									call rcrc
									jmp @@endline
								@@c110100??c??010c:
									jmp @@invalid
							@@c110100??c??00c:
								jmp @@invalid
			@@c1100c:
				test al, 00001000b
				jnz	@@c11001c
				jz	@@c11000c

				@@c11001c:
					jmp @@invalid

				@@c11000c:
					test al, 00000100b
					jnz	@@c110001c
					jz	@@c110000c

					@@c110001c:
						test al, 00000010b
						jnz	@@c1100011c
						jz	@@c1100010c

						@@c1100011c:
							call getByte
							test al, 001000000b
							jnz	@@c1100011?c??1c
							jz	@@c1100011?c??0c

							@@c1100011?c??1c:
								jmp @@invalid
							@@c1100011?c??0c:
								test al, 00010000b
								jnz	@@c1100011?c??01c
								jz	@@c1100011?c??00c

								@@c1100011?c??01c:
									jmp @@invalid
								@@c1100011?c??00c:
									test al, 00001000b
									jnz	@@c1100011?c??001c 
									jz	@@c1100011?c??000c

									@@c1100011?c??001c:
										jmp @@invalid
									@@c1100011?c??000c:
										call mov_rmimm
										jmp @@endline
						@@c1100010c:
							jmp @@invalid
					@@c110000c:
						jmp @@invalid	

	@@c10c:
		test al, 00100000b
		jnz @@c101c
		jz	@@c100c

		@@c101c:
			test al, 00010000b
			jnz @@c1011c
			jz 	@@c1010c

			@@c1011c:
				call mov_regimm
				jmp @@endline
		
			@@c1010c:
				test al, 00001000b
				jnz	@@c10101c
				jz	@@c10100c

				@@c10101c:
					jmp @@invalid
				@@c10100c:
					test al, 00000100b
					jnz	@@c101001c
					jz	@@c101000c

					@@c101001c:
						test al, 0000010b
						jnz	@@c1010011c
						jz	@@c1010010c

						@@c1010011c:
							jmp @@invalid

						@@c1010010c:
							call movsc
							jmp @@endline

					@@c101000c:
						call mov_memacc
						jmp @@endline
		@@c100c:
			test al, 00010000b
			jnz @@c1001c
			jz 	@@c1000c

			@@c1001c:
				jmp @@invalid
			@@c1000c:
				test al, 00001000b
				jnz @@c10001c
				jz	@@c10000c

				@@c10001c:
					test al, 00000100b
					jnz	@@c100011c
					jz	@@c100010c

					@@c100011c:
						call mov_segrm
						jmp @@endline
					@@c100010c:
						call mov_regrm
						jmp @@endline

				@@c10000c:
					jmp @@invalid
@@c0c:
	test al, 01000000b
	jnz	@@c01c
	jz	@@c00c
	@@c01c:
		jmp @@returnc
	@@c00c:
		test al, 00100000b
		jnz @@c001c
		jz 	@@c000c

		@@c001c:
			test al, 00000100b
			jnz @@c001??1c
			jz 	@@c001??0c
			@@c001??1c:
				test al, 00000010b
				jnz	@@c001??11c
				jz	@@c001??10c

				@@c001??11c:
					call seg_label
					jmp @@returnc
				@@c001??10c:
					jmp @@returnc
			@@c001??0c:
				jmp @@returnc
		@@c000c:
			jmp @@returnc
@@invalid:
	call writeAddress
	mov dx, offset __INVALID
	call writeFile
@@endline:
	mov dx, offset endline
	call writeFile
@@returnc:	
	ret
defineInstruction endp
;########################################


;#######################################
movsc proc
	call writeAddress

	; 1010 010w – movsb; movsw
	test first_byte, 00000001b
	jz	@@movsb

	mov dx, offset __MOVSW
	jmp @@returnmovs
@@movsb:
	mov dx, offset __MOVSB	
@@returnmovs:
	call writeFile	
	ret
movsc endp


xlatc proc
	call writeAddress

	; 1101 0111 – XLAT
	mov dx, offset __XLAT
	call writeFile

	ret
xlatc endp


rcrc proc
	call writeAddress

	; 1101 00vw mod 011 r/m [poslinkis] – RCR registras/atmintis, {1; CL}
	mov dx, offset __RCR
	call writeFile
	mov mod_byte, al
	call mods
	mov dx, offset __COMMA
	call writeFile
	call v

	ret
rcrc endp

outc proc
	call writeAddress

	; 1110 011w portas – out portas
	; 1110 111w – out
	mov dx, offset __OUT
	call writeFile
	
	call port

	ret
outc endp


notc proc
	call writeAddress

	; 1111 011w mod 010 r/m [poslinkis] – NOT registras/atmintis
	mov dx, offset __NOT
	call writeFile
	xor first_byte, 00000010b
	call mods

	ret
notc endp


mov_regrm proc
	call writeAddress
	call getByte

	; 1000 10dw mod reg r/m [poslinkis] – MOV registras <=> registras/atmintis
	mov dx, offset __MOV
	call writeFile
	call mods
	mov dx, offset __COMMA
	call writeFile
	xor first_byte, 00000010b
	call mods
	
	ret
mov_regrm endp


mov_segrm proc
	call writeAddress
	call getByte

	; 1000 11d0 mod 0sr r/m [poslinkis] – MOV segment registras <=> registras/atmintis
	mov dx, offset __MOV
	call writeFile
	call d_segregw
	mov dx, offset __COMMA
	call writeFile
	xor first_byte, 00000010b
	call d_segregw

	ret
mov_segrm endp


mov_regimm proc
	call writeAddress

	;1011 wreg bojb [bovb] – MOV registras <- betarpiškas operandas
	mov dx, offset __MOV
	call writeFile

	shr first_byte, 3
	call reg
	mov dx, offset __COMMA
	call writeFile
	
	call w
	call writeNumber

	ret
mov_regimm endp


mov_rmimm proc
	call writeAddress
	
	;1100 011w mod 000 r/m [poslinkis] bojb [bovb] – MOV registras/atmintis <- betarpiškas
	mov dx, offset __MOV
	call writeFile

	xor first_byte, 00000010b
	mov mod_byte, al
	call mods
	mov dx, offset __COMMA
	call writeFile
	call w
	call writeNumber

	ret
mov_rmimm endp


mov_memacc proc
	call writeAddress

	;1010 000w adrjb adrvb - MOV akumuliatorius <- atmintis
	;1010 001w adrjb adrvb - MOV atmintis <- akumuliatorius
	mov dx, offset __MOV
	call writeFile

	call d_accmem
	mov dx, offset __COMMA
	call writeFile
	xor first_byte, 00000010b
	call d_accmem

	ret
mov_memacc endp


;; *ONLY WORKS on MOV segm reg <=> r/m
seg_label proc
	mov sr_flag, 1
	mov sr_byte, al
	;; change shr for specific command
	shr sr_byte, 3 
	ret
seg_label endp

;###############################


end @@beginning 

