.model small

.stack
	MAX_BUFF 	EQU 10
.data
	about               db "Tomas Kozakas, 2 kursas, 1 grupe.", 10, 13, 'Programa disasembleris vercia masinini koda i assemblerio kalba.',  10, 13, 9, 'dis.exe mano.com kodas.asm$'	
	err_op 			    db ' - nepavyko atidaryti', 13, 10, '$'
	err_re 				db 'nepavyko skaityti', 13, 10, '$'
	
	succ_op 			db ' - sekmingai atidarytas skaitymui', 13, 10, 13, 10, '$'

	com     			db MAX_BUFF, ?, MAX_BUFF dup(0)
		comHandle    	dw ?
	asm     			db MAX_BUFF, ?, MAX_BUFF dup(0)
		asmHandle	    dw ?
	buffer 				db MAX_BUFF dup(?)
	
	not_recogn			db 'NOT RECOGNISED', 13, '$'
	mov_				db 'MOV$'
	rcr_				db 'RCR$'
	not_				db 'NOT$'
	out_				db 'OUT$'
	xlat_				db 'XLAT$'
	
	endline          db 13, '$'
.code

beginning:	
	jmp near ptr main



main proc near
    ;; open files
    call near ptr read_terminal
    
	;; read com
	call near ptr file_read

main_end:
	;; return to dos
    mov ax, 4c00h
    int 21h
main endp


file_read proc near
new_buffer:
	mov ah, 3Fh
	mov dx, offset buffer
	mov bx, comHandle
	mov cx, MAX_BUFF
	int 21h
	jc error_read
	
	;; compare if nothing was read
	mov cx, ax
	cmp ax, 0                   
	je read_end        

	;; point si to dx
	mov si, dx
repeat:
	;; get one byte, store it in al
	lodsb
	call dissasemble
	loop repeat

	jmp new_buffer
error_read:
    mov ah, 9h
	mov dx, offset err_re
	int 21h
read_end:
	;; close file
	mov ah, 3eh
	int 21h
    ret
file_read endp


;; write stuff to file
file_write proc near
	push cx

	;; print in standart output
	mov ah, 9
	int 21h

	;; find length of string (store length in cx)
	call strlen
	mov ah, 40h
	mov bx, asmHandle
	int  21h

	pop cx
	ret
file_write ENDP



dissasemble proc near
	; 1000 10dw mod reg r/m [poslinkis] – MOV registras <=> registras/atmintis
	; 1000 11d0 mod 0sr r/m [poslinkis] – MOV segmento registras <=> registras/atmintis
	; 1101 00vw mod 011 r/m [poslinkis] – RCR registras/atmintis, {1; CL}
	; 1111 011w mod 010 r/m [poslinkis] – NOT registras/atmintis
	; 1110 011w portas – OUT portas
	; 1110 111w – OUT
	; 1101 0111 – XLAT

	
dissasemble_end:
	ret
dissasemble endp


__mov proc
	mov dx, offset mov_
	call file_write
	mov dx, offset endline
	call file_write

	jmp dissasemble_end
endp __mov

__rcr proc
	mov dx, offset rcr_
	call file_write
	mov dx, offset endline
	call file_write

	jmp dissasemble_end
__rcr endp 

__not proc
	mov dx, offset not_
	call file_write
	mov dx, offset endline
	call file_write

	jmp dissasemble_end
__not endp

__out proc
	mov dx, offset out_
	call file_write
	mov dx, offset endline
	call file_write

	jmp dissasemble_end
 __out endp

__xlat proc
	mov dx, offset xlat_
	call file_write
	mov dx, offset endline
	call file_write

	jmp dissasemble_end
__xlat endp



read_terminal proc near
	mov ax, @data
	mov es, ax 

	;; terminal parameters start at ds:81h 
    mov si, 81h                  

    ;; compare if there is no param
	mov al, byte ptr ds:[si]      
	cmp al, 13                   
	je _about

    ;; compare if there are '/?'
	mov ax, word ptr ds:[si]
	cmp ax, 3F2Fh                   
	je _about                        
 
    ;; read com file name
    lea	di, com
	call	file_readname	

    ;; read asm file name
    lea	di, asm
	call	file_readname		

	mov ax, @data
	mov ds, ax

	;; compare if file names were read
    cmp byte ptr ds:[com], '$'   
	je _about
    cmp byte ptr ds:[asm], '$'   
    je _about

	;; open com file for reading
    mov	dx, offset com		        
	call near ptr open_reading
	mov comHandle, ax        

	;; open asm file for reading
	mov dx, offset asm
    call near ptr open_printing
	mov asmHandle, ax      	

    jmp input_end
_about:
	mov dx, offset about
	mov ah, 09h
	int 21h
    jmp main_end
succ_open:
    mov ah, 9h
    int 21h
	mov dx, offset succ_op
	int 21h
    ret
err_open:
    mov ah, 09h
	int 21h
	mov dx, offset err_op
	int 21h
    jmp main_end
input_end:
    ret
read_terminal endp



open_reading PROC near
	mov ah, 3Dh                     
	mov al, 0                      
	int 21h                         
	jc err_open                  
    ret
open_reading endp



open_printing PROC near 
	mov	ah, 3ch				        
	mov	cx, 0				        ; normal - no attributes
	int	21h					        ; INT 21h / AH= 3Ch - create or truncate file.
	jc	err_open
	mov	ah, 3dh				        
	mov	al, 1				        
	int	21h					        ; INT 21h / AH= 3Dh - open existing file.
	jc	err_open
    ret
open_printing endp



skip_spaces PROC near
skip_spaces_loop:
	cmp byte ptr ds:[si], ' '
	jne skip_spaces_end
	inc si
	jmp skip_spaces_loop

skip_spaces_end:
	ret
skip_spaces ENDP



file_readname PROC near
	push ax
	call skip_spaces

file_readname_start:
	cmp byte ptr ds:[si], 13      
	je file_readname_end         
	cmp byte ptr ds:[si], ' '     
	jne file_readname_next       

file_readname_end:
	mov al, '$'                  
	stosb                        ; Store AL at address ES:(E)DI, di = di + 1
	pop ax
	ret

file_readname_next:
	lodsb                        
	stosb                        ; Store AL at address ES:(E)DI, di = di + 1
	jmp file_readname_start
file_readname ENDP



;; get length of the string
strlen proc
	mov di, dx 			; in the di
	mov al, '$' 		; search for endline
	mov cx, 30 			; maximum word size
	repnz scasb 		; search
	sub di, dx 			; substract to get the distance 
	sub di, 1 			; substract 1 to remove endline sign
	mov cx, di
	ret
strlen ENDP


end beginning 


