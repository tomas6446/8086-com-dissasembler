.model small

.stack
	MAX_BUFF 	EQU 10
.data
	bufSize				db MAX_BUFF
	about               db "Tomas Kozakas, 2 kursas, 1 grupe.", 10, 13, 'Programa disasembleris vercia masinini koda i assemblerio kalba.',  10, 13, 9, 'dis.exe mano.com kodas.asm$'	
	err_op 			    db ' - nepavyko atidaryti', 13, 10, '$'
	err_re 				db 'nepavyko skaityti', 13, 10, '$'
	
	succ_op 			db ' - sekmingai atidarytas skaitymui', 13, 10, 13, 10, '$'

	com     			db MAX_BUFF, ?, MAX_BUFF dup(0)
		comHandle    	dw ?
	asm     			db MAX_BUFF, ?, MAX_BUFF dup(0)
		asmHandle	    dw ?
	buffer 				db MAX_BUFF dup(?)

	not_recogn			db 'NOT RECOGNISED', 13, 10, '$'


	dollar          db 10, 13, '$'
.code



beginning:	
	jmp near ptr main



main proc near
    ;; open files
    call near ptr read_terminal
    
	;; read com
	mov bx, comHandle
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



dissasemble proc near
	mov bx, asmHandle	
	; 1110 OUT
	; 1000 MOV
	; 1101 XLAT
	; 1101 RCR
	; 1111 NOT
	
	test al, 10000000b
	jne not_recognised

	jmp dissasemble_end
not_recognised:
	; mov ah, 9
	; mov dx, offset not_recogn
	; int 21h

	; ;; write to file
	; call file_write

dissasemble_end:
    ret
dissasemble endp



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
	cmp ax, 3F2Fh                   ; jei nuskaityta " / ?" - 3F = '?'; 2F = ' / '
	je _about                        ; ne rastas " / ?", vadinasi reikia inputo pabaiga
 
    ;; read com file name
    lea	di, com
	call	file_readname		; perkelti is parametro i eilute source fila

    ;; read asm file name
    lea	di, asm
	call	file_readname		; perkelti is parametro i eilute source fila

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
    call near ptr open_reading
	mov asmHandle, bx      	

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
	mov ah, 3Dh                     ; atidaro faila - komandos kodas
	mov al, 0                       ; 0 - reading, 1 - writing, 2 - abu
	int 21h                         ; INT 21h / AH= 3Dh - open existing file
	jc err_open                   ; CF set on error AX = error code.
    ret
open_reading endp



open_printing PROC near 
	mov	ah, 3ch				        ; isvalo/sukuria faila - komandos kodas
	mov	cx, 0				        ; normal - no attributes
	int	21h					        ; INT 21h / AH= 3Ch - create or truncate file.
							        ;   Jei nebus isvalytas - tai perrasines senaji,
							        ;   t.y. jei pries tai buves failas ilgesnis - like simboliai isliks.
	jc	err_open
	mov	ah, 3dh				        ; atidaro faila - komandos kodas
	mov	al, 1				        ; rasymui
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
	cmp byte ptr ds:[si], 13      ; jei nera parametru
	je file_readname_end         ; tai taip, tai baigtas failo vedimas
	cmp byte ptr ds:[si], ' '     ; jei tarpas
	jne file_readname_next       ; tai praleisti visus tarpus, ir sokti prie kito parametro

file_readname_end:
	mov al, '$'                  ; irasyti gale dolleri
	stosb                        ; Store AL at address ES:(E)DI, di = di + 1
	pop ax
	ret

file_readname_next:
	lodsb                        ; uzkrauna kita simboli
	stosb                        ; Store AL at address ES:(E)DI, di = di + 1
	jmp file_readname_start
file_readname ENDP


;; write stuff to file
file_write proc
	push ax
	push cx

	call strlen

	mov ah, 40h
	int  21h

	pop ax
	pop cx
	ret
file_write ENDP


;; get length of the string
strlen proc
	mov di, dx 			; in the di
	mov al, '$' 		; search for dollar
	mov cx, MAX_BUFF 	; maximum buffer size
	repnz scasb 		; search
	sub di, dx 			; substract to get the distance 
	sub di, 1 			; substract 1 to remove dollar sign
	mov cx, di
	ret
strlen ENDP


end beginning 


