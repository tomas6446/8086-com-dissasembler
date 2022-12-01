.model small
buferioDydis	EQU	121
BSeg SEGMENT
	ORG	100h
	ASSUME ds:BSeg, cs:BSeg, ss:BSeg
Pradzia:
	xlat
	not ax
	mov ax, dx
	mov cl, 123
	mov dx, 123
	mov word ptr[di], 123

	rcr al, 3
	mov word ptr[bp+si+5000], dx
	hlt
	mov byte ptr[si], dl
	mov word ptr[bp+di+6000], dx
	mov byte ptr[bp+di+12], dl
	mov word ptr[bp+di+6000], dx
	mov word ptr[bp+di+12], dx
	mov ax, [bx+si] 
	mov es:[bx+si], dl
	mov ax, cs:[bx+si] 
	mov ds:[si], ax
	mov ss:[si+5123], 1000
	

BSeg ENDS

	
END	Pradzia		