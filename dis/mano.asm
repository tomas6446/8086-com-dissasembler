.model small
buferioDydis	EQU	121
BSeg SEGMENT
	ORG	100h
	ASSUME ds:BSeg, cs:BSeg, ss:BSeg
Pradzia:
	mov ax, dx
	mov al, 1
	mov byte ptr [bp + si], al
	mov al, al
	mov cl, al
	movsb
	movsw
	mov dx, 123
	rcr	ax, cl
	rcr	ax, 1
	out dx, ax
BSeg ENDS
	
END	Pradzia		