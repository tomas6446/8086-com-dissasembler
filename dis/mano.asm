.model small

buferioDydis	EQU	121

BSeg SEGMENT
	ORG	100h
	ASSUME ds:BSeg, cs:BSeg, ss:BSeg

Pradzia:
	mov ax, dx
	rcr ax, 1
	mov cl, cl
	mov cl, 12
	mov bx, 123
BSeg ENDS

END	Pradzia		