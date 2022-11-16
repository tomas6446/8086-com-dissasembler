.model small

buferioDydis	EQU	121

BSeg SEGMENT
	ORG	100h
	ASSUME ds:BSeg, cs:BSeg, ss:BSeg

Pradzia:
	MOV ax, dx
	MOV ds, ax
	RCR cl, 1
	NOT cl
	out dx, ax
	xlat 

BSeg ENDS

END	Pradzia		