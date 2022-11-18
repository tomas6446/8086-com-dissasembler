.model small

buferioDydis	EQU	121

BSeg SEGMENT
	ORG	100h
	ASSUME ds:BSeg, cs:BSeg, ss:BSeg

Pradzia:
	MOV ax, dx

BSeg ENDS

END	Pradzia		