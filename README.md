# com-dissasembler
8086-dissasembler

# MOV
1000 10dw mod reg r/m [shift] – MOV register <=> register/memory\
1000 11d0 mod 0sr r/m [shift] – MOV segment register  <=> register/memory\
1010 000w adrjb adrvb – MOV accumulator <= memory\
1010 001w adrjb adrvb – MOV memory <= accumulator\
1010 010w – MOVSB; MOVSW\
1011 wreg bojb [bovb] – MOV register <= operand\
1100 011w mod 000 r/m [shift] bojb [bovb] – MOV register/memory <= operand

# RCR
1101 00vw mod 011 r/m [shift] – RCR register/memory, {1; CL}

# NOT
1111 011w mod 010 r/m [shift] – NOT register/memory

# OUT
1110 011w port – OUT port
1110 111w – OUT

# XLAT
1101 0111 – XLAT

# Key words
bojb –  operand least significant byte\
[bovb] – operand most significant byte, which is not mandatory\
[shift] – shift, depends on mod value can be 1 or 2 bytes, or not exist at all\
d - direction {0 = r/m <- reg; 1 = reg <- r/m}\
v - push size {0 = 1 bit; 1 = cl}\
w - operand size {0 = 1 byte; 1 = 2 bytes}\


# Run in DoxBox
"c dis" inside of the directiory\
dis.exe mano.com kodas.asm
