|*cbscanf() implementation for TIGCCLIB
|WARNING: This implementation might accept broken format strings (for size
|         reasons). For example: "%1h2d" == "%h12d".
|Copyright (C) Kevin Kofler, 2002-2003

/*Prototypes:
  typedef CALLBACK short (*__cbscanf_get_Callback_t) (void *param);
  typedef CALLBACK void (*__cbscanf_unget_Callback_t) (short c, void *param);
  short cbscanf(register __cbscanf_get_Callback_t getfun asm("a0"),
                register __cbscanf_unget_Callback_t ungetfun asm("a1"),
                register void *param asm("a2"),
                register const char *format asm("a3"), ...);
  short vcbscanf(register __cbscanf_get_Callback_t getfun asm("a0"),
                 register __cbscanf_unget_Callback_t ungetfun asm("a1"),
                 register void *param asm("a2"),
                 register const char *format asm("a3"),
                 register va_list arglist asm("a4"));
  */

.text
.xdef vcbscanf
vcbscanf:
movem.l %d3-%d7/%a2-%a4/%a6,-(%a7)
bra.s __vcbscanf_entry_point

.xdef cbscanf
cbscanf:
movem.l %d3-%d7/%a2-%a4/%a6,-(%a7)
lea.l 4+9*4(%a7),%a4
__vcbscanf_entry_point:
clr.w %d4 |number of bytes read from input
clr.w %d0 |result (number of tokens read in)
__cbscanf_next:
move.b (%a3)+,%d1 |current format character
beq.s __cbscanf_return
cmp.b #'%',%d1
beq __cbscanf_conversion
cmp.b #32,%d1
beq.s __cbscanf_whitespace
cmp.b #13,%d1
bhi.s 0f
cmp.b #9,%d1
bcc.s __cbscanf_whitespace
0:
__cbscanf_literal:
|literal character
bsr.s __cbscanf_getc
cmp.w #-1,%d2
beq.s __cbscanf_return
cmp.b %d1,%d2
beq.s __cbscanf_next
__cbscanf_return:
|return EOF if EOF occured with no conversions done
tst.w %d0
bne.s 45f
addq.w #1,%d2
bne.s 45f
moveq.l #-1,%d0
45:
movem.l (%a7)+,%d3-%d7/%a2-%a4/%a6
rts

__cbscanf_getc:
movem.l %d0-%d1/%a0-%a1,-(%a7)
pea.l (%a2)
jsr (%a0)
addq.l #4,%a7
move.w %d0,%d2
addq.w #1,%d4
movem.l (%a7)+,%d0-%d1/%a0-%a1
rts

__cbscanf_ungetc:
cmp.w #-1,%d2 |don't unget EOF
beq.s 46f
movem.l %d0-%d2/%a0-%a1,-(%a7)
pea.l (%a2)
move.w %d2,-(%a7)
jsr (%a1)
addq.l #6,%a7
movem.l (%a7)+,%d0-%d2/%a0-%a1
46:
subq.w #1,%d4
rts

__cbscanf_whitespace:
addq.l #1,%a3
move.b (%a3),%d1
cmp.b #32,%d1
beq.s __cbscanf_whitespace
cmp.b #13,%d1
bhi.s 1f
cmp.b #9,%d1
bcc.s __cbscanf_whitespace
1:
clr.w %d1
__cbscanf_whitespace2:
addq.w #1,%d1 |count whitespace chars
bsr.s __cbscanf_getc
cmp.b #32,%d2
beq.s __cbscanf_whitespace2
cmp.b #13,%d2
bhi.s 2f
cmp.b #9,%d2
bcc.s __cbscanf_whitespace2
2:
subq.w #1,%d1 |last char is not whitespace
beq.s __cbscanf_return |no whitespace = error
bsr.s __cbscanf_ungetc
bra __cbscanf_next

__cbscanf_conversion_skip_whitespace:
bsr.s __cbscanf_getc
cmp.b #32,%d2
beq.s __cbscanf_conversion_skip_whitespace
cmp.b #13,%d2
bhi.s 12f
cmp.b #9,%d2
bcc.s __cbscanf_conversion_skip_whitespace
12:
bra.s __cbscanf_ungetc |bsr+rts = bra

__cbscanf_conversion:
moveq.l #0,%d3 |flags
clr.w %d5 |field width
3:
move.b (%a3)+,%d1 |current format character
cmp.b #'*',%d1
bne.s 4f
bset.l #0,%d3
bra.s 3b
4:
cmp.b #'h',%d1
bne.s 5f
btst.l #2,%d3
bne __cbscanf_return
bset.l #1,%d3
bra.s 3b
5:
cmp.b #'l',%d1
bne.s 6f
btst.l #1,%d3
bne __cbscanf_return
bset.l #2,%d3
bra.s 3b
6:

|read field width
cmp.b #'0',%d1
bcs.s 9f
cmp.b #'9',%d1
bhi.s 9f
subi.b #'0',%d1
ext.w %d1
mulu.w #10,%d5
add.w %d1,%d5
bra.s 3b
9:
move.w %d5,%d6

cmp.b #'%',%d1
beq __cbscanf_literal

|now, the next char must be a conversion
cmp.b #'i',%d1
bne.s __cbscanf_conversion_not_i
moveq.l #0,%d7
btst.l #0,%d3
bne.s 27f
move.l (%a4)+,%a6
27:
bsr __cbscanf_conversion_skip_whitespace
__cbscanf_conversion_i_next:
bsr __cbscanf_getc
cmp.b #'-',%d2
beq.s __cbscanf_conversion_i_neg
cmp.b #173,%d2
beq.s __cbscanf_conversion_i_neg
cmp.b #'0',%d2
bne.s __cbscanf_conversion_d_continue_as_u
subq.w #1,%d6
beq __cbscanf_conversion_u_finish
bsr __cbscanf_getc
cmp.b #'X',%d2
beq.s 28f
cmp.b #'x',%d2
bne __cbscanf_conversion_i_continue_as_o
28:
subq.w #1,%d6
beq __cbscanf_conversion_u_finish
bsr __cbscanf_getc
bra __cbscanf_conversion_i_continue_as_x
__cbscanf_conversion_i_neg:
bchg.l #3,%d3
subq.w #1,%d6
beq.s __cbscanf_conversion_u_finish
bra.s __cbscanf_conversion_i_next
__cbscanf_conversion_not_i:

cmp.b #'d',%d1
bne.s __cbscanf_conversion_not_d
moveq.l #0,%d7
btst.l #0,%d3
bne.s 20f
move.l (%a4)+,%a6
20:
bsr __cbscanf_conversion_skip_whitespace
__cbscanf_conversion_d_next:
bsr __cbscanf_getc
cmp.b #'-',%d2
beq.s 21f
cmp.b #173,%d2
bne.s __cbscanf_conversion_d_continue_as_u
21:
bchg.l #3,%d3
subq.w #1,%d6
beq.s __cbscanf_conversion_u_finish
bra.s __cbscanf_conversion_d_next
__cbscanf_conversion_not_d:

cmp.b #'u',%d1
bne.s __cbscanf_conversion_not_u
moveq.l #0,%d7
btst.l #0,%d3
bne.s 17f
move.l (%a4)+,%a6
17:
bsr __cbscanf_conversion_skip_whitespace
__cbscanf_conversion_u_next:
bsr __cbscanf_getc
__cbscanf_conversion_d_continue_as_u:
cmp.b #'0',%d2
bcs.s __cbscanf_conversion_u_unget
cmp.b #'9',%d2
bhi.s __cbscanf_conversion_u_unget
subi.b #'0',%d2
ext.w %d2
ext.l %d2
add.l %d7,%d7
move.l %d7,-(%a7)
lsl.l #2,%d7
add.l (%a7)+,%d7
add.l %d2,%d7
subq.w #1,%d6
bne.s __cbscanf_conversion_u_next
bra.s __cbscanf_conversion_u_finish
__cbscanf_conversion_u_unget:
bsr __cbscanf_ungetc
__cbscanf_conversion_u_finish:
btst.l #0,%d3
bne __cbscanf_conversion_check
btst.l #3,%d3
beq.s 19f
neg.l %d7
19:
btst.l #2,%d3
bne.s 18f
move.w %d7,(%a6)
bra __cbscanf_conversion_check
18:
move.l %d7,(%a6)
bra __cbscanf_conversion_check
__cbscanf_conversion_not_u:

cmp.b #'x',%d1
beq.s 7f
cmp.b #'X',%d1
bne.s __cbscanf_conversion_not_x
7:
__cbscanf_conversion_hexadecimal:
moveq.l #0,%d7
btst.l #0,%d3
bne.s 22f
move.l (%a4)+,%a6
22:
bsr __cbscanf_conversion_skip_whitespace
__cbscanf_conversion_x_next:
bsr __cbscanf_getc
__cbscanf_conversion_i_continue_as_x:
cmp.b #'0',%d2
bcs.s __cbscanf_conversion_u_unget
cmp.b #'9',%d2
bhi.s 23f
subi.b #'0',%d2
bra.s 25f
23:
cmp.b #'A',%d2
bcs.s __cbscanf_conversion_u_unget
cmp.b #'F',%d2
bhi.s 24f
subi.b #'A'-10,%d2
bra.s 25f
24:
cmp.b #'a',%d2
bcs.s __cbscanf_conversion_u_unget
cmp.b #'f',%d2
bhi.s __cbscanf_conversion_u_unget
subi.b #'a'-10,%d2
25:
ext.w %d2
ext.l %d2
lsl.l #4,%d7
add.l %d2,%d7
subq.w #1,%d6
bne.s __cbscanf_conversion_x_next
bra __cbscanf_conversion_u_finish
__cbscanf_conversion_not_x:

cmp.b #'p',%d1
bne.s __cbscanf_conversion_not_p
bclr.l #1,%d3
bset.l #2,%d3 |p=lx
bra.s __cbscanf_conversion_hexadecimal
__cbscanf_conversion_not_p:

cmp.b #'o',%d1
bne.s __cbscanf_conversion_not_o
bsr __cbscanf_conversion_skip_whitespace
moveq.l #0,%d7
btst.l #0,%d3
bne.s 26f
move.l (%a4)+,%a6
26:
bsr __cbscanf_conversion_skip_whitespace
__cbscanf_conversion_o_next:
bsr __cbscanf_getc
__cbscanf_conversion_i_continue_as_o:
cmp.b #'0',%d2
bcs __cbscanf_conversion_u_unget
cmp.b #'7',%d2
bhi __cbscanf_conversion_u_unget
subi.b #'0',%d2
ext.w %d2
ext.l %d2
lsl.l #3,%d7
add.l %d2,%d7
subq.w #1,%d6
bne.s __cbscanf_conversion_o_next
bra __cbscanf_conversion_u_finish
__cbscanf_conversion_not_o:

cmp.b #'f',%d1
beq.s 8f
cmp.b #'e',%d1
beq.s 8f
cmp.b #'g',%d1
beq.s 8f
cmp.b #'E',%d1
bne __cbscanf_conversion_not_f
8:
bsr __cbscanf_conversion_skip_whitespace
tst.w %d5 |default/maximum is 29 chars (not 100% ANSI compliant, but who cares?)
beq.s 30f
cmp.l #29,%d5
bls.s 31f
30:
moveq.l #29,%d5
moveq.l #29,%d6
31:
lea.l -30(%a7),%a7 |buffer
move.l %a7,%a6
__cbscanf_conversion_f_next:
bsr __cbscanf_getc
cmp.w #-1,%d2
beq.s __cbscanf_conversion_f_unget
cmp.b #'.',%d2
beq.s 32f
cmp.b #'0',%d2
bcs.s __cbscanf_conversion_f_unget
cmp.b #'9',%d2
bls.s 32f
cmp.b #149,%d2
beq.s 32f
cmp.b #173,%d2
bne.s __cbscanf_conversion_f_unget
32:
move.b %d2,(%a6)+
subq.w #1,%d6
bne.s __cbscanf_conversion_f_next
bra.s __cbscanf_conversion_f_add_nul
__cbscanf_conversion_f_unget:
bsr __cbscanf_ungetc
__cbscanf_conversion_f_add_nul:
clr.b (%a6)+
movem.l %d0-%d2/%a0-%a1,-(%a7)
pea.l 5*4(%a7)
jsr atof
addq.l #4,%a7
cmp.l #0x7fffaa00,%d0
bne.s 33f
tst.l %d1
bne.s 33f
tst.w %d2
beq.s __cbscanf_conversion_f_nan
33:
btst.l #0,%d3
bne.s 34f
move.l (%a4)+,%a6
move.l %d0,(%a6)+
move.l %d1,(%a6)+
move.w %d2,(%a6)
34:
movem.l (%a7)+,%d0-%d2/%a0-%a1
lea.l 30(%a7),%a7
bra __cbscanf_conversion_check
__cbscanf_conversion_f_nan:
movem.l (%a7)+,%d0-%d2/%a0-%a1
lea.l 30(%a7),%a7
bra __cbscanf_return
__cbscanf_conversion_not_f:

cmp.b #'s',%d1
bne.s __cbscanf_conversion_not_s
btst.l #0,%d3
bne.s 10f
move.l (%a4)+,%a6
10:
bsr __cbscanf_conversion_skip_whitespace
__cbscanf_conversion_s_next:
bsr __cbscanf_getc
cmp.w #-1,%d2
beq.s __cbscanf_conversion_s_unget
cmp.b #32,%d2
beq.s __cbscanf_conversion_s_unget
cmp.b #13,%d2
bhi.s 13f
cmp.b #9,%d2
bcc.s __cbscanf_conversion_s_unget
13:
btst.l #0,%d3
bne.s 14f
move.b %d2,(%a6)+
14:
subq.w #1,%d6
bne.s __cbscanf_conversion_s_next
bra.s __cbscanf_conversion_s_add_nul
__cbscanf_conversion_s_unget:
bsr __cbscanf_ungetc
__cbscanf_conversion_s_add_nul:
btst.l #0,%d3
bne __cbscanf_conversion_check
clr.b (%a6)+
bra __cbscanf_conversion_check
__cbscanf_conversion_not_s:

cmp.b #'[',%d1
bne __cbscanf_conversion_not_set
tst.b (%a3)
beq __cbscanf_return
cmp.b #'^',(%a3)
bne.s 38f
addq.l #1,%a3
tst.b (%a3)
beq __cbscanf_return
bset.l #3,%d3
38:
btst.l #0,%d3
bne.s 35f
move.l (%a4)+,%a6
35:
__cbscanf_conversion_set_next:
bsr __cbscanf_getc
cmp.w #-1,%d2
beq.s __cbscanf_conversion_set_unget
|check if in set
pea.l (%a3)
tst.b (%a3)
beq.s 39f
cmp.b (%a3)+,%d2
beq.s 36f
43:
move.b (%a3)+,%d1
beq.s 39f
cmp.b #']',%d1
beq.s 39f
cmp.b #'-',%d1
bne.s 40f
move.b (%a3)+,%d1
beq.s 39f
cmp.b #']',%d1
beq.s 42f
cmp.b %d1,%d2
bhi.s 43b
cmp.b -3(%a3),%d2
bcs.s 43b
bra.s 36f
40:
cmp.b %d1,%d2
beq.s 36f
bra.s 43b
42:
cmp.b #'-',%d2
beq.s 36f
39: |not in set
movea.l (%a7)+,%a3
btst.l #3,%d3
bne.s 41f
bra.s __cbscanf_conversion_set_unget
36: |in set
movea.l (%a7)+,%a3
btst.l #3,%d3
bne.s __cbscanf_conversion_set_unget
41:
btst.l #0,%d3
bne.s 37f
move.b %d2,(%a6)+
37:
subq.w #1,%d6
bne.s __cbscanf_conversion_set_next
bra.s __cbscanf_conversion_set_add_nul
__cbscanf_conversion_set_unget:
bsr __cbscanf_ungetc
__cbscanf_conversion_set_add_nul:
addq.l #1,%a3
47:
tst.b (%a3)
beq __cbscanf_return
cmp.b #']',(%a3)+
bne.s 47b
bra __cbscanf_conversion_s_add_nul
__cbscanf_conversion_not_set:

cmp.b #'c',%d1
bne.s __cbscanf_conversion_not_c
btst.l #0,%d3
bne.s 11f
move.l (%a4)+,%a6
11:
tst.w %d5 |default is 1 char
bne.s 15f
addq.w #1,%d5
addq.w #1,%d6
15:
__cbscanf_conversion_c_next:
bsr __cbscanf_getc
cmp.w #-1,%d2
beq.s __cbscanf_conversion_check
btst.l #0,%d3
bne.s 16f
move.b %d2,(%a6)+
16:
subq.w #1,%d6
bne.s __cbscanf_conversion_c_next
bra.s __cbscanf_conversion_check
__cbscanf_conversion_not_c:

cmp.b #'n',%d1
bne.s __cbscanf_conversion_not_n
btst.l #0,%d3
bne.s __cbscanf_conversion_no_check
move.l (%a4)+,%a6
move.w %d4,(%a6)
bra.s __cbscanf_conversion_no_check
__cbscanf_conversion_not_n:

__cbscanf_conversion_check:
cmp.w %d5,%d6
beq __cbscanf_return
__cbscanf_conversion_no_check:
btst.l #0,%d3
bne.s 44f
addq.w #1,%d0 |count successful conversion
44:
bra __cbscanf_next
