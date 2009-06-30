CC	= gcc

FORCEDCFLAGS = -funsigned-char -fno-exceptions -D__NOPROTO
CFLAGS = -Os -s

SRC := A68kmain.c Opcodes.c Operands.c Adirect.c A68kmisc.c Symtab.c Codegen.c

OBJ	= $(SRC:%.c=%.o)

EXE	= A68k

all: $(EXE)

.c.o:
	$(CC) $(FORCEDCFLAGS) $(CFLAGS) -c $< -o $@

$(EXE):\
	$(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(OBJ)

$(OBJ): A68kdef.h A68kglb.h protos.h

clean:; rm -f $(OBJ) $(EXE) core
