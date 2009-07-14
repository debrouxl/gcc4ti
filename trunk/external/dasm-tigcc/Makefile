ifeq ($(OSTYPE), msys)
EXEEXT = .exe
else
EXEEXT =
endif

DASM_TIGCC = dasm-tigcc$(EXEEXT)

CC = gcc
CFLAGS = -s -Os -fno-exceptions
LDFLAGS = -s -fno-exceptions
WARN_CFLAGS = -W -Wall -Wwrite-strings
DEFINES = 

RM = rm
RMFLAGS = -f

COMPILE_C = $(CC) -c $(CFLAGS) $(ARCHFLAGS) $(WARN_CFLAGS) $(DEFINES)

HEADERS = *.h int_def.inc
OBJECTS = xmalloc.o integers.o rwfile.o filetypes.o files9x.o filesxx.o timem.o hwpm.o images.o romcalls.o er_codes.o mem89.o mem89tm.o mem92.o mem92p.o memv2.o mem.o fpudasm.o m68k-opc.o m68k-dis.o dis-buf.o dis-init.o disassemble.o disasm.o main.o

.PHONY: all clean

all: $(DASM_TIGCC)

romcalls.o: romcalls.c romcalls.inc $(HEADERS)
	$(COMPILE_C) romcalls.c -o romcalls.o

%.o: %.c $(HEADERS)
	$(COMPILE_C) $< -o $@

$(DASM_TIGCC): $(OBJECTS)
	$(CC) $(LDFLAGS) $(ARCHFLAGS) $(OBJECTS) -o $@

clean:
	-$(RM) $(RMFLAGS) $(OBJECTS) $(DASM_TIGCC)
