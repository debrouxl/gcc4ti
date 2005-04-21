ifeq ($(OSTYPE), msys)
EXEEXT = .exe
else
EXEEXT =
endif

LD_TIGCC = ld-tigcc$(EXEEXT)
AR_TIGCC = ar-tigcc$(EXEEXT)
LINK_DLL = link.dll

ifeq ($(OSTYPE),msys)
ALL_TARGETS = $(LD_TIGCC) $(AR_TIGCC) $(LINK_DLL)
else
ALL_TARGETS = $(LD_TIGCC) $(AR_TIGCC)
endif

CC = gcc
CFLAGS = -s -Os -fno-exceptions
WARN_CFLAGS = -W -Wall -Wwrite-strings -Wpointer-arith
DEFINES = -DCOFF_SUPPORT -DAMIGAOS_SUPPORT -DTIOS_SUPPORT -DFLASH_OS_SUPPORT -DNOSTUB_DLL_SUPPORT -DFARGO_SUPPORT -DDATA_VAR_SUPPORT -DTIOS_FILE_SUPPORT -DTIOS_UPGRADE_FILE_SUPPORT -DDEBUGGING_INFO_SUPPORT -DCOFF_TIGCC_EXTENSIONS -DAMIGAOS_TIGCC_EXTENSIONS
EXE_DEFINES = -DENABLE_HELP -DENABLE_STATS -DENABLE_DUMP

RM = rm
RMFLAGS = -f

COMPILE_C = $(CC) -c $(CFLAGS) $(ARCHFLAGS) $(WARN_CFLAGS) $(DEFINES)

HEADERS = *.h formats/*.h import/*.h export/*.h bincode/*.h insert/*.h insert/model/*.h int_def.inc
AR_HEADERS = ar/*.h ar/import/*.h ar/export/*.h *.h formats/*.h
OBJECTS = main.o integers.o int_arb.o manip.o constmrg.o gcunused.o reorder.o import/import.o import/imp_coff.o import/imp_amig.o import/imp_ar.o export/export.o export/exp_def.o export/exp_os.o export/exp_tios.o export/exp_ndll.o export/exp_farg.o export/exp_data.o export/exp_dbg.o bincode/fix_m68k.o bincode/fix_tios.o bincode/fix_emu.o bincode/cutrange.o insert/ins_def.o insert/model/list.o insert/kernel.o insert/comprrlc.o insert/other.o special.o dump.o
AR_OBJECTS = ar/main.o ar/manip.o ar/import/import.o ar/import/imp_coff.o ar/import/imp_amig.o ar/export/exp_ar.o ar/dump.o integers.o
DLL_OBJECTS = main.do integers.do int_arb.do manip.do constmrg.do gcunused.do reorder.do import/import.do import/imp_coff.do import/imp_amig.do import/imp_ar.do export/export.do export/exp_def.do export/exp_os.do export/exp_tios.do export/exp_ndll.do export/exp_farg.do export/exp_data.do export/exp_dbg.do bincode/fix_m68k.do bincode/fix_tios.do bincode/fix_emu.do bincode/cutrange.do insert/ins_def.do insert/model/list.do insert/kernel.do insert/comprrlc.do insert/other.do special.do
DLL_AR_OBJECTS = ar/main.do ar/manip.do ar/import/import.do ar/import/imp_coff.do ar/import/imp_amig.do ar/export/exp_ar.do
BACKUPS = *~ format/*~ import/*~ export/*~ bincode/*~ insert/*~ insert/model/*~ ar/*~ ar/import/*~ ar/export/*~

.PHONY: all clean clean_backups

all: $(ALL_TARGETS)

ar/main.o: ar/main.c ar/main_opt.inc $(AR_HEADERS)
	$(COMPILE_C) $(EXE_DEFINES) $< -o $@

ar/%.o: ar/%.c $(AR_HEADERS)
	$(COMPILE_C) $(EXE_DEFINES) $< -o $@

ar/%.do: ar/%.c $(AR_HEADERS)
	$(COMPILE_C) -DTARGET_DLL $< -o $@

main.o: main.c main_opt.inc main_vbs.inc $(HEADERS)
	$(COMPILE_C) $(EXE_DEFINES) $< -o $@

%.o: %.c $(HEADERS)
	$(COMPILE_C) $(EXE_DEFINES) $< -o $@

%.do: %.c $(HEADERS)
	$(COMPILE_C) -DTARGET_DLL $< -o $@

$(LD_TIGCC): $(OBJECTS)
	$(CC) $(CFLAGS) $(ARCHFLAGS) $(OBJECTS) -o $@

$(AR_TIGCC): $(AR_OBJECTS)
	$(CC) $(CFLAGS) $(ARCHFLAGS) $(AR_OBJECTS) -o $@

$(LINK_DLL): $(DLL_OBJECTS) $(DLL_AR_OBJECTS)
	$(CC) -shared --dll $(CFLAGS) $(ARCHFLAGS) $(DLL_OBJECTS) $(DLL_AR_OBJECTS) -o $@

clean:
	-$(RM) $(RMFLAGS) $(OBJECTS) $(AR_OBJECTS) $(DLL_OBJECTS) $(DLL_AR_OBJECTS) $(ALL_TARGETS)

clean_backups:
	-$(RM) $(RMFLAGS) $(BACKUPS)
