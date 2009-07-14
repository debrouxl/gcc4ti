/*
   dasm-tigcc - Disassembler for TI calculators

   Copyright (C) 2000-2002 Thomas Nussbaumer
   Copyright (C) 2005 Lionel Debroux
   Copyright (C) 2007 Kevin Kofler

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

/* The code in this file is based on ttdasm, but with the VTI-derived code
   removed. Calls to VTI functions have been replaced with calls to the
   equivalent TiEmu functions. */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "main.h"
#include "tt.h"
#include "ti68k_def.h"
#include "images.h"
#include "timem.h"
#include "mem.h"
#include "romcalls.h"
#include "disasm.h"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    fprintf(USAGE_OUT, "dasm-tigcc 1.00 - Disassembler for TI calculators\n\n"                \
                       "Copyright (C) 2000-2002 Thomas Nussbaumer\n"                          \
                       "Copyright (C) 2005 Lionel Debroux\n"                                  \
                       "Copyright (C) 2007 Kevin Kofler\n"                                    \
                       "Portions (GDB) Copyright (C) 1986-2007 Free Software Foundation\n"    \
                       "Portions (TiEmu) Copyright (C) 2000-2007 Romain Lievin et. al.\n"     \
                       "Portions (ld-tigcc) Copyright (C) 2002-2004 Sebastian Reichelt\n\n"   \
                       "This program is free software; you may redistribute it under the\n"   \
                       "terms of the the GNU General Public License. There is NO warranty;\n" \
                       "not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n"\
                       "Usage: dasm-tigcc [flags] [<infile>]\n\n"                             \
                       "       -o <number>       ... use given offset as start point\n"       \
                       "       -l <bytes>        ... disassemble number of given bytes\n"     \
                       "       -r <romfile>      ... disassemble given rom file\n"            \
                       "       -f                ... show ROM function table\n"               \
                       "       -name             ... show possible ROM calls\n"               \
                       "       -virtual <number> ... use additionally virtual offset\n"       \
                       "       -nolabels         ... use no labels in output\n"               \
                       "       -raw              ... treat file as raw binary\n"              \
                       "       -vti              ... treat file as VTI save file\n"           \
                       "       -stub             ... prints additionally information of\n"    \
                       "                             kernel based programs\n\n"               \
                       "       disassembles a TI89/TI92plus NOSTUB executable,\n"             \
                       "       a TI89/TI92plus raw dump,parts of a given ROM file or\n"       \
                       "       a VTI save file.\n\n"                                          \
                       "       NOTE1: the offset and the length can be specified as\n"        \
                       "              hex numbers, too (for example: -o 0x4c)\n\n"            \
                       "       NOTE2: offsets are not treated as pure file offsets,\n"        \
                       "              but are handled depending on the target.\n"             \
                       "              Within executables offset 0 starts at file offset\n"    \
                       "              88 which is just behind the header and the length\n"    \
                       "              information. Offsets within ROM files maps\n"           \
                       "              directly to the real addresses within the calculator.\n\n");
}


int            calc_type;
unsigned char* rom;
unsigned char* ram;
int            initialPC;
int            initIntTabOfs;
unsigned char* mem[256];
unsigned char  unused[0x10000];
int            label_parse = 0;
int            no_labels   = 0;
uint32_t       rom_base;
uint32_t       rom_size;
uint32_t       ram_size;
uint32_t       entry_point;
static uint32_t abs_labels_start = 0;
static uint32_t abs_labels_len = 0;

#define GET_WORD(m)   ((unsigned short)((((m)[0]) << 8) + ((m)[1])))

uint32_t* label_list       = NULL;
int   label_count      = 0;
int   labels_allocated = 0;

#define LABEL_ALLOC_STEP 10

//=============================================================================
// adds a label to the list
//=============================================================================
int AddLabel(uint32_t address) {
    int i = -1;

    if (label_count != 0) {
        for (i=0;i<label_count;i++) if (label_list[i] == address) return 1;
    }

    if (label_count == labels_allocated) {
        uint32_t* tmp = (uint32_t*)realloc(label_list,(labels_allocated+LABEL_ALLOC_STEP)*sizeof(uint32_t));
        if (!tmp) {
            label_parse = 0;
            return 0;
        }
        label_list = tmp;
        labels_allocated+=LABEL_ALLOC_STEP;
    }
    label_list[label_count++] = address;
    return 1;
}


//=============================================================================
// adds a label to the list
//=============================================================================
char label_name[200];
char* GetLabel(uint32_t address) {
    int i = -1;

    if (no_labels) return NULL;

    if (label_count != 0) {
        for (i=0;i<label_count;i++) if (label_list[i] == address) break;
    }
    if (i != label_count && i != -1) {
        sprintf(label_name,"__L%lX",(unsigned long)label_list[i]);
        return label_name;
    }
    return NULL;
}


void PrintAddress (int32_t reladdr, uint32_t absaddr, int flags, void *stream,
                   void (*fprintf_f) (void *, const char *, ...),
                   void (*fputs_f) (const char *, void *))
{
    if ((flags & 4) && !(absaddr >= abs_labels_start && absaddr < abs_labels_start + abs_labels_len)) {
        fprintf_f(stream, "0x%lX", (unsigned long) absaddr);
        return;
    }
    if (label_parse) {
        AddLabel(absaddr);
    } else {
        char *s = GetLabel(absaddr);
        if (!s) {
            if (flags & 1) {
                if (reladdr < 0)
                    fprintf_f(stream, ".-0x%X", (int) -reladdr);
                else
                    fprintf_f(stream, ".+0x%X", (int) reladdr);
            } else {
                if (reladdr < 0)
                    fprintf_f(stream, "-0x%X", (int) -reladdr);
                else
                    fprintf_f(stream, "0x%X", (int) reladdr);
            }
        } else {
            fprintf_f(stream, "%s", s);
            if (flags & 2)
                fputs_f("-.", stream);
        }
    }
}


static void PrintROMInfo(IMG_INFO *info) {
    switch (info->calc_type) {
      case TI92: printf("TI92  "); break;
      case TI89: printf("TI89  "); break;
      case TI92p: printf("TI92p  "); break;
      case V200: printf("V200  "); break;
      case TI89t: printf("TI89t  "); break;
    }
    if (info->calc_type==TI92 && info->rom_base==0x20) printf("INTERNAL ");
    if (!info->has_boot) printf("UPDATE ");
    if (!info->flash==FLASH_ROM) printf("FLASHROM ");
}


//=============================================================================
// frees buffers used for ROM
//=============================================================================
void CleanupROM() {
   if (rom) free(rom);
   if (ram) free(ram);
   rom = 0;
}


//=============================================================================
// load a ROM file
//=============================================================================
int LoadROM(char* filename) {
    IMG_INFO       info;

    rom  = (unsigned char*)malloc(4096*1024+3);
    ram  = (unsigned char*)malloc(256*1024+3);

    if (!rom || !ram) {
        CleanupROM();
            fprintf(stderr,"ERROR: cannot allocate memory for ROM\n");
        return 0;
    }

    memset(rom,0xff,4096*1024+3);

    if (ti68k_is_a_rom_file(filename)) {
        if (ti68k_get_rom_infos(filename,&info,1)) return 0;
        printf("/*[ ");PrintROMInfo(&info);printf("]*/\n");
    } else if (ti68k_is_a_tib_file(filename)) {
        if (ti68k_get_tib_infos(filename,&info,1)) return 0;
        printf("/*[ ");PrintROMInfo(&info);printf("]*/\n");
    } else if (ti68k_is_a_img_file(filename)) {
        if (ti68k_load_image(filename,&info)) return 0;
        printf("/*[ ");PrintROMInfo(&info);printf("]*/\n");
    } else {
        fprintf(stderr,"ERROR: invalid ROM filename %s (neither .ROM nor .TIB nor .IMG)\n",filename);
        CleanupROM();
        return 0;
    }
    calc_type = info.calc_type;

    if (info.has_boot)
        memcpy(rom,info.data,info.size);
    else
        ti68k_convert_tib_to_image(&info,rom);
    rom_base = info.rom_base << 16;
    rom_size = info.size;
    ram_size = (rom_size==1024*1024)?0x100000:0x200000;

    if (info.flash==FLASH_ROM) {
        initialPC     = rd_long(&rom[0x1208c]);
        initIntTabOfs = 0x12088;
    }
    else {
        initialPC     = rd_long(&rom[4]);
        initIntTabOfs = 0;
    }

    free(info.data);

    printf("/*[initialPC     = 0x%x (%d)]*/\n",initialPC,initialPC);
    printf("/*[initIntTabOfs = 0x%x (%d)]*/\n\n",initIntTabOfs,initIntTabOfs);

    return 1;
}


//=============================================================================
// initialize memory pointers
//=============================================================================
void InitMemoryPointers() {
    memset(ram, 0, 0x40000);
    memcpy(ram, rom+initIntTabOfs, 256);
    memset(unused, 0x14, 0x10000);
    hw_mem_init();
}


static const char* offsetname_ptr;

//=============================================================================
// helper function for disassembling engine
//=============================================================================
void Offset2Name(int addr) {
    offsetname_ptr = (romcalls_is_loaded() && !(addr & 3)
                      && addr < (NMAX_ROMCALLS << 2))
                     ? romcalls_get_name(addr >> 2) : 0;
}


//=============================================================================
// disassemble given buffer until length bytes are processed
// the given offset is used just for the address output in the comment
//=============================================================================
void Disassemble(unsigned char* mem,int length,int used_offset,
                 int virtual_offset,int show_possible)
{
    char buffer[1024];
    int  start = 0;
    int  bytes;
    int  i;

    //printf("starting ...\n");

    if (!no_labels) {
        label_parse=1;
        while (start < length) {
            bytes = Dasm68000(mem+start,buffer,start+used_offset+virtual_offset);

            if (!label_parse) {
                printf("ERROR: OUT-OF-MEMORY - cannot add that much levels\n");
                return;
            }
            start+=bytes;
        }

        start       = 0;
        label_parse = 0;
    }

    printf("asm(\"\n");

    while (start < length) {
        if (!no_labels) {
            char* s = GetLabel(start+used_offset+virtual_offset);
            if (s) printf("%s:\n",s);
        }
        offsetname_ptr = 0;
        bytes = Dasm68000(mem+start,buffer,start+used_offset+virtual_offset);
        printf("%s",buffer);
        for (i=strlen(buffer);i<30;i++) printf(" ");
        printf("/* [0x%X (%u)] ",start+used_offset+virtual_offset,start+used_offset+virtual_offset);
        for (i=0;i<bytes;i++) printf("%02x ",*(mem+start+i));
        for (i=bytes;i<6;i++) printf("   ");
        if (show_possible) {
            if (offsetname_ptr) printf("?%s? */\n",offsetname_ptr);
            else                printf("*/\n");
        }
        else {
            printf("*/\n");
        }
        start+=bytes;
    }
    printf("\");\n");
}


//=============================================================================
// outputs header details of kernel-based programs or libraries
//=============================================================================
void OutputStubDetails(unsigned char* mem) {
    int i,offset,count;
    printf("******** STUB-HEADER INFO *********\n");

    if ((!strncmp((char*)(mem+4),"68kP",4)) || (!strncmp((char*)(mem+4),"68kL",4))) {
        printf("signature:        %c%c%c%c\n",*(mem+4),*(mem+5),*(mem+6),*(mem+7));

        printf("relcount:         %d\n",GET_WORD(mem+8));
        printf("offset comment:   0x%04x\n",GET_WORD(mem+0xa));
        printf("offset main:      0x%04x\n",GET_WORD(mem+0xc));
        printf("offset exit:      0x%04x\n",GET_WORD(mem+0xe));
        printf("compatiblity:     0x%04x\n",GET_WORD(mem+0x10));
        printf("offset BSS:       0x%04x\n",GET_WORD(mem+0x14));
        offset = GET_WORD(mem+0x16);
        printf("offset export:    0x%04x\n",offset);
        printf("offset RAM table: 0x%04x\n",GET_WORD(mem+0x18));
        printf("********* EXPORT TABLE ************\n");
        count = GET_WORD(mem+offset);
        for (i=0;i<count;i++) printf("export@%04x:      0x%04x\n",i,GET_WORD(mem+offset+2+i*2));
    }
    else {
        printf("--- not found --- (missing 68kP or 68kL)\n");
    }
}


//=============================================================================
// its a main ...
//=============================================================================
int main(int argc,char *argv[]) {
    char*          infile = 0;
    FILE*          ifp;
    char           sig[9];
    int            length;
    unsigned char* data;
    int            n;
    int            given_offset            = -1;
    int            given_length            = -1;
    char*          given_rom               = 0;
    int            show_functions          = 0;
    int            show_possible_functions = 0;
    int            virtual_offset          = 0;
    int            raw                     = 0;
    int            vti                     = 0;
    int            stub                    = 0;

    //---------------------------------------------------------------
    // check for too less arguments
    //---------------------------------------------------------------
    if (argc < 2) {
        PrintUsage();
        return 1;
    }

    printf("/* Disassembled with dasm-tigcc\n\n"
           "   Copyright (C) 2000-2002 Thomas Nussbaumer\n"
           "   Copyright (C) 2005 Lionel Debroux\n"
           "   Copyright (C) 2007 Kevin Kofler\n"
           "   Portions (GDB) Copyright (C) 1986-2007 Free Software Foundation\n"
           "   Portions (TiEmu) Copyright (C) 2000-2007 Romain Lievin et. al.\n"
           "   Portions (ld-tigcc) Copyright (C) 2002-2004 Sebastian Reichelt */\n\n");

    //---------------------------------------------------------------
    // parse commandline arguments
    //---------------------------------------------------------------
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-o")) {
            // handle offset commandline tag
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                if (!strncmp(argv[n+1],"0x",2)) {
                    if (sscanf(&argv[n+1][2],"%x",&given_offset) != 1) {
                        PrintUsage();
                        return 1;
                    }
                }
                else {
                    if (sscanf(argv[n+1],"%d",&given_offset) != 1) {
                        PrintUsage();
                        return 1;
                    }
                }
                if (given_offset<0) {
                    fprintf(stderr,"ERROR: offset cannot be negative\n");
                    return 1;
                }
                n++;
            }
        }
        else if (!strcmp(argv[n], "-virtual")) {
            // handle virtual offset commandline tag
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                if (!strncmp(argv[n+1],"0x",2)) {
                    if (sscanf(&argv[n+1][2],"%x",&virtual_offset) != 1) {
                        PrintUsage();
                        return 1;
                    }
                }
                else {
                    if (sscanf(argv[n+1],"%d",&virtual_offset) != 1) {
                        PrintUsage();
                        return 1;
                    }
                }
                if (virtual_offset<0) {
                    fprintf(stderr,"ERROR: offset cannot be negative\n");
                    return 1;
                }
                n++;
            }
        }
        else if (!strcmp(argv[n], "-l")) {
            // handle length commandline tag
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                if (!strncmp(argv[n+1],"0x",2)) {
                    if (sscanf(&argv[n+1][2],"%x",&given_length) != 1) {
                        PrintUsage();
                        return 1;
                    }
                }
                else {
                    if (sscanf(argv[n+1],"%d",&given_length) != 1) {
                        PrintUsage();
                        return 1;
                    }
                }
                if (given_length<0) {
                    fprintf(stderr,"ERROR: length cannot be negative\n");
                    return 1;
                }
                n++;
            }
        }
        else if (!strcmp(argv[n], "-r")) {
            // handle romfile commandline tag
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                given_rom = argv[n+1];
                n++;
            }
        }
        else if (!strcmp(argv[n], "-f")) {
            // handle show ROM functions commandline tag
            show_functions = 1;
        }
        else if (!strcmp(argv[n], "-vti")) {
            // handle show ROM functions commandline tag
            vti = 1;
            if (raw || given_rom) {
                fprintf(stderr,"ERROR: cannot use -vti with -r or -raw\n");
                return 1;
            }
        }
        else if (!strcmp(argv[n], "-stub")) {
            // shows additionally informations about kernel based programs
            stub = 1;
        }
        else if (!strcmp(argv[n], "-raw")) {
            // handle show ROM functions commandline tag
            raw = 1;
            if (vti || given_rom) {
                fprintf(stderr,"ERROR: cannot use -raw with -r or -vti\n");
                return 1;
            }
        }
        else if (!strcmp(argv[n], "-name")) {
            // handle show possible ROM functions commandline tag
            show_possible_functions = 1;
        }
        else if (!strcmp(argv[n], "-nolabels")) {
            // handle show possible ROM functions commandline tag
            no_labels = 1;
        }
        else if (argv[n][0] == '-') {
            fprintf(stderr,"ERROR: invalid option %s",argv[n]);
            return 1;
        }

        else if (!infile) infile = argv[n];
        else {
            PrintUsage();
            return 1;
        }
    }

    //---------------------------------------------------------------
    // some parameter validation checks
    //---------------------------------------------------------------
    if (!infile && !given_rom) {
        fprintf(stderr,"ERROR: neither file nor ROM is given\n");
        return 1;
    }

    if (infile && given_rom) {
        fprintf(stderr,"ERROR: cannot use file and ROM at the same time\n");
        return 1;
    }

    if (show_functions && infile) {
        fprintf(stderr,"WARNING: cannot show ROM functions (only possible with given ROM)\n");
    }


    //---------------------------------------------------------------
    // load ROM file if user had supplied a ROM filename
    //---------------------------------------------------------------
    if (given_rom) {
        if (!LoadROM(given_rom)) {
            fprintf(stderr,"/*[program terminated due to ROM loading error]*/\n");
            return 1;
        }
        InitMemoryPointers();
        romcalls_load(0);
    } else romcalls_load(1);

    //----------------------------------------------------
    // treat ROM disassembling
    //----------------------------------------------------
    if (given_rom) {
        if (show_functions) {
            uint32_t addr;
            int size;
            int i;

            romcalls_get_table_infos(&addr, (uint32_t *)&size);
            for(i = 0; i < size; i++) {
                const char *name = romcalls_get_name(i);
                addr = romcalls_get_addr(i);
                printf("%06x (%07d) %s\n",addr,addr,name);
            }
            CleanupROM();
            return 0;
        }


        if (given_offset == -1 || given_length == -1) {
            fprintf(stderr,"ERROR: please specify offset and length\n");
            CleanupROM();
            return 1;
        }

        if (!(data = malloc(given_length+20))) {
            fprintf(stderr,"ERROR: cannot allocate %d bytes\n",given_length+20);
            CleanupROM();
            return 1;
        }

        for (n=0;n<given_length;n++) data[n] = hw_get_byte(given_offset+n);

        abs_labels_start = given_offset + virtual_offset;
        abs_labels_len = given_length;

        Disassemble(data,given_length,given_offset,virtual_offset,show_possible_functions);
        if (label_list) free(label_list);
        CleanupROM();
        free(data);
        return 0;
    }

    //----------------------------------------------------
    // if we'll come here user wants to disassemble a file
    //----------------------------------------------------
    entry_point = virtual_offset;

    if (!(ifp = fopen(infile,"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",infile);
        if (given_rom) CleanupROM();
        return 1;
    }

    if (!vti && !raw) {
        memset(sig,0,9);
        if (fread(sig,1,8,ifp) < 8) {
            fprintf(stderr,"ERROR: read error\n");
            fclose(ifp);
            if (given_rom) CleanupROM();
            return 1;
        }

        if (strcmp(sig,SIGNATURE_TI89) && strcmp(sig,SIGNATURE_TI92P)) {
            fprintf(stderr,"ERROR: neither TI89 nor TI92p executable\n");
            fclose(ifp);
            if (given_rom) CleanupROM();
            return 1;
        }
    }

    fseek(ifp,0,SEEK_END);
    length = ftell(ifp);

    if (vti) {
        length -= 0x100;
        if (given_offset != -1 && given_offset >= 256000) {
            fprintf(stderr,"ERROR: given offset is too large (%d >= 256000)\n",given_offset);
            fclose(ifp);
            if (given_rom) CleanupROM();
            return 1;
        }
        if (length < 256000+0x100) {
            fprintf(stderr,"ERROR: given file no valid VTI save file (too short)\n");
            fclose(ifp);
            if (given_rom) CleanupROM();
            return 1;
        }
    }
    else {
        if (!raw) {
            length -= 90; // 86 bytes for header + 2 bytes for length + 2 bytes for trailing checksum
        }
        if (given_offset != -1 && given_offset >= length) {
            fprintf(stderr,"ERROR: given offset is too large (%d >= %d)\n",given_offset,length);
            fclose(ifp);
            if (given_rom) CleanupROM();
            return 1;
        }

        if (given_length != -1 && given_length >= length) {
            fprintf(stderr,"ERROR: given length is too large (%d >= %d)\n",given_length,length);
            fclose(ifp);
            if (given_rom) CleanupROM();
            return 1;
        }
    }

    rewind(ifp);
    if (!raw) {
        if (vti) fseek(ifp,0x100,SEEK_SET);
        else     fseek(ifp,88,SEEK_SET);
    }

    if (!(data = malloc(length+20))) {
        fprintf(stderr,"ERROR: cannot allocate %d bytes\n",length+20);
        fclose(ifp);
        if (given_rom) CleanupROM();
        return 1;
    }

    memset(data,0,length+20);
    if (fread(data,1,length,ifp) < (size_t) length) {
        fprintf(stderr,"ERROR: read error\n");
        fclose(ifp);
        return 1;
    }
    fclose(ifp);

    if (given_length != -1) length = given_length;
    if (given_offset == -1) given_offset = 0;

    if (stub) OutputStubDetails(data+given_offset);

    Disassemble(data+given_offset,length,given_offset,virtual_offset,show_possible_functions);
    if (label_list) free(label_list);

    if (given_rom) CleanupROM();
    free(data);
    return 0;
}

