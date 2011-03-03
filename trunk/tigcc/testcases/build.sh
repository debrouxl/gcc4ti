#!/bin/sh
tigcc -Wall -W -Wwrite-strings -O2 -mregparm=5 -fomit-frame-pointer -ffunction-sections -fdata-sections -fmerge-all-constants --optimize-code --cut-ranges --reorder-sections --merge-constants -fmerge-all-constants -Wa,--all-relocs -o sorts sorts.c
tigcc -Wall -W -Wwrite-strings -Os -mregparm=5 -fomit-frame-pointer -ffunction-sections -fdata-sections -fmerge-all-constants --optimize-code --cut-ranges --reorder-sections --merge-constants -fmerge-all-constants -Wa,--all-relocs -o mul64 multiply_64.c
