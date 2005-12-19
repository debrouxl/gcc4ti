rm -f .brik
rm -f md5.sum
rm -f -r INSTALL
ls -A config/mh-* >tmpfiles.lst
for name in `grep -v -e "cygwin" -e "mingw" tmpfiles.lst`; do rm -f "$name"; done
rm -f config/mpw-mh-mpw
ls -A config/mt-* >tmpfiles.lst
for name in `grep -v -e "m68k" tmpfiles.lst`; do rm -f "$name"; done
rm -f -r contrib
ls -A gcc/config/ >tmpfiles.lst
for name in `grep -v -e "^i386$" -e "^m68k$" -e "README" -e "install" -e "libgcc-glibc" -e "dbx" -e "usegas" -e "x-linux" -e "host-linux" tmpfiles.lst`; do rm -f -r "gcc/config/$name"; done
ls -A gcc/config/i386/ >tmpfiles.lst
for name in `grep -v -e "x-cygwin" -e "x-mingw" -e "xm-cygwin" -e "xm-mingw" -e "host-mingw" tmpfiles.lst`; do rm -f "gcc/config/i386/$name"; done
ls -A gcc/config/m68k/ >tmpfiles.lst
for name in `grep -v -e "m68k\.c" -e "t-m68kbare" -e "m68k\.md" -e "^m68k\.h" -e "m68k-coff" -e "m68k-none" -e "m68k-protos" -e "coff\.h" -e m68k-modes -e "m68k\.opt" -e "predicates\.md" tmpfiles.lst`; do rm -f "gcc/config/m68k/$name"; done
rm -f -r gcc/fixinc
rm -f -r fixincludes
rm -f -r gcc/ginclude
rm -f -r gcc/po
rm -f -r gcc/doc
rm -f -r gcc/treelang
rm -f -r gcc/objcp
rm -f -r libcpp/po
rm -f -r libmudflap
rm -f -r libssp
rm -f -r libiberty/testsuite
rm -f -r maintainer-scripts
rm -f -r bfd/doc
rm -f -r bfd/po
rm -f bfd/hosts/*.c
rm -f bfd/hosts/*.h
rm -f -r binutils
rm -f -r etc
rm -f -r gas/doc
rm -f -r gas/po
rm -f -r gas/testsuite
rm -f -r gprof
rm -f -r include/nlm
rm -f -r include/regs
rm -f -r ld
rm -f -r opcodes/po
rm -f -r texinfo
rm -f bfd/*bout*
rm -f bfd/*aix*
rm -f bfd/*386*
rm -f bfd/*linux*
rm -f bfd/*lynx*
rm -f bfd/*cisco*
rm -f bfd/*svm68k*
rm -f bfd/*mach*
rm -f bfd/*mmo*
rm -f bfd/*news*
rm -f bfd/*osf*
rm -f bfd/*pdp*
rm -f bfd/*ptrace*
rm -f bfd/*risc*
rm -f bfd/*rs6*
rm -f bfd/*sco5*
rm -f bfd/*sun*
rm -f bfd/*trad*
rm -f bfd/*versados*
rm -f bfd/efi-*
rm -f bfd/epoc-*
rm -f bfd/irix-*
rm -f bfd/pei-*
rm -f bfd/pe-*
rm -f bfd/peX*
rm -f bfd/hp*
ls -A bfd/*aout* >tmpfiles.lst
for name in `grep -v -e "libaout.h" tmpfiles.lst`; do rm -f "$name"; done
rm -f bfd/*bsd*
rm -f bfd/*demo64*
ls -A bfd/*elf* >tmpfiles.lst
for name in `grep -v -e "elf-bfd.h" tmpfiles.lst`; do rm -f "$name"; done
rm -f bfd/*go32*
rm -f bfd/*ieee*
rm -f bfd/*mpw*
rm -f bfd/*nlm*
rm -f bfd/*ns32k*
rm -f bfd/*oasys*
rm -f bfd/*pef*
rm -f bfd/*pei*
rm -f bfd/*ppc*
rm -f bfd/*som*
rm -f bfd/*ticoff*
rm -f bfd/*vms*
rm -f bfd/*xcoff*
rm -f bfd/*xsym*
rm -f bfd/*xtensa*
ls -A bfd/coff-* >tmpfiles.lst
for name in `grep -v -e "m68k" tmpfiles.lst`; do rm -f "$name"; done
ls -A bfd/cpu-* >tmpfiles.lst
for name in `grep -v -e "m68k" tmpfiles.lst`; do rm -f "$name"; done
rm -f -r cpu
rm -f gas/*aout*
rm -f gas/*bout*
rm -f gas/*elf*
rm -f gas/config/e-*
rm -f gas/config/*aout*
rm -f gas/config/*bout*
rm -f gas/config/*vax*
rm -f gas/config/*vms*
rm -f gas/config/*mips*
rm -f gas/config/*m88k*
rm -f gas/config/*xtensa*
ls -A gas/config/obj-* >tmpfiles.lst
for name in `grep -v -e "generic" -e "obj-coff" tmpfiles.lst`; do rm -f "$name"; done
ls -A gas/config/te-* >tmpfiles.lst
for name in `grep -v -e "generic" -e "m68k" tmpfiles.lst`; do rm -f "$name"; done
ls -A gas/config/tc-* >tmpfiles.lst
for name in `grep -v -e "generic" -e "m68k" tmpfiles.lst`; do rm -f "$name"; done
rm -f include/*sim*
rm -f include/*os9*
rm -f include/*oasys*
rm -f include/*bout*
ls -A include/coff/*.h >tmpfiles.lst
for name in `grep -v -e "m68k" -e "ecoff" -e "internal" -e "external" -e "sym" tmpfiles.lst`; do rm -f "$name"; done
ls -A include/opcode/*.h >tmpfiles.lst
for name in `grep -v -e "m68k" tmpfiles.lst`; do rm -f "$name"; done
ls -A include/aout/*.h >tmpfiles.lst
for name in `grep -v -e "aout64.h" -e "ar.h" -e "ranlib.h" -e "stab_gnu.h" tmpfiles.lst`; do rm -f "$name"; done
ls -A include/elf/*.h >tmpfiles.lst
for name in `grep -v -e "common.h" -e "dwarf2.h" -e "internal.h" -e "external.h" tmpfiles.lst`; do rm -f "$name"; done
rm -f -r include/gdb
rm -f -r include/*mpw*
rm -f -r include/*ieee*
rm -f -r include/*xtensa*
rm -f opcodes/*64*
rm -f opcodes/*s390*
rm -f opcodes/*vms*
rm -f opcodes/*cgen*
rm -f opcodes/*mpw*
ls -A opcodes/*.c >tmpfiles.lst
for name in `grep -v -e "m68k" -e "disass" -e "dis-" tmpfiles.lst`; do rm -f "$name"; done
ls -A opcodes/*.h >tmpfiles.lst
for name in `grep -v -e "m68k" -e "disass" -e "dis-" -e "opintl" -e "sysdep" tmpfiles.lst`; do rm -f "$name"; done
rm -f tmpfiles.lst
