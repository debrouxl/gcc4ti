|exit function copyright (C) 2002, Kevin Kofler
|requires new exit support (__save__sp__)
|Thanks to Patrick Pélissier for the code. I have just changed it from a patch
|to an archive function, so it won't be there when not needed.

	.xdef __exit

| This file requires the startup exit support.
	.xdef __ref_all___save_all_registers_main

__exit:
	movea.l __save__sp__,%a7 |(NOT PC-relative because of programs >32 KB)
	rts
