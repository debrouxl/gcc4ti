#ifndef __GDRAW
#define __GDRAW

#include <default.h>

/* Begin Auto-Generated Part */
#ifndef __HAVE_GraphModes
#define __HAVE_GraphModes
enum GraphModes{GR_FUNC=1,GR_PAR=2,GR_POL=3,GR_SEQ=4,GR_3D=5,GR_DE=6};
#endif
#define GD_Circle ({__need_in_use_bit;_rom_call(void,(void),176);})
#define GD_Contour ({__need_in_use_bit;_rom_call(void,(void),17D);})
#define GD_Eraser ({__need_in_use_bit;_rom_call(void,(void),17A);})
#define GD_HVLine ({__need_in_use_bit;_rom_call(void,(short),178);})
#define GD_Line ({__need_in_use_bit;_rom_call(void,(void),177);})
#define GD_Pen ({__need_in_use_bit;_rom_call(void,(void),179);})
#define GD_Select ({__need_in_use_bit;_rom_call(void,(void),17C);})
#define GD_Text ({__need_in_use_bit;_rom_call(void,(void),17B);})
#define GR3_paint3d ({__need_in_use_bit;_rom_call(void,(void),1FF);})
#define GR3_xyToWindow ({__need_in_use_bit;_rom_call(void,(float,float,float*,__pshort,__pshort),200);})
#define GZ_Box ({__need_in_use_bit;_rom_call(void,(void),22B);})
#define GZ_Center ({__need_in_use_bit;_rom_call(void,(void),22C);})
/* End Auto-Generated Part */

#endif
