#ifndef __GRAPHING
#define __GRAPHING

#include <default.h>

/* Begin Auto-Generated Part */
#ifndef __HAVE_Attrs
#define __HAVE_Attrs
enum Attrs{A_REVERSE,A_NORMAL,A_XOR,A_SHADED,A_REPLACE,A_OR,A_AND,A_THICK1,A_SHADE_V,A_SHADE_H,A_SHADE_NS,A_SHADE_PS};
#endif
#ifndef __HAVE_ESI
#define __HAVE_ESI
typedef ESQ*ESI;
#endif
#ifndef __HAVE_GraphModes
#define __HAVE_GraphModes
enum GraphModes{GR_FUNC=1,GR_PAR=2,GR_POL=3,GR_SEQ=4,GR_3D=5,GR_DE=6};
#endif
#ifndef __HAVE_HSym
#define __HAVE_HSym
typedef struct{HANDLE folder;unsigned short offset;}HSym;
#endif
#ifndef __HAVE_WINDOW_AMS1
#define __HAVE_WINDOW_AMS1
typedef struct WindowStruct_AMS1{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;}WINDOW_AMS1;
#endif
#ifndef __HAVE_WINDOW
#define __HAVE_WINDOW
typedef struct WindowStruct{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;SCR_STATE savedScrState;unsigned char Reserved[16];}WINDOW;
#endif
#ifndef __HAVE_DB3
#define __HAVE_DB3
typedef struct{long cVertices;HANDLE hVertices;long cEdges;HANDLE hEdges;HANDLE hContours;long ciVertices;HANDLE hiVertices;long ciEdges;HANDLE hiEdges;short DCM[3][3];short DCM0[3][3];short DCMhome[3][3];short cDCMangle;unsigned short nSpinDir;unsigned char nSpinSpeed;unsigned char bSpinning;short calp,salp;}DB3;
#endif
#ifndef __HAVE_EQU_DS_AMS1
#define __HAVE_EQU_DS_AMS1
typedef struct SEquDS_AMS1{WINDOW_AMS1 wMain;short focus;short cFunc;short yPos;short xOffset;short yBelow;unsigned short bAltForm;}EQU_DS_AMS1;
#endif
#ifndef __HAVE_EQU_DS
#define __HAVE_EQU_DS
typedef struct SEquDS{WINDOW wMain;short focus;short cFunc;short yPos;short xOffset;short yBelow;unsigned short bAltForm;}EQU_DS;
#endif
#ifndef __HAVE_FUNCID
#define __HAVE_FUNCID
typedef struct{unsigned char FNum;unsigned char FNum2;unsigned short LNum;unsigned short PlotIndex;unsigned char PlotDir;}FUNCID;
#endif
#ifndef __HAVE_GrFmtFlags2
#define __HAVE_GrFmtFlags2
enum GrFmtFlags2{GR_DE_CUSTOM=0x0010,GR_DE_FIELDS=0x0004,GR_DIRFLD=0x0002,GR_EULER=0x0001};
#endif
#ifndef __HAVE_GrFmtFlags
#define __HAVE_GrFmtFlags
enum GrFmtFlags{GR_SEQ_TIME=0x8000,GR_SEQ_WEB=0x4000,GR_BUILD_WEB=0x2000,GR_3dEXPAND=0x0800,GR_COORDOFF=0x0080,GR_SIMUL=0x0040,GR_GRIDON=0x0020,GR_AXESOFF=0x0010,GR_AXESBOX=0x0008,GR_LABELSON=0x0004,GR_LEAD_CURSOR=0x0002,GR_COORD_POLAR=0x0001};
#endif
#ifndef __HAVE_GrMode3dStyles
#define __HAVE_GrMode3dStyles
enum GrMode3dStyles{GR_3D_WIRE_FRAME=0,GR_3D_HIDDEN_SURFACE=1,GR_3D_CONTOUR=2,GR_3D_CONTOUR_WIRE=3,GR_3D_IMPLICIT=4};
#endif
#ifndef __HAVE_GR_MODES
#define __HAVE_GR_MODES
typedef struct{unsigned short gr_fmt_flags;unsigned char gr_xaxis;signed char gr_yaxis;unsigned short gr_fmt_flags2;unsigned char gr_3dflags;unsigned char pad;}GR_MODES;
#endif
#ifndef __HAVE_GrSides
#define __HAVE_GrSides
enum GrSides{AP_SIDE_A=0,AP_SIDE_B=1,AP_SIDE_UNKNOWN=2};
#endif
#ifndef __HAVE_GrWinFlags
#define __HAVE_GrWinFlags
enum GrWinFlags{GR_REDRAW=0x0100,GR_DIRTY=0x0080,TAB_DIRTY=0x0040,GR_ADD_TO=0x0020,GR_OPEN=0x0010,GRAPH_FOLDER=0x0008,EYE_DIRTY=0x0004,GR_SHADE_NO_PAN=0x0002,FLDPIC_DIRTY=0x0001};
#endif
#ifndef __HAVE_TABLE_WIN_VARS
#define __HAVE_TABLE_WIN_VARS
typedef struct{short last_col;short last_row;unsigned short table_xpix;unsigned short table_ypix;unsigned char curtblinc;unsigned char curtblincy;unsigned char col_first;unsigned char col_last;short row_first;short row_last;unsigned char tbfn[10];unsigned char gr_xpix;float orgtblmax;}TABLE_WIN_VARS;
#endif
#ifndef __HAVE_TableFlags
#define __HAVE_TableFlags
enum TableFlags{TBL_CONNECT_TRC=0x80,TBL_INDEP_ASK=0x40,TBL_NO_MODE_CHANGE=0x20};
#endif
#ifndef __HAVE_WinVarEnum
#define __HAVE_WinVarEnum
enum WinVarEnum{GR_XMIN=0,GR_XMAX=1,GR_XSCL=2,GR_YMIN=3,R_YMAX=4,GR_YSCL=5,GR_DELTAX=6,GR_DELTAY=7,GR_XRES=8,GR_TMIN=8,GR_T0=8,GR_TMAX=9,GR_TSTEP=10,GR_TPLOT=11,GR_DIFTOL=12,GR_ESTEP=13,GR_FLDRES=14,GR_NCURVES=15,GR_DTIME=16,GR_THETMIN=8,GR_THETMAX=9,GR_THETSTEP=10,GR_XGRID=2,GR_YGRID=5,GR_ZMIN=8,GR_ZMAX=9,GR_ZSCL=10,GR_EYE_THETA=11,GR_EYE_PHI=12,GR_EYE_PSI=13,GR_NCONTOUR=14,GR_XSCALE=15,GR_YSCALE=16,GR_ZSCALE=17,GR_NMIN=8,GR_NMAX=9,GR_NPLOT=10,GR_NSTEP=11};
#endif
#ifndef __HAVE_GR_WIN_VARS
#define __HAVE_GR_WIN_VARS
typedef struct{float flt_xcursor;float flt_ycursor;float flt_zcursor;float flt_tcursor;float flt_rcursor;float flt_thetacursor;float flt_ncursor;float recip_delx;float recip_dely;float orgxmin;float orgxmax;float panshift;float orgtblst;float tblshift;float tblstart;float deltatbl;float*rngp;float PrevRange[12];float UserRange[29];GR_MODES*gr_modep;WINDOW*grwinp;WINDOW*rngwinp;WINDOW*tblwinp;TABLE_WIN_VARS*tableptr;union{struct{EQU_DS_AMS1 equedDS;unsigned short curinc;unsigned short curincy;unsigned short tblindx;short yaxispix;unsigned short TBL_WidthLimit;HANDLE zval;DB3 DB3z;HANDLE htbinput;HANDLE hfldpic;unsigned short gr_win_flags;unsigned char xmaxpix;unsigned char ymaxpix;unsigned char gr_ref_mask;unsigned char graph_mode;unsigned char gr_side;unsigned char gr_folder_cnt;unsigned char gr_shade_pat;unsigned char rng_xpix;unsigned char rng_ypix;unsigned char tbl_flags;unsigned char tbl_par_flags;unsigned char gr_top_flags;unsigned char ValidCursBits;signed char de_twopass;FUNCID CurFunc;unsigned char PrevZoomMode;}ams1;struct{EQU_DS equedDS;unsigned short curinc;unsigned short curincy;unsigned short tblindx;short yaxispix;unsigned short TBL_WidthLimit;HANDLE zval;DB3 DB3z;HANDLE htbinput;HANDLE hfldpic;unsigned short gr_win_flags;unsigned char xmaxpix;unsigned char ymaxpix;unsigned char gr_ref_mask;unsigned char graph_mode;unsigned char gr_side;unsigned char gr_folder_cnt;unsigned char gr_shade_pat;unsigned char rng_xpix;unsigned char rng_ypix;unsigned char tbl_flags;unsigned char tbl_par_flags;unsigned char gr_top_flags;unsigned char ValidCursBits;signed char de_twopass;FUNCID CurFunc;unsigned char PrevZoomMode;}ams2;};}GR_WIN_VARS;
#endif
#define gr_active (*((GR_WIN_VARS**)(_rom_call_addr(10B))))
#define gr_other (*((GR_WIN_VARS**)(_rom_call_addr(10C))))
#define CkValidDelta _rom_call(unsigned char,(float,float,float),1C3)
#define CptDeltax _rom_call(unsigned char,(GR_WIN_VARS*),1C1)
#define CptDeltay _rom_call(unsigned char,(GR_WIN_VARS*),1C2)
#define gdb_len ({__need_in_use_bit;_rom_call(unsigned long,(void),1E0);})
#define gdb_recall ({__need_in_use_bit;_rom_call(void,(HSym),1E2);})
#define gdb_store ({__need_in_use_bit;_rom_call(void,(ESI),1E1);})
#define rngLen _rom_call(unsigned long,(char),1DF)
#define StepCk _rom_call(void,(float*),1DD)
#if MIN_AMS>=200
typedef struct{int gr_in_progress:1;int gr_zoom_fit:1;int gr_cpt_seq_flag:1;int stat_in_progress:1;int gr_trace_seq:1;int de_init_conds:1;int gr_cpt_de_flag:1;int new_eqn:1;int de_error:1;}GR_FLAGS;
#define gr_flags (*((GR_FLAGS*)(_rom_call_addr(463))))
#endif
/* End Auto-Generated Part */

#endif

