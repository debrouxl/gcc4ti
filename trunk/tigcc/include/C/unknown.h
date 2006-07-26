#ifndef __UNKNOWN
#define __UNKNOWN

#include <default.h>

/* Begin Auto-Generated Part */
#define SYM_LEN 8
#ifndef __HAVE_Attrs
#define __HAVE_Attrs
enum Attrs{A_REVERSE,A_NORMAL,A_XOR,A_SHADED,A_REPLACE,A_OR,A_AND,A_THICK1,A_SHADE_V,A_SHADE_H,A_SHADE_NS,A_SHADE_PS};
#endif
#ifndef __HAVE_BITMAP
#define __HAVE_BITMAP
typedef struct{unsigned short NumRows,NumCols;unsigned char Data[];}BITMAP;
#endif
#ifndef __HAVE_ESQ
#define __HAVE_ESQ
typedef unsigned char ESQ;
#endif
#ifndef __HAVE_CESI
#define __HAVE_CESI
typedef const ESQ*CESI;
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_DB3
#define __HAVE_DB3
typedef struct{long cVertices;HANDLE hVertices;long cEdges;HANDLE hEdges;HANDLE hContours;long ciVertices;HANDLE hiVertices;long ciEdges;HANDLE hiEdges;short DCM[3][3];short DCM0[3][3];short DCMhome[3][3];short cDCMangle;unsigned short nSpinDir;unsigned char nSpinSpeed;unsigned char bSpinning;short calp,salp;}DB3;
#endif
#ifndef __HAVE_div_t
#define __HAVE_div_t
typedef struct{short quot,rem;}div_t;
#endif
#ifndef __HAVE_SCR_RECT
#define __HAVE_SCR_RECT
typedef union{struct{unsigned char x0,y0,x1,y1;}xy;unsigned long l;}SCR_RECT;
#endif
#ifndef __HAVE_SCR_STATE
#define __HAVE_SCR_STATE
typedef struct{void*ScrAddr;unsigned char XMax,YMax;short CurFont,CurAttr,CurX,CurY;SCR_RECT CurClip;}SCR_STATE;
#endif
#ifndef __HAVE_WINDOW_AMS1
#define __HAVE_WINDOW_AMS1
typedef struct WindowStruct_AMS1{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;}WINDOW_AMS1;
#endif
#ifndef __HAVE_WINDOW
#define __HAVE_WINDOW
typedef struct WindowStruct{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;SCR_STATE savedScrState;unsigned char Reserved[16];}WINDOW;
#endif
#ifndef __HAVE_EQU_DS_AMS1
#define __HAVE_EQU_DS_AMS1
typedef struct SEquDS_AMS1{WINDOW_AMS1 wMain;short focus;short cFunc;short yPos;short xOffset;short yBelow;unsigned short bAltForm;}EQU_DS_AMS1;
#endif
#ifndef __HAVE_EQU_DS
#define __HAVE_EQU_DS
typedef struct SEquDS{WINDOW wMain;short focus;short cFunc;short yPos;short xOffset;short yBelow;unsigned short bAltForm;}EQU_DS;
#endif
#ifndef __HAVE_ESI_Callback_t
#define __HAVE_ESI_Callback_t
typedef CALLBACK void(*ESI_Callback_t)(ESI);
#endif
#ifndef __HAVE_ESI
#define __HAVE_ESI
typedef ESQ*ESI;
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
#ifndef __HAVE_GraphModes
#define __HAVE_GraphModes
enum GraphModes{GR_FUNC=1,GR_PAR=2,GR_POL=3,GR_SEQ=4,GR_3D=5,GR_DE=6};
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
#ifndef __HAVE_HSym
#define __HAVE_HSym
typedef struct{HANDLE folder;unsigned short offset;}HSym;
#endif
#ifndef __HAVE_ldiv_t
#define __HAVE_ldiv_t
typedef struct{long quot,rem;}ldiv_t;
#endif
#ifndef __HAVE_SCR_COORDS
#define __HAVE_SCR_COORDS
typedef unsigned char SCR_COORDS;
#endif
#ifndef __HAVE_SYM_ENTRY
#define __HAVE_SYM_ENTRY
typedef struct{char name[8];unsigned short compat;union{unsigned short flags_n;struct{unsigned int busy:1,local:1,flag1_5:1,flag1_4:1,collapsed:1,twin:1,archived:1,in_view:1;unsigned int folder:1,overwritten:1,checked:1,hidden:1,locked:1,statvar:1,graph_ref_1:1,graph_ref_0:1;}bits;}flags;HANDLE handle;}SYM_ENTRY;
#endif
#ifndef __HAVE_SYM_STR
#define __HAVE_SYM_STR
typedef CESI SYM_STR;
#endif
#ifndef __HAVE_WIN_COORDS
#define __HAVE_WIN_COORDS
typedef short WIN_COORDS;
#endif
#ifndef __HAVE_WIN_RECT
#define __HAVE_WIN_RECT
typedef struct{short x0,y0,x1,y1;}WIN_RECT;
#endif
typedef enum{SELECT_ON,SELECT_OFF,SELECT_TOGGLE}EQU_SELECT;
typedef unsigned long pFrame;
typedef struct{unsigned char tag_order;unsigned char tag_version;unsigned char min_max_args;unsigned char proc_args;void(*CALLBACK tag_proc)(void);unsigned long tag_str;}tag_info;
typedef CALLBACK void(*Two_ESI_Callback_t)(ESI,ESI);
typedef unsigned short unknown_retval;
#define EV_appA (*((short*)(_rom_call_addr_hack(452,(((unsigned long)*(signed short*)((unsigned char*)_rom_call_addr(D8)+0x20))),200))))
#define EV_appB (*((short*)(_rom_call_addr_hack(453,(((unsigned long)*(signed short*)((unsigned char*)_rom_call_addr(D8)+0x26))),200))))
#define EV_appSide (*((unsigned short*)(_rom_call_addr_hack(458,(((unsigned long)*(signed short*)((unsigned char*)_rom_call_addr(D8)+0x1A))),200))))
#define CharNumber _rom_call(unsigned char,(char,char,unsigned char*),113)
#define CheckGraphRef _rom_call(short,(const SYM_ENTRY*),7D)
#define ck_valid_float _rom_call(short,(float*),1E5)
#define cleanup_de_mem ({__need_in_use_bit;_rom_call(unknown_retval,(),1EC);})
#define cleanup_seq_mem ({__need_in_use_bit;_rom_call(unknown_retval,(),1D7);})
#define convert_to_TI_92 _rom_call(void,(HANDLE),258)
#define CountGrFunc ({__need_in_use_bit;_rom_call(unknown_retval,(),1D5);})
#define cpt_gr_fun ({__need_in_use_bit;_rom_call(unknown_retval,(),1D1);})
#define cpt_gr_param ({__need_in_use_bit;_rom_call(unknown_retval,(),1D2);})
#define cpt_gr_polar ({__need_in_use_bit;_rom_call(unknown_retval,(),1D3);})
#define CptFuncX(x,y) _tios_float_2(1B9,x,y,float,GR_WIN_VARS*)
#define CptIndep _rom_call(unsigned char,(float*,float*,short),1C9)
#define CptLastIndepDE _rom_call(unknown_retval,(),1EF)
#define CreateEmptyList _rom_call(HANDLE,(void),1E6)
#define CustomBegin _rom_call(void,(void),14A)
#define CustomEnd _rom_call(void,(void),14C)
#define CustomFree _rom_call(void,(void),150)
#define CustomMenuItem _rom_call(short,(short),14B)
#define de_initRes ({__need_in_use_bit;_rom_call(unknown_retval,(),1F0);})
#define de_loop ({__need_in_use_bit;_rom_call(unknown_retval,(),1EB);})
#define de_rng_no_graph _rom_call(unknown_retval,(),2A2)
#define deStepCk _rom_call(unknown_retval,(),1F9)
#define dv_create_graph_titles ({__need_in_use_bit;_rom_call(unknown_retval,(),117);})
#define dv_findColumn _rom_call(unknown_retval,(),119)
#define EQU_deStatus _rom_call(unknown_retval,(),16E)
#define EQU_getNameInfo _rom_call(unknown_retval,(),120)
#define EQU_select ({__need_in_use_bit;_rom_call(short,(short,EQU_SELECT),11E);})
#define EQU_setStyle ({__need_in_use_bit;_rom_call(void,(GR_WIN_VARS*,short,short,short,short),11F);})
#define execute_graph_func ({__need_in_use_bit;_rom_call(unknown_retval,(),1D0);})
#define FindFunc ({__need_in_use_bit;_rom_call(HSym,(char,char,HSym*),1C5);})
#define FindGrFunc ({__need_in_use_bit;_rom_call(SYM_ENTRY*,(char,HSym*,ESI*,char,char),1C6);})
#define FirstSeqPlot _rom_call(unknown_retval,(),1D6)
#define fix_loop_displacements _rom_call(void,(ESI),170)
#define FuncLineFlt _rom_call(unknown_retval,(),1BF)
#define gen_version _rom_call(unsigned char,(ESI,ESI),259)
#define GetStatValue ({__need_in_use_bit;_rom_call(unknown_retval,(),11B);})
#define GetSysGraphRef _rom_call(unsigned short,(short),8A)
#define GM_Derivative ({__need_in_use_bit;_rom_call(unknown_retval,(),1B0);})
#define GM_DistArc ({__need_in_use_bit;_rom_call(unknown_retval,(),1B1);})
#define GM_Inflection ({__need_in_use_bit;_rom_call(unknown_retval,(),1AD);})
#define GM_Integrate ({__need_in_use_bit;_rom_call(unknown_retval,(),1AC);})
#define GM_Intersect ({__need_in_use_bit;_rom_call(unknown_retval,(),1AB);})
#define GM_Math1 ({__need_in_use_bit;_rom_call(unknown_retval,(),1AF);})
#define GM_Shade ({__need_in_use_bit;_rom_call(unknown_retval,(),1B2);})
#define GM_TanLine ({__need_in_use_bit;_rom_call(unknown_retval,(),1AE);})
#define GM_Value ({__need_in_use_bit;_rom_call(unknown_retval,(),1AA);})
#define gr_add_fldpic ({__need_in_use_bit;_rom_call(void,(void),1F6);})
#define gr_ck_solvergraph ({__need_in_use_bit;_rom_call(void,(GR_WIN_VARS*),1FA);})
#define gr_CptIndepInc _rom_call(unsigned char,(float,float*,__pushort),1CA)
#define gr_de_axes_lbl _rom_call(unknown_retval,(),1F2)
#define gr_de_value ({__need_in_use_bit;_rom_call(unknown_retval,(),1ED);})
#define gr_del_locals ({__need_in_use_bit;_rom_call(unknown_retval,(),1CB);})
#define gr_del_vars_in_folder ({__need_in_use_bit;_rom_call(unknown_retval,(),1F1);})
#define gr_delete_fldpic ({__need_in_use_bit;_rom_call(void,(GR_WIN_VARS*),1F4);})
#define gr_DelFolder ({__need_in_use_bit;_rom_call(void,(GR_WIN_VARS*),1CC);})
#define gr_DispLabels _rom_call(void,(GR_WIN_VARS*),1E3)
#define gr_execute_de ({__need_in_use_bit;_rom_call(unknown_retval,(),1F3);})
#define gr_execute_seq ({__need_in_use_bit;_rom_call(unknown_retval,(),1D4);})
#define gr_find_de_result _rom_call(unknown_retval,(),1E8)
#define gr_find_el _rom_call(unknown_retval,(),1F8)
#define gr_find_func_index _rom_call(unknown_retval,(),1EE)
#define gr_initCondName _rom_call(unknown_retval,(),1C8)
#define gr_openFolder _rom_call(void,(GR_WIN_VARS*),1CD)
#define GR_Pan _rom_call(unknown_retval,(),1C4)
#define gr_remove_fldpic ({__need_in_use_bit;_rom_call(void,(GR_WIN_VARS*),1F5);})
#define gr_seq_value ({__need_in_use_bit;_rom_call(unknown_retval,(),1DC);})
#define gr_stopic ({__need_in_use_bit;_rom_call(unknown_retval,(),1F7);})
#define gr_xres_pixel _rom_call(unsigned short,(short,char),1B8)
#define GR3_addContours ({__need_in_use_bit;_rom_call(unknown_retval,(),1FB);})
#define GR3_freeDB _rom_call(unknown_retval,(),1FD)
#define GR3_handleEvent ({__need_in_use_bit;_rom_call(unknown_retval,(),1FE);})
#define GraphActivate _rom_call(short,(short),1FC)
#define GraphOrTableCmd ({__need_in_use_bit;_rom_call(void,(ESI,short),1E4);})
#define GrAxes _rom_call(void,(short,GR_WIN_VARS*),1B7)
#define GrClipLine _rom_call(unsigned char,(float,float,float,float,float*,float*,float*,float*,GR_WIN_VARS*),1C0)
#define grFuncName _rom_call(unknown_retval,(),1C7)
#define GrLineFlt _rom_call(void,(float,float,float,float,GR_WIN_VARS*,short,char,char),1BE)
#define GS_PlotAll ({__need_in_use_bit;_rom_call(unknown_retval,(),202);})
#define GS_PlotTrace ({__need_in_use_bit;_rom_call(unknown_retval,(),201);})
#define GT_BackupToScr _rom_call(unknown_retval,(),20C)
#define GT_CalcDepVals ({__need_in_use_bit;_rom_call(unknown_retval,(),20D);})
#define GT_CenterGraphCursor _rom_call(unknown_retval,(),20E)
#define GT_CursorKey ({__need_in_use_bit;_rom_call(unknown_retval,(),20F);})
#define GT_DE_Init_Conds ({__need_in_use_bit;_rom_call(unknown_retval,(),22A);})
#define GT_DspFreeTraceCoords _rom_call(unknown_retval,(),210)
#define GT_DspMsg _rom_call(void,(const char*,short),212)
#define GT_DspTraceCoords _rom_call(unknown_retval,(),211)
#define GT_Error ({__need_in_use_bit;_rom_call(unknown_retval,(),213);})
#define GT_Format ({__need_in_use_bit;_rom_call(unknown_retval,(),214);})
#define GT_FreeTrace ({__need_in_use_bit;_rom_call(unknown_retval,(),215);})
#define GT_IncXY ({__need_in_use_bit;_rom_call(unknown_retval,(),216);})
#define GT_KeyIn ({__need_in_use_bit;_rom_call(unknown_retval,(),217);})
#define GT_Open ({__need_in_use_bit;_rom_call(unknown_retval,(),21B);})
#define GT_PrintCursor _rom_call(unknown_retval,(),229)
#define GT_QFloatCursorsInRange _rom_call(unknown_retval,(),218)
#define GT_Regraph_if_neccy ({__need_in_use_bit;_rom_call(void,(void),21A);})
#define GT_Regraph ({__need_in_use_bit;_rom_call(void,(void),219);})
#define GT_SaveAs ({__need_in_use_bit;_rom_call(unknown_retval,(),21C);})
#define GT_SelFunc ({__need_in_use_bit;_rom_call(unknown_retval,(),21D);})
#define GT_Set_Graph_Format _rom_call(unknown_retval,(),228)
#define GT_SetCursorXY _rom_call(unknown_retval,(),21F)
#define GT_SetGraphRange _rom_call(unknown_retval,(),21E)
#define GT_ShowMarkers _rom_call(unknown_retval,(),220)
#define GT_Trace ({__need_in_use_bit;_rom_call(short,(short,float*,short),221);})
#define GT_ValidGraphRanges _rom_call(unknown_retval,(),222)
#define GT_WinBound _rom_call(unknown_retval,(),223)
#define GT_WinCursor _rom_call(unknown_retval,(),224)
#define GXcoord _rom_call(unknown_retval,(),226)
#define GYcoord _rom_call(unknown_retval,(),225)
#define GZ_Decimal ({__need_in_use_bit;_rom_call(unknown_retval,(),22D);})
#define GZ_Fit ({__need_in_use_bit;_rom_call(unknown_retval,(),22E);})
#define GZ_InOut ({__need_in_use_bit;_rom_call(unknown_retval,(),22F);})
#define GZ_Integer ({__need_in_use_bit;_rom_call(unknown_retval,(),230);})
#define GZ_Previous ({__need_in_use_bit;_rom_call(unknown_retval,(),231);})
#define GZ_Recall ({__need_in_use_bit;_rom_call(unknown_retval,(),232);})
#define GZ_SetFactors ({__need_in_use_bit;_rom_call(unknown_retval,(),233);})
#define GZ_Square ({__need_in_use_bit;_rom_call(unknown_retval,(),234);})
#define GZ_Standard ({__need_in_use_bit;_rom_call(unknown_retval,(),235);})
#define GZ_Stat ({__need_in_use_bit;_rom_call(unknown_retval,(),236);})
#define GZ_Store ({__need_in_use_bit;_rom_call(unknown_retval,(),237);})
#define GZ_Trig ({__need_in_use_bit;_rom_call(void,(void),238);})
#define InitDEAxesRng _rom_call(unknown_retval,(),1E9)
#define InitDEMem ({__need_in_use_bit;_rom_call(unknown_retval,(),1EA);})
#define InitTimeSeq ({__need_in_use_bit;_rom_call(unknown_retval,(),1D9);})
#define is_executable _rom_call(unknown_retval,(),25A)
#define LoadSymFromFindHandle _rom_call(void,(void),281)
#define NG_cleanup_graph_fun ({__need_in_use_bit;_rom_call(unknown_retval,(),262);})
#define NG_setup_graph_fun ({__need_in_use_bit;_rom_call(unknown_retval,(),261);})
#define OSKeyScan _rom_call(short,(void),298)
#define paint_all_except _rom_call(unknown_retval,(),11D)
#define ParseSymName _rom_call(short,(CESI),78)
#define PlotDel _rom_call(unknown_retval,(),203)
#define PlotDup _rom_call(unknown_retval,(),207)
#define PlotGet _rom_call(unknown_retval,(),205)
#define PlotInit _rom_call(unknown_retval,(),206)
#define PlotLookup _rom_call(unknown_retval,(),209)
#define PlotPut _rom_call(unknown_retval,(),204)
#define PlotSize _rom_call(unknown_retval,(),208)
#define QActivePlots _rom_call(unknown_retval,(),20A)
#define QPlotActive _rom_call(unknown_retval,(),20B)
#define QSkipGraphErr _rom_call(unknown_retval,(),1E7)
#define ReallocExprStruct _rom_call(short,(HANDLE struct,short,ESI),14D)
#define recall_data_var ({__need_in_use_bit;_rom_call(unknown_retval,(),112);})
#define Regraph ({__need_in_use_bit;_rom_call(unknown_retval,(),1B6);})
#define run_one_seq ({__need_in_use_bit;_rom_call(unknown_retval,(),1DB);})
#define ScrRectDivide _rom_call(SCR_RECT*,(const SCR_RECT*,const SCR_RECT*,__pshort),28)
#define SearchExprStruct _rom_call(char*,(HANDLE struct,short),14E)
#define seqStepCk _rom_call(unknown_retval,(),1DE)
#define seqWebInit ({__need_in_use_bit;_rom_call(unknown_retval,(),1DA);})
#define SetGraphMode ({__need_in_use_bit;_rom_call(void,(char),1B5);})
#define setup_more_graph_fun _rom_call(unknown_retval,(),1CE)
#define SP_Define ({__need_in_use_bit;_rom_call(unknown_retval,(),110);})
#define spike_chk_gr_dirty _rom_call(unknown_retval,(),11A)
#define spike_geo_titles ({__need_in_use_bit;_rom_call(unknown_retval,(),115);})
#define spike_in_editor ({__need_in_use_bit;_rom_call(unknown_retval,(),116);})
#define spike_optionD ({__need_in_use_bit;_rom_call(unknown_retval,(),114);})
#define spike_titles_in_editor ({__need_in_use_bit;_rom_call(unknown_retval,(),118);})
#define store_data_var ({__need_in_use_bit;_rom_call(void,(ESI,char),111);})
#define time_loop ({__need_in_use_bit;_rom_call(unknown_retval,(),1D8);})
#define tokenize_if_TI_92_or_text _rom_call(unknown_retval,(),265)
#define unlock_more_graph_fun _rom_call(unknown_retval,(),1CF)
#define UpdateWindows _rom_call(short,(SCR_RECT*),2B)
#define ValidateStore ({__need_in_use_bit;_rom_call(HSym,(HSym,char),8D);})
#define VarGraphRefBitsClear _rom_call(void,(void),268)
#define VarSaveTitle _rom_call(short,(HANDLE,const char*,char*),290)
#define VarStoreLink ({__need_in_use_bit;_rom_call(HSym,(ESI,HANDLE*,__pushort),87);})
#define XCvtFtoP _rom_call(short,(float,GR_WIN_VARS*),1BD)
#define XCvtPtoF(x,y) _tios_float_2(1BA,x,y,short,GR_WIN_VARS*)
#define YCvtFtoP _rom_call(short,(float,GR_WIN_VARS*),1BC)
#define YCvtFtoWin _rom_call(unknown_retval,(),1B3)
#define YCvtPtoF(x,y) _tios_float_2(1BB,x,y,short,GR_WIN_VARS*)
#if MIN_AMS>=101
#define assign_between ({__need_in_use_bit;_rom_call(short,(ESI,ESI,ESI),2AC);})
#define delete_list_element ({__need_in_use_bit;_rom_call(short,(CESI,short),2AF);})
#define did_push_divide_units ({__need_in_use_bit;_rom_call(unknown_retval,(),3AE);})
#define did_push_var_val ({__need_in_use_bit;_rom_call(short,(ESI),2AD);})
#define does_push_fetch ({__need_in_use_bit;_rom_call(short,(ESI),2AE);})
#define get_list_indices _rom_call(unknown_retval,(),2B8)
#define get_matrix_indices _rom_call(unknown_retval,(),2B9)
#define has_unit_base _rom_call(short,(ESI),3AF)
#define index_after_match_endtag _rom_call(ESI,(ESI,char),2B1)
#define init_list_indices _rom_call(unknown_retval,(),2BA)
#define init_matrix_indices _rom_call(unknown_retval,(),2BB)
#define init_unit_system _rom_call(unknown_retval,(),3B0)
#define is_pathname _rom_call(short,(CESI),3C6)
#define is_units_term _rom_call(short,(CESI),3B1)
#define next_token _rom_call(short,(short),3C7)
#define nonblank _rom_call(void,(void),3C8)
#define push_ans_entry _rom_call(HANDLE,(CESI),2B0)
#define push_auto_units_conversion ({__need_in_use_bit;_rom_call(unknown_retval,(),3B2);})
#define push_float_qr_fact ({__need_in_use_bit;_rom_call(unknown_retval,(),2BC);})
#define push_format ({__need_in_use_bit;_rom_call(unknown_retval,(),315);})
#define push_lu_fact ({__need_in_use_bit;_rom_call(unknown_retval,(),2BD);})
#define push_mrow_aux ({__need_in_use_bit;_rom_call(unknown_retval,(),31B);})
#define push_parse_prgm_or_func_text _rom_call(void,(const char*,ESI,short),3C9)
#define push_sq_matrix_to_whole_number ({__need_in_use_bit;_rom_call(unknown_retval,(),3C0);})
#define push_symbolic_qr_fact ({__need_in_use_bit;_rom_call(unknown_retval,(),2BE);})
#define push_unit_system_list _rom_call(unknown_retval,(),3B3)
#define push_user_func ({__need_in_use_bit;_rom_call(void,(ESI,short),2B3);})
#define push_var _rom_call(void,(const char*,const char*),3CB)
#define push_zero_partial_column _rom_call(unknown_retval,(),3C2)
#define setup_unit_system _rom_call(unknown_retval,(),3B4)
#define store_func_def ({__need_in_use_bit;_rom_call(void,(ESI),2B4);})
#define store_to_subscripted_element ({__need_in_use_bit;_rom_call(void,(CESI,CESI),2B5);})
#if MIN_AMS>=200
typedef union{unsigned short*pW;SYM_ENTRY*pS;unsigned char*pB;}FOLDER_TYPE;
enum OO_Indices{OO_APP_FLAGS=0x1,OO_APP_NAME=0x2,OO_APP_TOK_NAME=0x3,OO_APP_PROCESS_EVENT=0x4,OO_APP_DEFAULT_MENU=0x5,OO_APP_DEFAULT_MENU_HANDLE=0x6,OO_APP_EXT_COUNT=0x7,OO_APP_EXTENSIONS=0x8,OO_APP_EXT_ENTRIES=0x9,OO_APP_LOCALIZE=0xA,OO_APP_UNLOCALIZE=0xB,OO_APP_CAN_DELETE=0xC,OO_APP_CAN_MOVE=0xD,OO_APP_VIEWER=0xE,OO_APP_ICON=0xF,OO_APP_EXT_HELP=0x10,OO_APP_NOTICE_INSTALL=0x11,OO_APP_ABOUT=0x12,OO_SFONT=0x300,OO_LFONT=0x301,OO_HFONT=0x302,OO_APP_SFONT=0x300,OO_APP_LFONT=0x301,OO_APP_HFONT=0x301,OO_LANGUAGE=0x310,OO_DATE_FORMAT=0x311,OO_BUILTIN_HELP=0x312,OO_KTLIST=0x320,OO_CAT_TABLE=0x312,OO_CAT_INDEX=0x322,OO_CAT_COUNT=0x323,OO_CHAR_MENU=0x330,OO_CHAR_HANDLER=0x331,OO_APPS_HANDLER=0x332,OO_FLASH_APPS_HANDLER=0x333,OO_MATH_HANDLER=0x334,OO_MEM_HANDLER=0x335,OO_STO_HANDLER=0x336,OO_QUIT_HANDLER=0x337};
typedef struct SymPrivateGlobals{unsigned char SPG_Ver;unsigned short SymTempFolCount;unsigned char*SymFolder,*SymName;HANDLE SymHandle,HomeHandle,MainHandle,DefTempHandle,DefFolderHandle;unsigned short SymErrCode;BOOL CallStatFree;HANDLE FindHandle;unsigned short DefFolderName[SYM_LEN+1];struct{FOLDER_TYPE Low,High,Cur;unsigned short CurOffset;}Find,Prev;}SymPG_S;
#define EV_currentApp (*((short*)(_rom_call_addr(45C))))
#define EV_errorCode (*((short*)(_rom_call_addr(46D))))
#define EV_runningApp (*((short*)(_rom_call_addr(45D))))
#define FLOATTAB ((float*const)(_rom_call_addr(464)))
#define NG_control (*((unsigned long*)(_rom_call_addr(466))))
#define OO_firstACB (*((short*)(_rom_call_addr(451))))
#define OO_SuperFrame (*((pFrame*)(_rom_call_addr(48F))))
#define OSModKeyStatus (*((unsigned short*)(_rom_call_addr(431))))
#define primary_tag_list ((const tag_info*const)(_rom_call_addr(467)))
#define pSymPG ((SymPG_S*const)(_rom_call_addr(430)))
#define ST_flags (*((unsigned long*)(_rom_call_addr(443))))
#define VarOptList ((unsigned short*const)(_rom_call_addr(444)))
#define are_units_consistent ({__need_in_use_bit;_rom_call(short,(ESI,ESI),40F);})
#define clear_error_context _rom_call(void,(void),44C)
#define DataTypeNames _rom_call(unsigned char*,(char),43B)
#define ER_throwFrame _rom_call(void,(short,pFrame),507)
#define ERD_dismissNotice _rom_call(void,(void),476)
#define ERD_notice _rom_call(short,(unsigned char const*,unsigned char const*),475)
#define estack_to_float(x) _tios_float_1(469,x,CESI)
#define EV_quit ({__need_in_use_bit;_rom_call(void,(void),48E);})
#define FirstNonblank _rom_call(unsigned char*,(unsigned char*),4DE)
#define ForceFloat(x) ({__need_in_use_bit;_tios_float_1(47D,x,ESI);})
#define freeIdList _rom_call(void,(void),4D6)
#define GetTagStr _rom_call(char*,(ESI,char*),46C)
#define hStrAppend _rom_call(void,(HANDLE,unsigned char*),45F)
#define is_cFloat_agg _rom_call(short,(ESI),465)
#define is_complex_Float _rom_call(short,(CESI),48B)
#define LIO_SendIdList _rom_call(unsigned short,(short),3D0)
#define OO_appGetPublicStorage _rom_call(unsigned long,(void),425)
#define OO_appIsMarkedDelete _rom_call(short,(short),426)
#define OO_appMarkDelete _rom_call(void,(short),427)
#define OO_AppNameToACB _rom_call(short,(unsigned char const*,short),406)
#define OO_appSetPublicStorage _rom_call(void,(long),429)
#define OO_CondGetAttr _rom_call(short,(pFrame,long,void**),3FA)
#define OO_Deref _rom_call(void*,(pFrame),3FB)
#define OO_Destroy _rom_call(pFrame,(pFrame),423)
#define OO_DestroyAll _rom_call(pFrame,(pFrame),4F5)
#define OO_GetAppAttr _rom_call(void*,(short,long),3FC)
#define OO_GetAttr _rom_call(void*,(pFrame,long),3FD)
#define OO_HasAttr _rom_call(short,(pFrame,long),3FE)
#define OO_InstallAppHook _rom_call(short,(short,pFrame,pFrame*),490)
#define OO_InstallAppHookByName _rom_call(short,(unsigned char const*,pFrame,pFrame*),492)
#define OO_InstallSystemHook _rom_call(short,(pFrame,pFrame*),404)
#define OO_New _rom_call(pFrame,(pFrame),3FF)
#define OO_NextACB _rom_call(short,(short),402)
#define OO_PrevACB _rom_call(short,(short),403)
#define OO_SetAppAttr _rom_call(short,(short,long,void*),400)
#define OO_SetAttr _rom_call(short,(pFrame,long,void*),401)
#define OO_UninstallAppHook _rom_call(short,(short,pFrame),491)
#define OO_UninstallAppHookByName _rom_call(short,(unsigned char const*,pFrame),493)
#define OO_UninstallSystemHook _rom_call(short,(pFrame),405)
#define push0 _rom_call(void,(void),4E7)
#define push1 _rom_call(void,(void),4E8)
#define push_negate_quantum_as_negint _rom_call(void,(ESQ),4F1)
#define push_simplify_statements ({__need_in_use_bit;_rom_call(void,(ESI),44F);})
#define QstatRcl _rom_call(short,(void),40B)
#define sf_width _rom_call(unsigned char,(char),4D3)
#define statEnd _rom_call(void,(void),409)
#define statFree ({__need_in_use_bit;_rom_call(void,(void),40A);})
#define statStart ({__need_in_use_bit;_rom_call(void,(void),408);})
#define TIOS_EV_getAppID _rom_call(short,(unsigned char const*),454)
#define TIOS_strtol _rom_call(long,(const char*,char**,short),4FF)
#define TokenizeName _rom_call(unsigned short,(const char*,unsigned char*),3E9)
#if MIN_AMS>=202
#define compare_numbers _rom_call(short,(ESI,ESI),50D)
#define did_push_approx_inflection_point ({__need_in_use_bit;_rom_call(short,(ESI,ESI,float,float),5B0);})
#define did_push_lincf ({__need_in_use_bit;_rom_call(short,(ESI,ESI),572);})
#define divide_top ({__need_in_use_bit;_rom_call(void,(ESI),527);})
#define get_lb(x) ({__need_in_use_bit;_tios_float_1(516,x,ESI);})
#define get_ub(x) ({__need_in_use_bit;_tios_float_1(517,x,ESI);})
#define has_different_variable _rom_call(short,(ESI,ESI,short),5A0)
#define index_if_pushed_binomial_info ({__need_in_use_bit;_rom_call(ESI,(ESI,ESI),5A1);})
#define index_if_pushed_qquad_info ({__need_in_use_bit;_rom_call(ESI,(ESI,ESI,ESI),5A2);})
#define index_reductum_with_tag_base _rom_call(ESI,(ESI,ESQ,short),59F)
#define index_rmng_factor _rom_call(ESI,(ESI,ESI),565)
#define index_rmng_fctrs_start_base_tag _rom_call(ESI,(ESI,ESQ),563)
#define index_rmng_fctrs_start_base _rom_call(ESI,(ESI,ESI),564)
#define index_rmng_fctrs_start_fctr_tag _rom_call(ESI,(ESI,ESQ),562)
#define is_equivalent_to ({__need_in_use_bit;_rom_call(short,(ESI,ESI),578);})
#define is_neg_lead_numr_coef_re_part _rom_call(short,(ESI),56D)
#define is_pos_int_and_eq_quantum _rom_call(short,(CESI,ESQ),509)
#define is_real ({__need_in_use_bit;_rom_call(short,(ESI),5A9);})
#define is_reciprocal_of_quantum _rom_call(short,(CESI,ESQ),50A)
#define is_term_improper ({__need_in_use_bit;_rom_call(short,(ESI),556);})
#define lead_conjunct_factor_index _rom_call(ESI,(ESI),580)
#define lead_disjunct_term_index _rom_call(ESI,(ESI),57E)
#define linear_degree _rom_call(short,(ESI,ESI),571)
#define next_var_or_kernel_index _rom_call(ESI,(ESI,ESI),56E)
#define or_onto_top _rom_call(void,(ESI),57B)
#define push_but_conjunct_factor ({__need_in_use_bit;_rom_call(void,(ESI,ESI),582);})
#define push_but_factor ({__need_in_use_bit;_rom_call(void,(ESI,ESI),561);})
#define push_but_term ({__need_in_use_bit;_rom_call(void,(ESI,ESI),56A);})
#define push_constant_factors ({__need_in_use_bit;_rom_call(void,(ESI),55D);})
#define push_constant_terms ({__need_in_use_bit;_rom_call(void,(ESI),566);})
#define push_dependent_factors ({__need_in_use_bit;_rom_call(void,(ESI,ESI),55F);})
#define push_dependent_terms ({__need_in_use_bit;_rom_call(void,(ESI,ESI),568);})
#define push_gcd_then_cofactors ({__need_in_use_bit;_rom_call(ESI,(ESI,ESI,ESI*),59E);})
#define push_independent_factors ({__need_in_use_bit;_rom_call(void,(ESI,ESI),560);})
#define push_independent_terms ({__need_in_use_bit;_rom_call(void,(ESI,ESI),569);})
#define push_make_proper ({__need_in_use_bit;_rom_call(void,(ESI),557);})
#define push_minus_recip_of_quantum _rom_call(void,(ESQ),510)
#define push_nonconstant_factors ({__need_in_use_bit;_rom_call(void,(ESI),55E);})
#define push_nonconstant_terms ({__need_in_use_bit;_rom_call(void,(ESI),567);})
#define push_nonnumeric_factors ({__need_in_use_bit;_rom_call(void,(ESI),55C);})
#define push_pi_on_quantum ({__need_in_use_bit;_rom_call(void,(ESQ),513);})
#define push_pi _rom_call(void,(void),512)
#define push_poly_deg_in_var_or_kernel ({__need_in_use_bit;_rom_call(void,(ESI,ESI),570);})
#define push_poly_qr ({__need_in_use_bit;_rom_call(ESI,(ESI,ESI,ESI,short),59D);})
#define push_quantum_as_nonnegative_int _rom_call(void,(ESQ),50E)
#define push_quantum_pair_as_pos_frac _rom_call(void,(ESQ,ESQ),511)
#define push_reciprocal_of_quantum _rom_call(void,(ESQ),50F)
#define push_reciprocal ({__need_in_use_bit;_rom_call(void,(ESI),597);})
#define push_standardize ({__need_in_use_bit;_rom_call(void,(ESI),558);})
#define push_trig ({__need_in_use_bit;_rom_call(void,(ESI_Callback_t,ESI),535);})
#define push_var_kern_tail _rom_call(void,(ESI),56F)
#define raise_to_top ({__need_in_use_bit;_rom_call(void,(ESI),529);})
#define remaining_conjuncts_index _rom_call(ESI,(ESI),581)
#define remaining_disjuncts_index _rom_call(ESI,(ESI),57F)
#define replace_top_with_post_simplified ({__need_in_use_bit;_rom_call(void,(ESI),5B6);})
#define replace_top_with_reciprocal ({__need_in_use_bit;_rom_call(void,(void),52D);})
#define replace_top2_with_and ({__need_in_use_bit;_rom_call(void,(ESI),57A);})
#define replace_top2_with_difference ({__need_in_use_bit;_rom_call(void,(ESI),51C);})
#define replace_top2_with_imre ({__need_in_use_bit;_rom_call(void,(ESI),559);})
#define replace_top2_with_or _rom_call(void,(ESI),57C)
#define replace_top2_with_pow ({__need_in_use_bit;_rom_call(void,(ESI),52A);})
#define replace_top2_with_prod ({__need_in_use_bit;_rom_call(void,(ESI),523);})
#define replace_top2_with_ratio ({__need_in_use_bit;_rom_call(void,(ESI),528);})
#define replace_top2_with_sum ({__need_in_use_bit;_rom_call(void,(ESI),519);})
#define times_top ({__need_in_use_bit;_rom_call(void,(ESI),522);})
#if MIN_AMS>=204
typedef enum{EV_OFF=0x0001,EV_SUSPEND_PAINTING=0x0002}EV_FLAGS;
#define errno (*((short*)(_rom_call_addr(5D9))))
#define estack_max_index (*((ESI*)(_rom_call_addr(5BF))))
#define EV_flags (*((EV_FLAGS*)(_rom_call_addr(5DF))))
#define Float0Index (*((ESI*)(_rom_call_addr(5D0))))
#define Float1Index (*((ESI*)(_rom_call_addr(5D1))))
#define FloatExp1Index (*((ESI*)(_rom_call_addr(5D4))))
#define FloatHalfIndex (*((ESI*)(_rom_call_addr(5E0))))
#define FloatMinus1Index (*((ESI*)(_rom_call_addr(5D2))))
#define FloatPiIndex (*((ESI*)(_rom_call_addr(5D3))))
#define IM_re_tol (*((float*)(_rom_call_addr(5C1))))
#define index_false (*((ESI*)(_rom_call_addr(5D6))))
#define index_true (*((ESI*)(_rom_call_addr(5D5))))
#define Integer0Index (*((ESI*)(_rom_call_addr(5CD))))
#define Integer1Index (*((ESI*)(_rom_call_addr(5CE))))
#define Integer2Index (*((ESI*)(_rom_call_addr(5E1))))
#define IntegerMinus1Index (*((ESI*)(_rom_call_addr(5CF))))
#define NG_such_that_index (*((ESI*)(_rom_call_addr(5D7))))
#define RAtionalize_tol (*((float*)(_rom_call_addr(5C0))))
#define RM_Type (*((unsigned char*)(_rom_call_addr(5DC))))
#define did_map_aggregate_arg _rom_call(short,(Two_ESI_Callback_t,ESI,ESI),5B8)
#define is_undefined _rom_call(short,(ESI),5B7)
#define TIOS_abs _rom_call(short,(short),5BA)
#define TIOS_div _rom_call(div_t,(short,short),5BB)
#define TIOS_labs _rom_call(long,(long),5BC)
#define TIOS_ldiv _rom_call(ldiv_t,(long,long),5BD)
#if MIN_AMS>=205
#define OSCheckLinkOpen _rom_call(unsigned short,(void),5E3)
#endif
#endif
#endif
#endif
#endif
/* End Auto-Generated Part */

#endif
