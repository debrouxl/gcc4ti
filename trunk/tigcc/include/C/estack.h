#ifndef __ESTACK
#define __ESTACK

#include <default.h>

#define _push_zstr(s) ({register const char*__p=(s);register long __l=_rom_call(long,(const char*),27E)(__p);char __s[__l+2];__s[0]=0;push_expr_quantum(_rom_call(char*,(char*,const char*),26C)(__s+1,__p)+__l,STR_TAG);})
#define _push_zstr_const(s) (push_expr_quantum(SYMSTR(s),STR_TAG))

/* Begin Auto-Generated Part */
#define H_NULL 0
#define NULL_INDEX ((CESI)0)
#ifndef __HAVE_bcd
#define __HAVE_bcd
typedef struct{unsigned short exponent;unsigned long long mantissa;}bcd __attribute__((__may_alias__));
#endif
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_SCR_RECT
#define __HAVE_SCR_RECT
typedef union{struct{unsigned char x0,y0,x1,y1;}xy;unsigned long l;}SCR_RECT;
#endif
#ifndef __HAVE_SCR_STATE
#define __HAVE_SCR_STATE
typedef struct{void*ScrAddr;unsigned char XMax,YMax;short CurFont,CurAttr,CurX,CurY;SCR_RECT CurClip;}SCR_STATE;
#endif
#ifndef __HAVE_ti_float
#define __HAVE_ti_float
typedef float ti_float;
#endif
#ifndef __HAVE_WINDOW
#define __HAVE_WINDOW
typedef struct WindowStruct{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;SCR_STATE savedScrState;unsigned char Reserved[16];}WINDOW;
#endif
#ifndef __HAVE_ESQ
#define __HAVE_ESQ
typedef unsigned char ESQ;
#endif
typedef CALLBACK unsigned short(*CESI_Callback_t)(CESI);
#ifndef __HAVE_CESI
#define __HAVE_CESI
typedef const ESQ*CESI;
#endif
typedef CALLBACK unsigned short(*ESI_Callback_Int_t)(ESI,unsigned short);
#ifndef __HAVE_ESI_Callback_t
#define __HAVE_ESI_Callback_t
typedef CALLBACK void(*ESI_Callback_t)(ESI);
#endif
#ifndef __HAVE_ESI
#define __HAVE_ESI
typedef ESQ*ESI;
#endif
#define EStackIndex ESI
enum ExtTags{INDIR_TAG=0x01,GETKEY_TAG=0x02,GETFOLD_TAG=0x03,SWITCH_TAG=0x04,UNITCONV_TAG=0x05,ORD_TAG=0x06,EXPR_TAG=0x07,CHAR_TAG=0x08,STRING_TAG=0x09,GETTYPE_TAG=0x0A,GETMODE_TAG=0x0B,SETFOLD_TAG=0x0C,PTTEST_TAG=0x0D,PXLTEST_TAG=0x0E,SETGRAPH_TAG=0x0F,SETTABLE_TAG=0x10,SETMODE_TAG=0x11,FORMAT_TAG=0x12,INSTRING_TAG=0x13,APPEND_TAG=0x14,DD_TAG=0x15,EXPR2DMS_TAG=0x16,VEC2RECT_TAG=0x17,VEC2POLAR_TAG=0x18,VEC2CYLIND_TAG=0x19,VEC2SPHERE_TAG=0x1A,PARENTH_START_TAG=0x1B,PARENTH_END_TAG=0x1C,MAT_START_TAG=0x1D,MAT_END_TAG=0x1E,LIST_START_TAG=0x1F,LIST_END_TAG=0x20,COMMA_TAG=0x21,SEMICOLON_TAG=0x22,COMPLEX_ANGLE_TAG=0x23,SINGLE_QUOTE_TAG=0x24,QUOTE_TAG=0x25,POLCPLX_TAG=0x26,TMPCNV_TAG=0x27,DELTA_TMPCNV_TAG=0x28,GETUNITS_TAG=0x29,SETUNITS_TAG=0x2A,BIN_TAG=0x2B,HEX_TAG=0x2C,INT2BIN_TAG=0x2D,INT2DEC_TAG=0x2E,INT2HEX_TAG=0x2F,DET_TOL_TAG=0x30,REF_TOL_TAG=0x31,RREF_TOL_TAG=0x32,SIMULT_TOL_TAG=0x33,GETCONFG_TAG=0x34,V_AUGMENT_TAG=0x35
#if MIN_AMS>=200
,MEAN_TWOARG_TAG=0x36,PRODUCT_TWOARG_TAG=0x37,STDDEV_TWOARG_TAG=0x39,SUM_TWOARG_TAG=0x3A,VARIANCE_TWOARG_TAG=0x3A,DELTA_LIST_TAG=0x3B
#endif
#if MIN_AMS>=207
,ISCLKON_TAG=0x46,GETDATE_TAG=0x47,GETTIME_TAG=0x48,GETTMZN_TAG=0x49,SETDATE_TAG=0x4A,SETTIME_TAG=0x4B,SETTMZN_TAG=0x4C,DAYOFWK_TAG=0x4D,STARTTMR_TAG=0x4E,CHECKTMR_TAG=0x4F
#endif
,TIMECNV_TAG=0x50
#if MIN_AMS>=207
,GETDTFMT_TAG=0x51,GETTMFMT_TAG=0x52,GETDTSTR_TAG=0x53,GETTMSTR_TAG=0x54,SETDTFMT_TAG=0x55,SETTMFMT_TAG=0x56
#endif
,INDIRECTION_TAG=1,CONVERT_TAG=5,STR_TO_EXPR_TAG=7,CONCATENATE_TAG=20,TO_DD_TAG=21,TO_DMS_TAG=22,TO_RECT_TAG=23,TO_POLAR_TAG=24,TO_CYLIND_TAG=25,TO_SPHERE_TAG=26,POLAR_COMPLEX_TAG=38,BIN_NUM_TAG=43,HEX_NUM_TAG=44,TO_BIN_TAG=45,TO_DEC_TAG=46,TO_HEX_TAG=47,SIMULT_EQ_TAG=51,ROWAUG_TAG=53
#if MIN_AMS>=200
,MEAN_2ARG_TAG=54,PRODLIST_3ARG_TAG=55,STDDEV_2ARG_TAG=56,SUMLIST_3ARG_TAG=57,VARIANCE_2ARG_TAG=58,DELTLIST_TAG=59
#endif
};
enum InstructionTags{CLRDRAW_ITAG=1,CLRGRAPH_ITAG=2,CLRHOME_ITAG=3,CLRIO_ITAG=4,CLRTABLE_ITAG=5,CUSTOM_ITAG=6,CYCLE_ITAG=7,DIALOG_ITAG=8,DISPG_ITAG=9,DISPTBL_ITAG=0xA,ELSE_ITAG=0xB,ENDCUSTM_ITAG=0xC,ENDDLOG_ITAG=0xD,ENDFOR_ITAG=0xE,ENDFUNC_ITAG=0xF,ENDIF_ITAG=0x10,ENDLOOP_ITAG=0x11,ENDPRGM_ITAG=0x12,ENDTBAR_ITAG=0x13,ENDTRY_ITAG=0x14,ENDWHILE_ITAG=0x15,EXIT_ITAG=0x16,FUNC_ITAG=0x17,LOOP_ITAG=0x18,PRGM_ITAG=0x19,SHOWSTAT_ITAG=0x1A,STOP_ITAG=0x1B,THEN_ITAG=0x1C,TOOLBAR_ITAG=0x1D,TRACE_ITAG=0x1E,TRY_ITAG=0x1F,ZOOMBOX_ITAG=0x20,ZOOMDATA_ITAG=0x21,ZOOMDEC_ITAG=0x22,ZOOMFIT_ITAG=0x23,ZOOMIN_ITAG=0x24,ZOOMINT_ITAG=0x25,ZOOMOUT_ITAG=0x26,ZOOMPREV_ITAG=0x27,ZOOMRCL_ITAG=0x28,ZOOMSQR_ITAG=0x29,ZOOMSTD_ITAG=0x2A,ZOOMSTO_ITAG=0x2B,ZOOMTRIG_ITAG=0x2C,DRAWFUNC_ITAG=0x2D,DRAWINV_ITAG=0x2E,GOTO_ITAG=0x2F,LBL_ITAG=0x30,GET_ITAG=0x31,SEND_ITAG=0x32,GETCALC_ITAG=0x33,SENDCALC_ITAG=0x34,NEWFOLD_ITAG=0x35,PRINTOBJ_ITAG=0x36,RCLGDB_ITAG=0x37,STOGDB_ITAG=0x38,ELSEIF_ITAG=0x39,IF_ITAG=0x3A,IFTHEN_ITAG=0x3B,RANDSEED_ITAG=0x3C,WHILE_ITAG=0x3D,LINETAN_ITAG=0x3E,COPYVAR_ITAG=0x3F,RENAME_ITAG=0x40,STYLE_ITAG=0x41,FILL_ITAG=0x42,REQUEST_ITAG=0x43,POPUP_ITAG=0x44,PTCHG_ITAG=0x45,PTOFF_ITAG=0x46,PTON_ITAG=0x47,PXLCHG_ITAG=0x48,PXLOFF_ITAG=0x49,PXLON_ITAG=0x4A,MOVEVAR_ITAG=0x4B,DROPDOWN_ITAG=0x4C,OUTPUT_ITAG=0x4D,PTTEXT_ITAG=0x4E,PXLTEXT_ITAG=0x4F,DRAWSLP_ITAG=0x50,PAUSE_ITAG=0x51,RETURN_ITAG=0x52,INPUT_ITAG=0x53,PLOTSOFF_ITAG=0x54,PLOTSON_ITAG=0x55,TITLE_ITAG=0x56,ITEM_ITAG=0x57,INPUTSTR_ITAG=0x58,LINEHORZ_ITAG=0x59,LINEVERT_ITAG=0x5A,PXLHORZ_ITAG=0x5B,PXLVERT_ITAG=0x5C,ANDPIC_ITAG=0x5D,RCLPIC_ITAG=0x5E,RPLCPIC_ITAG=0x5F,XORPIC_ITAG=0x60,DRAWPOL_ITAG=0x61,TEXT_ITAG=0x62,ONEVAR_ITAG=0x63,STOPIC_ITAG=0x64,GRAPH_ITAG=0x65,TABLE_ITAG=0x66,NEWPIC_ITAG=0x67,DRAWPARM_ITAG=0x68,CYCLEPIC_ITAG=0x69,CUBICREG_ITAG=0x6A,EXPREG_ITAG=0x6B,LINREG_ITAG=0x6C,LNREG_ITAG=0x6D,MEDMED_ITAG=0x6E,POWERREG_ITAG=0x6F,QUADREG_ITAG=0x70,QUARTREG_ITAG=0x71,TWOVAR_ITAG=0x72,SHADE_ITAG=0x73,FOR_ITAG=0x74,CIRCLE_ITAG=0x75,PXLCRCL_ITAG=0x76,NEWPLOT_ITAG=0x77,LINE_ITAG=0x78,PXLLINE_ITAG=0x79,DISP_ITAG=0x7A,FNOFF_ITAG=0x7B,FNON_ITAG=0x7C,LOCAL_ITAG=0x7D,DELFOLD_ITAG=0x7E,DELVAR_ITAG=0x7F,LOCK_ITAG=0x80,PROMPT_ITAG=0x81,SORTA_ITAG=0x82,SORTD_ITAG=0x83,UNLOCK_ITAG=0x84,NEWDATA_ITAG=0x85,DEFINE_ITAG=0x86,ELSE_TRY_ITAG=0x87,CLRERR_ITAG=0x88,PASSERR_ITAG=0x89,DISPHOME_ITAG=0x8A,EXEC_ITAG=0x8B,ARCHIVE_ITAG=0x8C,UNARCHIV_ITAG=0x8D,LU_ITAG=0x8E,QR_ITAG=0x8F,BLDDATA_ITAG=0x90,DRWCTOUR_ITAG=0x91,NEWPROB_ITAG=0x92,SINREG_ITAG=0x93,LOGISTIC_ITAG=0x94,CUSTMON_ITAG=0x95,CUSTMOFF_ITAG=0x96,SENDCHAT_ITAG=0x97
#if MIN_AMS>=207
,REQUEST_THREEARG_TAG=0x99,REQUEST_THREEARG_ITAG=0x99,CLOCKON_ITAG=0x9A,CLOCKOFF_ITAG=0x9B
#endif
,CLRDRAW_TAG=1,CLRGRAPH_TAG=2,CLRHOME_TAG=3,CLRIO_TAG=4,CLRTABLE_TAG=5,CUSTOM_TAG=6,CYCLE_TAG=7,DIALOG_TAG=8,DISPG_TAG=9,DISPTBL_TAG=10,ELSE_TAG=11,ENDCUSTM_TAG=12,ENDDLOG_TAG=13,ENDFOR_TAG=14,ENDFUNC_TAG=15,ENDIF_TAG=16,ENDLOOP_TAG=17,ENDPRGM_TAG=18,ENDTBAR_TAG=19,ENDTRY_TAG=20,ENDWHILE_TAG=21,EXIT_TAG=22,FUNC_BEGIN_TAG=23,LOOP_TAG=24,PRGM_TAG=25,SHOWSTAT_TAG=26,STOP_TAG=27,THEN_TAG=28,TOOLBAR_TAG=29,TRACE_TAG=30,TRY_TAG=31,ZOOMBOX_TAG=32,ZOOMDATA_TAG=33,ZOOMDEC_TAG=34,ZOOMFIT_TAG=35,ZOOMIN_TAG=36,ZOOMINT_TAG=37,ZOOMOUT_TAG=38,ZOOMPREV_TAG=39,ZOOMRCL_TAG=40,ZOOMSQR_TAG=41,ZOOMSTD_TAG=42,ZOOMSTO_TAG=43,ZOOMTRIG_TAG=44,DRAWFUNC_TAG=45,DRAWINV_TAG=46,GOTO_TAG=47,LBL_TAG=48,GET_TAG=49,SEND_TAG=50,GETCALC_TAG=51,SENDCALC_TAG=52,NEWFOLD_TAG=53,RCLGDB_TAG=55,STOGDB_TAG=56,ELSEIF_TAG=57,IF_TAG=58,IFTHEN_TAG=59,RANDSEED_TAG=60,WHILE_TAG=61,LINETAN_TAG=62,COPYVAR_TAG=63,RENAME_TAG=64,TYLE_TAG=65,FILL_TAG=66,REQUEST_TAG=67,POPUP_TAG=68,PTCHG_TAG=69,PTOFF_TAG=70,PTON_TAG=71,PXLCHG_TAG=72,PXLOFF_TAG=73,PXLON_TAG=74,MOVEVAR_TAG=75,DROPDOWN_TAG=76,OUTPUT_TAG=77,PTTEXT_TAG=78,PXLTEXT_TAG=79,DRAWSLP_TAG=80,PAUSE_TAG=81,RETURN_TAG=82,INPUT_TAG=83,PLOTSOFF_TAG=84,PLOTSON_TAG=85,TITLE_TAG=86,ITEM_TAG=87,INPUTSTR_TAG=88,LINEHORZ_TAG=89,LINEVERT_TAG=90,PXLHORZ_TAG=91,PXLVERT_TAG=92,ANDPIC_TAG=93,RCLPIC_TAG=94,RPLCPIC_TAG=95,XORPIC_TAG=96,DRAWPOL_TAG=97,ONEVAR_TAG=99,STOPIC_TAG=100,GRAPH_TAG=101,TABLE_TAG=102,NEWPIC_TAG=103,DRAWPARM_TAG=104,CYCLEPIC_TAG=105,CUBICREG_TAG=106,EXPREG_TAG=107,LINREG_TAG=108,LNREG_TAG=109,MEDMED_TAG=110,POWERREG_TAG=111,QUADREG_TAG=112,QUARTREG_TAG=113,TWOVAR_TAG=114,SHADE_TAG=115,FOR_TAG=116,CIRCLE_TAG=117,PXLCIRCLE_TAG=118,NEWPLOT_TAG=119,LINE_TAG=120,PXLLINE_TAG=121,DISP_TAG=122,FNOFF_TAG=123,FNON_TAG=124,LOCAL_TAG=125,DELFOLD_TAG=126,DELVAR_TAG=127,LOCK_TAG=128,PROMPT_TAG=129,SORTA_TAG=130,SORTD_TAG=131,UNLOCK_TAG=132,NEWDATA_TAG=133,DEFINE_TAG=134,TRYELSE_TAG=135,CLRERR_TAG=136,PASSERR_TAG=137,DISPHOME_TAG=138,EXEC_TAG=139,ARCHIVE_TAG=140,UNARCHIV_TAG=141,LU_TAG=142,QR_TAG=143,BLDDATA_TAG=144,DRWCTOUR_TAG=145,NEWPROB_TAG=146,SINREG_TAG=147,LOGISTIC_TAG=148,CUSTMON_TAG=149,CUSTMOFF_TAG=150,SENDCHAT_TAG=151
#if MIN_AMS>=207
,REQUEST3_TAG=153,CLOCKON_TAG=154,CLOCKOFF_TAG=155
#endif
,LOCAL_FUNC_TAG=248,LOCAL_PRGM_TAG=249};
#ifndef __HAVE_MULTI_EXPR
#define __HAVE_MULTI_EXPR
typedef struct{unsigned short Size;ESQ Expr[];}MULTI_EXPR;
#endif
#define Quantum ESQ
#ifndef __HAVE_SYM_STR
#define __HAVE_SYM_STR
typedef CESI SYM_STR;
#endif
enum SysvarTags{X_BAR_TAG=1,Y_BAR_TAG=2,SIGMA_X_TAG=3,SIGMA_X2_TAG=4,SIGMA_Y_TAG=5,SIGMA_Y2_TAG=6,SIGMA_XY_TAG=7,SX_TAG=8,SY_TAG=9,SMLSIGMA_X_TAG=0xA,SMLSIGMA_Y_TAG=0xB,NSTAT_TAG=0xC,MINX_TAG=0xD,MINY_TAG=0xE,Q1_TAG=0xF,MEDSTAT_TAG=0x10,Q3_TAG=0x11,MAXX_TAG=0x12,MAXY_TAG=0x13,CORR_TAG=0x14,R2_TAG=0x15,MEDX1_TAG=0x16,MEDX2_TAG=0x17,MEDX3_TAG=0x18,MEDY1_TAG=0x19,MEDY2_TAG=0x1A,MEDY3_TAG=0x1B,XC_TAG=0x1C,YC_TAG=0x1D,ZC_TAG=0x1E,TC_TAG=0x1F,RC_TAG=0x20,THETA_C_TAG=0x21,NC_TAG=0x22,XFACT_TAG=0x23,YFACT_TAG=0x24,ZFACT_TAG=0x25,XMIN_TAG=0x26,XMAX_TAG=0x27,XSCL_TAG=0x28,YMIN_TAG=0x29,YMAX_TAG=0x2A,YSCL_TAG=0x2B,DELTA_X_TAG=0x2C,DELTA_Y_TAG=0x2D,XRES_TAG=0x2E,XGRID_TAG=0x2F,YGRID_TAG=0x30,ZMIN_TAG=0x31,ZMAX_TAG=0x32,ZSCL_TAG=0x33,EYE_THETA_TAG=0x34,EYE_PHI_TAG=0x35,THETA_MIN_TAG=0x36,THETA_MAX_TAG=0x37,THETA_STEP_TAG=0x38,TMIN_TAG=0x39,TMAX_TAG=0x3A,TSTEP_TAG=0x3B,NMIN_TAG=0x3C,NMAX_TAG=0x3D,PLOTSTRT_TAG=0x3E,PLOTSTEP_TAG=0x3F,ZXMIN_TAG=0x40,ZXMAX_TAG=0x41,ZXSCL_TAG=0x42,ZYMIN_TAG=0x43,ZYMAX_TAG=0x44,ZYSCL_TAG=0x45,ZXRES_TAG=0x46,Z_THETA_MIN_TAG=0x47,Z_THETA_MAX_TAG=0x48,Z_THETA_STEP_TAG=0x49,ZTMIN_TAG=0x4A,ZTMAX_TAG=0x4B,ZTSTEP_TAG=0x4C,ZXGRID_TAG=0x4D,ZYGRID_TAG=0x4E,ZZMIN_TAG=0x4F,ZZMAX_TAG=0x50,ZZSCL_TAG=0x51,ZEYE_THETA_TAG=0x52,ZEYE_PHI_TAG=0x53,ZNMIN_TAG=0x54,ZNMAX_TAG=0x55,ZPLTSTEP_TAG=0x56,ZPLTSTRT_TAG=0x57,SEED1_TAG=0x58,SEED2_TAG=0x59,OK_TAG=0x5A,ERRORNUM_TAG=0x5B,SYSMATH_TAG=0x5C,SYSDATA_TAG=0x5D,REGEQ_TAG=0x5E,REGCOEF_TAG=0x5F,TBLINPUT_TAG=0x60,TBLSTART_TAG=0x61,DELTA_TBL_TAG=0x62,FLDPIC_TAG=0x63,EYE_PSI_TAG=0x64,TPLOT_TAG=0x65,DIFTOL_TAG=0x66,ZEYE_PSI_TAG=0x67,T0_TAG=0x68,DTIME_TAG=0x69,NCURVES_TAG=0x6A,FLDRES_TAG=0x6B,ESTEP_TAG=0x6C,ZT0DE_TAG=0x6D,ZTMAXDE_TAG=0x6E,ZTSTEPDE_TAG=0x6F,ZTPLOTDE_TAG=0x70,NCONTOUR_TAG=0x71};
#ifndef __HAVE_Tags
#define __HAVE_Tags
enum Tags{VAR_NAME_TAG=0x00,_VAR_Q_TAG=0x01,VAR_R_TAG=0x02,VAR_S_TAG=0x03,VAR_T_TAG=0x04,VAR_U_TAG=0x05,VAR_V_TAG=0x06,VAR_W_TAG=0x07,VAR_X_TAG=0x08,VAR_Y_TAG=0x09,VAR_Z_TAG=0x0A,VAR_A_TAG=0x0B,VAR_B_TAG=0x0C,VAR_C_TAG=0x0D,VAR_D_TAG=0x0E,VAR_E_TAG=0x0F,VAR_F_TAG=0x10,VAR_G_TAG=0x11,VAR_H_TAG=0x12,VAR_I_TAG=0x13,VAR_J_TAG=0x14,VAR_K_TAG=0x15,VAR_L_TAG=0x16,VAR_M_TAG=0x17,VAR_N_TAG=0x18,VAR_O_TAG=0x19,VAR_P_TAG=0x1A,VAR_Q_TAG=0x1B,EXT_SYSTEM_TAG=0x1C,ARB_REAL_TAG=0x1D,ARB_INT_TAG=0x1E,POSINT_TAG=0x1F,NEGINT_TAG=0x20,POSFRAC_TAG=0x21,NEGFRAC_TAG=0x22,FLOAT_TAG=0x23,BCD_TAG=0x23,PI_TAG=0x24,EXP_TAG=0x25,IM_TAG=0x26,NEGINFINITY_TAG=0x27,INFINITY_TAG=0x28,PN_INFINITY_TAG=0x29,UNDEF_TAG=0x2A,FALSE_TAG=0x2B,TRUE_TAG=0x2C,STR_TAG=0x2D,NOTHING_TAG=0x2E,ACOSH_TAG=0x2F,ASINH_TAG=0x30,ATANH_TAG=0x31
#if MIN_AMS>=208
,ASECH_TAG=0x32,ACSCH_TAG=0x33,ACOTH_TAG=0x34
#endif
,COSH_TAG=0x35,SINH_TAG=0x36,TANH_TAG=0x37
#if MIN_AMS>=208
,SECH_TAG=0x38,CSCH_TAG=0x39,COTH_TAG=0x3A
#endif
,ACOS_TAG=0x3B,ASIN_TAG=0x3C,ATAN_TAG=0x3D
#if MIN_AMS>=208
,ASEC_TAG=0x3E,ACSC_TAG=0x3F,ACOT_TAG=0x40
#endif
,RACOS_TAG=0x41,RASIN_TAG=0x42,RATAN_TAG=0x43,COS_TAG=0x44,SIN_TAG=0x45,TAN_TAG=0x46
#if MIN_AMS>=208
,SEC_TAG=0x47,CSC_TAG=0x48,COT_TAG=0x49
#endif
,ITAN_TAG=0x4A,ABS_TAG=0x4B,ANGLE_TAG=0x4C,CEILING_TAG=0x4D,FLOOR_TAG=0x4E,INT_TAG=0x4F,SIGN_TAG=0x50,SQRT_TAG=0x51,EXPF_TAG=0x52,LN_TAG=0x53,LOG_TAG=0x54,FPART_TAG=0x55,IPART_TAG=0x56,CONJ_TAG=0x57,IMAG_TAG=0x58,REAL_TAG=0x59,APPROX_TAG=0x5A,TEXPAND_TAG=0x5B,TCOLLECT_TAG=0x5C,GETDENOM_TAG=0x5D,GETNUM_TAG=0x5E,ERROR_TAG=0x5F,CUMSUM_TAG=0x60,DET_TAG=0x61,COLNORM_TAG=0x62,ROWNORM_TAG=0x63,NORM_TAG=0x64,MEAN_TAG=0x65,MEDIAN_TAG=0x66,PRODUCT_TAG=0x67,STDDEV_TAG=0x68,SUM_TAG=0x69,VARIANCE_TAG=0x6A,UNITV_TAG=0x6B,DIM_TAG=0x6C,MAT2LIST_TAG=0x6D,NEWLIST_TAG=0x6E,RREF_TAG=0x6F,REF_TAG=0x70,IDENTITY_TAG=0x71,DIAG_TAG=0x72,COLDIM_TAG=0x73,ROWDIM_TAG=0x74,TRANSPOSE_TAG=0x75,FACTORIAL_TAG=0x76,PERCENT_TAG=0x77,RADIANS_TAG=0x78,NOT_TAG=0x79,MINUS_TAG=0x7A,VEC_POLAR_TAG=0x7B,VEC_CYLIND_TAG=0x7C,VEC_SPHERE_TAG=0x7D,START_TAG=0x7E,ISTORE_TAG=0x7F,STORE_TAG=0x80,WITH_TAG=0x81,XOR_TAG=0x82,OR_TAG=0x83,AND_TAG=0x84,LT_TAG=0x85,LE_TAG=0x86,EQ_TAG=0x87,GE_TAG=0x88,GT_TAG=0x89,NE_TAG=0x8A,ADD_TAG=0x8B,ADDELT_TAG=0x8C,SUB_TAG=0x8D,SUBELT_TAG=0x8E,MUL_TAG=0x8F,MULELT_TAG=0x90,DIV_TAG=0x91,DIVELT_TAG=0x92,POW_TAG=0x93,POWELT_TAG=0x94,SINCOS_TAG=0x95,SOLVE_TAG=0x96,CSOLVE_TAG=0x97,NSOLVE_TAG=0x98,ZEROS_TAG=0x99,CZEROS_TAG=0x9A,FMIN_TAG=0x9B,FMAX_TAG=0x9C,COMPLEX_TAG=0x9D,POLYEVAL_TAG=0x9E,RANDPOLY_TAG=0x9F,CROSSP_TAG=0xA0,DOTP_TAG=0xA1,GCD_TAG=0xA2,LCM_TAG=0xA3,MOD_TAG=0xA4,INTDIV_TAG=0xA5,REMAIN_TAG=0xA6,NCR_TAG=0xA7,NPR_TAG=0xA8,P2RX_TAG=0xA9,P2RY_TAG=0xAA,P2PTHETA_TAG=0xAB,P2PR_TAG=0xAC,AUGMENT_TAG=0xAD,NEWMAT_TAG=0xAE,RANDMAT_TAG=0xAF,SIMULT_TAG=0xB0,PART_TAG=0xB1,EXP2LIST_TAG=0xB2,RANDNORM_TAG=0xB3,MROW_TAG=0xB4,ROWADD_TAG=0xB5,ROWSWAP_TAG=0xB6,ARCLEN_TAG=0xB7,NINT_TAG=0xB8,PI_PRODUCT_TAG=0xB9,SIGMA_SUM_TAG=0xBA,MROWADD_TAG=0xBB,ANS_TAG=0xBC,ENTRY_TAG=0xBD,EXACT_TAG=0xBE,LOGB_TAG=0xBF,COMDENOM_TAG=0xC0,EXPAND_TAG=0xC1,FACTOR_TAG=0xC2,CFACTOR_TAG=0xC3,INTEGRATE_TAG=0xC4,DIFFERENTIATE_TAG=0xC5,AVGRC_TAG=0xC6,NDERIV_TAG=0xC7,TAYLOR_TAG=0xC8,LIMIT_TAG=0xC9,PROPFRAC_TAG=0xCA,WHEN_TAG=0xCB,ROUND_TAG=0xCC,DMS_TAG=0xCD,LEFT_TAG=0xCE,RIGHT_TAG=0xCF,MID_TAG=0xD0,SHIFT_TAG=0xD1,SEQ_TAG=0xD2,LIST2MAT_TAG=0xD3,SUBMAT_TAG=0xD4,SUBSCRIPT_TAG=0xD5,RAND_TAG=0xD6,MIN_TAG=0xD7,MAX_TAG=0xD8,LIST_TAG=0xD9,USERFUNC_TAG=0xDA,MATRIX_TAG=0xDB,FUNC_TAG=0xDC,DATA_TAG=0xDD,GDB_TAG=0xDE,PIC_TAG=0xDF,TEXT_TAG=0xE0,FIG_TAG=0xE1,MAC_TAG=0xE2,EXT_TAG=0xE3,EXT_INSTR_TAG=0xE4,END_TAG=0xE5,COMMENT_TAG=0xE6,NEXTEXPR_TAG=0xE7,NEWLINE_TAG=0xE8,ENDSTACK_TAG=0xE9,PN1_TAG=0xEA,PN2_TAG=0xEB,ERROR_MSG_TAG=0xEC,EIGVC_TAG=0xED,EIGVL_TAG=0xEE,DASH_TAG=0xEF,LOCALVAR_TAG=0xF0,DESOLVE_TAG=0xF1,FDASH_TAG=0xF2,ASM_TAG=0xF3,ISPRIME_TAG=0xF4,OTH_TAG=0xF8,ROTATE_TAG=0xF9,VAR_TAG=0,PRIVILEDGED_VAR_TAG=1,R_VAR_TAG=2,S_VAR_TAG=3,T_VAR_TAG=4,U_VAR_TAG=5,V_VAR_TAG=6,W_VAR_TAG=7,X_VAR_TAG=8,Y_VAR_TAG=9,Z_VAR_TAG=10,A_VAR_TAG=11,B_VAR_TAG=12,C_VAR_TAG=13,D_VAR_TAG=14,E_VAR_TAG=15,F_VAR_TAG=16,G_VAR_TAG=17,H_VAR_TAG=18,I_VAR_TAG=19,J_VAR_TAG=20,K_VAR_TAG=21,L_VAR_TAG=22,M_VAR_TAG=23,N_VAR_TAG=24,O_VAR_TAG=25,P_VAR_TAG=26,Q_VAR_TAG=27,SYSVAR_TAG=28,NONNEGATIVE_INTEGER_TAG=31,NEGATIVE_INTEGER_TAG=32,POSITIVE_FRACTION_TAG=33,NEGATIVE_FRACTION_TAG=34,E_TAG=37,I_TAG=38,MINUS_INFINITY_TAG=39,PLUS_INFINITY_TAG=40,PLUS_OR_MINUS_INFINITY_TAG=41,UNDEFINED_TAG=42,STR_DATA_TAG=45,OMITTED_ARG_TAG=46,ACOS_RAD_TAG=65,ASIN_RAD_TAG=66,ATAN_RAD_TAG=67,TAN_RAD_TAG=74,PHASE_TAG=76,GREATEST_INT_TAG=79,LOG10_TAG=84,RAC_PART_TAG=85,INT_PART_TAG=86,IM_PART_TAG=88,RE_PART_TAG=89,APPROXIMATE_TAG=90,DENR_TAG=93,NUMR_TAG=94,DETERMINANT_TAG=97,MATNORM_TAG=100,PRODLIST_TAG=103,SUMLIST_TAG=105,DIMENSION_TAG=108,MAT_TO_LIST_TAG=109,RED_ROW_ECH_TAG=111,ROW_ECHELON_TAG=112,IDENTITY_MAT_TAG=113,CHS_TAG=122,POLARVEC_TAG=123,CYLINVEC_TAG=124,SPHERVEC_TAG=125,ASSIGN_TAG=127,SUCH_THAT_TAG=129,EQUATION_TAG=135,DOT_ADD_TAG=140,SUBTRACT_TAG=141,DOT_SUB_TAG=142,MULTIPLY_TAG=143,DOT_MULT_TAG=144,DIVIDE_TAG=145,DOT_DIV_TAG=146,EXPONENTIATION_TAG=147,DOT_EXPONENTIATE_TAG=148,SIN2_TAG=149,MIN_PT_TAG=155,MAX_PT_TAG=156,IM_RE_TAG=157,DENSE_POLY_EVAL_TAG=158,CROSS_PROD_TAG=160,DOTPRODUCT_TAG=161,INT_GCD_TAG=162,INT_LCM_TAG=163,INT_QUOTIENT_TAG=165,INT_REMAINDER_TAG=166,COMB_TAG=167,PERM_TAG=168,PLR_TO_X_TAG=169,PLR_TO_Y_TAG=170,REC_TO_ANGLE_TAG=171,REC_TO_RADIUS_TAG=172,EXP_TO_LIST_TAG=178,EXTENDED_PROD_TAG=185,SUMMATION_TAG=186,RATIONALIZE_TAG=190,LOG_GEN_TAG=191,COM_DEN_TAG=192,INTEGRAL_TAG=196,DIV_DIF_1F_TAG=198,DIV_DIF_1C_TAG=199,SERIES_TAG=200,LIM_TAG=201,DEGREES_TAG=205,SEQUENCE_TAG=210,LIST_TO_MAT_TAG=211,USER_FUN_TAG=218,USER_DEF_TAG=220,DATA_VAR_TAG=221,GDB_VAR_TAG=222,PIC_VAR_TAG=223,TEXT_VAR_TAG=224,GEO_FILE_TAG=225,GEO_MACRO_TAG=226,SECONDARY_TAG=227,COMMAND_TAG=228,SEPARATOR_TAG=231,EOL_TAG=232,END_OF_SEGMENT_TAG=233,UNARY_PLUS_OR_MINUS_TAG=234,BINARY_PLUS_OR_MINUS_TAG=235,PRIME_TAG=239,PARM_TAG=240,FUNC_DIF_TAG=242,ASM_PRGM_TAG=243,GEN_DATA_TAG=248};
#endif
enum TokenizeSymNameFlags{TSF_FULLY_QUALIFIED=0x01,TSF_ALLOW_RESERVED=0x02,TSF_PASS_ERRORS=0x04};
#define bottom_estack (*((CESI*)(_rom_call_addr_hack(432,((&top_estack-2)),200))))
#define top_estack (*((ESI*)(_rom_call_addr(109))))
#define display_statements _rom_call(HANDLE,(CESI,short,short),4E)
#define ESTACK(idx) (*(idx))
#define HS_popEStack _rom_call(HANDLE,(void),244)
#define HToESI _rom_call(ESI,(HANDLE),247)
#define next_expression_index _rom_call(ESI,(CESI),10A)
#define NG_approxESI ({__need_in_use_bit;_rom_call(void,(CESI),25C);})
#define NG_execute ({__need_in_use_bit;_rom_call(void,(HANDLE,short),25D);})
#define NG_graphESI ({__need_in_use_bit;_rom_call(void,(CESI,HANDLE),25E);})
#define NG_rationalESI ({__need_in_use_bit;_rom_call(void,(CESI),25F);})
#define NG_RPNToText _rom_call(HANDLE,(HANDLE,short,short),25B)
#define NG_tokenize _rom_call(short,(HANDLE,__pushort,__pushort),260)
#define Parms2D _rom_call(void,(CESI,__pshort,__pshort,__pshort),4D)
#define Parse1DExpr _rom_call(HANDLE,(CESI,short,short),4F)
#define Parse2DExpr _rom_call(ESI,(CESI,short),4A)
#define Parse2DMultiExpr _rom_call(ESI,(HANDLE,short),4B)
#define Print2DExpr _rom_call(void,(CESI,WINDOW*,short,short),4C)
#define push_END_TAG _rom_call(void,(void),263)
#define push_LIST_TAG _rom_call(void,(void),264)
#define push_quantum(tag) (MIN_AMS<101?(void)(*(++top_estack)=tag):_rom_call(void,(ESQ),2EE)(tag))
#define TokenizeSymName _rom_call(ESI,(const char*,short),80)
#if MIN_AMS>=101
#define all_tail _rom_call(short,(CESI_Callback_t,ESI),3B5)
#define any_tail _rom_call(short,(CESI_Callback_t,ESI),3B6)
#define are_expressions_identical _rom_call(short,(CESI,CESI),2BF)
#define can_be_approxed ({__need_in_use_bit;_rom_call(short,(CESI,short),2F5);})
#define check_estack_size _rom_call(void,(short),2C2)
#define compare_complex_magnitudes ({__need_in_use_bit;_rom_call(short,(CESI,CESI),2F6);})
#define compare_expressions _rom_call(short,(CESI,CESI),2C0)
#define compare_Floats _rom_call(long,(CESI,CESI),2F7)
#define delete_between _rom_call(void,(ESI,ESI),2C3)
#define delete_expression _rom_call(void,(ESI),2C5)
#define deleted_between _rom_call(unsigned short,(ESI,ESI),2C4)
#define deleted_expression _rom_call(unsigned short,(ESI),2C6)
#define did_push_cnvrt_Float_to_integer _rom_call(short,(CESI),2F8)
#define estack_number_to_Float(x) _tios_float_1(2F9,x,CESI)
#define estack_to_short _rom_call(short,(CESI,__pshort),2C7)
#define estack_to_ushort _rom_call(short,(CESI,__pushort),2C8)
#define factor_base_index _rom_call(ESI,(CESI),2C9)
#define factor_exponent_index _rom_call(ESI,(CESI),2CA)
#define gcd_exact_whole_Floats(x,y) _tios_float_2(2FC,x,y,CESI,CESI)
#define get_key_ptr _rom_call(char*,(ESQ,ESQ),2B7)
#define GetValue _rom_call(long,(CESI,long,long),2CB)
#define im_index _rom_call(ESI,(CESI),2CC)
#define index_below_display_expression_aux _rom_call(ESI,(CESI),2B6)
#define index_main_var _rom_call(ESI,(CESI),2CF)
#define index_numeric_term _rom_call(ESI,(CESI),2CD)
#define index_of_lead_base_of_lead_term _rom_call(ESI,(CESI),2CE)
#define is_advanced_tag _rom_call(short,(ESQ),2D0)
#define is_antisymmetric ({__need_in_use_bit;_rom_call(short,(CESI,CESI),2D1);})
#define is_complex0 _rom_call(short,(CESI),2D3)
#define is_complex_number _rom_call(short,(CESI),2D2)
#define is_Float_exact_whole_number _rom_call(short,(CESI),2FE)
#define is_free_of_tag _rom_call(short,(CESI,ESQ),2D4)
#define is_independent_of_de_seq_vars _rom_call(short,(CESI),2D6)
#define is_independent_of_elements _rom_call(short,(CESI,CESI),2D8)
#define is_independent_of_tail _rom_call(short,(CESI,CESI),2D7)
#define is_independent_of _rom_call(short,(CESI,CESI),2D5)
#define is_matrix _rom_call(short,(CESI),3B7)
#define is_monomial_in_kernel _rom_call(short,(CESI),2DA)
#define is_monomial _rom_call(short,(CESI),2D9)
#define is_narrowly_independent_of _rom_call(short,(CESI,CESI),2DB)
#define is_square_matrix _rom_call(short,(CESI),3B8)
#define is_symmetric ({__need_in_use_bit;_rom_call(short,(CESI,CESI),2DC);})
#define is_tail_independent_of _rom_call(short,(CESI,CESI),2DD)
#define is_valid_smap_aggregate _rom_call(short,(CESI),3B9)
#define last_element_index _rom_call(ESI,(CESI),3BA)
#define lead_base_index _rom_call(ESI,(CESI),2DE)
#define lead_exponent_index _rom_call(ESI,(CESI),2DF)
#define lead_factor_index _rom_call(ESI,(CESI),2E0)
#define lead_term_index _rom_call(ESI,(CESI),2E1)
#define likely_approx_to_complex_number _rom_call(short,(CESI),307)
#define likely_approx_to_number _rom_call(short,(CESI),308)
#define main_gen_var_index _rom_call(ESI,(CESI),2E2)
#define map_tail_Int _rom_call(short,(ESI_Callback_Int_t,ESI,short),3BC)
#define map_tail _rom_call(void,(ESI_Callback_t,ESI),3BB)
#define map_unary_over_comparison _rom_call(void,(ESI_Callback_t,ESI),2E3)
#define min_quantum _rom_call(ESQ,(ESQ,ESQ),2E4)
#define move_between_to_top _rom_call(void,(ESI,ESI),2E5)
#define moved_between_to_top _rom_call(unsigned short,(ESI,ESI),2E6)
#define norm1_complex_Float(x) _tios_float_1(309,x,CESI)
#define numeric_factor_index _rom_call(ESI,(CESI),2E7)
#define push_ANSI_string push_zstr
#define push_between _rom_call(void,(void*,void*),2E8)
#define push_cnvrt_integer_if_whole_nmb _rom_call(void,(CESI),30D)
#define push_expr_quantum _rom_call(void,(CESI,ESQ),2E9)
#define push_expr2_quantum _rom_call(void,(CESI,CESI,ESQ),2EA)
#define push_expression(ptr) (MIN_AMS<200?({push_expr_quantum(ptr,NOTHING_TAG);(void)(--top_estack);}):_rom_call(void,(CESI),44D)(ptr))
#define push_Float_to_nonneg_int _rom_call(void,(float),30B)
#define push_Float_to_rat _rom_call(void,(CESI),30C)
#define push_Float _rom_call(void,(float),30A)
#define push_internal_simplify ({__need_in_use_bit;_rom_call_hack(void,(CESI),4F8,(*((void*const*)(((const char*)(_rom_call_addr(385)))+22))),200);})
extern void push_longint(long)__ATTR_LIB_ASM__;
extern void push_longlongint(long long)__ATTR_LIB_ASM__;
#define push_next_arb_int _rom_call(void,(void),2EB)
#define push_next_arb_real _rom_call(void,(void),2EC)
#define push_next_internal_var _rom_call(void,(ESQ),2ED)
#define push_offset_array _rom_call(unsigned short,(CESI,__pushort*),3C4)
#define push_overflow_to_infinity _rom_call(void,(ESQ),30E)
#define push_parse_text _rom_call(short,(const char*),3CA)
#define push_quantum_pair _rom_call(void,(ESQ,ESQ),2EF)
#define push_reversed_tail _rom_call(void,(CESI),3BF)
#define push_round_Float _rom_call(void,(CESI),310)
extern void push_shortint(short)__ATTR_LIB_ASM__;
#define push_transpose_aux ({__need_in_use_bit;_rom_call(void,(CESI,short),3C1);})
#define push_zstr(s) (MIN_AMS<200?_push_zstr(s):_rom_call(void,(const char*),48A)(s))
#define re_index _rom_call(ESI,(CESI),2F2)
#define reductum_index _rom_call(ESI,(CESI),2F0)
#define remaining_element_count _rom_call(unsigned short,(CESI),3C3)
#define remaining_factors_index _rom_call(ESI,(CESI),2F1)
#define reset_control_flags _rom_call(void,(void),2F4)
#define reset_estack_size _rom_call(void,(short),2F3)
#define should_and_did_push_approx_arg2 ({__need_in_use_bit;_rom_call(short,(CESI,CESI),311);})
#define signum_Float _rom_call(long,(CESI),312)
#if MIN_AMS>=200
#define is0 _rom_call(short,(CESI),269)
#define is1 _rom_call(short,(CESI),2FD)
#define is_variable _rom_call(short,(CESI),488)
#define push_long_to_integer _rom_call(void,(long),4E3)
#define push_simplify ({__need_in_use_bit;_rom_call(void,(CESI),44E);})
#define push_ulong_to_integer _rom_call(void,(long),4E4)
#define push_ushort_to_integer _rom_call(void,(short),4E5)
#if MIN_AMS>=202
#define add_to_top ({__need_in_use_bit;_rom_call(void,(CESI),518);})
#define add1_to_top ({__need_in_use_bit;_rom_call(void,(void),51D);})
#define and_onto_top ({__need_in_use_bit;_rom_call(void,(CESI),579);})
#define integer_non_unknown ({__need_in_use_bit;_rom_call(short,(CESI),50C);})
#define is_constant _rom_call(short,(CESI),593)
#define is_minus1 _rom_call(short,(CESI),508)
#define is_negative ({__need_in_use_bit;_rom_call(short,(CESI),575);})
#define is_never0 ({__need_in_use_bit;_rom_call(short,(CESI),577);})
#define is_nonnegative ({__need_in_use_bit;_rom_call(short,(CESI),574);})
#define is_nonpositive ({__need_in_use_bit;_rom_call(short,(CESI),576);})
#define is_polynomial_in_var_or_kern _rom_call(short,(CESI,CESI),56B)
#define is_positive ({__need_in_use_bit;_rom_call(short,(CESI),573);})
#define is_totally_polynomial _rom_call(short,(CESI),56C)
#define is_whole_number _rom_call(short,(CESI),50B)
#define negate_top ({__need_in_use_bit;_rom_call(void,(void),525);})
#define subtract_from_top ({__need_in_use_bit;_rom_call(void,(CESI),51B);})
#define subtract1_from_top ({__need_in_use_bit;_rom_call(void,(void),51E);})
#if MIN_AMS>=204
#define ARb_int_count (*((ESQ*)(_rom_call_addr(5C3))))
#define ARb_real_count (*((ESQ*)(_rom_call_addr(5C2))))
#endif
#endif
#endif
#endif
/* End Auto-Generated Part */

#endif
