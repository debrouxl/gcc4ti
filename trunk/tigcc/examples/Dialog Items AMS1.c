// Dialog items example for all AMS versions
// Example contributed by Wazabbe and Sébastien Leurent

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

// #define OPTIMIZE_ROM_CALLS // Use ROM Call Optimization

#define MIN_AMS 100           // Compile for AMS 1.00 or higher

#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files


static HANDLE FirstPopup,SecondPopup, ThirdPopup;
static char EditBuffer[20], *SecondEditBuffer;
static HANDLE BufferHandle;
static unsigned short Options[5];

		extern void MenuWindow;
		asm("
.align 2
MenuWindow:
			dc.w MenuWindowFirstString-MenuWindow,2,5,18+100*0x100,3,0,0,0,-1,-1
			dc.w	MT_TEXT+1,MW_1-MenuWindowFirstString
			dc.w	MT_TEXT+2,MW_2-MenuWindowFirstString
			dc.w	MT_TEXT+3+MT_CASCADE,MW_3-MenuWindowFirstString,MW_sub-MenuWindow
			dc.l	-1
MW_sub: dc.w	MT_TEXT+4,MW_4-MenuWindowFirstString
			dc.w	MT_TEXT+5,MW_5-MenuWindowFirstString
			dc.l	-1
MenuWindowFirstString:
MW_1:	.asciz \"MENU1\"
MW_2:	.asciz \"MENU2\"
MW_3:	.asciz \"SUBITEMS\"
MW_4:	.asciz \"MENU SUBITEM 1\"
MW_5:	.asciz \"MENU SUBITEM 2\"
		");

HANDLE GetPopup( unsigned short dlgId )  /* This function is called by DialogAddDynamicPulldown whenever it is needed */
{
   if(dlgId == 5)
   {
      return FirstPopup;
   }
   else
   {
      return ThirdPopup;
   }
}

long CallBack( short dlgId, long Value )   /* This function is given as a pointer to DialogNew and can be called whenever it is needed */
{
   unsigned short HitKey;
   HANDLE MenuHandle = H_NULL;
   switch( dlgId )
   {
      case 0:
            return TRUE;
      case 1:
            return TRUE;
      case 2:
            MenuHandle = HI_WORD(Value);
            HitKey = LO_WORD(Value);
            int   result = -2;
            switch( HitKey )
            {
               case KEY_F1:
               return 0;
               case KEY_F2:
               return 3;
               case KEY_F3:
                  while (result == -2)
                  {
                     result = MenuKey (MenuHandle, HitKey);
                  }
                  switch(result)
                  {
                      case 4:
                         return 0;
                      case 5:
                         return 3;
                      default:
                         return 0;
                  }
               default:
                  return 1;
            }
            break;
      case 3:
            if (Options[1] == 20)
            {
                  return DB_EXIT;
            }
            else
            {
               return DB_REDRAW_AND_CONTINUE;
            }
            break;
      case 4:
            if (0 == strcmp("EXIT", EditBuffer))
                  return DB_EXIT;
            break;
      case 5:
            if (Options[2] == 20)
                  return DB_EXIT;
            break;
      case 6:
            SecondEditBuffer =  HeapDeref(BufferHandle);
            if (0 == strcmp("EXIT", SecondEditBuffer))
                  return DB_EXIT;
            break;
      case 7:
            if (Options[3] == 30)
            {
                  return DB_EXIT;
            }
            else
            {
               return DB_REDRAW_AND_CONTINUE;
            }
            break;
      case 8:
               return DB_REDRAW_AND_CONTINUE;
            break;
      case 9:
            if (Options[1] == 20)
            {
                  return DB_EXIT;
            }
            else
            {
               return DB_REDRAW_AND_CONTINUE;
            }
            break;
      case DB_QACTIVE:
            return TRUE;
            break;
      case DB_GET_EDIT_HANDLE:
            return BufferHandle;
            break;
      default :
            return TRUE;
            break;
   }
   return TRUE;
}


void _main(void)
{
   static WORD Key;
   BufferHandle = HeapAlloc (20 * sizeof(char));
   SecondEditBuffer =  HeapDeref(BufferHandle);
   strcpy( SecondEditBuffer, "DYNAMIC" );
// On AMS 1.xx, Menus made with MenuNew and MenuAddText can't have submenus
//the help of MenuPoup explains this (pseudo-)structure
   if ((FirstPopup = PopupNew(NULL,0))) 
   {
      PopupAddText( FirstPopup, -1, "ENTRY 1", 10 );
      PopupAddText( FirstPopup, -1, "EXIT DIALOG", 20 );
      PopupAddText( FirstPopup, -1, "ENTRY 2", 30 );
   }
   else return;
      if ((SecondPopup = PopupNew(NULL,0)))
   {
      PopupAddText( SecondPopup, -1, "ENTRY 1", 10 );
      PopupAddText( SecondPopup, -1, "ENTRY 2", 20 );
   }
   else return;
   if ((ThirdPopup = PopupNew(NULL,0))) 
   {
      PopupAddText( ThirdPopup, -1, "ENTRY 1", 10);
      PopupAddText( ThirdPopup, -1, "ENTRY 2", 20);
      PopupAddText( ThirdPopup, -1, "EXIT", 30);
   }
   else return;

  #define ItemsNum 11
  #define Strings "Dialog Test\0Hpopup1\0Edit\0DPopup\0DEdit\0DPopup 2\0HPopup 2\0HPopup 1.2\0Static Text\0Exit"
  static SIZED_DIALOG(ItemsNum,sizeof(Strings)) DialogWindow={offsetof(SIZED_DIALOG(ItemsNum,0),String), ItemsNum,150, 93,(DialogNew_t) CallBack,
  {
  {//Scroll
  	D_SCROLL_REGION,DF_SKIP | DF_CLR_ON_REDRAW,8,31,{.dScrollR={140,71,3,9,4,7,10}}
  },
  {//TiltleEx
  	D_HEADER,DF_SKIP,0,0,{.dHeader={0,BT_OK,BT_CANCEL}}
  },
  {//Menu
  	D_MENU,DF_SKIP,20,12,{.dMenu={&MenuWindow,0}}
  },
  {//PulldownEx
  	D_HPOPUP,DF_SCROLLABLE, 8, 33,{.dHPopUp={12,0/*FirstPopup*/,0,1}}
  },
  {//RequestEx
  	D_EDIT_FIELD,DF_SCROLLABLE, 8, 43,{.dEdit={20,0,10,10}}
  },
  {//DynamicPulldown
  	D_DYNPOPUP,DF_SCROLLABLE, 8, 53,{.dDynPopUp={25,&GetPopup,2}}
  },
  {//DynamicRequest
  	D_HEDIT,DF_SCROLLABLE, 8, 63,{.dEdit={32,0,0,10}}
  },
  {//DynamicPulldown
  	D_DYNPOPUP,DF_SCROLLABLE, 8, 73,{.dDynPopUp={38,&GetPopup,3}}
  },
  {//PulldownEx
  	D_HPOPUP,DF_SCROLLABLE, 8, 83,{.dHPopUp={47,0/*SecondPopup*/,0,4}}
  },
  {//PulldownEx
  	D_HPOPUP,DF_SCROLLABLE, 8, 93,{.dHPopUp={56,0/* FirstPopup*/,0,1}}
  },
  {//Text
  	D_TEXT,DF_SKIP, 98, 73,{.dText={67}}
  },
	{//End , verry important
  	.f={}
  }
  },
  Strings
};
  //It is static so that it can include pointers (such as Callback) but no Var.
  // Therefore fields with vars must be initialized separately:
  
  DialogWindow.Fields[3].f.dHPopUp.hPopUp=FirstPopup;
  DialogWindow.Fields[8].f.dHPopUp.hPopUp=SecondPopup;
  DialogWindow.Fields[9].f.dHPopUp.hPopUp=FirstPopup;
  
   
   Options[0] = 10;
   Options[1] = 10;
   Options[2] = 10;
   Options[3] = 20;
   strcpy( EditBuffer, "TEST" );
   do
   {
		Dialog((DIALOG *) &DialogWindow,-1, -1, &EditBuffer[0], &Options[0] );
    Key = DlgMessage("Exit ?", "exit : ESC ; else : ENTER", BT_OK, BT_CANCEL );
   } while (KEY_ESC != Key);
   HeapFree( FirstPopup );
   HeapFree( SecondPopup );
   HeapFree( ThirdPopup );
}
