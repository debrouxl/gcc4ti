// Dialog items example for AMS 2.00 or higher
// Example contributed by Wazabbe and Sébastien Leurent

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 200           // Compile for AMS 2.00 or higher

#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files


static HANDLE FirstPopup, SecondPopup, ThirdPopup,BeginPopup,ExitPopup,EmptyPopup;
static HANDLE MenuWindow;
static char EditBuffer[20], *SecondEditBuffer;
static HANDLE BufferHandle;
static unsigned short Options[6];

static BITMAP unselected=
{
	8,9,
	{
		0b11111111,0b10000000,
		0b10000000,0b10000000,
		0b10000000,0b10000000,
		0b10000000,0b10000000,
		0b10000000,0b10000000,
		0b10000000,0b10000000,
		0b10000000,0b10000000,
		0b11111111,0b10000000,
	}
};
static BITMAP selected=
{
	8,9,
	{
		0b11111111,0b10000000,
		0b10000000,0b10000000,
		0b10000010,0b10000000,
		0b10000100,0b10000000,
		0b10101000,0b10000000,
		0b10010000,0b10000000,
		0b10000000,0b10000000,
		0b11111111,0b10000000,
	}
};



enum Items {Scroll_ID,Title_ID,Toolbar_ID,HPopup1_ID,Edit_ID,DPopup1_ID,DEdit_ID,DPopup2_ID,HPopup2_ID,Radio_ID,Exit_ID,OwnerDrawn_ID,Text_ID,XFlags_ID};
#define ItemHeight 10
#define FirstScrollableItemID 3
#define FirstScrollableItemY 33
#define ScrollableItemY(ID) (FirstScrollableItemY+ItemHeight*(ID-FirstScrollableItemID))


HANDLE GetPopup( unsigned short dlgId )  /* This function is called by DialogAddDynamicPulldown whenever it is needed */
{
   if(dlgId == DPopup1_ID)
   {
      return FirstPopup;
   }
   else
   {
      return ThirdPopup;
   }
}

unsigned short Scroll;
short CallBack( short dlgId, long Value )   /* This function is given as a pointer to DialogNew and can be called whenever it is needed */
{
   unsigned short HitKey;
   HANDLE MenuHandle = H_NULL;
   DIALOG_ITEMS *DlogItems;
   WINDOW *WindowStruct;
   switch( dlgId )
   {
      case DB_QACTIVE:
      			/*When the dialog is scrolled, DB_QACTIVE is sent to all visible items.
      			It enables to know wich Items are visible, and to change scrolling acording to this.*/
            if(Value>=3 && Value<=10)//scrollable Item (other ones are always visible
            {
            	#define ItemY ((Value-FirstScrollableItemID)*ItemHeight)
            	#define YMaxScrolled (Scroll+ItemHeight*3/*since four Items are disped at a time*/)
            	//Item Y is the ordinate of this Item, Scroll must be the ordinate of the first visible Item
            	//and YMaxScrolled is the Ordinate of the last visible Item
            	//These ordinates are relative to the top of the scroll region
            	
             	if(ItemY<Scroll) Scroll-=ItemHeight;
            	//If with the current value of scroll, this item is not visible (too high up),
            	//It means that it has been scrolled and the value of Scroll must be changed, by substracting ItemHeight
            	if(ItemY>YMaxScrolled) Scroll+=ItemHeight;
            	//If with the current value of scroll, this item is not visible (too high down),
            	//It means that it has been scrolled and the value of Scroll must be changed, by adding ItemHeight
            	#undef ItemY
            	#undef YMaxScrolled
            }
      			break;
      case Scroll_ID:
            return TRUE;
      case Title_ID:
            return TRUE;
      case Toolbar_ID:
            MenuHandle = HI_WORD(Value);
            HitKey = LO_WORD(Value);
            int   result = -2;
            switch( HitKey )
            {
               case KEY_F1:
               Scroll=0;
               return 0;
               case KEY_F2:
               Scroll=4*ItemHeight;
               return 4;
               case KEY_F3:
                  while (result == -2)
                  {
                     result = MenuKey (MenuHandle, HitKey);
                  }
                  switch(result)
                  {
                      case 4:
                         Scroll=0;return 0;
                      case 5:
                         Scroll=4*ItemHeight;return 4;
                      case 6:
                         return DB_EXIT;
                      default:
                         Scroll=0;
                         return 0;
                  }
               default:
                  return 1;
            }
            break;
      case HPopup1_ID:
            if (Options[1] == 20)
            {
                  return DB_EXIT;
            }
            else
            {
               return DB_REDRAW_AND_CONTINUE;
            }
            break;
      case Edit_ID:
            if (0 == strcmp("EXIT", EditBuffer))
                  return DB_EXIT;
            break;
      case DPopup1_ID:
            if (Options[2] == 20)
                  return DB_EXIT;
            break;
      case DEdit_ID:
            SecondEditBuffer =  HeapDeref(BufferHandle);
            if (0 == strcmp("EXIT", SecondEditBuffer))
                  return DB_EXIT;
            break;
      case DPopup2_ID:
            if (Options[3] == 30)
            {
                  return DB_EXIT;
            }
            else
            {
               return DB_REDRAW_AND_CONTINUE;
            }
            break;
      case HPopup2_ID:
            return DB_REDRAW_AND_CONTINUE;
            break;
      case Radio_ID:
            return DB_REDRAW_AND_CONTINUE;
      case Exit_ID:
      			return DB_EXIT;
      case OwnerDrawn_ID:
            DlogItems = (DIALOG_ITEMS *)(((OWNER_DRAW_STRUCT *)Value)->Item);
            WindowStruct = (WINDOW *)(((OWNER_DRAW_STRUCT *)Value)->pW);
            char Buffer[sizeof("Scroll : xx")];
            sprintf(Buffer,"Scroll : %d",Scroll);
            WinStrXY( (WindowStruct), 8, 73, Buffer);
            WinAttr( (WindowStruct), A_XOR);
             // If it is visible, disp my RadioButton
            if(Scroll>=30)
            {
	            WinStrXY( (WindowStruct), 8, 93-Scroll, "Show Msg");
	            WinBitmapPut (WindowStruct, CALCULATOR?63:42, (CALCULATOR?92:91)-Scroll, (Options[5]==1)?&selected:&unselected,A_REPLACE);
            }
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
   static HANDLE DialogWindow;
   static WORD Key;
   BufferHandle = HeapAlloc (20 * sizeof(char));
   SecondEditBuffer =  HeapDeref(BufferHandle);
   strcpy( SecondEditBuffer, "DYNAMIC" );
   if ((MenuWindow = MenuNew(0,0,0)))
   {
      MenuAddText( MenuWindow, 0, "MENU1", 1, DMF_TOP );
      MenuAddText( MenuWindow, 0, "MENU2", 2, DMF_TOP );
      MenuAddText( MenuWindow, 0, "SUBITEMS", 3, DMF_TOP_SUB );
      MenuAddText( MenuWindow, 3, "MENU SUBITEM 1", 4, DMF_CHILD_SUB );
      MenuAddText( MenuWindow, 3, "MENU SUBITEM 2", 5, DMF_CHILD_SUB );
      MenuAddText( MenuWindow, 3, "EXIT", 6, DMF_CHILD_SUB );
   }
   else return;
   if ((FirstPopup = PopupNew(NULL,0))) 
   {
      PopupAddText( FirstPopup, -1, "ENTRY 1", 10 );
      PopupAddText( FirstPopup, -1, "EXIT DIALOG", 20 );
      PopupAddText( FirstPopup, -1, "ENTRY 2", 30 );
   }
   else return;
   if ((SecondPopup = PopupNew(NULL,0)))
   {
      PopupAddText( SecondPopup, -1, "ENTRY 1", 1 );
      PopupAddText( SecondPopup, -1, "ENTRY 2", 2 );
   }
   else return;
   if ((ThirdPopup = PopupNew(NULL,0)))
   {
      PopupAddText( ThirdPopup, -1, "ENTRY 1", 10);
      PopupAddText( ThirdPopup, -1, "ENTRY 2", 20);
      PopupAddText( ThirdPopup, -1, "EXIT", 30);
   }
   if ((BeginPopup = PopupNew(NULL,0)))
   {
      PopupAddText( BeginPopup, -1, "Use DIALOG_STRUCT", 10);
      PopupAddText( BeginPopup, -1, "Use DialogNew", 20);
   }
   else return;
   if((ExitPopup=PopupNew(NULL,0)))
   {
   		PopupAddText( ExitPopup, -1, "Right arrow to exit",1);
   }
   else return;
     if ((EmptyPopup = PopupNew(NULL,0)))
   {
      PopupAddText( EmptyPopup, -1, "Toggle", 1);
      PopupAddText( EmptyPopup, -1, "Toggle", 2);
   }
   else return;
   if ((DialogWindow = DialogNew( 0,0, (DialogNew_t) CallBack)))
   {
      DialogAddScrollRegion( DialogWindow, DF_CLR_ON_REDRAW, 8, 31,140, 71,FirstScrollableItemID,10,4,8,ItemHeight );
      DialogAddTitleEx( DialogWindow, 0, "Dialog Test", BT_OK, BT_CANCEL);
      DialogAddMenu( DialogWindow, 0, 0, 12, HLock(MenuWindow), 60 );
      DialogAddPulldownEx( DialogWindow, DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(HPopup1_ID), "HPopup 1", FirstPopup, 1);
      DialogAddRequestEx( DialogWindow, DF_TAB_ELLIPSES | DF_SCROLLABLE, 8,ScrollableItemY(Edit_ID), "Edit", 0, 10, 10);
      DialogAddDynamicPulldown( DialogWindow, DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(DPopup1_ID), "DPopup", &GetPopup, 2 );
      DialogAddDynamicRequest( DialogWindow, DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(DEdit_ID), "DEdit", 10);
      DialogAddDynamicPulldown( DialogWindow, DF_TAB_ELLIPSES | DF_SCROLLABLE, 8,ScrollableItemY(DPopup2_ID), "DPopup 2", &GetPopup, 3 );
      DialogAddPulldownEx( DialogWindow, DF_TAB_ELLIPSES | DF_SCROLLABLE | DF_POPUP_RADIO, 8,ScrollableItemY(HPopup2_ID), "HPopup 2", SecondPopup, 4);
      DialogAddPulldownEx( DialogWindow, DF_SCROLLABLE | DF_POPUP_RADIO,(CALCULATOR?75:53),ScrollableItemY(Radio_ID), "", EmptyPopup, 5);
      DialogAddPulldownEx( DialogWindow, DF_POPUP_RADIO | DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(Exit_ID), "Exit", ExitPopup,0);
      DialogAddTextEx( DialogWindow, DF_OWNER_DRAW, 0, 0, "");
      DialogAddTextEx( DialogWindow, 0, 98, 73, "Static Text");
      DialogAddXFlags( DialogWindow, 0, XF_VARLINK_SELECT_ONLY,0, 0, 0);
   }
   else return;

  #define ItemsNum 14
  #define Strings "Dialog Test\0Hpopup1\0Edit\0DPopup\0DEdit\0DPopup 2\0HPopup 2\0Exit\0Static Text"
  static SIZED_DIALOG(ItemsNum,sizeof(Strings)) DialogWindowB={offsetof(SIZED_DIALOG(ItemsNum,0),String), ItemsNum,0, 0,(DialogNew_t) CallBack,
  {
  {//Scroll
  	D_SCROLL_REGION,DF_SKIP | DF_CLR_ON_REDRAW,8,31,{.dScrollR={140,71,FirstScrollableItemID,10,4,8,ItemHeight}}
  },
  {//TiltleEx
  	D_HEADER,DF_SKIP,0,0,{.dHeader={0,BT_OK,BT_CANCEL}}
  },
  {//Menu
  	D_MENU,DF_SKIP,0,12,{.dMenu={0/*will be replaced by HLock(MenuWindow) later*/,60}}
  },
  {//PulldownEx
  	D_HPOPUP,DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(HPopup1_ID),{.dHPopUp={12,0/*FirstPopup*/,0,1}}
  },
  {//RequestEx
  	D_EDIT_FIELD,DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(Edit_ID),{.dEdit={20,0,10,10}}
  },
  {//DynamicPulldown
  	D_DYNPOPUP,DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(DPopup1_ID),{.dDynPopUp={25,&GetPopup,2}}
  },
  {//DynamicRequest
  	D_HEDIT,DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(DEdit_ID),{.dEdit={32,0,0,10}}
  },
  {//DynamicPulldown
  	D_DYNPOPUP,DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(DPopup2_ID),{.dDynPopUp={38,&GetPopup,3}}
  },
  {//PulldownEx
  	D_HPOPUP,DF_TAB_ELLIPSES | DF_SCROLLABLE | DF_POPUP_RADIO, 8, ScrollableItemY(HPopup2_ID),{.dHPopUp={47,0/* SecondPopup*/,0,4}}
  },
  {//PulldownEx
  	D_HPOPUP, DF_SCROLLABLE | DF_POPUP_RADIO, 0/*to be changed*/, ScrollableItemY(Radio_ID),{.dHPopUp={11,0/* EmptyPopup*/,0,5}}
  },
   {//PulldownEx
  	D_HPOPUP,DF_POPUP_RADIO | DF_TAB_ELLIPSES | DF_SCROLLABLE, 8, ScrollableItemY(Exit_ID),{.dHPopUp={56,0/* ExitPopup*/,0,0}}
  },
  {//TextEx
  	D_TEXT,DF_OWNER_DRAW | DF_SKIP, 0, 0,{.dText={11}}
  },
  {//Text, again
  	D_TEXT,DF_SKIP, 98, 73,{.dText={61}}
  },
  {//Xflags
  	D_XFLAGS,DF_SKIP,0,0,{.dFlags={{XF_VARLINK_SELECT_ONLY,0, 0, 0}}}
  },
  {//End , verry important
  	.f={}
  }
  },
  Strings
};
  //It is static so that it can include pointers (such as Callback) but no Var.
  // Therefore fields with vars must be initialized separately:
  
  DialogWindowB.Fields[2].f.dMenu.Menu=HLock(MenuWindow);
  DialogWindowB.Fields[3].f.dHPopUp.hPopUp=FirstPopup;
  DialogWindowB.Fields[8].f.dHPopUp.hPopUp=SecondPopup;
  DialogWindowB.Fields[9].f.dHPopUp.hPopUp=EmptyPopup;
  DialogWindowB.Fields[9].x0=CALCULATOR?75:53;
  DialogWindowB.Fields[10].f.dHPopUp.hPopUp=ExitPopup;
  
  Scroll=0;
   
   Options[0] = 10;
   Options[1] = 10;
   Options[2] = 10;
   Options[3] = 20;
   Options[4] = 1;
   Options[5] = 1;
   
   strcpy( EditBuffer, "TEST" );
   do
   {
    Scroll=0;
    if(PopupDo (BeginPopup, CENTER, CENTER, 10)==10)
		Dialog((DIALOG *) &DialogWindowB,-1, -1, &EditBuffer[0], &Options[0] );
		else
    DialogDo( DialogWindow, -1, -1, &EditBuffer[0], &Options[0] );
    if(Options[5]==1) Key = DlgMessage("Exit ?", "If you wish to completely exit press ESC, else press ENTER", BT_OK, BT_CANCEL );
   } while (Options[5]==1 && KEY_ESC != Key);
   HeapFree(EmptyPopup);
   HeapFree( ExitPopup);
   HeapFree( DialogWindow );
   HeapFree( FirstPopup );
   HeapFree( SecondPopup );
   HeapFree( ThirdPopup );
   HeapFree( BeginPopup );
   HeapUnlock( MenuWindow );
   HeapFree( MenuWindow );
}
