// Dialog structure example
// Example contributed by Sébastien Leurent

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization

#define SET_FILE_IN_USE_BIT   // Needed to prevent crashes

#define MIN_AMS 100           // Compile for AMS 1.00 or higher

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  char TxtBuffer[27] = "Hello ";
    // 6 bytes "Hello ", max. 20 bytes name, 1 zero byte
  
  #define ItemsNum 3
  #define MyStrings "EXAMPLE\0EnterYourName (max. 20 chars)\0Your name"
  static SIZED_DIALOG(ItemsNum,sizeof(MyString)) DialogWindow={offsetof(SIZED_DIALOG(ItemsNum,0),String), ItemsNum,130, 50,NoCallBack,
  {
  {//Title
  	D_HEADER,DF_SKIP,0,0,{.dHeader={0,BT_OK,BT_CANCEL}}
  },
  {
  	D_TEXT,DF_SKIP,3,20,{.dText={sizeof("EXAMPLE")}}
  },
  {
  	D_EDIT_FIELD,0, 3, 30,{.dEdit={sizeof("EXAMPLE\0EnterYourName&Age (max. 20 chars)"),6,20,14}}
  },
	{//End : nothing
		.f={}
	}
  },
  MyStrings
};

    if (Dialog ((DIALOG *) &DialogWindow, CENTER, CENTER, TxtBuffer, NULL) == KEY_ENTER)
    DlgMessage ("GREETINGS", TxtBuffer, BT_OK, BT_NONE);
}

