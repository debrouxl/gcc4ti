// A simple menu example, with several submenus

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define RETURN_VALUE          // Return a Value
#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 200           // Compile for AMS 2.00 or higher

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  HANDLE menu_handle = MenuNew (2, 240, 18);
  DynMenuAdd (menu_handle, 0, "First", 1, DMF_TEXT | DMF_TOP_SUB);
  DynMenuAdd (menu_handle, 1, "Subitem 1.1", 5, DMF_TEXT | DMF_CHILD_SUB);
  DynMenuAdd (menu_handle, 5, "Subitem 1.1.1", 8, DMF_TEXT | DMF_CHILD_SUB);
  DynMenuAdd (menu_handle, 5, "Subitem 1.1.2", 9, DMF_TEXT | DMF_CHILD);
  DynMenuAdd (menu_handle, 8, "Subitem 1.1.1.1", 10, DMF_TEXT | DMF_CHILD);
  DynMenuAdd (menu_handle, 1, "Subitem 1.2", 6, DMF_TEXT | DMF_CHILD);
  DynMenuAdd (menu_handle, 0, "Second", 2, DMF_TEXT | DMF_TOP_SUB);
  DynMenuAdd (menu_handle, 2, "Subitem 2.1", 7, DMF_TEXT | DMF_CHILD);
  DynMenuAdd (menu_handle, -1, "Third", 3, DMF_TEXT | DMF_TOP);
  DynMenuAdd (menu_handle, -1, "Fourth", 4, DMF_TEXT | DMF_TOP);
  HANDLE exec_handle = MenuBegin (NULL, 0, 0, MBF_HMENU, menu_handle);
  short result;
  do {
    result = MenuKey (exec_handle, ngetchx ());
  } while (result == M_NOTMENUKEY);
  MenuEnd (exec_handle);
  MenuUpdate ();
  push_shortint (result);
}
