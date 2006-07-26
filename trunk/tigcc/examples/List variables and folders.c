// Sends the list of all variables and folders through the link port.
// This program does what HeapWalk(H_WALK_SYM); does on AMS 2.04 and
// later, but also works on any AMS version.

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define SAVE_SCREEN           // Save/Restore LCD Contents
#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define NO_CALC_DETECT
#define ENABLE_ERROR_RETURN
#define MIN_AMS 100           // Compile for AMS 1.00 or higher

#include <tigcclib.h>         // Include All Header Files

void _main(void)
{
SYM_ENTRY *symptr;
unsigned char buffer[256];
#ifdef SAVE_SYMPG // Saving the SymPG isn't necessary in _main, nobody else does it.
SymPG_S save;
TRY
  memcpy(&save, pSymPG, sizeof(SymPG_S));
#endif
  if ((symptr = SymFindFirst(NULL,2)) != NULL)
  {
    strcpy(buffer, "\r\nName/Flags/hVal (dec)\r\n");
    LIO_SendData(buffer, strlen((char*)buffer));
    do
    {
      short flags = symptr->flags.flags_n;
      if ((flags&SF_FOLDER))
        sprintf((char *)buffer, "FOLDER: %-8s   %04X  %hd\r\n", symptr->name,
        flags, symptr->handle);
      else
        sprintf((char *)buffer, "%8s\\%-8s %04X  %hd\r\n", SymFindFolderName(),
        symptr->name, flags, symptr->handle);
      LIO_SendData(buffer, strlen((char *)buffer));
      symptr = SymFindNext();
    } while (symptr != NULL);
  }
#ifdef SAVE_SYMPG // See above.
FINALLY
  memcpy(pSymPG, &save, sizeof(SymPG_S));
ENDFINAL
#endif
}
