// Handle a variable with VAT functions.
// Unlike "Create Variable", this example doesn't use any intermediate helper functions.

#define OPTIMIZE_ROM_CALLS

#define USE_TI89
#define USE_TI92P
#define USE_V200
#define NO_CALC_DETECT
#define MIN_AMS 100
#define NO_AMS_CHECK
#include <tigcclib.h>

void _main(void)
{
    HSym hs;
    SYM_ENTRY *SymPtr;
    unsigned short *VarPtr;
    unsigned short savednumber;
    unsigned char buffer[25];

    //
    // Read old score
    //

    // First, find the file. This returns an HSym which is HS_NULL if
    // the file didn't exist.
    hs = SymFind(SYMSTR("example"));

    if(hs.folder == 0)
    {
        savednumber = 0;
    }
    else
    {
        SymPtr = DerefSym(hs);
        VarPtr = (unsigned short*)(HeapDeref(SymPtr->handle) + 2);
            // The "+2" skips over the first 2 bytes of the file, which
            // store the file's size (and which we ignore).
        savednumber = *VarPtr;
    }

    //
    // Write new score
    //

    sprintf(buffer,"Runs before: %i.",savednumber);
    ST_helpMsg(buffer);
    savednumber++;

    SymDel(SYMSTR("example"));

    // NOTE: This completely lacks error checking. Checks should be
    // made for the following cases:
    //   SymAdd returning HS_NULL;
    //   HeapAlloc returning H_NULL;
    SymPtr = DerefSym(SymAdd(SYMSTR("example")));
    VarPtr = (unsigned short*)HeapDeref(SymPtr->handle = HeapAlloc(sizeof savednumber));
    // In a more complex case you'd cast this as a struct, not an array of
    // shorts, where the first field is the file size.

    VarPtr[0] = 2;
    // Size of the file, not counting the 2 bytes indicating the size
    VarPtr[1] = savednumber;
}
