// Return the EStack handle.

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define RETURN_VALUE          // Return Pushed Expression
#define MIN_AMS 101           // Compile for AMS 1.01 or higher

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  push_END_TAG ();
  push_END_TAG ();
  // Outside all allocated blocks.
  push_shortint (HeapPtrToHandle((void *)1));
  // Strictly inside a block.
  push_shortint (HeapPtrToHandle((void *)estack_max_index));
  // Strictly inside a block, must be the same handle as estack_max_index.
  push_shortint (HeapPtrToHandle((void *)top_estack));
  // Beginning of a block.
  push_shortint (PtrToHandle(HeapDeref(1)));
  push_LIST_TAG ();
  push_END_TAG ();
  // Outside all allocated blocks.
  push_shortint (PtrToHandle((void *)1));
  // Strictly inside a block.
  push_shortint (PtrToHandle((void *)estack_max_index));
  // Strictly inside a block, must be the same handle as estack_max_index.
  push_shortint (PtrToHandle((void *)top_estack));
  // Beginning of a block.
  push_shortint (PtrToHandle(HeapDeref(1)));
  push_LIST_TAG ();
  push_LIST_TAG ();
}
