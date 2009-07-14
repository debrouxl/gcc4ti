// Shows how the init_matrix_indices function works.

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 101           // Compile for AMS 1.01 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// The [1, 0; 0, 1] matrix.
static const ESQ matrix_identity_2[16] = {END_TAG, END_TAG, 0x01, 0x01, POSINT_TAG, 0x00, POSINT_TAG, LIST_TAG, END_TAG, 0x00, POSINT_TAG, 0x01, 0x01, POSINT_TAG, LIST_TAG, LIST_TAG};

// Main Function
void _main(void)
{
  ESI elements[4];
  HANDLE h;

  TRY
    ClrScr();
    DrawStr (0, 0, "The elements of matrix", A_NORMAL);
    DrawStr (0, 20, "are", A_NORMAL);
    // Print whole expression.
    h = Parse1DExpr (matrix_identity_2 + 15, FALSE, 0);
    DrawStr(0, 10, HeapDeref(h), A_NORMAL);
    HeapFree(h);

    // Get the individual constituents of the expression and print them.
    init_matrix_indices(elements, matrix_identity_2 + 15);
    h = Parse1DExpr (elements[0], FALSE, 0);
    DrawStr(0, 30, HeapDeref(h), A_NORMAL);
    HeapFree(h);
    h = Parse1DExpr (elements[1], FALSE, 0);
    DrawStr(0, 40, HeapDeref(h), A_NORMAL);
    HeapFree(h);
    h = Parse1DExpr (elements[2], FALSE, 0);
    DrawStr(0, 50, HeapDeref(h), A_NORMAL);
    HeapFree(h);
    h = Parse1DExpr (elements[3], FALSE, 0);
    DrawStr(0, 60, HeapDeref(h), A_NORMAL);
    HeapFree(h);
  ONERR
    DrawStr (0, 70, "Error!", A_NORMAL);
  ENDTRY
  GKeyIn (NULL, 0);
}
