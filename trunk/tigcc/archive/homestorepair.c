#include <homescr.h>
#include <error.h>
#include <alloc.h>

__ATTR_LIB_C__ void HomeStorePair(HANDLE Entry, HANDLE Ans)
{
  HANDLE hNode;
  TRY
    hNode = HS_newFIFONode ();
  ONERR
    HeapFree (Entry);
    if (Ans != Entry)
      HeapFree (Ans);
    PASS;
  ENDTRY
  TRY
    FIFO_NODE *Node = HeapDeref (hNode);
    Node->Entry.Expr = Entry;
    Node->Ans.Expr = Ans;
    HS_pushEmptyFIFONode (hNode);
  ONERR
    HS_freeFIFONode (hNode);
    PASS;
  ENDTRY
}
