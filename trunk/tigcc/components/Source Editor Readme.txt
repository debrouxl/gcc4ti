                       Source Editor Component
                      =========================
                    for Borland Delphi 4 or later

                 Copyright (c) 2000 Sebastian Reichelt
                          SebastianR@gmx.de


TSourceEdit is a source editor control with several features that are
useful for IDE programmers with Delphi knowledge.  It is based on
TMemoComponent, which is completely home-made from scratch.  However,
many useful properties and methods of TMemo have been implemented.

All properties and methods should be self-explaining.  If you have any
questions concerning their usage, feel free to ask me.


Note that you should use the property Text instead of Lines to access
the whole text.  To modify a piece of text at runtime, use the
following code, or something similar:

with TMCRange.Create (nil) do
  try
    Editor := MyComponent;  // or whatever the name is
    RStart := S;            // the first character
    REnd := E;              // the last character; can be < S
    Text := NewText;        // the new text;
  finally
    Free;
  end;

Instead of RStart and REnd, you can also use StartRowCol, EndRowCol,
and RLength.

Line breaks are signaled by #13#10.

You also can track positions in your code using ranges.  If you do the
following:

MyRange := TMCRange.Create (MyComponent.TrackedRanges);
with MyRange do begin
  RStart := S;
  REnd := E;
  OnOverwrite := MyOverwriteHandle;  // optional
end;

then MyRange will always be updated when the text changes, so that you
can easily jump to the correct position of an error, the definition of
an identifier, or wherever you want.  Just use:

MyComponent.Selection.Assign (MyRange);

and the range will be selected.


For automatic saving and loading of TSyntaxColoring, a
TSyntaxColoringCopy class has been created, whose instances can be
passed to TStream.ReadComponent and TStream.WriteComponent.  Note that
TStream is an abstract class; use TMemoryStream instead.  Then you can,
for example, save your whole highlighting options in the registry.


That's all I want to write for now.  Happy programming!

Sebastian Reichelt
