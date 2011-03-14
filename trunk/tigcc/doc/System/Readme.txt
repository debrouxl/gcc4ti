Modified:
Mon Mar 14 2011 Lionel Debroux <lionel_debroux@yahoo.fr>
  - Added a note, in the .hsh section, about manual tasks needed for correct documentation of a new header.
Sat Jan 24 2009 Conrad Meyer <cemeyer@u.washington.edu>
  - Updated to include [References]

Comment by Sebastian Reichelt (original author of this file):
This is just some information I quickly put together, meant especially for Zeljko Juric.  I guess it's better than no documentation at all.

 *.ref
-------

.ref files are always generated automatically.  They simply contain references to other files (as you can see by viewing them in a text editor).  For example, if you update the alloc.h folder, references to 'Bool' (for example) are created in all other folders where Bool needs to be defined.  You do not need to care about them.  Except: if you make a mistake, you'll need to delete them manually.  But that's not a problem since you don't need to send me any .ref files (and if you do, I will not add them anyway).  So, you don't need to know more about them; they are more or less internal.

 *.hsf
-------

.hsf means "Help System File".  This is the basic format, or the one which I created at first when I wrote the help system.  An .hsf file always contains one single identifier, which must be a valid identifier in C.  It works like an INI file which can have the following entries:

[Main]
Name: The identifier name; this is usually equal to the file name, but this does not have to be the case: the file name is completely arbitrary - see "dollar" instead of "$".
Type: Can be Function/Variable/Constant/Type or anything else.  Anything else (e.g. "Language Extension") will not get an icon in the .chm file, and will be in the "Other Identifiers" category.
Subtype: Can be anything, but some values have special meanings (see below)
Header Files: The header files the identifier appears in, separated by commas.  The .ref files are generated from this item, and the header files are also listed in the .chm release.
MinAMS: The minimum AMS version, in human-readable format (e.g. 1.01).
Definition: The visible definition for the identifier.  Note that a '{' followed by a space increases the indent, and a space followed by a '}' decreases it.  ';' operators will force the converter to enter a new line, except if a comment follows (in this case, the comment still appears on the same line).  So "enum {blah...};" will be on a single line, whereas "struct { item1; item2; } name;" will occupy four lines.
Real Definition: The definition which is used for the real header files.  If this is empty or doesn't exist, the "Definition" is used instead of "Real Definition" (in fact, this is the case most of the time).
Attributes: An optional attribute specifier, including the __attribute__ part and double parentheses.  This can also be a macro from default.h; it is included as-is.
See Also: A list of links, separated by commas.  Links may start with 'info/', which means that they do not point to a header file but to an info section.  They may be followed by a colon and a description, but the description must not contain any commas.
Needed By: Specifies identifiers, separated by commas, which may only appear after the definition of this item in the header file.  Usually identifiers are sorted alphabetically.  Actually, what really happens is that the current definition is inserted in the line before the first one of the identifiers in this list.

[ROM Call]
(only used if "Subtype=ROM Call" is specified)
Index: The index of the ROM call, as an integer.  In Pascal, a '$' can be used to indicate a hex number.  If the index is not in hex, it will be converted automatically to hex the next time you update the file.
Reference: (only used for variables, can be "1" or "0") I noticed that there are two kinds of variables: The jump table can contain a pointer to a variable of the specified type, or it can contain the reference itself (a constant pointer, of course).  I don't know how to put this in words.  For example, look at the definitions of top_estack (which is a normal ROM call variable), and ScrRect (which has Reference set to "1").
Address Hack: Specifies an alternative way to get the address of the ROM call.  This alternative is used in all AMS versions that do not export the ROM call in their jump table.  The code specified here must resolve to an arbitrary pointer.
Value Hack: (only used for functions) Specifies an alternative way to obtain the return value of the function.  This alternative is used in all AMS versions that do not export the ROM call in their jump table.  The code specified here must have the same type as the function's return type.  This can also be used to simulate the effect of the function, even for functions returning void.  If the function takes parameters, their names may be used in the calculation (in parentheses!).

If "ROM Call" is specified, the definition will be converted to the appropriate definition of the corresponding ROM call, as you can guess.  But of course this only works for functions and variables.

[Library Call]
(only used if "Subtype=tigcc.a" is specified)
Asm: (can be "1" or "0") Specifies if __ATTR_LIB_C__ or __ATTR_LIB_ASM__ should be used.
TIOS Callback: (can be "1" or "0") Specifies if __ATTR_TIOS_CALLBACK__ should be used (which means that the library call uses the same convention as the TIOS).

[<enum constant>]
MinAMS: The minimum AMS version, in human-readable format (e.g. 2.00).

[Registers]
(only used if the definition has the format of a function)
Contains lines of the form: <param>=<register>
Note that parameter names are case sensitive.  Misspelled parameter names will be ignored silently.  Not all parameters need register assignments.

[Description]
Contains HTML code for the short description of the identifier, which also appears in the list.  If this is left empty, a plea for help is inserted instead in the .chm release.

[Explanation]
HTML code for the rest of the documentation for the identifier.

[References]
In: Other symbols that make use of this symbol (comma seperated list of foo.h/Sym items).
Out: Other symbols that this symbol uses (same format as for "In").

 *.hsh
-------

.hsh means "Help System Header".  It is the description of a header file, containing three fields: [Description], [Top], and [Bottom].  They are displayed in this order, and [Description] is also included in the list of header files.  As the names suggest, only [Bottom] is displayed below the list of identifiers.  All of the are in HTML, as always.  A [Main] section with a "See Also" field is also supported.
NOTE: when creating a new header, for correct operation, UpdateInclude depends on two manual tasks:
    * creating a .h file containing "/* Begin Auto-Generated Part */" and "/* End Auto-Generated Part */";
    * adding a
<LI> <OBJECT TYPE="TEXT/SITEMAP">
<PARAM NAME="Name" VALUE="[newheaderfile].h">
<PARAM NAME="Local" VALUE="[newheaderfile].html">
</OBJECT>
<UL>
</UL>
      block to Contents.hhc.

 *.hsi
-------

.hsi means "Help System Include".  There is a file "Include.hsi" in the "Include" folder, specifying the usual [Top] and [Bottom] part for the list of header files.

 *.irf
-------

.irf means "Info Reference".  There are two special files in the "Include" folder ("unknown.irf" and "minams.irf"), and one in the "Keywords" folder ("define.irf"), which specify locations with special meanings.

 *.hss
-------

.hss means "Help System Section".  For informational parts of the documentation, these files specify single sections.  In each folder, there is one file with this extension and with the same name as the folder.  This file is the main section.  Each section can specify subsections.

[Main]
Title: The title of the section.  No HTML is allowed.  This title will also appear in the list of subsections of the parent section.
Subsections: The subsections for this section, separated by commas.  Each section can only be referenced once, due to the fact that it needs a parent section.
Index: Any number of index entries for this section (in addition to the title), separated by commas.
Style: (main section only, can be "Stack" or "Queue") If "Queue" is specified, subsections are queued in the web release, instead of written out recursively.  This has the effect that all sections with the same parent section appear one after another; they are not separated by their own subsections.  This is used in the FAQ list, where all categories of questions appear at the top, and all question/answer pairs are below them.
See Also: Same as "See Also" in .hsf files.

[Top]/[Bottom]
These two parts contain HTML code which appears above and below the list of subsections.  If no subsections are specified, [Top] should be used exclusively.

 minams.chk
------------

This file contains a list of all known AMS versions with their maximum number of ROM calls.  For some, the exact number of ROM calls is known, but for 2.00, we only know that it has to be 0x508 or less.  This is used to check the MinAMS values automatically based on the ROM call index.  If the specified ROM call does not exist in the specified AMS version, or if no AMS version is specified, the AMS version is adjusted automatically.
