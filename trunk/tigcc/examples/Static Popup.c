// Display a predefined popup menu

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher

#include <tigcclib.h>         // Include All Header Files

extern void menu;
asm ("menu:\n"
"        .word  title_text - menu\n"
"        .word  0xD5\n"
"        .word  9\n"
"        .byte  0, 40\n"
"        .word  5\n"
"        .word  0\n"
"        .word  0\n"
"        .word  0\n"
"        .long  -1\n"
"      main_entries:\n"
"        .word  MT_TEXT + 1                   /* | is a comment character, */\n"
"        .word  option_1_text - title_text    /* so use + instead */\n"
"        .word  MT_TEXT + 2\n"
"        .word  option_2_text - title_text\n"
"        .word  MT_TEXT + MT_CASCADE + 3\n"
"        .word  submenu_3_text - title_text\n"
"        .word  submenu_3_cascade - menu\n"
"        .word  MT_TEXT + MT_CASCADE + 4\n"
"        .word  submenu_4_text - title_text\n"
"        .word  submenu_4_cascade - menu\n"
"        .word  MT_TEXT + 5\n"
"        .word  option_5_text - title_text\n"
"        .long  -1\n"
"      submenu_3_cascade:\n"
"        .word  MT_TEXT + 6\n"
"        .word  suboption_3_1_text - title_text\n"
"        .word  MT_TEXT + 7\n"
"        .word  suboption_3_2_text - title_text\n"
"        .word  MT_TEXT + 8\n"
"        .word  suboption_3_3_text - title_text\n"
"        .long  -1\n"
"      submenu_4_cascade:\n"
"        .word  MT_TEXT + 9\n"
"        .word  suboption_4_1_text - title_text\n"
"        .long  -1\n"
"      title_text:\n"
"        .asciz \"EXAMPLE\"\n"
"      option_1_text:\n"
"        .asciz \"Option 1\"\n"
"      option_2_text:\n"
"        .asciz \"Option 2\"\n"
"      submenu_3_text:\n"
"        .asciz \"Submenu 3\"\n"
"      submenu_4_text:\n"
"        .asciz \"Submenu 4\"\n"
"      option_5_text:\n"
"        .asciz \"Option 5\"\n"
"      suboption_3_1_text:\n"
"        .asciz \"Suboption 3.1\"\n"
"      suboption_3_2_text:\n"
"        .asciz \"Suboption 3.2\"\n"
"      suboption_3_3_text:\n"
"        .asciz \"Suboption 3.3\"\n"
"      suboption_4_1_text:\n"
"        .asciz \"Suboption 4.1\"\n");

void _main(void)
{
  MenuPopup (&menu, CENTER, CENTER, 0);
}
