// Guess the number, an example how to use dialogs in TIGCC

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define MIN_AMS 100

#include <alloc.h>
#include <ctype.h>
#include <dialogs.h>
#include <peekpoke.h>

static unsigned int rand_seed=0; // seed for random numbers

int randomnum(int x)             // returns a random number between 0 and x-1
{
  return (rand_seed=(75*rand_seed+1))%x;
}

void _main(void)
{
  const char *text;
  const char *ptr;
  char buffer[10];
  int number,guessed,dialogkey;
  HANDLE dialog;
  rand_seed=137*peek(0x600017);  // pick the seed from the timer
  while(TRUE)
    {
      text="Guess the number between 1 and 1000";
      number=0; guessed=1+randomnum(1000);
      while(TRUE)
        {
          *buffer=0;
          if(!(dialog=DialogNewSimple(140,50))) return;
          DialogAddTitle(dialog,"~GUESS THE NUMBER~ GAME",BT_OK,BT_CANCEL);
          if(number!=guessed)
            DialogAddRequest(dialog,4,24,"Enter the number",0,5,5);
          else
            {
              text="YOU GUESSED THE NUMBER!!!";
              DialogAddText(dialog,4,24,"Press ENTER for a new game...");
            }
          DialogAddText(dialog,4,15,text);
          dialogkey=DialogDo(dialog,CENTER,CENTER,buffer,NULL);
          HeapFree(dialog);
          if(dialogkey!=13) return;
          if(number==guessed) break;
          number=0; ptr=buffer;
          while(isdigit(*ptr)) number=10*number+(*ptr++)-'0';
          if(*ptr) text="Non-digit character entered!";
          else if(guessed>number) text="Sorry, my number is greater!";
          else if(guessed<number) text="Sorry, my number is smaller!";
        }
    }
}
