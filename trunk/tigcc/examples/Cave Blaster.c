// Cave Blaster, an example of arcade game written in C
// You need the TIGCC cross-compiler to compile it

// Compile for TI-89 only
#define USE_TI89

#define MIN_AMS 100
#define SAVE_SCREEN

#include <string.h>
#include <graph.h>
#include <kbd.h>
#include <system.h>
#include <printf.h>
#include <peekpoke.h>

#define ARROWS_ROW 0xfffe      // Constants for low-level keyboard access
#define LEFT_KEY 2
#define RIGHT_KEY 8
#define UP_KEY 1
#define DOWN_KEY 4
#define SECOND_KEY 16
#define ESC_ROW 0xffbf
#define ESC_KEY 1

typedef struct
  {
    char name[10];
    unsigned long score;
  } HSC_ITEM;

SCR_RECT screen_area={{0,0,159,99}};

unsigned int rand_seed=0;

void draw_str(int x,int y,const char *str)
{
  DrawStr(x,y,str,A_REPLACE);
}

// Look at the following 4 routines: do you know why C is a write-only language?
// In my opinion, even assembler is often more readable!

#define LCD_HARD_BYTE_WIDTH 30
#define LCD_BYTE_WIDTH 20

#define START_ROW 10
#define END_ROW 99

void scroll_left(void)
{
  register char *ptr=LCD_MEM+LCD_HARD_BYTE_WIDTH*START_ROW;
  register int i;
  while(ptr<=(char*)LCD_MEM+LCD_HARD_BYTE_WIDTH*END_ROW)
    {
      for(i=0;i<LCD_BYTE_WIDTH/4;i++,ptr+=4)
        poke_l(ptr,(peek_l(ptr)<<1)+(speek_l(ptr+4)<0));
      ptr+=LCD_HARD_BYTE_WIDTH-LCD_BYTE_WIDTH;
    }
}

#undef START_ROW
#undef END_ROW

int get_pixel(int x,int y)  // Faster replacement for GetPix from TIOS
{
  return !!(peek(0x4c00+30*y+(x>>3))&(1<<(~x&7)));
}

void invert_scr(void)
{
  register long *ptr;
  for(ptr=(long*)0x4c00;(unsigned long)ptr<=0x57b8;*ptr++^=-1);
}

// Yet another ugly-looking but very efficient routine follows: it puts a
// sprite (in EXOR mode) defined by "sprite" at position (x,y). Returns 1 if
// OK, or 0 if there is a collision with at least one pixel drawn on screen.

int put_sprite(int x,int y,int sprite[])
{
  register long addr=0x4c00+30*y+((x>>3)&0xfffe),collision=0,data;
  register int i,cnt=16-(x&15);
  for(i=0;i<=7;i++,addr+=30)
    collision|=((data=(long)sprite[i]<<cnt)&peek_l(addr)),*(long*)addr^=data;
  return !collision;
}

int random(int x)           // Returns a random number between 0 and x-1
{
  return ((rand_seed=(75*rand_seed+1))>>4)%x;
}

void randomize(unsigned int x)
{
  rand_seed=x;
}

void draw_border(void)
{
  int i;
  ClrScr();
  for(i=0;i<=4;i+=2)
    DrawClipRect(MakeWinRect(i,i,159-i,99-i),&screen_area,A_NORMAL);
}

long game(int speed)        // Main game; returns score
{
  int up=20,down=90,fire=0,laser=250;
  int scr_cnt=30,level_cnt=0,level=1,met_loc=0,met_freq=150;
  int x,y,old_x,old_y,i,rnd,rl,cnt=0,mxcnt=0,key,lives,f_sum,crash;
  unsigned long score=0,extra=1;
  static int plane[]={0,0x3800,0x1c38,0x0efc,0x3fff,0x7fff,0x1ffe,0x1f00};
  ClrScr();
  OSSetSR(0x0400);          // Disable interrupts below Auto-Int 5
  for(i=1;i<=120;i+=2)
    DrawClipEllipse(80,50,i,i,&screen_area,1);
  ClrScr();
  for(lives=5;lives>0;lives--)
    {
      ClrScr();
      FontSetSys(F_4x6);
      DrawClipRect(&(WIN_RECT){0,0,158,8},&screen_area,A_NORMAL);
      put_sprite(x=100,y=50,plane);
      crash=0;
      while(!crash)
        {
          if(_rowread(ESC_ROW)&ESC_KEY) return score;
          scroll_left();
          put_sprite(x-1,y,plane);
          if(!put_sprite(x,y,plane))
            {
              for(i=1;i<=100;i++) invert_scr();
              crash=1;
            }
          while(!OSTimerExpired(6));      // Delay
          OSTimerRestart(6);
          old_x=x; old_y=y;
          key=_rowread(ARROWS_ROW);
          if(key&SECOND_KEY&&laser)
            {
              DrawLine(x+16,y+5,159,y+5,A_XOR);
              fire=1; f_sum=0; laser--;
              for(i=x+10;i<=159;i++) f_sum+=!get_pixel(i,y+5);
              score+=f_sum*3;
            }
          if(scr_cnt++==30)
            {
              printf_xy(3,2,"Lives: %d  Zone: %c  Laser: %d  Score: %ld  ",
                lives,64+level,laser,score);           
              scr_cnt=0; score+=speed+(level<<1)+x/15;
              if(score>extra*5000) lives++,extra+=2;
              if(level_cnt++==30)
                {
                  level_cnt=0; level++; up++; down--;
                  met_freq=30+160/level; laser=200+50*level;
                  for(i=1;i<=12;i++) invert_scr();
                }
            }
          x+=((x<142)&&(key&RIGHT_KEY))-((x>2)&&(key&LEFT_KEY));
          y+=((y<92)&&(key&DOWN_KEY))-((y>1)&&(key&UP_KEY));
          if(old_x!=x||old_y!=y)
            {
              put_sprite(x,y,plane); put_sprite(old_x,old_y,plane);
              score++;
            }
          DrawLine(159,10,159,up,A_SHADE_H);
          DrawLine(159,99,159,down,A_SHADE_H);
          rnd=random((level+1)|1)-((level+1)>>1);
          up+=rnd; down+=rnd;
          if(up<10||down>98) up-=rnd,down-=rnd;
          if(!random(met_freq))
            mxcnt=cnt=4+random(8), met_loc=((up+down)>>1)+random(7)-3;
          if(--cnt>0&&met_loc)
            {
              rl=1+random(2+((cnt>mxcnt/2)?(mxcnt-cnt):cnt));
              DrawLine(159,met_loc-rl,159,met_loc+rl,A_NORMAL);
            }
          if(fire)
            {
              DrawLine(old_x+16,old_y+5,159,old_y+5,A_REVERSE);
              fire--;
            }
        }
    }
  FontSetSys(F_8x10);
  draw_str(50,45,"GAME OVER !");
  ngetchx();
  return score;
}

void high_scores(HSC_ITEM hsc_table[])
{
  int i;
  draw_border();
  FontSetSys(F_8x10);
  draw_str(12,10,">> HIGH SCORES <<");
  for(i=0;i<=4;i++)
    printf_xy(12,30+12*i,"%d. %-8s%6ld",i+1,hsc_table[i].name,
      hsc_table[i].score);
  ngetchx();
}

void input_str(char *str,int x,int y,int maxlen)
{
  int i=0,key=0;
  while(key!=13)
    {
      str[i]='_'; str[i+1]=' '; str[i+2]=0;
      draw_str(x,y,str);
      key=ngetchx();
      if(key>=' '&&key<='~'&&i<maxlen) str[i++]=key;
      if(key==257&&i) i--;
    }
  str[i]=0;
}

void update_hsc(HSC_ITEM hsc_table[],unsigned long score)
{
  int i;
  for(i=4;(hsc_table[i-1].score<score)&&i;i--)
    memcpy(&hsc_table[i],&hsc_table[i-1],sizeof(HSC_ITEM));
  hsc_table[i].score=score;
  ClrScr();
  FontSetSys(F_8x10);
  draw_str(35,10,"WELL DONE !!!");
  FontSetSys(F_6x8);
  draw_str(17,30,"You have achieved one");
  draw_str(27,40,"of the best scores!");
  draw_str(30,60,"Enter your name");
  draw_str(20,70,"(max. 8 characters):");
  input_str(hsc_table[i].name,50,84,8);
  high_scores(hsc_table);
}

void _main(void)            // Main program
{
  static HSC_ITEM hsc_table[5]={{"PPanther",15000},{"DDuck",13000},
    {"MMouse",10000},{"Popeye",8000},{"BBunny",5000}};
  static char land[]="1",speed[]="4";
  int key=0;
  unsigned long score;
  pokeIO(0x600017,0xFA);      // Force timer to work faster
  while(key!=264)
    {
      if(key!='3'&&key!='4') draw_border();
      OSFreeTimer(6);
      OSRegisterTimer(6,6-(speed[0]-'0'));
      FontSetSys(F_8x10);
      draw_str(16,10,"CaveBlaster V1.0 ");
      FontSetSys(F_4x6);
      draw_str(10,70,"KEYS: Arrows: move, 2nd: fire, ESC: exit");
      draw_str(30,79,"(C) 2000 by Zeljko Juric");
      draw_str(8,88,"Example of arcade written in C language");
      FontSetSys(F_6x8);
      draw_str(18,27,"1. Play the game");
      draw_str(18,36,"2. View high scores");
      draw_str(18,45,"3. Select landscape");
      draw_str(18,54,"4. Select speed");
      draw_str(137,45,land);
      draw_str(137,54,speed);
      do
        {
          switch(key=ngetchx())
            {
              case '1':
                randomize(1000*(land[0]-'1'));
                if((score=game(speed[0]-'1'))>hsc_table[4].score)
                  update_hsc(hsc_table,score);
                break;
              case '2':
                high_scores(hsc_table);
                break;
              case '3':
                if(land[0]++=='9') land[0]='1';
                break;
              case '4':
                if(speed[0]++=='5') speed[0]='1';
                break;
              case 264:
                break;
              default:
                key = 0;
            }
        } while(!key);
    }
  pokeIO(0x600017,0xB2);      // Restore normal timer speed
}

// If you have any comments, suggestions, questions or bug reports, mail me at:
// zjuric@utic.net.ba

// Zeljko Juric
// Sarajevo
// Bosnia & Herzegovina
