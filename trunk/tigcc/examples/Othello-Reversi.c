// OTHELLO/REVERSI V1.6 (C) 2000 by Zeljko Juric    
// You need the TIGCC cross-compiler to compile it

// Only use one of these at a time if you want to save space and increase speed
#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define MIN_AMS 100

#include <string.h>
#include <setjmp.h>
#include <graph.h>
#include <kbd.h>
#include <compat.h>

// Key codes

#define ESC_KEY 264                    // Key codes
#define ENTER_KEY 13

// Macro for selecting right constants for both TI-89 and TI-92 Plus

#define Q89_92(x,y) (TI89?(x):(y))

// Type definitinos

typedef enum Bool BOOL;
typedef enum {WHITE,BLACK,BLANK_NEAR,BLANK_FAR} FIGURE;
typedef char BOARD[10][10];

// Forward declarations

void draw_figure(int,int,char);
void draw_str(int,int,const char*);
void draw_line(int,int,int,int);
void display_board(void);
void one_player_game(void);
void two_players_game(void);
void input_move(int*,int*);
int try_a_move(int,int,char);
int minmax_search(char,int,int,int*,int*);
int getch(void);
int convert_key(int);
BOOL human_move(char);
BOOL computer_move(void);

// Global and static data must have initializers in "nostub" mode.

BOARD board={};
int level=0;
unsigned int rand_seed=0;
char jmp_buffer[56]={};
BOOL black_on_turn=FALSE,two_players=FALSE,escaped=FALSE;
BOOL black_played=TRUE,white_played=TRUE;

// Macro for randomly selecting TRUE or FALSE (this is a very bad algorithm,
// but good enough for a purpose in this game)

#define random ((rand_seed=75*rand_seed+1)>=32768)

// Main program

void _main(void)
{
  SCR_RECT screen_area={{0,0,Q89_92(159,239),Q89_92(99,127)}};
  BOOL restore=FALSE;
  int i,j,score=0,answer=0;
  const char *level_names[]={"NOVICE","ELEMENTARY","MEDIUM","GOOD","ADVANCED",
    "EXPERT"};  
  LCD_BUFFER scr_buf;
  BOARD board_save;
  LCD_save(scr_buf);
  if(!setjmp(jmp_buffer))
    {
      if(escaped)
        {
          display_board();
          draw_str(5,Q89_92(90,118),"RESTORE A BREAKED GAME (Y/N)?");
          while(answer!='y'&&answer!='n')
            answer=getch()|32;
          restore=(answer=='y');
        }
      escaped=FALSE;
      if(!restore)
        {
          black_on_turn=FALSE; black_played=TRUE; white_played=TRUE;
          for(i=0;i<=9;i++)
            for(j=0;j<=9;j++)
              board[i][j]=BLANK_FAR;
          for(i=3;i<=6;i++)
            for(j=3;j<=6;j++)
              board[i][j]=BLANK_NEAR;
          board[5][4]=board[4][5]=BLACK;
          board[4][4]=board[5][5]=WHITE;
          display_board();

          if(TI89)
            FontSetSys(F_6x8);
          draw_str(Q89_92(5,120),90,"[1] or [2] players?");
          while(answer<'1'||answer>'2')
            answer=getch();
          if(!(two_players=answer=='2'))
            {
              display_board();
              if(TI89)
                FontSetSys(F_6x8);
              draw_str(Q89_92(5,130),90,"Level (1-6)?");
              answer=0;
              while(answer<'1'||answer>'6')
                answer=getch();
              level=answer-'0';
              display_board();

              if(!TI89)
                {
                  draw_str(130,90,"Level: ");
                  draw_str(172,90,level_names[level-1]);
                }
              draw_str(5,Q89_92(90,118),
                "DO YOU WANT TO HAVE A FIRST MOVE (Y/N)?");
              while(answer!='y'&&answer!='n')
                answer=getch()|32;
              black_on_turn=(answer=='y');
            }
        }
      display_board();
      memcpy(board_save,board,sizeof(BOARD));
      escaped=TRUE;
      while(black_played||white_played)
        {

          if(TI89)
            draw_str(5,90,"Made using TI-GCC compiler by Xavier Vassor");
          else
            {
              FontSetSys(F_4x6);
              draw_str(5,118,
                "Compiled using TI-GCC compiler by Xavier Vassor & Jean Canazzi");
              FontSetSys(F_6x8);
            }
          if(two_players)
            draw_str(Q89_92(93,130),Q89_92(70,90),"TWO-PLAYER MODE");
          else
            {
              draw_str(Q89_92(93,130),Q89_92(70,90),"Level: ");
              draw_str(Q89_92(117,172),Q89_92(70,90),level_names[level-1]);
            }
          if(black_on_turn)
            black_played=human_move(BLACK);
          else if(two_players)
            white_played=human_move(WHITE);
          else
            white_played=computer_move();
          display_board();
          memcpy(board_save,board,sizeof(BOARD));
          black_on_turn=!black_on_turn;
        }
      escaped=FALSE;
      FontSetSys(Q89_92(F_6x8,F_8x10));
      draw_str(Q89_92(93,130),Q89_92(42,50),"Game over!");
      for(i=1;i<=8;i++)
        for(j=1;j<=8;j++)
          score+=(board[i][j]==WHITE)-(board[i][j]==BLACK);
      FontSetSys(Q89_92(F_8x10-two_players,F_8x10));
      draw_str(Q89_92(93,130),Q89_92(55,65),
        score?((score>0)?(two_players?"White won!":"I won!"):
          (two_players?"Black won!":"You won!")):"Draw!");
      getch();
    }
  for(i=1;i<Q89_92(100,140);i++)
    DrawClipEllipse(Q89_92(80,120),Q89_92(50,64),i,i,&screen_area,A_NORMAL);
  LCD_restore(scr_buf);
  memcpy(board,board_save,sizeof(BOARD));
}

// Tries to perform a move, and returns an advance in the material plus
// extra bonus for corners (returns zero if the move is not possible)  

int try_a_move(int i,int j,char figure)
{
  int captured,loc_i,loc_j,i_step,j_step,p,q,score=0;
  char next;
  if(board[i][j]==BLANK_NEAR)
    {
      for(i_step=-1;i_step<=1;i_step++)
        for(j_step=-1;j_step<=1;j_step++)
          if(i_step||j_step)
            {
               loc_i=i; loc_j=j; captured=0;
               do
                 {
                   next=board[loc_i+=i_step][loc_j+=j_step];
                   if(next>=BLANK_NEAR)
                     captured=0;
                   else if(next!=figure)
                     captured++;
                 } while(next==!figure);
              if(captured)
                {
                  loc_i=i; loc_j=j;
                  while(board[loc_i+=i_step][loc_j+=j_step]!=figure)
                    board[loc_i][loc_j]=figure;
                  score+=captured;
                }
            }
      if(score)
        {
          board[i][j]=figure;
          for(p=i-1;p<=i+1;p++)
            for(q=j-1;q<=j+1;q++)
              if(board[p][q]==BLANK_FAR)
                board[p][q]=BLANK_NEAR;
          if((i==1||i==8)&&(j==1||j==8))
            score+=3;
          score+=!score;
        }
    }
  return score;
}

// Performs a human move, and returns FALSE if there is no moves avaliable

BOOL human_move(char figure)
{
  int i,j,score,char1,char2;
  BOARD board_save;
  memcpy(board_save,board,sizeof(BOARD));
  if(two_players)
    draw_str(Q89_92(93,130),Q89_92(34,46),
      black_on_turn?">> BLACK <<":">> WHITE <<");
  for(i=1;i<=8;i++)
    for(j=1;j<=8;j++)
      if(try_a_move(i,j,figure))
        {
          memcpy(board,board_save,sizeof(BOARD));
          do
            {
              input_move(&char1,&char2);
              if(!(score=try_a_move(char2-'0',char1-'A'+1,figure)))
                draw_str(Q89_92(93,130),Q89_92(55,68),"WRONG MOVE!");
            } while(!score);
          return TRUE;
        }
  draw_str(Q89_92(93,130),Q89_92(42,56),"You must pass!");
  getch();
  return FALSE;
}

// Recursive minmax search for a best move with alpha pruning: returns best  
// minmax value, and as a side effect, returns coordinates of such best move 

int minmax_search(char figure,int depth,int alpha,int *best_i,int *best_j)
{
  int i,j,max=-1000,score,dummy;
  BOARD board_save;
  *best_i=0; *best_j=0;
  memcpy(board_save,board,sizeof(BOARD));
  if(kbhit())
    if(ngetchx()==ESC_KEY)
      longjmp(jmp_buffer,1);
  for(i=1;i<=8;i++)
    for(j=1;j<=8;j++)
      if((score=try_a_move(i,j,figure)))       // Yes, "=", not "==" !
        {
          if(depth!=level)
            score-=minmax_search(!figure,depth+1,score-max,&dummy,&dummy);
          if(level==1)
            score+=5*((i==1||i==8)+(j==1||j==8)-(i==2||i==7)-(j==2||j==7));
          if(score>max)
            {
              max=score; *best_i=i; *best_j=j;
            }
          else if((score==max)&&random)
            {
              *best_i=i; *best_j=j;
            }
          memcpy(board,board_save,sizeof(BOARD));
          if(score>=alpha)
            return max;
        }
  if(!*best_i)
    return 0;
  return max;
}

// Performs computer move, and returns FALSE if there is no avaliable moves

BOOL computer_move(void)
{
  int best_i,best_j;
  char str[50];
  draw_str(Q89_92(93,130),Q89_92(42,56),"Thinking...");
  minmax_search(WHITE,1,1000,&best_i,&best_j);
  if(!best_i)
    draw_str(Q89_92(93,130),Q89_92(50,66),"I must pass!");
  else
    {
      try_a_move(best_i,best_j,WHITE);
      sprintf(str,"My move: %c%d",best_j-1+'A',best_i);
      draw_str(Q89_92(93,130),Q89_92(50,68),str);
    }
  getch();
  return best_i!=0;
}

// Asks the user for a move, and put its coordinates in char1 and char2

void input_move(int *char1,int *char2)
{
  char tmp[]=" _";
  int key=0;
  while(key!=ENTER_KEY)
    {
      draw_str(Q89_92(93,130),Q89_92(42,56),"Your move: _     ");
      *char1=key&0xffdf;
      while(*char1<'A'||*char1>'H')
        *char1=Q89_92(convert_key(getch()),getch())&0xffdf;
      *tmp=*char1;
      draw_str(Q89_92(133,196),Q89_92(42,56),tmp);
      *char2=0;
      while(*char2<'1'||*char2>'8')
        *char2=getch();
      *tmp=*char2;
      draw_str(Q89_92(133+FontCharWidth(*char1),202),Q89_92(42,56),tmp);
      key=getch();
    }
}

// Routine for getting a key, with handling ESC keypress

int getch(void)
{
  int i;
  if ((i=ngetchx())!=ESC_KEY)
    return i;
  else
    longjmp(jmp_buffer,1);
  return 0;
}

#ifdef USE_TI89

// Converts non-alpha keys into alpha ones on TI-89 (for easier typing)
// '='->'A' '('->'B' ')'->'C' ','->'D' '/'->'E' '|'-> 'F' '7'->'G' '8'->'H'

int convert_key(int key)
{
  int i;
  for(i=0;i<8;i++)
    if(key=="=(),/|78"[i])
      return 'A'+i;
  return key;
}

#endif

// Displays the board

void display_board(void)
{
  int i,j,black_score=0,white_score=0;
  char xs[]="A",ys[]="1";
  char str[50];
  ClrScr();
  FontSetSys(Q89_92(F_4x6,F_6x8));
  draw_str(5,1,"OTHELLO/REVERSI V1.5  (C) Zeljko Juric");
  for(i=Q89_92(16,23);i<=Q89_92(80,111);i+=Q89_92(8,11))
    {
      draw_line(Q89_92(16,23),Q89_92(i+2,i),Q89_92(80,111),Q89_92(i+2,i));
      draw_line(i,Q89_92(18,23),i,Q89_92(82,111));
      if(i!=Q89_92(80,111))
        {
          draw_str(i+Q89_92(2,3),Q89_92(12,14),xs); (*xs)++;
          draw_str(Q89_92(10,14),i+Q89_92(4,3),ys); (*ys)++;           
        }
    }
  for(i=1;i<=8;i++)
    for(j=1;j<=8;j++)
      {
        draw_figure(Q89_92(8*j+10,11*j+14),Q89_92(8*i+12,11*i+14),
          board[i][j]);
        black_score+=(board[i][j]==BLACK);
        white_score+=(board[i][j]==WHITE);
      }
  sprintf(str,two_players?"White: %d  Black: %d":"Me: %d  You: %d",
    white_score,black_score);
  draw_str(Q89_92(93,130-11*two_players),Q89_92(20,30),str);
}

// Draws a figure on screen

void draw_figure(int x,int y,char c)
{
  int i;
  if(c==WHITE||c==BLACK)
    {
      draw_line(x+Q89_92(1,2),y,x+Q89_92(3,5),y);
      draw_line(x+Q89_92(1,2),y+Q89_92(4,7),x+Q89_92(3,5),y+Q89_92(4,7));
    }
  if(c==WHITE)
    {
      if(TI89)
        {
          draw_line(x,y+1,x,y+3); draw_line(x+4,y+1,x+4,y+3);
        }
      else
        {
          draw_line(x,y+2,x+2,y); draw_line(x+5,y,x+7,y+2);
          draw_line(x,y+2,x,y+5); draw_line(x+7,y+2,x+7,y+5);
          draw_line(x,y+5,x+2,y+7); draw_line(x+5,y+7,x+7,y+5);
        }
    }
  else if(c==BLACK)
    {
      for(i=y+Q89_92(1,2);i<=y+Q89_92(3,5);i++)
        draw_line(x,i,x+Q89_92(4,7),i);
        if(!TI89)
          draw_line(x+1,y+1,x+6,y+1); draw_line(x+1,y+6,x+6,y+6);
    }
}

// Drawing lines and strings are used so often in this game, so defining them
// as a short subroutines which just calls the ROM equivalents makes the whole
// program a bit shorter

void draw_str(int x,int y,const char *str)
{
  DrawStr(x,y,str,A_REPLACE);
}

void draw_line(int x1,int y1,int x2,int y2)
{
  DrawLine(x1,y1,x2,y2,A_NORMAL);
}

// Please mail any comments, suggestions, questions and bug reports to: 
// zjuric@utic.net.ba

// Zeljko Juric
// Sarajevo
// Bosnia & Herzegovina
