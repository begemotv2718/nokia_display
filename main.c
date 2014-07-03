#define F_CPU 12000000UL                                    /* Clock Frequency = 1Mhz */

#include <inttypes.h>
#include <string.h>
#include <avr/io.h>
#include <util/delay.h>
#include <avr/pgmspace.h>

#define DISP_SCE BIT(2)
#define DISP_SDC BIT(3)
#define DISP_DIN BIT(4)
#define DISP_CLK BIT(5)
#define DISP_RESET BIT(1)
#define DISP_PORT PORTC


#define DISP_COMMAND 0
#define DISP_DATA 1

#define DISP_XSIZE 84
#define DISP_YSIZE 6


#define DISP_POWERDOWN 0x04
#define DISP_ENTRYMODE 0x02
#define DISP_EXTENDEDINSTRUCTION 0x01

#define DISP_DISPLAYBLANK 0x0
#define DISP_DISPLAYNORMAL 0x4
#define DISP_DISPLAYALLON 0x1
#define DISP_DISPLAYINVERTED 0x5

// H = 0
#define DISP_FUNCTIONSET 0x20
#define DISP_DISPLAYCONTROL 0x08
#define DISP_SETYADDR 0x40
#define DISP_SETXADDR 0x80

// H = 1
#define DISP_SETTEMP 0x04
#define DISP_SETBIAS 0x10
#define DISP_SETVOP 0x80



#define BIT_GET(p,m) ((p) & (m))
#define BIT_SET(p,m) ((p) |= (m))
#define BIT_CLR(p,m) ((p) &= ~(m))
#define BIT_FLIP(p,m) ((p) ^= (m))
#define BIT_WRITE(c,p,m) (c ? BIT_SET(p,m) : BIT_CLR(p,m))
#define BIT(x) (0x01 << (x))
#define LONGBIT(x) ((unsigned long)0x00000001 << (x))


uint8_t data[DISP_XSIZE*DISP_YSIZE];

PROGMEM const char font[] = {
0x00,0x3c,0x42,0x3c,0x00,
0x00,0x44,0x7e,0x40,0x00,
0x64,0x52,0x52,0x4c,0x00,
0x22,0x4a,0x4e,0x32,0x00,
0x18,0x14,0x7e,0x10,0x00,
0x2e,0x4a,0x4a,0x32,0x00,
0x3c,0x4a,0x4a,0x30,0x00,
0x02,0x62,0x1a,0x06,0x00,
0x34,0x4a,0x4a,0x34,0x00,
0x0c,0x52,0x52,0x3c,0x00,
0x00,0x6c,0x6c,0x00,0x00
};

#define FONT_CHARWIDTH 5
#define FONT_STARTCHAR 0x30
#define FONT_ENDCHAR 0x3A

void send_byte(unsigned char db, unsigned char dc){
  unsigned char bit;
  BIT_WRITE(dc,DISP_PORT,DISP_SDC);
  BIT_CLR(DISP_PORT,DISP_SCE);
  for(bit = 0x80; bit; bit >>=1){
    BIT_CLR(DISP_PORT,DISP_CLK);
    if(db & bit){
      BIT_SET(DISP_PORT,DISP_DIN);
    }else{
      BIT_CLR(DISP_PORT,DISP_DIN);
    }
    _delay_ms(1);
    BIT_SET(DISP_PORT,DISP_CLK);
    _delay_ms(1);
  }
} 

  
void init_display(uint8_t contrast){
  BIT_CLR(DISP_PORT,DISP_RESET);
  _delay_ms(500);
  BIT_SET(DISP_PORT,DISP_RESET);
  _delay_ms(5);
  if(contrast>0x7f){
    contrast=0x7f;
  }
  send_byte(DISP_FUNCTIONSET | DISP_EXTENDEDINSTRUCTION,DISP_COMMAND);
  send_byte(DISP_SETVOP | contrast,DISP_COMMAND);
  send_byte(DISP_SETBIAS | 0x04,DISP_COMMAND);
  send_byte(DISP_FUNCTIONSET,DISP_COMMAND);
  send_byte(DISP_DISPLAYCONTROL | DISP_DISPLAYNORMAL,DISP_COMMAND);

  send_byte(DISP_SETYADDR, DISP_COMMAND);
  send_byte(DISP_SETXADDR, DISP_COMMAND);
  memset(data,0,sizeof(data));
}



void upload_box(uint8_t x, uint8_t y, uint8_t dx, uint8_t dy){ 
  uint8_t i,j;
  send_byte(DISP_SETYADDR |y , DISP_COMMAND);
  for(j=y; (j<y+dy && j<DISP_YSIZE); j++){
    send_byte(DISP_SETXADDR |x , DISP_COMMAND);
    for(i=x;i<x+dx;i++){
      send_byte(data[j*DISP_XSIZE+i], DISP_DATA);
    }
  }
}  

void putsxy(uint8_t x, uint8_t y, char *msg){
  uint8_t curx;
  curx=x;
  while(*msg){
    if(*msg>=FONT_STARTCHAR && *msg<=FONT_ENDCHAR){
      for(uint8_t i=0; i< FONT_CHARWIDTH; i++){
        data[y*DISP_XSIZE+curx]=pgm_read_byte(&font[(*msg-FONT_STARTCHAR)*FONT_CHARWIDTH+i]);
        curx++;
        if(curx>DISP_XSIZE) break;
      }
      if(curx>DISP_XSIZE) break;
    }
    msg++;
  }
  upload_box(x,y,curx-x+1,1);
}
/*  
int read_data(unsigned char* result){
  unsigned char i;
  DDRB=0b00000001;
  PORTB=0;
  _delay_ms(18);
  PORTB=1;
  _delay_us(20);
  DDRB=0b00000000;
  i=0;
  while((PINB & 1)&&(i<100)){i++;}
  if(i>=100) return(1);
  i=0;
  while(!(PINB & 1) && (i<100)){i++;}
  if(i>=100) return(2);
  i=0;
  while((PINB &1) && (i<100)){i++;}
  if(i>=100) return(3);
  
  unsigned char charpos;
  unsigned char bitpos;
  
 
  for(charpos=0;charpos<5;charpos++){
    result[charpos]=0;
    for(bitpos=0; bitpos<8;bitpos++){
      i=0;
      while(!(PINB & 1) && (i<100)){i++;}
      if(i>=100) return(4);
      i=0;
      while((PINB &1) && (i<255)){i++;}
      if(i>=255) return(5+bitpos);
      if(i>5){
        //1 transmitted
        result[charpos] |= 1<<(7-bitpos);
      }
    }
  }
  return 0;
}
*/
       
    
int main(void){                         // The main function

  DDRD=0b11111111;
  DDRC=0b00111110;
  DDRB=0b00000001;
  PORTB=1;




  init_display(15);
  while (1) {                        // Set up an infinite loop
  send_byte(DISP_SETXADDR |0,DISP_COMMAND);
  send_byte(DISP_SETYADDR |0,DISP_COMMAND);
  /* 
  send_byte(0x1F,DISP_DATA);
  send_byte(0x05,DISP_DATA);
  send_byte(0x07,DISP_DATA);
  send_byte(0x00,DISP_DATA);
  send_byte(0x1F,DISP_DATA);
  send_byte(0x04,DISP_DATA);
  send_byte(0x1F,DISP_DATA);
  char *msg="0123";
  while(*msg){
    for(uint8_t i=0; i<FONT_CHARWIDTH; i++){
      send_byte(pgm_read_byte(&font[(*msg-FONT_STARTCHAR)*FONT_CHARWIDTH + i]),DISP_DATA);
    }
    msg++;
  }
  */

  putsxy(5,2, "12:34");  
  BIT_SET(DISP_PORT,DISP_SCE);
  _delay_ms(3000);

  }

}
