#define F_CPU 12000000UL                                    /* Clock Frequency = 1Mhz */
#include <avr/io.h>
#include <util/delay.h>
#include <inttypes.h>
#include "display.h"
    
int main(void){                         // The main function

  DDRD=0b11111111;
  DDRC=0b00111110;
  DDRB=0b00000001;
  PORTB=1;




  init_display(15);
  while (1) {                        // Set up an infinite loop

  putsxy(0,0, "987");
  putsxy(5,2, "12:34");  
  _delay_ms(3000);

  }

}
