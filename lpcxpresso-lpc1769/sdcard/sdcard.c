#include <string.h>
#include <stdio.h>

#include "api.h"


int main(void) {
  fiprintf(stderr, "Power Up!\n\r");

  FILE *f = fopen("/sd/test.log", "a");
  fprintf(f, "Test text\n\r");
  fclose(f);

  FILE *f1 = fopen("/sd/test.log", "r");
  while (!feof(f1)) {
    char buffer[100];
    *buffer = 0;
    fgets(buffer, sizeof(buffer), f1);
    fprintf(stderr, buffer);
  }
  fclose(f1);


  /*
  FRESULT r = f_mkfs(0, 0, 0);
  fiprintf(stderr, "MKFS result: %d\n\r", r);
  */

  while (1) {
    GPIO_SET(IO_LED);
    GPIO_SET(IO_ASSIST_AIR);
    GPIO_CLEAR(IO_EXHAUST);
    GPIO_CLEAR(IO_LASER_FIRE);
    delay(200);

    GPIO_CLEAR(IO_LED);
    GPIO_CLEAR(IO_ASSIST_AIR);
    GPIO_SET(IO_EXHAUST);
    GPIO_SET(IO_LASER_FIRE);
    delay(200);
  }
}
