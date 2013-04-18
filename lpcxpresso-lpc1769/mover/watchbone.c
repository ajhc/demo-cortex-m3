#include <stdio.h>
#include <string.h>
#include "api.h"

#include "state.h"

WatchdogState wdStates[2];
unsigned int wdStateIndex;

void handleUart3Line(const char *line, int lineLength) {
  if (lineLength) {
    wdStateIndex = wdStateIndex ? 0:1;

    wdStates[wdStateIndex].timestamp = systick;
    strncpy(wdStates[wdStateIndex].msg, line, WD_STATE_MAX-1);
    wdStates[wdStateIndex].bad = strcmp(line, "OK");

    wdState = wdStates+wdStateIndex;
  }
}
