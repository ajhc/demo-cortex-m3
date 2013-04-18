#include <stdio.h>
#include <string.h>

#include "api.h"
#include "commander.h"

char commandBuffer[1<<7];
volatile int pendingCommand;

void handleUart0Line(const char *line, int lineLength) {
  if (!pendingCommand) {
    strcpy(commandBuffer, line);
    pendingCommand = 1;
  }
}

int consolePending() {
  return pendingCommand;
}

void consoleHandle() {
  if (!pendingCommand) {
    return;
  }

  commandRun(commandBuffer, stderr);

  pendingCommand = 0;
}
