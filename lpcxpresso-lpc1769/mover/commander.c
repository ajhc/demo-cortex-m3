#include <stdio.h>
#include <string.h>

#include "api.h"
#include "state.h"
#include "alarm.h"
#include "shaker.h"
#include "hexparser.h"
#include "commander.h"
#include "laser.h"

char READY[] = "\r\nReady\r\n";
void respondReady(FILE *output) {
  fflush(output);
  if (output == stdout) {
    usbSendFlush(READY, sizeof(READY)-1);
  } else {
    fiprintf(output, READY);    
  }
}

void respondError(char *msg, FILE *output) {
  fiprintf(output, "result Error %s\r\n", msg);
  respondReady(output);
}

void respondSyntaxError(char *msg, FILE *output) {
  if (output != stderr) {
    fiprintf(stderr, "Got invalid command from USB: %s\r\n", msg);
  }

  if (*msg == '~' || *msg == 'A') {
    fiprintf(stderr, "Note: Ignoring invalid command silently\r\n");

  } else {
    fiprintf(output, "result Error, unknown command: %s\r\n", msg);
    respondReady(output);
  }
}

void cmdHelp(FILE *output) {
    fiprintf(output, "Known commands:\r\n");
    fiprintf(output, "blank line: clear terminal and print status.\r\n");
    fiprintf(output, "st: Print status\r\n");
    fiprintf(output, "pf: Preflight, sets alarms in case of trouble\r\n");
    fiprintf(output, "ac <id>: Clears alarm with <id>\r\n");
    fiprintf(output, "ai <flags>: Ignore alarms\r\n");
    fiprintf(output, "bs Report buffer state\r\n");
    fiprintf(output, "bm (-nc) <moves> <code>... to buffer move codes\r\n");
    fiprintf(output, "bs: Buffer status\r\n");
    fiprintf(output, "br: Buffer reset\r\n");
    fiprintf(output, "jv <x> <y> <z> <a> Jog speed vector\r\n");
    fiprintf(output, "me <axis> <current> <usm>: Motor Enable\r\n");
    fiprintf(output, "mr: Reset motor drivers and turn them off\r\n");
    fiprintf(output, "aa on|off: Turn assist air on or off\r\n");
    fiprintf(output, "ex on|off: Turn exhaust on or off\r\n");
    fiprintf(output, "crc <length> <crc> <id> <command>: Run a command only if the length and checksum matches\r\n");
}

void cmdAssistAir(char *line, FILE *output) {

  if (!strcmp(line, "on")) {
      GPIO_SET(IO_ASSIST_AIR);
      fprintf(output, "result  OK\r\n");

  } else if (!strcmp(line, "off")) {
      GPIO_CLEAR(IO_ASSIST_AIR);
      fprintf(output, "result  OK\r\n");

  } else {
    fiprintf(output, "result  Error: Unable to parse: %s\r\n", line);
  }
}

void cmdExhaust(char *line, FILE *output) {

  if (!strcmp(line, "on")) {
      GPIO_SET(IO_EXHAUST);
      fprintf(output, "result  OK\r\n");
      
  } else if (!strcmp(line, "off")) {
      GPIO_CLEAR(IO_EXHAUST);
      fprintf(output, "result  OK\r\n");

  } else {
    fiprintf(output, "result  Error: Unable to parse: %s\r\n", line);
  }
}


void cmdAlarmClear(char *line, FILE *output) {
  int id;
  if (sscanf(line, "%d", &id) != 1) {
    fiprintf(output, "result  Error: Unable to parse alarm id: %s\r\n", line);    
    return;
  } 

  if (alarmClear(id)) {
    fiprintf(output, "result  Error: Alarm id not valid: %d\r\n", id);      
  } else {
    fiprintf(output, "result  OK: Alarm %d cleared\r\n", id);      
  }

  printAlarmState(output);
}

void cmdAlarmIgnore(char *line, FILE *output) {
  unsigned int flags; 
  if (sscanf(line, "%x", &flags) != 1) {
    fiprintf(output, "result  Error: Unable to parse alarm mask as hex: %s\r\n", line);    
    return;
  }

  alarmsIgnored = flags;
  fiprintf(output, "result  OK: New alarm mask: %x\r\n", alarmsIgnored);

  printAlarmState(output);
}


static const char NOCOMMIT[] = "-nc";

void cmdBufferMoves(char *line, FILE *output) {
  char *lineStart = line;

  char commit = 1;
  while (*line && (*line == ' ' || *line == '-')) {
    if (*line == ' ') {
      line++;
    } else if (!strncmp(line, NOCOMMIT, sizeof(NOCOMMIT))) {
      *line += sizeof(NOCOMMIT);
      commit = 0;
    }
  }
  
  unsigned int moves = 0;
  if (parseHex(&line, &moves) < 1) {
    fprintf(output, "result  Error: Unable to parse the number of moves int at char %d\r\n", (line-lineStart));
    return;
  }

  if (moves > bufferAvailable()) {
    fprintf(output, "result  Error: Not enough room in buffer for %d moves (only %d words free)\r\n", moves, bufferAvailable());
    return;
  }

  while (*line) {
    unsigned int mc;
    if (parseHex(&line, &mc) < 1) {
      fiprintf(output, "result  Error: Unable to parse the move code at char %d\r\n", (line-lineStart));
      bufferRollback();
      return;
    }
    bufferMoveCode(mc);
  }

  if (commit) {
    bufferCommit();
  }
  
  fiprintf(output, "result  OK\r\n");
  printBufferState(output);
  return;
}

void cmdMotorEnable(char *line, FILE *output) {
  unsigned int axis, current, usm;
  if (sscanf(line, "%d %d %d", &axis, &current, &usm) != 3) {
    fprintf(output, "result  Error: Unable to parse motor enable parameters: %s\r\n", line);
    return;
  }

  if (axis < 0 || axis > 3) {
    fprintf(output, "result  Error: Invalid axis: %d, must be (0..3)\r\n", axis);
    return;
  }

  if (usm < 0 || usm > 3) {
    fprintf(output, "result  Error: Invalid microstepping: %d, must be 0..3\r\n", usm);
    return;
  }

  if (current < 0 || current > STEPPER_MAX_CURRENT) {
    fprintf(output, "result  Error: Invalid current: %d, must be (0 to %d)\r\n",
	    current, STEPPER_MAX_CURRENT);
    return;
  }

  axisMotorEnable(&axes[axis], current, usm);
  return;
}

void cmdMotorReset(FILE *output) {

  for (int i=0;i<4;i++) {
    axisMotorEnable(&axes[i], 0, 0);
  }
  setLaserPWM(0);
  GPIO_CLEAR(IO_ASSIST_AIR);
  GPIO_CLEAR(IO_EXHAUST);

  return;
}


void cmdJog(char *line, FILE *output) {
  int axes[4];
  if (sscanf(line, "%d %d %d %d", axes+0, axes+1, axes+2, axes+3) != 4) {
    fprintf(output, "result  Error: Unable to parse jog speed vector: %s\r\n", line);
    return;
  }

  if (!bufferIsEmpty()) {
    fprintf(output, "result Error: Cannot jog while moves are buffered\r\n");
    return;
  }

  setJogSpeed(axes);
  
  fiprintf(output, "result  OK\r\n");
  printMotionState(output);
}

void cmdLaserPWM(char *line, FILE *output) {
  unsigned int pwm;
  if (sscanf(line, "%d", &pwm) != 1) {
    fprintf(output, "result  Error: Unable to parse pwm: %s\r\n", line);
    return;
  }

  setLaserPWM(pwm);

  fiprintf(output, "result  OK\r\n");
}

unsigned int lastCommandId;

void cmdWRAP(char *line, FILE *output) {
  unsigned int length;
  if (parseHex(&line, &length) < 1) {
    fprintf(output, "result  WRAP-Error: Unable to parse length: %s\r\n", line);
    respondReady(output);
    return;
  }

  unsigned int crc;
  if (parseHex(&line, &crc) < 1) {
    fprintf(output, "result  WRAP-Error: Unable to parse crc: %s\r\n", line);
    respondReady(output);
    return;
  }
  
  unsigned int cmdid;
  if (parseHex(&line, &cmdid) < 1) {
    fprintf(output, "result  WRAP-Error: Unable to parse id: %s\r\n", line);
    respondReady(output);
    return;
  }
  
  // Skip whitespace
  while (*line == ' ') {
    line++;
  }

  unsigned int pl = 0;
  unsigned int ps = 0;
  char *payload = line;
  char pc;
  
  while ((pc = *(payload++))) {
    pl++;
    ps += pc;
  }

  if (pl != length) {
    fprintf(output, "result  WRAP-Error: Payload length mismatch: %x != %x\r\n", pl, length);
    respondReady(output);
    return;
  }

  if (ps != crc) {
    fprintf(output, "result  WRAP-Error: Payload checksum mismatch: %x != %x\r\n", ps, crc);
    respondReady(output);
    return;
  }

  fiprintf(output, "wrap.ack %x\r\n", cmdid);

  if (cmdid == lastCommandId) { 
    // Repeat command, so check if it's not an idempotent command and skip it if it is
    lastCommandId = cmdid;

    if (!strncmp(line, "bm ", 3)) {
      fiprintf(output, "result  OK\r\n");
      printBufferState(output);
      return;
    }
  }

  lastCommandId = cmdid;
  commandRun(line, output); 
}

void commandRun(char *line, FILE *output) {
  if (!*line) {
    fiprintf(output, "\x1b[2JPhotonSaw console\r\n");
    printState(output);
    fiprintf(output, "\r\nTry ? for help\r\n");
    respondReady(output);
    return;
  }

  if (!strncmp(line, "wrap ", 5)) {
    cmdWRAP(line+5, output);
    return; // Avoid duplicate Ready

  } else if (!strncmp(line, "bm ", 3)) {
    cmdBufferMoves(line+3, output);
    
  } else if (!strcmp(line, "bs")) {
    fprintf(output, "result  OK\r\n");
    printBufferState(output);

  } else if (!strcmp(line, "br")) {
    fprintf(output, "result  OK\r\n");
    shakerResetBuffer();
    printBufferState(output);

  } else if (!strcmp(line, "?")) {
    cmdHelp(output);

  } else if (!strcmp(line, "st")) {
    fprintf(output, "result  OK\r\n");
    printState(output);

  } else if (!strcmp(line, "pf")) {
    fprintf(output, "result  OK\r\n");
    checkAlarmInputs();
    printState(output);

  } else if (!strncmp(line, "ac ", 3)) {
    cmdAlarmClear(line+3, output);

  } else if (!strncmp(line, "ai ", 3)) {
    cmdAlarmIgnore(line+3, output);
    
  } else if (!strncmp(line, "me ", 3)) {
    cmdMotorEnable(line+3, output);
    
  } else if (!strncmp(line, "mr", 2)) {
    cmdMotorReset(output);
    
  } else if (!strncmp(line, "jv ", 3)) {
    cmdJog(line+3, output);
    
  } else if (!strncmp(line, "aa ", 3)) {
    cmdAssistAir(line+3, output);

  } else if (!strncmp(line, "ex ", 3)) {
    cmdExhaust(line+3, output);

  } else if (!strncmp(line, "lp ", 3)) {
    cmdLaserPWM(line+3, output);

  } else {
    respondSyntaxError(line, output);
    return;
  }
  respondReady(output);
}
