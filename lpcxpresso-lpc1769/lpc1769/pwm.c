#include "lpc17xx_pwm.h"
#include "board.h"
#include "pwm.h"


void configurePWMpin(const unsigned int pin) {
  const int channel = IO_CHAN(pin);

  /* Configure each PWM channel: --------------------------------------------- */
  /* - Single edge
   * - PWM Duty on each PWM channel determined by
   * the match on channel 0 to the match of that match channel.
   * Example: PWM Duty on PWM channel 1 determined by
   * the match on channel 0 to the match of match channel 1.
   */

  /* Configure PWM channel edge option
   * Note: PWM Channel 1 is in single mode as default state and
   * can not be changed to double edge mode */
  if (channel > 1) {
    PWM_ChannelConfig(LPC_PWM1, channel, PWM_CHANNEL_SINGLE_EDGE);
  }
  
  /* Configure match value for each match channel */
  /* Set up match value */
  PWM_MatchUpdate(LPC_PWM1, channel, 0, PWM_MATCH_UPDATE_NOW);

  /* Configure match option */
  PWM_MATCHCFG_Type PWMMatchCfgDat;
  PWMMatchCfgDat.MatchChannel = channel;
  PWMMatchCfgDat.IntOnMatch = DISABLE;
  PWMMatchCfgDat.ResetOnMatch = DISABLE;
  PWMMatchCfgDat.StopOnMatch = DISABLE;
  PWM_ConfigMatch(LPC_PWM1, &PWMMatchCfgDat);

  /* Enable PWM Channel Output */
  PWM_ChannelCmd(LPC_PWM1, channel, ENABLE);

  configPin(pin);
}

void initPWM() {
  
  /* PWM block section -------------------------------------------- */
  /* Initialize PWM peripheral, timer mode
   * PWM prescale value = 1 (absolute value - tick value) */
  PWM_TIMERCFG_Type PWMCfgDat;
  PWMCfgDat.PrescaleOption = PWM_TIMER_PRESCALE_TICKVAL;
  PWMCfgDat.PrescaleValue = 1;
  PWM_Init(LPC_PWM1, PWM_MODE_TIMER, (void *) &PWMCfgDat);

  /* Set match value for PWM match channel 0 = 256, update immediately */
  PWM_MatchUpdate(LPC_PWM1, 0, 256, PWM_MATCH_UPDATE_NOW);
  /* PWM Timer/Counter will be reset when channel 0 matching
   * no interrupt when match
   * no stop when match */
  PWM_MATCHCFG_Type PWMMatchCfgDat;
  PWMMatchCfgDat.IntOnMatch = DISABLE;
  PWMMatchCfgDat.MatchChannel = 0;
  PWMMatchCfgDat.ResetOnMatch = ENABLE;
  PWMMatchCfgDat.StopOnMatch = DISABLE;
  PWM_ConfigMatch(LPC_PWM1, &PWMMatchCfgDat);

  configurePWMpin(IO_X_CURRENT);
  configurePWMpin(IO_Y_CURRENT);
  configurePWMpin(IO_Z_CURRENT);
  configurePWMpin(IO_A_CURRENT);
  configurePWMpin(IO_LASER_POWER);

  /* Reset and Start counter */
  PWM_ResetCounter(LPC_PWM1);
  PWM_CounterCmd(LPC_PWM1, ENABLE);
  PWM_Cmd(LPC_PWM1, ENABLE);
}

void setPWM(const unsigned int channel, const unsigned int value) {
  PWM_MatchUpdate(LPC_PWM1, channel, value, PWM_MATCH_UPDATE_NOW);
}

