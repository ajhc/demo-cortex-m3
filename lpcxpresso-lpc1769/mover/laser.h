#ifndef __LASER_H__
#define __LASER_H__

extern unsigned int currentLaserPWM;
extern unsigned int currentLaserOn;

inline void setLaserPWM(unsigned int laserPWM) {
  laserPWM &= 0xff;
  
  if (laserPWM != currentLaserPWM) {
    currentLaserPWM = laserPWM;
    setPWM(IO_CHAN(IO_LASER_POWER), 0xff-laserPWM);
  }
}

inline void setLaserFire(unsigned int laserOn) {

  if (laserOn && !currentLaserOn) {
    GPIO_SET(IO_LASER_FIRE);
    currentLaserOn = 1;

  } else if (!laserOn && currentLaserOn) {
    GPIO_CLEAR(IO_LASER_FIRE);
    currentLaserOn = 0;
  }
}

#endif
