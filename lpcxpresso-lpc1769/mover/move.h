#ifndef __MOVE_H__
#define __MOVE_H__

/*
  This header specifies the move byte code which the machine can execute,
  a number of 32 bit words are used to make up each move.

  The structure of a move is:

Header: 05AAffff
Duration: Number of ticks this move lasts
0: ID 
1: X speed
2: X accel
12&&(1|2): X end pos
3: Y speed
4: Y accel
12&&(3|4): Y end pos
5: Z speed
6: Z accel
12&&(5|6): Z end pos
7: A speed
8: A accel
12&&(7|8): A end pos
13: Switches: bits to set  
13: Switches: bits to clear
9: LASER intensity 
10: LASER intensity acceleration
11: Pixel speed
11: Pixel word count
... pixel word count pixels

The lower 16 bits of the header word (ffff above) signal that the corrosponding
optional word is present in the following data.

If the ID word is omitted, then the id of the move is one higher than the previous one,
the ID is used to provide a progress indication to the controlling host and to assist in
debugging, it's not otherwise significant.

The speed and acceleration values are fixed point, signed values with 30 bits to the
right of the period or Q2.30.

To get the actual speed in steps / tick do: speed >> 30

If a speed is omitted, then it is 0 at the start of the move.

If an acceleration is omitted, then it's 0.


When bit 12 is set, then an extra word is emitted for each axis with movement (speed or acceleration) and
the firmware checks that the position after the move matches the supplied position, if the position doesn't match,
an alarm is triggered, this mechanism is used to validate that the host controllers algorithms match the firmware.


If LASER intensity is present then the laser will be on and it is a packed word consisting of:
0x1A5E00ii

Where ii is the LASER PWM power value, if the LASER is not supposed to be on, do not
set ii to zero, simply omit the LASER intensity word.


If pixel speed is present then it's used to count through the pixels, just like the
other speeds count though steps.

The pixel word count is only there to allow parsing though the moves without doing
any calculation on the pixel speed, it can also be used to sanity check the code.

If pixels are present then the laser word must also be present to set the pwm and enable the laser.
*/

#define MOVE_MAGIC      0x05aa0000

#define IS_MOVE_START_CODE(x) (((x) & 0xffff0000) == MOVE_MAGIC)

#define MOVE_HAS_ID(x) ((x) & (1<<0))

#define MOVE_HAS_SPEED(x, a)          ((x) & (1<<(((a)<<1)+1)))
#define MOVE_HAS_ACCEL(x, a)          ((x) & (1<<(((a)<<1)+2)))
#define MOVE_HAS_ACCEL_OR_SPEED(x, a) ((x) & (3<<(((a)<<1)+1)))

#define MOVE_HAS_XS(x) ((x) & (1<<1))
#define MOVE_HAS_XA(x) ((x) & (1<<2))

#define MOVE_HAS_YS(x) ((x) & (1<<3))
#define MOVE_HAS_YA(x) ((x) & (1<<4))

#define MOVE_HAS_ZS(x) ((x) & (1<<5))
#define MOVE_HAS_ZA(x) ((x) & (1<<6))

#define MOVE_HAS_AS(x) ((x) & (1<<7))
#define MOVE_HAS_AA(x) ((x) & (1<<8))

#define MOVE_HAS_LASER(x) ((x) & (1<<9))
#define MOVE_HAS_LASER_A(x) ((x) & (1<<10))
#define MOVE_HAS_PIXELS(x) ((x) & (1<<11))

#define MOVE_HAS_SWITCHES(x) ((x) & (1<<13))


#define LASER_MAGIC      0x1A5E0000

#define IS_LASER_CODE(x) (((x) & 0xffff0000) == LASER_MAGIC)
#define LASER_PWM(x)      ((x) & 0x000000ff)

#define ONE_STEP_ORDER 30
#define ONE_STEP (1<<ONE_STEP_ORDER)

#define MOVE_SW_ASSIST_AIR (1<<0)

#endif
