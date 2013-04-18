This directory contains the various pieces of firmware needed for the PhotonSaw.

The tools subdirectory contains a script which will download and build the entire toolchain needed along with the NXP driver code.


LPC1769 software:

The lpc1769 directory contains the low level framework
(startup code, linker script, makefile, openocd config)
needed to build the lpc1769 based firmware.

* blinky: A very minimal test project that simply blinks the status LED.
* uart: blinky plus output via debug serial port and ADC readout
* stepper: is a step motor test program
* sdcard: SDcard and FAT test code
* mover: The actual 


ATMega328 software:

The atmega328 directory contains the framework for the AVR based part of the firmware:

* lapdog: A sample firmware for the watchdog MCU, it doesn't really protect
anything, it just flashes the LEDs and enables both steppers and the LASER.




-------------

= Thoughts on the general structure of the main firmware =


USBCDC 
 -> usbDataCallback(char *data) 
   -> usbLineCallback(char *line)
    -> Interpret command

G-code commands are buffered and the reply includes the space taken in the buffer by the command
as well as the space left to allow flow control.

     --> g: G-code + store in buffer + handshake with buffer free.
     --> q: Machine query
     --> c: Machine control                                    
     <-- r: Reply message

The host must ensure that only one command is sent to the controller at a time
and that there is room in the buffer for commands that are going to be buffered.

Once a g-code has been received it is interepreted and turned into moves which are placed into
the move buffer.

The stepper timer interrupt pops moves from the buffer whenever it's done with a move.

The move buffer is a fixed size, circular buffer of 32 bit words,
with a atomic push and a pop operations.


Each move starts with a magic word, the size of the move in words and a number which refers back
to an id assigned as each g-code command is buffered, this is to allow the front end to be told
exactly what g-code is being executed.

The commands could look something like this:

0: 05AA1000: Pause 
1: move id
2: ms

0: 05AA2aaa: Move
1: move id
2: Length in ticks

a0: x-speed
a1: x-accleration

a2: y-speed
a3: y-accleration

a4: z-speed
a5: z-accleration

a6: a-speed
a7: a-accleration

a8: Laser intensity
a9: Laser acceleration.

a10: If bit 10 of the feature field is set, then there are pixels in the command: 
p:   pixel count  
p+1: pixel speed
p+2: pixels 0..31
p+3: pixels 32..63


The aa byte is a bit field which dictates which of the words are present in the move stream,
thus the minimal move will be 3 words long (constant speed along one axis).

Notice that if a scan line consists of long blank stretches and long filled stretches,
then it's possible to compress the scanline to lines which are just one pixel.   


Each pixel is one bit.

One word is popped from the move buffer whenever the previous bunch of 32 bits has been consumed,
this allows the planner to fill the buffer earlier than if it had been forced to wait for each
move to complete, which matters if the moves are very large, as the case is with engraving moves.

Pushing into the buffer has to happen atomically so a full move can always be popped out without
having the stepper wait for the planner to finish pushing, to that end the first word pushed
in a new move is always BB05AA00, which means that the move isn't ready yet, this magic is then
overwritten as the last operation when the planner is done pusing the move.

To be able to check the magic word at the head of the queue, the consumer needs a peek operation.

To be able to fix up the magic word, once the entire package has been written, the producer needs an
interface like:
unsigned int index = startOperation(buffer, size)
push(buffer, word)
push(buffer, word)
push(buffer, word)
...
commitOperation(buffer, index, type)

The commitOperation should check that the size already written actually matches the number of words
pushed.

Another annoying problem is that the planner needs to modify the acceleration profile of the planned moves
as new moves are added, so to keep the stepper from executing moves before the planner is done modifying them
it must hold back the commitOperation call until it's done with lookahead.


== The Planner ==

The stepper routine is very simple, but the planner is horrendously complex, so I think I want to
split the part of the code transforms g-code to moves out as a separate module so it's possible
to test and debug it in isolation on the host PC and do things such as generate a nice graph over
the speed and acceleration over the entire run and accurately predict the buffer space needed to
buffer a g-code.

An interesting option would be to run the g-code engine on the host and only shuffle the moves over
to the controller, the only disadvantage to this approach is that more bandwidth is needed, but
timing-wise the constraints are nearly the same as each g-code is converted to moves immediately
and we're using USB, so that's not a problem.

Special handling would be needed to stop the machine in a controlled way if it runs out of moves,
but this is very easy to handle as it's only a matter of generating two synthetic moves, one to
decellerate from whatever speed the last move was going at and one to creep back to the stopping
point.

Moving the planner off the controller and on to the host has several advantages:
* Allows the planner to use many more resources to optimize the path, so it might give a better result
* It's easier to upgrade code on the host.
* The planner doesn't share its internal move representation with the stepper, so it's possible to
remove a lot of the data fields that are only used by the planner from the data sent to the stepper,
so the only real buffer in the system, the move buffer, will be able to contain more moves.
* It might even be possible to remove data from the move bitstream that the stepper doesn't need for
that move, like the acceleration data if the move happens at full speed or the data for an axis that
doesn't move (the z, a and pixel axises would be still for most of the moves)


To be fair, the compression trick (removal of planner-only and dead data) from the move stream could
be implemented with the planner on the controller too, but it's slightly more tricky as it would have
to happen in-place.

It might be neccesary to have a separate look-ahead buffer where the uncompressed, planner-friendly,
moves can be stored, but that raises an issue with engraving moves which have a huge amount of pixels.

Perhaps the best way around this problem is to always force a flush and immediate compression of
engraving moves, which would seem to fit the physical reality of how engraving works.


= Current sticking points =

I'll need to look more into how grbl handles modifying the moves during lookahead to design the buffer
primitives correctly.

I currently intend to fire the stepper interrupt at a fixed rate rather than do the complex
timer reprogramming and acceleration management that grbl does.

20-50 kHz, would leave 6000 to 2400 cycles per interrupt which my intuition tells me would be plenty.
I might have to code up the stepper routine first to see how many cycles it takes before deciding how
to approach this.


if (done) {
   if (moveBufferEmpty) {
      if (currentSpeed) {
         decellerateAndStopHere(); // create the two moves needed to stop the machine and return it to this location in an orderly fashion.
      } else {
	  	 return;
      }
   }

   pop move;
   done = false;
} 

if (accelTicks) { // Accelerating
   accelTicks--;
   xSpeed += xAccel;
   ySpeed += yAccel;
  ...

} else if (runTicks) { // Running full speeed.
  runTicks--;

} else if (deccelTicks) { // Deccelerating
   done = !--deccelTicks;
   xSpeed -= xDeccel;
  ...

   
}

xError += xSpeed;
if (xError & (1<<31)) {
   xError &=~ (1<<31);
   xPos += xDir;
   step(x);
}


---------------


Stick this in /etc/udev/rules.d/jtag.rules:

SUBSYSTEM=="usb", ATTR{idVendor}=="15ba", GROUP="adm"
SUBSYSTEM=="usb", ATTR{idVendor}=="03eb", GROUP="adm"

