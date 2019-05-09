# snake-gameboy

### To Compile:
  Using [RGBDS — Rednex Game Boy Development System](https://rednex.github.io/rgbds/)
  ```
  rgbasm -o main.o main.asm
  rgblink -o main.gb main.o
  rgbfix -v -p 0 main.gb
  ```
  
### To Run:
  In theory, this should run on any emulator - even an *actual gameboy*.  
  For Windows, I recommend [BGB](http://bgb.bircd.org/).  
  
### Sources:
All of my resources came from the following directory:  
[Awesome Game Boy Development](https://gbdev.github.io/list.html)  
  
Specifically the following:  
- [ISSOtm's — GB Programming](https://eldred.fr/gb-asm-tutorial) - Step by step instructions to get a simple Hello World program running
- [Duo's GameBoy ASMSchool](http://gameboy.mongenel.com/asmschool.html) - Great documentation of instruction and register semantics
- [hardware.inc](https://github.com/gbdev/hardware.inc) - Hardware definitions for specific IO/RAM/VRAM Addresses to make ASM code more readable

### Implementation Details:
#### Working with the Display:  
The Gameboy has a 128 * 128 byte section of VRAM that represents the background map. There is another region that stores 16 byte tiles (8 bytes for the pixel positions and 8 bytes for the shading). To display an 8x8 pixel tile on the 14x13 tile display (the tile display can scroll to accommodate for all 32 * 32 tiles), you simply specify the tileset to use then write the offset of the tile in the map you would like to display into the VRAM address of the desired coordinate.  
  
To update the display, developers typically wait until [VBlank](https://eldred.fr/gb-asm-tutorial/displaying.html) and then have a few cycles to modify VRAM. Instead, I opted to turn off the LCD, perform all of my VRAM operations, then turn it back on. The side effect of that is that the screen flashes every update.  

#### Timer Implementation:
I set the internal timer interrupt to trigger at 255/4000Hz which equates to about 0.0638 seconds per interrupt. Since that causes the game to flash too much, I settled on actually drawing a new frame every other interrupt. 

#### "Randomly" Generating Food Positions:
To get seemingly random bytes, I take advantage of the timer I/O register and use that as my source of entropy. Since the game has exponentially possible states, each run will appear distinct since the timer will theoretically be read after a different number of CPU cycles every time.
