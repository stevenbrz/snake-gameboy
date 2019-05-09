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
