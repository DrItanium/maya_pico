This directory contains a complete reimplementation of the SxChipset firmware for a raspberry pi 4B+ running Raspberry Pi OS 64-bit.
Here are the important points:

1. The i960 is controlled through a Management Engine 
2. The address and data lines are controlled through SPI (the raspberry pi needs at least 56 free GPIOSs do that...)
3. The raspberry pi's peripherals are mapped into the i960's memory space.

One of the cores of the raspberry pi is acting as chipset handler. The other cores are free to be used for 
other purposes. This core will _never_ sleep to always be ready to respond to requests from the i960.

The linkage between different peripherals and the chipset handler code is actually serviced by the CLIPS code. 
In the code you may see references to "microcode". This is the name I give clips code (compiled or not). In this
case it is very much the embodiment of target specific differences that should not be compiled into the chipset binary
itself. In this case, it will be the description of the memory map itself. This makes updating the map far faster while
making it easy to understand.