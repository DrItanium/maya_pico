This directory contains a complete reimplementation of the SxChipset firmware for a raspberry pi 4B+ running Raspberry Pi OS 64-bit.
Here are the important points:

1. The i960 is controlled through a Management Engine 
2. The address and data lines are controlled through SPI (the raspberry pi needs at least 56 free GPIOSs do that...)
3. The raspberry pi's peripherals are mapped into the i960's memory space.