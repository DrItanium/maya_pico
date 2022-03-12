/**
 * @file
 * Code to make it easy for a given device to act as chipset for an i960
 * @copyright
 * maya
 * Copyright (c) 2012-2022, Joshua Scoggins
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
#include "platform/os.h"
extern "C" {
#include "clips/clips.h"
}
#include <iostream>
#include <array>
#include "interface/spi.h"
#include "interface/gpio.h"
#include "ChipsetInterface.h"
extern "C" {
#include <signal.h>
}
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
static void                    CatchCtrlC(int);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

int main(int argc, char *argv[]) {
#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
    signal(SIGINT,CatchCtrlC);
#endif
    try {
        Neutron::GPIO::begin();
        Neutron::SPI::begin(0, 10 * 1024 * 1024);
    } catch (std::system_error& err) {
        std::cout << "Error starting up: " << err.what() << std::endl;
        return 1;
    }
    auto& theChipset = i960::ChipsetInterface::get();
    theChipset.setupPins();
    theChipset.putManagementEngineInReset();
    /// @todo insert calls to add extended clips functionality here
    theChipset.loadMicrocode();

    std::cout << "Keeping the i960 In Reset but the Management Engine active" << std::endl;
    theChipset.pullManagementEngineOutOfReset();
    theChipset.setupDataLines();
    std::cout << "Pulling the i960 out of reset!" << std::endl;
    theChipset.pull960OutOfReset();
    theChipset.waitForBootSignal();
    while (true) {
       theChipset.waitForTransactionStart();
       theChipset.newDataCycle();
    }

    theChipset.shutdown("Strange Termination!");
    return -1;
    // configure the pins of the raspberry pi
}



#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC || DARWIN
/***************/
/* CatchCtrlC: */
/***************/
void CatchCtrlC(int sgnl)
{
    i960::shutdown("CTRL+C Given");
}
#endif
