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
#include <type_traits>
#include "interface/spi.h"
#include "interface/gpio.h"
#include "ram.h"
#include "ChipsetInterface.h"
extern "C" {
#include <signal.h>
}

void
loadStoreStyle(Electron::FunctionBuilder* builder) noexcept {
    auto& theChipset = i960::ChipsetInterface::get();
    // do a sanity check ahead of time to stop if we run into illegal load store styles
    // otherwise just install it as a number. We want to eliminate the overhead from doing integer -> symbol and symbol -> integer
    // since this is going into the microcode, it is okay to need to update the microcode if we change the communication protocol
    //
    // Ideally, the microcode is designed in such a way to make such changes only require one area to be updated. The rest of the
    // microcode should be unaware of the change
    switch (auto theStyle = theChipset.getStyle(); theStyle) {
        case i960::LoadStoreStyle::Full16:
        case i960::LoadStoreStyle::Lower8:
        case i960::LoadStoreStyle::Upper8:
            builder->add(static_cast<std::underlying_type_t<decltype(theStyle)>>(theStyle));
            break;
        default:
            theChipset.shutdown("Illegal load store style!");
            break;
    }
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
        if (!Neutron::SPI::begin(0, 10 * 1000 * 1000)) {
            i960::shutdown("Could not open SPI Bus!");
        }
    } catch (std::system_error& err) {
        i960::shutdown(err.what());
        return -1;
    }
    auto& theChipset = i960::ChipsetInterface::get();
    theChipset.setupPins();
    theChipset.putManagementEngineInReset();
    theChipset.setupDataLines();
    std::cout << "Keeping the i960 In Reset but the Management Engine active" << std::endl;
    theChipset.pullManagementEngineOutOfReset();
    /// @todo insert calls to add extended clips functionality here
    i960::installRAMExtensions(theChipset);
    theChipset.loadMicrocode();
    std::cout << "Pulling the i960 out of reset!" << std::endl;
    theChipset.pull960OutOfReset();
    theChipset.waitForBootSignal();
    std::cout << "Successfully booted the i960!" << std::endl;
    /// @todo expand to actually be a proper chipset handler
    while (true) {
       theChipset.waitForTransactionStart();
       if (auto baseAddress = theChipset.getAddress(); theChipset.isReadOperation()) {
           theChipset.setupDataLinesForRead();
           while (true) {
               theChipset.waitForCycleUnlock();
               theChipset.setDataLines(theChipset.call<uint16_t>("perform-read", baseAddress));
               if (theChipset.signalCPU()) {
                   break;
               }
               baseAddress += 2;
           }
       } else {
           theChipset.setupDataLinesForWrite();
           // write operation
           while (true) {
               theChipset.waitForCycleUnlock();
               Electron::Value returnNothing;
               theChipset.call("perform-write",
                               &returnNothing,
                               baseAddress,
                               theChipset.getDataLines(),
                               [](auto* builder) { loadStoreStyle(builder); });
               if (theChipset.signalCPU()) {
                   break;
               }
               baseAddress += 2;
           }

       }
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
