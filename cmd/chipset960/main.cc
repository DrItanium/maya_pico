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
#include "electron/Environment.h"
#include "interface/spi.h"
#include "interface/gpio.h"
#if   UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
extern "C" {
    #include <signal.h>
}
#endif
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
   static void                    CatchCtrlC(int);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

Electron::Environment mainEnv;
Neutron::GPIO::Chip primaryChip_;
constexpr auto boardToBCMPinout(uint8_t value) noexcept {
    switch (value) {
        case 7:
        case 29:
            case
        default:
            return value;
    }
}
enum class Pinout {
    BootSuccessful = 7,
    Ready = 29,
    WR = 31,
    BE0 = 32,
    BE1 = 33,
    InTransaction = 8,
    DoCycle = 10,
    Blast = 36,
    MeReset = 11,
    WaitBoot960 = 12,
    IoExpander_Int0 = 35,
    IoExpander_Int1 = 38,
    IoExpander_Int2 = 40,
    IoExpander_Int3 = 15,
    IoExpander_Int4 = 16,
    IoExpander_Int5 = 18,
    IoExpander_Int6 = 22,
    IoExpander_Int7 = 37,
};

int main(int argc, char *argv[]) {
#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
    signal(SIGINT,CatchCtrlC);
#endif
    try {
        // hardcode this for the raspberry pi for now
        primaryChip_.open("/dev/gpiochip0");
    } catch (std::system_error& err) {
        std::cout << "ERROR OPENING /dev/gpiochip0: " << err.what() << std::endl;
        return 1;
    }

    RerouteStdin(mainEnv, argc, argv);
    CommandLoop(mainEnv);

    // unlike normal CLIPS, the environment will automatically clean itself up

    return -1;
}

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC || DARWIN
/***************/
/* CatchCtrlC: */
/***************/
void CatchCtrlC(int sgnl)
{
    SetHaltExecution(mainEnv,true);
    CloseAllBatchSources(mainEnv);
    signal(SIGINT,CatchCtrlC);
}
#endif
