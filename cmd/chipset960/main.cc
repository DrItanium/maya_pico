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
#include "electron/Environment.h"
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

Electron::Environment mainEnv;
union MemoryCell {
    uint16_t word;
    uint8_t bytes[2];
};
constexpr uint32_t MemorySize = 64 * 1024 * 1024;
constexpr auto NumberOfCells = MemorySize / sizeof(MemoryCell);
Electron::SingleArgument returnsVoid{Electron::ArgumentTypes::Void};
Electron::SingleArgument returnsInteger{Electron::ArgumentTypes::Integer};
std::unique_ptr<MemoryCell[]> ram;
void doSetWord(UDF_ARGS__) noexcept;
void doSetUpper8(UDF_ARGS__) noexcept;
void doSetLower8(UDF_ARGS__) noexcept;
void doGetWord(UDF_ARGS__) noexcept;
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

    // configure the pins of the raspberry pi
    configurePinBlock(Ready,
                      BootSuccessful,
                      WR,
                      BE0,
                      BE1,
                      InTransaction,
                      DoCycle,
                      Blast,
                      ManagementEngineReset,
                      WaitBoot960,
                      IOEXP_INT0,
                      IOEXP_INT1,
                      IOEXP_INT2,
                      IOEXP_INT3,
                      IOEXP_INT4,
                      IOEXP_INT5,
                      IOEXP_INT6,
                      IOEXP_INT7);
    std::cout << "Pulling Management Engine into Reset and starting configuration" << std::endl;
    ManagementEngineReset.assertPin();
    WaitBoot960.assertPin();
    mainEnv.addFunction("ram:set-word",
                        returnsVoid.str(),
                        2, 2,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                   Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                   Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                        doSetWord,
                        "doSetWord");
    mainEnv.addFunction("ram:set-upper8",
                        returnsVoid.str(),
                        2, 2,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                   Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                   Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                        doSetUpper8,
                        "doSetUpper8");
    mainEnv.addFunction("ram:set-lower8",
                        returnsVoid.str(),
                        2, 2,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                   Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                   Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                        doSetLower8,
                        "doSetLower8");
    mainEnv.addFunction("ram:get-word",
                        returnsInteger.str(),
                        1, 1,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                   Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                        doGetWord,
                        "doGetWord");
    ram = std::make_unique<MemoryCell[]>(NumberOfCells);
    for (uint32_t i = 0; i < NumberOfCells; ++i) {
        ram[i].word = 0;
    }
    /// @todo introduce a delay?
    ManagementEngineReset.deassertPin();
    std::cout << "Keeping the i960 In Reset but the Management Engine active" << std::endl;
    systemSetup();
    setupDataLinesForRead();
    WaitBoot960.deassertPin();
    waitForBootSignal();
    std::cout << "i960 Successfully Booted!" << std::endl;
    while (true) {
        waitForTransactionStart();
        newDataCycle();
    }
    //RerouteStdin(mainEnv, argc, argv);
    //CommandLoop(mainEnv);

    // unlike normal CLIPS, the environment will automatically clean itself up

    return -1;
}

void
digitalWrite(Pinout pin, PinValue value) {
    Neutron::GPIO::digitalWrite(static_cast<int>(pin), value) ;

}
PinValue
digitalRead(Pinout pin) {
    return Neutron::GPIO::digitalRead(static_cast<int>(pin));
}
void
pinMode(Pinout pin, PinDirection direction) noexcept {
    Neutron::GPIO::pinMode(static_cast<int>(pin), direction);
}


#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC || DARWIN
/***************/
/* CatchCtrlC: */
/***************/
void CatchCtrlC(int sgnl)
{
    shutdown("CTRL+C Given");
}
#endif
void
shutdown(const std::string& str) noexcept {
    SetHaltExecution(mainEnv,true);
    CloseAllBatchSources(mainEnv);
    ManagementEngineReset.assertPin();
    WaitBoot960.assertPin();
    ManagementEngineReset.deassertPin();
    // exit at this point
    exit(1);
}
void
doSetWord(UDF_ARGS__) noexcept {

}
void
doSetLower8(UDF_ARGS__) noexcept {

}
void
doSetUpper8(UDF_ARGS__) noexcept {

}

void
doGetWord(UDF_ARGS__) noexcept {

}
