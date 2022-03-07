/**
 * @file
 * Thin wrapper over wiring pi spi features
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
//
// Created by jwscoggins on 3/6/22.
//

#ifndef MAYA_WIRINGPISPI_H
#define MAYA_WIRINGPISPI_H
#include "platform/config.h"
#ifdef HAVE_WIRING_PI_H
#include <cstdint>
#include <wiringPi.h>
#include <wiringPiSPI.h>
namespace Neutron::SPI {
    inline bool beginTransaction(int channel, int speed, int mode) noexcept { return wiringPiSPISetupMode(channel, speed, mode); }
    inline void endTransaction() noexcept { }
    inline bool begin(int channel, int speed) noexcept { return wiringPiSPISetup(channel, speed); }
    inline int transfer(int channel, uint8_t* data, int len) noexcept { return wiringPiSPIDataRW(channel, data, len); }
}
#endif
#endif //MAYA_WIRINGPISPI_H
