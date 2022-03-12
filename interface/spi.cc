/**
 * @file
 * Cross-platform SPI implementation
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
#include "platform/config.h"
#include "interface/spi.h"
#include "LinuxSPIDev.h"
#include "WiringPiSPI.h"

namespace Neutron::SPI {
    bool
    begin(int channel, int speed) {
#ifdef HAVE_WIRING_PI_H
#elif defined(HAVE_LINUX_SPIDEV_H)
#else
        return false;
#endif
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::begin(channel, speed);
#elif defined(HAVE_LINUX_SPIDEV_H)
        return SPIDEV::Implementation::begin(channel, speed);
#else
        return false;
#endif
    }
    bool
    beginTransaction(int channel, int speed, int mode) {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::beginTransaction(channel, speed, mode);
#elif defined(HAVE_LINUX_SPIDEV_H)
        return SPIDEV::Implementation::beginTransaction(channel, speed, mode);
#else
        return false;
#endif

    }
    bool
    endTransaction(int channel) {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::endTransaction(channel);
#elif defined(HAVE_LINUX_SPIDEV_H)
        return SPIDEV::Implementation::endTransaction(channel);
#else
        return false;
#endif

    }
    void
    transfer(int channel, char* data, int count) {
#ifdef HAVE_WIRING_PI_H
        WiringPi::Implementation::transfer(channel, data, count);
#elif defined(HAVE_LINUX_SPIDEV_H)
        SPIDEV::Implementation::transfer(channel, data, count);
#endif
    }
    int
    mode0() noexcept {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::mode0();
#elif defined(HAVE_LINUX_SPIDEV_H)
        return SPIDEV::Implementation::mode0();
#else
        return 0;
#endif
    }
    int
    mode1() noexcept {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::mode1();
#elif defined(HAVE_LINUX_SPIDEV_H)
        return SPIDEV::Implementation::mode1();
#else
        return 1;
#endif
    }
    int
    mode2() noexcept {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::mode2();
#elif defined(HAVE_LINUX_SPIDEV_H)
        return SPIDEV::Implementation::mode2();
#else
        return 2;
#endif
    }
    int
    mode3() noexcept {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::mode3();
#elif defined(HAVE_LINUX_SPIDEV_H)
        return SPIDEV::Implementation::mode3();
#else
        return 3;
#endif
    }
} // end namespace Neutron::SPI