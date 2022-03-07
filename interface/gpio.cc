/**
 * @file
 * Generic GPIO interface
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
#include "platform/config.h"
#include "interface/gpio.h"
#include "interface/GPIODCxx.h"
namespace Neutron::GPIO
{
    void pinMode(int targetPin, PinMode direction) {
#ifdef HAVE_WIRING_PI_H
        WiringPi::Implementation::pinMode(targetPin, direction);
#elif defined(HAVE_GPIOD_HPP)
        GPIOD::Implementation::pinMode(targetPin, direction);
#endif

    }
    PinValue digitalRead(int targetPin) {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::digitalRead(targetPin);
#elif defined(HAVE_GPIOD_HPP)
        return GPIOD::Implementation::digitalRead(targetPin);
#else
        return PinValue::Low;
#endif

    }
    void digitalWrite(int targetPin, PinValue value) {
#ifdef HAVE_WIRING_PI_H
        WiringPi::Implementation::digitalWrite(targetPin, value);
#elif defined(HAVE_GPIOD_HPP)
        GPIOD::Implementation::digitalWrite(targetPin, value);
#endif
    }
    bool attachInterrupt(int targetPin, InterruptMode mode, ISRFunction function) {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::attachInterrupt(targetPin, mode, function);
#elif defined(HAVE_GPIOD_HPP)
        return GPIOD::Implementation::attachInterrupt(targetPin, mode, function);
#else
        return false;
#endif
    }
    void
    begin() {
#ifdef HAVE_WIRING_PI_H
        WiringPi::Implementation::begin();
#elif defined(HAVE_GPIOD_HPP)
        GPIOD::Implementation::begin();
#endif

    }
}