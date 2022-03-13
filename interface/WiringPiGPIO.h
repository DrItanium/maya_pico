/**
 * @file
 * Thin wrapper over wiring pi
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

#ifndef MAYA_WIRINGPIGPIO_H
#define MAYA_WIRINGPIGPIO_H
#include "platform/config.h"
#ifdef HAVE_WIRING_PI_H
#include <wiringPi.h>
namespace Neutron::GPIO::WiringPi::Implementation {
    inline void pinMode(int targetPin, PinMode direction) {
        switch (direction) {
            case PinMode::Input:
                ::pinMode(targetPin, INPUT);
                ::pullUpDnControl(targetPin, PUD_OFF);
                break;
            case PinMode::Output:
                ::pinMode(targetPin, OUTPUT);
                ::pullUpDnControl(targetPin, PUD_OFF);
                break;
            case PinMode::InputPullup:
                ::pinMode(targetPin, INPUT);
                ::pullUpDnControl(targetPin, PUD_UP);
                break;
            case PinMode::InputPulldown:
                ::pinMode(targetPin, INPUT);
                ::pullUpDnControl(targetPin, PUD_UP);
                break;
            default:
                break;
        }
    }
    bool begin() {
        wiringPiSetup();
        return true;
    }
    PinValue digitalRead(int targetPin) { return static_cast<PinValue>(::digitalRead(targetPin)); }
    void digitalWrite(int targetPin, PinValue value) { ::digitalWrite(targetPin, static_cast<int>(value)); }
    bool attachInterrupt(int targetPin, InterruptMode mode, ISRFunction function) {
        switch (mode) {
            case InterruptMode::Both:
                return wiringPiISR(targetPin, INT_EDGE_BOTH, function);
            case InterruptMode::Falling:
                return wiringPiISR(targetPin, INT_EDGE_FALLING, function);
            case InterruptMode::Rising:
                return wiringPiISR(targetPin, INT_EDGE_RISING, function);
            default:
                return wiringPiISR(targetPin, INT_EDGE_SETUP, function);
        }
    }
    void delay(unsigned int amount) { ::delay(amount); }
    void delayMicroseconds(unsigned int amount) { ::delayMicroseconds(amount); }
} // end namesapce Neutron::GPIO
#endif

#endif //MAYA_WIRINGPIGPIO_H
