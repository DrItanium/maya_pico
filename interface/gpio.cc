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
#include <boost/algorithm/string/case_conv.hpp>
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
    bool
    begin() {
#ifdef HAVE_WIRING_PI_H
        return WiringPi::Implementation::begin();
#elif defined(HAVE_GPIOD_HPP)
        return GPIOD::Implementation::begin();
#endif

    }
    void
    doDigitalRead(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        out->lexemeValue = theEnv.falseSymbol();
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &arg0)) {
            return;
        }
        out->integerValue = theEnv.createInteger(static_cast<int>(digitalRead(static_cast<int>(arg0.integerValue->contents))));
    }
    void
    doDigitalWrite(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0, arg1;
        out->lexemeValue = theEnv.falseSymbol();
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &arg0)) {
            return;
        }
        if (!theEnv.nextArgument(context, Electron::ArgumentBits::Integer, &arg1)) {
            return;
        }
        auto thePin = static_cast<int>(arg0.integerValue->contents);
        auto theAction = PinMode::None;
        auto theValue = arg1.integerValue->contents != 0 ? PinValue::High : PinValue::Low;
        digitalWrite(thePin, theValue);
        out->lexemeValue = theEnv.trueSymbol();
    }
    void
    doPinMode(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0, arg1;
        out->lexemeValue = theEnv.falseSymbol();
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &arg0)) {
            return;
        }
        if (!theEnv.nextArgument(context, Electron::ArgumentBits::Lexeme, &arg1)) {
            return;
        }
        auto thePin = static_cast<int>(arg0.integerValue->contents);
        auto theAction = PinMode::None;
        if (std::string locaseValue(boost::algorithm::to_lower_copy(std::string(arg1.lexemeValue->contents))); locaseValue == "input") {
            theAction = PinMode::Input;
        } else if (locaseValue == "output") {
            theAction = PinMode::Output;
        } else if (locaseValue == "input-pullup") {
            theAction = PinMode::InputPullup;
        } else if (locaseValue == "input-pulldown") {
            theAction = PinMode::InputPulldown;
        }
        pinMode(thePin, theAction);
        out->lexemeValue = theEnv.createBool(theAction != PinMode::None);
    }
    void
    installExtensions(Electron::Environment& theEnv) {
        theEnv.addFunction("digital-read",
                           Electron::makeReturnType(Electron::ArgumentTypes::Boolean, Electron::ArgumentTypes::Integer),
                           1, 1,
                           Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                           doDigitalRead,
                           "doDigitalRead");

        theEnv.addFunction("digital-write",
                           Electron::returnsNothing.str(),
                           2, 2,
                           Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                      Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                      Electron::SingleArgument{Electron::ArgumentTypes::Boolean}),
                           doDigitalWrite,
                           "doDigitalWrite");
        theEnv.addFunction("pin-mode",
                           Electron::makeReturnType(Electron::ArgumentTypes::Boolean),
                           2, 2,
                           Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                      Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                      Electron::SingleArgument{Electron::ArgumentTypes::Symbol,
                                                                               Electron::ArgumentTypes::String}),
                           doPinMode,
                           "doPinMode");
    }
}