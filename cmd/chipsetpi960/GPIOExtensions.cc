/**
 * @file
 * Add gpio manipulation functionality to maya
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
#include "electron/Environment.h"
#include "GPIOExtensions.h"
#include <gpiod.hpp>
#include <memory>

using GPIOPin = gpiod::line;
using GPIOPinPtr = std::shared_ptr<GPIOPin>;
namespace Electron
{
    DefWrapperSymbolicName(GPIOPinPtr, "gpio-pin")
}
namespace {
    gpiod::chip primaryChip_;
    void
    doGPIOOpen(UDF_ARGS__) {
        // fromRaw acts as a safe way to re-encapsulate the raw environment pointer back into the correct reference
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::Lexeme, &arg0)) {
            out->lexemeValue = theEnv.falseSymbol();
            return;
        }
        std::string path(arg0.lexemeValue->contents);
        try {
            primaryChip_.open(path);
            out->lexemeValue = theEnv.createBool(static_cast<bool>(primaryChip_));
        } catch(std::system_error&) {
            // by default we get a thrown exception
            out->lexemeValue = theEnv.createBool(false);
        }
    }
    void
    doGPIOClose(UDF_ARGS__) {
        primaryChip_.reset();
    }
    void
    doGPIOActive(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        out->lexemeValue = theEnv.createBool(static_cast<bool>(primaryChip_));
    }
    void
    doGPIOName(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        out->lexemeValue = theEnv.createString(primaryChip_.name());
    }
    void
    doGPIOLabel(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        out->lexemeValue = theEnv.createString(primaryChip_.label());
    }
    void
    doGPIOLength(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        out->integerValue = theEnv.createInteger(primaryChip_.num_lines());
    }
    void
    doGPIOLine(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &arg0)) {
            out->lexemeValue = theEnv.falseSymbol();
            return;
        }
        auto theOffset = arg0.integerValue->contents;
        auto thePtr = std::make_shared<GPIOPin>(primaryChip_.get_line(theOffset));
        out->externalAddressValue = theEnv.createExternalAddress<GPIOPinPtr>(thePtr);
    }
    void
    getPinName(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        out->lexemeValue = theEnv.falseSymbol();
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) {
            return;
        }
        if (!theEnv.externalAddressIsOfType<GPIOPinPtr>(arg0)) {
            return;
        }
        // okay so we have the right type, get it out of there and work with it
        auto thePin = theEnv.fromExternalAddressAsRef<GPIOPinPtr>(arg0);
        out->lexemeValue = theEnv.createString(thePin->name());
    }

    void
    getPinConsumer(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        out->lexemeValue = theEnv.falseSymbol();
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) {
            return;
        }
        if (!theEnv.externalAddressIsOfType<GPIOPinPtr>(arg0)) {
            return;
        }
        // okay so we have the right type, get it out of there and work with it
        auto thePin = theEnv.fromExternalAddressAsRef<GPIOPinPtr>(arg0);
        out->lexemeValue = theEnv.createString(thePin->consumer());
    }

#define DefIntegerFunction(name, operation) \
    void \
    name (UDF_ARGS__) { \
        auto& theEnv = Electron::Environment::fromRaw(env); \
        UDFValue arg0; \
        out->lexemeValue = theEnv.falseSymbol(); \
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) { \
            return; \
        } \
        if (!theEnv.externalAddressIsOfType<GPIOPinPtr>(arg0)) { \
            return; \
        } \
        auto thePin = theEnv.fromExternalAddressAsRef<GPIOPinPtr>(arg0); \
        out->integerValue = theEnv.createInteger(thePin-> operation ()); \
    }
    DefIntegerFunction(getPinDirection, direction);
    DefIntegerFunction(getPinOffset, offset);
    DefIntegerFunction(getPinActiveState, active_state);
    DefIntegerFunction(getPinValue, get_value);
#undef DefIntegerFunction
#define DefBoolFunction(name, operation) \
    void \
    name (UDF_ARGS__) { \
        auto& theEnv = Electron::Environment::fromRaw(env); \
        UDFValue arg0; \
        out->lexemeValue = theEnv.falseSymbol(); \
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) { \
            return; \
        } \
        if (!theEnv.externalAddressIsOfType<GPIOPinPtr>(arg0)) { \
            return; \
        } \
        auto thePin = theEnv.fromExternalAddressAsRef<GPIOPinPtr>(arg0); \
        out->lexemeValue = theEnv.createBool(thePin-> operation ()); \
    }
    DefBoolFunction(pinIsUsed, is_used);
    DefBoolFunction(pinIsOpenDrain, is_open_drain);
    DefBoolFunction(pinIsOpenSource, is_open_source);
    DefBoolFunction(pinIsRequested, is_requested);
#undef DefBoolFunction
}
void
installGPIOExtensions(Electron::Environment& theEnv) {
    /// @todo add support for gpiod::chip as an external address type
    theEnv.addFunction("gpio-open", "b", 1, 1, "sy;sy", doGPIOOpen, "doGPIOOpen");
    theEnv.addFunction("gpio-close", "v", 0, 0, "", doGPIOClose, "doGPIOClose");
    theEnv.addFunction("gpio-active", "b", 0, 0, "", doGPIOActive, "doGPIOActive");
    theEnv.addFunction("gpio-name", "sy", 0, 0, "", doGPIOName, "doGPIOName");
    theEnv.addFunction("gpio-label", "sy", 0, 0, "", doGPIOLabel, "doGPIOLabel");
    theEnv.addFunction("gpio-count", "l", 0, 0, "", doGPIOLength, "doGPIOLength");
    theEnv.addFunction("gpio-pin", "eb", 1, 1, "l", doGPIOLine, "doGPIOLine");
    // make sure we use a std::shared_ptr to be on the safe side
    theEnv.registerExternalAddressType<GPIOPinPtr>(nullptr, // cannot create lines from the ether, must come from a chip
                                                   nullptr, // the call mechanism is something that I still need to flesh out, it can be very slow
                                                   nullptr);
    theEnv.addFunction("pin-name", "sb", 1, 1, "e", getPinName, "getPinName");
    theEnv.addFunction("pin-offset", "lb", 1, 1, "e", getPinOffset, "getPinOffset");
    theEnv.addFunction("pin-consumer", "sb", 1, 1, "e", getPinConsumer, "getPinConsumer");
    theEnv.addFunction("pin-direction", "lb", 1, 1, "e", getPinDirection, "getPinDirection");
    theEnv.addFunction("pin-is-used", "b", 1, 1, "e", pinIsUsed, "pinIsUsed");
    theEnv.addFunction("pin-is-open-source", "b", 1, 1, "e", pinIsOpenSource, "pinIsOpenSource");
    theEnv.addFunction("pin-is-open-drain", "b", 1, 1, "e", pinIsOpenDrain, "pinIsOpenDrain");
    theEnv.addFunction("pin-is-requested", "b", 1, 1, "e", pinIsRequested, "pinIsRequested");
    theEnv.addFunction("pin-value", "lb", 1, 1, "e", getPinValue, "getPinValue");
    // for now I will just be using direct access functions instead

}
