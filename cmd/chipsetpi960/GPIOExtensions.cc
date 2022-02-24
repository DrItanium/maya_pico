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
#include "electron/ArgumentConstructor.h"
#include "GPIOExtensions.h"
#include <gpiod.hpp>
#include <memory>
#include <map>

using GPIOChip = gpiod::chip;
using GPIOChipPtr = std::shared_ptr<GPIOChip>;
using GPIOPin = gpiod::line;
using GPIOPinPtr = std::shared_ptr<GPIOPin>;
using OpenGPIOChipTracking = std::map<std::string, GPIOChipPtr>;
using IndexToPinMapping = std::map<size_t, GPIOPinPtr>;
namespace Electron
{
    DefWrapperSymbolicName(GPIOChipPtr, "gpio-chip")
    DefWrapperSymbolicName(GPIOPinPtr, "gpio-pin")
}
namespace {
    OpenGPIOChipTracking openedChips_;
    /**
     * @brief Maps an open GPIOChip to a corresponding set of pins
     */
    std::map<std::string, IndexToPinMapping> chipToPinMapping_;
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

            if (auto search = openedChips_.find(path); search != openedChips_.end()) {
                out->externalAddressValue = theEnv.createExternalAddress<GPIOChipPtr>(search->second);
            } else {
                // emplace and then put that into the output result
                auto theChip = std::make_shared<GPIOChip>(path);
                auto [iter, _] = openedChips_.try_emplace(theChip->name(), theChip);
                out->externalAddressValue = theEnv.createExternalAddress<GPIOChipPtr>(iter->second);
                chipToPinMapping_.try_emplace(iter->second->name(), IndexToPinMapping {});
            }
        } catch(std::system_error&) {
            // by default we get a thrown exception
            out->lexemeValue = theEnv.createBool(false);
        }
    }
    void
    doGPIOName(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        out->lexemeValue = theEnv.createBool(false);
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) {
            return;
        }
        if (!theEnv.externalAddressIsOfType<GPIOChipPtr>(arg0)) {
            return;
        }
        out->lexemeValue = theEnv.createString(theEnv.fromExternalAddressAsRef<GPIOChipPtr>(arg0)->name());
    }
    void
    doGPIOLabel(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        out->lexemeValue = theEnv.createBool(false);
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) {
            return;
        }
        if (!theEnv.externalAddressIsOfType<GPIOChipPtr>(arg0)) {
            return;
        }
        out->lexemeValue = theEnv.createString(theEnv.fromExternalAddressAsRef<GPIOChipPtr>(arg0)->label());
    }
    void
    doGPIOLength(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        out->lexemeValue = theEnv.createBool(false);
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) {
            return;
        }
        if (!theEnv.externalAddressIsOfType<GPIOChipPtr>(arg0)) {
            return;
        }
        out->integerValue = theEnv.createInteger(theEnv.fromExternalAddressAsRef<GPIOChipPtr>(arg0)->num_lines());
    }
    void
    doGPIOLine(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0, arg1;
        out->lexemeValue = theEnv.createBool(false);
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) {
            return;
        }
        if (!theEnv.nextArgument(context, Electron::ArgumentBits::Integer, &arg1)) {
            return;
        }
        if (!theEnv.externalAddressIsOfType<GPIOChipPtr>(arg0)) {
            return;
        }
        auto thePtr = theEnv.fromExternalAddressAsRef<GPIOChipPtr>(arg0);
        auto theOffset = arg1.integerValue->contents;
        try {
            auto outputPtr = std::make_shared<GPIOPin>(thePtr->get_line(theOffset));
            out->externalAddressValue = theEnv.createExternalAddress<GPIOPinPtr>(outputPtr);
        } catch (std::out_of_range&) {
            // swallow the error and just return false
            out->lexemeValue = theEnv.falseSymbol();
        }
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
    template<typename T>
    bool discardSharedPtr(Electron::Environment& theEnv, std::shared_ptr<T>& theThing) noexcept {
        // free the pointer to make sure it doesn't stick around
        theThing.reset();
        return true;
    }
    template<typename T>
    bool rawDiscardSharedPtr(Electron::RawEnvironment* theEnv, void* thePtr) noexcept {
        if (auto& ref = Electron::Environment::fromRaw(theEnv); thePtr) {
            return discardSharedPtr<T>(ref, *reinterpret_cast<std::shared_ptr<T>*>(thePtr));
        } else {
            return false;
        }
    }
}
void
installGPIOExtensions(Electron::Environment& theEnv) {
    // go with the simplest registration by forcing type checks in each setup
    // perhaps we'll go to using the call operation at some point but not now
    theEnv.registerExternalAddressType<GPIOChipPtr>(nullptr,
                                                    nullptr,
                                                    nullptr);
    using ArgType = Electron::ArgumentTypes;
    using SingleArgument = Electron::SingleArgument;
    SingleArgument symbolOrString(ArgType::Symbol, ArgType::String);
    SingleArgument externalAddressOrFalse(ArgType::ExternalAddress, ArgType::Boolean);
    auto externalAddressesOnly = Electron::makeArgumentList(SingleArgument{ArgType::ExternalAddress},
                                                            SingleArgument{ArgType::ExternalAddress});
    auto returnsSymbolOrString = symbolOrString.str();
    auto returnsOptionalExternalAddress = externalAddressOrFalse.str();
    theEnv.addFunction("gpio-open",
                       returnsSymbolOrString,
                       1,1,
                       Electron::makeArgumentList(symbolOrString,
                                                  symbolOrString),
                       doGPIOOpen,
                       "doGPIOOpen");
    theEnv.addFunction("gpio-name",
                       returnsSymbolOrString,
                       1, 1,
                       externalAddressesOnly,
                       doGPIOName,
                       "doGPIOName");
    theEnv.addFunction("gpio-label",
                       returnsSymbolOrString,
                       1, 1,
                       externalAddressesOnly,
                       doGPIOLabel,
                       "doGPIOLabel");
    theEnv.addFunction("gpio-count",
                       Electron::makeReturnType(ArgType::Integer),
                       1, 1,
                       externalAddressesOnly,
                       doGPIOLength,
                       "doGPIOLength");
    theEnv.addFunction("gpio-pin",
                       returnsOptionalExternalAddress,
                       2, 2,
                       Electron::makeArgumentList(SingleArgument{ArgType::Any},
                                                  SingleArgument{ArgType::ExternalAddress},
                                                  SingleArgument{ArgType::Integer}),
                       doGPIOLine,
                       "doGPIOLine");
    // make sure we use a std::shared_ptr to be on the safe side
    theEnv.registerExternalAddressType<GPIOPinPtr>(nullptr, // cannot create lines from the ether, must come from a chip
                                                   nullptr, // the call mechanism is something that I still need to flesh out, it can be very slow
                                                   nullptr);
    auto returnsOptionalString = Electron::makeReturnType(ArgType::String, ArgType::Boolean);
    auto returnsOptionalInteger = Electron::makeReturnType(ArgType::Integer, ArgType::Boolean);
    auto returnsBoolean = Electron::makeReturnType(ArgType::Boolean);
    theEnv.addFunction("pin-name",
                       returnsOptionalString,
                       1, 1,
                       externalAddressesOnly,
                       getPinName,
                       "getPinName");
    theEnv.addFunction("pin-offset",
                       returnsOptionalInteger,
                       1, 1,
                       externalAddressesOnly,
                       getPinOffset,
                       "getPinOffset");
    theEnv.addFunction("pin-consumer",
                       returnsOptionalString,
                       1, 1,
                       externalAddressesOnly,
                       getPinConsumer,
                       "getPinConsumer");
    theEnv.addFunction("pin-direction",
                       returnsOptionalInteger,
                       1, 1,
                       externalAddressesOnly,
                       getPinDirection,
                       "getPinDirection");
    theEnv.addFunction("pin-is-used", returnsBoolean, 1, 1, externalAddressesOnly, pinIsUsed, "pinIsUsed");
    theEnv.addFunction("pin-is-open-source", returnsBoolean, 1, 1, externalAddressesOnly, pinIsOpenSource, "pinIsOpenSource");
    theEnv.addFunction("pin-is-open-drain", returnsBoolean, 1, 1, externalAddressesOnly, pinIsOpenDrain, "pinIsOpenDrain");
    theEnv.addFunction("pin-is-requested", returnsBoolean, 1, 1, externalAddressesOnly, pinIsRequested, "pinIsRequested");
    theEnv.addFunction("pin-value", returnsOptionalInteger, 1, 1, externalAddressesOnly, getPinValue, "getPinValue");
    // for now I will just be using direct access functions instead

}
