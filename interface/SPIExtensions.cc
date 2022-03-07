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
#include "interface/spi.h"
#include "electron/MultifieldBuilder.h"
#include <vector>

namespace Neutron::SPI {
    namespace {
        void
        doSPIBegin(UDF_ARGS__) {
            auto& theEnv = Electron::Environment::fromRaw(env);
            out->lexemeValue = theEnv.falseSymbol();
            UDFValue arg0, arg1, arg2;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &arg0)) {
                return;
            }
            if (!theEnv.nextArgument(context, Electron::ArgumentBits::Integer, &arg1)) {
                return;
            }
            if (!theEnv.nextArgument(context, Electron::ArgumentBits::Integer, &arg2)) {
                return;
            }
            auto theChannel = static_cast<int>(arg0.integerValue->contents);
            auto theSpeed = static_cast<int>(arg1.integerValue->contents);
            auto theMode = static_cast<int>(arg2.integerValue->contents);
            out->lexemeValue = theEnv.createBool(Neutron::SPI::beginTransaction(theChannel, theSpeed, theMode));
        }
        void
        doSPITransfer(UDF_ARGS__) {
            auto& theEnv = Electron::Environment::fromRaw(env);
            out->lexemeValue = theEnv.falseSymbol();
            UDFValue arg0, arg1, arg2;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &arg0)) {
                return;
            }
            if (!theEnv.nextArgument(context, Electron::ArgumentBits::Multifield, &arg1)) {
                return;
            }
            if (!theEnv.nextArgument(context, Electron::ArgumentBits::Integer, &arg2)) {
                return;
            }
            auto theDevice = static_cast<int>(arg0.integerValue->contents);
            auto theMultifield = arg1.multifieldValue->contents;
            auto theCount = static_cast<int>(arg2.integerValue->contents);
            auto end = arg1.begin + arg1.range;
            std::vector<char> multifieldUnpack;
            for (size_t i = arg1.begin; i < end; ++i) {
                auto& currentValue = theMultifield[i];
                if (Electron::isInteger(currentValue)) {
                    // force convert to a byte if it is a number
                    multifieldUnpack.emplace_back(static_cast<byte>(currentValue.integerValue->contents));
                }
            }
            std::unique_ptr<char[]> receiveBuffer = std::make_unique<char[]>(multifieldUnpack.size());
            // perform the transfer itself
            Neutron::SPI::transfer(theDevice, multifieldUnpack.data(), theCount);
            Electron::MultifieldBuilder mb(theEnv);
            for (size_t i = 0;i < multifieldUnpack.size(); ++i) {
                mb.append(static_cast<int64_t>(receiveBuffer[i]));
            }
            out->multifieldValue = mb.create();
        }
    }
    void
    installExtensions(Electron::Environment& theEnv) noexcept {
        /// @todo implement
        using ArgType = Electron::ArgumentTypes;
        theEnv.addFunction("spi-open",
                           Electron::makeReturnType(ArgType::Boolean),
                           3, 3,
                           Electron::makeArgumentList(Electron::SingleArgument{ArgType::Integer},
                                                      Electron::SingleArgument{ArgType::Integer},
                                                      Electron::SingleArgument{ArgType::Integer},
                                                      Electron::SingleArgument{ArgType::Integer}),
                           doSPIBegin,
                           "doSPIBegin");

        theEnv.addFunction("spi-transfer",
                           Electron::makeReturnType(ArgType::Multifield),
                           3, 3,
                           Electron::makeArgumentList(
                                   Electron::SingleArgument{ArgType::Integer},
                                   Electron::SingleArgument{ArgType::Integer},
                                   Electron::SingleArgument{ArgType::Multifield},
                                   Electron::SingleArgument{ArgType::Integer}),
                           doSPITransfer,
                           "doSPITransfer");
    }
} // end namespace Neutron::SPI
