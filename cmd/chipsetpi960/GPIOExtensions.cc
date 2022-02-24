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
namespace {
    gpiod::chip primaryChip_;
    void
    doGPIOOpen(UDF_ARGS__) {
        auto& theEnv = Electron::Environment::fromRaw(env);
        UDFValue arg0;
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::Lexeme, &arg0)) {
            out->lexemeValue = theEnv.falseSymbol();
            return;
        }
        std::string path(arg0.lexemeValue->contents);
        primaryChip_.open(path);
        out->lexemeValue = theEnv.createBool(static_cast<bool>(primaryChip_));
    }
    void
    doGPIOClose(UDF_ARGS__) {
        primaryChip_.reset();
    }
}
void
installGPIOExtensions(Electron::Environment& theEnv) {
    theEnv.addFunction("open-gpio", "b", 1, 1, "sy;sy", doGPIOOpen, "doGPIOOpen");
    theEnv.addFunction("close-gpio", "v", 0, 0, "", doGPIOClose, "doGPIOClose");
}
