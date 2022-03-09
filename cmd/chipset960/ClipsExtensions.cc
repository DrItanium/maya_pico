/**
 * @file
 * Implementation of CLIPS interaction routines
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
// Created by jwscoggins on 3/8/22.
//
#include "ChipsetInterface.h"
namespace i960 {
    namespace {
        Electron::SingleArgument returnsVoid{Electron::ArgumentTypes::Void};
        Electron::SingleArgument returnsInteger{Electron::ArgumentTypes::Integer};
    }
    void
    ChipsetInterface::installProcessorExtensions() noexcept {
        if (!extensionsInstalled_) {
            extensionsInstalled_ = true;
            addFunction("ram:set-word",
                               returnsVoid.str(),
                               2, 2,
                               Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                          Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                          Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                               doSetWord,
                               "doSetWord");
            addFunction("ram:set-upper8",
                               returnsVoid.str(),
                               2, 2,
                               Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                          Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                          Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                               doSetUpper8,
                               "doSetUpper8");
            addFunction("ram:set-lower8",
                               returnsVoid.str(),
                               2, 2,
                               Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                          Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                          Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                               doSetLower8,
                               "doSetLower8");
            addFunction("ram:get-word",
                               returnsInteger.str(),
                               1, 1,
                               Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer},
                                                          Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                               doGetWord,
                               "doGetWord");
        }
    }
        void
        ChipsetInterface::doSetWord(UDF_ARGS__) noexcept {

        }
        void
        ChipsetInterface::doSetLower8(UDF_ARGS__) noexcept {

        }
        void
        ChipsetInterface::doSetUpper8(UDF_ARGS__) noexcept {

        }

        void
        ChipsetInterface::doGetWord(UDF_ARGS__) noexcept {

        }
}