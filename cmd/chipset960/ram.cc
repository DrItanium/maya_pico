/**
 * @file
 * Ram interface
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
#include "ram.h"
#include "ChipsetInterface.h"

namespace i960 {
    namespace {
        union SplitWord16 {
            explicit constexpr SplitWord16(uint16_t value = 0) noexcept : value_(value) { }
            constexpr SplitWord16(uint8_t lower, uint8_t upper) noexcept : bytes_{lower, upper} { }
            [[nodiscard]] constexpr auto getValue() const noexcept { return value_; }
            [[nodiscard]] constexpr auto getLowerHalf() const noexcept { return bytes_[0]; }
            [[nodiscard]] constexpr auto getUpperHalf() const noexcept { return bytes_[1]; }
            void setValue(uint16_t value) noexcept { value_ = value; }
            void setLowerHalf(uint8_t value) noexcept { bytes_[0] = value; }
            void setUpperHalf(uint8_t value) noexcept { bytes_[1] = value; }
            uint16_t value_;
            uint8_t bytes_[sizeof(uint16_t)];
        };
        static constexpr size_t RAMSize = 256 * 1024 * 1024;
        static constexpr size_t WordSize = RAMSize / sizeof(SplitWord16);
        SplitWord16 ram_[WordSize];
        void
        setupRAM() noexcept {
            static bool initialized_ = false;
            if (!initialized_) {
                initialized_ = true;
                for (size_t i = 0; i < WordSize; ++i) {
                    ram_[i].setValue(0);
                }
            }
        }
        constexpr auto getWordAddress(uint32_t byteAddress) noexcept { return byteAddress >> 1; }
        uint8_t
        loadByte(uint32_t byteAddress) noexcept {
            auto lowest = byteAddress & 1;
            if (auto& theWord = ram_[getWordAddress(byteAddress)]; lowest) {
                return theWord.getUpperHalf();
            } else {
                return theWord.getLowerHalf();
            }
        }
        uint16_t
        loadWord(uint32_t byteAddress) noexcept {
            return ram_[getWordAddress(byteAddress)].getValue();
        }
        void
        storeByte(uint32_t byteAddress, uint8_t value) noexcept {
            auto lowest = byteAddress & 1;
            if (auto& theWord = ram_[getWordAddress(byteAddress)]; lowest) {
                theWord.setUpperHalf(value);
            } else {
                theWord.setLowerHalf(value);
            }
        }
        void
        storeWord(uint32_t byteAddress, uint16_t value) noexcept {
            ram_[getWordAddress(byteAddress)].setValue(value);
        }
    }
    void
    installRAMExtensions(Electron::Environment& theEnv) noexcept {
        setupRAM();
    }
}
