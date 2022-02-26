/**
 * @file
 * Add SPI manipulation functionality
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
#include "electron/MultifieldBuilder.h"
#include <map>
#include <string>
#include <sys/ioctl.h>
#include <linux/types.h>
#include <linux/spi/spidev.h>
#include <memory>
#include <optional>
#include <vector>
extern "C" {
#include <sys/stat.h>
#include <fcntl.h>
}

#include "SPIExtensions.h"
class SPIDevice {
public:
    using Self = SPIDevice;
    using Ptr = std::shared_ptr<Self>;
    enum class Mode {
        Zero = SPI_MODE_0,
        One = SPI_MODE_1,
        Two = SPI_MODE_2,
        Three = SPI_MODE_3,
    };
    enum class BitDirection : uint8_t {
        MSBFirst = 0,
        LSBFirst,
    };
    explicit SPIDevice(const std::string& path) : path_(path), fd_(::open(path.c_str(), O_RDWR)) {
    }
    ~SPIDevice() noexcept {
        if (fd_) {
           close(fd_);
        }
    }
private:
    template<typename T>
    [[nodiscard]] bool setViaIOCTL(unsigned long message, T value) noexcept {
           return ioctl(fd_, message, &value) >= 0;
    }
    template<typename R, typename T = R>
    [[nodiscard]] std::optional<R> getViaIOCTL(unsigned long message) const noexcept {
        if (T result{}; ioctl(fd_, message, &result) < 0) {
            return std::nullopt;
        } else {
            return std::make_optional(static_cast<R>(result));
        }
    }
public:
    bool setMode(Mode mode) noexcept { return setViaIOCTL(SPI_IOC_WR_MODE, static_cast<byte>(mode)); }
    bool setBitsPerWord(uint8_t numBits = 8) noexcept { return setViaIOCTL(SPI_IOC_WR_BITS_PER_WORD, numBits); }
    bool setDirection(BitDirection direction) noexcept { return setViaIOCTL(SPI_IOC_WR_LSB_FIRST, static_cast<byte>(direction)); }
    bool setMaxSpeed(uint32_t speed) noexcept { return setViaIOCTL(SPI_IOC_WR_MAX_SPEED_HZ, speed); }
    [[nodiscard]] std::optional<Mode> getMode() const noexcept { return getViaIOCTL<Mode, byte>(SPI_IOC_RD_MODE); }
    [[nodiscard]] std::optional<uint8_t> getNumBitsPerWord() const noexcept { return getViaIOCTL<uint8_t>(SPI_IOC_RD_BITS_PER_WORD); }
    [[nodiscard]] std::optional<BitDirection> getDirection() const noexcept { return getViaIOCTL<BitDirection, uint8_t>(SPI_IOC_RD_LSB_FIRST); }
    [[nodiscard]] std::optional<uint32_t> getMaxSpeed() const noexcept { return getViaIOCTL<uint32_t>(SPI_IOC_RD_MAX_SPEED_HZ); }
    [[nodiscard]] constexpr bool valid() const noexcept { return static_cast<bool>(fd_); }
    [[nodiscard]] constexpr const std::string& getPath() const noexcept { return path_; }
    int transfer(uint32_t speed, char* txBuf, char* rxBuf, unsigned int count, unsigned int delay = 0) noexcept {
        spi_ioc_transfer spi{};
        /// @todo implement
        spi.tx_buf = reinterpret_cast<decltype(spi.tx_buf)>(txBuf);
        spi.rx_buf = reinterpret_cast<decltype(spi.rx_buf)>(rxBuf);
        spi.len = count;
        spi.speed_hz = speed;
        if (auto result = getNumBitsPerWord(); result) {
            spi.bits_per_word = *result;
        } else {
            spi.bits_per_word = 8;
        }
        spi.cs_change = 0;
        return ioctl(fd_, SPI_IOC_MESSAGE(1), &spi);
    }
    int transfer(uint32_t speed, char* buf, unsigned int count, unsigned int delay = 0) noexcept {
        return transfer(speed, buf, buf, count, delay);
    }
private:
    std::string path_;
    int fd_;
};
namespace Electron
{
    DefWrapperSymbolicName(SPIDevice::Ptr, "spidevice")
}
namespace {
    /**
     * @brief Maintains a global list of opened spi devices, this is done to make sure that there is a safe area to get the underlying spi device from
     * should we introduce functionality to access this device again. It ties the opened path to the underlying device.
     * This can have some problems but normally it isn't one if you're careful.
     */
    std::map<std::string, SPIDevice::Ptr> openDeviceList_;
    void
    openSPIDevice(UDF_ARGS__) noexcept {
        auto& theEnv = Electron::Environment::fromRaw(env);
        out->lexemeValue = theEnv.falseSymbol();
        UDFValue arg0;
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::Lexeme, &arg0)) {
            return;
        }
        std::string path(arg0.lexemeValue->contents);
        if (auto search = openDeviceList_.find(path); search != openDeviceList_.end()) {
            out->externalAddressValue = theEnv.createExternalAddress<SPIDevice::Ptr>(search->second);
        } else {
            // emplace and then put that into the output result
            auto theChip = std::make_shared<SPIDevice>(path);
            auto [iter, _] = openDeviceList_.try_emplace(path, theChip);
            // then make sure we stash the _emplaced_ copy into the return cell, this is very important to prevent
            // errors at runtime that are very very very hard to debug
            if (iter->second->valid()) {
                out->externalAddressValue = theEnv.createExternalAddress<SPIDevice::Ptr>(iter->second);
            } else {
                // after all of that we were not successful in opening the device for whatever reason
                // this is why the function return is considered "optional" in this case.
                out->lexemeValue = theEnv.falseSymbol();
            }
        }
    }
    void
    doSPITransfer(UDF_ARGS__) noexcept {
        auto& theEnv = Electron::Environment::fromRaw(env);
        out->lexemeValue = theEnv.falseSymbol();
        UDFValue arg0, arg1, arg2, arg3;
        if (!theEnv.firstArgument(context, Electron::ArgumentBits::ExternalAddress, &arg0)) {
            return;
        }
        if (!theEnv.nextArgument(context, Electron::ArgumentBits::Multifield, &arg1)) {
            return;
        }
        if (!theEnv.nextArgument(context, Electron::ArgumentBits::Integer, &arg2)) {
            return;
        }
        if (!theEnv.nextArgument(context, Electron::ArgumentBits::Integer, &arg3)) {
            return;
        }
        if (!theEnv.externalAddressIsOfType<SPIDevice::Ptr>(arg0)) {
            return;
        }
        auto theDevice = theEnv.fromExternalAddressAsRef<SPIDevice::Ptr>(arg0);
        auto theMultifield = arg1.multifieldValue->contents;
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
        if (auto err = theDevice->transfer(static_cast<uint32_t>(arg2.integerValue->contents),
                                       multifieldUnpack.data(),
                                       receiveBuffer.get(),
                                       multifieldUnpack.size(),
                                       static_cast<uint32_t>(arg3.integerValue->contents)); err == 0) {
            Electron::MultifieldBuilder mb(theEnv);
            for (size_t i = 0;i < multifieldUnpack.size(); ++i) {
                mb.append(static_cast<int64_t>(receiveBuffer[i]));
            }
            out->multifieldValue = mb.create();
        } else {
            /// @todo maybe check error code?
            out->lexemeValue = theEnv.falseSymbol();
        }

    }
}
void
installSPIExtensions(Electron::Environment& theEnv) {
    theEnv.registerExternalAddressType<SPIDevice::Ptr>(nullptr,
                                                       nullptr,
                                                       nullptr);
    using ArgType = Electron::ArgumentTypes;
    theEnv.addFunction("spi-open",
                       Electron::optionalReturnType(ArgType::ExternalAddress),
                       1, 1,
                       Electron::makeArgumentList(Electron::SingleArgument{ArgType::Symbol, ArgType::String}),
                       openSPIDevice,
                       "openSPIDevice");

    theEnv.addFunction("spi-transfer",
                       Electron::optionalReturnType(ArgType::Multifield),
                       4, 4,
                       Electron::makeArgumentList(
                               Electron::SingleArgument{ArgType::ExternalAddress},
                               Electron::SingleArgument{ArgType::ExternalAddress},
                               Electron::SingleArgument{ArgType::Multifield},
                               Electron::SingleArgument{ArgType::Integer},
                               Electron::SingleArgument{ArgType::Integer}),
                       doSPITransfer,
                       "doSPITransfer");
}
