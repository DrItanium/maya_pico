/**
 * @file
 * Linux SPIDev backend tracking
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

#include "platform/config.h"
#ifdef HAVE_LINUX_SPIDEV_H
#include "interface/LinuxSPIDev.h"
#include <map>
#include <memory>
#include <sstream>
namespace Neutron::SPI::SPIDEV::Implementation
{
    namespace {
        using SPIDevice = Device;
        std::map<std::string, SPIDevice::Ptr> openDeviceList_;
        std::string
        makeDevicePath(int channel) noexcept {
            std::stringstream builder;
            builder << "/dev/gpiochip" << channel;
            return builder.str(); /// @todo should we give this a name or will eliding the temporary work safely?
        }
        SPIDevice::Ptr
        getDevice(int channel) noexcept {
            auto path = makeDevicePath(channel);
            if (auto search = openDeviceList_.find(path); search != openDeviceList_.end()) {
                return search->second;
            } else {
                auto theChip = std::make_shared<SPIDevice>(path);
                auto [iter, _] = openDeviceList_.try_emplace(path, theChip);
                return iter->second;
            }
        }
    }
    bool
    begin(int channel, int speed) {
        // unlike the first generation spi interface, this one abstracts the underlying implementation
        auto result = getDevice(channel);
        result->setMaxSpeed(speed);
        return result->valid();
    }
    bool
    beginTransaction(int channel, int speed, int mode) {
        auto result = getDevice(channel);
        result->setMode(static_cast<Device::Mode>(mode));
        result->setMaxSpeed(speed);
        return result->valid();
    }
    bool
    endTransaction(int) {
        // do nothing
        return true;
    }
    void
    transfer(int channel, char *data, int count) {
        if (auto result = getDevice(channel); result->valid()) {
            if (auto speed = result->getMaxSpeed(); speed) {
                result->transfer(speed.value_or(0), data, count);

            }
        }
    }
    Device::~Device() noexcept {
        if (fd_) {
            ::close(fd_);
        }
    }
}
#endif // end HAVE_LINUX_SPIDEV_H
