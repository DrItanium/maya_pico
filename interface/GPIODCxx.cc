/**
 * @file
 * Linux GPIODCXX basic interface
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
#ifdef HAVE_GPIOD_HPP
#include "platform/types.h"
#include <gpiod.hpp>
#include <memory>
#include <map>
#include "interface/gpio.h"
namespace Neutron::GPIO::GPIOD::Implementation {

    using Chip = gpiod::chip;
    using ChipPtr = std::shared_ptr<Chip>;
    using Pin = gpiod::line;
    using PinPtr = std::shared_ptr<Pin>;
    using PinRequest = gpiod::line_request;
    using PinEvent = gpiod::line_event;
    using PinBulk = gpiod::line_bulk;
    using OpenGPIOChipTracking = std::map<std::string, ChipPtr>;
    using IndexToPinMapping = std::map<size_t, PinPtr>;
    namespace
    {
        OpenGPIOChipTracking openedChips_;
        /**
         * @brief Maps an open GPIOChip to a corresponding set of pins
         */
        std::map<std::string, IndexToPinMapping> chipToPinMapping_;
        void
        digitalWrite(const PinPtr &thePin, PinValue value) noexcept {
            thePin->set_value(static_cast<int>(value));
        }
        int
        digitalRead(const PinPtr &thePin) noexcept {
            return thePin->get_value();
        }
        void
        configurePin(const PinPtr &ptr, PinMode direction) {
            switch (direction) {
                case PinMode::Input:
                    ptr->request({
                                         ptr->consumer(),
                                         gpiod::line_request::DIRECTION_INPUT,
                                         gpiod::line_request::FLAG_BIAS_DISABLE,
                                 });
                    break;
                case PinMode::Output:
                    ptr->request({
                                         ptr->consumer(),
                                         gpiod::line_request::DIRECTION_OUTPUT,
                                         0
                                 });
                    break;
                case PinMode::InputPullup:
                    ptr->request({
                                         ptr->consumer(),
                                         gpiod::line_request::DIRECTION_INPUT,
                                         gpiod::line_request::FLAG_BIAS_PULL_UP,
                                 });
                    break;
                case PinMode::InputPulldown:
                    ptr->request({
                                         ptr->consumer(),
                                         gpiod::line_request::DIRECTION_INPUT,
                                         gpiod::line_request::FLAG_BIAS_PULL_DOWN,
                                 });
                    break;
                default:
                    // do nothing
                    break;
            }
        }
        std::string
        makeDevicePath(int channel) noexcept {
            std::stringstream builder;
            builder << "/dev/gpiochip" << channel;
            return builder.str(); /// @todo should we give this a name or will eliding the temporary work safely?
        }
        ChipPtr
        getDevice(int channel) noexcept {
            auto path = makeDevicePath(channel) ;
            try {
                if (auto search = openedChips_.find(path); search != openedChips_.end()) {
                    return search->second;
                } else {
                    // emplace and then put that into the output result
                    auto theChip = std::make_shared<Chip>(path);
                    auto [iter, _] = openedChips_.try_emplace(path, theChip);
                    chipToPinMapping_.try_emplace(theChip->name(), IndexToPinMapping {});
                    return iter->second;
                }
            } catch(std::system_error&) {
                // by default we get a thrown exception
                return nullptr;
            }
        }
        PinPtr
        getPin(int channel, int pin) noexcept {
            if (auto theDevice = getDevice(channel); theDevice) {
                try {
                    if (auto search = chipToPinMapping_.find(theDevice->name()); search != chipToPinMapping_.end()) {
                        if (auto find2 = search->second.find(pin); find2 != search->second.end()) {
                            // we got a hit so return that
                            return find2->second;
                        } else {
                            // no hit, so install it
                            auto outputPtr = std::make_shared<Pin>(theDevice->get_line(pin));
                            auto [result, _] = search->second.try_emplace(pin, outputPtr);
                            return result->second;
                        }
                    } else {
                        return nullptr;
                    }

                } catch (std::out_of_range&) {
                    // swallow the error and just return false
                    return nullptr;
                }
            } else {
                return nullptr;
            }
        }

    }
    void
    pinMode(int targetPin, PinMode direction) {
        if (auto thePin = getPin(0, targetPin); thePin) {
            configurePin(thePin, direction);
        }
    }
    PinValue digitalRead(int targetPin) {
        if (auto thePin = getPin(0, targetPin); thePin) {
            return static_cast<PinValue>(digitalRead(thePin));
        } else {
            return PinValue::Low;
        }
    }
    void digitalWrite(int targetPin, PinValue value) {
        if (auto thePin = getPin(0, targetPin); thePin) {
            digitalWrite(thePin, value);
        }
    }
    bool attachInterrupt(int targetPin, InterruptMode mode, ISRFunction function) {
        return false;
    }
    bool
    begin() {
        return getDevice(0).operator bool();
    }
}
#endif // end HAVE_GPIOD_HPP