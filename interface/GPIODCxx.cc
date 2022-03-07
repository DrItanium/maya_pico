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
        OpenGPIOChipTracking tracking_;
        void
        digitalWrite(const PinPtr &thePin, int value) noexcept {
            thePin->set_value(value != 0 ? 1 : 0);
        }
        int
        digitalRead(const PinPtr &thePin) noexcept {
            return thePin->get_value();
        }
        void
        pinMode(const PinPtr &ptr, PinMode direction) {
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
    }
    void
    pinMode(int targetPin, PinMode direction) {

    }
    PinValue digitalRead(int targetPin) {
        return PinValue::Low;
    }
    void digitalWrite(int targetPin, PinValue value) {

    }
    bool attachInterrupt(int targetPin, InterruptMode mode, ISRFunction function) {
        return false;
    }
    void
    begin() {

    }
}
#endif // end HAVE_GPIOD_HPP