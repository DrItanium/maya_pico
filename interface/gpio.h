/**
 * @file
 * Generalized interface for handling GPIO devices, this is not CLIPS specific
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

#ifndef MAYA_GPIO_H
#define MAYA_GPIO_H
#ifdef HAVE_GPIOD_HPP
#include <gpiod.hpp>
#endif
#include <memory>
#include "platform/types.h"
namespace Neutron::GPIO {
    using Chip = gpiod::chip;
    using ChipPtr = std::shared_ptr<Chip>;
    using Pin = gpiod::line;
    using PinPtr = std::shared_ptr<Pin>;
    using PinRequest = gpiod::line_request;
    using PinEvent = gpiod::line_event;
    using PinBulk = gpiod::line_bulk;
    enum class PinDirection {
        None,
        Input,
        Output,
        InputPullup,
    };
    enum class PinValue {
        Low = 0,
        High = 1,
    };
    /**
     * @brief GPIO information specific to the raspberry pi
     */
    namespace RaspberryPi {
        /**
         * @brief Translates the physical pin id to its internal broadcom pin id
         * @tparam index
         */
        template<int index>
        constexpr auto PhysicalToBCMTranslation_v = -1;
#define X(from, to) template<> constexpr auto PhysicalToBCMTranslation_v < from > = to
        X(3,2);
        X(5,3);
        X(7, 4);
        X(8, 14);
        X(10, 15);
        X(11, 17);
        X(12, 18);
        X(13, 27);
        X(15, 22);
        X(16, 23);
        X(18, 24);
        X(19, 10);
        X(21, 9);
        X(22, 25);
        X(23, 11);
        X(24, 8);
        X(26, 7);
        // do not include the EEPROM i2c pins to be on the safe side
        X(29, 5);
        X(31, 6);
        X(32, 12);
        X(33, 13);
        X(35, 19);
        X(36, 16);
        X(37, 26);
        X(38, 20);
        X(40, 21);
#undef X
        /**
         * @brief Compile time constant that allows one to figure out if the given physical pin index maps to a valid pin
         * @tparam index The physical pin index to check
         */
        template<int index>
        constexpr bool PhysicalPinIndexIsValid_v = PhysicalToBCMTranslation_v<index> != -1;
    } // end namespace Neutron::GPIO::RaspberryPi
} // end namespace Neutron::GPIO

#endif //MAYA_GPIO_H
