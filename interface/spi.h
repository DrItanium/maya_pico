/**
 * @file
 * Generalized interface for handling SPI devices, this is not CLIPS specific nor is it target specific
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
// Created by jwscoggins on 2/25/22.
//

#ifndef MAYA_SPI_H
#define MAYA_SPI_H
#include "platform/os.h"
#include "platform/config.h"
#include "platform/types.h"
#include "electron/Environment.h"
namespace Neutron::SPI {
    bool begin(int channel, int speed);
    bool beginTransaction(int channel, int speed, int mode);
    bool endTransaction(int channel);
    void transfer(int channel, char* data, int count);
    /**
     * @brief General wrapper around the very simple features that make up this gpio device
     * @param theEnv The environment to install the gpio devices into
     */
    void installExtensions(Electron::Environment& theEnv) noexcept;
} // end namespace Neutron::SPI

#endif //MAYA_SPI_H
