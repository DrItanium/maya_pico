/**
 * @file
 * Implementation of processor interface routines
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
#include <iostream>
#include "ChipsetInterface.h"

namespace i960 {

    template<typename ... T>
    void
    configurePinBlock(T&& ... pins) noexcept {
        (pins.configure(), ...);
    }
    void
    digitalWrite(Pinout pin, PinValue value) {
        Neutron::GPIO::digitalWrite(static_cast<int>(pin), value) ;

    }
    PinValue
    digitalRead(Pinout pin) {
        return Neutron::GPIO::digitalRead(static_cast<int>(pin));
    }
    void
    pinMode(Pinout pin, PinDirection direction) noexcept {
        Neutron::GPIO::pinMode(static_cast<int>(pin), direction);
    }

    bool
    ChipsetInterface::isReadOperation() noexcept {
        return WR.isAsserted();
    }

    bool
    ChipsetInterface::isWriteOperation() noexcept {
        return WR.isDeasserted();
    }

    LoadStoreStyle
    ChipsetInterface::getStyle() noexcept {
        if (BE0.isAsserted()) {
            if (BE1.isAsserted())  {
                return LoadStoreStyle::Full16;
            } else {
                return LoadStoreStyle::Lower8;
            }
        } else {
            if (BE1.isAsserted()) {
                return LoadStoreStyle::Upper8;
            } else {
                return LoadStoreStyle::None;
            }
        }
    }
    uint32_t
    ChipsetInterface::getAddress() noexcept {
        /// @todo implement
        return 0;
    }
    void
    ChipsetInterface::performReadTransaction() noexcept {

    }
    void
    ChipsetInterface::performWriteTransaction() noexcept {

    }
    void
    ChipsetInterface::newDataCycle() noexcept {
        auto address = getAddress();
        if (isReadOperation()) {
            performReadTransaction();
        } else {
            performWriteTransaction();
        }
    }
    void
    ChipsetInterface::waitForTransactionStart() noexcept {
        while (InTransaction.isDeasserted());
    }
    union IOExpanderCommand {
        uint32_t raw;
        uint8_t bytes[sizeof(uint32_t)];
    };
    void
    ChipsetInterface::write8(IOExpanderAddress address, MCP23x17Registers target, uint8_t value) {
        uint8_t command[4] {
                generateWriteOpcode(address),
                static_cast<uint8_t>(target),
                value,
                0,
        };
        doSPITransaction(command, 3);
    }
    void
    ChipsetInterface::write16(IOExpanderAddress address, MCP23x17Registers target, uint16_t value) {
        uint8_t command[4] {
                generateWriteOpcode(address) ,
                static_cast<uint8_t>(target),
                static_cast<uint8_t>(value),
                static_cast<uint8_t>(value >> 8),
        };
        doSPITransaction(command, 4);
    }
    uint8_t
    ChipsetInterface::read8(IOExpanderAddress address, MCP23x17Registers target) {
        uint8_t command[3] {
                generateReadOpcode(address) ,
                static_cast<uint8_t>(target),
                0,
        };
        for (int i = 0; i < 3; ++i) {
            std::cout << "before command[" << i << "] = " << static_cast<int>(command[i]) << std::endl;
        }
        doSPITransaction(command, 3);
        for (int i = 0; i < 3; ++i) {
            std::cout << "after command[" << i << "] = " << static_cast<int>(command[i]) << std::endl;
        }
        return command[2];
    }
    uint16_t
    ChipsetInterface::read16(IOExpanderAddress address, MCP23x17Registers target) {
        uint8_t command[4] {
                generateReadOpcode(address) ,
                static_cast<uint8_t>(target),
                0,
                0,
        };
        doSPITransaction(command, 4);
        return static_cast<uint16_t>(command[2]) | (static_cast<uint16_t>(command[3]) << 8);
    }
    void
    ChipsetInterface::doSPITransaction(uint8_t *storage, int count) {
        Neutron::SPI::beginTransaction(0, 10 * 1000 * 1000, Neutron::SPI::mode0());
        Neutron::SPI::transfer(0, reinterpret_cast<char*>(storage), count);
        Neutron::SPI::endTransaction(0);
    }
    void
    ChipsetInterface::setupDataLines() noexcept {
        std::cout << "Setting up Data Lines" << std::endl;
        /// setup HAEN
        write8(IOExpanderAddress::Lower16Lines, MCP23x17Registers::IOCON, 0b0100'1000);
        write8(IOExpanderAddress::Upper16Lines, MCP23x17Registers::IOCON, 0b0100'1000);
        write8(IOExpanderAddress::DataLines, MCP23x17Registers::IOCON, 0b0000'1000);
        write8(IOExpanderAddress::Extras, MCP23x17Registers::IOCON, 0b0000'1000);
        int result = read8(IOExpanderAddress::Lower16Lines, MCP23x17Registers::IOCON);
        std::cout << "\tLower16Lines.IOCON = 0x" << std::hex << result << std::endl;
        result = read8(IOExpanderAddress::Upper16Lines, MCP23x17Registers::IOCON);
        std::cout << "\tUpper16Lines.IOCON = 0x" << std::hex << result << std::endl;
        write16(IOExpanderAddress::Extras, MCP23x17Registers::IODIR, currentGPIO4Direction_);
        write16(IOExpanderAddress::Extras, MCP23x17Registers::GPIO, currentGPIO4Status_);
        write16(IOExpanderAddress::Lower16Lines, MCP23x17Registers::IODIR, 0xFFFF); // input
        write16(IOExpanderAddress::Upper16Lines, MCP23x17Registers::IODIR, 0xFFFF); // input
        write16(IOExpanderAddress::DataLines, MCP23x17Registers::IODIR, currentDataLineDirection_); // input
        write16(IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPINTEN, 0xFFFF);
        write16(IOExpanderAddress::Lower16Lines, MCP23x17Registers::INTCON, 0);
        write16(IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPINTEN, 0xFFFF);
        write16(IOExpanderAddress::Upper16Lines, MCP23x17Registers::INTCON, 0);
        write16(IOExpanderAddress::DataLines, MCP23x17Registers::GPINTEN, 0xFFFF);
        write16(IOExpanderAddress::DataLines, MCP23x17Registers::INTCON, 0);

        write16(IOExpanderAddress::DataLines, MCP23x17Registers::OLAT, latchedDataOutput_);
    }
    void
    ChipsetInterface::waitForBootSignal() noexcept {
        while (BootSuccessful.digitalRead() == PinValue::Low);
        /// @todo add interrupt to BootSuccessful pin
        Neutron::GPIO::attachInterrupt(static_cast<int>(Pinout::BootSuccessful),
                                       Neutron::GPIO::InterruptMode::Falling,
                                       []() { i960::shutdown("CHECKSUM FAILURE"); });
    }
    void
    ChipsetInterface::shutdown(const std::string& str) noexcept {
        std::cout << "SHUTDOWN: " << str << std::endl;
        SetHaltExecution(getRawEnvironment(),true);
        CloseAllBatchSources(getRawEnvironment());
        ManagementEngineReset.assertPin();
        WaitBoot960.assertPin();
        ManagementEngineReset.deassertPin();
        // exit at this point
        exit(1);
    }
    ChipsetInterface::ChipsetInterface() : ChipsetInterface::Parent() { }
    void
    ChipsetInterface::setupPins() noexcept {
        i960::configurePinBlock(Ready,
                                BootSuccessful,
                                WR,
                                BE0,
                                BE1,
                                InTransaction,
                                DoCycle,
                                Blast,
                                ManagementEngineReset,
                                WaitBoot960,
                                IOEXP_INT0,
                                IOEXP_INT1,
                                IOEXP_INT2,
                                IOEXP_INT3,
                                IOEXP_INT4,
                                IOEXP_INT5,
                                IOEXP_INT6,
                                IOEXP_INT7);
        std::cout << "Pulling Management Engine into Reset and starting configuration" << std::endl;
    }
    void
    shutdown(const std::string& msg) noexcept {
        ChipsetInterface::get().shutdown(msg);
    }
    void
    ChipsetInterface::putManagementEngineInReset() noexcept {
        ManagementEngineReset.assertPin();
        WaitBoot960.assertPin();
    }
    void
    ChipsetInterface::pullManagementEngineOutOfReset() noexcept {
        ManagementEngineReset.deassertPin();
    }
    void
    ChipsetInterface::loadMicrocode() noexcept {
        if (!batchFile("ucode.clp")) {
            shutdown("Cannot find ucode.clp!");
        }
    }
    void
    ChipsetInterface::pull960OutOfReset() noexcept {
        WaitBoot960.deassertPin();
    }
    /*
    void
    ChipsetInterface::begin() {
        setupPins();
        putManagementEngineInReset();
        loadMicrocode();
        // pull the management engine into reset
        /// @todo introduce a delay?
        std::cout << "Keeping the i960 In Reset but the Management Engine active" << std::endl;
        pullManagementEngineOutOfReset();
        systemSetup();
        setupDataLinesForRead();
        pull960OutOfReset();
        waitForBootSignal();
        std::cout << "i960 Successfully Booted!" << std::endl;
    }
    */
}