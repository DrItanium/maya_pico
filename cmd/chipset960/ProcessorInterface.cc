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
    void
    ChipsetInterface::waitForTransactionStart() noexcept {
        while (InTransaction.isDeasserted());
    }
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
        uint8_t command[4] {
                generateReadOpcode(address) ,
                static_cast<uint8_t>(target),
                0,
                0,
        };
        doSPITransaction(command, 3);
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
    ChipsetInterface::doSPITransaction(uint8_t *command, int count) {
        Neutron::SPI::transfer(0, reinterpret_cast<char*>(command), count);
    }
    void
    ChipsetInterface::setupDataLines() noexcept {
        std::cout << "Setting up Data Lines" << std::endl;
        /// setup HAEN
        static constexpr uint8_t ioconDefault = 0b0000'1000;
        setIOCON<IOExpanderAddress::Lower16Lines>(ioconDefault);
        setIOCON<IOExpanderAddress::Upper16Lines>(ioconDefault);
        setIOCON<IOExpanderAddress::DataLines>(ioconDefault);
        setIOCON<IOExpanderAddress::Backplane>(ioconDefault);
        if (auto result = getIOCON<IOExpanderAddress::Lower16Lines>(); result != ioconDefault) {
            if (result == 0) {
                shutdown("Unable to communicate with the IO Expanders! There is probably a lock open on the SPI device! Reboot the Raspberry Pi!");
            } else {
                shutdown("Outcome from ioexpander is not what is expected but is not zero! Double check the code!");
            }
        }
        setDirection<IOExpanderAddress::Backplane>(backplaneGPIODirection_);
        writeGPIO16<IOExpanderAddress::Backplane>(backplaneGPIOStatus_);
        setDirection<IOExpanderAddress::Lower16Lines>(0xFFFF);
        setDirection<IOExpanderAddress::Upper16Lines>(0xFFFF);
        setDirection<IOExpanderAddress::DataLines>(currentDataLineDirection_);
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
        std::cout << "Waiting for boot signal" << std::endl;
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
        /// @todo insert GPIO release command here
        // exit at this point
        exit(1);
    }
    ChipsetInterface::ChipsetInterface() : ChipsetInterface::Parent() {
        installExtensions();
    }
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
    bool
    ChipsetInterface::signalCPU() noexcept {
        Ready.assertPin();
        while (InTransaction.isAsserted() && Blast.isDeasserted());
        auto outcome = InTransaction.isDeasserted();
        Ready.deassertPin();
        return outcome;
    }
    void
    ChipsetInterface::waitForCycleUnlock() noexcept {
        while (DoCycle.isDeasserted());
    }
    uint16_t
    ChipsetInterface::getDataLines() noexcept {
        return readGPIO16<IOExpanderAddress::DataLines>();
    }

    void
    ChipsetInterface::setDataLines(uint16_t value) noexcept {
        writeGPIO16<IOExpanderAddress::DataLines>(value);
    }
    void
    ChipsetInterface::setupDataLinesForWrite() noexcept {
        // input
        if (!currentDataLineDirection_) {
            currentDataLineDirection_ = ~currentDataLineDirection_;
            setDirection<IOExpanderAddress::DataLines>(currentDataLineDirection_);
        }
    }
    void
    ChipsetInterface::setupDataLinesForRead() noexcept {
        // output
        if (currentDataLineDirection_) {
            currentDataLineDirection_ = ~currentDataLineDirection_;
            setDirection<IOExpanderAddress::DataLines>(currentDataLineDirection_);
        }
    }
    namespace {
        void
        doChipsetShutdown(UDF_ARGS__) noexcept {
            auto& theEnv = reinterpret_cast<ChipsetInterface&>(Electron::Environment::fromRaw(env));
            UDFValue message;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Lexeme, &message)) {
                out->lexemeValue = theEnv.falseSymbol();
                return;
            }
            out->lexemeValue = theEnv.trueSymbol();
            std::string theStr(message.lexemeValue->contents);
            theEnv.shutdown(theStr);
        }
    }
    void
    ChipsetInterface::installExtensions() noexcept {
        if (!extensionsInstalled_) {
            extensionsInstalled_ = true;
            addFunction("shutdown960",
                        Electron::makeReturnType(Electron::ArgumentTypes::Boolean),
                        1, 1, Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Symbol,
                                                                                  Electron::ArgumentTypes::String}),
                        doChipsetShutdown,
                        "doChipsetShutdown");
        }
    }
}