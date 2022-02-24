/**
 * @file
 * Make constructing argument lists simpler
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

#ifndef MAYA_ARGUMENTCONSTRUCTOR_H
#define MAYA_ARGUMENTCONSTRUCTOR_H
#include <functional>
#include <string>
#include <set>
#include <list>
#include <type_traits>
#include <sstream>
namespace Electron
{
    /**
     * @brief Mapping of CLIPS argument characters to an enum
     */
    enum class ArgumentTypes : char {
        Any = '*',
        Integer = 'l',
        Float = 'd',
        String = 's',
        Symbol = 'y',
        InstanceName = 'n',
        Multifield = 'm',
        FactAddress = 'f',
        InstanceAddress = 'i',
        ExternalAddress = 'e',
        Void = 'v',
    };

    /**
     * @brief Defines the types acceptible by a single argument in a CLIPS output or input argument
     */
    class SingleArgument final {
    public:
        template<typename ... T>
        static SingleArgument make(T&&... types) noexcept {
            SingleArgument arg;
            arg.addMany(std::forward(types...)) ;
            return arg;
        }
    public:
        void add(ArgumentTypes type) noexcept {
            args_.emplace(type);
        }
        template<typename ... T>
        void addMany(T&&... types) noexcept {
            (add(types), ...);
        }
        [[nodiscard]] std::string str() const noexcept {
            std::ostringstream stream;
            for (const auto& a : args_) {
                stream << static_cast<std::underlying_type_t<ArgumentTypes>>(a);
            }
            // make sure that we don't have issues with temporaries
            auto newStr = stream.str();
            return newStr;
        }
        [[nodiscard]] bool empty() const noexcept {
            return args_.empty();
        }
        [[nodiscard]] auto size() const noexcept { return args_.size(); }
    private:
        std::set<ArgumentTypes> args_;
    };

    /**
     * @brief manages multiple single arguments at a time
     */
    class MultiArgument final {
    public:
        template<typename ... T>
        static MultiArgument make(T&&... args) noexcept {
            MultiArgument arg;
            arg.addMany(std::forward(args...));
            return arg;
        }
    public:
        void add(const SingleArgument& argument) noexcept {
            args_.emplace_back(argument);
        }
        template<typename ... T>
        void addMany(T&&... args) noexcept {
            (add(args), ...);
        }
        [[nodiscard]] bool empty() const noexcept {
            return args_.empty();
        }
        [[nodiscard]] std::string str() const noexcept {
            if (args_.empty()) {
                return "";
            }
            std::ostringstream stream;
            // a simple flag to make sure that we don't add ; to the front of the first element
            bool start = true;
            for (const auto& a : args_) {
                if (auto theStr = a.str(); start)  {
                    start = false;
                    stream << theStr;
                } else {
                    stream << ';' << theStr;
                }
            }
            auto outcome = stream.str();
            return outcome;
        }
    private:
        std::list<SingleArgument> args_;
    };
    template<typename ... T>
    std::string makeReturnType(T&&... args) noexcept {
        if (sizeof...(T) == 0) {
            return "";
        } else {
            return SingleArgument::make(std::forward(args...)).str();
        }
    }
    template<typename ... T>
    std::string makeArgumentList(T&&... args) noexcept {
        if (sizeof...(T) == 0) {
            return "";
        } else {
            return MultiArgument::make(std::forward(args...)).str();
        }
    }

} // end namespace Electron

#endif //MAYA_ARGUMENTCONSTRUCTOR_H
