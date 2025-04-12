/**
 * @file
 * Make it possible to do the equivalent of static_assert(false) in constexpr if statements.
 * @copyright
 * Copyright (c) 2015-2022 Parasoft Corporation
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */
#ifndef Neutron_Features_Dependent_False_h__
#define Neutron_Features_Dependent_False_h__
#include <type_traits>

namespace Neutron {
/**
 * Allow static_assert(false) equivalent inside of constexpr if statement blocks.
 * taken from https://en.cppreference.com/w/cpp/language/if
 * @tparam T the type to return false on
 */
template<typename T>
struct DependentFalse final : std::false_type { };
} // end namespace Neutron
#endif // end Neutron_Features_Dependent_False_h__
