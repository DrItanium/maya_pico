/**
 * @file
 * Wrapper for boost's tokenizer
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

//#include "boost/tokenizer.hpp"

namespace Neutron
{
  /// @todo: there are more template arguments but we don't need them right now
  //using boost::keep_empty_tokens;
  //template<typename Char>
  //using CharSeparator = boost::char_separator<Char>;
  //template<typename TokenizerFunc>
  //using Tokenizer = boost::tokenizer<TokenizerFunc>;
  //template<typename Char>
  //using CharTokenizer = Tokenizer<CharSeparator<Char>>;
  /**
   * A character tokenizer that operates on chars
   */
  //using SingleCharTokenizer = CharTokenizer<char>;
  //using SingleCharSeparator = CharSeparator<char>;
} // Neutron
