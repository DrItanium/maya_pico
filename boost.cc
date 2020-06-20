// maya
// Copyright (c) 2012-2019, Joshua Scoggins
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
extern "C" {
#include "clips.h"
}
#include "mayasetup.h"
#include "boost.h"
#include <string>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/math/common_factor.hpp>
#include <boost/filesystem.hpp>
#include <boost/system/error_code.hpp>
#include <boost/algorithm/clamp.hpp>


#if BOOST_EXTENSIONS
using Path = boost::filesystem::path;
using DirectoryIterator = boost::filesystem::directory_iterator;
using FilesystemError = boost::filesystem::filesystem_error;
using boost::filesystem::is_regular_file;
using boost::filesystem::is_directory;
using boost::starts_with;
using boost::ends_with;
void HasPrefix(Environment*, UDFContext*, UDFValue*);
void HasSuffix(Environment*, UDFContext*, UDFValue*);
void TrimString(Environment*, UDFContext*, UDFValue*);
void TrimStringFront(Environment*, UDFContext*, UDFValue*);
void TrimStringBack(Environment*, UDFContext*, UDFValue*);
void NewUUID(Environment*, UDFContext*, UDFValue*);
void gcdFunction(Environment*, UDFContext*, UDFValue*);
void lcmFunction(Environment*, UDFContext*, UDFValue*);
void FileExists(Environment*, UDFContext*, UDFValue*);
void IsDirectory(Environment*, UDFContext*, UDFValue*);
void IsRegularFile(Environment*, UDFContext*, UDFValue*);
void ClampValue(Environment*, UDFContext*, UDFValue*);
void GetDirectoryContents(Environment*, UDFContext*, UDFValue*);
void GetFileExtension(Environment*, UDFContext*, UDFValue*);
#endif

extern "C" void InstallBoostExtensions(Environment* theEnv) {
#if BOOST_EXTENSIONS
	AddUDF(theEnv, "has-prefix", "b", 2, 2, "sy;sy;sy", HasPrefix, "HasPrefix",  nullptr);
	AddUDF(theEnv, "has-suffix", "b", 2, 2, "sy;sy;sy", HasSuffix, "HasSuffix",  nullptr);
	AddUDF(theEnv, "string-trim", "y", 1, 1, "s", TrimString, "TrimString", nullptr);
	AddUDF(theEnv, "string-trim-front", "y", 1, 1, "s", TrimStringFront, "TrimStringFront", nullptr);
	AddUDF(theEnv, "string-trim-back", "y",  1, 1, "s", TrimStringBack, "TrimStringBack", nullptr);
	AddUDF(theEnv, "new-uuid", "s", 0, 0, "", NewUUID, "NewUUID", nullptr);
	AddUDF(theEnv, "gcd", "l",  2, 2, "l;l;l", gcdFunction, "gcdFunction", nullptr);
	AddUDF(theEnv, "lcm", "l",  2, 2, "l;l;l", lcmFunction, "lcmFunction", nullptr);
	AddUDF(theEnv, "path-exists",   "b", 1, 1, "sy", FileExists, "FileExists", nullptr);
	AddUDF(theEnv, "directoryp",    "b", 1, 1, "sy", IsDirectory, "IsDirectory", nullptr);
	AddUDF(theEnv, "regular-filep", "b", 1, 1, "sy", IsRegularFile, "IsRegularFile", nullptr);
	AddUDF(theEnv, "clamp", "l",  3, 3, "l;l;l;l", ClampValue, "ClampValue", nullptr);
	AddUDF(theEnv, "get-directory-contents", "m", 1, 1, "sy", GetDirectoryContents, "GetDirectoryContents", nullptr);
	AddUDF(theEnv, "get-file-extension", "sy", 1, 1, "sy", GetFileExtension, "GetFileExtension", nullptr);
#endif
}


#if BOOST_EXTENSIONS
void GetFileExtension(Environment* env, UDFContext* context, UDFValue* ret) {
	if (UDFValue path; !UDFFirstArgument(context, LEXEME_BITS, &path)) {
		ret->lexemeValue = CreateString(env, "");
	} else {
		if (Path p(path.lexemeValue->contents); is_regular_file(p)) {
			ret->lexemeValue = CreateString(env, p.has_extension() ? p.extension().string().c_str() : "");
		} else {
			ret->lexemeValue = CreateString(env, "");
		}
	}
}
void GetDirectoryContents(Environment* env, UDFContext* context, UDFValue* ret) {
	if (UDFValue path; !UDFFirstArgument(context, LEXEME_BITS, &path)) {
		ret->multifieldValue = EmptyMultifield(env);
	} else {
		if (Path p(path.lexemeValue->contents); is_directory(p)) {
			auto mb = CreateMultifieldBuilder(env, 10);
			try {
				DirectoryIterator it(p);
				for (const auto& path : it) {
					MBAppendString(mb, path.path().string().c_str());
				}
				ret->multifieldValue = MBCreate(mb);
				MBDispose(mb);
			} catch (FilesystemError &) {
				// probably permission denied or something similar so just 
				// emit empty
				ret->multifieldValue = EmptyMultifield(env);
			}
		} else {
			ret->multifieldValue = EmptyMultifield(env);
		}
	}
}
void ClampValue(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue v, lo, hi;
	if (!UDFFirstArgument(context, INTEGER_BIT,  &v)) {
		ret->lexemeValue = FalseSymbol(env);
	} else if (!UDFNextArgument(context, INTEGER_BIT, &lo)) {
		ret->lexemeValue = FalseSymbol(env);
	} else if (!UDFNextArgument(context, INTEGER_BIT, &hi)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->integerValue = CreateInteger(env, boost::algorithm::clamp(CVCoerceToInteger(&v), CVCoerceToInteger(&lo), CVCoerceToInteger(&hi)));
	}
}

void FileExists(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string p(path.lexemeValue->contents);
		ret->lexemeValue = CreateBoolean(env, boost::filesystem::exists(p));
	}
}

void IsDirectory(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string p(path.lexemeValue->contents);
		ret->lexemeValue = CreateBoolean(env, is_directory(p));
	}
}

void IsRegularFile(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string p(path.lexemeValue->contents);
		ret->lexemeValue = CreateBoolean(env, is_regular_file(p));
	}
}

void gcdFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue first, second;
	if (!UDFFirstArgument(context, INTEGER_BIT, &first)) {
		ret->lexemeValue = FalseSymbol(env);
	} else if (!UDFNextArgument(context, INTEGER_BIT, &second)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->integerValue = CreateInteger(env, boost::math::gcd(CVCoerceToInteger(&first), CVCoerceToInteger(&second)));
	}
}
void lcmFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue first, second;
	if (!UDFFirstArgument(context, INTEGER_BIT, &first)) {
		ret->lexemeValue = FalseSymbol(env);
	} else if (!UDFNextArgument(context, INTEGER_BIT, &second)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->integerValue = CreateInteger(env, boost::math::lcm(CVCoerceToInteger(&first), CVCoerceToInteger(&second)));
	}
}
void NewUUID(Environment* env, UDFContext* context, UDFValue* ret) {
	boost::uuids::random_generator rgen;
	boost::uuids::uuid theUUID(rgen());
	const std::string tmp = boost::lexical_cast<std::string>(theUUID);
	ret->value = CreateSymbol(env, tmp.c_str());
}
void HasPrefix(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue data, prefix;
	if (!UDFFirstArgument(context, LEXEME_BITS, &data)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	} else if (!UDFNextArgument(context, LEXEME_BITS, &prefix)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	}
	std::string dataStr(data.lexemeValue->contents);
	std::string prefixStr(prefix.lexemeValue->contents);
	ret->lexemeValue = CreateBoolean(env, starts_with(dataStr, prefixStr));
}

void HasSuffix(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue data, suffix;
	if (!UDFFirstArgument(context, LEXEME_BITS, &data)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	} else if (!UDFNextArgument(context, LEXEME_BITS, &suffix)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	}
	std::string dataStr(data.lexemeValue->contents);
	std::string suffixStr(suffix.lexemeValue->contents);
	ret->lexemeValue = CreateBoolean(env, ends_with(dataStr, suffixStr));
}
void TrimString(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
void TrimStringFront(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim_left(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
void TrimStringBack(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim_right(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
#endif



