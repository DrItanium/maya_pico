// maya
// Copyright (c) 2012-2016, Joshua Scoggins 
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


#if BOOST_EXTENSIONS
void HasPrefix(UDFContext*, CLIPSValue*);
void HasSuffix(UDFContext*, CLIPSValue*);
void TrimString(UDFContext*, CLIPSValue*);
void TrimStringFront(UDFContext*, CLIPSValue*);
void TrimStringBack(UDFContext*, CLIPSValue*);
void NewUUID(UDFContext*, CLIPSValue*);
void gcdFunction(UDFContext*, CLIPSValue*);
void lcmFunction(UDFContext*, CLIPSValue*);
#endif 

extern "C" void InstallBoostExtensions(void* theEnv) {
#if BOOST_EXTENSIONS
	EnvAddUDF(theEnv, "has-prefix", "b", HasPrefix, "HasPrefix", 2, 2, "sy;sy;sy", NULL);
	EnvAddUDF(theEnv, "has-suffix", "b", HasSuffix, "HasSuffix", 2, 2, "sy;sy;sy", NULL);
	EnvAddUDF(theEnv, "string-trim", "y", TrimString, "TrimString", 1, 1, "s", NULL);
	EnvAddUDF(theEnv, "string-trim-front", "y", TrimStringFront, "TrimStringFront", 1, 1, "s", NULL);
	EnvAddUDF(theEnv, "string-trim-back", "y", TrimStringBack, "TrimStringBack", 1, 1, "s", NULL);
	EnvAddUDF(theEnv, "new-uuid", "s", NewUUID, "NewUUID", 0, 0, "", NULL);
	EnvAddUDF(theEnv, "gcd", "l", gcdFunction, "gcdFunction", 2, 2, "l;l;l", NULL);
	EnvAddUDF(theEnv, "lcm", "l", lcmFunction, "lcmFunction", 2, 2, "l;l;l", NULL);
#endif 
}


#if BOOST_EXTENSIONS
void gcdFunction(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue first, second;
	if (!UDFFirstArgument(context, INTEGER_TYPE, &first)) {
		CVSetBoolean(ret, false);
	} else if (!UDFNextArgument(context, INTEGER_TYPE, &second)) {
		CVSetBoolean(ret, false);
	} else {
		CVSetInteger(ret, boost::math::gcd(CVToInteger(&first), CVToInteger(&second)));
	}
}
void lcmFunction(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue first, second;
	if (!UDFFirstArgument(context, INTEGER_TYPE, &first)) {
		CVSetBoolean(ret, false);
	} else if (!UDFNextArgument(context, INTEGER_TYPE, &second)) {
		CVSetBoolean(ret, false);
	} else {
		CVSetInteger(ret, boost::math::lcm(CVToInteger(&first), CVToInteger(&second)));
	}
}
void NewUUID(UDFContext* context, CLIPSValue* ret) {
	boost::uuids::random_generator rgen;
	boost::uuids::uuid theUUID(rgen());
	const std::string tmp = boost::lexical_cast<std::string>(theUUID);
	CVSetSymbol(ret, tmp.c_str());
}
void HasPrefix(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue data, prefix;
	if (!UDFFirstArgument(context, LEXEME_TYPES, &data)) {
		CVSetBoolean(ret, false);
		return;
	} else if (!UDFNextArgument(context, LEXEME_TYPES, &prefix)) {
		CVSetBoolean(ret, false);
		return;
	}
	std::string dataStr(CVToString(&data));
	std::string prefixStr(CVToString(&prefix));
	CVSetBoolean(ret, boost::starts_with(dataStr, prefixStr));
}

void HasSuffix(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue data, suffix;
	if (!UDFFirstArgument(context, LEXEME_TYPES, &data)) {
		CVSetBoolean(ret, false);
		return;
	} else if (!UDFNextArgument(context, LEXEME_TYPES, &suffix)) {
		CVSetBoolean(ret, false);
		return;
	}
	std::string dataStr(CVToString(&data));
	std::string suffixStr(CVToString(&suffix));
	CVSetBoolean(ret, boost::ends_with(dataStr, suffixStr));
}
void TrimString(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue str;
	if (!UDFFirstArgument(context, STRING_TYPE, &str)) {
		CVSetBoolean(ret, false);
	} else {
		std::string tmp(CVToString(&str));
		boost::algorithm::trim(tmp);
		CVSetString(ret, tmp.c_str());
	}
}
void TrimStringFront(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue str;
	if (!UDFFirstArgument(context, STRING_TYPE, &str)) {
		CVSetBoolean(ret, false);
	} else {
		std::string tmp(CVToString(&str));
		boost::algorithm::trim_left(tmp);
		CVSetString(ret, tmp.c_str());
	}
}
void TrimStringBack(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue str;
	if (!UDFFirstArgument(context, STRING_TYPE, &str)) {
		CVSetBoolean(ret, false);
	} else {
		std::string tmp(CVToString(&str));
		boost::algorithm::trim_right(tmp);
		CVSetString(ret, tmp.c_str());
	}
}
#endif



