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
#include "taglib_interface.h"
#include <taglib/fileref.h>
#include <taglib/tag.h>
#include <taglib/tpropertymap.h>
#include <string>

#if !TAGLIB_EXTENSIONS
extern "C" void InstallTagLibMethods(Environment* env) { }
#else
void GetBasicTagInformation(Environment*, UDFContext*, UDFValue*);
void GetTagProperties(Environment*, UDFContext*, UDFValue*);
void GetAudioProperties(Environment*, UDFContext*, UDFValue*);
extern "C" void InstallTagLibMethods(Environment* env) { 
	AddUDF(theEnv, "get-basic-tag-info", "m", 1, 1, "sy", GetBasicTagInformation, "GetBasicTagInformation",  NULL);
}

void GetBasicTagInformation(Environment* env, UDFContext* context, UDFValue* retValue) {
	UDFValue theArg;
	if (! UDFFirstArgument(context, LEXEME_BITS, &theArg)) {
		return;
	}
	std::string path(theArg.lexemeValue->contents);
	TagLib::FileRef f(path.c_str());
	if (!f.isNull() && f.tag()) {
		auto* mb = CreateMultifieldBuilder(env, 10);
		auto* tag = f.tag();
		MBAppendString(mb, tag->title().c_str());
		MBAppendString(mb, tag->artist().c_str());
		MBAppendString(mb, tag->album().c_str());
		MBAppendInteger(mb, tag->year());
		MBAppendInteger(mb, tag->track());
		MBAppendString(mb, tag->genre().c_str());
		retValue->multifieldValue = MBCreate(mb);
		MBDispose(mb);
	} else {
		retValue->multifieldValue = EmptyMultifield(env);
	}
}

#endif
