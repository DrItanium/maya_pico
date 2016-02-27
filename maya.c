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
#include "clips.h"
#include "maya.h"
#include "mayasetup.h"

#if BOOST_EXTENSIONS
#include "boost.h"
#endif 

#if FUNCTIONAL_EXTENSIONS
#include "functional.h"
#endif

#if !MAYA_EXTENSIONS
void InstallMayaExtensions(void* environment) { }
#else
static void EmptyFunction(UDFContext*, CLIPSValue*);
static void IsDeffunction(UDFContext*, CLIPSValue*);

void InstallMayaExtensions(void* environment) {
	EnvAddUDF(environment, "empty$", "b", EmptyFunction, "EmptyFunction", 1, 1, "m", NULL);
	EnvAddUDF(environment, "deffunctionp", "b", IsDeffunction, "IsDeffunction", 1, 1, "y", NULL);
	EnvAddUDF(environment, "quit",   "v", ExitCommand,    "ExitCommand", 0,1,"l",NULL);
	EnvAddUDF(environment, "bye",   "v", ExitCommand,    "ExitCommand", 0,1,"l",NULL);
#if  BOOST_EXTENSIONS
	InstallBoostExtensions(environment);
#endif
#if FUNCTIONAL_EXTENSIONS
	InstallFunctionalExtensions(environment);
#endif
}

void
IsDeffunction(UDFContext* context, CLIPSValue* ret) {
	FUNCTION_REFERENCE theReference;
	CLIPSValue func;
	Environment* environment = UDFContextEnvironment(context);

	if (!UDFFirstArgument(context, SYMBOL_TYPE, &func)) {
		CVSetBoolean(ret, false);
		return;
	}
	if (! GetFunctionReference(environment, CVToString(&func), &theReference)) {
		CVSetBoolean(ret, false);
	} else {
		CVSetBoolean(ret, true);
	}
}
void
EmptyFunction(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue collection;
	if (!UDFFirstArgument(context, MULTIFIELD_TYPE, &collection)) {
		return;
	}
	if (mMFLength(&collection) == 0) {
		CVSetBoolean(ret, true);
	} else {
		CVSetBoolean(ret, false);
	}
}





#endif // end MAYA_EXTENSIONS
