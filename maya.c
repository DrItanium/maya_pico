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
#include "miscfun.h"
#include "extnfunc.h"
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
static void EmptyFunction(Environment*, UDFContext*, UDFValue*);
//static void IsDeffunction(Environment*, UDFContext*, UDFValue*);
//static void NextToken(Environment*, UDFContext* context, UDFValue* ret);
static void LastFunction(Environment*, UDFContext* context, UDFValue* ret);

void InstallMayaExtensions(Environment* environment) {
	AddUDF(environment, "empty$", "b", 1, 1, "m", EmptyFunction, "EmptyFunction", NULL);
//	AddUDF(environment, "deffunctionp", "b", 1, 1, "y", IsDeffunction, "IsDeffunction", NULL);
	AddUDF(environment, "quit", "v", 0, 1, "l", ExitCommand, "ExitCommand", NULL);
	AddUDF(environment, "bye", "v", 0, 1, "l", ExitCommand, "ExitCommand", NULL);
//	AddUDF(environment, "next-token", "synldfie", 1, 1, "y", NextToken, "NextToken", NULL);
	AddUDF(environment, "last$", "m", 1, 1, "m", LastFunction, "LastFunction", NULL);
#if  BOOST_EXTENSIONS
//	InstallBoostExtensions(environment);
#endif
#if FUNCTIONAL_EXTENSIONS
//	InstallFunctionalExtensions(environment);
#endif
}

void
LastFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue theArg;
	Multifield* theList;

	/*===================================*/
	/* Get the segment to be subdivided. */
	/*===================================*/
	if (!UDFNthArgument(context, 1, MULTIFIELD_BIT, &theArg)) {
		return; 
	}

	theList = theArg.multifieldValue;

	ret->value = theList;
	if (theArg.range >= 1) {
		ret->begin = (theArg.begin + theArg.range) - 1;
		ret->range = 1;
	} else {
		ret->begin = theArg.begin;
		ret->range = theArg.range;
	}
}

//void
//IsDeffunction(Environment* env, UDFContext* context, CLIPSValue* ret) {
//	FUNCTION_REFERENCE theReference;
//	CLIPSValue func;
//	CVSetBoolean(ret, UDFFirstArgument(context, SYMBOL_TYPE, &func) && GetFunctionReference(UDFContextEnvironment(context), CVToString(&func), &theReference));
//}
void
EmptyFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	//CLIPSValue collection;
	//CVSetBoolean(ret, UDFFirstArgument(context, MULTIFIELD_TYPE, &collection) && (mMFLength(&collection) == 0));
	UDFValue theArg;
	if (!UDFFirstArgument(context, MULTIFIELD_BIT, &theArg)) {
		return;
	}
	ret->lexemeValue = (theArg.range > 0) ? FalseSymbol(env) : TrueSymbol(env);
}

//void
//NextToken(Environment* env, UDFContext* context, CLIPSValue* ret) {
//    struct token theToken;
//    const char *logicalName = 0;
//    Environment* theEnv = UDFContextEnvironment(context);
//    logicalName = GetLogicalName(context, STDIN); // if we find a logical name then return it, else stdin
//    if (!logicalName) {
//        IllegalLogicalNameMessage(theEnv, "next-token");
//        EnvSetHaltExecution(theEnv, true);
//        EnvSetEvaluationError(theEnv, true);
//        mCVSetString(ret, "*** READ ERROR ***");
//        return;
//    }
//
//    if (!QueryRouters(theEnv, logicalName)) {
//        UnrecognizedRouterMessage(theEnv, logicalName);
//        EnvSetHaltExecution(theEnv, true);
//        EnvSetEvaluationError(theEnv, true);
//        mCVSetString(ret, "*** READ ERROR ***");
//        return;
//    }
//    GetToken(theEnv, logicalName, &theToken);
//
//    RouterData(theEnv)->CommandBufferInputCount = 0;
//    RouterData(theEnv)->AwaitingInput = false;
//
//    // copy the token tot he return value data structure
//    ret->type = theToken.type;
//
//    if ((theToken.type == FLOAT) || (theToken.type == STRING) ||
//#if OBJECT_SYSTEM
//        (theToken.type == INSTANCE_NAME) ||
//#endif
//        (theToken.type == SYMBOL) || (theToken.type == INTEGER)) {
//        ret->value = theToken.value;
//    } else if (theToken.type == UNKNOWN_VALUE) {
//        mCVSetString(ret, "*** READ ERROR ***");
//	}
//#define specialCaseEntry(symbol, str) \
//   else if(theToken.type == symbol) { \
//       void *mf = EnvCreateMultifield(theEnv, 2); \
//       SetMFType(mf, 1, SYMBOL); \
//       SetMFValue(mf, 1, EnvAddSymbol(theEnv, str)); \
//       SetMFType(mf, 2, STRING); \
//       SetMFValue(mf, 2, EnvAddSymbol(theEnv, theToken.printForm)); \
//       ret->type = MULTIFIELD; \
//       ret->value = mf; \
//       SetpDOBegin(ret, 1); \
//       SetpDOEnd(ret, 2); \
//   }
//specialCaseEntry(STOP, "STOP")
//specialCaseEntry(NOT_CONSTRAINT, "NOT_CONSTRAINT")
//specialCaseEntry(AND_CONSTRAINT, "AND_CONSTRAINT")
//specialCaseEntry(OR_CONSTRAINT, "OR_CONSTRAINT")
//specialCaseEntry(LPAREN, "LPAREN")
//specialCaseEntry(RPAREN, "RPAREN")
//specialCaseEntry(GBL_VARIABLE, "GBL_VARIABLE")
//specialCaseEntry(MF_GBL_VARIABLE, "MF_GBL_VARIABLE")
//specialCaseEntry(SF_VARIABLE, "SF_VARIABLE")
//specialCaseEntry(MF_VARIABLE, "MF_VARIABLE")
//specialCaseEntry(SF_WILDCARD, "SF_WILDCARD")
//specialCaseEntry(MF_WILDCARD, "MF_WILDCARD")
//#undef specialCaseEntry
//    else {
//        ret->type = STRING;
//        ret->value = (void *) EnvAddSymbol(theEnv,theToken.printForm);
//    }
//
//}





#endif // end MAYA_EXTENSIONS
