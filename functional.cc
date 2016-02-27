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
#include "functional.h"
#include <functional>
#include <string>
#include <vector>

#if FUNCTIONAL_EXTENSIONS
void MapFunction(UDFContext* context, CLIPSValue* ret);
void NextToken(UDFContext* context, CLIPSValue* ret);
#endif


extern "C" void InstallFunctionalExtensions(void* theEnv) {
#if FUNCTIONAL_EXTENSIONS
	EnvAddUDF((Environment*)theEnv, "map$", "m", MapFunction, "MapFunction", 1, UNBOUNDED, "*;y;*", NULL);
    EnvAddUDF((Environment*)theEnv, "next-token", "synldfie", NextToken, "NextToken", 1, 1, "y", NULL);
#endif
}

#if FUNCTIONAL_EXTENSIONS
void
NextToken(UDFContext* context, CLIPSValue* ret) {
    struct token theToken;
    const char *logicalName = nullptr;
    Environment* theEnv = UDFContextEnvironment(context);
    logicalName = GetLogicalName(context, STDIN); // if we find a logical name then return it, else stdin
    if (!logicalName) {
        IllegalLogicalNameMessage(theEnv, "next-token");
        EnvSetHaltExecution(theEnv, true);
        EnvSetEvaluationError(theEnv, true);
        mCVSetString(ret, "*** READ ERROR ***");
        return;
    }

    if (!QueryRouters(theEnv, logicalName)) {
        UnrecognizedRouterMessage(theEnv, logicalName);
        EnvSetHaltExecution(theEnv, true);
        EnvSetEvaluationError(theEnv, true);
        mCVSetString(ret, "*** READ ERROR ***");
        return;
    }
    GetToken(theEnv, logicalName, &theToken);

    RouterData(theEnv)->CommandBufferInputCount = 0;
    RouterData(theEnv)->AwaitingInput = false;

    // copy the token tot he return value data structure
    ret->type = theToken.type;

    if ((theToken.type == FLOAT) || (theToken.type == STRING) ||
#if OBJECT_SYSTEM
        (theToken.type == INSTANCE_NAME) ||
#endif
        (theToken.type == SYMBOL) || (theToken.type == INTEGER)) {
        ret->value = theToken.value;
    } else if (theToken.type == UNKNOWN_VALUE) {
        mCVSetString(ret, "*** READ ERROR ***");
    } else if (theToken.type == STOP) {
        ret->type = SYMBOL;
        ret->value = (void*)EnvAddSymbol(theEnv, "EOF");
    }
#define specialCaseEntry(symbol, str) \
   else if(theToken.type == symbol) { \
       void *mf = EnvCreateMultifield(theEnv, 2); \
       SetMFType(mf, 1, SYMBOL); \
       SetMFValue(mf, 1, EnvAddSymbol(theEnv, str)); \
       SetMFType(mf, 2, STRING); \
       SetMFValue(mf, 2, EnvAddSymbol(theEnv, theToken.printForm)); \
       ret->type = MULTIFIELD; \
       ret->value = mf; \
       SetpDOBegin(ret, 1); \
       SetpDOEnd(ret, 2); \
   }
specialCaseEntry(NOT_CONSTRAINT, "NOT_CONSTRAINT")
specialCaseEntry(AND_CONSTRAINT, "AND_CONSTRAINT")
specialCaseEntry(OR_CONSTRAINT, "OR_CONSTRAINT")
specialCaseEntry(LPAREN, "LPAREN")
specialCaseEntry(RPAREN, "RPAREN")
specialCaseEntry(GBL_VARIABLE, "GBL_VARIABLE")
specialCaseEntry(MF_GBL_VARIABLE, "MF_GBL_VARIABLE")
specialCaseEntry(SF_VARIABLE, "SF_VARIABLE")
specialCaseEntry(MF_VARIABLE, "MF_VARIABLE")
specialCaseEntry(SF_WILDCARD, "SF_WILDCARD")
specialCaseEntry(MF_WILDCARD, "MF_WILDCARD")
#undef specialCaseEntry
    else {
        ret->type = STRING;
        ret->value = (void *) EnvAddSymbol(theEnv,theToken.printForm);
    }

}
void
MapFunction(UDFContext* context, CLIPSValue* ret) {
	std::vector<CLIPSValue> retStorage;
	int count = 0;
	std::string name;
	Environment* env= UDFContextEnvironment(context);
	struct expr *tmp2 = nullptr;
	CLIPSValue func, curr, tmp;
	if (!UDFFirstArgument(context, LEXEME_TYPES, &func)) {
		CVSetBoolean(ret, false);
		return;
	} else {
		name = CVToString(&func);
		std::function<bool(UDFContext*, CLIPSValue*, CLIPSValue*, const std::string&)> body =
			[](UDFContext* context, CLIPSValue* ret, CLIPSValue* theArg, const std::string& name) -> bool {
				int j;
				struct multifield *theMultifield;
				struct expr *lastAdd = NULL, *nextAdd, *multiAdd;
				struct FunctionDefinition *theFunction;
				Environment* theEnv = UDFContextEnvironment(context);
				FUNCTION_REFERENCE fref;
				if (!GetFunctionReference(theEnv, name.c_str(), &fref)) {
					ExpectedTypeError1(theEnv,"map$",1,"function, deffunction, or generic function name");
					return false;
				}

				if (fref.type == FCALL)
				{
					theFunction = FindFunction(theEnv,name.c_str());
					if (theFunction->parser != NULL)
					{
						ExpectedTypeError1(theEnv,"map$",1,"function without specialized parser");
						return false;
					}
				}
				ExpressionInstall(theEnv,&fref);

				switch(GetpType(theArg))
				{
					case MULTIFIELD:
						nextAdd = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"create$"));

						if (lastAdd == NULL)
						{ fref.argList = nextAdd; }
						else
						{ lastAdd->nextArg = nextAdd; }
						lastAdd = nextAdd;

						multiAdd = NULL;
						theMultifield = (struct multifield *) GetpValue(theArg);
						for (j = GetpDOBegin(theArg); j <= GetpDOEnd(theArg); j++)
						{
							nextAdd = GenConstant(theEnv,GetMFType(theMultifield,j),GetMFValue(theMultifield,j));
							if (multiAdd == NULL)
							{ lastAdd->argList = nextAdd; }
							else
							{ multiAdd->nextArg = nextAdd; }
							multiAdd = nextAdd;
						}

						ExpressionInstall(theEnv,lastAdd);
						break;

					default:
						nextAdd = GenConstant(theEnv,GetpType(theArg),GetpValue(theArg));
						if (lastAdd == NULL)
						{ fref.argList = nextAdd; }
						else
						{ lastAdd->nextArg = nextAdd; }
						lastAdd = nextAdd;
						ExpressionInstall(theEnv,lastAdd);
						break;
				}

				/*===========================================================*/
				/* Verify a deffunction has the correct number of arguments. */
				/*===========================================================*/

#if DEFFUNCTION_CONSTRUCT
				if (fref.type == PCALL)
				{
					if (CheckDeffunctionCall(theEnv,fref.value,CountArguments(fref.argList)) == false)
					{
						PrintErrorID(theEnv,"MISCFUN",4,false);
						EnvPrintRouter(theEnv,WERROR,"Function map$ called with the wrong number of arguments for deffunction ");
						EnvPrintRouter(theEnv,WERROR,EnvGetDeffunctionName(theEnv,fref.value));
						EnvPrintRouter(theEnv,WERROR,"\n");
						ExpressionDeinstall(theEnv,&fref);
						ReturnExpression(theEnv,fref.argList);
						return false;
					}
				}
#endif

				/*=========================================*/
				/* Verify the correct number of arguments. */
				/*=========================================*/

				if (fref.type == FCALL)
				{
					if (CheckExpressionAgainstRestrictions(theEnv,&fref,theFunction,name.c_str()))
					{
						ExpressionDeinstall(theEnv,&fref);
						ReturnExpression(theEnv,fref.argList);
						return false;
					}
				}

				/*======================*/
				/* Call the expression. */
				/*======================*/

				EvaluateExpression(theEnv,&fref,ret);

				/*========================================*/
				/* Return the expression data structures. */
				/*========================================*/

				ExpressionDeinstall(theEnv,&fref);
				ReturnExpression(theEnv,fref.argList);

				return true;
			};
		while (UDFHasNextArgument(context))
		{
			if (! UDFNextArgument(context,ANY_TYPE,&curr))
			{
				CVSetBoolean(ret, false);
				return;
			} else {
				if (body(context, &tmp, &curr, name)) {
					if (!tmp2) {
						tmp2 = ConvertValueToExpression(env, &tmp);
					} else {
						tmp2 = AppendExpressions(tmp2, ConvertValueToExpression(env, &tmp));
					}
				} else {
					return;
				}
			}
		}
		StoreInMultifield(env, ret, tmp2, true);
	}
}

#endif

