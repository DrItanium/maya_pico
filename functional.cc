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

#if FUNCTIONAL_EXTENSIONS
static void MapFunction(Environment* env, UDFContext* context, UDFValue* ret);
//static void FilterFunction(Environment* env, UDFContext* context, UDFValue* ret);
//static void ExistsFunction(Environment* env, UDFContext* context, UDFValue* ret);
//static void NotExistsFunction(Environment* env, UDFContext* context, UDFValue* ret);
#endif



extern "C" void InstallFunctionalExtensions(Environment* theEnv) {
#if FUNCTIONAL_EXTENSIONS
	AddUDF(theEnv, "map$", "m", 1, UNBOUNDED, "*;y;*", MapFunction, "MapFunction", nullptr);
	//AddUDF(theEnv, "filter", "m", 1. UNBOUNDED, "*;y;*", FilterFunction, "FilterFunction", nullptr);
	//AddUDF(theEnv, "exists", "m", 1. UNBOUNDED, "*;y;*", ExistsFunction, "ExistsFunction", nullptr);
	//AddUDF(theEnv, "not-exists", "m", 1, UNBOUNDED, "*;y;*", NotExistsFunction, "NotExistsFunction", nullptr);
#endif
}

#if FUNCTIONAL_EXTENSIONS
void
MapFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue func, curr;
	if (!UDFFirstArgument(context, LEXEME_BITS, &func)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	} else {
		maya::MultifieldBuilder mb(env);
		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE_BITS,&curr)) {
				ret->lexemeValue = FalseSymbol(env);
				return;
			} else {
				CLIPSValue tmp;
				maya::FunctionCallBuilder fcb(env);
				fcb.append(&curr);
				fcb.call(func.lexemeValue->contents, &tmp);
				mb.append(&tmp);
			}
		}
		ret->multifieldValue = mb.create();
	}
}
//
//void
//FilterFunction(Environment* env, UDFContext* context, UDFValue* ret) {
//	UDFValue func;
//	if (!UDFFirstArgument(context, LEXEME_BITS, &func)) {
//		ret->lexemeValue = FalseSymbol(env);
//		return;
//	} else {
//		auto body = [](Environment* env, UDFContext* context, UDFValue* ret, UDFValue* theArg, const std::string& name, Expression* fref, FunctionDefinition *theFunction) -> bool {
//			struct multifield *theMultifield = nullptr;
//			struct expr *lastAdd = nullptr, 
//						*nextAdd = nullptr, 
//						*multiAdd = nullptr;
//			Environment* theEnv = UDFContextEnvironment(context);
//			ExpressionInstall(theEnv,fref);
//
//			switch(GetpType(theArg)) {
//				case MULTIFIELD:
//					nextAdd = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"create$"));
//
//					if (lastAdd == NULL) { 
//						fref->argList = nextAdd; 
//					} else { 
//						lastAdd->nextArg = nextAdd; 
//					}
//					lastAdd = nextAdd;
//
//					multiAdd = NULL;
//					theMultifield = (struct multifield *) GetpValue(theArg);
//					for (int j = GetpDOBegin(theArg); j <= GetpDOEnd(theArg); j++) {
//						nextAdd = GenConstant(theEnv,GetMFType(theMultifield,j),GetMFValue(theMultifield,j));
//						if (multiAdd == NULL) {
//							lastAdd->argList = nextAdd;
//						} else {
//							multiAdd->nextArg = nextAdd;
//						}
//						multiAdd = nextAdd;
//					}
//
//					ExpressionInstall(theEnv,lastAdd);
//					break;
//
//				default:
//					nextAdd = GenConstant(theEnv,GetpType(theArg),GetpValue(theArg));
//					if (lastAdd == NULL) { 
//						fref->argList = nextAdd; 
//					} else { 
//						lastAdd->nextArg = nextAdd; 
//					}
//					lastAdd = nextAdd;
//					ExpressionInstall(theEnv,lastAdd);
//					break;
//			}
//
//			/*===========================================================*/
//			/* Verify a deffunction has the correct number of arguments. */
//			/*===========================================================*/
//
//#if DEFFUNCTION_CONSTRUCT
//			if (fref->type == PCALL) {
//				if (!CheckDeffunctionCall(theEnv,fref->value,CountArguments(fref->argList))) {
//					PrintErrorID(theEnv,"MISCFUN",4,false);
//					WriteString(theEnv,STDERR,"Function filter called with the wrong number of arguments for deffunction ");
//					WriteString(theEnv,STDERR,EnvGetDeffunctionName(theEnv,fref->value));
//					WriteString(theEnv,STDERR,"\n");
//					ExpressionDeinstall(theEnv,fref);
//					ReturnExpression(theEnv,fref->argList);
//					return false;
//				}
//			}
//#endif
//
//			/*=========================================*/
//			/* Verify the correct number of arguments. */
//			/*=========================================*/
//
//			if (fref->type == FCALL) {
//				if (CheckExpressionAgainstRestrictions(theEnv,fref,theFunction,name.c_str())) {
//					ExpressionDeinstall(theEnv,fref);
//					ReturnExpression(theEnv,fref->argList);
//					return false;
//				}
//			}
//
//			/*======================*/
//			/* Call the expression. */
//			/*======================*/
//
//			EvaluateExpression(theEnv,fref,ret);
//
//			/*========================================*/
//			/* Return the expression data structures. */
//			/*========================================*/
//
//			ExpressionDeinstall(theEnv,fref);
//			ReturnExpression(theEnv,fref->argList);
//			fref->argList = nullptr;
//
//			return true;
//		};
//		std::string name(CVToString(&func));
//		Environment* env = UDFContextEnvironment(context);
//		struct expr *tmp2 = nullptr;
//		FunctionDefinition *theFunction = nullptr;
//		UDFValue curr, tmp;
//		Expression fref;
//
//		if (!GetFunctionReference(env, name.c_str(), &fref)) {
//			ExpectedTypeError1(env,"filter",1,"function, deffunction, or generic function name");
//			return;
//		}
//
//		if (fref.type == FCALL) {
//			theFunction = FindFunction(env, name.c_str());
//			if (theFunction->parser != NULL) {
//				ExpectedTypeError1(env,"filter",1,"function without specialized parser");
//				return;
//			}
//		}
//
//		while (UDFHasNextArgument(context)) {
//			if (! UDFNextArgument(context,ANY_TYPE,&curr)) {
//				ret->lexemeValue = FalseSymbol(env);
//				return;
//			} else {
//				if (body(context, &tmp, &curr, name, &fref, theFunction)) {
//					if (!mCVIsFalseSymbol(&tmp)) {
//						if (!tmp2) {
//							tmp2 = ConvertValueToExpression(env, &curr);
//						} else {
//							tmp2 = AppendExpressions(tmp2, ConvertValueToExpression(env, &curr));
//						}
//					}
//				} else {
//					return;
//				}
//			}
//		}
//		StoreInMultifield(env, ret, tmp2, true);
//	}
//}
//
//void
//ExistsFunction(Environment* env, UDFContext* context, UDFValue* ret) {
//	UDFValue func;
//	if (!UDFFirstArgument(context, LEXEME_BITS, &func)) {
//		ret->lexemeValue = FalseSymbol(env);
//		return;
//	} else {
//		auto body = [](Environment* env, UDFContext* context, UDFValue* ret, UDFValue* theArg, const std::string& name, Expression* fref, FunctionDefinition *theFunction) -> bool {
//			struct multifield *theMultifield = nullptr;
//			struct expr *lastAdd = nullptr, 
//						*nextAdd = nullptr, 
//						*multiAdd = nullptr;
//			Environment* theEnv = UDFContextEnvironment(context);
//			ExpressionInstall(theEnv,fref);
//
//			switch(GetpType(theArg)) {
//				case MULTIFIELD:
//					nextAdd = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"create$"));
//
//					if (lastAdd == NULL) { 
//						fref->argList = nextAdd; 
//					} else { 
//						lastAdd->nextArg = nextAdd; 
//					}
//					lastAdd = nextAdd;
//
//					multiAdd = NULL;
//					theMultifield = (struct multifield *) GetpValue(theArg);
//					for (int j = GetpDOBegin(theArg); j <= GetpDOEnd(theArg); j++) {
//						nextAdd = GenConstant(theEnv,GetMFType(theMultifield,j),GetMFValue(theMultifield,j));
//						if (multiAdd == NULL) {
//							lastAdd->argList = nextAdd;
//						} else {
//							multiAdd->nextArg = nextAdd;
//						}
//						multiAdd = nextAdd;
//					}
//
//					ExpressionInstall(theEnv,lastAdd);
//					break;
//
//				default:
//					nextAdd = GenConstant(theEnv,GetpType(theArg),GetpValue(theArg));
//					if (lastAdd == NULL) { 
//						fref->argList = nextAdd; 
//					} else { 
//						lastAdd->nextArg = nextAdd; 
//					}
//					lastAdd = nextAdd;
//					ExpressionInstall(theEnv,lastAdd);
//					break;
//			}
//
//			/*===========================================================*/
//			/* Verify a deffunction has the correct number of arguments. */
//			/*===========================================================*/
//
//#if DEFFUNCTION_CONSTRUCT
//			if (fref->type == PCALL) {
//				if (!CheckDeffunctionCall(theEnv,fref->value,CountArguments(fref->argList))) {
//					PrintErrorID(theEnv,"MISCFUN",4,false);
//					WriteString(theEnv,STDERR,"Function exists called with the wrong number of arguments for deffunction ");
//					WriteString(theEnv,STDERR,EnvGetDeffunctionName(theEnv,fref->value));
//					WriteString(theEnv,STDERR,"\n");
//					ExpressionDeinstall(theEnv,fref);
//					ReturnExpression(theEnv,fref->argList);
//					return false;
//				}
//			}
//#endif
//
//			/*=========================================*/
//			/* Verify the correct number of arguments. */
//			/*=========================================*/
//
//			if (fref->type == FCALL) {
//				if (CheckExpressionAgainstRestrictions(theEnv,fref,theFunction,name.c_str())) {
//					ExpressionDeinstall(theEnv,fref);
//					ReturnExpression(theEnv,fref->argList);
//					return false;
//				}
//			}
//
//			/*======================*/
//			/* Call the expression. */
//			/*======================*/
//
//			EvaluateExpression(theEnv,fref,ret);
//
//			/*========================================*/
//			/* Return the expression data structures. */
//			/*========================================*/
//
//			ExpressionDeinstall(theEnv,fref);
//			ReturnExpression(theEnv,fref->argList);
//			fref->argList = nullptr;
//
//			return true;
//		};
//		std::string name(CVToString(&func));
//		Environment* env = UDFContextEnvironment(context);
//		struct expr *tmp2 = nullptr;
//		FunctionDefinition *theFunction = nullptr;
//		UDFValue curr, tmp;
//		Expression fref;
//
//		if (!GetFunctionReference(env, name.c_str(), &fref)) {
//			ExpectedTypeError1(env,"exists",1,"function, deffunction, or generic function name");
//			return;
//		}
//
//		if (fref.type == FCALL) {
//			theFunction = FindFunction(env, name.c_str());
//			if (theFunction->parser != NULL) {
//				ExpectedTypeError1(env,"exists",1,"function without specialized parser");
//				return;
//			}
//		}
//
//		while (UDFHasNextArgument(context)) {
//			if (! UDFNextArgument(context,ANY_TYPE,&curr)) {
//				ret->lexemeValue = FalseSymbol(env);
//				return;
//			} else {
//				if (body(context, &tmp, &curr, name, &fref, theFunction)) {
//					if (!mCVIsFalseSymbol(&tmp)) {
//						ret->lexemeValue = TrueSymbol(env);
//						return;
//					}
//				} else {
//					return;
//				}
//			}
//		}
//		ret->lexemeValue = FalseSymbol(env);
//	}
//}
//
//void
//NotExistsFunction(Environment* env, UDFContext* context, UDFValue* ret) {
//	UDFValue func;
//	if (!UDFFirstArgument(context, LEXEME_BITS, &func)) {
//		ret->lexemeValue = FalseSymbol(env);
//		return;
//	} else {
//		auto body = [](Environment* theEnv, UDFContext* context, UDFValue* ret, UDFValue* theArg, const std::string& name, Expression* fref, FunctionDefinition *theFunction) -> bool {
//			struct multifield *theMultifield = nullptr;
//			struct expr *lastAdd = nullptr, 
//						*nextAdd = nullptr, 
//						*multiAdd = nullptr;
//			ExpressionInstall(theEnv,fref);
//			switch(theArg->header->type) {
//				case MULTIFIELD_TYPE:
//					nextAdd = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"create$"));
//
//					if (lastAdd == NULL) { 
//						fref->argList = nextAdd; 
//					} else { 
//						lastAdd->nextArg = nextAdd; 
//					}
//					lastAdd = nextAdd;
//
//					multiAdd = NULL;
//					theMultifield = (struct multifield *) GetpValue(theArg);
//					for (int j = GetpDOBegin(theArg); j <= GetpDOEnd(theArg); j++) {
//						nextAdd = GenConstant(theEnv,GetMFType(theMultifield,j),GetMFValue(theMultifield,j));
//						if (multiAdd == NULL) {
//							lastAdd->argList = nextAdd;
//						} else {
//							multiAdd->nextArg = nextAdd;
//						}
//						multiAdd = nextAdd;
//					}
//
//					ExpressionInstall(theEnv,lastAdd);
//					break;
//
//				default:
//					nextAdd = GenConstant(theEnv,GetpType(theArg),GetpValue(theArg));
//					if (lastAdd == NULL) { 
//						fref->argList = nextAdd; 
//					} else { 
//						lastAdd->nextArg = nextAdd; 
//					}
//					lastAdd = nextAdd;
//					ExpressionInstall(theEnv,lastAdd);
//					break;
//			}
//
//			/*===========================================================*/
//			/* Verify a deffunction has the correct number of arguments. */
//			/*===========================================================*/
//
//#if DEFFUNCTION_CONSTRUCT
//			if (fref->type == PCALL) {
//				if (!CheckDeffunctionCall(theEnv,fref->value,CountArguments(fref->argList))) {
//					PrintErrorID(theEnv,"MISCFUN",4,false);
//					WriteString(theEnv,STDERR,"Function not-exists called with the wrong number of arguments for deffunction ");
//					WriteString(theEnv,STDERR, DeffunctionName(fref->value));
//					WriteString(theEnv,STDERR,"\n");
//					ExpressionDeinstall(theEnv,fref);
//					ReturnExpression(theEnv,fref->argList);
//					return false;
//				}
//			}
//#endif
//
//			/*=========================================*/
//			/* Verify the correct number of arguments. */
//			/*=========================================*/
//
//			if (fref->type == FCALL) {
//				if (CheckExpressionAgainstRestrictions(theEnv,fref,theFunction,name.c_str())) {
//					ExpressionDeinstall(theEnv,fref);
//					ReturnExpression(theEnv,fref->argList);
//					return false;
//				}
//			}
//
//			/*======================*/
//			/* Call the expression. */
//			/*======================*/
//
//			EvaluateExpression(theEnv,fref,ret);
//
//			/*========================================*/
//			/* Return the expression data structures. */
//			/*========================================*/
//
//			ExpressionDeinstall(theEnv,fref);
//			ReturnExpression(theEnv,fref->argList);
//			fref->argList = nullptr;
//
//			return true;
//		};
//		std::string name(func.lexemeValue->contents);
//		FunctionDefinition *theFunction = nullptr;
//		UDFValue curr, tmp;
//		Expression fref;
//
//		if (!GetFunctionReference(env, name.c_str(), &fref)) {
//			ExpectedTypeError1(env,"not-exists",1,"function, deffunction, or generic function name");
//			return;
//		}
//
//		if (fref.type == FCALL) {
//			theFunction = FindFunction(env, name.c_str());
//			if (theFunction->parser != NULL) {
//				ExpectedTypeError1(env,"not-exists",1,"function without specialized parser");
//				return;
//			}
//		}
//
//		while (UDFHasNextArgument(context)) {
//			if (! UDFNextArgument(context,ANY_TYPE_BITS,&curr)) {
//				ret->lexemeValue = FalseSymbol(env);
//				return;
//			} else {
//				if (body(env, context, &tmp, &curr, name, &fref, theFunction)) {
//					if (tmp.value != FalseSymbol(env)) {
//						ret->lexemeValue = FalseSymbol(env);
//						return;
//					}
//				} else {
//					return;
//				}
//			}
//		}
//		ret->lexemeValue = TrueSymbol(env);
//	}
//}

namespace maya {
	FunctionCallBuilder::FunctionCallBuilder(Environment* theEnv, size_t size) : _builder(CreateFunctionCallBuilder(theEnv, size)) { }
	FunctionCallBuilder::~FunctionCallBuilder() {
		FCBDispose(_builder);
		_builder = nullptr;
	}
	FunctionCallBuilder::ErrorKind FunctionCallBuilder::call(const std::string& functionName, CLIPSValue* ret) noexcept {
		return FCBCall(_builder, functionName.c_str(), ret);
	}
	void FunctionCallBuilder::append(UDFValue* value) noexcept { FCBAppendUDFValue(_builder, value); }
	void FunctionCallBuilder::append(CLIPSValue* value) noexcept { FCBAppend(_builder, value); }
	void FunctionCallBuilder::append(CLIPSInteger* value) noexcept { FCBAppendCLIPSInteger(_builder, value); }
	void FunctionCallBuilder::append(int64_t value) noexcept { FCBAppendInteger(_builder, value); }
	void FunctionCallBuilder::append(CLIPSFloat* value) noexcept { FCBAppendCLIPSFloat(_builder, value); }
	void FunctionCallBuilder::append(double value) noexcept { FCBAppendFloat(_builder, value); }
	void FunctionCallBuilder::append(CLIPSLexeme* value) noexcept { FCBAppendCLIPSLexeme(_builder, value); }
	void FunctionCallBuilder::append(CLIPSExternalAddress* value) noexcept { FCBAppendCLIPSExternalAddress(_builder, value); }
	void FunctionCallBuilder::append(Fact* value) noexcept { FCBAppendFact(_builder, value); }
	void FunctionCallBuilder::append(Instance* value) noexcept { FCBAppendInstance(_builder, value); }
	void FunctionCallBuilder::append(Multifield* value) noexcept { FCBAppendMultifield(_builder, value); }
	void FunctionCallBuilder::appendSymbol(const std::string& sym) noexcept { FCBAppendSymbol(_builder, sym.c_str()); }
	void FunctionCallBuilder::appendString(const std::string& sym) noexcept { FCBAppendString(_builder, sym.c_str()); }
	void FunctionCallBuilder::appendInstanceName(const std::string& sym) noexcept { FCBAppendInstanceName(_builder, sym.c_str()); }
	MultifieldBuilder::MultifieldBuilder(Environment* theEnv, size_t size) : _builder(CreateMultifieldBuilder(theEnv, size)) { }
	MultifieldBuilder::~MultifieldBuilder() { MBDispose(_builder); }
	void MultifieldBuilder::append(UDFValue* value) noexcept { MBAppendUDFValue(_builder, value); }
	void MultifieldBuilder::append(CLIPSValue* value) noexcept { MBAppend(_builder, value); }
	void MultifieldBuilder::append(CLIPSInteger* value) noexcept { MBAppendCLIPSInteger(_builder, value); }
	void MultifieldBuilder::append(CLIPSFloat* value) noexcept { MBAppendCLIPSFloat(_builder, value); }
	void MultifieldBuilder::append(CLIPSLexeme* value) noexcept { MBAppendCLIPSLexeme(_builder, value); }
	void MultifieldBuilder::append(CLIPSExternalAddress* value) noexcept { MBAppendCLIPSExternalAddress(_builder, value); }
	void MultifieldBuilder::append(Fact* value) noexcept { MBAppendFact(_builder, value); }
	void MultifieldBuilder::append(Instance* value) noexcept { MBAppendInstance(_builder, value); }
	void MultifieldBuilder::append(Multifield* value) noexcept { MBAppendMultifield(_builder, value); }
	void MultifieldBuilder::append(int64_t value) noexcept { MBAppendInteger(_builder, value); }
	void MultifieldBuilder::append(double value) noexcept { MBAppendFloat(_builder, value); }
	void MultifieldBuilder::appendSymbol(const std::string& value) noexcept { MBAppendSymbol(_builder, value.c_str()); }
	void MultifieldBuilder::appendString(const std::string& value) noexcept { MBAppendString(_builder, value.c_str()); }
	void MultifieldBuilder::appendInstanceName(const std::string& value) noexcept { MBAppendInstanceName(_builder, value.c_str()); }
} // end namespace maya
#endif

