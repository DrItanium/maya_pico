/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/08/18            */
/*                                                     */
/*            PREDICATE FUNCTIONS HEADER FILE          */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Deprecated the pointerp function and added     */
/*            the external-addressp function.                */
/*                                                           */
/*************************************************************/

#ifndef _H_prdctfun

#pragma once

#define _H_prdctfun

void PredicateFunctionDefinitions(Environment *);
void EqFunction(Environment *env, UDFContext *context, UDFValue *ret);
void NeqFunction(Environment *env, UDFContext *context, UDFValue *ret);
void StringpFunction(Environment *env, UDFContext *context, UDFValue *ret);
void SymbolpFunction(Environment *env, UDFContext *context, UDFValue *ret);
void LexemepFunction(Environment *env, UDFContext *context, UDFValue *ret);
void NumberpFunction(Environment *env, UDFContext *context, UDFValue *ret);
void FloatpFunction(Environment *env, UDFContext *context, UDFValue *ret);
void IntegerpFunction(Environment *env, UDFContext *context, UDFValue *ret);
void MultifieldpFunction(Environment *env, UDFContext *context, UDFValue *ret);
void ExternalAddresspFunction(Environment *env, UDFContext *context, UDFValue *ret);
void NotFunction(Environment *env, UDFContext *context, UDFValue *ret);
void AndFunction(Environment *env, UDFContext *context, UDFValue *ret);
void OrFunction(Environment *env, UDFContext *context, UDFValue *ret);
void LessThanOrEqualFunction(Environment *env, UDFContext *context, UDFValue *ret);
void GreaterThanOrEqualFunction(Environment *env, UDFContext *context, UDFValue *ret);
void LessThanFunction(Environment *env, UDFContext *context, UDFValue *ret);
void GreaterThanFunction(Environment *env, UDFContext *context, UDFValue *ret);
void NumericEqualFunction(Environment *env, UDFContext *context, UDFValue *ret);
void NumericNotEqualFunction(Environment *env, UDFContext *context, UDFValue *ret);
void OddpFunction(Environment *env, UDFContext *context, UDFValue *ret);
void EvenpFunction(Environment *env, UDFContext *context, UDFValue *ret);

#endif /* _H_prdctfun */



