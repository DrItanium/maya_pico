   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  11/01/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when DEBUGGING_FUNCTIONS   */
/*            is set to 0 and PROFILING_FUNCTIONS is set to  */
/*            1.                                             */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_genrccom

#pragma once

#define _H_genrccom

#include "Entities.h"
#include "constrct.h"
#include "genrcfun.h"

   void                           SetupGenericFunctions(Environment *);
   Defgeneric                    *FindDefgeneric(Environment *,const char *);
   Defgeneric                    *FindDefgenericInModule(Environment *,const char *);
   Defgeneric                    *LookupDefgenericByMdlOrScope(Environment *,const char *);
   Defgeneric                    *LookupDefgenericInScope(Environment *,const char *);
   Defgeneric                    *GetNextDefgeneric(Environment *,Defgeneric *);
   unsigned short                 GetNextDefmethod(Defgeneric *,unsigned short);
   bool                           DefgenericIsDeletable(Defgeneric *);
   bool                           DefmethodIsDeletable(Defgeneric *,unsigned short);
   void                           UndefgenericCommand(Environment *,UDFContext *,UDFValue *);
   void                           GetDefgenericModuleCommand(Environment *,UDFContext *,UDFValue *);
   void                           UndefmethodCommand(Environment *,UDFContext *,UDFValue *);
   Defmethod                     *GetDefmethodPointer(Defgeneric *,long);
   bool                           Undefgeneric(Defgeneric *,Environment *);
   bool                           Undefmethod(Defgeneric *,unsigned short,Environment *);
#if ! OBJECT_SYSTEM
   void                           TypeCommand(Environment *,UDFContext *,UDFValue *);
#endif
#if DEBUGGING_FUNCTIONS || PROFILING_FUNCTIONS
   void                           DefmethodDescription(Defgeneric *,unsigned short,StringBuilder *);
#endif
#if DEBUGGING_FUNCTIONS
   bool                           DefgenericGetWatch(Defgeneric *);
   void                           DefgenericSetWatch(Defgeneric *,bool);
   bool                           DefmethodGetWatch(Defgeneric *,unsigned short);
   void                           DefmethodSetWatch(Defgeneric *,unsigned short,bool);
   void                           PPDefgenericCommand(Environment *,UDFContext *,UDFValue *);
   void                           PPDefmethodCommand(Environment *,UDFContext *,UDFValue *);
   void                           ListDefmethodsCommand(Environment *,UDFContext *,UDFValue *);
   const char                    *DefmethodPPForm(Defgeneric *,unsigned short);
   void                           ListDefgenericsCommand(Environment *,UDFContext *,UDFValue *);
   void                           ListDefgenerics(Environment *,const char *,Defmodule *);
   void                           ListDefmethods(Environment *,const char *,Defgeneric *);
#endif
   void                           GetDefgenericListFunction(Environment *,UDFContext *,UDFValue *);
   void                           GetDefgenericList(Environment *,CLIPSValue *,Defmodule *);
   void                           GetDefmethodListCommand(Environment *,UDFContext *,UDFValue *);
   void                           GetDefmethodList(Environment *,CLIPSValue *,Defgeneric *);
   void                           GetMethodRestrictionsCommand(Environment *,UDFContext *,UDFValue *);
   void                           GetMethodRestrictions(Defgeneric *,unsigned short,CLIPSValue *);
   void                           SetNextDefgeneric(Defgeneric *,Defgeneric *);
   const char                    *DefgenericModule(Defgeneric *);
   const char                    *DefgenericName(Defgeneric *);
   const char                    *DefgenericPPForm(Defgeneric *);
   CLIPSLexeme                   *GetDefgenericNamePointer(Defgeneric *);
   void                           SetDefgenericPPForm(Environment *,Defgeneric *,const char *);

#endif /* _H_genrccom */





