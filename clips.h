/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/17/19             */
/*                                                     */
/*                   API HEADER FILE                   */
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
/*      6.24: Added filertr.h and tmpltfun.h to include      */
/*            list.                                          */
/*                                                           */
/*      6.30: Added classpsr.h, iofun.h, and strngrtr.h to   */
/*            include list.                                  */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_CLIPS_API

#pragma once

#define _H_CLIPS_API

#include <stdio.h>

#include "setup.h"
#include "ArgumentAccess.h"
#include "Constants.h"
#include "MemoryAllocation.h"
#include "Construct.h"
#include "FileUtilities.h"
#include "StringFunctions.h"
#include "Environment.h"
#include "CommandLine.h"
#include "Symbol.h"

#include "PrintUtility.h"
#include "Router.h"
#include "FileRouter.h"
#include "StringRouter.h"

#include "IOFunctions.h"

#include "SystemDependency.h"
#include "BasicMathFunctions.h"
#include "Expression.h"
#include "Expression.h"
#include "Evaluation.h"
#include "MiscFunctions.h"
#include "Construct.h"
#include "Utility.h"
#include "Watch.h"
#include "modulbsc.h"

#if BLOAD || BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#endif

#if BLOAD_AND_BSAVE
#include "BinarySave.h"
#endif

#if DEFRULE_CONSTRUCT
#include "Defrule.h"
#include "BasicRuleCommands.h"
#include "Engine.h"
#include "Drive.h"
#include "incrrset.h"
#include "RuleCommands.h"
#include "ConflictResolutionStrategy.h"
#endif

#if DEFFACTS_CONSTRUCT
#include "dffctdef.h"
#include "dffctbsc.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "tmpltdef.h"
#include "tmpltbsc.h"
#include "tmpltfun.h"
#include "Fact.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#include "globlbsc.h"
#include "globlcom.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccom.h"
#include "genrcfun.h"
#endif

#if OBJECT_SYSTEM
#include "ClassCommands.h"
#include "ClassExamination.h"
#include "ClassFunctions.h"
#include "ClassInfo.h"
#include "ClassInitialization.h"
#include "ClassParser.h"
#include "Definstances.h"
#include "inscom.h"
#include "insfile.h"
#include "insfun.h"
#include "insmngr.h"
#include "msgcom.h"
#include "msgpass.h"
#include "ObjectReteMatch.h"
#endif

#endif /* _H_CLIPS_API */
