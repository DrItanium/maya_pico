   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  07/25/14            */
   /*                                                     */
   /*                 ENGINE HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functionality primarily associated with */
/*   the run and focus commands.                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Removed INCREMENTAL_RESET and                  */
/*            LOGICAL_DEPENDENCIES compilation flag.         */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added access functions to the HaltRules flag.  */
/*                                                           */
/*            Added EnvGetNextFocus, EnvGetFocusChanged, and */
/*            EnvSetFocusChanged functions.                  */
/*                                                           */
/*      6.30: Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Added context information for run functions.   */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*************************************************************/

#ifndef _H_engine

#define _H_engine

#ifndef _H_lgcldpnd
#include "lgcldpnd.h"
#endif
#ifndef _H_ruledef
#include "ruledef.h"
#endif
#ifndef _H_network
#include "network.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_retract
#include "retract.h"
#endif

struct focus
  {
   struct defmodule *theModule;
   struct defruleModule *theDefruleModule;
   struct focus *next;
  };
  
#define ENGINE_DATA 18

struct engineData
  { 
   struct defrule *ExecutingRule;
   intBool HaltRules;
   struct joinNode *TheLogicalJoin;
   struct partialMatch *TheLogicalBind;
   struct dependency *UnsupportedDataEntities;
   int alreadyEntered;
   struct callFunctionItem *ListOfRunFunctions;
   struct callFunctionItemWithArg* ListOfBeforeRunFunctions;
   struct focus *CurrentFocus;
   int FocusChanged;
#if DEBUGGING_FUNCTIONS
   unsigned WatchStatistics;
   unsigned WatchFocus;
#endif
   intBool IncrementalResetInProgress;
   intBool IncrementalResetFlag;
   intBool JoinOperationInProgress;
   struct partialMatch *GlobalLHSBinds;
   struct partialMatch *GlobalRHSBinds;
   struct joinNode *GlobalJoin;
   struct partialMatch *GarbagePartialMatches;
   struct alphaMatch *GarbageAlphaMatches;
   int AlreadyRunning;
#if DEVELOPER
   long leftToRightComparisons;
   long rightToLeftComparisons;
   long leftToRightSucceeds;
   long rightToLeftSucceeds;
   long leftToRightLoops;
   long rightToLeftLoops;
   long findNextConflictingComparisons;
   long betaHashHTSkips;
   long betaHashListSkips;
   long unneededMarkerCompare;
#endif
  };

#define EngineData(theEnv) ((struct engineData *) GetEnvironmentData(theEnv,ENGINE_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _ENGINE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

/**************************************************************/
/* The GetFocus function is remapped under certain conditions */
/* because it conflicts with a Windows 3.1 function.          */
/**************************************************************/
/*
#if ! ((GENERIC || IBM) && WINDOW_INTERFACE)
#define WRGetFocus GetFocus
#endif
*/
#define MAX_PATTERNS_CHECKED 64

#define ClearFocusStack() EnvClearFocusStack(GetCurrentEnvironment())
#define DefruleHasBreakpoint(a) EnvDefruleHasBreakpoint(GetCurrentEnvironment(),a)
#define Focus(a) EnvFocus(GetCurrentEnvironment(),a)
#define GetFocus() EnvGetFocus(GetCurrentEnvironment())
#define GetFocusChanged() EnvGetFocusChanged(GetCurrentEnvironment())
#define GetFocusStack(a) EnvGetFocusStack(GetCurrentEnvironment(),a)
#define GetNextFocus(a) EnvGetNextFocus(GetCurrentEnvironment(),a)
#define Halt() EnvHalt(GetCurrentEnvironment())
#define ListFocusStack(a) EnvListFocusStack(GetCurrentEnvironment(),a)
#define PopFocus() EnvPopFocus(GetCurrentEnvironment())
#define RemoveBreak(a) EnvRemoveBreak(GetCurrentEnvironment(),a)
#define RemoveRunFunction(a) EnvRemoveRunFunction(GetCurrentEnvironment(),a)
#define SetBreak(a) EnvSetBreak(GetCurrentEnvironment(),a)
#define SetFocusChanged(a) EnvSetFocusChanged(GetCurrentEnvironment(),a)
#define ShowBreaks(a,b) EnvShowBreaks(GetCurrentEnvironment(),a,b)

#if ALLOW_ENVIRONMENT_GLOBALS
   LOCALE long long               Run(long long);
#endif

   LOCALE long long               EnvRun(void *,long long);
   LOCALE intBool                 EnvAddRunFunction(void *,const char *,
                                                    void (*)(void *),int);
   LOCALE intBool                 EnvAddRunFunctionWithContext(void *,const char *,
                                                               void (*)(void *),int,void *);
   LOCALE intBool                 AddRunFunction(const char *,void (*)(void),int);
   LOCALE intBool                 EnvRemoveRunFunction(void *,const char *);
   LOCALE intBool                 EnvAddBeforeRunFunction(void *,const char *,
                                                    void (*)(void *,void *),int);
   LOCALE intBool                 EnvAddBeforeRunFunctionWithContext(void *,const char *,
                                                               void (*)(void *, void *),int,void *);
   LOCALE intBool                 AddBeforeRunFunction(const char *,void (*)(void *),int);
   LOCALE intBool                 EnvRemoveBeforeRunFunction(void *,const char *);
   LOCALE void                    InitializeEngine(void *);
   LOCALE void                    EnvSetBreak(void *,void *);
   LOCALE void                    EnvHalt(void *);
   LOCALE intBool                 EnvRemoveBreak(void *,void *);
   LOCALE void                    RemoveAllBreakpoints(void *);
   LOCALE void                    EnvShowBreaks(void *,const char *,void *);
   LOCALE intBool                 EnvDefruleHasBreakpoint(void *,void *);
   LOCALE void                    RunCommand(void *);
   LOCALE void                    SetBreakCommand(void *);
   LOCALE void                    RemoveBreakCommand(void *);
   LOCALE void                    ShowBreaksCommand(void *);
   LOCALE void                    HaltCommand(void *);
   LOCALE int                     FocusCommand(void *);
   LOCALE void                    ClearFocusStackCommand(void *);
   LOCALE void                    EnvClearFocusStack(void *);
   LOCALE void                   *EnvGetNextFocus(void *,void *);
   LOCALE void                    EnvFocus(void *,void *);
   LOCALE int                     EnvGetFocusChanged(void *);
   LOCALE void                    EnvSetFocusChanged(void *,int);
   LOCALE void                    ListFocusStackCommand(void *);
   LOCALE void                    EnvListFocusStack(void *,const char *);
   LOCALE void                    GetFocusStackFunction(void *,DATA_OBJECT_PTR);
   LOCALE void                    EnvGetFocusStack(void *,DATA_OBJECT_PTR);
   LOCALE void                   *PopFocusFunction(void *);
   LOCALE void                   *GetFocusFunction(void *);
   LOCALE void                   *EnvPopFocus(void *);
   LOCALE void                   *EnvGetFocus(void *);
   LOCALE intBool                 EnvGetHaltRules(void *);
   LOCALE void                    EnvSetHaltRules(void *,intBool);
   LOCALE struct activation      *NextActivationToFire(void *);

#endif






