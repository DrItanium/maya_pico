   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*              PREDICATE FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several predicate          */
/*   functions including not, and, or, eq, neq, <=, >=, <,   */
/*   >, =, <>, symbolp, stringp, lexemep, numberp, integerp, */
/*   floatp, oddp, evenp, multifieldp, sequencep, and        */
/*   pointerp.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#define _PRDCTFUN_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "envrnmnt.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "multifld.h"
#include "router.h"

#include "prdctfun.h"

/**************************************************/
/* PredicateFunctionDefinitions: Defines standard */
/*   math and predicate functions.                */
/**************************************************/
globle void PredicateFunctionDefinitions(
  void *theEnv)
  {
#if ! RUN_TIME
   EnvDefineFunction2(theEnv,(char*)"not", 'b', NotFunction, (char*)"NotFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"and", 'b', AndFunction, (char*)"AndFunction", (char*)"2*");
   EnvDefineFunction2(theEnv,(char*)"or", 'b', OrFunction, (char*)"OrFunction", (char*)"2*");

   EnvDefineFunction2(theEnv,(char*)"eq", 'b', EqFunction, (char*)"EqFunction", (char*)"2*");
   EnvDefineFunction2(theEnv,(char*)"neq", 'b', NeqFunction, (char*)"NeqFunction", (char*)"2*");

   EnvDefineFunction2(theEnv,(char*)"<=", 'b', LessThanOrEqualFunction, (char*)"LessThanOrEqualFunction", (char*)"2*n");
   EnvDefineFunction2(theEnv,(char*)">=", 'b', GreaterThanOrEqualFunction, (char*)"GreaterThanOrEqualFunction", (char*)"2*n");
   EnvDefineFunction2(theEnv,(char*)"<", 'b', LessThanFunction, (char*)"LessThanFunction", (char*)"2*n");
   EnvDefineFunction2(theEnv,(char*)">", 'b', GreaterThanFunction, (char*)"GreaterThanFunction", (char*)"2*n");
   EnvDefineFunction2(theEnv,(char*)"=", 'b', NumericEqualFunction, (char*)"NumericEqualFunction", (char*)"2*n");
   EnvDefineFunction2(theEnv,(char*)"<>", 'b', NumericNotEqualFunction, (char*)"NumericNotEqualFunction", (char*)"2*n");
   EnvDefineFunction2(theEnv,(char*)"!=", 'b', NumericNotEqualFunction, (char*)"NumericNotEqualFunction", (char*)"2*n");

   EnvDefineFunction2(theEnv,(char*)"symbolp", 'b', SymbolpFunction, (char*)"SymbolpFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"wordp", 'b', SymbolpFunction, (char*)"SymbolpFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"stringp", 'b', StringpFunction, (char*)"StringpFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"lexemep", 'b', LexemepFunction, (char*)"LexemepFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"numberp", 'b', NumberpFunction, (char*)"NumberpFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"integerp", 'b', IntegerpFunction, (char*)"IntegerpFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"floatp", 'b', FloatpFunction, (char*)"FloatpFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"oddp", 'b', OddpFunction, (char*)"OddpFunction", (char*)"11i");
   EnvDefineFunction2(theEnv,(char*)"evenp", 'b', EvenpFunction, (char*)"EvenpFunction", (char*)"11i");
   EnvDefineFunction2(theEnv,(char*)"multifieldp",'b', MultifieldpFunction, (char*)"MultifieldpFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"sequencep",'b', MultifieldpFunction, (char*)"MultifieldpFunction", (char*)"11");
   EnvDefineFunction2(theEnv,(char*)"pointerp", 'b', PointerpFunction, (char*)"PointerpFunction", (char*)"11");
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

/************************************/
/* EqFunction: H/L access routine   */
/*   for the eq function.           */
/************************************/
globle intBool EqFunction(
  void *theEnv)
  {
   DATA_OBJECT item, nextItem;
   int numArgs, i;
   struct expr *theExpression;

   /*====================================*/
   /* Determine the number of arguments. */
   /*====================================*/

   numArgs = EnvRtnArgCount(theEnv);
   if (numArgs == 0) return(FALSE);

   /*==============================================*/
   /* Get the value of the first argument against  */
   /* which subsequent arguments will be compared. */
   /*==============================================*/

   theExpression = GetFirstArgument();
   EvaluateExpression(theEnv,theExpression,&item);

   /*=====================================*/
   /* Compare all arguments to the first. */
   /* If any are different, return FALSE. */
   /*=====================================*/

   theExpression = GetNextArgument(theExpression);
   for (i = 2 ; i <= numArgs ; i++)
     {
      EvaluateExpression(theEnv,theExpression,&nextItem);

      if (GetType(nextItem) != GetType(item))
        { return(FALSE); }

      if (GetType(nextItem) == MULTIFIELD)
        {
         if (MultifieldDOsEqual(&nextItem,&item) == FALSE)
           { return(FALSE); }
        }
      else if (nextItem.value != item.value)
        { return(FALSE); }

      theExpression = GetNextArgument(theExpression);
     }
   /*=====================================*/
   /* All of the arguments were identical */
   /* to the first. Return TRUE.          */
   /*=====================================*/

   return(TRUE);
  }

/*************************************/
/* NeqFunction: H/L access routine   */
/*   for the neq function.           */
/*************************************/
globle intBool NeqFunction(
  void *theEnv)
  {
   DATA_OBJECT item, nextItem;
   int numArgs, i;
   struct expr *theExpression;

   /*====================================*/
   /* Determine the number of arguments. */
   /*====================================*/

   numArgs = EnvRtnArgCount(theEnv);
   if (numArgs == 0) return(FALSE);

   /*==============================================*/
   /* Get the value of the first argument against  */
   /* which subsequent arguments will be compared. */
   /*==============================================*/

   theExpression = GetFirstArgument();
   EvaluateExpression(theEnv,theExpression,&item);

   /*=====================================*/
   /* Compare all arguments to the first. */
   /* If any are the same, return FALSE.  */
   /*=====================================*/

   for (i = 2, theExpression = GetNextArgument(theExpression);
        i <= numArgs;
        i++, theExpression = GetNextArgument(theExpression))
     {
      EvaluateExpression(theEnv,theExpression,&nextItem);
      if (GetType(nextItem) != GetType(item))
        { continue; }
      else if (nextItem.type == MULTIFIELD)
        {
         if (MultifieldDOsEqual(&nextItem,&item) == TRUE)
           { return(FALSE); }
        }
      else if (nextItem.value == item.value)
        { return(FALSE); }
     }

   /*=====================================*/
   /* All of the arguments were different */
   /* from the first. Return TRUE.        */
   /*=====================================*/

   return(TRUE);
  }

/*****************************************/
/* StringpFunction: H/L access routine   */
/*   for the stringp function.           */
/*****************************************/
globle intBool StringpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;

   if (EnvArgCountCheck(theEnv,(char*)"stringp",EXACTLY,1) == -1) return(FALSE);

   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) == STRING)
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/*****************************************/
/* SymbolpFunction: H/L access routine   */
/*   for the symbolp function.           */
/*****************************************/
globle intBool SymbolpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;

   if (EnvArgCountCheck(theEnv,(char*)"symbolp",EXACTLY,1) == -1) return(FALSE);

   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) == SYMBOL)
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/*****************************************/
/* LexemepFunction: H/L access routine   */
/*   for the lexemep function.           */
/*****************************************/
globle intBool LexemepFunction(
  void *theEnv)
  {
   DATA_OBJECT item;

   if (EnvArgCountCheck(theEnv,(char*)"lexemep",EXACTLY,1) == -1) return(FALSE);

   EnvRtnUnknown(theEnv,1,&item);

   if ((GetType(item) == SYMBOL) || (GetType(item) == STRING))
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/*****************************************/
/* NumberpFunction: H/L access routine   */
/*   for the numberp function.           */
/*****************************************/
globle intBool NumberpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;

   if (EnvArgCountCheck(theEnv,(char*)"numberp",EXACTLY,1) == -1) return(FALSE);

   EnvRtnUnknown(theEnv,1,&item);

   if ((GetType(item) == FLOAT) || (GetType(item) == INTEGER))
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/****************************************/
/* FloatpFunction: H/L access routine   */
/*   for the floatp function.           */
/****************************************/
globle intBool FloatpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;

   if (EnvArgCountCheck(theEnv,(char*)"floatp",EXACTLY,1) == -1) return(FALSE);

   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) == FLOAT)
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/******************************************/
/* IntegerpFunction: H/L access routine   */
/*   for the integerp function.           */
/******************************************/
globle intBool IntegerpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;

   if (EnvArgCountCheck(theEnv,(char*)"integerp",EXACTLY,1) == -1) return(FALSE);

   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) != INTEGER) return(FALSE);

   return(TRUE);
  }

/*********************************************/
/* MultifieldpFunction: H/L access routine   */
/*   for the multifieldp function.           */
/*********************************************/
globle intBool MultifieldpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;

   if (EnvArgCountCheck(theEnv,(char*)"multifieldp",EXACTLY,1) == -1) return(FALSE);

   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) != MULTIFIELD) return(FALSE);

   return(TRUE);
  }

/******************************************/
/* PointerpFunction: H/L access routine   */
/*   for the pointerp function.           */
/******************************************/
globle intBool PointerpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;

   if (EnvArgCountCheck(theEnv,(char*)"pointerp",EXACTLY,1) == -1) return(FALSE);

   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) != EXTERNAL_ADDRESS) return(FALSE);

   return(TRUE);
  }

/*************************************/
/* NotFunction: H/L access routine   */
/*   for the not function.           */
/*************************************/
globle intBool NotFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT result;

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(FALSE); }

   if (EvaluateExpression(theEnv,theArgument,&result)) return(FALSE);

   if ((result.value == EnvFalseSymbol(theEnv)) && (result.type == SYMBOL))
     { return(TRUE); }
   
   return(FALSE);
  }

/*************************************/
/* AndFunction: H/L access routine   */
/*   for the and function.           */
/*************************************/
globle intBool AndFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT result;

   for (theArgument = GetFirstArgument();
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument))
     {
      if (EvaluateExpression(theEnv,theArgument,&result)) return(FALSE);
      if ((result.value == EnvFalseSymbol(theEnv)) && (result.type == SYMBOL))
        { return(FALSE); }
     }

   return(TRUE);
  }

/************************************/
/* OrFunction: H/L access routine   */
/*   for the or function.           */
/************************************/
globle intBool OrFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT result;

   for (theArgument = GetFirstArgument();
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument))
     {
      if (EvaluateExpression(theEnv,theArgument,&result)) return(FALSE);

      if ((result.value != EnvFalseSymbol(theEnv)) || (result.type != SYMBOL))
        { return(TRUE); }
     }

   return(FALSE);
  }

/*****************************************/
/* LessThanOrEqualFunction: H/L access   */
/*   routine for the <= function.        */
/*****************************************/
globle intBool LessThanOrEqualFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theEnv,theArgument,(char*)"<=",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*====================================================*/
   /* Compare each of the subsequent arguments to its    */
   /* predecessor. If any is greater, then return FALSE. */
   /*====================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,(char*)"<=",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) > ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) > ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) > (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) > ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*======================================*/
   /* Each argument was less than or equal */
   /* to it predecessor. Return TRUE.      */
   /*======================================*/

   return(TRUE);
  }

/********************************************/
/* GreaterThanOrEqualFunction: H/L access   */
/*   routine for the >= function.           */
/********************************************/
globle intBool GreaterThanOrEqualFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theEnv,theArgument,(char*)">=",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*===================================================*/
   /* Compare each of the subsequent arguments to its   */
   /* predecessor. If any is lesser, then return FALSE. */
   /*===================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,(char*)">=",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) < ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) < ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) < (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) < ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*=========================================*/
   /* Each argument was greater than or equal */
   /* to its predecessor. Return TRUE.        */
   /*=========================================*/

   return(TRUE);
  }

/**********************************/
/* LessThanFunction: H/L access   */
/*   routine for the < function.  */
/**********************************/
globle intBool LessThanFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theEnv,theArgument,(char*)"<",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*==========================================*/
   /* Compare each of the subsequent arguments */
   /* to its predecessor. If any is greater or */
   /* equal, then return FALSE.                */
   /*==========================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,(char*)"<",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) >= ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) >= ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) >= (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) >= ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*=================================*/
   /* Each argument was less than its */
   /* predecessor. Return TRUE.       */
   /*=================================*/

   return(TRUE);
  }

/*************************************/
/* GreaterThanFunction: H/L access   */
/*   routine for the > function.     */
/*************************************/
globle intBool GreaterThanFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theEnv,theArgument,(char*)">",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*==========================================*/
   /* Compare each of the subsequent arguments */
   /* to its predecessor. If any is lesser or  */
   /* equal, then return FALSE.                */
   /*==========================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,(char*)">",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) <= ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) <= ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) <= (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) <= ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*================================*/
   /* Each argument was greater than */
   /* its predecessor. Return TRUE.  */
   /*================================*/

   return(TRUE);
  }

/**************************************/
/* NumericEqualFunction: H/L access   */
/*   routine for the = function.      */
/**************************************/
globle intBool NumericEqualFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();

   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theEnv,theArgument,(char*)"=",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*=================================================*/
   /* Compare each of the subsequent arguments to the */
   /* first. If any is unequal, then return FALSE.    */
   /*=================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,(char*)"=",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) != ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) != ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) != (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) != ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
     }

   /*=================================*/
   /* All arguments were equal to the */
   /* first argument. Return TRUE.    */
   /*=================================*/

   return(TRUE);
  }

/*****************************************/
/* NumericNotEqualFunction: H/L access   */
/*   routine for the <> function.        */
/*****************************************/
globle intBool NumericNotEqualFunction(
  void *theEnv)
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theEnv,theArgument,(char*)"<>",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*=================================================*/
   /* Compare each of the subsequent arguments to the */
   /* first. If any is equal, then return FALSE.      */
   /*=================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,(char*)"<>",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) == ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) == ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) == (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) == ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
     }

   /*===================================*/
   /* All arguments were unequal to the */
   /* first argument. Return TRUE.      */
   /*===================================*/

   return(TRUE);
  }

/**************************************/
/* OddpFunction: H/L access routine   */
/*   for the oddp function.           */
/**************************************/
globle intBool OddpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;
   long long num, halfnum;

   if (EnvArgCountCheck(theEnv,(char*)"oddp",EXACTLY,1) == -1) return(FALSE);
   if (EnvArgTypeCheck(theEnv,(char*)"oddp",1,INTEGER,&item) == FALSE) return(FALSE);

   num = DOToLong(item);

   halfnum = (num / 2) * 2;
   if (num == halfnum) return(FALSE);

   return(TRUE);
  }

/***************************************/
/* EvenpFunction: H/L access routine   */
/*   for the evenp function.           */
/***************************************/
globle intBool EvenpFunction(
  void *theEnv)
  {
   DATA_OBJECT item;
   long long num, halfnum;

   if (EnvArgCountCheck(theEnv,(char*)"evenp",EXACTLY,1) == -1) return(FALSE);
   if (EnvArgTypeCheck(theEnv,(char*)"evenp",1,INTEGER,&item) == FALSE) return(FALSE);

   num = DOToLong(item);

   halfnum = (num / 2) * 2;
   if (num != halfnum) return(FALSE);

   return(TRUE);
  }



