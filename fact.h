//
// Created by jwscoggins on 6/20/20.
//

#ifndef MAYA_FACT_H
#define MAYA_FACT_H

typedef struct factBuilder FactBuilder;
typedef struct factModifier FactModifier;

#include "entities.h"
#include "tmpltdef.h"
#include "expressn.h"
#include "evaluatn.h"
#include "scanner.h"
#include "symbol.h"
typedef void ModifyCallFunction(Environment *, Fact *, Fact *, void *);
typedef struct modifyCallFunctionItem ModifyCallFunctionItem;

typedef enum {
    RE_NO_ERROR = 0,
    RE_NULL_POINTER_ERROR,
    RE_COULD_NOT_RETRACT_ERROR,
    RE_RULE_NETWORK_ERROR
} RetractError;

typedef enum {
    AE_NO_ERROR = 0,
    AE_NULL_POINTER_ERROR,
    AE_RETRACTED_ERROR,
    AE_COULD_NOT_ASSERT_ERROR,
    AE_RULE_NETWORK_ERROR
} AssertError;

typedef enum {
    ASE_NO_ERROR = 0,
    ASE_NULL_POINTER_ERROR,
    ASE_PARSING_ERROR,
    ASE_COULD_NOT_ASSERT_ERROR,
    ASE_RULE_NETWORK_ERROR
} AssertStringError;

typedef enum {
    FBE_NO_ERROR = 0,
    FBE_NULL_POINTER_ERROR,
    FBE_DEFTEMPLATE_NOT_FOUND_ERROR,
    FBE_IMPLIED_DEFTEMPLATE_ERROR,
    FBE_COULD_NOT_ASSERT_ERROR,
    FBE_RULE_NETWORK_ERROR
} FactBuilderError;

typedef enum {
    FME_NO_ERROR = 0,
    FME_NULL_POINTER_ERROR,
    FME_RETRACTED_ERROR,
    FME_IMPLIED_DEFTEMPLATE_ERROR,
    FME_COULD_NOT_MODIFY_ERROR,
    FME_RULE_NETWORK_ERROR
} FactModifierError;

struct modifyCallFunctionItem {
    const char *name;
    ModifyCallFunction *func;
    int priority;
    ModifyCallFunctionItem *next;
    void *context;
};

struct fact {
    union {
        struct patternEntity patternHeader;
        TypeHeader header;
    };
    Deftemplate *whichDeftemplate;
    void *list;
    long long factIndex;
    unsigned long hashValue;
    unsigned int garbage: 1;
    Fact *previousFact;
    Fact *nextFact;
    Fact *previousTemplateFact;
    Fact *nextTemplateFact;
    Multifield *basisSlots;
    Multifield theProposition;
};

struct factBuilder {
    Environment *fbEnv;
    Deftemplate *fbDeftemplate;
    CLIPSValue *fbValueArray;
};

struct factModifier {
    Environment *fmEnv;
    Fact *fmOldFact;
    CLIPSValue *fmValueArray;
    char *changeMap;
};

#include "facthsh.h"

#define FACTS_DATA 3

struct factsData {
    bool ChangeToFactList;
#if DEBUGGING_FUNCTIONS
    bool WatchFacts;
#endif
    Fact DummyFact;
    Fact *GarbageFacts;
    Fact *LastFact;
    Fact *FactList;
    long long NextFactIndex;
    unsigned long NumberOfFacts;
    struct callFunctionItemWithArg *ListOfAssertFunctions;
    struct callFunctionItemWithArg *ListOfRetractFunctions;
    ModifyCallFunctionItem *ListOfModifyFunctions;
    struct patternEntityRecord FactInfo;
    Deftemplate *CurrentDeftemplate;
    struct factHashEntry **FactHashTable;
    unsigned long FactHashTableSize;
    bool FactDuplication;
#if DEFRULE_CONSTRUCT
    Fact                    *CurrentPatternFact;
    struct multifieldMarker *CurrentPatternMarks;
#endif
    long LastModuleIndex;
    RetractError retractError;
    AssertError assertError;
    AssertStringError assertStringError;
    FactModifierError factModifierError;
    FactBuilderError factBuilderError;
};

#define FactData(theEnv) ((struct factsData *) GetEnvironmentData(theEnv,FACTS_DATA))

Fact *Assert(Fact *);
AssertStringError GetAssertStringError(Environment *);
Fact *AssertDriver(Fact *, long long, Fact *, Fact *, char *);
Fact *AssertString(Environment *, const char *);
Fact *CreateFact(Deftemplate *);
void ReleaseFact(Fact *);
void DecrementFactCallback(Environment *, Fact *);
long long FactIndex(Fact *);
GetSlotError GetFactSlot(Fact *, const char *, CLIPSValue *);
void PrintFactWithIdentifier(Environment *, const char *, Fact *, const char *);
void PrintFact(Environment *, const char *, Fact *, bool, bool, const char *);
void PrintFactIdentifierInLongForm(Environment *, const char *, Fact *);
RetractError Retract(Fact *);
RetractError RetractDriver(Environment *, Fact *, bool, char *);
RetractError RetractAllFacts(Environment *);
Fact *CreateFactBySize(Environment *, size_t);
void FactInstall(Environment *, Fact *);
void FactDeinstall(Environment *, Fact *);
Fact *GetNextFact(Environment *, Fact *);
Fact *GetNextFactInScope(Environment *, Fact *);
void FactPPForm(Fact *, StringBuilder *, bool);
bool GetFactListChanged(Environment *);
void SetFactListChanged(Environment *, bool);
unsigned long GetNumberOfFacts(Environment *);
void InitializeFacts(Environment *);
Fact *FindIndexedFact(Environment *, long long);
void RetainFact(Fact *);
void IncrementFactCallback(Environment *, Fact *);
void PrintFactIdentifier(Environment *, const char *, Fact *);
void DecrementFactBasisCount(Environment *, Fact *);
void IncrementFactBasisCount(Environment *, Fact *);
bool FactIsDeleted(Environment *, Fact *);
void ReturnFact(Environment *, Fact *);
void MatchFactFunction(Environment *, Fact *);
bool PutFactSlot(Fact *, const char *, CLIPSValue *);
bool AssignFactSlotDefaults(Fact *);
bool CopyFactSlotValues(Environment *, Fact *, Fact *);
bool DeftemplateSlotDefault(Environment *, Deftemplate *,
                            struct templateSlot *, UDFValue *, bool);
bool AddAssertFunction(Environment *, const char *,
                       VoidCallFunctionWithArg *, int, void *);
bool RemoveAssertFunction(Environment *, const char *);
bool AddRetractFunction(Environment *, const char *,
                        VoidCallFunctionWithArg *, int, void *);
bool RemoveRetractFunction(Environment *, const char *);
FactBuilder *CreateFactBuilder(Environment *, const char *);
PutSlotError FBPutSlot(FactBuilder *, const char *, CLIPSValue *);
Fact *FBAssert(FactBuilder *);
void FBDispose(FactBuilder *);
void FBAbort(FactBuilder *);
FactBuilderError FBSetDeftemplate(FactBuilder *, const char *);
PutSlotError FBPutSlotCLIPSInteger(FactBuilder *, const char *, CLIPSInteger *);
PutSlotError FBPutSlotInteger(FactBuilder *, const char *, long long);
PutSlotError FBPutSlotCLIPSFloat(FactBuilder *, const char *, CLIPSFloat *);
PutSlotError FBPutSlotFloat(FactBuilder *, const char *, double);
PutSlotError FBPutSlotCLIPSLexeme(FactBuilder *, const char *, CLIPSLexeme *);
PutSlotError FBPutSlotSymbol(FactBuilder *, const char *, const char *);
PutSlotError FBPutSlotString(FactBuilder *, const char *, const char *);
PutSlotError FBPutSlotInstanceName(FactBuilder *, const char *, const char *);
PutSlotError FBPutSlotFact(FactBuilder *, const char *, Fact *);
PutSlotError FBPutSlotInstance(FactBuilder *, const char *, Instance *);
PutSlotError FBPutSlotCLIPSExternalAddress(FactBuilder *, const char *, CLIPSExternalAddress *);
PutSlotError FBPutSlotMultifield(FactBuilder *, const char *, Multifield *);
FactBuilderError FBError(Environment *);
FactModifier *CreateFactModifier(Environment *, Fact *);
PutSlotError FMPutSlot(FactModifier *, const char *, CLIPSValue *);
Fact *FMModify(FactModifier *);
void FMDispose(FactModifier *);
void FMAbort(FactModifier *);
FactModifierError FMSetFact(FactModifier *, Fact *);
PutSlotError FMPutSlotCLIPSInteger(FactModifier *, const char *, CLIPSInteger *);
PutSlotError FMPutSlotInteger(FactModifier *, const char *, long long);
PutSlotError FMPutSlotCLIPSFloat(FactModifier *, const char *, CLIPSFloat *);
PutSlotError FMPutSlotFloat(FactModifier *, const char *, double);
PutSlotError FMPutSlotCLIPSLexeme(FactModifier *, const char *, CLIPSLexeme *);
PutSlotError FMPutSlotSymbol(FactModifier *, const char *, const char *);
PutSlotError FMPutSlotString(FactModifier *, const char *, const char *);
PutSlotError FMPutSlotInstanceName(FactModifier *, const char *, const char *);
PutSlotError FMPutSlotFact(FactModifier *, const char *, Fact *);
PutSlotError FMPutSlotInstance(FactModifier *, const char *, Instance *);
PutSlotError FMPutSlotExternalAddress(FactModifier *, const char *, CLIPSExternalAddress *);
PutSlotError FMPutSlotMultifield(FactModifier *, const char *, Multifield *);
FactModifierError FMError(Environment *);

bool AddModifyFunction(Environment *, const char *, ModifyCallFunction *, int, void *);
bool RemoveModifyFunction(Environment *, const char *);
ModifyCallFunctionItem *AddModifyFunctionToCallList(Environment *, const char *, int,
                                                    ModifyCallFunction *, ModifyCallFunctionItem *, void *);
ModifyCallFunctionItem *RemoveModifyFunctionFromCallList(Environment *, const char *,
                                                         ModifyCallFunctionItem *, bool *);
void DeallocateModifyCallList(Environment *, ModifyCallFunctionItem *);
// factmanager end
#if FACT_SET_QUERIES

typedef struct query_template
  {
   Deftemplate *templatePtr;
   struct query_template *chain, *nxt;
  } QUERY_TEMPLATE;

typedef struct query_soln
  {
   Fact **soln;
   struct query_soln *nxt;
  } QUERY_SOLN;

typedef struct query_core
  {
   Fact **solns;
   Expression *query,*action;
   QUERY_SOLN *soln_set,*soln_bottom;
   unsigned soln_size,soln_cnt;
   UDFValue *result;
  } QUERY_CORE;

typedef struct query_stack
  {
   QUERY_CORE *core;
   struct query_stack *nxt;
  } QUERY_STACK;

#define FACT_QUERY_DATA 63

struct factQueryData
  {
   CLIPSLexeme *QUERY_DELIMITER_SYMBOL;
   QUERY_CORE *QueryCore;
   QUERY_STACK *QueryCoreStack;
   bool AbortQuery;
  };

#define FactQueryData(theEnv) ((struct factQueryData *) GetEnvironmentData(theEnv,FACT_QUERY_DATA))

#define QUERY_DELIMITER_STRING     "(QDS)"

   void                           SetupFactQuery(Environment *);
   void                           GetQueryFact(Environment *,UDFContext *,UDFValue *);
   void                           GetQueryFactSlot(Environment *,UDFContext *,UDFValue *);
   void                           AnyFacts(Environment *,UDFContext *,UDFValue *);
   void                           QueryFindFact(Environment *,UDFContext *,UDFValue *);
   void                           QueryFindAllFacts(Environment *,UDFContext *,UDFValue *);
   void                           QueryDoForFact(Environment *,UDFContext *,UDFValue *);
   void                           QueryDoForAllFacts(Environment *,UDFContext *,UDFValue *);
   void                           DelayedQueryDoForAllFacts(Environment *,UDFContext *,UDFValue *);

   Expression                    *FactParseQueryNoAction(Environment *,Expression *,const char *);
   Expression                    *FactParseQueryAction(Environment *,Expression *,const char *);
#endif /* FACT_SET_QUERIES */

bool FactPNGetVar1(Environment *, void *, UDFValue *);
bool FactPNGetVar2(Environment *, void *, UDFValue *);
bool FactPNGetVar3(Environment *, void *, UDFValue *);
bool FactJNGetVar1(Environment *, void *, UDFValue *);
bool FactJNGetVar2(Environment *, void *, UDFValue *);
bool FactJNGetVar3(Environment *, void *, UDFValue *);
bool FactSlotLength(Environment *, void *, UDFValue *);
bool FactJNCompVars1(Environment *, void *, UDFValue *);
bool FactJNCompVars2(Environment *, void *, UDFValue *);
bool FactPNCompVars1(Environment *, void *, UDFValue *);
bool FactPNConstant1(Environment *, void *, UDFValue *);
bool FactPNConstant2(Environment *, void *, UDFValue *);
bool FactStoreMultifield(Environment *, void *, UDFValue *);
size_t AdjustFieldPosition(Environment *, struct multifieldMarker *,
                           unsigned short, unsigned short, size_t *);
struct expr *BuildRHSAssert(Environment *, const char *, struct token *, bool *, bool, bool, const char *);
struct expr *GetAssertArgument(Environment *, const char *, struct token *, bool *, TokenType, bool, bool *);
struct expr *GetRHSPattern(Environment *, const char *, struct token *, bool *, bool,
                           bool, bool, TokenType);
Fact *StringToFact(Environment *, const char *);

void PrintFactJNCompVars1(Environment *, const char *, void *);
void PrintFactJNCompVars2(Environment *, const char *, void *);
void PrintFactPNCompVars1(Environment *, const char *, void *);
void PrintFactJNGetVar1(Environment *, const char *, void *);
void PrintFactJNGetVar2(Environment *, const char *, void *);
void PrintFactJNGetVar3(Environment *, const char *, void *);
void PrintFactPNGetVar1(Environment *, const char *, void *);
void PrintFactPNGetVar2(Environment *, const char *, void *);
void PrintFactPNGetVar3(Environment *, const char *, void *);
void PrintFactSlotLength(Environment *, const char *, void *);
void PrintFactPNConstant1(Environment *, const char *, void *);
void PrintFactPNConstant2(Environment *, const char *, void *);

void FactPatternMatch(Environment *, Fact *,
                      struct factPatternNode *, size_t, size_t,
                      struct multifieldMarker *,
                      struct multifieldMarker *);
void MarkFactPatternForIncrementalReset(Environment *, struct patternNodeHeader *, bool);
void FactsIncrementalReset(Environment *);

#endif //MAYA_FACT_H
