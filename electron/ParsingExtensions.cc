/**
 * @file
 * Implementation of the extensions to electron which provide CLIPS parsing * capabilities 
 * @copyright
 * Copyright (c) 2023 Joshua Scoggins 
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */

#include "electron/ParsingExtensions.h"
#include "electron/Environment.h"
#include "electron/MultifieldBuilder.h"
#include "fs/path.h"
#include <string>
extern "C" {
#include "clips/clips.h"
}


namespace Electron
{

static void nextTokenFunction(UDF_ARGS__);

void
InitializeFilesystemExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("next-token", "synldfie", 1, 1, "y", nextTokenFunction, "nextTokenFunction");
}

void
nextTokenFunction(UDF_ARGS__) {
    auto& theEnv = Environment::fromRaw(env);
    auto logicalName = ::GetLogicalName(context, STDIN);
    if (!logicalName) {
        theEnv.illegalLogicalNameMessage("next-token");
        theEnv.setHaltExecution(true);
        theEnv.setEvaluationError(true);
        out->lexemeValue = theEnv.createString("*** READ ERROR ***");
    }
    if (!theEnv.queryRouters(logicalName)) {
        theEnv.unrecognizedRouterMessage(logicalName);
        theEnv.setHaltExecution(true);
        theEnv.setEvaluationError(true);
        out->lexemeValue = theEnv.createString("*** READ ERROR ***");
    }
    auto theToken = theEnv.getToken(logicalName);
    auto& rtrData = theEnv.routerData();
    rtrData.CommandBufferInputCount = 0;
    rtrData.AwaitingInput = false;
    switch(theToken.tknType) {
        case TokenType::SYMBOL_TOKEN:
            out->lexemeValue = theToken.lexemeValue;
            break;
        case TokenType::STRING_TOKEN:
            out->lexemeValue = theToken.lexemeValue;
            break;
        case TokenType::INSTANCE_NAME_TOKEN:
            out->lexemeValue = theToken.lexemeValue;
            break;
        case TokenType::FLOAT_TOKEN:
            out->floatValue = theToken.floatValue;
            break;
        case TokenType::INTEGER_TOKEN:
            out->integerValue = theToken.integerValue;
            break;
        case TokenType::LEFT_PARENTHESIS_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("LEFT_PARENTHESIS", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::RIGHT_PARENTHESIS_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("RIGHT_PARENTHESIS", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::SF_VARIABLE_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("SF_VARIABLE", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::MF_VARIABLE_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("MF_VARIABLE", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::GBL_VARIABLE_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("GBL_VARIABLE", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::SF_WILDCARD_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("SF_WILDCARD", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::MF_WILDCARD_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("MF_WILDCARD", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::MF_GBL_VARIABLE_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("MF_GBL_WILDCARD", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::NOT_CONSTRAINT_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("NOT_CONSTRAINT", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::AND_CONSTRAINT_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("AND_CONSTRAINT", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::OR_CONSTRAINT_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("OR_CONSTRAINT", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::STOP_TOKEN:
        {
            MultifieldBuilder bldr(theEnv, 2);
            bldr.append("STOP", TreatLexemeAsSymbol{});
            bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
            out->multifieldValue = bldr.create();
            break;
        }
        case TokenType::UNKNOWN_VALUE_TOKEN:
        default:
            out->lexemeValue = theEnv.createString("*** READ ERROR ***");
            break;
    }

}

} // end namespace Electron
