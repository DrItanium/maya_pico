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
}

} // end namespace Electron
