/**
 * @file
 * A C++ singleton which provides clips with access to an include path like in C.
 * This is meant to make the concept of an include easier.
 * @copyright
 * Copyright (c) 2015-2025 Parasoft Corporation
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

#include "electron/Environment.h"
#include "electron/MultifieldBuilder.h"
#include "error/Exception.h"
#include <sstream>
extern "C" {
#include "clips/clips.h"
#include "clips/prcdrpsr.h"
#include "clips/pprint.h"
}

namespace Electron
{

void getIncludeList(UDF_ARGS__) noexcept;
void addToIncludeListBack(UDF_ARGS__) noexcept;
void addToIncludeListFront(UDF_ARGS__) noexcept;
bool parseIncludeStatement(RawEnvironment* env, const char* readSource);
void clearIncludeCache(RawEnvironment* env, void* context) noexcept;
bool parseUseModuleDeclStatement(RawEnvironment* env, const char* readSource);
bool parseUseModuleTypeStatement(RawEnvironment* env, const char* readSource);
bool parseUseModuleLogicStatement(RawEnvironment* env, const char* readSource);

void
Environment::installIncludePathFunctions()
{
    // not sure where to store this yet, no need for pretty print it looks like
    _importConstruct = ::AddConstruct(_env, 
            "include", 
            "includes", 
            parseIncludeStatement,
            nullptr, nullptr, nullptr, nullptr,
            nullptr, nullptr, nullptr, nullptr, nullptr);
    addFunctionNoArgs("get-include-path", "b", getIncludeList, "getIncludeList");
    addFunction("add-to-include-path-back", "b", 1, 1, "sy;sy", addToIncludeListBack, "addToIncludeListBack");
    addFunction("add-to-include-path-front", "b", 1, 1, "sy;sy", addToIncludeListFront, "addToIncludeListFront");
    addClearFunction("clear_include_cache", clearIncludeCache, 0, nullptr);

    // these routines are used to make include a little easier to manage when it comes to creating modules which
    // are enclosed as follows:
    // ?module-name/module.clp - module declaration
    // ?module-name/types.clp - module type, decls etc
    // ?module-name/logic.clp - module logic rules
    //
    // These routines just call the include function with the appropriate file name appended to the module name
    // Originally, I was going to have a single construct which did all three but I remember that the way that matching works requires that we define things in the following order:
    // 1. modules
    // 2. types
    // 3. logic/rules
    // Failing to follow this order will result in cases where some rules do not match everything properly.
    // This is especially true with objects since subtypes may not be applied to a given rule if it comes before.
    // This may be a bug or a bad assumption but I have found it is a good idea to follow this order.
    // It has prevented potentially hard to find bugs in commercial CLIPS code I maintain at work

    _useModuleDeclConstruct = ::AddConstruct(_env,
                                             "need-package-decl",
                                             "need-package-decls",
                                             parseUseModuleDeclStatement,
                                             nullptr, nullptr, nullptr, nullptr,
                                             nullptr, nullptr, nullptr, nullptr, nullptr);
    _useModuleTypeConstruct = ::AddConstruct(_env,
                                             "need-package-types",
                                             "need-multiple-package-types",
                                             parseUseModuleTypeStatement,
                                             nullptr, nullptr, nullptr, nullptr,
                                             nullptr, nullptr, nullptr, nullptr, nullptr);
    _useModuleLogicConstruct = ::AddConstruct(_env,
                                              "need-package-logic",
                                              "need-package-logics",
                                              parseUseModuleLogicStatement,
                                              nullptr, nullptr, nullptr, nullptr,
                                              nullptr, nullptr, nullptr, nullptr, nullptr);

}

void
clearIncludeCache(RawEnvironment* env, void*) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    theEnv.clearIncludedFileSet();
}
void
addToIncludeListBack(UDF_ARGS__) noexcept {
    auto &theEnv = Environment::fromRaw(env);
    UDFValue arg;
    if (!theEnv.firstArgument(context, LEXEME_BITS, &arg)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else {
        std::string str(arg.lexemeValue->contents);
        theEnv.addToIncludePathBack(str);
        out->lexemeValue = theEnv.trueSymbol();
    }
}


void
addToIncludeListFront(UDF_ARGS__) noexcept {
    auto &theEnv = Environment::fromRaw(env);
    UDFValue arg;
    if (!theEnv.firstArgument(context, LEXEME_BITS, &arg)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else {
        std::string str(arg.lexemeValue->contents);
        theEnv.addToIncludePathFront(str);
        out->lexemeValue = theEnv.trueSymbol();
    }
}

void
getIncludeList(UDF_ARGS__) noexcept 
{
    auto &theEnv = Environment::fromRaw(env);
    auto container = theEnv.getIncludePathList();
    MultifieldBuilder mb(theEnv);
    for (const auto& line : container) {
        mb.append(line.string(), TreatLexemeAsString { });
    }

    out->multifieldValue = mb.create();
}
bool
onParseSuccess(Environment& theEnv, const char*readSource, const std::string& func) {
    Token tmp;
    theEnv.getToken(readSource, tmp);
    auto result = tmp.tknType != TokenType::RIGHT_PARENTHESIS_TOKEN;
    if (tmp.tknType != TokenType::RIGHT_PARENTHESIS_TOKEN) {
        theEnv.syntaxErrorMessage(func);
    }
    return result;
}
bool
parseModuleSyntaticSugarStatement(RawEnvironment* env, const char* readSource, const std::string& func, const std::string& file) {
    ::GCBlock frame;
    GCBlockStart(env, &frame);
    bool outcome = true;
    auto &theEnv = Environment::fromRaw(env);
    Token inputToken;
    theEnv.getToken(readSource, inputToken);
    if (inputToken.tknType == TokenType::SYMBOL_TOKEN || inputToken.tknType == TokenType::STRING_TOKEN) {
        std::string moduleName(inputToken.lexemeValue->contents);
        Neutron::Path targetModulePath(moduleName);
        if (!Neutron::exists(targetModulePath)) {
            theEnv.openErrorMessage(func, targetModulePath.string());
        } else {
            if (Neutron::Path targetFile = targetModulePath / file; Neutron::exists(targetFile)) {
                std::stringstream ss;
                ss << "(include "<< targetFile << ")";
                std::string str = ss.str();
                auto code = ::Build(theEnv.getRawEnvironment(), str.c_str());
                switch (code) {
                    case BuildError::BE_NO_ERROR:
                    case BuildError::BE_COULD_NOT_BUILD_ERROR: /* this can be from the thing already being included */
                        outcome = onParseSuccess(theEnv, readSource, func);
                        break;
                    default:
                        theEnv.syntaxErrorMessage(func);
                        break;
                }
            }
        }
    } else {
        theEnv.syntaxErrorMessage(func);
    }
    ::GCBlockEnd(env, &frame);
    ::CallPeriodicTasks(env);
    return outcome;
}
bool
parseUseModuleDeclStatement(RawEnvironment* env, const char* readSource) {
    return parseModuleSyntaticSugarStatement(env, readSource, "need-package-decl", "module.clp");
}

bool
parseUseModuleTypeStatement(RawEnvironment* env, const char* readSource) {
    return parseModuleSyntaticSugarStatement(env, readSource, "need-package-types", "types.clp");
}

bool
parseUseModuleLogicStatement(RawEnvironment* env, const char* readSource) {
    return parseModuleSyntaticSugarStatement(env, readSource, "need-package-logic", "logic.clp");
}

bool
parseIncludeStatement(RawEnvironment* env, const char* readSource)
{
    ::GCBlock frame;
    GCBlockStart(env, &frame);
    auto &theEnv = Environment::fromRaw(env);
    bool outcome = true;
    Token inputToken;
    theEnv.getToken(readSource, inputToken);
    if (inputToken.tknType == TokenType::SYMBOL_TOKEN || inputToken.tknType == TokenType::STRING_TOKEN) {
        Neutron::Path basePath(inputToken.lexemeValue->contents);
        Neutron::Path targetPrefix;
        bool foundPrefix = false;
        for (const auto& prefix : theEnv.getIncludePathList()) {
            auto newPath = prefix / basePath;
            if (Neutron::exists(newPath)) {
                foundPrefix = true;
                targetPrefix = newPath;
                break;
            } 
        }
        if (foundPrefix) {
            if (!theEnv.fileAlreadyIncluded(targetPrefix)) {
                // preserve the old fast ptr result since CLIPS tramples it inside
                // Load. Our include command supports embedding includes as constructs
                auto oldParsingConstruct = theEnv.constructData().ParsingConstruct;
                auto oldParsedBindNames = ::GetParsedBindNames(env);
                auto oldReturnContext = theEnv.expressionData().ReturnContext;
                auto oldBreakContext = theEnv.expressionData().BreakContext;
                auto oldFastPtr = ::GetFastLoad(env);
                auto lip = ::GetLoadInProgress(env);
                auto oldWarningFileName = ::CopyString(env, ::GetWarningFileName(env));
                auto oldErrorFileName = ::CopyString(env, ::GetErrorFileName(env));
                auto oldHaltExecution = ::GetHaltExecution(env);
                ::ClearParsedBindNames(env);
                auto result = ::Load(env, targetPrefix.string().c_str());
                theEnv.setHaltExecution(oldHaltExecution);
                ::SetFastLoad(env, oldFastPtr);
                ::SetLoadInProgress(env, lip);
                ::SetWarningFileName(env, oldWarningFileName);
                ::SetErrorFileName(env, oldErrorFileName);
                ::SetParsedBindNames(env, oldParsedBindNames);
                ::DeleteString(env, oldWarningFileName);
                ::DeleteString(env, oldErrorFileName);
                theEnv.constructData().ParsingConstruct = oldParsingConstruct;
                theEnv.expressionData().ReturnContext = oldReturnContext;
                theEnv.expressionData().BreakContext = oldBreakContext;
                switch (result) {
                    case ::LoadError::LE_PARSING_ERROR:
                        theEnv.syntaxErrorMessage("include");
                        break;
                    case ::LoadError::LE_OPEN_FILE_ERROR:
                        theEnv.openErrorMessage("include", basePath.string());
                        break;
                    case ::LoadError::LE_NO_ERROR:
                        theEnv.addPathToIncludedFileSet(targetPrefix);
                        outcome = onParseSuccess(theEnv, readSource, "include");
                        break;
                    default:
                        outcome = false;
                        break;
                }
            } else {
                // make sure that we still continue execution even though we
                // already saw this file
                outcome = onParseSuccess(theEnv, readSource, "include");
            }
        }  else {
            theEnv.openErrorMessage("include", basePath.string());
        }
    } else {
        theEnv.syntaxErrorMessage("include");
    }
    ::GCBlockEnd(env, &frame);
    ::CallPeriodicTasks(env);
    return outcome;
}

void
Environment::addToIncludePathBack(const Neutron::Path& path)
{
    _path.push_back(path);
}

void
Environment::addToIncludePathFront(const Neutron::Path& path)
{
    _path.push_front(path);
}

void
Environment::addPathToIncludedFileSet(const Neutron::Path& p) {
    _includedFiles.emplace(p);
}
void
Environment::clearIncludedFileSet() noexcept {
    _includedFiles.clear();
}
bool
Environment::fileAlreadyIncluded(const Neutron::Path& p) {
    return _includedFiles.count(p);
}



} // end namespace Electron
