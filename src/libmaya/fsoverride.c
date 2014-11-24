/*
maya
Copyright (c) 2012-2014, Joshua Scoggins 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include <stdlib.h>
#include "clips.h"
#include "libmaya.h"
#if FILE_SYSTEM_ROOTING
#if ! RUN_TIME
static void deallocateFileSystemRootData(void* theEnv);
static int GetFileSystemRootingIsEnabled(void* theEnv);
static int SetFileSystemRootingIsEnabled(void* theEnv);
static void GetFileSystemRoot(void* theEnv, DATA_OBJECT_PTR ret);
static void SetFileSystemRoot(void* theEnv, DATA_OBJECT_PTR ret);
#endif
void DefineFSOverrideFunctions(void* theEnv) {
#if ! RUN_TIME
	char* base;
	char* tmp;
	int size;
	if(!AllocateEnvironmentData(theEnv, FILE_SYSTEM_ROOT_DATA, 
				sizeof(FileSystemRootData), deallocateFileSystemRootData)) {
		printf("Error allocating environment data for FILE_SYSTEM_ROOT_DATA\n");
		exit(EXIT_FAILURE);
	}
	base = getenv((const char*)FILE_SYSTEM_BASE);
	if (base == NULL) {
		size = 1;
		tmp = gm1(theEnv, 1);
		tmp[0] = '\0';
		FileSystemRootData(theEnv)->rootingEnabled = FALSE;
	} else {
		//make a copy
		size = sizeof(char) * (strlen(base) + 2);
		tmp = gm1(theEnv, size);
		gensprintf(tmp, "%s", base);
		FileSystemRootData(theEnv)->rootingEnabled = TRUE;
	}
	FileSystemRootData(theEnv)->root = tmp;
	FileSystemRootData(theEnv)->stringlength = size;
	//TODO: define custom environment data structure for base path and if
	//file-system rooting should take place (
	EnvDefineFunction2(theEnv,
			"get-file-system-rooting-is-enabled",
			'b',
			PTIEF GetFileSystemRootingIsEnabled,
			"GetFileSystemRootingIsEnabled",
			"00a");
	EnvDefineFunction2(theEnv,
			"set-file-system-rooting-is-enabled",
			'b',
			PTIEF SetFileSystemRootingIsEnabled,
			"SetFileSystemRootingIsEnabled",
			"11w");
	EnvDefineFunction2(theEnv,
			"get-file-system-root",
			'k',
			PTIEF GetFileSystemRoot,
			"GetFileSystemRoot",
			"00a");
	EnvDefineFunction2(theEnv,
			"set-file-system-root",
			'k',
			PTIEF SetFileSystemRoot,
			"SetFileSystemRoot",
			"11k");
    EnvDefineFunction2(theEnv,
            "remove",   
            'b', 
            PTIEF FS_RemoveFunction,  
            "FS_RemoveFunction", 
            "11k");
    EnvDefineFunction2(theEnv,
            "rename",   
            'b',
            PTIEF FS_RenameFunction, 
            "FS_RenameFunction", 
            "22k");
#endif
}

FILE* GenOpen(void* theEnv, char* fileName, char* accessType) {
	char* base;
	char* tmp;
	int size;
	FILE* result;

	base = getenv((const char*)FILE_SYSTEM_BASE);

	if(base == NULL) {
		result = _GenOpen(theEnv, fileName, accessType);
	} else {
		size = sizeof(char) * (strlen(fileName) + strlen(base) + 2);
		tmp = gm1(theEnv, size);
		gensprintf(tmp, "%s/%s", base, fileName);
		result = _GenOpen(theEnv, tmp, accessType);
		rm(theEnv,tmp, size);
	}
	return result;
}
int GenOpenReadBinary(void* theEnv, char* funcName, char* fileName) {
	char* base;
	char* tmp;
	int result, size;

	base = getenv((const char*)FILE_SYSTEM_BASE);

	if(base == NULL) {
		result = _GenOpenReadBinary(theEnv, funcName, fileName);
	} else {
		size = sizeof(char) * (strlen(fileName) + strlen(base) + 2);
		tmp = gm1(theEnv, size);
		gensprintf(tmp, "%s/%s", base, fileName);
		result = _GenOpenReadBinary(theEnv, funcName, tmp);
		rm(theEnv,tmp, size);
	}
	return result;
}
int FS_RemoveFunction(void *theEnv) {
    char *theFileName;
    char *base;
    char* tmp;
    int result, size;


    if (EnvArgCountCheck(theEnv,(char*)"remove",EXACTLY,1) == -1) 
        return(FALSE);


    if ((theFileName = GetFileName(theEnv,(char*)"remove",1)) == NULL) 
        return(FALSE);

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base == NULL) {
        result = (genremove(theFileName));
    } else {
        size = sizeof(char) * (strlen(theFileName) + strlen(base) + 2);
        tmp = gm1(theEnv, size);
        gensprintf(tmp, "%s/%s", base, theFileName);
        result = (genremove(tmp));
        rm(theEnv,tmp, size);
    }
	return result;

}

int FS_RenameFunction(void *theEnv) {
    char* oldFileName;
    char* newFileName;
    char* old;
    char* new;
    char* base;
    int sizeOld, sizeNew, result;

    if (EnvArgCountCheck(theEnv,(char*)"rename",EXACTLY,2) == -1) 
        return(FALSE);


    if ((oldFileName = GetFileName(theEnv,(char*)"rename",1)) == NULL) 
        return(FALSE);
    if ((newFileName = GetFileName(theEnv,(char*)"rename",2)) == NULL) 
        return(FALSE);

    base = getenv((const char*)FILE_SYSTEM_BASE);

	if(base == NULL) {
		result = genrename(oldFileName, newFileName);
	} else {
		sizeOld = sizeof(char) * (strlen(base) + strlen(oldFileName) + 2);
		sizeNew = sizeof(char) * (strlen(base) + strlen(newFileName) + 2);
		old = gm1(theEnv, sizeOld);
		new = gm1(theEnv, sizeNew);
		gensprintf(old, "%s/%s", base, oldFileName);
		gensprintf(new, "%s/%s", base, newFileName);
		result = genrename(old,new);
		rm(theEnv,old, sizeOld);
		rm(theEnv,new, sizeNew);
	}
	return result;

}

void deallocateFileSystemRootData(void* theEnv) {
	rm(theEnv,FileSystemRootData(theEnv)->root, 
			  FileSystemRootData(theEnv)->stringlength);
	FileSystemRootData(theEnv)->root = 0;
	FileSystemRootData(theEnv)->stringlength = 0;
}
int GetFileSystemRootingIsEnabled(void* theEnv) {
	if (FileSystemRootData(theEnv)->rootingEnabled) {
		return TRUE;
	} else {
		return FALSE;
	}
}
int SetFileSystemRootingIsEnabled(void* theEnv) {
	int oldValue;
	DATA_OBJECT arg;
	oldValue = FileSystemRootData(theEnv)->rootingEnabled;
	EnvRtnUnknown(theEnv, 1, &arg);
	if((arg.value == EnvFalseSymbol(theEnv)) && (arg.type == SYMBOL)) {
		FileSystemRootData(theEnv)->rootingEnabled = FALSE;
	} else {
		FileSystemRootData(theEnv)->rootingEnabled = TRUE;
	}
	return oldValue;
}
void* GetFileSystemRoot(void* theEnv) {
	return EnvAddSymbol(theEnv, FileSystemRootData(theEnv)->root);
}
void SetFileSystemRoot(void* theEnv, DATA_OBJECT_PTR ret) {
	DATA_OBJECT arg0;
	void* oldValue;
	char* newRoot;
	char* value;
	int size;
	if (EnvArgCountCheck("set-file-system-root", EXACTLY, 1) == -1) {
		SetpType(ret, SYMBOL);
		SetpValue(ret, EnvFalseSymbol(theEnv));
		return;
	}
	if (!ArgTypeCheck("set-file-system-root", 1, SYMBOL_OR_STRING, &arg0)) {
		SetpType(ret, SYMBOL);
		SetpValue(ret, EnvFalseSymbol(theEnv));
		return;
	}
	oldValue = EnvAddSymbol(theEnv, FileSystemRootData(theEnv)->root);
	rm(theEnv, FileSystemRootData(theEnv)->root,
			   FileSystemRootData(theEnv)->stringlength);
	newRoot = EnvRtnLexeme(theEnv, 1);
    size = sizeof(char) * strlen(newRoot) + 1;
	value = gm1(theEnv, size);
	gensprintf(value, "%s", newRoot);
	FileSystemRootData(theEnv)->root = value;
	FileSystemRootData(theEnv)->stringlength = size;
	//okay that is all setup
	return oldValue;
}
#endif /* FILE_SYSTEM_ROOTING */
