/**
 * @file
 * Frontend to maya repl (CLIPS)
 * @copyright
 * maya-app
 * Copyright (c) 2012-2023, Joshua Scoggins
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
#include <Arduino.h>
#include "platform/os.h"
extern "C" {
    #include "clips.h"
}
#include "electron/Environment.h"
//#include <VFS.h>
#include <LittleFS.h>
#include <SDFS.h>
#include <SemiFS.h>
#include <FS.h>
#include <FSImpl.h>
#include <list>
#include <map>
// need the FILENOs for stdin/stdout/stderr
#include <unistd.h>


// so the VFS implementation takes over the newlib syscalls so we need to work
// within its confines. So a new custom filesystem for stdin and stdout need to
// be handled? Not actually possible if VFS is used. The file handles start at
// 3 so 0, 1, and 2 are ignored. What I need to do is actually implement my own
// copy of the VFS to make it possible to handle all of this. Or I need to make
// a custom file which wraps the stdin/out/err devices I want but open them
// through the VFS implementation
//

// taken from VFS.cpp/h but updated for my purposes
struct Entry{
    Entry(const char* p, FS* f) : path(p), fs(f) { }
    const char* path;
    FS *fs;
};

namespace {
    static FS* root = nullptr;
    static std::list<Entry> mountPoints;
    static std::map<int, File> files;
    static int fd = 3;
} // end namespace 

class CLIPSVFSClass {
    CLIPSVFSClass() = default;
    void root(FS& fs) noexcept {
        ::root = &fs;
    }
    void map(const char* path, FS& fs) {
        mountPoints.emplace_back(Entry{strdup(path), &fs});
    }
};

namespace {
    static FS* pathToFS(const char** name) {
        const char* nm = *name;
        for (const auto& a : mountPoints) {
            if (!strncmp(a.path, nm, strlen(a.path))) {
                *name += strlen(a.path);
                return a.fs;
            }
        }
        return ::root;
    }
}
extern "C"
int 
_open(char* file, int flags, int /*mode*/) {
    const char* nm = file;
    auto fs = pathToFS(&nm);
    if (!fs) {
        return -1;
    }
    const char *md = "r";
    flags &= O_RDONLY | O_WRONLY | O_CREAT | O_TRUNC | O_APPEND | O_RDWR;
    if (flags == O_RDONLY) {
        md = "r";
    } else if (flags == (O_WRONLY | O_CREAT | O_TRUNC)) {
        md = "w";
    } else if (flags == (O_WRONLY | O_CREAT | O_APPEND)) {
        md = "a";
    } else if (flags == O_RDWR) {
        md = "r+";
    } else if (flags == (O_RDWR | O_CREAT | O_TRUNC)) {
        md = "w+";
    } else if (flags == (O_RDWR | O_CREAT | O_APPEND)) {
        md = "a+";
    }
    File f = fs->open(nm, md);
    if (!f) {
        return -1;
    }
    files.insert({fd, f});
    return fd++;
}
static_assert(STDOUT_FILENO < 3, "the stdout filehandle is not right!");
static_assert(STDERR_FILENO < 3, "the stderr filehandle is not right!");
static_assert(STDIN_FILENO < 3, "the stdin filehandle is not right!");
extern "C"
ssize_t 
_write(int fd, const void* buf, size_t count) {
    switch (fd) {
        case STDOUT_FILENO:
        case STDERR_FILENO:
            return Serial.write((const char*)buf, count);
        default: {
                     auto f = files.find(fd);
                     if (f == files.end()) {
                         return 0;
                     }
                     return f->second.write((const char*)buf, count);
                 }
    }
}

extern "C" 
int 
_close(int fd) {
    // no difference from the VFS impl
    auto f = files.find(fd);
    if (f == files.end()) {
        return -1;
    }
    f->second.close();
    files.erase(f);
    return 0;
}

extern "C"
int
_lseek(int fd, int ptr, int dir) {
    auto f = files.find(fd);
    if (f == files.end()) {
        return -1;
    }
    SeekMode d = SeekSet;
    if (dir == SEEK_CUR) {
        d = SeekCur;
    } else if (dir == SEEK_END) {
        d = SeekEnd;
    }

    return f->second.seek(ptr, d) ? 0 : 1;

}

extern "C"
int _read(int fd, char* buf, int size) {
    if (fd == STDIN_FILENO) {
        return Serial.readBytes(buf, size);
    } 
    auto f = files.find(fd);
    if (f == files.end()) {
        return -1; // FD not found
    }
    return f->second.read((uint8_t*)buf, size);
}

extern "C"
int _unlink(char* name) {
    auto f = pathToFS((const char**)&name);
    if (f) {
        return f->remove(name) ? 0 : -1;
    }
    return -1;
}

extern "C"
int _stat(const char* name, struct stat* st) {
    auto f = pathToFS((const char**)&name);
    if (f) {
        fs::FSStat s;
        if (!f->stat(name, &s)) {
            return -1;
        }
        bzero(st, sizeof(*st));
        st->st_size = s.size;
        st->st_blksize = s.blocksize;
        st->st_ctim.tv_sec = s.ctime;
        st->st_atim.tv_sec = s.atime;
        st->st_mode = s.isDir ? S_IFDIR : S_IFREG;
        return 0;
    }
    return -1;
}


// CLIPS/Maya application body
Electron::Environment* mainEnv = nullptr;

void 
setup1() {
    mainEnv = new Electron::Environment();
    CommandLoop(*mainEnv);
}
void
loop1() {

}
void
setup() {
    LittleFS.begin();
    SDFS.begin();

    //VFS.map("/lfs", LittleFS);
    //VFS.map("/sd", SDFS);

    Serial.begin(9600);

    pinMode(LED_BUILTIN, OUTPUT);
    digitalWrite(LED_BUILTIN, LOW);
}

void
loop() {
    digitalWrite(LED_BUILTIN, LOW);
    delay(1000);
    digitalWrite(LED_BUILTIN, HIGH);
    delay(1000);
}
