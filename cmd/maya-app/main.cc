/**
 * @file
 * Frontend to my maya-app library
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
#include "platform/os.h"
extern "C" {
    #include "clips/clips.h"
}
#include "electron/Environment.h"
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <iostream>
#include <string>
#include <vector>
#include <list>

#if   UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
#include <signal.h>
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
   static void                    CatchCtrlC(int);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

Electron::Environment mainEnv;
/****************************************/
/* main: Starts execution of the expert */
/*   system development environment.    */
/****************************************/
int main(
  int argc,
  char *argv[])
  {
    boost::program_options::options_description desc{"Options"};
    desc.add_options()
            ("help,h", "Help screen")
            ("batch,f", boost::program_options::value<std::vector<boost::filesystem::path>>(), "files to batch")
            ("batch-star,f2", boost::program_options::value<std::vector<boost::filesystem::path>>(), "files to batch*")
            ("load,l", boost::program_options::value<std::vector<boost::filesystem::path>>(), "files to load")
            ;
    boost::program_options::variables_map vm;
    boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
    signal(SIGINT,CatchCtrlC);
#endif
      if (!mainEnv.batchFile("init.clp")) {
          std::cerr << "Could not find init.clp in current directory" << std::endl;
          return 1;
      } else {

      }

    //RerouteStdin(mainEnv, argc, argv);
    //CommandLoop(mainEnv);

    // unlike normal CLIPS, the environment will automatically clean itself up

    return 0;
}

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC || DARWIN
/***************/
/* CatchCtrlC: */
/***************/
static void CatchCtrlC(
        int sgnl)
{
    SetHaltExecution(mainEnv,true);
    CloseAllBatchSources(mainEnv);
    signal(SIGINT,CatchCtrlC);
}
#endif
