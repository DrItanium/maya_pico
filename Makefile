########### MAKEFILE FOR MAYA ###########
include config.mk
OBJECTS = $(patsubst %.c,%.o, $(wildcard *.c))
CFLAGS += -DBANNER_STRING=${BANNER_STRING} -DCOMMAND_PROMPT='$(COMMAND_PROMPT)'
CXXFLAGS += -DBANNER_STRING=${BANNER_STRING} -DCOMMAND_PROMPT='${COMMAND_PROMPT}'
ifeq ($(CXXEXTENSIONS), TRUE)
CXX_OBJECTS = $(patsubst %.cc,%.o, $(wildcard *.cc))
endif
ifeq ($(CXXEXTENSIONS), TRUE)
	OBJS = ${OBJECTS} ${CXX_OBJECTS}
else
	OBJS = ${OBJECTS}
endif

.PHONY: clean all

all: repl

repl: archive cmd/repl/main.o
	@echo Building maya
	@$(LD) $(LDFLAGS) -o maya libmaya.a cmd/repl/main.o

install: repl
	@echo Installing binaries to $(PREFIX)/bin
	@mkdir -p $(PREFIX)/bin
	@cp $(OUTPUT) $(PREFIX)/bin
	@echo Installing headers to $(PREFIX)/include/maya
	@mkdir -p $(PREFIX)/include/maya
	@cp *.h $(PREFIX)/include/maya
	@echo Installing libmaya.a to $(PREFIX)/lib
	@mkdir -p $(PREFIX)/lib
	@cp libmaya.a $(PREFIX)/lib

archive: $(OBJS)
	@echo Building archive
	@$(AR) cr libmaya.a $(OBJS)


deinstall uninstall:
	@echo Uninstalling...
	@rm -f $(PREFIX)/bin/$(OUTPUT)
	@echo Removing headers in $(PREFIX)/include/maya
	@rm -f $(PREFIX)/include/maya
	@echo Deleting libmaya.a from $(PREFIX)/lib/
	@rm $(PREFIX)/lib/libmaya.a


clean:
	@echo Cleaning
	@rm -f $(OBJS) cmd/repl/main.o $(OUTPUT) libmaya.a


.c.o :
	@echo CC $<
	@$(CC) -c $(CFLAGS) -o $@ -D_POSIX_C_SOURCE=200112L \
		-std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Wmissing-prototypes -Wnested-externs -Wstrict-prototypes \
	    -Waggregate-return -Wno-implicit -I. $<

.cc.o :
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) -o $@ -D_POSIX_C_SOURCE=200112L \
		-std=c++11 -Wall -Wundef -Wpointer-arith -Wcast-qual \
		-Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
		-Waggregate-return -I. $<

