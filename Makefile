########### MAKEFILE FOR MAYA ###########
include config.mk
CFLAGS += -I.
CXXFLAGS += -I.
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

repl: $(OBJS) cmd/repl/main.o
	@echo Building maya
	@$(LD) $(LDFLAGS) -o maya cmd/repl/main.o $(OBJS) ${LIBRARIES}

install: repl
	@echo Installing binaries to $(PREFIX)/bin
	@mkdir -p $(PREFIX)/bin
	@cp $(OUTPUT) $(PREFIX)/bin

deinstall uninstall:
	@echo Uninstalling...
	@rm -f $(PREFIX)/bin/$(OUTPUT)

clean:
	@echo Cleaning
	@rm -f $(OBJS) cmd/repl/main.o $(OUTPUT)

.c.o :
	@echo CC $<
	@${CC} ${CFLAGS} -c $< -o $@

.cc.o :
	@echo CXX $<
	@${CXX} ${CXXFLAGS} -c $< -o $@

