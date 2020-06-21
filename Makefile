########### MAKEFILE FOR MAYA ###########
include config.mk
CFLAGS += -I.
CXXFLAGS += -I.

OBJECTS = agenda.o analysis.o argacces.o bload.o bmathfun.o bsave.o classcom.o \
		  classexm.o classfun.o classinf.o classini.o classpsr.o clsltpsr.o \
		  commline.o constrct.o constrnt.o crstrtgy.o cstrcbin.o cstrccom.o cstrcpsr.o \
		  cstrnbin.o cstrnchk.o cstrnops.o cstrnpsr.o cstrnutl.o \
		  default.o defins.o developr.o dffctbin.o dffctbsc.o dffctdef.o dffctpsr.o \
		  dffnxbin.o dffnxexe.o dffnxfun.o dffnxpsr.o dfinsbin.o \
		  drive.o emathfun.o engine.o envrnbld.o envrnmnt.o evaluatn.o expressn.o \
		  exprnbin.o exprnops.o exprnpsr.o extnfunc.o factbin.o factbld.o \
		  factcom.o factfun.o factgen.o facthsh.o factlhs.o factmch.o factmngr.o \
		  factprt.o factqpsr.o factqury.o factrete.o factrhs.o filecom.o filertr.o \
		  fileutil.o generate.o genrcbin.o genrccom.o genrcexe.o genrcfun.o \
		  genrcpsr.o globlbin.o globlbsc.o globlcom.o globldef.o globlpsr.o \
		  immthpsr.o incrrset.o inherpsr.o inscom.o insfile.o insfun.o insmngr.o \
		  insmoddp.o insmult.o inspsr.o insquery.o insqypsr.o iofun.o lgcldpnd.o \
		  maya.o memalloc.o miscfun.o modulbin.o modulbsc.o moduldef.o modulpsr.o \
		  modulutl.o msgcom.o msgfun.o msgpass.o msgpsr.o multifld.o \
		  multifun.o objbin.o objrtbin.o objrtbld.o objrtfnx.o objrtgen.o \
		  objrtmch.o parsefun.o pattern.o pprint.o prccode.o prcdrfun.o prcdrpsr.o \
		  prdctfun.o prntutil.o proflfun.o reorder.o reteutil.o retract.o \
		  router.o rulebin.o rulebld.o rulebsc.o rulecom.o rulecstr.o \
		  ruledef.o ruledlt.o rulelhs.o rulepsr.o scanner.o sortfun.o \
		  strngfun.o strngrtr.o symblbin.o symbol.o sysdep.o textpro.o \
		  tmpltbin.o tmpltbsc.o tmpltdef.o tmpltfun.o tmpltlhs.o tmpltpsr.o \
		  tmpltrhs.o tmpltutl.o userdata.o userfunctions.o utility.o watch.o \
		  factfile.o
REPL_OBJS = cmd/repl/main.o
CFLAGS += -DBANNER_STRING=${BANNER_STRING} -DCOMMAND_PROMPT='$(COMMAND_PROMPT)'
CXXFLAGS += -DBANNER_STRING=${BANNER_STRING} -DCOMMAND_PROMPT='${COMMAND_PROMPT}'
ifeq ($(CXXEXTENSIONS), TRUE)
CXX_OBJECTS = boost.o \
			  functional.o
endif
ifeq ($(CXXEXTENSIONS), TRUE)
	OBJS = ${OBJECTS} ${CXX_OBJECTS}
else
	OBJS = ${OBJECTS}
endif

.PHONY: clean all

all: repl

repl: $(OBJS) $(REPL_OBJS)
	@echo Building maya
	@$(LD) $(LDFLAGS) -o maya $(OBJS) $(REPL_OBJS) ${LIBRARIES}

install: repl
	@echo Installing binaries to $(PREFIX)/bin
	@mkdir -p $(PREFIX)/bin
	@cp $(OUTPUT) $(PREFIX)/bin

deinstall uninstall:
	@echo Uninstalling...
	@rm -f $(PREFIX)/bin/$(OUTPUT)

clean:
	@echo Cleaning
	@rm -f $(OBJS) $(REPL_OBJS) $(OUTPUT)

.c.o :
	@echo CC $<
	@${CC} ${CFLAGS} -c $< -o $@

.cc.o :
	@echo CXX $<
	@${CXX} ${CXXFLAGS} -c $< -o $@

.cpp.o :
	@echo CXX $<
	@${CXX} ${CXXFLAGS} -c $< -o $@


