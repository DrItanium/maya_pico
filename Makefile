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

# generated with gcc -std=c99 -MM *.c cmd/repl/*.c -I.
# and
# g++ -std=c++17 -MM *.cc -I.

agenda.o: agenda.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h crstrtgy.h agenda.h ruledef.h \
 network.h match.h symbol.h constrnt.h cstrccom.h engine.h lgcldpnd.h \
 retract.h extnfunc.h memalloc.h modulutl.h scanner.h multifld.h \
 prntutil.h reteutil.h rulecom.h router.h rulebsc.h strngrtr.h sysdep.h \
 watch.h
analysis.o: analysis.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h cstrnchk.h constrnt.h evaluation.h cstrnutl.h \
 cstrnops.h  extnfunc.h expression.h  constrct.h \
 userdata.h moduldef.h utility.h symbol.h scanner.h generate.h analysis.h \
 reorder.h pattern.h match.h network.h ruledef.h agenda.h crstrtgy.h \
 cstrccom.h memalloc.h modulutl.h prntutil.h router.h rulecstr.h \
 rulepsr.h watch.h
argacces.o: argacces.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h cstrnchk.h constrnt.h evaluation.h constant.h extnfunc.h \
 expression.h  constrct.h userdata.h moduldef.h utility.h \
 symbol.h fact.h network.h match.h ruledef.h agenda.h crstrtgy.h \
 cstrccom.h tmpltdef.h scanner.h reorder.h pattern.h inscom.h insfun.h \
 object.h multifld.h objrtmch.h prntutil.h router.h sysdep.h argacces.h
bload.o: bload.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bsave.h cstrnbin.h constrnt.h \
  extnfunc.h symbol.h scanner.h memalloc.h prntutil.h router.h \
 bload.h  sysdep.h symblbin.h
bmathfun.o: bmathfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h  extnfunc.h \
 symbol.h scanner.h prntutil.h router.h bmathfun.h
bsave.o: bsave.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bload.h extnfunc.h symbol.h \
  sysdep.h symblbin.h cstrnbin.h constrnt.h  \
 scanner.h memalloc.h prntutil.h router.h bsave.h
classcom.o: classcom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h argacces.h classfun.h object.h constrnt.h multifld.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h cstrccom.h objrtmch.h scanner.h \
 classcom.h classini.h modulutl.h msgcom.h msgpass.h prntutil.h router.h
classexm.o: classexm.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h classcom.h cstrccom.h \
 object.h constrnt.h multifld.h symbol.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h classini.h insfun.h \
 memalloc.h msgcom.h msgpass.h msgfun.h prntutil.h router.h strngrtr.h \
 sysdep.h classexm.h extnfunc.h
classfun.o: classfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h classcom.h cstrccom.h object.h constrnt.h multifld.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h objrtmch.h classini.h cstrcpsr.h \
 strngfun.h inscom.h insfun.h insmngr.h memalloc.h modulutl.h scanner.h \
 msgfun.h msgpass.h prntutil.h router.h classfun.h
classinf.o: classinf.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h classcom.h cstrccom.h \
 object.h constrnt.h multifld.h symbol.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h objrtmch.h classexm.h extnfunc.h classfun.h \
 scanner.h classini.h memalloc.h insfun.h msgcom.h msgpass.h msgfun.h \
 prntutil.h classinf.h
classini.o: classini.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classexm.h extnfunc.h classfun.h scanner.h \
 classinf.h classpsr.h cstrcpsr.h strngfun.h inscom.h insfun.h memalloc.h \
 modulpsr.h modulutl.h msgcom.h msgpass.h watch.h defins.h insquery.h \
 bload.h  sysdep.h symblbin.h objbin.h objrtbld.h objrtfnx.h \
 classini.h
classpsr.o: classpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h classcom.h cstrccom.h object.h constrnt.h multifld.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h \
 clsltpsr.h cstrcpsr.h strngfun.h inherpsr.h memalloc.h modulpsr.h \
 modulutl.h msgpsr.h pprint.h prntutil.h router.h classpsr.h
clsltpsr.o: clsltpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classfun.h scanner.h cstrnchk.h cstrnpsr.h \
 cstrnutl.h default.h insfun.h memalloc.h pprint.h prntutil.h router.h \
 clsltpsr.h
commline.o: commline.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h argacces.h expression.h  constrct.h \
 userdata.h moduldef.h utility.h evaluation.h cstrcpsr.h strngfun.h \
  extnfunc.h symbol.h scanner.h fileutil.h memalloc.h \
 multifld.h pprint.h prcdrfun.h prcdrpsr.h constrnt.h prntutil.h router.h \
 strngrtr.h sysdep.h commline.h
constrct.o: constrct.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h commline.h cstrcpsr.h \
 strngfun.h  extnfunc.h symbol.h scanner.h memalloc.h miscfun.h \
 modulutl.h multifld.h prcdrfun.h prcdrpsr.h constrnt.h prntutil.h \
 router.h ruledef.h network.h match.h agenda.h crstrtgy.h cstrccom.h \
 sysdep.h watch.h
constrnt.o: constrnt.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h \
 memalloc.h multifld.h router.h scanner.h constrnt.h
crstrtgy.o: crstrtgy.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h argacces.h memalloc.h pattern.h \
 scanner.h reorder.h reteutil.h rulecom.h
cstrcbin.o: cstrcbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h cstrcbin.h
cstrccom.o: cstrccom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h extnfunc.h evaluation.h expression.h  \
 constrct.h userdata.h moduldef.h utility.h symbol.h memalloc.h \
 argacces.h multifld.h modulutl.h scanner.h prntutil.h router.h \
 commline.h sysdep.h bload.h  symblbin.h cstrcpsr.h strngfun.h \
 cstrccom.h
cstrcpsr.o: cstrcpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h router.h watch.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h prcdrpsr.h constrnt.h \
  extnfunc.h symbol.h scanner.h memalloc.h modulutl.h \
 modulpsr.h pprint.h prntutil.h strngrtr.h sysdep.h cstrcpsr.h strngfun.h
cstrnbin.o: cstrnbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h memalloc.h prntutil.h router.h bload.h utility.h \
 evaluation.h moduldef.h userdata.h extnfunc.h expression.h  \
 constrct.h symbol.h  sysdep.h symblbin.h bsave.h cstrnbin.h \
 constrnt.h
cstrnchk.o: cstrnchk.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h cstrnutl.h constrnt.h evaluation.h constant.h extnfunc.h \
 expression.h  constrct.h userdata.h moduldef.h utility.h \
 symbol.h multifld.h prntutil.h router.h classcom.h cstrccom.h object.h \
 match.h network.h ruledef.h agenda.h crstrtgy.h objrtmch.h classexm.h \
 inscom.h insfun.h cstrnchk.h
cstrnops.o: cstrnops.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h constrnt.h evaluation.h cstrnchk.h cstrnutl.h \
 extnfunc.h expression.h  constrct.h userdata.h moduldef.h \
 utility.h symbol.h memalloc.h multifld.h router.h scanner.h cstrnops.h
cstrnpsr.o: cstrnpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h cstrnchk.h constrnt.h evaluation.h cstrnutl.h \
 expression.h  constrct.h userdata.h moduldef.h utility.h \
 memalloc.h pprint.h prntutil.h router.h scanner.h sysdep.h cstrnpsr.h
cstrnutl.o: cstrnutl.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h \
 memalloc.h multifld.h router.h scanner.h cstrnutl.h constrnt.h
default.o: default.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h constrnt.h evaluation.h cstrnchk.h cstrnutl.h \
  extnfunc.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h symbol.h scanner.h fact.h network.h match.h \
 ruledef.h agenda.h crstrtgy.h cstrccom.h tmpltdef.h reorder.h pattern.h \
 inscom.h insfun.h object.h multifld.h objrtmch.h pprint.h prntutil.h \
 router.h default.h
defins.o: defins.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h dfinsbin.h defins.h cstrccom.h object.h constrnt.h multifld.h \
 match.h network.h ruledef.h agenda.h crstrtgy.h objrtmch.h argacces.h \
 classcom.h classfun.h scanner.h cstrcpsr.h strngfun.h insfun.h inspsr.h \
 memalloc.h modulpsr.h modulutl.h pprint.h prntutil.h router.h
developr.o: developr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h inscom.h \
 insfun.h object.h constrnt.h multifld.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h cstrccom.h objrtmch.h modulutl.h scanner.h \
 prntutil.h router.h fact.h tmpltdef.h reorder.h pattern.h classcom.h \
 classfun.h developr.h
dffctbin.o: dffctbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h dffctdef.h cstrccom.h memalloc.h dffctbin.h \
 cstrcbin.h modulbin.h
dffctbsc.o: dffctbsc.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h cstrccom.h cstrcpsr.h \
 strngfun.h dffctdef.h symbol.h dffctpsr.h extnfunc.h fact.h network.h \
 match.h ruledef.h agenda.h crstrtgy.h constrnt.h tmpltdef.h scanner.h \
 reorder.h pattern.h memalloc.h multifld.h router.h dffctbin.h cstrcbin.h \
 modulbin.h dffctbsc.h
dffctdef.o: dffctdef.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h dffctbsc.h dffctdef.h constrct.h userdata.h moduldef.h \
 utility.h evaluation.h constant.h cstrccom.h expression.h  \
 symbol.h dffctpsr.h memalloc.h bload.h extnfunc.h  sysdep.h \
 symblbin.h dffctbin.h cstrcbin.h modulbin.h
dffctpsr.o: dffctpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h cstrcpsr.h strngfun.h dffctbsc.h dffctdef.h cstrccom.h fact.h \
 network.h match.h ruledef.h agenda.h crstrtgy.h constrnt.h tmpltdef.h \
 scanner.h reorder.h pattern.h memalloc.h modulutl.h pprint.h prntutil.h \
 router.h dffctpsr.h
dffnxbin.o: dffnxbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h cstrcbin.h cstrccom.h memalloc.h modulbin.h \
 dffnxbin.h dffnxfun.h
dffnxexe.o: dffnxexe.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constrct.h userdata.h moduldef.h utility.h evaluation.h \
 constant.h prcdrfun.h prccode.h expression.h  scanner.h symbol.h \
 prntutil.h proflfun.h router.h watch.h dffnxexe.h dffnxfun.h
dffnxfun.o: dffnxfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h dffnxbin.h dffnxfun.h cstrcpsr.h strngfun.h dffnxpsr.h \
 modulpsr.h scanner.h dffnxexe.h watch.h argacces.h cstrccom.h memalloc.h \
 modulutl.h multifld.h prntutil.h router.h
dffnxpsr.o: dffnxpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h network.h match.h ruledef.h agenda.h crstrtgy.h constrnt.h \
 cstrccom.h genrccom.h genrcfun.h cstrcpsr.h strngfun.h dffnxfun.h \
  scanner.h memalloc.h modulutl.h pprint.h prccode.h prntutil.h \
 router.h dffnxpsr.h
dfinsbin.o: dfinsbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h cstrcbin.h defins.h cstrccom.h object.h constrnt.h \
 multifld.h match.h network.h ruledef.h agenda.h crstrtgy.h objrtmch.h \
 memalloc.h modulbin.h dfinsbin.h
drive.o: drive.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h engine.h lgcldpnd.h retract.h \
 incrrset.h memalloc.h prntutil.h reteutil.h rulecom.h router.h drive.h
emathfun.o: emathfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h miscfun.h \
 prntutil.h router.h emathfun.h
engine.o: engine.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h argacces.h commline.h fact.h tmpltdef.h \
 scanner.h reorder.h pattern.h inscom.h insfun.h object.h multifld.h \
 objrtmch.h memalloc.h modulutl.h prccode.h prcdrfun.h prntutil.h \
 proflfun.h reteutil.h rulecom.h retract.h router.h ruledlt.h sysdep.h \
 watch.h engine.h lgcldpnd.h
envrnbld.o: envrnbld.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bmathfun.h evaluation.h constant.h commline.h emathfun.h \
 engine.h lgcldpnd.h match.h network.h ruledef.h constrct.h userdata.h \
 moduldef.h utility.h expression.h  symbol.h agenda.h crstrtgy.h \
 constrnt.h cstrccom.h retract.h filecom.h iofun.h memalloc.h miscfun.h \
 multifun.h parsefun.h pprint.h prccode.h scanner.h prcdrfun.h prdctfun.h \
 prntutil.h proflfun.h router.h sortfun.h strngfun.h sysdep.h textpro.h \
 watch.h dffctdef.h genrccom.h genrcfun.h dffnxfun.h globldef.h \
 tmpltdef.h fact.h reorder.h pattern.h classini.h object.h multifld.h \
 objrtmch.h  extnfunc.h
envrnmnt.o: environment.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bmathfun.h evaluation.h constant.h commline.h emathfun.h \
 engine.h lgcldpnd.h match.h network.h ruledef.h constrct.h userdata.h \
 moduldef.h utility.h expression.h  symbol.h agenda.h crstrtgy.h \
 constrnt.h cstrccom.h retract.h filecom.h iofun.h memalloc.h miscfun.h \
 multifun.h parsefun.h prccode.h scanner.h prcdrfun.h prdctfun.h \
 prntutil.h proflfun.h router.h sortfun.h strngfun.h sysdep.h textpro.h \
 watch.h dffctdef.h genrccom.h genrcfun.h dffnxfun.h globldef.h \
 tmpltdef.h fact.h reorder.h pattern.h classini.h object.h multifld.h \
 objrtmch.h
evaluatn.o: evaluation.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h commline.h fact.h network.h \
 match.h ruledef.h symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h \
 tmpltdef.h scanner.h reorder.h pattern.h memalloc.h modulutl.h router.h \
 prcdrfun.h multifld.h prntutil.h  extnfunc.h proflfun.h \
 sysdep.h dffnxfun.h genrccom.h genrcfun.h object.h objrtmch.h inscom.h \
 insfun.h
expressn.o: expression.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h memalloc.h prntutil.h router.h
exprnbin.o: exprnbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h dffctdef.h cstrccom.h memalloc.h network.h match.h \
 ruledef.h agenda.h crstrtgy.h constrnt.h genrcbin.h genrcfun.h \
 dffnxbin.h dffnxfun.h fact.h tmpltdef.h scanner.h reorder.h pattern.h \
 tmpltbin.h cstrcbin.h modulbin.h globlbin.h globldef.h objbin.h object.h \
 multifld.h objrtmch.h insfun.h inscom.h
exprnops.o: exprnops.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h cstrnchk.h constrnt.h evaluation.h constant.h cstrnops.h \
 cstrnutl.h extnfunc.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h symbol.h memalloc.h prntutil.h router.h
exprnpsr.o: exprnpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h cstrnchk.h constrnt.h \
 memalloc.h modulutl.h symbol.h scanner.h pprint.h prcdrfun.h prntutil.h \
 router.h strngrtr.h network.h match.h ruledef.h agenda.h crstrtgy.h \
 cstrccom.h genrccom.h genrcfun.h dffnxfun.h  extnfunc.h
extnfunc.o: extnfunc.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h  extnfunc.h \
 symbol.h scanner.h fact.h network.h match.h ruledef.h agenda.h \
 crstrtgy.h constrnt.h cstrccom.h tmpltdef.h reorder.h pattern.h \
 memalloc.h router.h inscom.h insfun.h object.h multifld.h objrtmch.h
factbin.o: factbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h fact.h network.h match.h ruledef.h constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h expression.h  \
 symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h tmpltdef.h scanner.h \
 reorder.h pattern.h bload.h extnfunc.h  sysdep.h symblbin.h \
 bsave.h memalloc.h reteutil.h rulecom.h rulebin.h cstrcbin.h modulbin.h
factbld.o: factbld.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h fact.h network.h match.h \
 ruledef.h symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h tmpltdef.h \
 scanner.h reorder.h pattern.h memalloc.h modulutl.h reteutil.h rulecom.h \
 router.h
factcom.o: factcom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h fact.h \
 network.h match.h ruledef.h agenda.h crstrtgy.h constrnt.h cstrccom.h \
 tmpltdef.h scanner.h reorder.h pattern.h multifld.h pprint.h prntutil.h \
 router.h sysdep.h tmpltutl.h
factfile.o: factfile.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bload.h extnfunc.h symbol.h \
  sysdep.h symblbin.h cstrcpsr.h strngfun.h fact.h network.h \
 match.h ruledef.h agenda.h crstrtgy.h constrnt.h cstrccom.h tmpltdef.h \
 scanner.h reorder.h pattern.h insmngr.h object.h multifld.h objrtmch.h \
 inscom.h insfun.h memalloc.h modulpsr.h modulutl.h prntutil.h router.h \
 strngrtr.h tmpltutl.h
factfun.o: factfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h \
 multifld.h prntutil.h router.h sysdep.h tmpltutl.h constrnt.h fact.h \
 network.h match.h ruledef.h agenda.h crstrtgy.h cstrccom.h tmpltdef.h \
 scanner.h reorder.h pattern.h
factgen.o: factgen.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h  extnfunc.h expression.h  symbol.h \
 scanner.h fact.h network.h match.h ruledef.h agenda.h crstrtgy.h \
 constrnt.h cstrccom.h tmpltdef.h reorder.h pattern.h memalloc.h \
 prcdrpsr.h reteutil.h rulecom.h router.h sysdep.h tmpltfun.h tmpltlhs.h \
 tmpltutl.h
facthsh.o: facthsh.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h fact.h network.h match.h ruledef.h constrct.h \
 userdata.h moduldef.h utility.h evaluation.h expression.h  \
 symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h tmpltdef.h scanner.h \
 reorder.h pattern.h memalloc.h multifld.h router.h sysdep.h lgcldpnd.h
factlhs.o: factlhs.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h cstrcpsr.h strngfun.h modulpsr.h evaluation.h constant.h \
 moduldef.h userdata.h utility.h symbol.h scanner.h modulutl.h pattern.h \
 expression.h  constrct.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h constrnt.h cstrccom.h reorder.h pprint.h prntutil.h router.h \
 tmpltdef.h fact.h tmpltlhs.h tmpltpsr.h tmpltutl.h
factmch.o: factmch.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h drive.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h match.h network.h ruledef.h \
 symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h engine.h lgcldpnd.h \
 retract.h extnfunc.h fact.h tmpltdef.h scanner.h reorder.h pattern.h \
 incrrset.h memalloc.h prntutil.h reteutil.h rulecom.h router.h sysdep.h
factmngr.o: factmngr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h fact.h network.h match.h ruledef.h constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h expression.h  \
 symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h tmpltdef.h scanner.h \
 reorder.h pattern.h commline.h default.h engine.h lgcldpnd.h retract.h \
 memalloc.h multifld.h prntutil.h router.h strngrtr.h sysdep.h tmpltbsc.h \
 tmpltfun.h tmpltutl.h watch.h cstrnchk.h
factprt.o: factprt.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h fact.h network.h match.h ruledef.h constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h expression.h  \
 symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h tmpltdef.h scanner.h \
 reorder.h pattern.h prntutil.h router.h
factqpsr.o: factqpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h  extnfunc.h evaluation.h constant.h expression.h \
  constrct.h userdata.h moduldef.h utility.h symbol.h scanner.h \
 fact.h network.h match.h ruledef.h agenda.h crstrtgy.h constrnt.h \
 cstrccom.h tmpltdef.h reorder.h pattern.h modulutl.h prcdrpsr.h pprint.h \
 prntutil.h router.h strngrtr.h
factqury.o: factqury.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h memalloc.h  \
 extnfunc.h symbol.h scanner.h modulutl.h tmpltutl.h constrnt.h fact.h \
 network.h match.h ruledef.h agenda.h crstrtgy.h cstrccom.h tmpltdef.h \
 reorder.h pattern.h insfun.h object.h multifld.h objrtmch.h prcdrfun.h \
 prntutil.h router.h
factrete.o: factrete.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h drive.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h match.h network.h ruledef.h \
 symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h engine.h lgcldpnd.h \
 retract.h extnfunc.h fact.h tmpltdef.h scanner.h reorder.h pattern.h \
 incrrset.h memalloc.h multifld.h reteutil.h rulecom.h router.h
factrhs.o: factrhs.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h cstrcpsr.h strngfun.h  scanner.h modulutl.h \
 modulpsr.h pattern.h match.h network.h ruledef.h agenda.h crstrtgy.h \
 constrnt.h cstrccom.h reorder.h pprint.h prntutil.h router.h strngrtr.h \
 tmpltpsr.h tmpltdef.h fact.h tmpltrhs.h tmpltutl.h
filecom.o: filecom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bload.h extnfunc.h symbol.h \
  sysdep.h symblbin.h bsave.h commline.h cstrcpsr.h strngfun.h \
 fileutil.h memalloc.h router.h filecom.h
filertr.o: filertr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h memalloc.h router.h sysdep.h filertr.h
fileutil.o: fileutil.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h commline.h cstrcpsr.h \
 strngfun.h memalloc.h prcdrfun.h pprint.h prntutil.h router.h scanner.h \
 strngrtr.h sysdep.h filecom.h fileutil.h
generate.o: generate.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h  extnfunc.h \
 symbol.h scanner.h globlpsr.h memalloc.h pattern.h match.h network.h \
 ruledef.h agenda.h crstrtgy.h constrnt.h cstrccom.h reorder.h prntutil.h \
 router.h generate.h analysis.h
genrcbin.o: genrcbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h cstrcbin.h cstrccom.h genrccom.h genrcfun.h \
 memalloc.h modulbin.h objbin.h object.h constrnt.h multifld.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h objrtmch.h router.h genrcbin.h
genrccom.o: genrccom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bload.h extnfunc.h symbol.h \
  sysdep.h symblbin.h classcom.h cstrccom.h object.h constrnt.h \
 multifld.h match.h network.h ruledef.h agenda.h crstrtgy.h objrtmch.h \
 inscom.h insfun.h cstrcpsr.h strngfun.h genrcbin.h genrcfun.h genrcexe.h \
 genrcpsr.h memalloc.h modulpsr.h scanner.h modulutl.h router.h \
 strngrtr.h watch.h prntutil.h genrccom.h
genrcexe.o: genrcexe.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classfun.h scanner.h insfun.h argacces.h \
 genrccom.h genrcfun.h prcdrfun.h prccode.h prntutil.h proflfun.h \
 router.h genrcexe.h
genrcfun.o: genrcfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h classcom.h cstrccom.h object.h constrnt.h multifld.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h \
 argacces.h cstrcpsr.h strngfun.h genrccom.h genrcfun.h genrcexe.h \
 memalloc.h modulutl.h prccode.h prntutil.h router.h
genrcpsr.o: genrcpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h dffnxfun.h classfun.h object.h constrnt.h multifld.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h cstrccom.h objrtmch.h scanner.h \
 classcom.h cstrcpsr.h strngfun.h  genrccom.h genrcfun.h \
 immthpsr.h memalloc.h modulutl.h pprint.h prcdrpsr.h prccode.h \
 prntutil.h router.h genrcpsr.h
globlbin.o: globlbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h globlbsc.h globldef.h cstrccom.h memalloc.h \
 multifld.h globlbin.h modulbin.h cstrcbin.h
globlbsc.o: globlbsc.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constrct.h userdata.h moduldef.h utility.h evaluation.h \
 constant.h extnfunc.h expression.h  symbol.h globlbin.h \
 modulbin.h cstrcbin.h globldef.h cstrccom.h globlcom.h multifld.h \
 watch.h globlbsc.h
globlcom.o: globlcom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h \
 globldef.h cstrccom.h prntutil.h router.h globlcom.h
globldef.o: globldef.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h globlbin.h modulbin.h cstrcbin.h globldef.h cstrccom.h \
 commline.h globlbsc.h globlcom.h globlpsr.h memalloc.h modulpsr.h \
 scanner.h modulutl.h multifld.h prntutil.h router.h strngrtr.h
globlpsr.o: globlpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h cstrcpsr.h strngfun.h  scanner.h globlbsc.h \
 globldef.h cstrccom.h memalloc.h modulpsr.h modulutl.h multifld.h \
 pprint.h prntutil.h router.h watch.h globlpsr.h
immthpsr.o: immthpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classfun.h scanner.h cstrnutl.h  \
 extnfunc.h genrcpsr.h genrcfun.h memalloc.h prccode.h immthpsr.h
incrrset.o: incrrset.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h argacces.h drive.h engine.h lgcldpnd.h \
 retract.h pattern.h scanner.h reorder.h router.h reteutil.h rulecom.h \
 incrrset.h
inherpsr.o: inherpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classfun.h scanner.h memalloc.h modulutl.h \
 pprint.h prntutil.h router.h inherpsr.h
inscom.o: inscom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h classcom.h cstrccom.h \
 object.h constrnt.h multifld.h symbol.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h classinf.h \
 commline.h  extnfunc.h insfile.h insfun.h insmngr.h inscom.h \
 insmoddp.h insmult.h inspsr.h lgcldpnd.h memalloc.h msgcom.h msgpass.h \
 msgfun.h prntutil.h router.h strngrtr.h sysdep.h
insfile.o: insfile.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h classcom.h cstrccom.h \
 object.h constrnt.h multifld.h symbol.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h memalloc.h \
 extnfunc.h fact.h tmpltdef.h reorder.h pattern.h inscom.h insfun.h \
 insmngr.h inspsr.h prntutil.h router.h strngrtr.h symblbin.h sysdep.h \
 insfile.h
insfun.o: insfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h classcom.h cstrccom.h \
 object.h constrnt.h multifld.h symbol.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h cstrnchk.h drive.h \
 engine.h lgcldpnd.h retract.h inscom.h insfun.h insmngr.h memalloc.h \
 modulutl.h msgcom.h msgpass.h msgfun.h prccode.h prntutil.h router.h
insmngr.o: insmngr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h network.h match.h ruledef.h constrct.h userdata.h moduldef.h \
 utility.h evaluation.h constant.h expression.h  symbol.h agenda.h \
 crstrtgy.h constrnt.h cstrccom.h drive.h objrtmch.h object.h multifld.h \
 lgcldpnd.h classcom.h classfun.h scanner.h cstrnchk.h engine.h retract.h \
 extnfunc.h insfun.h memalloc.h miscfun.h modulutl.h msgcom.h msgpass.h \
 msgfun.h prccode.h prntutil.h router.h sysdep.h insmngr.h inscom.h \
 watch.h
insmoddp.o: insmoddp.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h inscom.h \
 insfun.h object.h constrnt.h multifld.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h cstrccom.h objrtmch.h insmngr.h inspsr.h memalloc.h \
 miscfun.h msgcom.h msgpass.h msgfun.h prccode.h scanner.h prntutil.h \
 router.h insmoddp.h
insmult.o: insmult.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h insfun.h \
 object.h constrnt.h multifld.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h cstrccom.h objrtmch.h msgfun.h msgpass.h multifun.h \
 prntutil.h router.h insmult.h
inspsr.o: inspsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classfun.h scanner.h classinf.h  \
 extnfunc.h pprint.h prntutil.h router.h inspsr.h
insquery.o: insquery.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h classcom.h cstrccom.h \
 object.h constrnt.h multifld.h symbol.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h  \
 extnfunc.h insfun.h insmngr.h inscom.h insqypsr.h memalloc.h prcdrfun.h \
 prntutil.h router.h insquery.h
insqypsr.o: insqypsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h  extnfunc.h scanner.h insquery.h \
 prcdrpsr.h pprint.h prntutil.h router.h strngrtr.h insqypsr.h
iofun.o: iofun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h commline.h extnfunc.h \
 symbol.h filertr.h memalloc.h miscfun.h prntutil.h router.h scanner.h \
 strngrtr.h sysdep.h iofun.h
lgcldpnd.o: lgcldpnd.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h engine.h lgcldpnd.h match.h \
 network.h ruledef.h symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h \
 retract.h fact.h tmpltdef.h scanner.h reorder.h pattern.h memalloc.h \
 reteutil.h rulecom.h router.h
maya.o: maya.c clips.h setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h memalloc.h cstrcpsr.h \
 strngfun.h fileutil.h  extnfunc.h symbol.h commline.h \
 prntutil.h router.h filertr.h strngrtr.h iofun.h sysdep.h bmathfun.h \
  scanner.h miscfun.h watch.h modulbsc.h bload.h  \
 symblbin.h bsave.h ruledef.h network.h match.h agenda.h crstrtgy.h \
 constrnt.h cstrccom.h rulebsc.h engine.h lgcldpnd.h retract.h drive.h \
 incrrset.h rulecom.h dffctdef.h dffctbsc.h tmpltdef.h fact.h reorder.h \
 pattern.h tmpltbsc.h tmpltfun.h globldef.h globlbsc.h globlcom.h \
 dffnxfun.h genrccom.h genrcfun.h classcom.h object.h multifld.h \
 objrtmch.h classexm.h classfun.h classinf.h classini.h classpsr.h \
 defins.h inscom.h insfun.h insfile.h insmngr.h msgcom.h msgpass.h maya.h \
 mayasetup.h boost.h functional.h
memalloc.o: memalloc.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h memalloc.h prntutil.h router.h utility.h \
 evaluation.h moduldef.h userdata.h
miscfun.o: miscfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h  extnfunc.h \
 symbol.h scanner.h memalloc.h multifld.h prntutil.h router.h sysdep.h \
 dffnxfun.h fact.h network.h match.h ruledef.h agenda.h crstrtgy.h \
 constrnt.h cstrccom.h tmpltdef.h reorder.h pattern.h tmpltutl.h \
 miscfun.h
modulbin.o: modulbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h cstrcbin.h memalloc.h modulbin.h
modulbsc.o: modulbsc.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bload.h extnfunc.h symbol.h \
  sysdep.h symblbin.h modulbin.h cstrcbin.h multifld.h \
 prntutil.h router.h modulbsc.h
moduldef.o: moduldef.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bload.h extnfunc.h symbol.h \
  sysdep.h symblbin.h modulbin.h cstrcbin.h memalloc.h \
 modulbsc.h modulpsr.h scanner.h prntutil.h router.h
modulpsr.o: modulpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h cstrcpsr.h strngfun.h \
 extnfunc.h symbol.h memalloc.h modulutl.h scanner.h pprint.h prntutil.h \
 router.h bload.h  sysdep.h symblbin.h modulpsr.h
modulutl.o: modulutl.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h cstrcpsr.h strngfun.h memalloc.h modulpsr.h evaluation.h \
 constant.h moduldef.h userdata.h utility.h symbol.h scanner.h pprint.h \
 prntutil.h router.h sysdep.h watch.h expression.h  constrct.h \
 modulutl.h
msgcom.o: msgcom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bload.h extnfunc.h symbol.h \
  sysdep.h symblbin.h classcom.h cstrccom.h object.h constrnt.h \
 multifld.h match.h network.h ruledef.h agenda.h crstrtgy.h objrtmch.h \
 classfun.h scanner.h classinf.h msgpsr.h insfun.h insmoddp.h msgfun.h \
 msgpass.h memalloc.h prccode.h prntutil.h router.h watch.h msgcom.h
msgfun.o: msgfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classfun.h scanner.h extnfunc.h inscom.h insfun.h \
 memalloc.h msgcom.h msgpass.h prccode.h prntutil.h router.h msgfun.h
msgpass.o: msgpass.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h classcom.h cstrccom.h \
 object.h constrnt.h multifld.h symbol.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h commline.h \
  extnfunc.h inscom.h insfun.h memalloc.h msgcom.h msgpass.h \
 msgfun.h prccode.h prcdrfun.h prntutil.h proflfun.h router.h strngfun.h
msgpsr.o: msgpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h classcom.h cstrccom.h object.h constrnt.h multifld.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h objrtmch.h classfun.h scanner.h \
 cstrcpsr.h strngfun.h cstrnchk.h  insfun.h memalloc.h \
 modulutl.h msgcom.h msgpass.h msgfun.h pprint.h prccode.h prntutil.h \
 router.h strngrtr.h msgpsr.h
multifld.o: multifld.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h evaluation.h  expression.h constrct.h \
 userdata.h moduldef.h utility.h memalloc.h object.h constrnt.h \
 multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
 cstrccom.h objrtmch.h scanner.h prntutil.h router.h strngrtr.h
multifun.o: multifun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h  extnfunc.h \
 symbol.h scanner.h memalloc.h multifld.h multifun.h object.h constrnt.h \
 match.h network.h ruledef.h agenda.h crstrtgy.h cstrccom.h objrtmch.h \
 pprint.h prcdrpsr.h prcdrfun.h prntutil.h router.h
objbin.o: objbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h classcom.h cstrccom.h object.h constrnt.h multifld.h \
 match.h network.h ruledef.h agenda.h crstrtgy.h objrtmch.h classfun.h \
 scanner.h classini.h cstrcbin.h cstrnbin.h insfun.h memalloc.h \
 modulbin.h msgcom.h msgpass.h msgfun.h prntutil.h router.h objrtbin.h \
 objbin.h
objrtbin.o: objrtbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h classfun.h object.h constrnt.h multifld.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h cstrccom.h objrtmch.h scanner.h \
 classcom.h memalloc.h insfun.h pattern.h reorder.h reteutil.h rulecom.h \
 rulebin.h cstrcbin.h modulbin.h objrtbin.h
objrtbld.o: objrtbld.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classfun.h scanner.h cstrnutl.h cstrnchk.h \
 cstrnops.h drive.h inscom.h insfun.h insmngr.h memalloc.h pattern.h \
 reorder.h prntutil.h reteutil.h rulecom.h rulepsr.h  \
 extnfunc.h objrtgen.h objrtfnx.h pprint.h router.h objrtbin.h objrtbld.h
objrtfnx.o: objrtfnx.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classcom.h cstrccom.h moduldef.h userdata.h utility.h \
 evaluation.h constant.h constrct.h object.h constrnt.h expression.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h \
 crstrtgy.h objrtmch.h classfun.h scanner.h bload.h extnfunc.h  \
 sysdep.h symblbin.h drive.h engine.h lgcldpnd.h retract.h memalloc.h \
 prntutil.h reteutil.h rulecom.h router.h objrtfnx.h
objrtgen.o: objrtgen.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classfun.h object.h constrct.h userdata.h moduldef.h \
 utility.h evaluation.h constant.h constrnt.h expression.h  \
 multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
 cstrccom.h objrtmch.h scanner.h classcom.h objrtfnx.h objrtgen.h \
 reorder.h pattern.h
objrtmch.o: objrtmch.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h classfun.h object.h constrct.h userdata.h moduldef.h \
 utility.h evaluation.h constant.h constrnt.h expression.h  \
 multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
 cstrccom.h objrtmch.h scanner.h classcom.h memalloc.h drive.h engine.h \
 lgcldpnd.h retract.h incrrset.h objrtfnx.h prntutil.h reteutil.h \
 rulecom.h ruledlt.h reorder.h pattern.h router.h insmngr.h inscom.h \
 insfun.h
parsefun.o: parsefun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h cstrcpsr.h strngfun.h \
  extnfunc.h symbol.h scanner.h memalloc.h multifld.h pprint.h \
 prcdrpsr.h constrnt.h prntutil.h router.h strngrtr.h parsefun.h
pattern.o: pattern.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h constrnt.h evaluation.h cstrnchk.h cstrnutl.h \
  extnfunc.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h symbol.h scanner.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h cstrccom.h memalloc.h pprint.h prntutil.h reteutil.h \
 rulecom.h router.h pattern.h reorder.h
pprint.o: pprint.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h memalloc.h sysdep.h utility.h evaluation.h \
 moduldef.h userdata.h pprint.h
prccode.o: prccode.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h memalloc.h constant.h globlpsr.h expression.h  \
 constrct.h userdata.h moduldef.h utility.h evaluation.h  \
 extnfunc.h symbol.h scanner.h multifld.h object.h constrnt.h match.h \
 network.h ruledef.h agenda.h crstrtgy.h cstrccom.h objrtmch.h pprint.h \
 prcdrpsr.h prntutil.h router.h prccode.h
prcdrfun.o: prcdrfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h constrnt.h cstrnchk.h \
 cstrnops.h  extnfunc.h symbol.h scanner.h memalloc.h \
 multifld.h prcdrpsr.h router.h prcdrfun.h globldef.h cstrccom.h
prcdrpsr.o: prcdrpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h constrnt.h cstrnchk.h \
 cstrnops.h cstrnutl.h  extnfunc.h symbol.h scanner.h \
 memalloc.h modulutl.h multifld.h pprint.h prntutil.h router.h prcdrpsr.h \
 globldef.h cstrccom.h globlpsr.h
prdctfun.o: prdctfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h  extnfunc.h \
 symbol.h scanner.h multifld.h router.h prdctfun.h
prntutil.o: prntutil.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h cstrcpsr.h strngfun.h fact.h \
 network.h match.h ruledef.h symbol.h agenda.h crstrtgy.h constrnt.h \
 cstrccom.h tmpltdef.h scanner.h reorder.h pattern.h inscom.h insfun.h \
 object.h multifld.h objrtmch.h insmngr.h memalloc.h multifun.h router.h \
 strngrtr.h sysdep.h prntutil.h
proflfun.o: proflfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h classcom.h cstrccom.h \
 object.h constrnt.h multifld.h symbol.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h objrtmch.h dffnxfun.h extnfunc.h genrccom.h \
 genrcfun.h memalloc.h msgcom.h msgpass.h router.h sysdep.h proflfun.h
reorder.o: reorder.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h cstrnutl.h constrnt.h evaluation.h constant.h extnfunc.h \
 expression.h  constrct.h userdata.h moduldef.h utility.h \
 symbol.h memalloc.h pattern.h scanner.h match.h network.h ruledef.h \
 agenda.h crstrtgy.h cstrccom.h reorder.h prntutil.h router.h rulelhs.h
reteutil.o: reteutil.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h drive.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h match.h network.h ruledef.h \
 symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h engine.h lgcldpnd.h \
 retract.h incrrset.h memalloc.h pattern.h scanner.h reorder.h prntutil.h \
 router.h rulecom.h reteutil.h
retract.o: retract.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h argacces.h drive.h engine.h lgcldpnd.h \
 retract.h memalloc.h prntutil.h reteutil.h rulecom.h router.h
router.o: router.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h filertr.h \
 memalloc.h prntutil.h scanner.h strngrtr.h sysdep.h router.h
rulebin.o: rulebin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h bload.h extnfunc.h  sysdep.h \
 symblbin.h bsave.h engine.h lgcldpnd.h retract.h memalloc.h pattern.h \
 scanner.h reorder.h reteutil.h rulecom.h rulebsc.h rulebin.h cstrcbin.h \
 modulbin.h
rulebld.o: rulebld.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h drive.h expression.h  match.h network.h ruledef.h \
 symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h incrrset.h memalloc.h \
 pattern.h scanner.h reorder.h prntutil.h reteutil.h rulecom.h router.h \
 rulebld.h rulepsr.h watch.h
rulebsc.o: rulebsc.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h drive.h match.h network.h \
 ruledef.h symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h engine.h \
 lgcldpnd.h retract.h extnfunc.h multifld.h reteutil.h rulecom.h router.h \
 watch.h rulebin.h cstrcbin.h modulbin.h rulebsc.h
rulecom.o: rulecom.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h crstrtgy.h agenda.h ruledef.h \
 network.h match.h symbol.h constrnt.h cstrccom.h engine.h lgcldpnd.h \
 retract.h extnfunc.h incrrset.h memalloc.h multifld.h pattern.h \
 scanner.h reorder.h prntutil.h reteutil.h rulecom.h router.h ruledlt.h \
 sysdep.h watch.h rulebin.h cstrcbin.h modulbin.h
rulecstr.o: rulecstr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h analysis.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h reorder.h pattern.h symbol.h \
 scanner.h match.h network.h ruledef.h agenda.h crstrtgy.h constrnt.h \
 cstrccom.h cstrnchk.h cstrnops.h cstrnutl.h extnfunc.h prcdrpsr.h \
 prntutil.h router.h rulepsr.h rulecstr.h
ruledef.o: ruledef.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h drive.h engine.h lgcldpnd.h retract.h \
 memalloc.h pattern.h scanner.h reorder.h reteutil.h rulecom.h rulebsc.h \
 rulepsr.h ruledlt.h bload.h extnfunc.h  sysdep.h symblbin.h \
 rulebin.h cstrcbin.h modulbin.h
ruledlt.o: ruledlt.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h bload.h extnfunc.h  sysdep.h \
 symblbin.h drive.h engine.h lgcldpnd.h retract.h memalloc.h pattern.h \
 scanner.h reorder.h reteutil.h rulecom.h ruledlt.h
rulelhs.o: rulelhs.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h agenda.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constant.h expression.h  network.h match.h symbol.h \
 constrnt.h cstrccom.h crstrtgy.h argacces.h cstrnchk.h  \
 extnfunc.h scanner.h memalloc.h pattern.h reorder.h pprint.h prntutil.h \
 router.h rulelhs.h
rulepsr.o: rulepsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h analysis.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h reorder.h pattern.h symbol.h \
 scanner.h match.h network.h ruledef.h agenda.h crstrtgy.h constrnt.h \
 cstrccom.h cstrcpsr.h strngfun.h cstrnchk.h cstrnops.h engine.h \
 lgcldpnd.h retract.h  extnfunc.h incrrset.h memalloc.h \
 modulutl.h prccode.h prcdrpsr.h pprint.h prntutil.h router.h rulebld.h \
 rulebsc.h rulecstr.h ruledlt.h rulelhs.h watch.h tmpltfun.h fact.h \
 tmpltdef.h bload.h  sysdep.h symblbin.h rulepsr.h
scanner.o: scanner.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h memalloc.h pprint.h prntutil.h router.h symbol.h \
 sysdep.h utility.h evaluation.h moduldef.h userdata.h scanner.h
sortfun.o: sortfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h dffnxfun.h extnfunc.h \
 symbol.h memalloc.h multifld.h sysdep.h sortfun.h
strngfun.o: strngfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h commline.h cstrcpsr.h \
 strngfun.h engine.h lgcldpnd.h match.h network.h ruledef.h symbol.h \
 agenda.h crstrtgy.h constrnt.h cstrccom.h retract.h  \
 extnfunc.h scanner.h memalloc.h miscfun.h multifld.h prcdrpsr.h pprint.h \
 prntutil.h router.h strngrtr.h sysdep.h drive.h
strngrtr.o: strngrtr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h memalloc.h prntutil.h router.h sysdep.h strngrtr.h \
 utility.h evaluation.h moduldef.h userdata.h
symblbin.o: symblbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h bload.h extnfunc.h symbol.h \
  sysdep.h symblbin.h bsave.h cstrnbin.h constrnt.h  \
 scanner.h memalloc.h router.h
symbol.o: symbol.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h memalloc.h multifld.h \
 prntutil.h router.h sysdep.h symbol.h
sysdep.o: sysdep.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h memalloc.h sysdep.h
textpro.o: textpro.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h commline.h extnfunc.h \
 symbol.h memalloc.h prntutil.h router.h sysdep.h textpro.h
tmpltbin.o: tmpltbin.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h bsave.h cstrnbin.h constrnt.h fact.h network.h match.h \
 ruledef.h agenda.h crstrtgy.h cstrccom.h tmpltdef.h scanner.h reorder.h \
 pattern.h memalloc.h tmpltpsr.h tmpltutl.h tmpltbin.h cstrcbin.h \
 modulbin.h
tmpltbsc.o: tmpltbsc.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h cstrccom.h cstrcpsr.h \
 strngfun.h extnfunc.h symbol.h fact.h network.h match.h ruledef.h \
 agenda.h crstrtgy.h constrnt.h tmpltdef.h scanner.h reorder.h pattern.h \
 memalloc.h multifld.h router.h tmpltbin.h cstrcbin.h modulbin.h \
 tmpltpsr.h tmpltutl.h tmpltbsc.h
tmpltdef.o: tmpltdef.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h cstrccom.h moduldef.h userdata.h utility.h evaluation.h \
 constant.h constrct.h cstrnchk.h constrnt.h  expression.h \
 memalloc.h modulpsr.h symbol.h scanner.h modulutl.h network.h match.h \
 ruledef.h agenda.h crstrtgy.h pattern.h reorder.h router.h tmpltbsc.h \
 tmpltdef.h fact.h tmpltfun.h tmpltpsr.h tmpltutl.h bload.h extnfunc.h \
  sysdep.h symblbin.h tmpltbin.h cstrcbin.h modulbin.h
tmpltfun.o: tmpltfun.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h commline.h cstrnchk.h \
 constrnt.h default.h  extnfunc.h symbol.h scanner.h fact.h \
 network.h match.h ruledef.h agenda.h crstrtgy.h cstrccom.h tmpltdef.h \
 reorder.h pattern.h memalloc.h modulutl.h multifld.h pprint.h prcdrpsr.h \
 prntutil.h router.h sysdep.h tmpltlhs.h tmpltrhs.h tmpltutl.h tmpltfun.h
tmpltlhs.o: tmpltlhs.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h constant.h constrct.h userdata.h moduldef.h utility.h \
 evaluation.h constrnt.h  extnfunc.h expression.h  \
 symbol.h scanner.h fact.h network.h match.h ruledef.h agenda.h \
 crstrtgy.h cstrccom.h tmpltdef.h reorder.h pattern.h memalloc.h \
 modulutl.h pprint.h prntutil.h router.h tmpltutl.h tmpltlhs.h
tmpltpsr.o: tmpltpsr.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h bload.h utility.h evaluation.h constant.h moduldef.h userdata.h \
 extnfunc.h expression.h  constrct.h symbol.h  sysdep.h \
 symblbin.h cstrcpsr.h strngfun.h cstrnchk.h constrnt.h cstrnpsr.h \
 cstrnutl.h default.h  scanner.h fact.h network.h match.h \
 ruledef.h agenda.h crstrtgy.h cstrccom.h tmpltdef.h reorder.h pattern.h \
 memalloc.h modulutl.h pprint.h prntutil.h router.h tmpltbsc.h watch.h \
 tmpltpsr.h
tmpltrhs.o: tmpltrhs.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h default.h constrnt.h evaluation.h constant.h extnfunc.h \
 expression.h  constrct.h userdata.h moduldef.h utility.h \
 symbol.h fact.h network.h match.h ruledef.h agenda.h crstrtgy.h \
 cstrccom.h tmpltdef.h scanner.h reorder.h pattern.h memalloc.h \
 modulutl.h pprint.h prntutil.h router.h tmpltfun.h tmpltlhs.h tmpltutl.h \
 tmpltrhs.h
tmpltutl.o: tmpltutl.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h cstrnchk.h constrnt.h \
 extnfunc.h symbol.h memalloc.h modulutl.h scanner.h multifld.h \
 prntutil.h router.h sysdep.h tmpltbsc.h tmpltdef.h fact.h network.h \
 match.h ruledef.h agenda.h crstrtgy.h cstrccom.h reorder.h pattern.h \
 tmpltfun.h tmpltpsr.h watch.h tmpltutl.h
userdata.o: userdata.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h userdata.h
userfunctions.o: userfunctions.c clips.h setup.h os_shim.h platform.h \
 environment.h entities.h usrsetup.h argacces.h expression.h  \
 constrct.h userdata.h moduldef.h utility.h evaluation.h constant.h \
 memalloc.h cstrcpsr.h strngfun.h fileutil.h  extnfunc.h \
 symbol.h commline.h prntutil.h router.h filertr.h strngrtr.h iofun.h \
 sysdep.h bmathfun.h  scanner.h miscfun.h watch.h modulbsc.h \
 bload.h  symblbin.h bsave.h ruledef.h network.h match.h \
 agenda.h crstrtgy.h constrnt.h cstrccom.h rulebsc.h engine.h lgcldpnd.h \
 retract.h drive.h incrrset.h rulecom.h dffctdef.h dffctbsc.h tmpltdef.h \
 fact.h reorder.h pattern.h tmpltbsc.h tmpltfun.h globldef.h globlbsc.h \
 globlcom.h dffnxfun.h genrccom.h genrcfun.h classcom.h object.h \
 multifld.h objrtmch.h classexm.h classfun.h classinf.h classini.h \
 classpsr.h defins.h inscom.h insfun.h insfile.h insmngr.h msgcom.h \
 msgpass.h maya.h
utility.o: utility.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h commline.h evaluation.h constant.h fact.h network.h match.h \
 ruledef.h constrct.h userdata.h moduldef.h utility.h expression.h \
  symbol.h agenda.h crstrtgy.h constrnt.h cstrccom.h tmpltdef.h \
 scanner.h reorder.h pattern.h memalloc.h multifld.h prntutil.h router.h \
 sysdep.h
watch.o: watch.c setup.h os_shim.h platform.h environment.h entities.h \
 usrsetup.h argacces.h expression.h  constrct.h userdata.h \
 moduldef.h utility.h evaluation.h constant.h extnfunc.h symbol.h \
 memalloc.h router.h watch.h
main.o: cmd/repl/main.c clips.h setup.h os_shim.h platform.h environment.h \
 entities.h usrsetup.h argacces.h expression.h  constrct.h \
 userdata.h moduldef.h utility.h evaluation.h constant.h memalloc.h \
 cstrcpsr.h strngfun.h fileutil.h  extnfunc.h symbol.h \
 commline.h prntutil.h router.h filertr.h strngrtr.h iofun.h sysdep.h \
 bmathfun.h  scanner.h miscfun.h watch.h modulbsc.h bload.h \
  symblbin.h bsave.h ruledef.h network.h match.h agenda.h \
 crstrtgy.h constrnt.h cstrccom.h rulebsc.h engine.h lgcldpnd.h retract.h \
 drive.h incrrset.h rulecom.h dffctdef.h dffctbsc.h tmpltdef.h fact.h \
 reorder.h pattern.h tmpltbsc.h tmpltfun.h globldef.h globlbsc.h \
 globlcom.h dffnxfun.h genrccom.h genrcfun.h classcom.h object.h \
 multifld.h objrtmch.h classexm.h classfun.h classinf.h classini.h \
 classpsr.h defins.h inscom.h insfun.h insfile.h insmngr.h msgcom.h \
 msgpass.h



boost.o: boost.cc clips.h setup.h os_shim.h platform.h environment.h \
 entities.h usrsetup.h argacces.h expression.h  constrct.h \
 userdata.h moduldef.h utility.h evaluation.h constant.h memalloc.h \
 cstrcpsr.h strngfun.h fileutil.h  extnfunc.h symbol.h \
 commline.h prntutil.h router.h filertr.h strngrtr.h iofun.h sysdep.h \
 bmathfun.h  scanner.h miscfun.h watch.h modulbsc.h bload.h \
  symblbin.h bsave.h ruledef.h network.h match.h agenda.h \
 crstrtgy.h constrnt.h cstrccom.h rulebsc.h engine.h lgcldpnd.h retract.h \
 drive.h incrrset.h rulecom.h dffctdef.h dffctbsc.h tmpltdef.h fact.h \
 reorder.h pattern.h tmpltbsc.h tmpltfun.h globldef.h globlbsc.h \
 globlcom.h dffnxfun.h genrccom.h genrcfun.h classcom.h object.h \
 multifld.h objrtmch.h classexm.h classfun.h classinf.h classini.h \
 classpsr.h defins.h inscom.h insfun.h insfile.h insmngr.h msgcom.h \
 msgpass.h mayasetup.h boost.h
functional.o: functional.cpp clips.h setup.h os_shim.h platform.h \
 environment.h entities.h usrsetup.h argacces.h expression.h  \
 constrct.h userdata.h moduldef.h utility.h evaluation.h constant.h \
 memalloc.h cstrcpsr.h strngfun.h fileutil.h  extnfunc.h \
 symbol.h commline.h prntutil.h router.h filertr.h strngrtr.h iofun.h \
 sysdep.h bmathfun.h  scanner.h miscfun.h watch.h modulbsc.h \
 bload.h  symblbin.h bsave.h ruledef.h network.h match.h \
 agenda.h crstrtgy.h constrnt.h cstrccom.h rulebsc.h engine.h lgcldpnd.h \
 retract.h drive.h incrrset.h rulecom.h dffctdef.h dffctbsc.h tmpltdef.h \
 fact.h reorder.h pattern.h tmpltbsc.h tmpltfun.h globldef.h globlbsc.h \
 globlcom.h dffnxfun.h genrccom.h genrcfun.h classcom.h object.h \
 multifld.h objrtmch.h classexm.h classfun.h classinf.h classini.h \
 classpsr.h defins.h inscom.h insfun.h insfile.h insmngr.h msgcom.h \
 msgpass.h mayasetup.h functional.h
