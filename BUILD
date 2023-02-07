c_library(
    name = "platform",
    hdrs = ["platform/os.h", "platform/types.h"],
)
c_library(
    name = "clips",
    compiler_flags = [ "--std=c99", "-Wno-error" ],
    deps = [ "//:platform" ],
    srcs = [ "clips/agenda.c", "clips/analysis.c", "clips/argacces.c", "clips/bload.c", 
            "clips/bmathfun.c", "clips/bsave.c", "clips/classcom.c", "clips/classexm.c",
            "clips/classfun.c", "clips/classinf.c", "clips/classini.c", "clips/classpsr.c",
            "clips/clsltpsr.c", "clips/commline.c", "clips/conscomp.c", "clips/constrct.c",
            "clips/constrnt.c", "clips/crstrtgy.c", "clips/cstrcbin.c", "clips/cstrccom.c",
            "clips/cstrcpsr.c", "clips/cstrnbin.c", "clips/cstrnchk.c", "clips/cstrncmp.c",
            "clips/cstrnops.c", "clips/cstrnpsr.c", "clips/cstrnutl.c", "clips/default.c",
            "clips/defins.c", "clips/developr.c", "clips/dffctbin.c", "clips/dffctbsc.c",
            "clips/dffctcmp.c", "clips/dffctdef.c", "clips/dffctpsr.c", "clips/dffnxbin.c",
            "clips/dffnxcmp.c", "clips/dffnxexe.c", "clips/dffnxfun.c", "clips/dffnxpsr.c",
            "clips/dfinsbin.c", "clips/dfinscmp.c", "clips/drive.c", "clips/emathfun.c",
            "clips/engine.c", "clips/envrnbld.c", "clips/envrnmnt.c", "clips/evaluatn.c",
            "clips/expressn.c", "clips/exprnbin.c", "clips/exprnops.c", "clips/exprnpsr.c",
            "clips/extnfunc.c", "clips/factbin.c", "clips/factbld.c", "clips/factcmp.c",
            "clips/factcom.c", "clips/factfile.c", "clips/factfun.c", "clips/factgen.c",
            "clips/facthsh.c", "clips/factlhs.c", "clips/factmch.c", "clips/factmngr.c",
            "clips/factprt.c", "clips/factqpsr.c", "clips/factqury.c", "clips/factrete.c",
            "clips/factrhs.c", "clips/filecom.c", "clips/filertr.c", "clips/fileutil.c",
            "clips/generate.c", "clips/genrcbin.c", "clips/genrccmp.c", "clips/genrccom.c",
            "clips/genrcexe.c", "clips/genrcfun.c", "clips/genrcpsr.c", "clips/globlbin.c",
            "clips/globlbsc.c", "clips/globlcmp.c", "clips/globlcom.c", "clips/globldef.c",
            "clips/globlpsr.c", "clips/immthpsr.c", "clips/incrrset.c", "clips/inherpsr.c",
            "clips/inscom.c", "clips/insfile.c", "clips/insfun.c", "clips/insmngr.c",
            "clips/insmoddp.c", "clips/insmult.c", "clips/inspsr.c", "clips/insquery.c",
            "clips/insqypsr.c", "clips/iofun.c", "clips/lgcldpnd.c", "clips/memalloc.c",
            "clips/miscfun.c", "clips/modulbin.c", "clips/modulbsc.c", "clips/modulcmp.c",
            "clips/moduldef.c", "clips/modulpsr.c", "clips/modulutl.c", "clips/msgcom.c",
            "clips/msgfun.c", "clips/msgpass.c", "clips/msgpsr.c", "clips/multifld.c",
            "clips/multifun.c", "clips/objbin.c", "clips/objcmp.c", "clips/objrtbin.c",
            "clips/objrtbld.c", "clips/objrtcmp.c", "clips/objrtfnx.c", "clips/objrtgen.c",
            "clips/objrtmch.c", "clips/parsefun.c", "clips/pattern.c", "clips/pprint.c",
            "clips/prccode.c", "clips/prcdrfun.c", "clips/prcdrpsr.c", "clips/prdctfun.c",
            "clips/prntutil.c", "clips/proflfun.c", "clips/reorder.c", "clips/reteutil.c",
            "clips/retract.c", "clips/router.c", "clips/rulebin.c", "clips/rulebld.c",
            "clips/rulebsc.c", "clips/rulecmp.c", "clips/rulecom.c", "clips/rulecstr.c",
            "clips/ruledef.c", "clips/ruledlt.c", "clips/rulelhs.c", "clips/rulepsr.c",
            "clips/scanner.c", "clips/sortfun.c", "clips/strngfun.c", "clips/strngrtr.c",
            "clips/symblbin.c", "clips/symblcmp.c", "clips/symbol.c", "clips/sysdep.c",
            "clips/textpro.c", "clips/tmpltbin.c", "clips/tmpltbsc.c", "clips/tmpltcmp.c",
            "clips/tmpltdef.c", "clips/tmpltfun.c", "clips/tmpltlhs.c", "clips/tmpltpsr.c",
            "clips/tmpltrhs.c", "clips/tmpltutl.c", "clips/userdata.c", "clips/userfunctions.c",
            "clips/utility.c", "clips/watch.c", ],

    hdrs = [
"clips/agenda.h",
"clips/analysis.h",
"clips/argacces.h",
"clips/bload.h",
"clips/bmathfun.h",
"clips/bsave.h",
"clips/classcom.h",
"clips/classexm.h",
"clips/classfun.h",
"clips/classinf.h",
"clips/classini.h",
"clips/classpsr.h",
"clips/clips.h",
"clips/clsltpsr.h",
"clips/commline.h",
"clips/conscomp.h",
"clips/constant.h",
"clips/constrct.h",
"clips/constrnt.h",
"clips/crstrtgy.h",
"clips/cstrcbin.h",
"clips/cstrccmp.h",
"clips/cstrccom.h",
"clips/cstrcpsr.h",
"clips/cstrnbin.h",
"clips/cstrnchk.h",
"clips/cstrncmp.h",
"clips/cstrnops.h",
"clips/cstrnpsr.h",
"clips/cstrnutl.h",
"clips/default.h",
"clips/defins.h",
"clips/developr.h",
"clips/dffctbin.h",
"clips/dffctbsc.h",
"clips/dffctcmp.h",
"clips/dffctdef.h",
"clips/dffctpsr.h",
"clips/dffnxbin.h",
"clips/dffnxcmp.h",
"clips/dffnxexe.h",
"clips/dffnxfun.h",
"clips/dffnxpsr.h",
"clips/dfinsbin.h",
"clips/dfinscmp.h",
"clips/drive.h",
"clips/emathfun.h",
"clips/engine.h",
"clips/entities.h",
"clips/envrnbld.h",
"clips/envrnmnt.h",
"clips/evaluatn.h",
"clips/expressn.h",
"clips/exprnbin.h",
"clips/exprnops.h",
"clips/exprnpsr.h",
"clips/extnfunc.h",
"clips/factbin.h",
"clips/factbld.h",
"clips/factcmp.h",
"clips/factcom.h",
"clips/factfile.h",
"clips/factfun.h",
"clips/factgen.h",
"clips/facthsh.h",
"clips/factlhs.h",
"clips/factmch.h",
"clips/factmngr.h",
"clips/factprt.h",
"clips/factqpsr.h",
"clips/factqury.h",
"clips/factrete.h",
"clips/factrhs.h",
"clips/filecom.h",
"clips/filertr.h",
"clips/fileutil.h",
"clips/generate.h",
"clips/genrcbin.h",
"clips/genrccmp.h",
"clips/genrccom.h",
"clips/genrcexe.h",
"clips/genrcfun.h",
"clips/genrcpsr.h",
"clips/globlbin.h",
"clips/globlbsc.h",
"clips/globlcmp.h",
"clips/globlcom.h",
"clips/globldef.h",
"clips/globlpsr.h",
"clips/immthpsr.h",
"clips/incrrset.h",
"clips/inherpsr.h",
"clips/inscom.h",
"clips/insfile.h",
"clips/insfun.h",
"clips/insmngr.h",
"clips/insmoddp.h",
"clips/insmult.h",
"clips/inspsr.h",
"clips/insquery.h",
"clips/insqypsr.h",
"clips/iofun.h",
"clips/lgcldpnd.h",
"clips/match.h",
"clips/memalloc.h",
"clips/miscfun.h",
"clips/modulbin.h",
"clips/modulbsc.h",
"clips/modulcmp.h",
"clips/moduldef.h",
"clips/modulpsr.h",
"clips/modulutl.h",
"clips/msgcom.h",
"clips/msgfun.h",
"clips/msgpass.h",
"clips/msgpsr.h",
"clips/multifld.h",
"clips/multifun.h",
"clips/network.h",
"clips/objbin.h",
"clips/objcmp.h",
"clips/object.h",
"clips/objrtbin.h",
"clips/objrtbld.h",
"clips/objrtcmp.h",
"clips/objrtfnx.h",
"clips/objrtgen.h",
"clips/objrtmch.h",
"clips/parsefun.h",
"clips/pattern.h",
"clips/pprint.h",
"clips/prccode.h",
"clips/prcdrfun.h",
"clips/prcdrpsr.h",
"clips/prdctfun.h",
"clips/prntutil.h",
"clips/proflfun.h",
"clips/reorder.h",
"clips/reteutil.h",
"clips/retract.h",
"clips/router.h",
"clips/rulebin.h",
"clips/rulebld.h",
"clips/rulebsc.h",
"clips/rulecmp.h",
"clips/rulecom.h",
"clips/rulecstr.h",
"clips/ruledef.h",
"clips/ruledlt.h",
"clips/rulelhs.h",
"clips/rulepsr.h",
"clips/scanner.h",
"clips/setup.h",
"clips/sortfun.h",
"clips/strngfun.h",
"clips/strngrtr.h",
"clips/symblbin.h",
"clips/symblcmp.h",
"clips/symbol.h",
"clips/sysdep.h",
"clips/textpro.h",
"clips/tmpltbin.h",
"clips/tmpltbsc.h",
"clips/tmpltcmp.h",
"clips/tmpltdef.h",
"clips/tmpltfun.h",
"clips/tmpltlhs.h",
"clips/tmpltpsr.h",
"clips/tmpltrhs.h",
"clips/tmpltutl.h",
"clips/userdata.h",
"clips/usrsetup.h",
"clips/utility.h",
"clips/watch.h",
    ]
)
