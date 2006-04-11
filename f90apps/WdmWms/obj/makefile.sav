HLOC      = ..\..\hasslibs
SRCLOC    = ..\src
BINLOC    = ..\bin

FC        = lf90
FFLAGS    = -c -o0 -i $(SRCLOC)

DLL       = $(BINLOC)\WdmWms.dll
LDMAP     = $(BINLOC)\WdmWms.map
LBLOC     = \lib3.0\lf90libs
LIBS      = adwdm.lib \
            wdm.lib \
            util.lib

OBJX      = wdmwms.obj

$(DLL): $(OBJX) #$(LIBS)
          $(FC) $(OBJX) -MAP $(LDMAP) -OUT $(DLL) \
               -LIB $(LIBS) -LIBP $(LBLOC) \
               -win -dll -ml msvb -mapn 32 -mapw 132 \
               @..\src\wdmwms.xp$$
          !if -e $(SRCLOC)\*.dll
            copy $(SRCLOC)\*.dll ..\bin
            erase $(SRCLOC)\*.dll
            erase $(SRCLOC)\*.def
            erase $(SRCLOC)\*.xpo
            erase $(SRCLOC)\*.obj
          !endif

$(OBJX): $(SRCLOC)\$(*).f90
     $(FC) $(SRCLOC)\$(*).f90 -nfix $(FFLAGS) -win -dll -ml msvb
     copy $(SRCLOC)\$(*).obj
     erase $(SRCLOC)\$(*).obj
