#
# MKMF template makefile for protected mode executables.
#
FC        = LF90
PROGRAM   = ..\bin\hspfbat.exe
LDMAP     = ..\bin\hspfbat.map

MODLOC    = \f90apps\hspfmod\obj

LSLOC     = \lib3.0\src
LSLOCH    = $(LSLOC)\hspf12
FFLAGS    = -c -g -mod $(MODLOC) -i $(LSLOCH);$(LSLOC)\aide
LDFLAGS   = -bind -pack -lisk # -bind -win32s
LBLOC     = \lib3.0\lf90libs


LIBS      = hspf12.lib     \
            hspnodss.lib \
            uci.lib      \
            osv.lib      \
            tsins.lib    \
            wdm.lib      \
            adwdm.lib    \
            util.lib

OLOC      = ..\..\hspf11.0\obj
OBJS      = $(OLOC)\hspfbat.obj $(OLOC)\hspsta.obj \
            hiouci.obj hiotsin.obj hioosv.obj

VPATH     = ..\src

$(PROGRAM): $(OBJS)  $(TOBJ) #$(LIBS)
          !if -e $(VPATH)\*.obj
            copy $(VPATH)\*.obj
          !endif
          $(FC) $(OBJS) $(TOBJ) -EXE $@ -MAP $(LDMAP) \
               -LIB $(LIBS) -LIBP $(LBLOC);$(MODLOC) $(LDFLAGS)
          !if -e $(VPATH)\*.obj
            erase $(VPATH)\*.obj
          !endif

.for.obj:
          (cd ..\src;$(FC) $(*).for $(FFLAGS))
