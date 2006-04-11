#
# MKMF template makefile for protected mode executables.
#
FC        = LF90
PROGRAM   = ..\bin\vertcon.exe
LDMAP     = ..\bin\vertcon.map
LIBP      = \lf90\lib

FFLAGS    = -c -i ..\srcAll -trace -g
LDFLAGS   = -g -bind 

OBJS      = vertcon.obj vinitl.obj vheadr.obj viparms.obj vmloop.obj \
            vnblank.obj vgetpt.obj vround.obj vwrtpt.obj vtype2.obj  \
            vprint2.obj vprint3.obj vrcard.obj vdcard.obj vtype1.obj \
            type34.obj print.obj print4.obj angle.obj vaskpt.obj     \
            interp.obj vfhelp.obj vfgrid.obj vcoeff.obj vsurf.obj

VPATH     = ..\srcAll

$(PROGRAM):     $(OBJS) $(LIBS)
          $(FC) $(OBJS) -EXE $@ -MAP $(LDMAP) -LIBP $(LIBP) $(LDFLAGS)
          !if -e $(VPATH)\*.obj
            copy $(VPATH)\*.obj
            erase $(VPATH)\*.obj
          !endif
