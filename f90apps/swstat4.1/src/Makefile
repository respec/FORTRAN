# Makefile for swstat
#
# Usage:  make [BINDIR=path_to_install_to] [install] [| tee makeout]
#
# Examples:  make
#            make install BINDIR=/usr/opt/bin
#            make install BINDIR=$HOME/bin
#
# History:  95/12/08 kmflynn
#           98/03/09 kmflynn
#           00/04/04 kmflynn

# pathnames
WrdA = /usr/opt/wrdapp
Libr = $(WrdA)/libanne4.0

FFVrsn = $(FFUnix)          # Version FF[Unix, AIX]
FFLAGS = $(OptM) $(FFVrsn)  # compile options [OptM or DbgF] + FFVrsn
FC     = f77                # Compiler name
LGks   = $(GliGks_S)        # Graph lib [Gli, X, Prior]Gks_[S, D]
LdA    = $(LdSunA)          # Ld[Unix, Sun]A
LdB    = $(LdSunB)          # Ld[Unix, Sun]B
LdC    = $(LdSunC)          # Ld[Unix, Sun]C
Strip  = $(Strip_Y)         # strip [Strip_Y or Strip_N]

#*******************************************************************
#*****   Some things may need to be changed below this line.   *****
#*******************************************************************

# gks graphics library, set LGks to appropriate library
# use -L[directory] to define the library search path, if needed
# use _S for static link
# use _D for dynamic link (requires shared library)
PriorGks_D= -lfgks -lhdl -lgks -lX11
XGks_S    = -L/usr/local/lib -lxgks
XGks_D    = -L/usr/local/lib -lxgks -lX11
GliGks_S  = -L/usr/local/lib -lgks
GliGks_D  = -L/usr/local/lib -lgks -lXt -lX11

# Compiler flags
#    C - check subranges and array bounds
#    u - default data type for undeclareds is undefined
#    g - generate source level symbolic debug information
#    O - optimization
# (NOTES: to generate debug code - set FFLAGS to Dbgf and
#                                  set Strip  to Strip_N
#            for production code - set FFLAGS to OptM and
#                                  set Strip to Strip_Y
DbgF   = -u -C -g
OptM   = -u -O

# compile options, use Unix for default
FFUnix =
FFAIX  = -qxlf77=leadzero        # force leading zero on output

# load options
#      use Unix for default, dynamic build
#      use Sun to link in Fortran library statically under Solaris
# Sun load options are very ugly.  After many tries, this is the
# best we could come up with for building a static version.  This
# combination works on our system with the GliGks_S option above.
# We did not test this with the XGks_S option above.  Nor have we
# tested the dynamic link options.
LdUnixA =
LdUnixB =
LdUnixC =
LdSunA  = -nolib -Bstatic
LdSunB  = -lF77 -lM77 -lsunmath -lm -lcx -Bdynamic -lc
LdSunC  = -lnsl -lXt -lX11 -ldl -lmp -lSM -lICE -lsocket -lXext

#load and link options combined
LoadL  = $(LdA) $(LGks) $(LdB) $(LdC)

# strip the symbol table, string table, and line number information
Strip_Y = strip $@
Strip_N = @echo
#        note:  some makes have a problem with blank, use empty echo

# this description file uses the Bourne shell
SHELL = /bin/sh
shCmd = sh

#*******************************************************************
#***** You should not need to modify anything below this line. *****
#*******************************************************************

# library directories
libwdm  = $(Libr)/lib_data
libmsg  = $(Libr)/msg
LibBin  = $(Libr)/bin
LibDir  = $(Libr)/lib

program = swstat
msgFile = swstms.wdm
wdmTest = ../data/test.wdm

Objects = swstat.o prbast.o ndhilo.o prdrhy.o kentau.o ktutil.o
message = ../msg/wdimex.sh            ../msg/swstat.seq           \
          ../msg/prbast.seq           ../msg/ndhilo.seq           \
          ../msg/kentms.seq           ../msg/drhyms.seq           \
          $(libwdm)/message.wdm                                   \
          $(libmsg)/waide/agplot.seq  $(libmsg)/waide/tbltmp.seq  \
          $(libmsg)/awstat/profdr.seq $(libmsg)/awstat/tscmpr.seq \
          $(libmsg)/awstat/a193.seq
Libs    = $(LibDir)/annlib.a   $(LibDir)/awstatlib.a $(LibDir)/waidelib.a  \
          $(LibDir)/graphlib.a $(LibDir)/statslib.a  $(LibDir)/aidelib.a   \
          $(LibDir)/wdmlib.a   $(LibDir)/adwdmlib.a  $(LibDir)/utillib.a
seqwdm  = ../msg/wdimex.sh     ../data/test0.log     ../data/klamath.gsd   \
          ../data/scottsha.gsd ../data/bult17b.pks   ../data/va.ndy

BINDIR  = ../bin
binary  = ../bin
binMake = $(binary)/$(program)
binRX   = $(binary)/wdmrx
binInst = $(BINDIR)/$(program)
binData = ../bin_data
msgMake = $(binData)/$(msgFile)

# Rules
all:  first $(binMake) $(binRX) $(msgMake) $(wdmTest) install
	@echo "***"
	@echo "*** Done making all for $(program), files are now up-to-date."
	@echo "***"

# program dependencies
$(binMake):  $(Objects) $(Libs)
	@echo "***"
	@echo "*** making $(binMake)"
	@echo "***  using $(Libs)"
	@echo "***"
	$(FC) $(Objects) $(Libs) $(LoadL) -o $@
	$(Strip)
$(binRX):  $(LibBin)/wdmrx
	rm -f $(binRX)
	cp -p $(LibBin)/wdmrx $(binRX)
	@echo "*** copied $(binRX)"

# message file and test wdm file
$(msgMake):  $(message)
	rm -f $(msgMake)
	rm -f ../msg/swstms.out
	cd ../msg ; $(shCmd) ./wdimex.sh swstms > swstms.out ; cd ../src
	@echo "*** built $(msgMake)"
$(wdmTest):  $(seqwdm)
	rm -f $(wdmTest)
	rm -f ../msg/test.out
	cd ../msg ; $(shCmd) ./wdimex.sh test ; cd ../src
	@echo "*** built $(wdmTest)"

first:
	@if [ ! -d $(binary) ]   ; then    \
	   mkdir $(binary)  ;              \
	   echo "*** Created $(binary)" ;  \
	fi
	@if [ ! -d $(binData) ]  ; then    \
	   mkdir $(binData) ;              \
	   echo "*** Created $(binData)" ; \
	fi
install:
# Create directory for binary file, if necessary
	@if [ ! -d $(BINDIR) ]   ; then    \
	   mkdir $(BINDIR) ;               \
	   echo "*** Created $(BINDIR)" ;  \
	fi
# Link executable to BINDIR if installing elsewhere
	@if [ ! -s $(binMake) ] ; then                                    \
	   echo "*** program $(binMake) does not exist," ;                \
	   echo "*** use one of the following commands to generate it:" ; \
	   echo "***     make" ;  echo "***     make all" ;               \
	else                                                              \
	   if [ $(BINDIR) != $(binary) ] ; then                           \
	      rm -f $(binInst) ;                                          \
	      cd .. ; ln -s `pwd`/bin/$(program) $(binInst);              \
	      cd ./src ;                                                  \
	      chmod 0755 $(binInst) ;                                     \
	      echo "***  Program $(binInst) has been updated." ;          \
	   fi ;                                                           \
	fi
clean:
	@if [ $(BINDIR) != $(binary) ] ; then rm -f $(binInst) ; fi
	rm -f *.o $(binMake) $(msgMake)
	@echo "***"
	@echo "*** Removed files generated by make for $(program)"
	@echo "***"

# Define object file dependencies:
kentau.o:  ckntau.inc
ndhilo.o:  pnday.inc
swstat.o:  pmesfl.inc fstmes.inc

# end of make
