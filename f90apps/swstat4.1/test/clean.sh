#!/bin/sh
# clean.sh -- clean up unneeded files after test
#
# Usage: clean.sh
#
# History:  96/04/01  kmflynn
#           98/03/25  kmflynn
#           00/01/10  kmflynn

# standard test files
  Numb='0 1 2 3 4 5 6 7 8 9 10'
  Sufx='out ot2 ot3 ps hpgl cgm sum exp ex2 err'

# miscellaneous files generated automatically
  Misc='SWSTAT.LOG IOWDM.LOG ANNIE.LOG ERROR.FIL XPAD.DAT'
  GenO='duranl.out frqncy.out durhyd.out compar.out'
  Data='klamath.gsd scottsha.gsd bult17b.pks'
  Othr='test.wdm check.log test.out'

# remove miscellaneous files
  for File in $Misc $GenO $Data $Othr ; do
     if [ -f $File ] ; then rm $File ; fi
  done

# remove files specific to the tests
  for No in $Numb ; do
     for Sfx in $Sufx ; do
        if [ -f test$No.$Sfx ]  ; then rm test$No.$Sfx  ; fi
        if [ -f testx$No.$Sfx ] ; then rm testx$No.$Sfx ; fi
     done
  done

# end of shell
