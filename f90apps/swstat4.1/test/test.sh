#!/bin/sh
# test.sh -- run swstat test data sets
#
# Usage: test.sh [start [stop]]
#
#        where: start = starting test number
#                stop = ending test number (may be same as start)
#
# History: 96/04/01 kmflynn
#          98/03/25 kmflynn
#          00/04/04 kmflynn

# pathnames
WrdA=/usr/opt/wrdapp
Stat=$WrdA/swstat4.1
Iowd=$WrdA/iowdm4.1
Anne=$WrdA/annie4.1

#*******************************************************************
#*****   Some things may need to be changed below this line.   *****
#*******************************************************************

# version of gks being used, set Gks as appropriate
#    Xgks - if using Xgks library from Unidata
#    PriorGks - if using Prior Gks library from Computer Assoc.
#    GliGks - is using Gli/Gks
# for a value of 'x' - tests will produce screen graphs, only
#                ' ' - tests will also produce PostScript and Cgm
    XGks=x
PriorGks=
  GliGks=
#
     Gks=$GliGks           # gks version (GliGks, XGks, or PriorGks)

#*******************************************************************
#***** You should not need to modify anything below this line. *****
#*******************************************************************

  Data=$Stat/data
  Chck=$Stat/test

  exec 2>&1                          # stderr shows up in .out file
  Start=${1-1}                       # by default, start at 1
  Stop=${2-9}                        # by default, stop at 9
                                     # 0 - iowdm, build and fill wdm
                                     # 1 - swstat, n-day high/low, seasonal
                                     # 2 - swstat, n-day high/low, monthly
                                     # 3 - swstat, basic statistics
                                     # 4 - swstat, kendal tau
                                     # 5 - swstat, flow duration
                                     # 6 - swstat, compare
                                     # 7 - swstat, frequency analysis
                                     # 8 - swstat, durhyd, full year
                                     # 9 - swstat, durhyd, summer season
                                     # 10- annie, export wdm file

# begin test runs
  if [ -f test.out ] ; then rm test.out ; fi
  echo                                                        | tee    test.out
  echo ______________________________________________________ | tee -a test.out
  echo                                                        | tee -a test.out
  echo " `date` "                                             | tee -a test.out
  echo                                                        | tee -a test.out
  echo " Processing swstat test runs $Start to $Stop"         | tee -a test.out
  echo                                                        | tee -a test.out
  echo " swstat program from: $Stat"                          | tee -a test.out
  echo "  iowdm program from: $Iowd"                          | tee -a test.out
  echo "  annie program from: $Anne"                          | tee -a test.out
  echo "      test data from: $Data"                          | tee -a test.out
  echo                                             

  Test=$Start

  if [ $Test -eq 0  -o  ! -f test.wdm ] ; then
#    copy wdm file from data or build a new one
     echo                                                     | tee -a test.out
     echo                                                     | tee -a test.out
     echo ___________________________________________________ | tee -a test.out
     echo "Test run number $Test - new wdm file"              | tee -a test.out
     echo                                                     | tee -a test.out
#    delete old if looking for a new one
     if [ $Test -eq 0 ] ; then rm test.wdm ; fi
     if [ -f $Data/test.wdm ] ; then
#       copy wdm from data directory
        echo " Copying the test.wdm file from $Data"             | tee -a test.out
        cp -p $Data/test.wdm test.wdm
     else
#       create WDM file from WATSTORE input
        echo " Building the test.wdm file using iowdm"           | tee -a test.out
#       remove old output files
        for File in ERROR.FIL IOWDM.LOG test0.out test.wdm ; do
           if [ -f $File ] ; then rm $File ; fi
        done
#       update links to input files
        for File in klamath.gsd scottsha.gsd bult17b.pks va.ndy ; do
           if [ -f $File ] ; then rm $File ; fi
           ln -s $Data/$File $File
        done
        $Iowd/bin/iowdm <<-EOT
	@$Data/test0.log
	EOT
        mv ERROR.FIL test0.err
#       remove unneeded files
        for File in klamath.gsd scottsha.gsd bult17b.pks va.ndy IOWDM.LOG; do
           if [ -f $File ] ; then rm $File ; fi
        done
     fi
     if [ $Test -eq 0 ] ; then Test=`expr $Test + 1` ; fi
  fi

  while [ $Test -ge $Start  -a $Test -le $Stop -a $Test -le 9 ] ; do
#    swstat test runs
     echo                                                     | tee -a test.out
     echo                                                     | tee -a test.out
     echo ___________________________________________________ | tee -a test.out
     echo "Test run number $Test"                             | tee -a test.out
     echo                                                     | tee -a test.out
     FileO='ERROR.FIL SWSTAT.LOG DURANL.OUT FRQNCY.OUT DURHYD.OUT COMPAR.OUT'
     Sufix='out ot2 ot3 ps hpgl cgm'
#    remove old output files
     for File in $FileO ; do
#       remove old output files
        if [ -f $File ] ; then rm $File ; fi
     done
     for File in $Sufix ; do
#       remove old test output
        if [ -f test$Test.$File ] ; then rm test$Test.$File ; fi
     done
     if [ -f $Data/test$Gks$Test.log ] ; then
#       test includes graphics or using default log files
        Log=$Data/test$Gks$Test.log
     else
#       no graphics for this test
        Log=$Data/test$Test.log
     fi

     $Stat/bin/swstat <<-EOT
	@$Log
	EOT

#    save error file, remove unneeded files
     mv ERROR.FIL test$Gks$Test.err
     for File in $FileO; do
        if [ -f $File ] ; then rm $File ; fi
     done

     Test=`expr $Test + 1`
  done

  if [ $Test -eq 10  -a  $Stop -eq 10 ] ; then
#    export data sets using annie for verification
     echo                                                     | tee -a test.out
     echo                                                     | tee -a test.out
     echo ___________________________________________________ | tee -a test.out
     echo "Test run number $Test export data sets"            | tee -a test.out
     echo                                                     | tee -a test.out
     if [ -f $Anne/bin/annie ] ; then
        if [ -f test10.exp ] ; then rm test10.exp ;fi
        if [ -f test10.ex2 ] ; then rm test10.ex2 ;fi
        if [ -f test10.err ] ; then rm test10.err ;fi
        $Anne/bin/annie <<-EOT
	@$Data/test10.log
	EOT
        mv ERROR.FIL test$Test.err
        for File in ANNIE.LOG XPAD.DAT ; do
           if [ -f $File ] ; then rm $File ; fi
        done
     else
        echo                                                  | tee -a test.out
        echo "  annie program not available, cannot run test" | tee -a test.out
     fi
  fi

  echo                                                        | tee -a test.out
  echo                                                        | tee -a test.out
  echo ______________________________________________________ | tee -a test.out
  echo "Completed SWSTAT test runs $Start to $Stop"           | tee -a test.out
  echo                                                        | tee -a test.out
# check output against original output in $Data directory
  $Chck/check.sh $Data

# end of shell
