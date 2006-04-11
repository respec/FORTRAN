#!/bin/sh
#
#    check.sh:  Compare (using diff) 'original' versions of files to
#               files in current directory.  Used most often to compare
#               expected files to actual test output.
#   arguments:  DirO - directory containing original, expected versions
#                      defaults to ../data
#               Name - prefix of file names, defaults to test
#  file names:  naming convention is "nameN.sfx", where:
#               name - prefix of file name, see Names below
#                  N - test numbers, see Tests below
#                sfx - suffix for file names, see Sufx below
#
#     history:  96/04/17 kmflynn  modified version of check.sh

  DirO=${1-../data}             # by default, data in parallel directory
  Name=${2-test}                # by default, file prefix is test

# output file for differences
  checkout='check.log'
# define range of tests and sufixes to be considered
  Tests='0 1 2 3 4 5 6 7 8 9 10'
  Sufix='out ot1 ot2 ot3 exp ex2 err ps hpgl cgm'

# delete old file and write heading to file
  if [ -f $checkout ] ; then rm $checkout ; fi
  echo                                                        | tee $checkout
  echo                                                        | tee -a $checkout
  echo                                                        | tee -a $checkout
  echo "Comparing original output files and new output files" | tee -a $checkout
  echo "Original files in: " $DirO                            | tee -a $checkout
  echo "             date: " `date`                           | tee -a $checkout
  echo "  "                                                   | tee -a $checkout
  echo                                                        | tee -a $checkout
  echo " Note:  In checking the compare differences, you"     | tee -a $checkout
  echo "        should expect to find different dates in"     | tee -a $checkout
  echo "        the graphics files (.ps .cgm .hgpl).  The"    | tee -a $checkout
  echo "        values for attributes DATCRE and DATMOD"      | tee -a $checkout
  echo "        will differ in the export file (.exp)."       | tee -a $checkout
  echo                                                        | tee -a $checkout

  for Test in $Tests ; do
     for Sufx in $Sufix ; do
        if [ -f $DirO/$Name$Test.$Sufx -a -f $Name$Test.$Sufx ] ; then
           # both original and new output files exist, compare
           echo "_________________________________________________________ " \
                $Name$Test.$Sufx | tee -a $checkout
           if diff $DirO/$Name$Test.$Sufx $Name$Test.$Sufx >> $checkout
              then
              echo FILES ARE IDENTICAL | tee -a $checkout
           else
              echo FILES DIFFER:  see file $checkout for differences
           fi
        fi
     done
  done

# end of shell
