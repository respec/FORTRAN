#!/bin/sh
# wdimex.sh -- make the message file for swstat
#              build test wdm file
#
# History:  96/03/29  kmflynn
#           98/03/09  kmflynn
#           99/01/07  kmflynn
#
# Usage: wdimex.sh [ file_name ] [| tee output_file ]
#
# Where: file_name = the name of the message file or test wdm file to build
#                    swstms - to generate the swstat message file (default)
#                      test - generate test wdm file
#                      both - generate both message and test wdm files
#
# Examples:  wdimex.sh
#            wdimex.sh test

# pathnames and directories
  WrdA=/usr/opt/wrdapp
  Libr=$WrdA/libanne4.0
  Iowd=$WrdA/iowdm4.1

#*******************************************************************
#***** You should not need to modify anything below this line. *****
#*******************************************************************

Optn=${1-'swstms'}
     if [ ! $Optn ] ; then Optn=swstms ; fi

# files for message file
  SeqMsg='swstat prbast ndhilo kentms drhyms'
  SeqLib='waide/agplot  waide/tbltmp
          awstat/profdr awstat/tscmpr awstat/a193'
# files for test wdm file
  SeqDat='klamath.gsd scottsha.gsd bult17b.pks va.ndy'

  echo
  echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ' ; echo
  echo '     building wdm file(s) for:' $Optn                       ; echo
  echo '              Library =' $Libr
  echo

  if [ $Optn = 'swstms'  -o  $Optn = 'both' ] ; then
#    build the message file
     Name=swstms
     echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ' ; echo
     echo '             wdm name =' $Name.wdm
     echo '          sequentials =' $SeqMsg
     for Seq in $SeqLib ; do
     echo '                       ' $Seq
     done
     echo
     echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ' ; echo

#    remove any old files
     if [ -f error.fil ] ; then rm error.fil ; fi
     if [ -f $Name.wdm ] ; then rm $Name.wdm ; fi
     if [ -f $Name.in  ] ; then rm $Name.in  ; fi
     if [ -f $Name.err ] ; then rm $Name.err ; fi

#    build input file
     echo $Name.wdm > $Name.in
     for Seq in $SeqMsg ; do
        echo I >> $Name.in
        echo $Seq.seq >> $Name.in
     done
     for Seq in $SeqLib ; do
        echo I >> $Name.in
        echo $Libr/msg/$Seq.seq >> $Name.in
     done
     echo R >> $Name.in

#    get message file shell
     cp $Libr/lib_data/message.wdm $Name.wdm

#    build message file
     $Libr/bin/wdimex < $Name.in

#    remove old message file and move new one
     if [ -f ../bin_data/$Name.wdm ] ; then rm ../bin_data/$Name.wdm ; fi
     mv $Name.wdm ../bin_data
     mv error.fil $Name.err
  fi

  if [ $Optn = 'test'  -o  $Optn = 'both' ] ; then
#    build the test wdm file
     Name=test
     echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ' ; echo
     echo '                iowdm =' $Iowd
     echo '             wdm name =' $Name.wdm
     echo '           data files =' $SeqDat
     echo
     echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ' ; echo

#    remove any old files
     if [ -f error.fil ] ; then rm error.fil ; fi
     if [ -f $Name.wdm ] ; then rm $Name.wdm ; fi
     if [ -f $Name.in ]  ; then rm $Name.in  ; fi
     if [ -f $Name.err ] ; then rm $Name.err ; fi

#    update links to input files
     for File in $SeqDat ; do
        if [ -f $File ] ; then rm $File ; fi
        ln -s ../data/$File $File
     done
     $Iowd/bin/iowdm <<-EOT
	@../data/test0.log
	EOT
     rm IOWDM.LOG

#    remove old test wdm file and move new one into data
     if [ -f ../data/$Name.wdm ] ; then rm ../data/$Name.wdm ; fi
     mv $Name.wdm ../data/$Name.wdm
#    save error information file and remove data files
     for File in $SeqDat ; do
        if [ -f $File ] ; then rm $File ; fi
     done
     mv ERROR.FIL $Name.err
     mv test0.out $Name.out
  fi

# end of shell
