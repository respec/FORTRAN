README.TXT

                                SWSTAT

                       Surface-Water Statistics

                             Version 4.1

                             2002/02/25

IMPORTANT NOTE:  If you have n-day data sets that were created using
                 the N-day option in version 3.2 of SWSTAT, you will
                 need to run WDMRX to correct a problem with the 
                 SEADBG and SEADND attributes.  WDMRX is distributed
                 with SWSTAT.


Instructions for installation, execution, and testing are provided
below.  After installation, see swstat.txt in the doc subdirectory
for summary information on SWSTAT.

For assistance, enhancement requests, or to report bugs, contact the
Hydrologic Analysis Software Support Program by sending e-mail to
h2osoft@usgs.gov.


                          TABLE OF CONTENTS

                          A. DISTRIBUTION FILES
                          B. DOCUMENTATION
                          C. EXTRACTING FILES
                          D. COMPILING (optional)
                          E. INSTALLING
                          F. RUNNING THE PROGRAM
                          G. TESTING
                          H. CONTACTS


A. DISTRIBUTION FILES

The following distribution packages (containing the software, test data sets,
and information files) are currently available for UNIX systems:

  swstat4.1.source.tar.gz  - Source code only
  swstat4.1.Solaris.tar.gz - Compiled for Sun UltraSPARC-II under Solaris 2.6


B. DOCUMENTATION

Lumb, A.M., and Kittle, J.L., Jr., and Flynn, K.M., 1990, Users manual for
   ANNIE, a computer program for interactive hydrologic analyses and data
   management:  U.S. Geological Survey Water-Resources Investigations
   Report 89-4080, 236 p.

   The swstat program was originally a part of the annie program documented
   in the above report.  Updated documentation for the current version of
   the swstat program is not available.  The annie manual (listed below) may
   be useful for understanding how the user interface and the wdm file work.

Flynn, K.M., Hummel, P.R., Lumb, A.M., and Kittle, J.L., Jr., 1995, User's
   manual for ANNIE, version 2, a computer program for interactive hydrologic
   data management:  U.S. Geological Survey Water-Resources Investigations
   Report 95-4085, 211 p.

   This document is available in electronic format.  A Portable Document
   Format (PDF) version is included in the doc subdirectory of the ANNIE
   program distribution.  This and other formats can also be found at
   http://water.usgs.gov/software/annie.html.

See http://water.usgs.gov/software/ordering_documentation.html for
information on ordering printed copies of USGS publications.


C. EXTRACTING FILES

Compressed tar files are used to distribute the source code and versions
of the software compiled for selected UNIX operating systems.  All of
the files needed to install and test the program are contained in the
file swstat4.1.OS.tar.gz (where OS is a string indicating the intended
operating system.)  If there is not a tar file for your operating system
or you want to compile the software, the source version of the tar file
contains all of the files needed to compile and install the program on
a UNIX-based computer.  For all of these distributions, the directory
swstat4.1 is created (or overwritten) when the files are extracted from
the tar file; if this directory already exists, you may want to delete
or rename it before extracting the files.

Follow the steps below to extract the files from a distribution tar file.
The software is configured for installation under /usr/opt/wrdapp.  The
wrdapp directory may be a separate file system mounted at /usr/opt/wrdapp.
If you choose to install the software elsewhere, you will need to retrieve
the source version of the tar file and compile the software.

  Steps in extracting files                 explanation
  ----------------------------------------  -----------------------------------
  mv swstat4.1.____.tar.gz /usr/opt/wrdapp  If the tar file is not already in
                                            the directory where you want the
                                            distribution installed, move it
                                            there.

  cd /usr/opt/wrdapp                        If you are not in the directory
                                            where the tar file is located, go
                                            there.

  gunzip swstat4.1.____.tar.gz              Uncompress the distribution file.

  tar -xvpof swstat4.1.____.tar             Extract the distribution files
                                            from the tar file.

This creates the following directory structure (the contents of each
directory are shown to the right):

   swstat4.1           files NOTICE.TXT, RELEASE.TXT, and this README.TXT
     `-----bin         compiled executable
     `-----bin_data    message file required during program execution
     `-----doc         documentation files (see file Contents.txt)
     `-----src         Makefile (and, with source tar, the source code)
     `-----msg         (with source tar, script & file to build message file)
     `-----test        scripts to run verification tests
     `-----data        standard data sets used in verification tests

Notes:  a) The bin and bin_data subdirectories are not included in the
           source distribution; they are created during compilation.
        b) Source code is included only with the source distribution.
        c) It is recommended that no user files be kept in the program
           directory structure.  If you plan to put files in the program
           directory structure, do so only by creating subdirectories.
        d) To compile a new version of the software, you will also need:
           (1) libraries and other data files from the libanne library
           distribution (version 4.0, or later, available from
           http://water.usgs.gov/software/libanne.html), (2) a Fortran
           compiler (77 or later), and (3) a Graphical Kernel System (GKS)
           library.


D. COMPILING (optional)

If you have retrieved a pre-compiled distribution of the software, skip to
the Installing section below.

If a compiled version of the software is not available for your computer
or you want to build the executable yourself, follow the instructions in
this section.  The source distribution is provided for those users who
want the source code.  Little or no support can be provided for users
generating their own versions of the software.  In general, to compile a
new version of the software, you will need:

        a) a Fortran compiler (77 or later),
        b) a minimal level of knowledge of Make, the compiler, and
           the UNIX operating system,
        c) libraries and other data files from the libanne library
           distribution (version 4.0 or later, available from
           http://water.usgs.gov/software/libanne.html), and
        d) a Graphical Kernel System (GKS) library;  GKS libraries
           available without fee include Gli/gks (available from
           http://iff001.iff.kfa-juelich.de/gli/) and xgks (available
           as the file ftp://unidata.ucar.edu/pub/xgks/xgks-2.5.5.tar.Z).

As provided in the source distribution, the software is set up to be
compiled under Solaris in the /usr/opt/wrdapp directory.  A small number
of changes may be needed to compile on other UNIX platforms or in another
directory.  Additional changes will be required to compile on a PC or
other non-UNIX platform.  The versions.txt file found in the doc
subdirectory identifies items that may need to be changed.

To generate a new executable and message file, do the following:

1.  The values for the indicated variables in the following files may
    need to be modified (see versions.txt in the doc subdirectory for
    more details):

                               may need to be modified
                           -------------------------------
                            version     compiler
    file name              variables  flags  name  library
    ---------------------  ---------  -----------  -------
    src/Makefile           WrdA Libr  FFLAGS  FC     LGks
                                      FFVrsn
                                      Strip
                                      LdA LdB
                                      LdC
        fstmes.inc         FNAME
    msg/wdimex.sh          WrdA Libr
    test/test.sh           WrdA Stat
                           Iowd Anne
                           Gks

2.  Run make in the src directory to compile the source and build the
    message file.

        cd swstat4.1/src
        make

    make will:

        a.  create the subdirectories bin and bin_data, if they do
            not already exist,
        b.  compile the source code,
        c.  place the program executable in the bin subdirectory,
        d.  run the wdimex.sh script found in the msg subdirectory to build
            the message file (swstms.wdm); the file is placed in bin_data,
            and
        e.  run the wdimex.sh script in the msg subdirectory to build the
            test.wdm file; the file is placed in the data directory.


E. INSTALLING

To make the program easy to use, a link to the executable should be placed
in a directory that is included in each user's search path.  Run make in
the src subdirectory to create the link:

    make install BINDIR=directory_for_links

A link to the executable will be placed in the directory assigned to
BINDIR.  For example, if each user's search path consists of

    /usr/bin:/usr/opt/bin:/usr/local/bin

using the command

    make install BINDIR=/usr/local/bin

will make the program accessible from any directory without requiring the
full pathname of the executable.  Note that to create and delete links to
the executable, the installer must have sufficient rights to the BINDIR
directory.

This program uses the GLIGKS graphics library, V4.5.24, to generate
graphics.  You do not need to install the GLIGKS graphics library on
your computer to use this pre-compiled distribution.  You do need the
GLIGKS file gksfont.dat; the bin_data subdirectory of this distribution
contains a copy of the file.  The program expects to find the file in
the /usr/local/lib directory, but, you can install it elsewhere.  If
you do not already have gksfont.dat installed:

  o  For the standard installation, copy the file to /usr/local/lib,
     from the bin_data directory of this distribtution:

     cp -p gksfont.dat /usr/local/lib

     (note that you may need system administrator rights)


  o  If you install the file elsewhere, you will need to tell GLIGKS
     about the new font path:

         setenv GLI_HOME path
     or  export GLI_HOME=path

     (Note that each user of the software will need to set the path
     for GLI_HOME.)


F. RUNNING THE PROGRAM

If the program has been installed in a directory included in the users'
PATH (as described above), the program can be executed with the command
"swstat".  Otherwise, the full pathname of the executable will need to
be typed.


G. TESTING

Test data sets are provided to verify that the program is correctly installed
and running on the system.  The tests may also be looked at as examples of how
to use the program.  The test subdirectory contains the scripts to run the
tests.  The data subdirectory contains the input data and the expected results
for each test.  Tests are usually run in the test subdirectory, but they can
be run in any user directory.  Do NOT run the tests in the data subdirectory.
Type the following commands to test the program installation.

     [path]/test.sh [start [stop]]

where:  path  = path to the script
                use "." if running the tests in the test directory,
                use full pathname if running in a directory other than
                test; do _NOT_ run the test in the data directory.
        start = the number of the first test to perform, default = 1
        stop  = the number of the last test to perform, default = 9

For example:

     command                                 what happens
     --------------------------------------  --------------------------------
     ./test.sh                               runs tests 1 - 9
     ./test.sh 1 1                           runs the first test
     ./test.sh 2 4                           runs tests 2, 3, and 4
     /usr/opt/wrdapp/swstat4.1/test/test.sh  runs tests 1 - 9
     ./test.sh 0 10                          starts with new wdm file
                                             runs swstat tests 1 - 9, and
                                             runs annie export

After the tests are completed, the results are compared to the expected
results (found in the data subdirectory).  See the file check.log to
verify the tests produced the expected results.  Expect to find the
following differences:

        a) The standard data sets were created on a Sun UltraSPARC-II
           system.  You may notice slight numeric differences in the
           results on other computers.  These are generally due to
           different round-off algorithms and the different architecture
           of the central processing unit chip.  Slight differences
           in output formats may occur on other computers, particularly
           for the value 0.0.
        b) Each data set in an archive file (.exp) will contain the
           attribute DATCRE and DATMOD, the dates the data set was
           created and last modified, respectively.
        c) Graphics output files (.ps and .cgm) contain the date the
           file was generated.
        d) If a graphics library other than GliGks is used, there will
           probably be significant differences in the graphics output
           files (.ps and .cgm); they should still produce the same
           images.

To clean up after the tests, type the command:

     [path]/clean.sh

     where path is as described above.

The tests are described in the table below, where 'test' is the test number,
'program' is the program used to run the test, and the 'usage' column
indicates how a file is used, with i for input, o for output, and i/o for
both input and output.

Notes:  a) Some of the tests may require input generated by a previous test,
           so they should be run in sequential order.
        b) If the implementation of GKS being used does not support
           output files, the .ps and .cgm files will not be generated.
        c) Two of the tests require other programs,
           1) test 0 requires the iowdm program (version 4.0 or later)
           2) test 10 requires the annie program (version 4.0 or later)

test  program  description of test and files          file name & usage
----  -------  -------------------------------------  -----------------
  0   iowdm    create and fill the wdm file
               (NOTE:  if the test.wdm file is
               included in the data directory,
               it will be copied into the current
               directory instead of running IOWDM
               to build it.)

               command file                           test0.log      i
               daily data, WATSTORE                   klamath.gsd    i
               daily data, WATSTORE                   scottsha.gsd   i
               peak flow data, WATSTORE               bult17b.pks    i
               7-day low flows                        va.ndy         i
               data management file                   test.wdm       o
               summary of processed data              test0.out      o
               error file                             test0.err      o

  1   swstat   generate various high and low flows
               and various seasons; output to text
               file and/or wdm file

               command file                           test1.log      i
               data management file                   test.wdm      i/o
               20 standard n-day high and low flows   test1.out      o
               7-day flow, low for July high for May  test1.ot2      o
               7-day low & high for Jun 15 - Sep 15,  test1.ot3      o
                  7,30,60&4-day low, Jun 15 - Sep 15,
                  7-day high Jun 15 - Sep 15
               error file                             test1.err      o
               

  2   swstat   generate 7 & 30-day low flows for
               months May, June, July, and August
               for 3 sites; output to text file
               and wdm file.

               command file                           test2.log      i
               data management file                   test.wdm      i/o
               n-day tables                           test2.out      o
               error file                             test2.err      o


  3   swstat   calculate basic statistics 

               command file                           test3.log      i
               data management file                   test.wdm      i/o
               for daily and annual peaks             test3.out      o
               for n-day flows                        test3.ot2      o
               error file                             test3.err      o
               

  4   swstat   kendall tau trend analysis

               command file                           test4.log      i
               data management file                   test.wdm      i/o
               3-day low flow                         test4.out      o
               peak (table & time), 7-day low         test4.ot2      o
               daily (avg & min transformation),      test4.ot3      o
                  365-day high and low       
               error file                             test4.err      o

  5   swstat   perform flow-duration analysis

               command file                           test5.log      i  
               data management file                   test.wdm      i/o 
               daily, upper bound to 100000           test5.out      o  
               daily avg to annual, limits 50-400     test5.ot2      o
               flow duration plot, postscript         test5.ps       o
               flow duration plot, hp                 test5.hpgl     o
               flow duration plot, cgm                test5.cgm      o
               error file                             test5.err      o

  6   swstat   compare two daily time series

               command file                           test6.log      i
               data management file                   test.wdm       i
               comparison tables                      test6.out      o
               duration plot, postscript              test6.ps       o
               duration plot, cgm                     test6.cgm      o
               error file                             test6.err      o

  7   swstat   perform frequency analysis on 7-day
               low flow annual series produced in
               test 1; output to text and wdm files

               command file                           test7.log      i
               data management file                   test.wdm      i/o
               7-day low flow                         test7.out      o
               7-day low&high, 7-day low w/zero data  test7.ot2      o
               4-day low flow, no log/log transform   tet7.ot3       o
               frequency plot, postscript             test7.ps       o
               frequency plot, cgm                    test7.cgm      o
               error file                             test7.err      o

  8   swstat   compute duration hydrograph table
               and curves for a full year; output
               to text file and graphic file

               command file                           test8.log      i
               data management file                   test.wdm       i
               duration hydrograph table              test8.out      o
               duration hydrograph plot, ps           test8.ps       o
               duration hydrograph plot, cgm          test8.cgm      o
               error file                             test8.err      o

  9   swstat   compute duration hydrograph table and
               curves for June, July and August,
               output to text file and graphic file

               command file                           test9.log      i
               data management file                   test.wdm       i
               duration hydrograph table              test9.out      o
               duration hydrograph plot, ps           test9.ps       o
               duration hydrograph plot, cgm          test9.cgm      o
               error file                             test9.err      o

 10   annie    export data sets for verification

               command file                           test10.log     i
               data management file                   test.wdm       i
               tabular listing, selected attributes   test10.out     o
               all data from annual data sets         test10.exp     o
               attributes from daily data sets        test10.ex2     o
               error file                             test10.err     o


H. CONTACTS

Inquiries about this software distribution should be directed to:

  U.S. Geological Survey
  Hydrologic Analysis Software Support Program
  437 National Center
  Reston, VA  20192

  e-mail:  h2osoft@usgs.gov
