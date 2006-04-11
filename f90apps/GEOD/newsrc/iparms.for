*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/iparms.f,v 1.1 1998/07/07 19:32:23 grogers Exp $
*  iparms.f
*
      SUBROUTINE IPARMS (KEY, ITYPE, SCREEN,dsel)

*  Purpose: Requests for information needed by NADCON.
******************************************************
*
*  $Log: iparms.f,v $
*  Revision 1.1  1998/07/07 19:32:23  grogers
*  PR#0, initial load of nadcon_lib
*
*
c
c 1/92 - added print for hpgn, nad83 conversion

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*20 B20
      CHARACTER*80 B80
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)

      INTEGER KEY, ITYPE, IFLAG1, IFLAG2, N1, N2, IOS
      CHARACTER*80 INFILE, OFILE
      CHARACTER*1 ANS
      LOGICAL SCREEN, EFLAG,dsel

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA IFLAG1 /1/, IFLAG2 /2/

c  option for hpgn file
      IF(dsel) THEN
c nad27,nad83 selected
*****************************************************************
* CHECK TO SEE IF USER WANTS NAD 27 TO NAD 83 OR NAD 83 TO NAD 27
*****************************************************************

      WRITE (LUOUT,*) ' Do you want to convert from NAD 27 to NAD 83?'
      WRITE (LUOUT,*) ' Enter:'
      WRITE (LUOUT,*) '     ''Y'' to convert from NAD 27 to NAD 83'
      WRITE (LUOUT,*) '     ''N'' to convert from NAD 83 to NAD 27'
      WRITE (LUOUT,*) ' (Default is Y)'

        READ (LUIN, '(A1)') ANS
        IF (ANS .EQ. 'n'  .OR.  ANS .EQ. 'N') THEN
          KEY = -1
        ELSE
          KEY = 1
        ENDIF

      ELSE
C NAD83, HPGN

      WRITE(LUOUT,*) ' Do you want to convert from NAD 83 to HPGN?'
      WRITE(LUOUT,*) ' Enter:'
      WRITE(LUOUT,*) '     ''Y'' to convert from NAD 83 to HPGN'
      WRITE(LUOUT,*) '     ''N'' to convert from HPGN  to NAD 83'
      WRITE(LUOUT,*) ' (Default is Y)'

        READ (LUIN, '(A1)') ANS
        IF (ANS .EQ. 'n'  .OR.  ANS .EQ. 'N') THEN
          KEY = -1
        ELSE
          KEY = 1
        ENDIF
      END IF


**********************************
* GET THE NAME FOR THE OUTPUT FILE
**********************************

   14 WRITE (LUOUT,*) ' What is the name of the file which will',
     +                ' contain the program results?'
      WRITE (LUOUT,*) ' The default name is ''nadcon.out''.'
      READ (LUIN,'(A80)') OFILE
      IF (OFILE .EQ. B80) OFILE = 'nadcon.out'
      INQUIRE (FILE=OFILE, EXIST=EFLAG)
      IF (EFLAG) THEN
        CALL NBLANK (OFILE, IFLAG1, N1)
        CALL NBLANK (OFILE, IFLAG2, N2)
        WRITE (LUOUT,*) ' The file ''', OFILE(N1:N2), ''''
        WRITE (LUOUT,*) ' already exists.  Do you want to overwrite',
     +                                 ' it (Y/N)?'
        WRITE (LUOUT,*) ' (Default is Y)'
        READ (LUIN, '(A1)') ANS
        IF (ANS .EQ. 'n'  .OR.  ANS .EQ. 'N') GOTO 14
      ENDIF
C
cBriel: Did not change default ACTION for output file:
C
      OPEN (NOUT,FILE=OFILE,FORM='FORMATTED',STATUS='UNKNOWN',
     +      ACCESS='SEQUENTIAL',ERR=9910,IOSTAT=IOS)

C

**********************************
* GET THE NAME FOR THE INPUT FILE
**********************************

      WRITE (LUOUT,*) ' '
   13 WRITE (LUOUT,*) ' Do you have an input data file (Y/N)?'
      WRITE (LUOUT,*) ' (Default is N)'

      READ (LUIN,'(A1)') ANS
      IF (ANS .EQ. 'Y'  .OR.  ANS .EQ. 'y') THEN
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) ' What is the name of the input data file?'
        WRITE (LUOUT,*) ' The default name is ''BBOOK''.'
        READ (LUIN,'(A80)') INFILE
        IF (INFILE .EQ. B80) INFILE = 'BBOOK'
cBriel:
        OPEN (NIN,FILE=INFILE,FORM='FORMATTED',STATUS='OLD',
     +        ACCESS='SEQUENTIAL',ERR=9920,IOSTAT=IOS,
     +        action='READ')
C

*****************************
* CHOSE THE INPUT FILE FORMAT
*****************************

 9001   CONTINUE
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) ' What is your file format?'
        WRITE (LUOUT,*) '  0) Help - File format information'
        WRITE (LUOUT,*) '  1) Free Format Type 1'
        WRITE (LUOUT,*) '  2) Free Format Type 2'
        WRITE (LUOUT,*) '  3) NGS Blue Book format (default)'

        READ (LUIN,'(A1)') ANS
        IF (ANS .EQ. ' ') THEN

* NGS Horizontal Blue Book format

          ITYPE = 3

        ELSE
          READ (ANS,347,ERR=9940,IOSTAT=IOS) ITYPE
  347     FORMAT (I1)

          IF (ITYPE .GT. 3  .OR.  ITYPE .LT. 0) THEN

* Not a good answer - Try again.

            WRITE (LUOUT,*) ' Gotta pick one of these -',
     +                      ' sorry try again.'
            GOTO 9001
          ENDIF
        ENDIF

* Get help information

        IF (ITYPE .EQ. 0) THEN
          CALL FHELP
          GOTO 9001
        ENDIF

********************************
* CHECK FOR A SCREEN OUTPUT ALSO
********************************

        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) ' Do you want the results written to the',
     +                  ' terminal screen as well as to'
        WRITE (LUOUT,*) ' the output file (Y/N)?'
        WRITE (LUOUT,*) ' (Default is N)'
        READ (LUIN,'(A1)') ANS
        IF (ANS .NE. 'y'  .AND.  ANS .NE. 'Y') SCREEN = .FALSE.

        GOTO 9002

* Error message

 9940   WRITE (LUOUT,*) ' Gotta pick ''1'' or ''2'' or ''3'' -',
     +                  ' sorry try again.'
        GOTO 9001

 9002 ENDIF

      RETURN

* Error message

 9910 CONTINUE
      CALL NBLANK (OFILE, IFLAG1, N1)
      CALL NBLANK (OFILE, IFLAG2, N2)
      WRITE (LUOUT,9915) IOS, OFILE(N1:N2)
 9915 FORMAT (' ERROR (', I5, ') - The operating system could not',
     +        ' open the file ', /,
     +        ' ''', A, '''', /,
     +        ' Try again.', /)
      GOTO 14

 9920 CONTINUE
      CALL NBLANK (INFILE, IFLAG1, N1)
      CALL NBLANK (INFILE, IFLAG2, N2)
      WRITE (LUOUT,9915) IOS, INFILE(N1:N2)
      GOTO 13

      END
