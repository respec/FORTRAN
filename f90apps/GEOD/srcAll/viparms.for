*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/viparms.f,v 1.1 1998/07/07 20:10:37 grogers Exp $
*  viparms.f
*
      SUBROUTINE vIPARMS (ITYPE, SCREEN,VRSION)
*
*  Purpose: Requests information for VERTCON
********************************************
*
*  $Log: viparms.f,v $
*  Revision 1.1  1998/07/07 20:10:37  grogers
*  PR#0, initial add of vertcon_lib
*
*
      CHARACTER*80 B80
      CHARACTER*20 B20
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)
      CHARACTER*80 INFILE, OFILE
      CHARACTER*1 ANS
      INTEGER ITYPE
      INTEGER IFLAG1, IFLAG2, N1, N2, IOS
      LOGICAL SCREEN, EFLAG,TONGVD,CODE15
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, TONGVD, CODE15
      DATA IFLAG1 /1/, IFLAG2 /2/
* GET THE NAME FOR THE INPUT FILE
      WRITE (LUOUT,11) 
   11 FORMAT(/'  VERTCON  processing can be done :',//
     + '  (1) Interactively  - OR -',/
     + '  (2) By entering INPUT FILES in either of 4 formats'/)
      WRITE (LUOUT,111) 
  111 FORMAT('  VERTCON provides numerous ''defaults'' ',
     +     'while prompting for information;',/4x,
     +     ' if you accept an indicated default just press',
     +     ' <RETURN> !! '/)
      WRITE (LUOUT,*) ' '
      WRITE (LUOUT,*) ' Do you have an input data file (Y/N)?'
      WRITE (LUOUT,*) ' (Default is N)'
      READ (LUIN,'(A1)') ANS
      IF (ANS .EQ. 'Y'  .OR.  ANS .EQ. 'y') THEN
        WRITE (LUOUT,12)
   12  FORMAT(/30(' -')//,15x,'NON-INTERACTIVE PROCESSING'/)
* CHOSE THE INPUT FILE FORMAT
*       CALL vNBLANK (INFILE, IFLAG2, N2)
 9001   WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) ' What is your file format?'
        WRITE (LUOUT,*) '  0) Help - File format information'
        WRITE (LUOUT,*) '  1) Free Format Type 1'
        WRITE (LUOUT,*) '  2) Free Format Type 2'
        WRITE (LUOUT,*) '  3) NGS Blue Book format Type 3'
        WRITE (LUOUT,*) '  4) NGS Internal Bench Mark File Type 4'
c
        READ (LUIN,'(A1)') ANS
        IF (ANS .EQ. ' ') THEN
          ITYPE = 5
        ELSE
          READ (ANS,13,ERR=9940,IOSTAT=IOS) ITYPE
   13     FORMAT (I1)
          ENDIF
          IF (ITYPE .GT. 4  .OR.  ITYPE .LT. 0) GO TO 9940
* Get help information
        IF (ITYPE .EQ. 0) THEN
          CALL vFHELP(VRSION)
          GOTO 9001
        ENDIF
    1   WRITE (LUOUT,*) ' What is the name of the input data file?'
        WRITE (LUOUT,*) ' The default name is ''vertcon.inp''.'
        READ (LUIN,'(A80)') INFILE
        IF (INFILE .EQ. B80) INFILE = 'vertcon.inp'
cBriel:
c       +--------------------------------------------------------+
c       | NOTE: To allow for simultaneous access by more than    |
c       | one user, input files can be opened in READ-only mode. |
c       | Output files (if any) can be opened with a unique name |
c       | tag, such as:  userdatetime_outfilename.               |
c       +--------------------------------------------------------+
c
        OPEN (NIN,FILE=INFILE,FORM='FORMATTED',STATUS='OLD',
     +        ACCESS='SEQUENTIAL',ERR=9920,IOSTAT=IOS,
     +        action='READ')
C
      WRITE(LUOUT,101) INFILE
  101 FORMAT(' Opened - ',A80)
          TONGVD=.FALSE.
          CODE15=.FALSE.
* GET TRANSFORMATION INFO. FOR TYPE 4 CHOICE
        IF(ITYPE.EQ.4) THEN
          WRITE(LUOUT,112) 
  112 FORMAT(' Do you want to convert NGVD29 heights to NAVD88 ?',
     +     ' Enter y/n :'/
     +    ' [if answer is no -  NAVD88 heights are converted',
     +    ' to NGVD29'/ '  provided code 88 is in col. 95-96]')
          READ (LUIN,'(A1)') ANS
          IF((ANS.EQ.'y').OR.(ANS.EQ.'Y')) TONGVD=.TRUE.
        ENDIF
*
* CHECK FOR A SCREEN OUTPUT ALSO
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,14) 
   14  FORMAT(' Would you want to look at VERTCON predictions',
     +    ' [listed in Type 1 OUTPUT format!]'/,9x,
     +    ' in addition to generating an output file',
     +    ' (Y/N)?'/,' (Default is N)')
        READ (LUIN,'(A1)') ANS
*
        IF ((ANS.EQ.'y').OR.(ANS.EQ.'Y')) THEN
*  PROVIDE SCREEN DUMP TO TEMPORARY FILE FOR ITYPE > 1
           IF(ITYPE.GT.1) THEN
         WRITE (LUOUT,15)
   15  FORMAT(' Do you want VERTCON predictions',
     +     ' sent to your screen (Y/N)?'/ 
     +     '   [if not, they will go to temp.out scratch file]'/
     +     3x,' (Default is N)')
         READ (LUIN,'(A1)') ANS
          IF ((ANS.NE.'y').AND.(ANS.NE.'Y'))  THEN
           LDUMP=103
C
cBriel: Did not change default ACTION for output file:
C
           OPEN (LDUMP,FILE='temp.out',FORM='FORMATTED',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
         WRITE(LUOUT,*) ' Opened - temp.out'
           GO TO 2
          ELSE
           GO TO 2
          ENDIF
         ELSE
          GO TO 2
         ENDIF
        ENDIF
         SCREEN = .FALSE.
          GO TO 2
* Error message
 9920 CALL vNBLANK (INFILE, IFLAG1, N1)
      CALL vNBLANK (INFILE, IFLAG2, N2)
      WRITE (LUOUT,9915) IOS, INFILE(N1:N2)
      GOTO 1
 9940   WRITE (LUOUT,*) ' Gotta pick ''1'' or ''2'' or ''3'' or "4" ',
     +                  ' sorry try again.'
        GOTO 9001
      ENDIF
* GET THE NAME FOR THE OUTPUT FILE
    2 WRITE (LUOUT,*) ' '
      WRITE (LUOUT,*) ' Enter the name of file which will',
     +                ' contain VERTCON output data :'
      WRITE (LUOUT,*) ' '
      WRITE (LUOUT,*) ' ( if default file name',
     +                ' ''vertcon.out'' is o.k. press <RETURN> )'
      WRITE (LUOUT,*) ' '
      READ (LUIN,'(A80)') OFILE
      IF (OFILE .EQ. B80) OFILE = 'vertcon.out'
      INQUIRE (FILE=OFILE, EXIST=EFLAG)
      IF (EFLAG) THEN
        CALL vNBLANK (OFILE, IFLAG1, N1)
        CALL vNBLANK (OFILE, IFLAG2, N2)
        WRITE (LUOUT,*) ' The file ''', OFILE(N1:N2), ''''
        WRITE (LUOUT,*) ' already exists.  Do you want to overwrite',
     +                                 ' it (Y/N)?'
        WRITE (LUOUT,*) ' (Default is Y)'
        READ (LUIN, '(A1)') ANS
        IF (ANS .EQ. 'n'  .OR.  ANS .EQ. 'N') GOTO 2
      ENDIF
C
cBriel: Did not change default ACTION for output file:
C
      OPEN (NOUT,FILE=OFILE,FORM='FORMATTED',STATUS='UNKNOWN',
     +      ACCESS='SEQUENTIAL',ERR=9910,IOSTAT=IOS)
      WRITE(LUOUT,102) OFILE
  102 FORMAT(' Opened - ',A60/30(' -')/)
      RETURN
* Error message
 9910 CONTINUE
      CALL vNBLANK (OFILE, IFLAG1, N1)
      CALL vNBLANK (OFILE, IFLAG2, N2)
      WRITE (LUOUT,9915) IOS, OFILE(N1:N2)
 9915 FORMAT (' ERROR (', I5, ') - The operating system could not',
     +        ' open the file ', /, '''', A, '''', /, ' Try again'
     +        ' -  may enter <ctrl-c> to terminate !'/)
      GOTO 2
      END
