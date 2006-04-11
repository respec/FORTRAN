*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/ifiles.f,v 1.1 1998/07/07 20:20:40 grogers Exp $
*  ifiles.f
*
      SUBROUTINE IFILES (KIN, FLAG, BASEIN, RIDENT, PGM)

* Purpose: Interactively get input file basenames.
*          Open the input files
**************************************************
*
*  $Log: ifiles.f,v $
*  Revision 1.1  1998/07/07 20:20:40  grogers
*  PR#0, initial add of nadgrd_lib
*
*

*     IMPLICIT REAL (A-H, O-Z)
*     IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      CHARACTER*32 B32
      PARAMETER (B32 = '                                ')

      REAL XMIN0A, YMIN0A, DXA, DYA
      REAL ANGLE, ANGLEA
      INTEGER NZ, LRECL, KIN
      INTEGER IFLAG2, N2
      INTEGER NCA, NRA, NZA
      CHARACTER*56 RIDENT
      CHARACTER*32 FINA, FINO
      CHARACTER*28 BASEIN
      CHARACTER*8 PGM
      CHARACTER*1 ANS
      LOGICAL FLAG

      INTEGER LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO
      COMMON /INOUT/ LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO

      REAL XMIN0, XMAX0, YMIN0, YMAX0, DX, DY
      INTEGER NC, NR
      COMMON /GRID0/ XMIN0, XMAX0, YMIN0, YMAX0, DX, DY, NR, NC

      DATA  IFLAG2 /2/

* Find out about the input files

      WRITE (LUOUT,2)
*   2 FORMAT ('1')
    2 FORMAT ('')
  220 WRITE (LUOUT,225)
  225 FORMAT (   ' For the input files enter:',
     +        /, '     ''A'' for ASCII transfer format.',
     +        /, '     ''B'' for binary - NADCON data file format).',
     +        /, ' (Default is B)')
      READ (LUIN,'(A1)') ANS
      IF (ANS .EQ. ' '  .OR.  ANS .EQ. 'B'  .OR.  ANS .EQ. 'b') THEN
        KIN = 1
      ELSEIF (ANS .EQ. 'A'  .OR.  ANS .EQ. 'a') THEN
        KIN = -1
      ELSE
        WRITE (LUOUT, 210) ANS
  210   FORMAT (/, ' ERROR - ''', A1, ''' is not a legal answer.')
        GOTO 220
      ENDIF

* Get basename

      IF (KIN .EQ. 1) THEN
        WRITE (LUOUT,110)
  110   FORMAT (   ' Enter the basename for the pair of input NADCON',
     +             ' grid files from which the',
     +          /, ' new grids will be extracted.  The ''.las''',
     +             ' and ''.los'' extensions will be',
     +          /, ' added to the basename by NADGRD.  The default',
     +             ' basename is ''conus''.')
      ELSEIF (KIN .EQ. -1) THEN
        WRITE (LUOUT,115)
  115   FORMAT (   ' Enter the basename for the pair of input NADCON',
     +             ' grid files from which the',
     +          /, ' new grids will be extracted.  The ''.laa''',
     +             ' and ''.loa'' extensions will be',
     +          /, ' added to the basename by NADGRD.  The default',
     +             ' basename is ''conus''.')
      ENDIF

      READ (LUIN,'(A28)') BASEIN
      CALL NBLANK (BASEIN, IFLAG2, N2)
      IF (N2 .EQ. 0) THEN
        BASEIN = 'conus'
        N2 = 5
      ENDIF

      FINA = B32
      FINA(1:N2) = BASEIN(1:N2)
      FINO = B32
      FINO(1:N2) = BASEIN(1:N2)

      IF (KIN .EQ. 1) THEN

* Open binary input files

        N2 = N2 + 4
        FINA(N2-3:N2) = '.las'
        FINO(N2-3:N2) = '.los'
        LRECL = 256
cBriel:
c       +--------------------------------------------------------+
c       | NOTE: To allow for simultaneous access by more than    |
c       | one user, input files can be opened in READ-only mode. |
c       | Output files (if any) can be opened with a unique name |
c       | tag, such as:  userdatetime_outfilename.               |
c       +--------------------------------------------------------+
c
        OPEN (NINA,FILE=FINA,FORM='UNFORMATTED',ACCESS='DIRECT',
     +        RECL=LRECL,STATUS='OLD',ERR= 900,
     +        action='READ')
C
        OPEN (NINO,FILE=FINO,FORM='UNFORMATTED',ACCESS='DIRECT',
     +        RECL=LRECL,STATUS='OLD',ERR= 900,
     +        action='READ')
C
        READ (NINA,REC=1) RIDENT, PGM, NCA, NRA, NZA, XMIN0A, DXA,
     +                    YMIN0A, DYA, ANGLEA
        READ (NINO,REC=1) RIDENT, PGM, NC, NR, NZ, XMIN0, DX,
     +                    YMIN0, DY, ANGLE

      ELSEIF (KIN .EQ. -1) THEN

* Open ASCII input files

        N2 = N2 + 4
        FINA(N2-3:N2) = '.laa'
        FINO(N2-3:N2) = '.loa'
cBriel:
        OPEN (NINA,FILE=FINA,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +        STATUS='OLD',ERR=900,
     +        action='READ')
C
        OPEN (NINO,FILE=FINO,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +        STATUS='OLD',ERR=900,
     +        action='READ')
C
        READ (NINA,140) RIDENT, PGM
        READ (NINO,140) RIDENT, PGM
  140   FORMAT (A56, A8)
        READ (NINA, 150,ERR=900)  NCA, NRA, NZA, XMIN0A, DXA,
     +                            YMIN0A, DYA, ANGLEA
        READ (NINO, 150,ERR=900)  NC, NR, NZ, XMIN0, DX,
     +                            YMIN0, DY, ANGLE
  150   FORMAT (3I4, 5F12.5)
      ENDIF

* Check for corrupted files

      IF (NCA .NE. NC  .OR.  NRA .NE. NR  .OR.  NZA .NE.  NZ  .OR.
     +    XMIN0A .NE. XMIN0  .OR.  YMIN0A .NE. YMIN0  .OR.
     +    DXA .NE. DX  .OR.  DYA .NE. DY  .OR.  ANGLEA .NE. ANGLE)
     +  GOTO 960
      IF (NZ .NE. 1  .OR.  ANGLE .NE. 0.0) GOTO 960

* Reopen binary files with correct record length

      IF (KIN .EQ. 1) THEN
        CLOSE (NINA)
        CLOSE (NINO)
        LRECL = 4*(NC + 1)
cBriel:
        OPEN (NINA,FILE=FINA,FORM='UNFORMATTED',ACCESS='DIRECT',
     +        RECL=LRECL,STATUS='OLD',
     +        action='READ')
C
        OPEN (NINO,FILE=FINO,FORM='UNFORMATTED',ACCESS='DIRECT',
     +        RECL=LRECL,STATUS='OLD',
     +        action='READ')
C
      ENDIF

* Write the (user-significant) input file variables to the screen

      XMAX0 = DX * REAL( NC-1 )  +  XMIN0
      YMAX0 = DY * REAL( NR-1 )  +  YMIN0

      IF (KIN .EQ. 1) THEN
        WRITE (LUOUT,130) FINA(1:N2), FINO(1:N2)
  130   FORMAT (/, ' Binary files ''', A, ''' and ''', A, ''' have',
     +             ' been opened.')
      ELSEIF (KIN .EQ. -1) THEN
        WRITE (LUOUT,135) FINA(1:N2), FINO(1:N2)
  135   FORMAT (/, ' ASCII files ''', A, ''' and ''', A, ''' have',
     +             ' been opened.')
      ENDIF
      WRITE (LUOUT,180) YMIN0, YMAX0, -XMAX0, -XMIN0
  180 FORMAT (/, ' Their minimum and maximum latitudes  (+N) are: ',
     +           2F8.0,
     +        /, ' Their minimum and maximum longitudes (+W) are: ',
     +           2F8.0, /)
      WRITE (LUOUT,200)
  200 FORMAT (14X, '(Hit RETURN to continue.)')
      READ (LUIN,'(A1)') ANS
      WRITE (LUOUT,2)

  999 RETURN

* Error messages

  900 WRITE (LUOUT,910) FINA(1:N2), FINO(1:N2)
  910 FORMAT (/, ' *** ERROR *** ''', A, ''' and ''', A, ''' cannot',
     +           ' be opened!',
     +        /, ' Do you wish to try again (Y/N)?',
     +        /, ' (Default is Y)')
      READ (LUIN,'(A1)') ANS
      IF (ANS .NE. 'N'  .AND.  ANS .NE. 'n') GOTO 220
      FLAG = .TRUE.
      GOTO 999

* Corrupted input files

  960 WRITE (LUOUT, 965) FINA(1:N2), FINO(1:N2)
  965 FORMAT (/, 5X, ' *********** ERROR ***********', /,
     +           ' The header information in grid files', /,
     +           ' ''', A, ''' and ''', A, '''', /,
     +           ' is incorrect!  One or both of these files is',
     +                                ' corrupted or the wrong format.')
      FLAG = .TRUE.
      GOTO 999
      END
