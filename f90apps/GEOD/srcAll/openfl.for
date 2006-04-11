*     $Header: /nwiscvs/watstore/geod/src/nadcon_lib/openfl.f,v 1.2 1998/07/14 20:37:36 grogers Exp $
*     openfl.f
*
      SUBROUTINE OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX, DY,
     +                   XMAX1, XMIN1, YMAX1, YMIN1, NC1, CARD)

*  Purpose: Given base name of gridded data files, open the two data files
**************************************************************************
*
*  $Log: openfl.f,v $
*  Revision 1.2  1998/07/14 20:37:36  grogers
*  PR#0, added dynamic unit allocation
*
c Revision 1.1  1998/07/07  19:32:29  grogers
c PR#0, initial load of nadcon_lib
c
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*23 B23
      PARAMETER (B23 = '                       ')
      CHARACTER*69 B69
      PARAMETER (B69 = B23//B23//B23)

      DOUBLE PRECISION XMAX1, XMIN1, YMAX1, YMIN1, DX, DY
      REAL DX1, DY1, DX2, DY2
      REAL X01, Y01, ANGLE1, X02, Y02, ANGLE2
      INTEGER IFLAG1, IFLAG2, N1, N2, N3, N4
      INTEGER ITEMP, LRECL, ILA, ILO, IFILE, IOS
      INTEGER NC1, NR1, NZ1, NC2, NR2, NZ2
      CHARACTER*80 CARD
      CHARACTER*69 ALAS, ALOS
      CHARACTER*65 AFILE
      CHARACTER*56 RIDENT
      CHARACTER*8 PGM
      LOGICAL GFLAG, NOGO, OFLAG, EFLAG1, EFLAG2

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA IFLAG1 /1/, IFLAG2 /2/
      DATA OFLAG /.FALSE./, EFLAG1 /.FALSE./, EFLAG2 /.FALSE./

      logical iop

* Initialize

      NOGO = .FALSE.

* Form complete names of grid files

      CALL NBLANK (AFILE, IFLAG2, N2)
      IF (N2 .EQ. 0) STOP 'Logical Coding Error in OPENF'

      ALAS = B69
      ALAS(1:N2) = AFILE

      ALAS(N2+1:N2+4) = '.las'
      ALOS = B69
      ALOS(1:N2) = AFILE
      ALOS(N2+1:N2+4) = '.los'

*******************************************************
* DIRECT ACCESS GRID FILES
* Each file is opened once to get the grid variables.
* The file is then closed and reopened to ensure that
* the record length is correct
*******************************************************

* Seconds of latitude grid file

      LRECL = 256
      ILA = 2*ITEMP - 1
*GR   IFILE = ILA + 10
*GR       added following code to do dynamic unit allocation
        do 20 i=10,99
        inquire (unit=i, opened=iop, err=10)
        if (iop) goto 20
        goto 30
 10     write (*,'(a)') 'ERROR opening grid files from nadcon/openfl'
        stop
 20     continue
 30     ifile = i
*GR       end added code

      LUAREA(ILA) = IFILE
      INQUIRE (FILE=ALAS, EXIST=EFLAG1, OPENED=OFLAG)
      IF (.NOT. EFLAG1) GOTO 100
      IF (OFLAG) GOTO 980
cBriel:
c       +--------------------------------------------------------+
c       | NOTE: To allow for simultaneous access by more than    |
c       | one user, input files can be opened in READ-only mode. |
c       | Output files (if any) can be opened with a unique name |
c       | tag, such as:  userdatetime_outfilename.               |
c       +--------------------------------------------------------+
c
      OPEN (IFILE,FILE=ALAS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       action='READ')
C
      READ (IFILE,REC=1) RIDENT, PGM, NC1, NR1, NZ1, X01, DX1,
     +                   Y01, DY1, ANGLE1
      CLOSE (IFILE)

      LRECL = 4*(NC1+1)
cBriel:
      OPEN (IFILE,FILE=ALAS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       action='READ')
C

* Seconds of longitude grid file

  100 LRECL = 256
      ILO = 2*ITEMP
*GR   IFILE = ILO + 10
*GR       added following code to do dynamic unit allocation
        do 120 i=10,99
        inquire (unit=i, opened=iop, err=110)
        if (iop) goto 120
        goto 130
 110    write (*,'(a)') 'ERROR opening grid files from nadcon/openfl'
        stop
 120    continue
 130    ifile = i
*GR       end added code

      LUAREA(ILO) = IFILE
      INQUIRE (FILE=ALOS, EXIST=EFLAG2, OPENED=OFLAG)
      IF (.NOT. EFLAG1) GOTO 910
      IF (.NOT. EFLAG2) GOTO 920
      IF (OFLAG) GOTO 980
cBriel:
      OPEN (IFILE,FILE=ALOS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       action='READ')
C
      READ (IFILE,REC=1) RIDENT, PGM, NC2, NR2, NZ2, X02, DX2,
     +                   Y02, DY2, ANGLE2
      CLOSE (IFILE)

      LRECL = 4*(NC2+1)
cBriel:
      OPEN (IFILE,FILE=ALOS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       action='READ')
C

* Check to see if the two files have the same variables

      IF ( (NC2 .NE. NC1)  .OR.  (NR2 .NE. NR1)  .OR.
     +     (NZ2 .NE. NZ1)  .OR.
     +     (X02 .NE. X01)  .OR.  (DX2 .NE. DX1)  .OR.
     +     (Y02 .NE. Y01)  .OR.  (DY2 .NE. DY1)  .OR.
     +     (ANGLE2 .NE. ANGLE1) ) GOTO 960

* Calculate values used in this program

      XMIN1 = DBLE(X01)
      YMIN1 = DBLE(Y01)
      XMAX1 = DBLE(X01) + (NC1-1)*DBLE(DX1)
      YMAX1 = DBLE(Y01) + (NR1-1)*DBLE(DY1)
      DX = DBLE( ABS(DX1) )
      DY = DBLE( ABS(DY1) )

*****************************************
* REPORT SOMETHING ABOUT THE GRIDDED DATA
*****************************************
*     WRITE (LUOUT,4050) RIDENT, PGM, NC1, NR
*4050 FORMAT (1X, A56, /, 1X, A8, /, I5, I5)
*     WRITE (LUOUT,*) 'DX,DY,NR,NC', DX1, DY1, NR1, NC1
*     WRITE (LUOUT,4055) -XMAX1, -XMIN1, YMIN1, YMAX1
*4055 FORMAT (' MIN Longitude = ', F10.4, ' MAX Longitude = ', F10.4, /,
*    +        ' MIN Latitude  = ', F10.4, ' MAX Latitude  = ', F10.4, /)
*****************************************

 9999 RETURN

****************************
* WARNING AND ERROR MESSAGES
****************************

* Grid files do not exist

  910 CONTINUE
      NOGO = .TRUE.
      IF (GFLAG) THEN
        CALL NBLANK (ALAS, IFLAG1, N1)
        CALL NBLANK (ALAS, IFLAG2, N2)
        CALL NBLANK (CARD, IFLAG1, N3)
        CALL NBLANK (CARD, IFLAG2, N4)
        IF (EFLAG2) THEN

* .los exists, .las does not exist

          WRITE (LUOUT, 925) ALAS(N1:N2), CARD(N3:N4), ALOS(N1:N2)
        ELSE

* neither .los nor .las exist

          WRITE (LUOUT, 915) ALAS(N1:N2), ALOS(N1:N2), CARD(N3:N4)
  915     FORMAT (/, 5X, ' *********** WARNING ***********', /,
     +            5X, ' The grid files', /,
     +            6X, '''', A, '''', /,
     +            6X, '''', A, '''', /,
     +            5X, ' from record:', /, 
     +            6X, '''', A, '''', /,
     +            5X, ' do not exist!', /,
     +            5X, ' *******************************', /)
        ENDIF
      ENDIF
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

  920 CONTINUE
      NOGO = .TRUE.
      IF (GFLAG) THEN

* .las exists, .los does not exist

        CALL NBLANK (ALAS, IFLAG1, N1)
        CALL NBLANK (ALAS, IFLAG2, N2)
        CALL NBLANK (CARD, IFLAG1, N3)
        CALL NBLANK (CARD, IFLAG2, N4)
        WRITE (LUOUT, 925) ALOS(N1:N2), CARD(N3:N4), ALAS(N1:N2)
  925   FORMAT (/, 5X, ' *********** WARNING ***********', /,
     +          5X, ' The grid file', /,
     +          6X, '''', A, '''', /,
     +          5X, ' from record:', /,
     +          6X, '''', A, '''', /,
     +          5X, ' does not exist!  However, the grid file', /,
     +          6X, '''', A, '''', /,
     +          5X, ' does exist!', /,
     +          5X, ' *******************************', /)
      ENDIF
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid file(s) already open

  940 CONTINUE
      NOGO = .TRUE.
      IF (GFLAG) THEN
        CALL NBLANK (ALAS, IFLAG1, N1)
        CALL NBLANK (ALAS, IFLAG2, N2)
        CALL NBLANK (CARD, IFLAG1, N3)
        CALL NBLANK (CARD, IFLAG2, N4)
        WRITE (LUOUT, 945) ALAS(N1:N2), ALOS(N1:N2), CARD(N3:N4), IOS
  945   FORMAT (/, 5X, ' *********** WARNING ***********', /,
     +          5X, ' The grid file', /,
     +          6X, '''', A, '''', /,
     +          5X, ' and/or grid file', /,
     +          6X, '''', A, '''', /,
     +          5X, ' from record:', /,
     +          6X, '''', A, '''', /,
     +          5X, ' cannot be opened!',
     +                      '  These files will be ignored.', 5X, I5, /,
     +          5X, ' *******************************', /)
      ENDIF
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid files do not agree

  960 CONTINUE
      NOGO = .TRUE.
      CALL NBLANK (ALAS, IFLAG1, N1)
      CALL NBLANK (ALAS, IFLAG2, N2)
      WRITE (LUOUT, 965) ALAS(N1:N2), ALOS(N1:N2)
  965 FORMAT (/, 5X, ' *********** ERROR ***********', /,
     +        5X, ' The header information in grid files', /,
     +        6X, '''', A, '''', /,
     +        5X, ' and', /,
     +        6X, '''', A, '''', /,
     +        5X, ' do not agree!  One or both of these files must',
     +                                             ' be corrupted.', /,
     +        5X, ' These files will be ignored.', /,
     +        5X, ' *****************************', /)
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid files already open

  980 CONTINUE
      NOGO = .TRUE.
      IF (GFLAG) THEN
        CALL NBLANK (ALAS, IFLAG1, N1)
        CALL NBLANK (ALAS, IFLAG2, N2)
        WRITE (LUOUT, 985) ALAS(N1:N2), ALOS(N1:N2)
  985   FORMAT (/, 5X, ' *********** ERROR ***********', /,
     +          5X, ' The grid file', /,
     +          6X, '''', A, '''', /,
     +          5X, ' and the grid file', /,
     +          6X, '''', A, '''', /,
     +          5X, ' have already been opened!  These files',
     +                                      ' will not be reopened.', /,
     +          5X, ' *****************************', /)
      ENDIF
      GOTO 9999
      END
