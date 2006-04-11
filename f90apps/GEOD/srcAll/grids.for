*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/grids.f,v 1.1 1998/07/07 19:32:15 grogers Exp $
*  grids.f
*
      SUBROUTINE GRIDS(dsel)

* Purpose: Opens the NADCON grids
*********************************
*
* Opens the NADCON grids that are requested in
* a file named 'AREA.PAR' (if it exists) or in a file named 'area.par'
* (if it exists).  AREA.PAR (or area.par) will be read for the names
* and locations of the gridded latitude and longitude data set files.

*
*  $Log: grids.f,v $
*  Revision 1.1  1998/07/07 19:32:15  grogers
*  PR#0, initial load of nadcon_lib
*

* The data in the AREA.PAR file has the following format:
* Columns 1-15 contain the name of the area.  This name may
*   contain blanks or any other characters.
* Columns 16-80 (the rest of the record) contain the base name of the
*   grid files.  That is the '.las' and '.lon' extensions are not
*   included - They are added by this subroutine before each file is
*   opened.

* Comments are indicated by an '*' in column 1, blank records are
* also treated as comments.  Comment records are printed to the
* output file but otherwise ignored.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*20 B20
      CHARACTER*80 B80
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)

      DOUBLE PRECISION DX1, DY1, XMAX1, XMIN1, YMAX1, YMIN1
      INTEGER IOS, I, IFLAG1, IFLAG2, NC1, N1, N2, LENG, IERR, ITEMP
      INTEGER N3
      CHARACTER*15 AAREA
      CHARACTER*65 AFILE, GFILE
      CHARACTER*80 CARD, CCARD, DUM
      LOGICAL NOGO, GFLAG,dsel

      CHARACTER*15 AREAS
      COMMON /AREAS/ AREAS(MXAREA)

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA DUM / B80 /
      DATA IFLAG1 /1/, IFLAG2 /2/

***********************************************************************
* TRY AND OPEN THE 'AREA.PAR' FILE, IF NOT THEN TRY AND OPEN 'area.par'
***********************************************************************

      GFILE = 'AREA.PAR'
cBriel:
c       +--------------------------------------------------------+
c       | NOTE: To allow for simultaneous access by more than    |
c       | one user, input files can be opened in READ-only mode. |
c       | Output files (if any) can be opened with a unique name |
c       | tag, such as:  userdatetime_outfilename.               |
c       +--------------------------------------------------------+
c
      OPEN (NAPAR,FILE=GFILE,FORM='FORMATTED',STATUS='OLD',
     +      ACCESS='SEQUENTIAL',ERR=9100,IOSTAT=IOS,
     +      action='READ')
C

* File containing grid names exits

   10 GFLAG = .TRUE.
      WRITE (LUOUT, 80)
   80 FORMAT (/, '      Data Grids named in the AREA.PAR file', /,
     +           '   #  AREA NAME      LOCATION', /, 1X, 79('=') )

      DO 120 I = 1, MXAREA
  100   READ (NAPAR,110,ERR=9000,END=9000,IOSTAT=IOS) CARD
  110   FORMAT (A80)

* Check for comment records and blank records

        IF ( CARD(1:1) .EQ. '*' ) THEN
          CALL NBLANK (CARD, IFLAG2, N2)
          WRITE (LUOUT,'(5X, A)') CARD(1:N2)
          GOTO 100
        ELSEIF ( CARD .EQ. B80 ) THEN
          WRITE (LUOUT,*) ' '
          GOTO 100
        ENDIF

* Get area name and basename of file (i.e. location without extensions)

        DUM = CARD
        AAREA = DUM(1:15)
        CALL NBLANK (CARD(16:80), IFLAG1, N1)
        DUM(1:65) = CARD(15+N1:80)
        LENG = 65
        AFILE = CCARD(DUM, LENG, IERR)
        IF (IERR .NE. 0) STOP 'Coding error in GRIDS -- AFILE'
        IF (AFILE .EQ. '        ') GOTO 9000
c now find last non-blank in afile to check for hpgn at end of name
	CALL NBLANK (AFILE(1:65),IFLAG2,N3)

c check for state hpgn file; only 1 file can be open, must end
c in 'hpgn'
	if(.not.dsel) then
	   if(NAREA.ge.1) go to 9000
	   if(AFILE(N3-3:N3).ne.'hpgn') go to 120
	else
c nad27,nad83 datum; cannot access hpgn files
	   if(AFILE(N3-3:N3).eq.'hpgn') GO TO 120
	end if

        ITEMP = NAREA + 1
        CALL OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX1, DY1,
     +               XMAX1, XMIN1, YMAX1, YMIN1, NC1, CARD)

        IF (.NOT. NOGO) THEN

* Files opened OK and variables read

          NAREA = ITEMP
          AREAS(NAREA) = AAREA
          DX(NAREA) = DX1
          DY(NAREA) = DY1
          XMAX(NAREA) = XMAX1
          XMIN(NAREA) = XMIN1
          YMAX(NAREA) = YMAX1
          YMIN(NAREA) = YMIN1
          NC(NAREA) = NC1

          CALL NBLANK (CARD, IFLAG2, N2)
          WRITE (LUOUT,140) NAREA, CARD(1:N2)
  140     FORMAT (2X, I2, 2X, A)
        ENDIF

  120 CONTINUE

 9000 RETURN

* 'AREA.PAR' does not exist, try the name 'area.par'
* If it exists, open it and continue, if it does not exist, return.

 9100 CONTINUE
      GFILE = 'area.par'
cBriel:
      OPEN (NAPAR,FILE=GFILE,FORM='FORMATTED',STATUS='OLD',
     +      ACCESS='SEQUENTIAL',ERR=9000,IOSTAT=IOS,
     +      action='READ')
C
      GOTO 10
      END
