*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/nlon.f,v 1.1 1998/07/07 20:20:44 grogers Exp $
*  nlon.f
*
      SUBROUTINE NLON (KOUT, XMIN, XMAX)

* Purpose: Get the new longitude variables.
*******************************************
*
*  $Log: nlon.f,v $
*  Revision 1.1  1998/07/07 20:20:44  grogers
*  PR#0, initial add of nadgrd_lib
*
*

* The variables LMIN is the minimum number of degrees between the
* minimum longitude and the maximum longitude.  The reason that
* there must be this minimum is that in order to be read by NADCON,
* the extracted grid must have a logical record length of at least 96.
* This is because the header record length is 96 bytes.
* This variables is not used for the graphics output (KOUT=-2)

* What happens is that the output file is opened with a record length
* of 4*(NC2+1) and the first record written.  The second record
* is then written into the file starting at character 4*(NC2+1)+1.
* where NC2 is the number of columns in the output file and is
* determined by the longitude difference and the longitude increment.
* Since the shifts are written as REAL*4 numbers, there must be at
* least 24 numbers in a row.

* For example, the conus files have .25=DX.  NADGRD requires that the
* grid boundarys be even degrees.  In order to have at least 24
* number in a row, the (maximum-minimum) longitude must be at least 6
* degrees.  However, given buffering, the inputted (maximum-minimum)
* longitude must be at least 4 degrees.

*     IMPLICIT REAL (A-H, O-Z)
*     IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

* SMALL is system dependent and should be a very small real number

      REAL SMALL
      PARAMETER (SMALL = 1.E-5)

      REAL RCARD
      REAL XMIN, XMAX
      INTEGER LENG, IERR, KOUT, LMIN
      CHARACTER*10 ANS

      INTEGER LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO
      COMMON /INOUT/ LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO

      REAL XMIN0, XMAX0, YMIN0, YMAX0, DX, DY
      INTEGER NR, NC
      COMMON /GRID0/ XMIN0, XMAX0, YMIN0, YMAX0, DX, DY, NR, NC

  160 WRITE (LUOUT,150) -XMAX0
  150 FORMAT (/, ' Enter the minimum longitude,',
     +           ' west longitude positive.',
     +        /, ' The default value is:', F8.0)
      READ (LUIN,'(A10)') ANS
      IF (ANS .EQ. '          ') THEN
        XMAX = XMAX0
      ELSE
        LENG = 10
        XMAX = RCARD (ANS, LENG, IERR)
        XMAX = -XMAX
      ENDIF
      IF (KOUT .NE. -2) THEN
        LMIN = INT(24.E0*DX + SMALL)
        IF (LMIN .LT. 1) LMIN = 1
        WRITE (LUOUT,155) LMIN, -XMIN0
  155   FORMAT (/, ' Enter the maximum longitude,',
     +             ' west longitude positive.',
     +          /, ' The (buffered) difference between the minimum and',
     +             ' maximum longitudes',
     +          /, ' must be at least', I3, ' degrees.  The default',
     +             ' value is:', F8.0)
      ELSE
        WRITE (LUOUT,165) -XMIN0
  165   FORMAT (/, ' Enter the maximum longitude,',
     +             ' west longitude positive.',
     +          /, ' The default value is:', F8.0)
      ENDIF
      READ (LUIN,'(A10)') ANS
      IF (ANS .EQ. '          ') THEN
        XMIN = XMIN0
      ELSE
        LENG = 10
        XMIN = RCARD (ANS, LENG, IERR)
        XMIN = -XMIN
      ENDIF

* check values

      IF (XMIN .LT. XMIN0  .OR. XMIN .GT. XMAX0  .OR.
     +    XMAX .LT. XMIN0  .OR. XMAX .GT. XMAX0) THEN
        WRITE (LUOUT, 170) -XMAX0, -XMIN0
  170   FORMAT (/, ' *** ERROR *** ',
     +          /, ' One or both of the values that you gave for the',
     +             ' minimum and maximum',
     +          /, ' longitudes are out of bounds!  For the input',
     +             ' NADCON grids',
     +          /, ' the minimum and maximum longitudes (+W)  are: ',
     +             2F8.0)
        GOTO 160
      ELSEIF ( ( XMIN .GT. XMAX )  .OR.
     +         ( KOUT .EQ. -2  .AND.  XMIN .EQ. XMAX) ) THEN
        WRITE (LUOUT, 180) -XMAX0, -XMIN0
  180   FORMAT (/, ' *** ERROR *** ',
     +          /, ' The minimum value that you gave for the longitude',
     +             ' is less than the maximum!',
     +          /, ' For the input NADCON grids',
     +          /, ' the minimum and maximum longitudes (+W)  are: ',
     +             2F8.0)
        GOTO 160
      ENDIF

* The subgrid must be buffered by at least one whole degree of
* the original grid - BUT must be wholy within the original grid.

* For ASCII graphics format, buffer only to the nearest whole degree

      IF (KOUT .NE. -2) THEN
        XMIN = AINT(XMIN - 2.E0 + SMALL)
        XMAX = AINT(XMAX + 1.E0 - SMALL)
      ELSE
        XMIN = AINT(XMIN - 1.E0 + SMALL)
        XMAX = AINT(XMAX        - SMALL)
      ENDIF

      IF (XMIN .LT. XMIN0) XMIN = XMIN0
      IF (XMAX .GT. XMAX0) XMAX = XMAX0

* check range

      IF ( KOUT .NE. -2  .AND.  (XMAX-XMIN) .LT. LMIN ) THEN
        WRITE (LUOUT, 190) LMIN, -XMAX0, -XMIN0
  190   FORMAT (/, ' *** ERROR *** ',
     +          /, ' The difference between the minimum and maximum',
     +             ' longitudes MUST be at',
     +          /, ' least', I3, ' degrees. ',
     +             ' For the input NADCON grids',
     +          /, ' the minimum and maximum longitudes (+W)  are: ',
     +             2F8.0)
        GOTO 160
      ENDIF

      RETURN
      END
