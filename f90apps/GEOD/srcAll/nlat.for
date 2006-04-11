*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/nlat.f,v 1.1 1998/07/07 20:20:43 grogers Exp $
*  nlat.f
*
      SUBROUTINE NLAT (KOUT, YMIN, YMAX)

* Purpose: Get the new latitude variables
*****************************************
*
*  $Log: nlat.f,v $
*  Revision 1.1  1998/07/07 20:20:43  grogers
*  PR#0, initial add of nadgrd_lib
*
*

*     IMPLICIT REAL (A-H, O-Z)
*     IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

* SMALL is system dependent and should be a very small real number

      REAL SMALL
      PARAMETER (SMALL = 1.E-5)

      REAL RCARD
      REAL YMIN, YMAX
      INTEGER LENG, IERR, KOUT
      CHARACTER*10 ANS

      INTEGER LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO
      COMMON /INOUT/ LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO

      REAL XMIN0, XMAX0, YMIN0, YMAX0, DX, DY
      INTEGER NR, NC
      COMMON /GRID0/ XMIN0, XMAX0, YMIN0, YMAX0, DX, DY, NR, NC

  110 WRITE (LUOUT,115) YMIN0
  115 FORMAT (/, ' Enter the minimum latitude, north positive.',
     +        /, ' The default value is:', F8.0)
      READ (LUIN,'(A10)') ANS
      IF (ANS .EQ. '          ') THEN
        YMIN = YMIN0
      ELSE
        LENG = 10
        YMIN = RCARD (ANS, LENG, IERR)
      ENDIF

      WRITE (LUOUT,120) YMAX0
  120 FORMAT (/, ' Enter the maximum latitude, north positive.',
     +        /, ' The default value is:', F8.0)
      READ (LUIN,'(A10)') ANS
      IF (ANS .EQ. '          ') THEN
        YMAX = YMAX0
      ELSE
        LENG = 10
        YMAX = RCARD (ANS, LENG, IERR)
      ENDIF

* check values

      IF (YMIN .LT. YMIN0  .OR. YMIN .GT. YMAX0  .OR.
     +    YMAX .LT. YMIN0  .OR. YMAX .GT. YMAX0) THEN
        WRITE (LUOUT, 130) YMIN0, YMAX0
  130   FORMAT (/, ' *** ERROR *** ',
     +          /, ' One or both of the values that you gave for the',
     +             ' minimum and maximum',
     +          /, ' latitudes are out of bounds!  For the input',
     +             ' NADCON grids',
     +          /, ' the minimum and maximum latitudes (+N)  are: ',
     +             2F8.0)
        GOTO 110
      ELSEIF ( ( YMIN .GT. YMAX )  .OR.
     +         ( KOUT .EQ. -2  .AND.  YMIN .EQ. YMAX) ) THEN
        WRITE (LUOUT, 140) YMIN0, YMAX0
  140   FORMAT (/, ' *** ERROR *** ',
     +          /, ' The minimum value that you gave for the latitude',
     +             ' is less than the maximum!',
     +          /, ' For the input NADCON grids',
     +          /, ' the minimum and maximum latitudes (+N)  are: ',
     +             2F8.0)
        GOTO 110
      ENDIF

* The subgrid must be buffered by at least one whole degree of
* the original grid - BUT must be wholy within the original grid.

* For ASCII graphics format, buffer only to the nearest whole degree

      IF (KOUT .NE. -2) THEN
        YMIN = AINT(YMIN - 1.E0 + SMALL)
        YMAX = AINT(YMAX + 2.E0 - SMALL)
      ELSE
        YMIN = AINT(YMIN        + SMALL)
        YMAX = AINT(YMAX + 1.E0 - SMALL)
      ENDIF

      IF (YMIN .LT. YMIN0) YMIN = YMIN0
      IF (YMAX .GT. YMAX0) YMAX = YMAX0

      RETURN
      END
