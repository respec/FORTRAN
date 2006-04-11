*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/nparms.f,v 1.1 1998/07/07 20:20:45 grogers Exp $
*  nparms.f
*
      SUBROUTINE NPARMS (KOUT, XMIN, XMAX, YMIN, YMAX,
     +                   NRFRST, NRLAST, NCFRST, NCLAST)

* Purpose: Get the variables for the new grid files.
****************************************************
*
*  $Log: nparms.f,v $
*  Revision 1.1  1998/07/07 20:20:45  grogers
*  PR#0, initial add of nadgrd_lib
*
*

*     IMPLICIT REAL (A-H, O-Z)
*     IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

* SMALL is system dependent and should be a very small real number

      REAL SMALL
      PARAMETER (SMALL = 1.E-5)

      REAL XMIN, XMAX, YMIN, YMAX
      INTEGER KOUT, NCFRST, NCLAST, NRFRST, NRLAST
      CHARACTER*1 ANS

      INTEGER LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO
      COMMON /INOUT/ LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO

      REAL XMIN0, XMAX0, YMIN0, YMAX0, DX, DY
      INTEGER NC, NR
      COMMON /GRID0/ XMIN0, XMAX0, YMIN0, YMAX0, DX, DY, NR, NC

* If only printing grid information then set defaults and return

      IF (KOUT .EQ. 0) THEN
        YMIN = YMIN0
        YMAX = YMAX0
        XMAX = XMAX0
        XMIN = XMIN0
        NRFRST = 1
        NRLAST = NR
        NCFRST = 1
        NCLAST = NC
        RETURN
      ENDIF

*** Get the area variables of the grid

      WRITE (LUOUT,2)
*   2 FORMAT ('1')
    2 FORMAT ('')
      IF (KOUT .NE. -2) THEN
        WRITE (LUOUT,100)
  100   FORMAT (   ' The extracted NADCON grids will be automatically',
     +             ' buffered to the next',
     +          /, ' whole degree plus one by NADGRD.  Please enter',
     +             ' the latitude and longitude',
     +          /, ' extremes for your area of interest.  Enter the',
     +             ' values in decimal degrees.',
     +          /, ' If you enter blanks, the extremes from the input',
     +             ' files will be used.')
      ELSE
        WRITE (LUOUT,105)
  105   FORMAT (   ' The extracted NADCON grids will be automatically',
     +             ' rounded to the next',
     +          /, ' whole degree by NADGRD.  Please enter the',
     +             ' latitude and longitude',
     +          /, ' extremes for your area of interest.  Enter the',
     +             ' values in decimal degrees.',
     +          /, ' If you enter blanks, the extremes from the input',
     +             ' files will be used.')
      ENDIF

* latitude

  200 CALL NLAT (KOUT, YMIN, YMAX)

* longitude, change to east positive

      CALL NLON (KOUT, XMIN, XMAX)

* Ask if the new maximums and minimums are OK

      WRITE (LUOUT,2)
      IF ( KOUT .NE. -2  .AND.
     +     ( XMIN .NE. XMIN0  .OR.  XMAX .NE. XMAX0  .OR.
     +       YMIN .NE. YMIN0  .OR.  YMAX .NE. YMAX0 )  ) THEN
        WRITE (LUOUT, 210) YMIN, YMAX, -XMAX, -XMIN
  210   FORMAT (   ' For the output grids;',
     +          /, ' the (buffered) minimum and maximum latitudes ',
     +             ' (+N) are: ', 2F8.0,
     +          /, ' the (buffered) minimum and maximum longitudes',
     +             ' (+W) are: ', 2F8.0)
      ELSE
        WRITE (LUOUT, 215) YMIN, YMAX, -XMAX, -XMIN
  215   FORMAT (   ' For the output grids;',
     +          /, ' the minimum and maximum latitudes ',
     +             ' (+N) are: ', 2F8.0,
     +          /, ' the minimum and maximum longitudes',
     +             ' (+W) are: ', 2F8.0)
      ENDIF

      WRITE (LUOUT,220)
  220 FORMAT (/, ' If these values are OK, hit RETURN to continue.',
     +           '  Enter any other character',
     +        /, ' to re-enter the minimum and maximum latitudes',
     +           ' and longitudes.')
      READ (LUIN,'(A1)') ANS
      IF (ANS .NE. ' ') GOTO 200

* Calculate the new origin for the subgrid area,
* calculate the number of columns and rows

      NRFRST = NINT( (YMIN - YMIN0)/DY ) + 1
      NRLAST = NINT( (YMAX - YMIN0)/DY ) + 1
      NCFRST = NINT( (XMIN - XMIN0)/DX ) + 1
      NCLAST = NINT( (XMAX - XMIN0)/DX ) + 1

      RETURN
      END
