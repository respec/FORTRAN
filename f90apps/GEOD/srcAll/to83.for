*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/to83.f,v 1.1 1998/07/07 19:32:37 grogers Exp $
*  to83.f
*
      SUBROUTINE TO83 (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +                 DLAM, DLOM, DLAS, DLOS, ITYPE)

* Purpose: Predicts the NAD 83 latitude and longitude values
* given the NAD 27 latitude and longitude values in degree decimal
* format.  In addition, the program returns the shift values between
* the datums in both arc secs and meters.
********************************************************************
*
*  $Log: to83.f,v $
*  Revision 1.1  1998/07/07 19:32:37  grogers
*  PR#0, initial load of nadcon_lib
*
*

* All of the predictions are based upon a straight-forward interpolation
* of a gridded data set of datum shifts.  The datum shifts are assumed
* to be provided in the files opened in the NGRIDS subroutine.  The
* common AREAS contains the names of the valid areas while the common
* GDINFO contains the grid variables.  NAREA is the number of areas
* which had data files opened.  A total of two files are necessary for
* each area: one latitude and one longitude shift table (gridded data
* set) expressed in arc seconds.

* For this subroutine, it is important to remember that the
* input longitude is assumed to be positive east and the
* output longitude will be positive east.

*       Author:     Warren T. Dewhurst, PH. D.
*                   National Geodetic Survey
*                   November 1, 1989

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2
      DOUBLE PRECISION XGRID, YGRID
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION DX0, DY0, XMAX0, XMIN0, YMAX0, YMIN0
      INTEGER IROW, JCOL, IAREA, I, NC0, ITYPE
      INTEGER IFLAG1, IFLAG2, N1, N2
      CHARACTER*15 RESP
      LOGICAL NOGO, FLAG

      CHARACTER*15 AREAS
      COMMON /AREAS/ AREAS(MXAREA)

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      CHARACTER*80 CARD
      COMMON /CURNT/ CARD

      SAVE FLAG

      DATA IFLAG1 /1/, IFLAG2 /2/, FLAG /.FALSE./

******************************************************************
*                             INITIALIZE
******************************************************************

      NOGO  =  .FALSE.

****************************************************
* READ WHERE TO GET THE DATA AND HOW IT IS ORGANIZED
****************************************************

* Check to see which set of gridded files XPT,YPT is in.

      DO 100 IAREA = 1, NAREA

        DX0 = DX(IAREA)
        DY0 = DY(IAREA)
        XMAX0 = XMAX(IAREA)
        XMIN0 = XMIN(IAREA)
        YMAX0 = YMAX(IAREA)
        YMIN0 = YMIN(IAREA)
        NC0 = NC(IAREA)
        CALL FGRID (XPT, YPT, DX0, DY0, XMAX0, XMIN0,
     +              YMAX0, YMIN0, XGRID, YGRID, IROW, JCOL, NOGO)
        IF (.NOT. NOGO) GOTO 200

  100 CONTINUE

* Not in any of the grid areas

      NOGO = .TRUE.
      GOTO 950

  200 CONTINUE

* Point in area number IAREA and named AREAS(IAREA)

        RESP = AREAS(IAREA)
        CALL INTRP (IAREA, IROW, NC0, JCOL, XGRID, YGRID,
     +              XPT, YPT, XPT2, YPT2, DLOS, DLAS, DLAM, DLOM)
9999    RETURN

* Error Messages

  950 CONTINUE
      IF (ITYPE .NE. 0) THEN
        CALL NBLANK (CARD, IFLAG1, N1)
        CALL NBLANK (CARD, IFLAG2, N2)
        WRITE (LUOUT,955) CARD(N1:N2)
  955   FORMAT (' *** THIS POINT IS OUT OF BOUNDS ***', /,
     +          1X, '''', A, '''')
      ELSE
        WRITE (LUOUT,960)
  960   FORMAT (' *** THE POINT IS OUT OF BOUNDS ***')
      ENDIF

* Write out grid areas for the first out-of-bounds error message

      IF (.NOT.FLAG  .OR.  ITYPE .EQ. 0) THEN
        WRITE (LUOUT,*) ' It must be within one of the following grid',
     +                  ' areas;'
        WRITE (LUOUT,975)
  975   FORMAT (18X, 7X, 'Latitude', 7X, 'Longitude', /,
     +          5X, 'Area Name', 5X, 2(5X, 'MIN', 4X, 'MAX', 1X),
     +          '(degrees)' )
        DO 970 I = 1, NAREA
          WRITE (LUOUT,965) AREAS(I),
     +                      IDNINT(  YMIN(I) ), IDNINT(  YMAX(I) ),
     +                      IDNINT( -XMAX(I) ), IDNINT( -XMIN(I) )
  965     FORMAT (1X, '''', A15, '''', 2(2X, 2I7) )
  970   CONTINUE
        FLAG = .TRUE.
      ENDIF

      WRITE (LUOUT,*) ' '
      GOTO 9999
      END
