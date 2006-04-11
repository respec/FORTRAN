*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/fgrid.f,v 1.1 1998/07/07 19:32:07 grogers Exp $
*  fgrid.f
*
      SUBROUTINE FGRID (XPT, YPT, DX, DY, XMAX, XMIN,
     +                  YMAX, YMIN, XGRID, YGRID, IROW, JCOL, NOGO)

**********************************************************************
** Purpose: IDENTIFIES THE LOCAL GRID SQUARE FOR INTRP.     *
**********************************************************************

* This subroutine is designed to identify the grid square in which a
* particular point is located and get the corner coordinates
* converted into the index coordinate system.

*
*  $Log: fgrid.f,v $
*  Revision 1.1  1998/07/07 19:32:07  grogers
*  PR#0, initial load of nadcon_lib
*
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION XPT, YPT, XGRID, YGRID
      DOUBLE PRECISION XMAX, XMIN, YMAX, YMIN
      DOUBLE PRECISION DX, DY
      INTEGER IROW, JCOL
      LOGICAL NOGO

      NOGO = .FALSE.

* Check to see it the point is outside the area of the gridded data

      IF (XPT .GE. XMAX  .OR.  XPT .LE. XMIN   .OR.
     +    YPT .GE. YMAX  .OR.  YPT .LE. YMIN ) THEN
        NOGO = .TRUE.
*       WRITE (*,*) '***THE POINT IS OUT OF BOUNDS***'
        GOTO 200
      ENDIF

* Calculate the coordinate values for the point to be interpolated
* in terms of grid indices

      XGRID = ( XPT - XMIN )/DX + 1.D0
      YGRID = ( YPT - YMIN )/DY + 1.D0

* Find the I,J values for the SW corner of the local square

      IROW = IDINT(YGRID)
      JCOL = IDINT(XGRID)

  200 RETURN
      END
