*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/diagrm.f,v 1.1 1998/07/07 19:32:05 grogers Exp $
*  diagrm.f
*
      SUBROUTINE DIAGRM (LU, NCONV, XSMALL, XBIG, YSMALL, YBIG,
     +   KEY,dsel)

* Purpose: Prints out a small diagram showing the area
*          that was transformed.
******************************************************
*
*  $Log: diagrm.f,v $
*  Revision 1.1  1998/07/07 19:32:05  grogers
*  PR#0, initial load of nadcon_lib
*
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION XTEMP, XSMALL, XBIG, YSMALL, YBIG
      DOUBLE PRECISION SLOMIN, SLOMAX, SLAMIN, SLAMAX
      INTEGER LODMIN, LOMMIN, LODMAX, LOMMAX
      INTEGER LADMIN, LAMMIN, LADMAX, LAMMAX
      INTEGER LU, NCONV, KEY
      LOGICAL dsel


      IF (KEY .EQ. -1) THEN
        XTEMP = XSMALL
        XSMALL = XBIG
        XBIG = XTEMP
        CALL HMS (XBIG,   LODMIN, LOMMIN, SLOMIN)
        CALL HMS (XSMALL, LODMAX, LOMMAX, SLOMAX)
      ELSE
        CALL HMS (-XBIG,   LODMIN, LOMMIN, SLOMIN)
        CALL HMS (-XSMALL, LODMAX, LOMMAX, SLOMAX)
      ENDIF
      CALL HMS (YSMALL, LADMIN, LAMMIN, SLAMIN)
      CALL HMS (YBIG,   LADMAX, LAMMAX, SLAMAX)

      WRITE (LU,90) NCONV
   90 FORMAT (//, ' The total number of conversions: ', I8)
      WRITE (LU,100)
  100 FORMAT (//, 30X, 'Region of Conversions')
      if(dsel) then
      WRITE (LU,1000) LODMAX, LOMMAX, SLOMAX, LODMIN, LOMMIN, SLOMIN,
     +                LADMAX, LAMMAX, SLAMAX, LADMAX, LAMMAX, SLAMAX,
     +                LADMIN, LAMMIN, SLAMIN, LADMIN, LAMMIN, SLAMIN,
     +                LODMAX, LOMMAX, SLOMAX, LODMIN, LOMMIN, SLOMIN
      else
      WRITE (LU,1001) LODMAX, LOMMAX, SLOMAX, LODMIN, LOMMIN, SLOMIN,
     +                LADMAX, LAMMAX, SLAMAX, LADMAX, LAMMAX, SLAMAX,
     +                LADMIN, LAMMIN, SLAMIN, LADMIN, LAMMIN, SLAMIN,
     +                LODMAX, LOMMAX, SLOMAX, LODMIN, LOMMIN, SLOMIN
      end if

 1000 FORMAT (5(/), T4, 'NAD 27', /,
     +       T4, 'Longitude:', 8X, I4, 1X, I2.2, 1X, F6.3, 13X, I4, 1X,
     +       I2.2, 1X, F6.3, /,
     +       T4, 'Latitude:', 9X, I4, 1X, I2.2, 1X, F6.3,
     +       ' ************', I4, 1X, I2.2, 1X, F6.3, /,
     +       5(T27, 3X, '*', T57, '*', /),
     +       T27, 3X, '*', 9X, ' NAD 27', T57, '*', /,
     +       T27, 3X, '*', 9X, '  data ', T57, '*', /,
     +       T27, 3X, '*', 9X, ' points', T57, '*', /,
     +       5(T27, 3X, '*', T57, '*', /),
     +       T4, 'Latitude:', 9X, I4, 1X, I2.2, 1X, F6.3,
     +       ' ************', I4, 1X, I2.2, 1X, F6.3, /,
     +       T4, 'Longitude:', 8X, I4, 1X, I2.2, 1X, F6.3, 13X, I4, 1X,
     +       I2.2, 1X, F6.3, //)

 1001 FORMAT (5(/), T4, 'NAD 83', /,
     +       T4, 'Longitude:', 8X, I4, 1X, I2.2, 1X, F6.3, 13X, I4, 1X,
     +       I2.2, 1X, F6.3, /,
     +       T4, 'Latitude:', 9X, I4, 1X, I2.2, 1X, F6.3,
     +       ' ************', I4, 1X, I2.2, 1X, F6.3, /,
     +       5(T27, 3X, '*', T57, '*', /),
     +       T27, 3X, '*', 9X, ' NAD 83', T57, '*', /,
     +       T27, 3X, '*', 9X, '  data ', T57, '*', /,
     +       T27, 3X, '*', 9X, ' points', T57, '*', /,
     +       5(T27, 3X, '*', T57, '*', /),
     +       T4, 'Latitude:', 9X, I4, 1X, I2.2, 1X, F6.3,
     +       ' ************', I4, 1X, I2.2, 1X, F6.3, /,
     +       T4, 'Longitude:', 8X, I4, 1X, I2.2, 1X, F6.3, 13X, I4, 1X,
     +       I2.2, 1X, F6.3, //)
      RETURN
      END
