*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/print.f,v 1.1 1998/07/07 20:10:26 grogers Exp $
*  print.f
*
      SUBROUTINE PRINT (LU, NPOINT, NAME, VRSION, 
     +    IDLA, IMLA, SLA, IDLO, IMLO, SLO, GHT, IPAGE, PAGE)
*
* Purpose: Prints out the actual transformation results
*******************************************************
*
*  $Log: print.f,v $
*  Revision 1.1  1998/07/07 20:10:26  grogers
*  PR#0, initial add of vertcon_lib
*
*
* Prints out the actual transformation results using
* a pretty format - not the same as the input file format (if there
* is one).  This subroutine is used by type-1 format input and
* interactive input.
*
      REAL VRSION
      REAL SLA, SLO, GHT
      INTEGER LU, IPAGE
      INTEGER IDLA, IMLA, IDLO, IMLO
      CHARACTER*40 NAME
      LOGICAL PAGE
c
      IF (NPOINT.EQ. 1) THEN
* FIRST PAGE HEADING
        WRITE (LU,10) IPAGE
        WRITE (LU,5)
        WRITE (LU,7) VRSION
        WRITE (LU,8)
      ENDIF
c
      IF (PAGE) THEN
        IF (IPAGE .GT. 1) THEN
          WRITE (LU,2)
          WRITE (LU,10) IPAGE
        ENDIF
      ENDIF
   10 FORMAT (70(' '), 'Page ', I4)
    5 FORMAT (10X, ' VERTical CONversion (VERTCON) Transformation',
     +             ' Program'/23x,'Between NGVD 29 and NAVD 88')
    7 FORMAT (30X, ' Version', F4.1)
    8 FORMAT ( / 1X, 79('=') )
    2 FORMAT ('')
      IF (NAME(1:8) .NE. '        ') THEN
         WRITE (LU,922) NAME
       ELSE
         WRITE (LU,921) NPOINT
      ENDIF
      WRITE (LU,900)
      WRITE (LU,923) IDLA, IMLA, SLA, IDLO, IMLO, SLO, GHT
  921 FORMAT (/,2x, 'Station sequence #: ', I4)
  922 FORMAT (/,2X, 'Station Name:  ', A40)
  900 FORMAT (5X, 'Latitude', 17X, 'Longitude', 4X, 'NAVD 88 - NGVD 29',
     .             ' (meters)')
  923 FORMAT (2X, I2, 1X, I2.2, F9.5, 10X, I3, 1X, I2.2, F9.5,
     +        5X, F9.3)
      RETURN
      END
