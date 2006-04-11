*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/meters.f,v 1.1 1998/07/07 19:32:25 grogers Exp $
*  meters.f
*
      SUBROUTINE METERS (LAT1, LONG1, LAT2, LONG2, LATMTR, LONMTR)

* Purpose: Computes the difference in two positions in meters.
**************************************************************
*
*  $Log: meters.f,v $
*  Revision 1.1  1998/07/07 19:32:25  grogers
*  PR#0, initial load of nadcon_lib
*
*
*
* This method utilizes ellipsoidal rather than spherical
* parameters.  I believe that the original approach and code
* for this came from Ed McKay.
* The reference used by Ed McKay for this was:
*       'A Course in Higher Geodesy' by P.W. Zakatov, Israel Program
*       for Scientific Translations, Jerusalem, 1962
*
*       Warren T. Dewhurst
*       11/1/89
* Note that this subroutine is set up for +west longitude

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

* I think that these are GRS80 parameters

      DOUBLE PRECISION AXIS, E2, RHOSEC
      PARAMETER (AXIS = 6378137.0D0)
      PARAMETER (E2 = 0.0066943800229D0)
      PARAMETER (RHOSEC = 206264.806247D0)

      DOUBLE PRECISION W, LM, LP, AVLAT
      DOUBLE PRECISION LAT1S, LAT2S, LONG1S, LONG2S, LAT1, LAT2
      DOUBLE PRECISION LONG1, LONG2, DLAT, DLONG
      DOUBLE PRECISION LATMTR, LONMTR


*     LAT1  = (LATSEC + 60.D0*( LATMIN + 60.D0*LATDEG) )/RHOSEC
*     LONG1 = (LONSEC + 60.D0*( LONMIN + 60.D0*LONDEG) )/RHOSEC
*     LAT2  = (LATSEC + 60.D0*( LATMIN + 60.D0*LATDEG) )/RHOSEC
*     LONG2 = (LONSEC + 60.D0*( LONMIN + 60.D0*LONDEG) )/RHOSEC

* Change into sec.ddd and convert to +west longitude

      LAT1S =    LAT1*60.D0*60.D0/RHOSEC
      LONG1S = -LONG1*60.D0*60.D0/RHOSEC
      LAT2S =    LAT2*60.D0*60.D0/RHOSEC
      LONG2S = -LONG2*60.D0*60.D0/RHOSEC

      DLAT  = ( LAT2S -  LAT1S)*RHOSEC
      DLONG = (LONG2S - LONG1S)*RHOSEC

      AVLAT = (LAT1S + LAT2S)/2.0D0

      W  = DSQRT(1.0D0 - E2*DSIN(AVLAT)**2)
      LM = AXIS*(1.0D0 - E2)/(W**3*RHOSEC)
      LP = AXIS*DCOS(AVLAT)/(W*RHOSEC)

      LATMTR = LM*DLAT
      LONMTR = LP*DLONG

      RETURN
      END
