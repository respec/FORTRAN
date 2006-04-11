*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/meter2.f,v 1.1 1998/07/07 20:20:41 grogers Exp $
*  meter2.f
*
      SUBROUTINE METER2 (LAT, LON, DLAT, DLON, DLATM, DLONM)

* Purpose: Computes a distance in meters at a position
* from a distance in seconds of arc.
******************************************************
*
*  $Log: meter2.f,v $
*  Revision 1.1  1998/07/07 20:20:41  grogers
*  PR#0, initial add of nadgrd_lib
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

*     IMPLICIT REAL (A-H, O-Z)
*     IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

* I think that these are GRS80 parameters

      DOUBLE PRECISION AXIS, E2, RHOSEC
      PARAMETER (AXIS = 6378137.0D0)
      PARAMETER (E2 = 0.0066943800229D0)
      PARAMETER (RHOSEC = 206264.806247D0)

      DOUBLE PRECISION LAT, LON, RLAT, RLON, DLAT, DLON, RDLAT, RDLON
      DOUBLE PRECISION W, LM, LP
      REAL DLATM, DLONM

* Change into radians and convert to +west longitude

      RLAT =  LAT*60.D0*60.D0/RHOSEC
      RLON = -LON*60.D0*60.D0/RHOSEC

      RDLAT = DLAT/RHOSEC
      RDLON = DLON/RHOSEC

      W  = DSQRT(1.0D0 - E2*DSIN(RLAT)**2)
      LM = AXIS*(1.0D0 - E2)/(W**3)
      LP = AXIS*DCOS(RLAT)/W

      DLATM = SNGL(LM*RDLAT)
      DLONM = SNGL(LP*RDLON)

      RETURN
      END
