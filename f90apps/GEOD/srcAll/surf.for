*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/surf.f,v 1.1 1998/07/07 19:32:36 grogers Exp $
*  surf.f
*
      SUBROUTINE SURF (XGRID, YGRID, ZEE, AY, BEE, CEE, DEE, IROW, JCOL)

**********************************************************************
** Purpose: INTERPOLATES THE Z VALUE                         *
**********************************************************************
*
*  $Log: surf.f,v $
*  Revision 1.1  1998/07/07 19:32:36  grogers
*  PR#0, initial load of nadcon_lib
*
*
* Calculated the value of the grid at the point XPT, YPT.  The
* interpolation is done in the index coordinate system for convenience.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION XGRID, YGRID
      DOUBLE PRECISION AY, BEE, CEE, DEE
      DOUBLE PRECISION ZEE, ZEE1, ZEE2, ZEE3, ZEE4
      INTEGER IROW, JCOL

      ZEE1 = AY
      ZEE2 = BEE*(XGRID - DBLE(JCOL) )
      ZEE3 = CEE*(YGRID - DBLE(IROW) )
      ZEE4 = DEE*(XGRID - DBLE(JCOL) )*(YGRID - DBLE(IROW) )
      ZEE  = ZEE1 + ZEE2 + ZEE3 + ZEE4

      RETURN
      END
