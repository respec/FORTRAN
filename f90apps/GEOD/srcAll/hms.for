*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/hms.f,v 1.1 1998/07/07 19:32:18 grogers Exp $
*  hms.f
*
      SUBROUTINE HMS (DD, ID, IM, S)

*  Puroose: Change from decimal degrees (double precision)
*           to integer degrees, integer minutes, and
*           decimal seconds (double prec)
***********************************************************
*
*  $Log: hms.f,v $
*  Revision 1.1  1998/07/07 19:32:18  grogers
*  PR#0, initial load of nadcon_lib
*
*
* Seconds are assumed to have no more than 5 decimal places

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION SMALL
      PARAMETER (SMALL = 1.D-5)

      DOUBLE PRECISION DD, TEMP
      DOUBLE PRECISION S
      INTEGER ID, IM

      ID = IDINT(DD)
      TEMP = ( DD - DBLE(ID) )*60.0D0
      IM = IDINT(TEMP)
      S = ( TEMP - DBLE(IM) )*60.0D0

      IF (IM .EQ. 60) THEN
        IM = 0
        ID = ID + 1
      ENDIF

      IF (S .LT. SMALL) S = 0.D0

      IF (S .GT. (60.D0-SMALL)  ) THEN
        S = 0.D0
        IM = IM + 1
      ENDIF

      RETURN
      END
