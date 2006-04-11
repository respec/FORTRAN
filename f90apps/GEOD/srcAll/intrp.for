*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/intrp.f,v 1.1 1998/07/07 19:32:22 grogers Exp $
*  intrp.f
*
      SUBROUTINE INTRP (IAREA, IROW, NC, JCOL, XGRID, YGRID,
     +                  XPT, YPT, XPT2, YPT2, DLOS, DLAS, DLAM, DLOM)

**********************************************************************
** Purpose: DETERMINE SURFACE FUNCTION FOR THIS GRID SQUARE                   *
** AND INTERPOLATE A VALUE, ZEE, FOR XPT, YPT                        *
**********************************************************************

*
*  $Log: intrp.f,v $
*  Revision 1.1  1998/07/07 19:32:22  grogers
*  PR#0, initial load of nadcon_lib
*
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA, MAXCOL
      PARAMETER (MAXCOL = 600, MXAREA = 8)

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2, XGRID, YGRID
      DOUBLE PRECISION DLOS, DLAS, DLAM, DLOM
      DOUBLE PRECISION TEE1, TEE2, TEE3, TEE4, ZEE
      INTEGER IROW, JCOL, NC, IAREA, IFILE, IDUM, J
      REAL BUF(MAXCOL)

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DOUBLE PRECISION AY1, BEE1, CEE1, DEE1, AY2, BEE2, CEE2, DEE2
      SAVE AY1, BEE1, CEE1, DEE1, AY2, BEE2, CEE2, DEE2
      INTEGER IROWL, JCOLL, IAREAL
      SAVE IROWL, JCOLL, IAREAL

      DATA IROWL / 0 /, JCOLL / 0 /, IAREAL / 0 /

**********
* LATITUDE
**********

      IF ( IROW .NE. IROWL  .OR.  JCOL .NE. JCOLL  .OR.
     +    IAREA .NE. IAREAL ) THEN

* Lower boundary

        IFILE = LUAREA( 2*IAREA - 1 )
        READ (IFILE,REC=IROW+1) IDUM, (BUF(J), J=1,NC)
        TEE1 = DBLE( BUF(JCOL) )
*       TEE4 = DBLE( BUF(JCOL+1) )
        TEE3 = DBLE( BUF(JCOL+1) )

* Upper boundary

        READ (IFILE,REC=IROW+2) IDUM, (BUF(J), J=1,NC)
        TEE2 = DBLE( BUF(JCOL) )
*       TEE3 = DBLE( BUF(JCOL+1) )
        TEE4 = DBLE( BUF(JCOL+1) )

        CALL COEFF (TEE1, TEE2, TEE3, TEE4, AY1, BEE1, CEE1, DEE1)

      ENDIF

      CALL SURF (XGRID, YGRID, ZEE, AY1, BEE1, CEE1, DEE1, IROW, JCOL)
      DLAS = ZEE

***********
* LONGITUDE
***********

      IF ( IROW .NE. IROWL  .OR.  JCOL .NE. JCOLL  .OR.
     +    IAREA .NE. IAREAL ) THEN


* Lower boundary

        IFILE = LUAREA( 2*IAREA )
        READ (IFILE,REC=IROW+1) IDUM, (BUF(J), J=1,NC)
        TEE1 = DBLE( BUF(JCOL) )
*       TEE4 = DBLE( BUF(JCOL+1) )
        TEE3 = DBLE( BUF(JCOL+1) )

* Upper boundary

        READ (IFILE,REC=IROW+2) IDUM, (BUF(J), J=1,NC)
        TEE2 = DBLE( BUF(JCOL) )
*       TEE3 = DBLE( BUF(JCOL+1) )
        TEE4 = DBLE( BUF(JCOL+1) )

        CALL COEFF (TEE1, TEE2, TEE3, TEE4, AY2, BEE2, CEE2, DEE2)

      ENDIF

      CALL SURF (XGRID, YGRID, ZEE, AY2, BEE2, CEE2, DEE2, IROW, JCOL)
      DLOS = ZEE

**************************
* COMPUTE THE NAD 83 VALUES
**************************

      YPT2 = YPT + DLAS/3600.D0

* Longitude is positive west in this subroutine

      XPT2 = XPT - DLOS/3600.D0

*********************************************************************
* USE THE NEW ELLIPSOIDAL VARIABLES TO COMPUTE THE SHIFTS IN METERS
*********************************************************************

      CALL METERS (YPT, XPT, YPT2, XPT2, DLAM, DLOM)

* Update the last-value variables

      IROWL = IROW
      JCOLL = JCOL
      IAREAL = IAREA

      RETURN
      END
