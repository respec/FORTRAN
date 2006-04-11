*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/coeff.f,v 1.1 1998/07/07 19:32:02 grogers Exp $
*  coeff.f
*
      SUBROUTINE COEFF (TEE1, TEE2, TEE3, TEE4, AY, BEE, CEE, DEE)

**********************************************************************
** Purpose: GENERATES COEFFICIENTS FOR SURFACE FUNCTION     *
**********************************************************************
*
*  $Log: coeff.f,v $
*  Revision 1.1  1998/07/07 19:32:02  grogers
*  PR#0, initial load of nadcon_lib
*
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION AY, BEE, CEE, DEE
      DOUBLE PRECISION TEE1, TEE2, TEE3, TEE4

      AY = TEE1
      BEE = TEE3 - TEE1
      CEE = TEE2 - TEE1
      DEE = TEE4 - TEE3 - TEE2 + TEE1

      RETURN
      END
