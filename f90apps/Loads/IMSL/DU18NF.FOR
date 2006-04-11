C-----------------------------------------------------------------------
C  IMSL Name:  U18NF/DU18NF  (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Stopping conditions for unconstrained minimization.
C
C  Usage:      CALL U18NF (ICODE)
C
C  Arguments:
C     ICODE  - Integer flag containing an error code.  (Input)
C
C  Chapter:    MATH/LIBRARY Optimization
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DU18NF (ICODE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    ICODE
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES
C
      IF (ICODE .EQ. 3) THEN
         CALL E1MES (4, 3, ' Maximum number of iterations '//
     &               'exceeded.')
      ELSE IF (ICODE .EQ. 4) THEN
         CALL E1MES (4, 4, ' Maximum number of function evaluations '//
     &               'exceeded.')
      ELSE IF (ICODE .EQ. 5) THEN
         CALL E1MES (4, 5, ' Maximum number of gradient evaluations '//
     &               'exceeded.')
      ELSE IF (ICODE .EQ. 7) THEN
         CALL E1MES (4, 7, 'Maximum number of Hessian evaluations '//
     &               'exceeded.')
      ELSE IF (ICODE .EQ. 6) THEN
         CALL E1MES (4, 6, ' Five consecutive steps of length STEPMX '//
     &               'have been taken; either the function is '//
     &               'unbounded below, or has a finite asymptote in '//
     &               'some direction or the maximum allowable step '//
     &               'size STEPMX is too small.')
      END IF
      RETURN
      END
