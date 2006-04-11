C-----------------------------------------------------------------------
C  IMSL Name:  U19NF/DU19NF  (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Check validity of input to unconstrained minimization.
C
C  Usage:      CALL U19NF (ICODE)
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
      SUBROUTINE DU19NF (ICODE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    ICODE
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES
C
      IF (ICODE .EQ. 0) THEN
         CALL E1MES (5, 1, 'The size of the problem must be '//
     &               'positive while N = %(I1) is given.')
      ELSE IF (ICODE .EQ. 1) THEN
         CALL E1MES (6, 1, 'This routine may be inefficient '//
     &               'for a problem of size N = 1.')
      ELSE IF (ICODE .EQ. 2) THEN
         CALL E1MES (6, 2, 'The diagonal scaling matrix for '//
     &               'the variables must be positive while some '//
     &               'of the entries are less than or equal to zero. '//
     &               ' The algorithm will use the identity scaling '//
     &               'matrix for XSCALE.')
      ELSE IF (ICODE .EQ. 4) THEN
         CALL E1MES (6, 4, 'The estimate of the number of '//
     &               'good digits in the function must be positive '//
     &               'while FDIGIT = %(I1) is given.  The algorithm '//
     &               'will assume that the function is accurate to '//
     &               'the precision of the arithmetic.')
      ELSE IF (ICODE .EQ. 5) THEN
         CALL E1MES (6, 5, 'The maximum number of iterations '//
     &               'must be positive while MXITER = %(I1) is '//
     &               'given.  The algorithm will use MXITER = 100.')
      ELSE IF (ICODE .EQ. 6) THEN
         CALL E1MES (6, 6, 'The maximum number of function '//
     &               'evaluations must be positive while MAXFCN = '//
     &               '%(I1) is given.  The algorithm will use '//
     &               'MAXFCN = 400.')
      ELSE IF (ICODE .EQ. 7) THEN
         CALL E1MES (6, 7, 'The maximum number of gradient '//
     &               'evaluations must be positive while MAXGRD = '//
     &               '%(I1) is given.  The algorithm will use '//
     &               'MAXGRD = 400.')
      ELSE IF (ICODE .EQ. 8) THEN
         CALL E1MES (6, 8, 'The maximum number of Hessian '//
     &               'evaluations must be positive while MAXHES = '//
     &               '%(I1) is given.  The algorithm will use '//
     &               'MAXHES = 100.')
      END IF
      RETURN
      END
