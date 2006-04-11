C-----------------------------------------------------------------------
C  IMSL Name:  DMACH (Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    March 15, 1984
C
C  Purpose:    Generate double precision machine constants.
C
C  Usage:      DMACH(N)
C
C  Arguments:
C     N      - Index of desired constant.  (Input)
C     DMACH  - Machine constant.  (Output)
C              DMACH(1) = B**(EMIN-1), the smallest positive magnitude.
C              DMACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C              DMACH(3) = B**(-T), the smallest relative spacing.
C              DMACH(4) = B**(1-T), the largest relative spacing.
C              DMACH(5) = LOG10(B), the log, base 10, of the radix.
C              DMACH(6) = not-a-number.
C              DMACH(7) = positive machine infinity.
C              DMACH(8) = negative machine infinity.
C
C  GAMS:       R1
C
C  Chapters:   MATH/LIBRARY Reference Material
C              STAT/LIBRARY Reference Material
C              SFUN/LIBRARY Reference Material
C
C  Copyright:  1984 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DMACH (N)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N
C
      EXTERNAL   D1MACH
C
      DMACH(N) = D1MACH(N)
C
      RETURN
      END
