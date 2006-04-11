      SUBROUTINE VWTLA ( CL, IATT, IROW, ICOL )
C      ************************************************************
C      *                                                          *
C      *    CONVERTED TO IBM PC:                                  *
C      *                                                          *
C      *      CONVERSION BY:  JIM FOSS                            *
C      *      DATE COMPLETED: MAY 6, 1987                         *
C      *                                                          *
C      ************************************************************
C     WRITE LINE TO SCREEN WITH IATT AT IROW,ICOL
      LOGICAL LANSI,LFIRST,LCRT
      INTEGER IVUNIT
      COMMON/LANSI/LANSI,LFIRST,LCRT,IVUNIT
      CHARACTER C*8, CL*(*)
      IF(LANSI) THEN
      CALL VATT(IATT)
      CALL VMOVCR (IROW, ICOL)
      N = LEN (CL)
      CALL CHRWT(IVUNIT,CL, N)
      CALL VATT (0)
      ELSE IF ( LCRT ) THEN
      CALL VMOVCR (IROW, ICOL)
      N = LEN (CL)
      CALL CHRWT (IVUNIT,CL, N)
      ENDIF
      RETURN
      END
