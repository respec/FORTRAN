      SUBROUTINE VEND
C      ************************************************************
C      *                                                          *
C      *    CONVERTED TO IBM PC:                                  *
C      *                                                          *
C      *      CONVERSION BY:  JIM FOSS                            *
C      *      DATE COMPLETED: MAY 6, 1987                         *
C      *                                                          *
C      ************************************************************
C     MOVE CURSOR TO BOTTOM LEFT OF SCREEN
      LOGICAL LANSI,LFIRST,LCRT
      INTEGER IVUNIT
      COMMON/LANSI/LANSI,LFIRST,LCRT,IVUNIT
      IF(LANSI) THEN
      CALL CHRWT(IVUNIT,CHAR(27)//'[999D',6)
      CALL CHRWT(IVUNIT,CHAR(27)//'[99B',5)
C     ELSE IF ( LCRT ) THEN                                             H
C     CALL GIOPLW ( IVUNIT, '23, I,23, IST )                            H
      ELSE
      CALL CHRWT(IVUNIT, CHAR(13),1)
      DO 10 I=1,5
   10 CALL CHRWT(IVUNIT, CHAR(0), 1)
      CALL CHRWT(IVUNIT, CHAR(10),1)
      DO 20 I=1,5
   20 CALL CHRWT(IVUNIT, CHAR(0), 1)
      ENDIF
      RETURN
      END
