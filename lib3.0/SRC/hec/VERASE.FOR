      SUBROUTINE VERASE
C
C     ERASE SCREEN FOR ANSI TERMINAL  -  HOME CURSOR
C
C
      LOGICAL LSPKAP
C
      LOGICAL LANSI,LFIRST,LCRT
      INTEGER IVUNIT
      COMMON/LANSI/LANSI,LFIRST,LCRT,IVUNIT
C
      COMMON/SPKALF/LSPKAP
C
C
C     CALL VANSI ( 0 )                                                  H
C
      IF(LSPKAP)THEN
         CALL CHRWT(IVUNIT, CHAR(10), 1)
         RETURN
      ENDIF
C
C
C     IF(LANSI) THEN                                                    H
      CALL CHRWT(IVUNIT,CHAR(27)//'[2J',4)
      CALL CHRWT(IVUNIT,CHAR(27)//'[01;01H',8)
C     CALL CHRWT(IVUNIT,CHAR(27)//'[01;01H',8)
C     ELSE IF ( LCRT ) THEN                                             H
C     CALL GIOPLW ( IVUNIT, '22, I, I, IST )                            H
C     ELSE                                                              H
C     CALL CHRWT(IVUNIT, CHAR(13),1)                                    H
C     DO 10 I=1,1                                                       H
C  10 CALL CHRWT(IVUNIT, CHAR(0), 1)                                    H
C     CALL CHRWT(IVUNIT, CHAR(10),1)                                    H
C     DO 20 I=1,1                                                       H
C  20 CALL CHRWT(IVUNIT, CHAR(0), 1)                                    H
C     CALL CHRWT(IVUNIT, ' - - - - - - - - - - - - - - - - - - - -',-1) H
C     CALL CHRWT(IVUNIT, ' - - - - - - - - - - - - - - - - - - -',-1)   H
C     CALL CHRWT(IVUNIT, CHAR(13),1)                                    H
C     DO 30 I=1,1                                                       H
C  30 CALL CHRWT(IVUNIT, CHAR(0), 1)                                    H
C     CALL CHRWT(IVUNIT, CHAR(10),1)                                    H
C     DO 40 I=1,1                                                       H
C  40 CALL CHRWT(IVUNIT, CHAR(0), 1)                                    H
C     ENDIF                                                             H
      RETURN
      END
