      SUBROUTINE VMOVCR (IRP, ICP)
C
C     MOVE CURSOR TO SPECIFIED SCREEN COORDINATES FOR ANSI TERMINAL
      LOGICAL LANSI,LFIRST,LCRT, LABS
      INTEGER IVUNIT
      COMMON/LANSI/LANSI,LFIRST,LCRT,IVUNIT
      CHARACTER C*8, CR(2)*1, CC(2)*1
C ------
C ------ If IRP and/or ICP are biased by 1000 this is a relative
C ------ movement upward or left of current position.
C ------ If biased by 2000 relative movement down or right.
C ------
C ------ Both should be biased or both un-biased.
C ------ If not, the unbiased value will override and the
C ------ relative value will be set to 1, and an absolute move done.
C ------
      DATA CR / 'A', 'B' /
      DATA CC / 'D', 'C' /
      IR = MOD ( IRP, 1000 )
      IC = MOD ( ICP, 1000 )
      JR = IRP / 1000
      JC = ICP / 1000
      IF ( JR + JC .EQ. 0 ) THEN
      LABS = .TRUE.
      ELSE IF ( JR .EQ. 0 .AND. JC .NE. 0 ) THEN
      LABS = .TRUE.
      IC = 1
      ELSE IF ( JR .NE. 0 .AND .JC. EQ. 0 ) THEN
      LABS = .TRUE.
      IR = 1
      ELSE
      LABS = .FALSE.
      ENDIF
      IF(LANSI) THEN
      IF ( LABS ) THEN
      WRITE(C,5) CHAR(27),IR,IC
    5 FORMAT(A1,'[',I2.2,';',I2.2,'H')
      CALL CHRWT(IVUNIT,C,8)
      ELSE
      WRITE ( C, 6 ) CHAR(27), IR, CR(JR)
    6 FORMAT ( A1,'[',I2.2,A1 )
      IF ( IR .GT. 0 ) CALL CHRWT ( IVUNIT, C, 5 )
      WRITE ( C, 6 ) CHAR(27), IC, CC(JC)
      IF ( IC .GT. 0 ) CALL CHRWT ( IVUNIT, C, 5 )
      ENDIF
C     ELSE IF ( LCRT ) THEN                                             H
C     I = IR * 256 + IC                                                 H
C     CALL GIOPLW ( IVUNIT, '10, I, 1, IST )                            H
      ELSE
      CALL CHRWT(IVUNIT, CHAR(13),1)
      DO 10 I=1,1
   10 CALL CHRWT(IVUNIT, CHAR(0), 1)
      CALL CHRWT(IVUNIT, CHAR(10),1)
      DO 20 I=1,1
   20 CALL CHRWT(IVUNIT, CHAR(0), 1)
      ENDIF
      RETURN
      END
