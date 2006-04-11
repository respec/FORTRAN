      SUBROUTINE RBELL
C
C     Rings the Bell at the Terminal
C
C     LOGICAL LFIRST                                                    H
C     DATA LFIRST /.TRUE./                                              H
C
C     IF (LFIRST) THEN                                                  H
C     CALL LFN ( 3, IPDN)                                               H
C     LFIRST = .FALSE.                                                  H
C     ENDIF                                                             H
C
C     IF (IPDN.GT.0) CALL CHRWT (3, CHAR(07), 1)                        H
      CALL CHRWT1 (CHAR(07),1)                                          MLlg
C
      RETURN
      END
