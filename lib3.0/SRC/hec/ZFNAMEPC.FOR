      SUBROUTINE ZFNAME (CIN, CNAME, NNAME, LEXIST)
C
C     Add machine dependent extensions to form DSS file name
C     Determine if the file exists in the current directory
C
C     Written by Bill Charley, HEC, May 1990
C
C
      CHARACTER CIN*(*), CNAME*(*)
      LOGICAL LEXIST
C
C
      CALL CHRFLB (CIN, IBEG, IEND)
      IF (IEND.LE.0) THEN
      CNAME = ' '
      LEXIST = .FALSE.
      GO TO 800
      ENDIF
C
      NNAME = IEND - IBEG + 1
C
C     Harris computers, no extension
      CNAME = CIN(IBEG:IEND)
C
C     MS-DOS computers, default extension of '.DSS'
      I = NINDX (CIN(IBEG:IEND), '\') + IBEG - 1                        ML
C     I = NINDX (CIN(IBEG:IEND), '/') + IBEG - 1                        u
      IF (I.LE.IBEG) I = IBEG                                           MLu
      IF (INDEX(CIN(I:IEND),'.').EQ.0) THEN                             MLu
      CNAME = CIN(IBEG:IEND)//'.DSS'                                    ML
C     CNAME = CIN(IBEG:IEND)//'.dss'                                    u
      NNAME = NNAME+4                                                   MLu
      ENDIF                                                             MLu
C
      CALL UPCASE (CNAME(1:NNAME))                                      HML
      INQUIRE (FILE=CNAME(1:NNAME), EXIST=LEXIST)
C
 800  CONTINUE
      RETURN
      END
