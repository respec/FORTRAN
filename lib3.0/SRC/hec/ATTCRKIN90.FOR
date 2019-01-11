      SUBROUTINE ATTCRK (CLEFT,CRIGHT,JKS,KS,ISTAT)
C
C-----------------------------------------------------------------------
C  GETS EXECUTE LINE.  IF THERE'S FILE SUBSTITUTIONS, CRACK
C  LINE AND GET NAMES ON BOTH SIDES OF THE EQUAL SIGN.  PASS
C  BACK TO THE CALLING ROUTINE AS CLEFT AND CRIGHT.
C-----------------------------------------------------------------------
C
      CHARACTER *(*) CLEFT, CRIGHT
      DIMENSION CLEFT(JKS), CRIGHT(JKS)
      CHARACTER CXLINE*133, CXLIN2*133, CDLM*3
      DIMENSION IBF(30),ILF(30),IDT(30),IDP(30),ITBL(128)
C
      DATA CDLM /' ''"'/
C
      MXLNE = 132
C
      DO 100 K=1,JKS
        CLEFT(K)  = ' '
        CRIGHT(K) = ' '
 100  CONTINUE
C
C-----------------------------------------------------------------------
C  GET EXECUTE COMMAND LINE AND ENTER IT IN CXLINE
C-----------------------------------------------------------------------
C
      CXLINE = ' '
C     CALL CPARMS (CXLINE,MXLNE)                                        Mlg
C     CALL XQTLNE (CXLINE,MXLNE)                                        H
      CALL GETARG (1,CXLINE)                                            L
      CALL CHRLNB (CXLINE,MXLINE)                                       L
C
C-----------------------------------------------------------------------
C  REPLACE COMMAS WITH BLANKS
C-----------------------------------------------------------------------
C
 105  ITP = INDEX(CXLINE,',')
C
      IF (ITP .NE. 0) THEN
        CXLINE(ITP:ITP) = ' '
        GO TO 105
      ENDIF
C
C-----------------------------------------------------------------------
C SET JST = 2 WHEN THE EXECUTE NAME IS IN CXLINE;
C     JST = 1 WHEN ONLY THE PARAMETERS ARE IN CXLINE.
C-----------------------------------------------------------------------
C
      JST = 1                                                           MLlg
C     JST = 2                                                           H
C
      CALL FINDLM (CXLINE,1,MXLNE,NF,IBF,ILF,IDT,IDP,ITBL)
C
C-----------------------------------------------------------------------
C  NO SUBSTITUTIONS
C-----------------------------------------------------------------------
C
      IF (NF .LE. (JST-1)) THEN
        ISTAT= -2
        GO TO 999
C
C-----------------------------------------------------------------------
C  LOOK FOR A QUESTION MARK
C-----------------------------------------------------------------------
C
      ELSE
        IST = IBF(JST)
        IEN = IST + ILF(JST) -1
        IF (CXLINE(IST:IST) .EQ. '?') THEN
          ISTAT = -10
          IF (ILF(JST)        .EQ. 2 .AND.
     +        CXLINE(IEN:IEN) .EQ. '?') ISTAT = -11
          GO TO 999
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C  CHANGE LOWER CASE TO UPPER CASE
C-----------------------------------------------------------------------
C
C---- CALL UPCASE (CXLINE)  See below where only left side is U.C.
C
C-----------------------------------------------------------------------
C  START CRACKING THE COMMAND LINE
C-----------------------------------------------------------------------
C
      IST    = IBF(JST)
      CXLIN2 = CXLINE(IST:)
C
C-----------------------------------------------------------------------
C  LOOK FOR FILE SUBSTITUTION
C-----------------------------------------------------------------------
C
      KST   = 1
      KS    = 0
      MXLNE = MXLNE + 1
C
      CXLIN2(MXLNE:MXLNE) = ' '
C
C-----------------------------------------------------------------------
C  COUNT NUMBER OF SUBSTITUTIONS (EQUAL SIGNS)
C-----------------------------------------------------------------------
C
 110  JEQ = INDEX(CXLIN2(KST:MXLNE),'=')
      IF (JEQ .EQ. 0) GO TO 120
C
      KS = KS + 1
      IF (KS .GT. JKS) STOP 'TO MANY FILE SUBSTITUTIONS ON COMMAND LINE'
C
      KST = KST + JEQ
C
      GO TO 110
C
 120  IF (KS .EQ. 0) THEN
        ISTAT = 0
      ELSE
        ISTAT = 99
        IST   = 1
C
        DO 140 K=1,KS
C
C-----------------------------------------------------------------------
C  LOOK FOR AN EQUAL SIGN
C-----------------------------------------------------------------------
C
          JEQ             = INDEX(CXLIN2(1:MXLNE),'=')
          CXLIN2(JEQ:JEQ) = ' '
          JLN             = NINDXR(CXLIN2(1:JEQ-1),' ')
          JST             = INDEXR(CXLIN2(1:JLN),' ') +1
          CLEFT(K)        = CXLIN2(JST:JLN)
          CALL UPCASE ( CLEFT(K) )
C
C-----------------------------------------------------------------------
C  NOW WORK ON THE RIGHT SIDE OF THE EQUAL SIGN
C-----------------------------------------------------------------------
C
          IST = JEQ +1
          J   = NINDX(CXLIN2(IST:MXLNE),' ')
          IST = IST + J -1
          J   = IST
C
C-----------------------------------------------------------------------
C  LOOK FOR QUOTES (SINGLE OR DOUBLE)
C-----------------------------------------------------------------------
C
          M2 = 1
C
          IF (CXLIN2(IST:IST).EQ.'''') THEN
            J  = J + 1
            M2 = 2
          ELSE IF (CXLIN2(IST:IST).EQ.'"') THEN
            J  = J + 1
            M2 = 3
          ENDIF
C
          JEN = INDEX(CXLIN2(J:MXLNE),CDLM(M2:M2))
          IF (M2 .EQ. 1) JEN = JEN - 1
C
          JLN = J + JEN - 1
          IF (M2 .LE. 1) GO TO 130
C
          J         = J - 1
          JLN       = JLN + 2
 130      CRIGHT(K) = CXLIN2(J:JLN)
          IST       = JLN + 2
C
 140    CONTINUE
      ENDIF
C
 999  RETURN
      END
