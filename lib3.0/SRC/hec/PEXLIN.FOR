      SUBROUTINE PEXLIN
C
C        SUBROUTINE 'PEXLIN' EXPANDS A GIVEN LINE TO INCLUDE
C        CHARACTERS SIGNIFIED BY A FUNCTION KEY, AND ALSO
C        BY INHERENT FUNCTIONS.
C
C
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
C
C
      CHARACTER*1 CH
      LOGICAL LLOCAL
C
C
C   ***
C   ***
C   ** CHECK FOR ANY FUNCTION IN LINE
C   ***
      M = INDEX(CLINE,CSPL(4:4))
      LCLEAN=.TRUE.
      IF (INCHR .EQ. 0) RETURN
      IF (M.LE.0 .AND. .NOT.LFUN) RETURN
      LLOCAL=.FALSE.
      CTEMP = ' '
C ------ FORCE MAX LENGTH OF EXPANDED LINE TO 132
      MAXLEN = 132
C   ***
C   ***
C   ** FIND BEG OF LINE
C   ***
      IBEG=NINDX(CLINE(1:),CSPL(2:2))
      IF (IBEG.EQ.0) GO TO 1000
      IF (IBEG.GE.MAXLEN) GO TO 1000
C ------ Make this keep leading blanks on line for screens use
      IBEG = 1
C   ***
C   ***
C   ** FIND END OF LINE
C ** NOTE:  ANREAD RETURNS 'INCHR' AS EITHER THE LENGTH OF THE VARIABLE
C **  READ IN (CLINE IN THIS CASE), OR AS THE ORIGINAL MAXIMUM LENGTH
C **  OF THE VARIABLE.  AS OF 3-7-86 THE POINT HAS NOT BEEN DETERMINED.
C **  IN EITHER CASE, 'IEND' WILL BE SET TO THE LAST CHARACTER IN CLINE.
C   ***
      IEND=NINDXR(CLINE(1:),CSPL(2:2))
C   ***
C   ***
C   ** FIND DEFINED FUNCTION CHAR
C   ***
      IOUTP=1
      DO 500 I=IBEG,IEND
C   ***
C   ***
C   ** GET A CHAR
C   ***
        CH = CLINE(I:I)
C   ***
C   ***
C   ** CHECK FOR CSPL(4:4) - SPECIAL ESCAPE CHAR
C   ***
        IF(LLOCAL) GO TO 90
        IF (CH .EQ. CSPL(4:4)) LLOCAL=.TRUE.
        IF (LLOCAL) GO TO 500
        IF (.NOT.(LLOCAL.OR.LFUN)) GO TO 105
C   ***
C   ***
C   ** CHECK FOR DEFINED CHAR
C   ***
   90   DO 100 J=1,NKEY
          IF (CH .EQ. CKEY(J)) GO TO 110
  100   CONTINUE
C   ***
C   ***
C   ** NO MATCH PASS CHAR
C   ***
  105   CTEMP(IOUTP:IOUTP) = CH
        LLOCAL=.FALSE.
        IOUTP=IOUTP+1
        IF (IOUTP.GT.MAXLEN) GO TO 1000
        GO TO 500
C   ***
C   ***
C   ** MATCH FOUND, SUBSTITUTE REFERENCE
C   ***
  110   CONTINUE
        LCLEAN=.FALSE.
        N=IKEY(J)
        LL=MAXLEN-IOUTP+1
        IF (N.GT.LL) N=LL
        IF ( N .GT. 0 ) THEN
          CTEMP(IOUTP:) = CFUNCT(J)(1:N)
        ENDIF
        IOUTP=IOUTP+IKEY(J)
        IF (IOUTP.GT.MAXLEN) GO TO 1000
        LLOCAL=.FALSE.
  500 CONTINUE
C   ***
C   ***
C   ** MOVE CTEMP BACK TO LINE
C   ***
 1000 CONTINUE
      IF (IOUTP .GT. MAXLEN) THEN
         LTOBIG = .TRUE.
         CLINE(1:) = CTEMP(1:MAXLEN)
      ELSE
         CLINE(1:) = CTEMP(1:IOUTP)
      ENDIF
      RETURN
      END
