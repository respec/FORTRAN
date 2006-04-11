      SUBROUTINE ANREAD ( IUNIT, CPROMP, NPROMP, CLINE, NLINE)
C
C     PARAMETER (NBUFF=86)                                              H
C     PERFORM A PROMPTED READ
C
      CHARACTER CPROMP*(*), CLINE*(*)
C     CHARACTER CHRBUF*80, CJBUFF*132, CLINT*132                        H
C     CHARACTER*1 CLF,CESC,CCX,CBS*3,CEXCL*2, CDLINE*4, CCS             H
C     CHARACTER*1 CCR, CNU                                              H
C     CHARACTER  CONTNT*9, CTYPE*3, CREG*6                              H
      CHARACTER CNAM1*64, CNAM2*64                                      MLlg
C     CHARACTER CDLIM*1                                                 H
C
C     INTEGER IBUFF(NBUFF), JBUFF(44)                                   H
C     LOGICAL LFDUPX, LCRT, LASYNC, NOWAIT                              H
      LOGICAL LFIRST, LBATCH, LMOV, LPMT
      SAVE LFIRST, LBATCH
C     LOGICAL LMLD, LDLINE                                              H
C
C     EQUIVALENCE (JBUFF,CJBUFF)                                        H
C
C
      DATA LFIRST/.TRUE./
C
C     CNU = CHAR(0)                                                     H
C
C     SEE IF BATCH OR NOT
      IF (LFIRST) THEN                                                  MLlg
      LFIRST = .FALSE.                                                  MLlg
      CALL GETNAM (5, CNAM1, IERR)                                      MLlg
      CALL GETNAM (6, CNAM2, IERR)                                      ML
      IF (CNAM1.EQ.CNAM2) THEN                                          ML
C     IF (CNAM1(1:5).EQ.'stdin') THEN                                   lg
      LBATCH = .FALSE.                                                  MLlg
      ELSE                                                              MLlg
      LBATCH = .TRUE.                                                   MLlg
      ENDIF                                                             MLlg
      ENDIF                                                             MLlg
C
C
      NMAX = LEN(CLINE)
C
C     HERE FOR NON-HARRIS MACHINES
      IF ((NPROMP.GT.0).AND.(.NOT.LBATCH)) THEN                         PMLlg
      WRITE (6, 10) CPROMP(1:NPROMP)                                    PMLlg
      ENDIF                                                             PMLlg
C10   FORMAT (NN,A)                                                     P
C10   FORMAT (' ',A\)                                                   M
 10   FORMAT (' ',A)                                                    L
C10   FORMAT (' ',A,$)                                                  lg
      READ (5, 20,END=30) CLINE                                         PMLlg
 20   FORMAT (A)                                                        PMLlg
C
      CALL CHRLNB (CLINE(1:NMAX),NLINE)                                 PMLlg
C
      IF (LBATCH) THEN                                                  PMLlg
      N = NLINE                                                         PMLlg
      IF (NLINE.EQ.0) N = 1                                             PMLlg
      IF (NPROMP.GT.0) THEN                                             PMLlg
      WRITE (6, 25) CPROMP(1:NPROMP), CLINE(1:N)                        PMLlg
 25   FORMAT (' ',A,A)                                                  PMLlg
      ELSE                                                              PMLlg
      WRITE (6,25) CLINE(1:N)                                           PMLlg
      ENDIF                                                             PMLlg
      ENDIF                                                             PMLlg
C
      RETURN                                                            PMLlg
 30   CONTINUE                                                          PMLlg
      NLINE = -1                                                        PMLlg
      RETURN                                                            PMLlg
C
C
C     IF (LFIRST) THEN                                                  H
C     CBS=CHAR(8)//CHAR(32)//CHAR(8)                                    H
C     CCR=CHAR(13)                                                      H
C     CESC=CHAR(27)                                                     H
C     CLF = CHAR (10)                                                   H
C     CEXCL = CHAR (33) // CCR                                          H
C     CCX=CHAR(24)                                                      H
C     CCS = CHAR ( 19 )                                                 H
C     CDLINE = CHAR(21)                                                 H
C     LDLINE = .FALSE.                                                  H
C
C     GET REGISTER INFO
C     CALL TRMTYP(IUNIT,CTYPE)                                          H
C     IF(CTYPE.EQ.'CRT') THEN                                           H
C     LCRT=.TRUE.                                                       H
C     ELSE                                                              H
C     LCRT=.FALSE.                                                      H
C     ENDIF                                                             H
C
C     IF(CTYPE.EQ.'ASY') THEN                                           H
C     LASYNC=.TRUE.                                                     H
C     CALL STTY ( IUNIT, 'SET', 'ICRNL', 'ON', ISTAT)                   H
C     CDLINE = CNU//CNU//CNU//CNU                                       H
C     CALL STTY ( IUNIT, 'GET', 'DLINE', CDLINE, ISTAT)                 H
C     LDLINE = .FALSE.                                                  H
C     NDLINE = 0                                                        H
C     IF ( ICHAR(CDLINE(1:1)) .GT. 0 .AND. ISTAT .EQ. 0 ) THEN          H
C     LDLINE = .TRUE.                                                   H
C     NDLINE = INDEX ( CDLINE, CNU ) - 1                                H
C     IF ( NDLINE .LT. 0 ) NDLINE = 4                                   H
C     IF ( NDLINE .EQ. 0 ) LDLINE = .FALSE.                             H
C     ENDIF                                                             H
C     WRITE(3,*)LDLINE,ISTAT,NDLINE,CDLINE
C     ELSE                                                              H
C     LASYNC=.FALSE.                                                    H
C     ENDIF                                                             H
C
C     LFDUPX = .TRUE.                                                   H
C     CALL GSTRRG('SDX',CONTNT,NC,ISTAT)                                H
C     IF (ISTAT.EQ.0) THEN                                              H
C     IF(CONTNT(1:3).EQ.'HAL') LFDUPX = .FALSE.                         H
C     ENDIF                                                             H
C
C     CHECK FOR A 'BATCH' MODE
C     CALL LFN ( IUNIT, ITYPE)                                          H
C     IF (ITYPE.EQ.-1) THEN                                             H
C     LBATCH = .TRUE.                                                   H
C     ELSE                                                              H
C     LBATCH = .FALSE.                                                  H
C     ENDIF                                                             H
C
C     LFIRST=.FALSE.                                                    H
C     ENDIF                                                             H
C
C --- CHECK TO SEE IF #MLD REGISTER IS SET. IF SET, PLACE THE
C --- CHARACTER DELIMETER INTO CDLIM AND SET LOGICAL FLAG LMLD TRUE.
C
C     CALL GSTRRG ( 'MLD', CREG, NREG, ISTAT )                          H
C
C     IF ( ISTAT .EQ. 0 ) THEN                                          H
C        LMLD = .TRUE.                                                  H
C        CDLIM(1:1) = CREG(1:1)                                         H
C        WRITE (*,*) '#MLD REGISTER IS SET: CDLIM ===>', CDLIM(1:1)
C     ELSE                                                              H
C        LMLD = .FALSE.                                                 H
C        WRITE (*,*) '#MLD REGISTER IS NOT SET'
C     ENDIF                                                             H
C
C     Check for global time out
C
C     CALL GNUMRG ('STO', IDUM, ILIMIT, ISTAT)                          H
C     IF ( ISTAT.EQ.0 .AND. ILIMIT.GT.0 ) THEN                          H
C     CALL DATIME ( IYR, JDAY, ITENTH )                                 H
C     Assume that a timeout will never be more than 10 hours away
C     If it is must have crossed over midnight!
C     IF ( IABS( ILIMIT-ITENTH ) .GT. 360000 ) THEN                     H
C     IF ( ITENTH .LT. ILIMIT ) THEN                                    H
C     ITENTH = ITENTH + 864000                                          H
C     ELSE                                                              H
C     ILIMIT = ILIMIT + 864000                                          H
C     ENDIF                                                             H
C     ENDIF                                                             H
C     Check if past timeout time
C     IF ( ITENTH .GT. ILIMIT ) THEN                                    H
C     NLINE = -4                                                        H
C     RETURN                                                            H
C     ENDIF                                                             H
C     ENDIF                                                             H
C     Check for a register indicating a wait time in seconds
C     CALL GSTRRG ('SIA', CREG, NREG,  ISTAT)                           H
C     IF (ISTAT.EQ.0 ) THEN                                             H
C     READ(CREG, 27,IOSTAT=IST) NSECS                                   H
C27   FORMAT(BN,I6)                                                     H
C     IF ( NSECS .EQ. 0 .OR. IST .NE. 0 ) THEN                          H
C     NOWAIT = .TRUE.                                                   H
C     ELSE                                                              H
C     NOWAIT = .FALSE.                                                  H
C     MAXWAT = NSECS * 120                                              H
C     ENDIF                                                             H
C     ELSE                                                              H
C     NOWAIT = .TRUE.                                                   H
C     ENDIF                                                             H
C
C
C     IF (LBATCH) THEN                                                  H
C     READ ( IUNIT, 140, END=110) CLINE                                 H
C     NLINE=NINDXR(CLINE,' ')                                           H
C     RETURN                                                            H
C110  NLINE = -1                                                        H
C     CLINE = ' '                                                       H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C
C     IF (LCRT) THEN                                                    H
C     HERE FOR HARRIS CRT INPUT
C     IF ( NPROMP .GT. 0 ) THEN                                         H
C     WRITE(IUNIT,120)  CPROMP(1:NPROMP)                                H
C     NP = NPROMP                                                       H
C     ELSE                                                              H
C     NP = 0                                                            H
C     ENDIF                                                             H
C120  FORMAT(1X,A,A)                                                    H
C     CLINT = ' '                                                       H
C     READ ( IUNIT, 140, END=160) CLINE                                 H
C140  FORMAT (A)                                                        H
C 160 NLINE = NINDXR ( CLINE, ' ' )                                     H
C     MLINE = NLINE                                                     H
C     IF (NLINE.EQ.0) MLINE = 1                                         H
C     WRITE(3,*)' LINE READ =',CLINE,'=',NP,NLINE
C     IF(NP.EQ.0) THEN                                                  H
C     WRITE(IUNIT, 120)    CLINE(1:MLINE)                               H
C     ELSE                                                              H
C     WRITE(IUNIT, 120)CPROMP(1:NP),CLINE(1:MLINE)                      H
C     ENDIF                                                             H
C     RETURN                                                            H
C
C     ELSE                                                              H
C
C
C     TURN ON HOT READ FOR THIS LINE
C     CALL CHRIT1(IUNIT,IBUFF,NBUFF)                                    H
C
C180  CONTINUE                                                          H
C     CLINE(1:NMAX) = ' '                                               H
C     ISSUE PROMPT
C     NP = NPROMP                                                       H
C     LMOV = .TRUE.                                                     H
C     LPMT = .TRUE.                                                     H
C     IF ( NP .LT. 0 ) LMOV = .FALSE.                                   H
C     IF ( NP .EQ. -99 .OR. NP .EQ. 0 ) LPMT = .FALSE.                  H
C     IF ( LMOV ) CALL CHRWT1(CCR//CLF,2)                               H
C     NP = IABS ( NP )                                                  H
C     IF ( LPMT ) CALL CHRWT1(CPROMP,NP)                                H
C
C
C     READ IN RESPONSE
C
C     J=0                                                               H
C200  CONTINUE                                                          H
C
C     IF (NOWAIT) THEN                                                  H
C     CALL CHRRD1(CHRBUF,NCHS)                                          H
C     ELSE                                                              H
C
C     Check for a user timing out
C     INACT = 0                                                         H
C     IWAIT = 2                                                         H
C     NWAIT = 0                                                         H
C
C205  CONTINUE                                                          H
C     Read without waiting for a character
C     CALL CHRRI1 (CHRBUF, NCHS)                                        H
C     If a character was received, jump out of the loop
C     IF (NCHS.GT.0) GO TO 210                                          H
C
C     Have we exceeded the maximum amount of time to wait?
C     IF (NWAIT.GE.MAXWAT) THEN                                         H
C     NLINE = -3                                                        H
C     CALL CHRFN1                                                       H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     Wait for the specified amount of time, then bump the counter
C     CALL WAIT (IWAIT, 0, K)                                           H
C     INACT = INACT + 1                                                 H
C     NWAIT = NWAIT + IWAIT                                             H
C     IF (INACT.EQ.4) THEN                                              H
C     IWAIT = 4                                                         H
C     ELSE IF (INACT.EQ.30) THEN                                        H
C     IWAIT = 10                                                        H
C     ELSE IF (INACT.EQ.80) THEN                                        H
C     IWAIT = 20                                                        H
C     ELSE IF (INACT.EQ.160) THEN                                       H
C     IWAIT = 30                                                        H
C     ENDIF                                                             H
C     GO TO 205                                                         H
C     ENDIF                                                             H
C
C
C210  CONTINUE                                                          H
C     DO 220 K=1,NCHS                                                   H
C     KZ = K                                                            H
C
C     STRIP ALL BUT 7 BITS WORTH
C     I=ICHAR(CHRBUF(K:K)) .AND. '177                                   H
C     CHRBUF(K:K)= CHAR(I)                                              H
C
C     CHECK FOR CONTROL CHAR, BRANCH ON ALPHA-NUMERIC
C
C     IF ((CHRBUF(K:K).EQ.CCR).OR.(CHRBUF(K:K).EQ.CLF)) THEN            H
C     CHECK FOR CARRIAGE RETURN
C     IF(LFDUPX) THEN                                                   H
C     CALL CHRWT1(CCR,1)                                                H
C     ENDIF                                                             H
C     TURN OFF AND RETURN
C     CALL CHRFN1                                                       H
C
C
C --- IF MULTIPLE LINE DELIMETERS ARE USED, CHECK INPUT STRING FOR
C --- DELIMETER CHARACTER. IF FOUND, RESET POINTER TO FIRST OCCURRANCE
C --- OF DELIMETER AND BACKSTORE REST OF CHARACTER STRING.
C
C     IF ( LMLD ) THEN                                                  H
C
C
C       DO 1002 KK= K, NCHS                                             H
C       IF ( CHRBUF(KK:KK) .EQ. CCR ) CHRBUF(KK:KK) = CDLIM(1:1)        H
C       IF ( CHRBUF(KK:KK) .EQ. CLF ) CHRBUF(KK:KK) = CDLIM(1:1)        H
C       J = J + 1                                                       H
C1002   CLINE(J:J) = CHRBUF(KK:KK)                                      H
C
C
C
C      JP = J - 1                                                       H
C      KP = J                                                           H
C        DO 1001 I=J,1,-1                                               H
C           IF ( CLINE(I:I) .EQ. CDLIM(1:1) ) THEN                      H
C              CLINE (I:I) = CCR                                        H
C              JP=I-1                                                   H
C              KP=I                                                     H
C           ENDIF                                                       H
C1001    CONTINUE                                                       H
C
C
C
C       IF ( J .NE. KP ) THEN                                           H
C        CHRBUF(1:) = CLINE(KP+1:J)                                     H
C       ENDIF                                                           H
C
C       NCHS = J - KP                                                   H
C       J = JP                                                          H
C       KZ = 0                                                          H
C       CLINE(KP:) = ' '                                                H
C
C
C     ENDIF                                                             H
C
C     IF AN ASYNC PORT, AND MORE CHARACTERS THERE, BACKSTORE THEM
C     IF ((KZ.LT.NCHS).AND.(LASYNC)) THEN                               H
C     N = NCHS - KZ                                                     H
C     CJBUFF(1:N) = CHRBUF(KZ+1:NCHS)                                   H
C     CALL GIOPLW ( 0, '27, JBUFF, N, ISTAT)                            H
C     ENDIF                                                             H
C
C     NLINE=MIN0(NMAX,J)                                                H
C     IF ( NLINE .LT. 0 ) NLINE = 0                                     H
C     RETURN                                                            H
C
C     ELSE IF(CHRBUF(K:K).EQ.CBS(1:1)) THEN                             H
C
C     BACKSPACE
C     IF BACKSPACE INTO PROMPT, ISSUE ESCAPE
C     IF (J.LE.0) THEN                                                  H
C     CALL CHRWT1(CEXCL,2)                                              H
C     GO TO 180                                                         H
C     ENDIF                                                             H
C
C     CALL CHRWT1(CBS,3)                                                H
C
C     CLINE(J:J) = ' '                                                  H
C     J=J-1                                                             H
C
C     ELSEIF ( LDLINE .AND.  (                                          H
C    *                                (CHRBUF(K:K).EQ.CDLINE(1:1)).OR.  H
C    *  (CLINE(J:J).EQ.CDLINE(1:1).AND.CHRBUF(K:K).EQ.CDLINE(2:2)).OR.  H
C    *(CLINE(J-1:J).EQ.CDLINE(1:2).AND.CHRBUF(K:K).EQ.CDLINE(3:3)).OR.  H
C    *(CLINE(J-2:J).EQ.CDLINE(1:3).AND.CHRBUF(K:K).EQ.CDLINE(4:4))))THENH
C     DELETE LINE SEQUENCE - CHECK IF COMPLETE
C     IF ( CHRBUF(K:K).EQ.CDLINE(NDLINE:NDLINE) ) THEN                  H
C     CALL CHRWT1(CEXCL,2)                                              H
C     GO TO 180                                                         H
C     ENDIF                                                             H
C
C     J = J + 1                                                         H
C     IF (J.GT.NMAX) J=NMAX                                             H
C     CLINE(J:J) = CHRBUF(K:K)                                          H
C
C
C     ELSE IF(CHRBUF(K:K).EQ.CCX) THEN                                  H
C     SPECIAL INTERRUPT CONTROL X
C
C ----- If control-S don't echo it, because the terminal will
C ----- likely be placed in a hold state !!!!!
C     ELSE IF ( CHRBUF(K:K) .EQ. CCS ) THEN                             H
C ----- do nothing
C
C     ELSE                                                              H
C     ECHO FOR FULL DUPLEX
C
C     IF(LFDUPX) THEN                                                   H
C     CALL CHRWT1(CHRBUF(K:K),1)                                        H
C     ENDIF                                                             H
C
C     J = J + 1                                                         H
C     IF (J.GT.NMAX) J=NMAX                                             H
C     CLINE(J:J) = CHRBUF(K:K)                                          H
C
C     ENDIF                                                             H
C
C220  CONTINUE                                                          H
C
C     GO TO 200                                                         H
C
C     ENDIF                                                             H
C
      END
