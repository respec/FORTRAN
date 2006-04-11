      SUBROUTINE CHRIO1
C     PERFORM CHARACTER IO.
C     THIS ROUTINE USES HARRIS SPECIFIC CABILITIES AT THIS TIME
C     IT CAN BE MODIFIED TO BE USED ON OTHER MACHINES IF THE
C     INTERNALS OF THE MACHINE ARE KNOWN.
C
C     FORTRAN WRITES MAY BE USED WHEN CHARACTER I/O IS
C     IN USE.
C
C********************************************************************
C NOTE: FORTRAN READ MAY NOT, NOT, BE DONE WHEN IN USE !!!!
C********************************************************************
C
C
C     PARAMETER (IBLEN=256)                                             Mlg
C
C     INTEGER*2 IARRAY(9)                                               L
      INTEGER*4 IARRAY(9)                                               L3
      CHARACTER*(*) CSTR, CSTR1,CSTR2,CSTR3,CSTR4,CSTR5                 MlLg
C     CHARACTER*(*) CTAG, CACT                                          lg
      CHARACTER CSTRM*127                                               MlLg
C     CHARACTER CBUFF*(IBLEN)                                           Mlg
C     INTEGER              JBUFF(86)                                    H
C     JBUFF(86) X 3 = 258 CHARACTERS.
      LOGICAL LINIT1, L8BIT
C     DATA LINIT1 /.FALSE./
      DATA L8BIT / .FALSE. /
C
C     DATA IBPOS,IBNUM  /0,0/                                           Mlg
C
      ENTRY CHRIT1(ILFN,IBUFF,NBUFF)
C     INTEGER IBUFF(NBUFF)                                              H
C     INITIALIZE CHARACTER CHANNEL #1
C
      LINIT1 = .TRUE.
C     JLFN=ILFN                                                         H
C     CALL GIOPLW(JLFN,'51,IBUFF,NBUFF,JSTAT)                           H
      GO TO 1000
C
C
      ENTRY CHRSE1 ( CTAG, CACT, IACT )
C     CHARACTER*(*) CTAG, CACT                                          HL
      IF ( CTAG .EQ. '8BIT' ) THEN
      IF ( CACT .EQ. 'ON' ) THEN
      L8BIT = .TRUE.
      ELSE
      L8BIT = .FALSE.
      ENDIF
      IACT = 0
      ELSE
      IACT = -1
      ENDIF
      GO TO 1000
C
C
      ENTRY CHRFN1
C     FINISH CHARACTER CHANNEL #1
C
      LINIT1 = .FALSE.
C     CALL GIOPSW(JLFN,'24,JSTAT)                                       H
      GO TO 1000
C
C
      ENTRY CHRFL1
C     FLUSH CHARACTER CHANNEL #1 TYPE AHEAD BUFFER
C     CALL GIOPSW(JLFN,'54,JSTAT)                                       H
C     CALL STDINC ('N','N','N','Y',J,K)                                 Mlg
      GO TO 1000
C
C
C
      ENTRY CHRFL ( JLFNX )
C     FLUSH CHARACTER  TYPE AHEAD BUFFER
C     CALL GIOPSW(JLFNX,'54,JSTAT)                                      H
C     CALL STDINC ('N','N','N','Y',J,K)                                 Mlg
      GO TO 1000
C
C
      ENTRY CHRBK1(CSTR,NSTR)
C     CHARACTER*(*) CSTR                                                H
C     BACKSTORE ON CHANNEL #1
C     CALL CHRHOL(CSTR,1,NSTR,JBUFF,1)                                  H
C     CALL GIOPLW(JLFN,'27,JBUFF,NSTR,JSTAT)                            H
C     DO 110 I=NSTR,1,-1                                                Mlg
C     CBUFF(IBPOS+1:IBPOS+1) = CSTR(I:I)                                Mlg
C     IBPOS = MOD(IBPOS+1,IBLEN)                                        Mlg
C     IBNUM = IBNUM+1                                                   Mlg
C110  CONTINUE                                                          Mlg
      GO TO 1000
C
C
C
      ENTRY CHRST1(ISTAT1,KSTAT1)
C     PERFORM A STATUS (WAIT UNTIL COMPLETE) OF MOST RECIENT OPERATION
C     CALL GIOPSW (JLFN,'00,JSTAT)                                      H
C     KSTAT1 = JSTAT                                                    H
C     K = JSTAT.AND.'00400000                                           H
C     IF (K.EQ.0) THEN                                                  H
      ISTAT1 = 0
C     ELSE                                                              H
C     ISTAT1 = 1                                                        H
C     ENDIF                                                             H
      GO TO 1000
C
C
C
      ENTRY CHRSI1(ISTAT,KSTAT)
C     PERFORM A STATUS (WITHOUT WAIT) OF MOST RECIENT OPERATION
C     CALL GIOPSW (JLFN,'77,JSTAT)                                      H
C     KSTAT = JSTAT                                                     H
C     J = JSTAT.AND.'40000000                                           H
C     K = JSTAT.AND.'00400000                                           H
C     IF (J.NE.0) THEN                                                  H
C     ISTAT = -1                                                        H
C     ELSE                                                              H
C     IF (K.EQ.0) THEN                                                  H
      ISTAT = 0
C     ELSE                                                              H
C     ISTAT = 1                                                         H
C     ENDIF                                                             H
C     ENDIF                                                             H
      GO TO 1000
C
C
      ENTRY CHRWT1(CSTR1,NSTR1)
C     CHARACTER*(*) CSTR1                                               H
C     WRITE CHARACTERS TO CHANNEL #1
C     IF (.NOT.LINIT1) GOTO 2000                                        H
C     IF(NSTR1.EQ.0) GO TO 1000                                         H
      MSTR1 = NSTR1
      IF(NSTR1.LT.0) MSTR1 = LEN ( CSTR1 )
C     CALL CHRHOL(CSTR1,1,MSTR1,JBUFF,1)                                H
C     CALL GIOPLW(JLFN,'52,JBUFF,MSTR1,JSTAT)                           H
C     GO TO 1000                                                        H
      CSTRM = CSTR1                                                     MlLg
      NSTRM = MSTR1                                                     MlLg
      GOTO 120                                                          MlLg
C
C
C
      ENTRY CHRWT (KLFN,CSTR2,NSTR2)
C     CHARACTER*(*) CSTR2                                               H
C     WRITE CHARACTERS TO UNIT KLFN
C     IF(NSTR2.EQ.0) GO TO 1000                                         H
      MSTR2 = NSTR2
      IF(NSTR2.LT.0) MSTR2 = LEN ( CSTR2 )
C     CALL CHRHOL(CSTR2,1,MSTR2,JBUFF,1)                                H
C     CALL GIOPLW(KLFN,'52,JBUFF,MSTR2,JSTAT)                           H
C     GO TO 1000                                                        H
      CSTRM = CSTR2                                                     MlLg
      NSTRM = MSTR2                                                     MlLg
      GOTO 120                                                          MlLg
C
C
      ENTRY CHRWI1(CSTR3,NSTR3)
C     CHARACTER*(*) CSTR3                                               H
C     DO AN IMMEDIATE WRITE WITHOUT WAITING FOR COMPLETION
C     THIS MUST BE FOLLOWED BY A STATUS CALL BEFORE OTHER I/O
C
C     WRITE CHARACTERS TO CHANNEL #1
C     IF (.NOT.LINIT1) GOTO 2000                                        H
C     IF(NSTR3.LT.1) GO TO 1000                                         H
C     CALL CHRHOL(CSTR3,1,NSTR3,JBUFF,1)                                H
C     CALL GIOPL (JLFN,'52,JBUFF,NSTR3,JSTAT)                           H
      CSTRM = CSTR3                                                     MlLg
      NSTRM = NSTR3                                                     MlLg
 120  IF(NSTRM.LT.1) GO TO 1000                                         MlLg
      DO 150 I=1,NSTRM                                                  ML
C     CALL STDOUT ('Y',ICHAR(CSTRM(I:I)))                               M
      IARRAY(1) = 512                                                   L
      IARRAY(4) = ICHAR(CSTRM(I:I))                                     L
      CALL INTRUP (IARRAY,33)                                           L
 150  CONTINUE                                                          ML
C     WRITE (6, 151) CSTRM(1:NSTRM)                                     lg
C151  FORMAT (A,$)                                                      lg
      GO TO 1000
C
C
      ENTRY CHRRD1(CSTR4,NSTR4)
C     CHARACTER*(*) CSTR4                                               H
C     IF (.NOT.LINIT1) GOTO 2000                                        H
C     READ CHARACTERS FROM CHANNEL #1, WAIT FOR AT LEAST ONE CHARACTER
C     CALL GIOPSW(JLFN,'51,JSTAT)                                       H
C     NSTR4 = JSTAT .AND. '177777                                       H
C     IF (NSTR4 .GT. LEN (CSTR4)) NSTR4 = LEN(CSTR4)                    H
C     IF (NSTR4 .GT. 0) THEN                                            H
C     IF ( L8BIT ) THEN                                                 H
C     CALL HOLCH8(IBUFF,CSTR4,NSTR4)                                    H
C     ELSE                                                              H
C     CALL HOLCH7(IBUFF,CSTR4,NSTR4)                                    H
C     ENDIF                                                             H
C     ENDIF                                                             H
C     IF (IBNUM.NE.0) THEN                                              Mlg
C     DO 202 I=1,IBNUM                                                  Mlg
C     CSTR4(I:I) = CBUFF(MOD(IBPOS+IBLEN,IBLEN):                        Mlg
C    +   MOD(IBPOS+IBLEN,IBLEN))                                        Mlg
C     IBPOS = MOD(IBPOS-1+IBLEN,IBLEN)                                  Mlg
C     NSTR4 = NSTR4+1                                                   Mlg
C202  CONTINUE                                                          Mlg
C     IBNUM = 0                                                         Mlg
C     CALL STDINC ('N','N','N','N',JCHAR,K)                             Mlg
C     GOTO 220                                                          Mlg
C     ELSE                                                              Mlg
C     NSTR4 = 0                                                         Mlg
C     ENDIF                                                             Mlg
C210  CALL STDINC ('Y','N','Y','N',JCHAR,K)                             Mlg
C
C         Wait for at least one character
C
C220  IF (JCHAR .GT. 0) THEN                                            Mlg
C
C         Add character to string
C
C        NSTR4 = NSTR4 + 1                                              Mlg
C        CSTR4 (NSTR4:NSTR4) = CHAR(JCHAR)                              Mlg
C        CALL STDINC ('N','N','N','N',JCHAR,K)                          Mlg
C        GOTO 220                                                       Mlg
C     ENDIF                                                             Mlg
      GOTO 1000
C
C
C
      ENTRY CHRRI1(CSTR5,NSTR5)
C     CHARACTER*(*) CSTR5                                               H
C     IF (.NOT.LINIT1) GOTO 2000                                        H
C
C        Get characters from keyboard, do not wait.
C           NO-WAIT, ECHO, CHECK, NO-FLUSH
C
C     CALL GIOPSW(JLFN,'53,JSTAT)                                       H
C100  NSTR5=JSTAT .AND. '177777                                         H
C     IF(NSTR5 .GT. LEN(CSTR5)) NSTR5=LEN(CSTR5)                        H
C     IF (NSTR5 .GT. 0) THEN                                            H
C     IF ( L8BIT ) THEN                                                 H
C     CALL HOLCH8(IBUFF,CSTR5,NSTR5)                                    H
C     ELSE                                                              H
C     CALL HOLCH7(IBUFF,CSTR5,NSTR5)                                    H
C     ENDIF                                                             H
C     ENDIF                                                             H
C
C     NSTR5 = 0                                                         Mlg
C     IF (IBNUM.NE.0) THEN                                              Mlg
C     DO 302 I=1,IBNUM                                                  Mlg
C     CSTR5(I:I) = CBUFF(MOD(IBPOS+IBLEN,IBLEN):                        Mlg
C    +   MOD(IBPOS+IBLEN-1,IBLEN))                                      Mlg
C     IBPOS = MOD(IBPOS+IBLEN,IBLEN)                                    Mlg
C     NSTR5 = NSTR5+1                                                   Mlg
C302  CONTINUE                                                          Mlg
      IBNUM = 0
C     ELSE                                                              Mlg
C     NSTR4 = 0                                                         Mlg
C     ENDIF                                                             Mlg
C310  CALL STDINC('N','N','N','N',JCHAR,K)                              Mlg
C
C320  IF (JCHAR .GT. 0) THEN                                            Mlg
C
C         Add character to string
C
C        NSTR5 = NSTR5 + 1                                              Mlg
C        CSTR (NSTR5:NSTR5) = CHAR(JCHAR)                               Mlg
C        CALL STDOUT ('Y',JCHAR)                                        Mlg
C        GOTO 310                                                       Mlg
C     ENDIF                                                             Mlg
C
C
1000  CONTINUE
      RETURN
C
 2000 CONTINUE
C     WRITE (3,2010)                                                    H
C2010 FORMAT (/' ERROR - CHANNEL #1 NOT INITIALIZED')                   H
C     CALL ABORT                                                        H
C
C
      END
