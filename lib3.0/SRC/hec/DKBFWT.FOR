      SUBROUTINE DKBFWT(IH,CL,IB,ISTAT)
C
C     WRITE A SINGLE LINE TO IH USING BUFFER
C
C     IH- FILE HANDLE
C     CL- LINE TO BE RETURNED
C     IB- BUFFER USED TO WRITE
C        MUST BE (# OF BYTES/2) + 10   IN LENGTH(16 BIT WORDS)
C        MUST BE PRE SET BY CALL TO PCOPEN
C        DO NOT CHANGE AFTER FIRST USE !!!!!!
C     ISTAT- RETURN STATUS  0= OK
C                          -1= DISK FULL
C                          -3= BUFFER NOT INITIALIZED
C
C
C      IB(1)- UNIQUE FLAG SET AFTER FIRST USE
C      IB(2)- START WORD OF TRANSFER AREA
C      IB(3)- TOTAL WORDS IN BUFFER
C      IB(4)- OUT BYTE POINTER
C      IB(5)- IN BYTE POINTER (NEXT FREE BYTE)
C      IB(6)- READ/WRITE FLAG 1=READ/-1=WRITE
C      IB(7)- RESERVED
C      IB(8)- IH FILE HANDLE
C      IB(9)- LENGTH OF TRANSFER AREA IN BYTES
C      IB(10)- STARTING BYTE LOCATION OF TRANSFER AREA
C
C
C
      LOGICAL LFIRST
C
      PARAMETER (NCHEOL=2)                                              M
C     PARAMETER (NCHEOL=1)                                              u
C
      INTEGER IB(2058)
C
      CHARACTER CL*(*), CEND*2
C
      DATA JUNIQ/3566/
      DATA ICR, ILF / 13, 10 /
      DATA LFIRST/.TRUE./
C
      IF(LFIRST) THEN
      LFIRST=.FALSE.
      CEND(1:1)=CHAR(ICR)                                               M
      CEND(2:2)=CHAR(ILF)                                               M
C     CEND(1:1)=CHAR(ILF)                                               u
      ENDIF
C
C     TEST FOR INIT OF BUFFER
C
      IF(IB(1).NE.JUNIQ) THEN
C
      ISTAT=-3
      RETURN
      ENDIF
C
C     NEW LINE
      IS=1
C     SET WRITE FLAG
      IB(6)=-1
C
C     CHECK FOR ROOM IN BUFFER FOR LINE AND ICR + ILF
C
C
 500  CONTINUE
      CALL CHRLNB (CL, N)
      N = N - IS + 1 + NCHEOL
      M= IB(9)-(IB(5)-IB(4))
      J=MIN0(N,M) - 1
C     write(*,1)(IB(K),K=1,10)
C1    FORMAT(1X,10I7)
      CALL CHRHOL (CL(IS:IS+J), 1, J+1, IB, IB(5))
C
C     ------ CASE 1 -- ROOM TO SPARE
      IF(N.LT.M) THEN
C     STUFF ICR AND ILF AND GO ON
      CALL CHRHOL (CEND, 1, NCHEOL, IB, IB(5)+N-NCHEOL)
      IB(5)=IB(5) + N
      ISTAT=0
C     write(*,1)(IB(K),K=1,10)
      RETURN
C
C     ------ CASE 2 -- ROOM FOR PART OF LINE ONLY
      ELSE IF(N.GT.M+NCHEOL) THEN
C     DUMP BUFFER, MOVE POINTER AND RECYCLE
      CALL WRITF(IH,IB(IB(2)),IB(9),ISTAT,NTT)
      IF(IB(9).NE.NTT) THEN
      ISTAT=-1
      RETURN
      ENDIF
      IB(5)=IB(10)
      IS=IS + J + 1
      GO TO 500
C
C     ------ CASE 3 -- ROOM FOR ICR & ILF EXACTLY
      ELSE IF(N.EQ.M) THEN
C     STUFF ICR AND ILF, DUMP BUFFER AND GO ON
      CALL CHRHOL (CEND, 1, NCHEOL, IB,IB(5)+N-NCHEOL)
      CALL WRITF(IH,IB(IB(2)),IB(9),ISTAT,NTT)
      IF(IB(9).NE.NTT) THEN
      ISTAT=-1
      RETURN
      ENDIF
      IB(5)=IB(10)
      RETURN
C
C     ------ CASE 4 -- ROOM FOR ICR ONLY
      ELSE IF(N.EQ.M+1) THEN                                            M
C     STUFF ICR, DUMP BUFFER, STUFF ILF AND RETURN
      ILAST=IB(10) + IB(9) - 1                                          M
      CALL CHRHOL (CEND(1:1), 1, 1, IB,ILAST)                           M
      CALL WRITF(IH,IB(IB(2)),IB(9),ISTAT,NTT)                          M
      IF(IB(9).NE.NTT) THEN                                             M
      ISTAT=-1                                                          M
      RETURN                                                            M
      ENDIF                                                             M
      CALL CHRHOL (CEND(2:2), 1, 1, IB,IB(10))                          M
      IB(5)=IB(10) + 1                                                  M
      RETURN                                                            M
C
C     ------ CASE 5 -- ROOM FOR LINE BUT NOT ICR AND ILF
      ELSE IF(N.EQ.M+NCHEOL) THEN
C     DUMP BUFFER, STUFF ICR AND ILF AND RETURN
      CALL WRITF(IH,IB(IB(2)),IB(9),ISTAT,NTT)
      IF(IB(9).NE.NTT) THEN
      ISTAT=-1
      RETURN
      ENDIF
      CALL CHRHOL ( CEND, 1, NCHEOL, IB,IB(10))
      IB(5) = IB(10) + NCHEOL
      RETURN
C
      ENDIF
C
      END
