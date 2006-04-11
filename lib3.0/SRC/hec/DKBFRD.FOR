      SUBROUTINE DKBFRD(IH,CL,NL,IB,ISTAT)
      PARAMETER (IOOK=0,IEOF=-1,IEOT=-1)
C
C     READ IN A SINGLE LINE USING BUFFER
C
C     CL- LINE TO BE RETURNED
C     NL- LENGTH OF RETURNED LINE
C     IB- BUFFER
C     ISTAT- RETURN STATUS  IOOK
C                           IEOF
C                           IEOT
C                           BUFFER NOT INITIALIZED = -3
C
C      IB(1)- UNIQUE FLAG SET AFTER FIRST USE
C      IB(2)- START WORD OF TRANSFER AREA
C      IB(3)- TOTAL WORDS IN BUFFER
C      IB(4)- OUT BYTE POINTER (NEXT BYTE TO READ)
C      IB(5)- IN BYTE POINTER  (NEXT BYTE TO WRITE)
C      IB(6)- READ/WRITE FLAG  1=READ, -1=WRITE
C      IB(7)- NEW BUFFER LOADED TO PERFORM OPERATION(-1=NO,ELSE NT)
C      IB(8)- IH FILE HANDLE
C      IB(9)- LENGTH OF TRANSFER AREA IN BYTES
C      IB(10)- STARTING BYTE LOCATION OF TRANSFER AREA
C
C
C
      LOGICAL LLF
C
      INTEGER IB(2058)
C
      CHARACTER CL*(*)
C
      DATA JUNIQ/3566/
      DATA JCR, JLF / 13, 10 /
C
C     TEST FOR INIT OF BUFFER
C
      CL = ' '
      IF(IB(1).NE.JUNIQ) THEN
C
      ISTAT=-3
      GO TO 800
      ENDIF
C
C     SET OLD BUFFER FLAG
      IB(7) = -1
C
C     PROCESS
      GO TO 300
C
C     LOAD NEW BUFFER
C
 200  CONTINUE
      CALL READF(IH,IB(IB(2)), IB(9),JS,NT)
C
      IB(7) = NT
C
C     CHECK FOR EOF
      IF(NT.LE.0) THEN
      ISTAT=IEOF
      GO TO 800
      ENDIF
C
C
C     UPDATE NEW BUFFER POINTERS
      IB(5)=IB(10)+NT
      IB(4)=IB(10)
C
C     WRITE(*,*)(IB(IP),IP=1,10)
      GO TO 100
C     NEW LINE
300   IS=1
      NL=0
      IMAX=LEN(CL)
C
C     CHECK FOR EMPTY BUFFER
C
100   IF(IB(4).GE.IB(5)) GO TO 200
C
C     LOOK FOR A LF CHAR IN BUFFER
C
      I=INDXI(IB,IB(4),IB(5)-1,JLF)
C
C     Look for an end of file (cntl-Z)
      IF (I.EQ.0) THEN
      I=INDXI(IB,IB(4),IB(5)-1,26)
      IF (I.GT.0) I = I + 1
      ENDIF
C
      IF(I.EQ.0) THEN
C     NO LF CHAR - PARTIAL LINE OR EMPTY
      I=IB(5)-1
C     CHECK FOR CR AS LAST CHAR IN BUFFER
C
      II=INDXI(IB,I,I,JCR)                                              M
      IF(II.EQ.0) THEN                                                  M
      IDEAD=0
      ELSE                                                              M
      IDEAD=1                                                           M
      ENDIF                                                             M
      LLF=.FALSE.
C
      ELSE
C
C     FOUND LF IN BUFFER
C
      LLF=.TRUE.
C     CHECK IF AT FIRST CHAR POSITION IN TRANSFER AREA
      IF(I.EQ.IB(10)) THEN                                              M
      IDEAD=1
      ELSE                                                              M
      IDEAD=2                                                           M
      ENDIF                                                             M
      ENDIF
C     WRITE(*,*)' IB(4),I ,IS,NL,IDEAD',IB(4),I,IS,NL,IDEAD
C     CHECK FOR ROOM IN LINE
C
      N=IMAX-IS + 1
C     N=MAX0(0,N)
      M= I - IB(4) - IDEAD + 1
C     M=MAX0(0,M)
      J=MIN0(N,M) - 1
      IF(J.GE.0) THEN
      CALL HOLCHR(IB, IB(4), J+1, CL(IS:), 1)
      IS=IS + J  + 1
      ENDIF
      NL=NL + M
      IB(4)=I + 1
C
C     SPLIT BACK IF NO LF YET
C
      IF(.NOT.LLF) GO TO 200
C
C     SET FLAG IF MORE CHAR IN LINE THAN CAN FIT
C     IF(M.GT.N) KK=1
C
      IF(IS.LE.IMAX) THEN
      N=IMAX-IS + 1
C     BLANK TAIL OF LINE
      CALL CHRBLK(CL(IS:))
      ENDIF
      ISTAT=IOOK
 800  CONTINUE
      RETURN
C
      END
