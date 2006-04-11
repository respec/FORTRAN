      SUBROUTINE PLDSCN (CSCRN)
C
C     CHECK SCREEN BUFFER FOR DESIRED SCREEN
C     IF THERE RETURN, ELSE LOAD DESIRED SCREEN
C     IF NO ROOM IN BUFFER KICK OUT LOWEST PRIORITY SCREEN
C
      CHARACTER*8 CSCRN, CLINE*132
C
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PSCNCH                                                           H
      INCLUDE 'pscnch.h'                                                MLlg
C
C
C       USE OF ARRAY  IB(I,K) WHERE I=ITEM NUMBER BELOW
C     1       BEGIN BYTE POSITION OF CHAR AREA FOR ENTIRE INFO
C     2       END
C     3       BEGIN RELATIVE BYTE POS OF NAME
C     4       END
C     5       BEGIN RELATIVE BYTE POS OF SCREEN IMAGE
C     6       END
C     7       BEGIN RELATIVE BYTE POS OF TRANSLATE TABLE
C     8       END
C     9       BEGIN RELATIVE BYTE POS OF APPEND TABLE
C     10      END
C     11      # COLS IN SCREEN IMAGE
C     12      # ROWS IN SCREEN IMAGE
C     13      # COLS IN TRANSLATE/APPEND LINES ( ZERO FOR NO REPLY)
C     14      # TRANSLATE LINES
C     15      # APPEND LINES
C     16      TOTAL # BYTES REQ FOR ALL CHAR STORAGE
C     17      PROMPT CURSOR ROW
C     18      PROMPT CURSOR COL
C     19      SCREEN RETENTION PRIORITY
C     20      FULL SCREEN ERASE FLAG
C     21      BASE ROW
C     22      BASE COL
C     23      MESSAGE CURSOR ROW
C     24      MESSAGE CURSOR COL
C     25      ?
C     26      NUMBER OF ALLOWABLE CONSEQUECTIVE ERRORS
C     27      INACTIVITY TIMOUT IN SECONDS FOR THIS SCREEN
C     28      SCREEN DISPLAY LEVEL 0-9
C     29      # LINES IN ATTMAP AREA, EACH SAME WIDTH AS SCREEN
C     30      OFFSET POSITION IN ATTMAP LINE FOR ATTRIBUTES
C
C        USE OF ARRAY  IC(I)  WHERE  I=ITEM NUMBER BELOW
C
C     1       MAX # OF SCREENS SAVED (ONE LESS THAN DIMENSION  KIC)
C     2       NEXT AVAILABLE SLOT
C     3       MAX # CHAR IN CHAR AREA CSCN
C     4       NEXT AVAILABLE CHAR IN CHAR AREA
C
C
      CHARACTER CTMP*80
      LOGICAL LFIRST, LFLUSH
      DATA LFIRST/.TRUE./
C
C
C
      IF (ISCN.EQ.-1) RETURN
      IF(LFIRST) THEN
      OPEN (UNIT=ISCN,FILE=CSCNFL,STATUS='OLD',ERR=900,IOSTAT=ISTAT)
      IC(1)= KSLOTS - 1
      IC(2)=1
      IC(3)= KSCN
      IC(4)=1
      LFIRST=.FALSE.
      ENDIF
C     CHECK IF DESIRED SCREEN IS IN BUFFER
      DO 1 I=1,IC(2)-1
      IS=IB(3,I) + IB(1,I) - 1
      IE=IS + 7
      IT=0
      DO 2 II=IS,IE
      IT=IT+1
      IF(CSCN(II)(1:1) .NE. CSCRN(IT:IT)) GO TO 1
    2 CONTINUE
      ISCRN=I
      LSCN=.TRUE.
      RETURN
    1 CONTINUE
      REWIND ISCN
   10 READ(ISCN,20,END=700) CLINE
   20 FORMAT(A)
      IF(CLINE(1:8) .NE. '#SCREEN ') GO TO 10
      IF(CLINE(9:16) .NE. CSCRN) GO TO 10
C     FOUND DESIRED SCREEN
C     MAKE A SLOT FOR IT    - DOES NOT MEAN ROOM AVAILABLE YET !!
      IF(IC(2) .GT. IC(1)) THEN
      IC(2)=IC(2) - 1
      IC(4)=IC(4) - IB(16,IC(2))
      ENDIF
      J=IC(1) + 1
C ------
C     READ(CLINE,25,ERR=1010)IB(11,J),IB(12,J),IB(21,J),IB(22,J)
C    + ,IB(19,J),IB(20,J),NL,IB(13,J),IB(26,J),IB(27,J),IB(28,J)
C  25 FORMAT(16X,4I3,I2,Z2,2I3,I2,I4,I2)
C ------
      CALL PGFSCN ( CLINE(18:19), '(I2)', IB(11,J) )
      IF ( IB(11,J) .LT. 0 ) GO TO 1010
      CALL PGFSCN ( CLINE(21:22), '(I2)', IB(12,J) )
      IF ( IB(12,J) .LT. 0 ) GO TO 1010
      CALL PGFSCN ( CLINE(24:25), '(I2)', IB(21,J) )
      CALL PGFSCN ( CLINE(27:28), '(I2)', IB(22,J) )
      CALL PGFSCN ( CLINE(30:30), '(I1)', IB(19,J) )
      CALL PGFSCN ( CLINE(32:32), '(Z1)', IB(20,J) )
      CALL PGFSCN ( CLINE(33:35), '(I3)', NL       )
      IF ( NL       .LT. 0 ) GO TO 1010
      CALL PGFSCN ( CLINE(37:38), '(I2)', IB(13,J) )
      IF ( IB(13,J) .LT. 0 ) GO TO 1010
      CALL PGFSCN ( CLINE(40:40), '(I1)', IB(26,J) )
      CALL PGFSCN ( CLINE(42:44), '(I3)', IB(27,J) )
      CALL PGFSCN ( CLINE(46:46), '(I1)', IB(28,J) )
      CALL PGFSCN ( CLINE(48:49), '(I2)', IB(29,J) )
C ------ Check if screen has ATTRIBUTES
      IF ( IB(29,J) .GT. 0 ) THEN
      IA = 2
      ELSE
      IA = 1
      ENDIF
      CALL PGFSCN ( CLINE(51:52), '(I2)', IB(30,J) )
C
C ------
      NREQ= IB(11,J)*IB(12,J)*IA + NL*IB(13,J) + 12 + IB(29,J)*IB(11,J)
      IB(16,J) = NREQ
C     CHECK IF THERE IS ROOM IN BUFFER FOR THIS SCREEN
   30 IF(IC(3)-IC(4) .LT. NREQ) THEN
C     NEED TO KICK OUT LOWEST PRIORITY SCREEN
      ILOW=IC(2)-1
      IF(ILOW .LT. 1) GO TO 1020
      IC(2)=IC(2)-1
      IC(4)=IC(4) - IB(16,ILOW)
      GO TO 30
      ENDIF
C     LOAD IN NEW SCREEN, WE KNOW WE HAVE A SLOT AND ENOUGH SPACE
C     FIND WHICH SLOT AND MOVE THINGS IF NECESSARY
      DO 40 I=1,IC(2)-1
      IF(IB(19,I) .GT. IB(19,J)) GO TO 50
      IF(IB(19,I) .EQ. IB(19,J).AND. IB(16,I).GT.IB(16,J)) GO TO  50
   40 CONTINUE
      I=IC(2)
   50 K=I
      N=K+1
      IF(K.LT.IC(2)) THEN
C     MUST MOVE STUFF - BACKWARDS
      DO 75 I=IC(2),N,-1
      DO 60 II=1,KIB
   60 IB(II,I) = IB(II,I-1)
C     NOW MOVE CHAR STUFF
      DO 70 II=IB(2,I),IB(1,I),-1
   70 CSCN(II+NREQ) = CSCN(II)
      IB(1,I)=IB(1,I) + NREQ
      IB(2,I)=IB(2,I) + NREQ
   75 CONTINUE
      ENDIF
C
C
C     NOW FINISH LOADING NEW STUFF IN SLOT AND CHAR AREA
      DO 80 I=1,KIB
   80 IB(I,K) = IB(I,J)
      IF(K.EQ.IC(2)) THEN
      IB(1,K) = IC(4)
      ELSE
      IB(1,K) = IB(1,N) - NREQ
      ENDIF
      IC(4)= IC(4) +NREQ
      IC(2) = IC(2) + 1
      IB(2,K) = IB(1,K) + NREQ - 1
      IB(14,K)=0
      IB(15,K)=0
C     READ CHAR IN SCREEN IMAGE
      IB(3,K)= 1
      IB(4,K) = 8
C------ Keep char position 9,10,11,12 for special ATTRIBUTES
      IB( 5,K) = 13
C     STORE NAME OF SCREEN
      IS= IB(1,K) + IB(3,K) - 2
      DO 85 I=1,8
   85 CSCN(IS+I) = CSCRN(I:I)
      IE= IB(1,K) + IB( 5,K) - 2
      IBA = IB(12,K) * IA
      DO 100 II=1,IBA + IB(29,K)
      IS=IE+1
      IE=IS+IB(11,K) - 1
      READ(ISCN,90,END=800) (CSCN(I), I=IS,IE)
   90 FORMAT(80A1)
C------
      IF(IB(29,K).GT.0.AND.MOD(II,IB(12,K)).EQ.0.AND.II.LE.IBA) THEN
C------ Read one line out of the way ( ie, #ATTRIBUTES or #ATTMAP )
      READ(ISCN,20,END=800) CLINE
      IF ( CLINE(1:4) .NE. '#ATT' ) GO TO 1050
      ENDIF
C------
  100 CONTINUE
      IB(6,K)=IB(5,K) + IB(11,K)*IB(12,K)*IA +IB(29,K)*IB(11,K) -1
C     READ PROMPT LOCATION
      READ(ISCN,20) CLINE
      IF(CLINE(1:8) .NE. '#PROMPT ') GO TO 1060
      READ(CLINE,110) IB(17,K), IB(18,K)
  110 FORMAT(8X,I2,1X,I2)
      KATT = IB(1,K) + IB(4,K)
      CSCN(KATT)   = CLINE(15:15)
      CSCN(KATT+1) = CLINE(17:17)
C     READ MESSAGE LOCATION
      READ(ISCN,20) CLINE
      IF(CLINE(1:9) .NE. '#MESSAGE ') GO TO 1060
      READ(CLINE,111) IB(23,K), IB(24,K)
  111 FORMAT(9X,I2,1X,I2)
      CSCN(KATT+2) = CLINE(16:16)
C     READ TRANSLATE TABLE
      READ(ISCN,20) CLINE
      IF(CLINE(1:10) .NE. '#TRANSLATE') GO TO 1070
      IB(7,K)=IB(6,K) + 1
      READ(CLINE,120) IB(14,K)
  120 FORMAT(BN,10X,I4)
C ------ Check for flush type-ahead flag
      LFLUSH = .FALSE.
      IF ( CLINE(15:19) .EQ. 'FLUSH' ) LFLUSH = .TRUE.
C ------
      IE= IB(1,K) + IB(7,K) - 2
      DO 130 II=1,IB(14,K)
      READ ( ISCN, 20, END=800 ) CTMP
C------ Force upper case if case insensitive --- , or ; ----
      IF ( CTMP(2:2).EQ.',' .OR. CTMP(2:2).EQ.';' ) THEN
      CALL UPCASE ( CTMP(3:10) )
      ENDIF
      IS=IE+1
      IE=IS+IB(13,K)-1
      ITMP = 0
      DO 125 I = IS, IE
      ITMP = ITMP + 1
  125 CSCN(I) = CTMP (ITMP:ITMP)
  130 CONTINUE
      IB(8,K)=IB(7,K) + IB(14,K)*IB(13,K) -1
C     READ APPEND IF PRESENT
      IB(15,K) = 0
  135 READ(ISCN,20) CLINE
      IF(CLINE(1:8) .EQ. '#APPEND ') THEN
      IB(9,K)=IB(8,K) + 1
      READ(CLINE,140) IB(15,K)
  140 FORMAT(8X,I2)
      IE= IB(1,K) + IB(9,K) - 2
      DO 150 II=1,IB(15,K)
      IS=IE+1
      IE=IS+IB(13,K) -1
      READ(ISCN,90,END=800) (CSCN(I), I=IS,IE)
  150 CONTINUE
      GO TO 135
      ELSE IF(CLINE(1:10) .EQ. '#ENDSCREEN') THEN
C     ALL DONE CLEAN UP AND RETURN
C ------ Set Translate count negative to flag a flush of type-ahead
      IF ( LFLUSH ) IB(14,K) = -IB(14,K)
      ISCRN=K
      LSCN=.TRUE.
      ELSE
      GO TO 800
      ENDIF
C ------
C
      RETURN
C
C ------
  700 WRITE(IDSP,710) CSCRN
  710 FORMAT(/,' Requested screen  "',A,'"  not found')
      CALL WAITS (2.0)
      LSCN=.FALSE.
      RETURN
  800 WRITE(IDSP,810) CLINE
  810 FORMAT(/,' **** ERROR - UNEXPECTED EOF ON SCREEN FILE'/1X,A/)
      LSCN=.FALSE.
      RETURN
  900 WRITE(IDSP,910) CSCNFL
  910 FORMAT(/,' **** ERROR - SCREEN FILE NOT FOUND: ',A/)
      LSCN=.FALSE.
      RETURN
 1010 WRITE(IDSP,1011) CLINE
 1011 FORMAT(/,' **** ERROR - ILLEGAL CHAR FOR INTEGER CONVERSION'
     +  /1X,A/)
      CALL ABORT
 1020 WRITE(IDSP,1021) CLINE
 1021 FORMAT(/,' **** ERROR - BUFFER TOO SMALL FOR SCREEN'/1X,A/)
      CALL ABORT
 1050 WRITE(IDSP,1051) CLINE
 1051 FORMAT(/,' **** ERROR - EXPECTED #ATT    - NOT FOUND'/1X,A/)
      CALL ABORT
 1060 WRITE(IDSP,1061) CLINE
 1061 FORMAT(/,' **** ERROR - EXPECTED #PROMPT - NOT FOUND'/1X,A/)
      CALL ABORT
 1070 WRITE(IDSP,1071) CLINE
 1071 FORMAT(/,' **** ERROR - EXPECTED #TRANSLATE - NOT FOUND'/1X,A/)
      CALL ABORT
      END
