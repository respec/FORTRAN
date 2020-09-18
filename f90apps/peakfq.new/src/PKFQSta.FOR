C
C
C
      SUBROUTINE   PKFQSTA
     I                   (IOPT,CURSTA,NSTA)
C
C     + + + PURPOSE + + +
C     routine to show run status for PeakFQ in PKFQWin
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IOPT,CURSTA,NSTA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output
C     CURSTA - current station
C     NSTA   - total number of stations
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          OPCT,PCT,ILEN,OPT
      DOUBLE PRECISION BINC,LINC,BCUR,LCUR
      CHARACTER*1      CPCT,TXT(12)
      SAVE             OPCT,BINC,LINC
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TXT,LTXT)
      CHARACTER*12 LTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    UPDWIN
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(I1,'%')
2010  FORMAT(I2,'%')
C
C     + + +  END SPECIFICATIONS + + +
C
      IF (CURSTA.EQ.1 ) THEN
        BINC= FLOAT(1)/FLOAT(NSTA)
        !LINC= BINC * (FLOAT(1)/FLOAT(NOPNS))
        OPCT = 0
        ILEN= 12
        OPT = 1
        LTXT= 'Executing'
        CALL UPDWIN(OPT,ILEN,TXT)
        OPT = 2
        LTXT= '  Now    '
        CALL UPDWIN(OPT,ILEN,TXT)
        OPT = 4
        LTXT= 'Complete '
        CALL UPDWIN(OPT,ILEN,TXT)
      END IF
      BCUR= FLOAT(CURSTA-1) * BINC
      !LCUR= FLOAT(OPN-1)   * LINC
      !PCT = 100* (LCUR + BCUR)
      PCT = 100* BCUR
      IF (PCT .NE. OPCT) THEN
        ILEN= 1
        CPCT= CHAR(PCT)
        CALL UPDWIN(IOPT,ILEN,CPCT)
        IF (PCT < 10) THEN
          WRITE(LTXT,2000) PCT
        ELSE IF (PCT < 100) THEN
          WRITE(LTXT,2010) PCT
        ELSE
          LTXT = '99%'
        END IF
        OPT = 3
        CALL HDMES3(OPT,LTXT)
      END IF
      OPCT = PCT
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMES3
     I                   (IOPT,TXT)
C
C     + + + PURPOSE + + +
C     write text to window
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          ::  IOPT
      CHARACTER(LEN=*) ::  TXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT    - position to output
C     TXT     - text to write
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ILEN
C
C     + + + INTRINSICS + + +
      INTRINSIC    LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL     UPDWIN
C
C     + + +  END SPECIFICATIONS + + +
C
      ILEN = LEN(TXT)
      CALL UPDWIN(IOPT,ILEN,TXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESN
     I                   (IOPT,INUM)
C
C     + + + PURPOSE + + +
C     write integer number to window
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT, INUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output
C     INUM   - number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ILEN
      CHARACTER*6  CNUM
C
C     + + + EXTERNALS + + +
      EXTERNAL     UPDWIN
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(I6)
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(CNUM,2000) INUM
      ILEN = 6
      CALL UPDWIN(IOPT,ILEN,CNUM)
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION CKUSER ()
C
C     + + + PURPOSE + + +
C     ckeck user status - 1 is cancel
C
      USE SCENMOD, ONLY: UPDATESTATUS
C     INTEGER    UPDATESTATUS
C     DLL_IMPORT UPDATESTATUS
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IOPT,IRET
      INTEGER*1   JTXT(1)
C
C     + + + END SPECIFICATIONS + + +
C
      IOPT= 0
      IRET= UPDATESTATUS(IOPT,JTXT)
      IF (IRET .GT. 0) THEN
        CKUSER = 1
      ELSE
        CKUSER = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE UPDWIN(IOPT,ILEN,ATXT)
C
C     + + + PURPOSE + + +
C     write status (nowait) to ms window
C
      USE SCENMOD, ONLY: UPDATESTATUS
C     INTEGER    UPDATESTATUS
C     DLL_IMPORT UPDATESTATUS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IOPT, ILEN
      CHARACTER*1  ATXT(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT    - position to output
C     ILEN    - length of text to output
C     ATXT    - text to output
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      OLEN, JLEN
      INTEGER      I
      INTEGER*1    JTXT(256)
C
C     + + + END SPECIFICATIONS + + +
C
C     text to local string
      IF (IOPT .EQ. 5) THEN
        OLEN= 1
        JLEN= 1
      ELSE
        IF (IOPT.EQ.1) THEN
          OLEN = 72
        ELSE IF (IOPT.EQ.2 .OR. IOPT.EQ.4) THEN
          OLEN = 12
        ELSE IF (IOPT.EQ.3) THEN
          OLEN = 6
        ELSE IF (IOPT.EQ.10) THEN
          OLEN = 48
        ELSE IF (IOPT.EQ.6) THEN
          OLEN = 255
        ELSE
          OLEN = 6
        END IF
        JLEN = ILEN
        IF (JLEN .GT. OLEN) THEN
          JLEN = OLEN
        END IF
      END IF
      !WRITE(*,*)'HSPSTA:UPDWIN:stat:',IOPT,OLEN,ILEN,JLEN
C     text to I*1 array
      JTXT= 32
      I   = 1
      DO WHILE (I.LE.JLEN)
        JTXT(I)= ICHAR(ATXT(I))
        I      = I+ 1
      END DO
      JTXT(I)= 0
      !IF (IOPT .EQ. 5) WRITE(*,*) 'HSPSTA:slider',JTXT(1),JTXT(2)
      I= UPDATESTATUS(IOPT,JTXT)
C
      RETURN
      END