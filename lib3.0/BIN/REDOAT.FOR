C
C
C
      PROGRAM   REDOAT
C
C     + + + PURPOSE + + +
C     utility program to write scenario, location, and constituents
C     to wdm files for scenario generators
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       INPFL,I1,WDMSFL,RETCOD,DSN,SAIND,SALEN,IPOS,
     1              I4,I0,I80,FE,DSNTYP,NAMFL,MESSFL,I,LEN,I5
      REAL          RBUFF
      CHARACTER*1   BLNK,OSTR1(120)
      CHARACTER*64  WDMNAM,MESSNM,VERSN
      CHARACTER*120 OSTR
C
C     + + + FUNCTIONS + + +
      INTEGER       WDCKDT,CHRINT
      REAL          CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL      WDBOPN,WDBSAC,WDFLCL,WDCKDT,CHRINT,CVARAR
      EXTERNAL      WDBSAR,CHRDEC
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (A64)
1010  FORMAT (A120)
C
C     + + + END SPECIFICATIONS + + +
C
C     version information
      VERSN = '@(#)redoat - write scenario, location, and constituents'
      VERSN = '@(#)redoat - attributes to wdm file'
      INCLUDE 'fversn.inc'
C
      I0 = 0
      I1 = 1
      I4 = 4
      I5 = 5
      I80= 80
      BLNK= ' '
C
      FE  = 99
      OPEN(UNIT=FE,FILE='error.fil',STATUS='UNKNOWN')
      NAMFL = 10
      OPEN(UNIT=NAMFL,FILE='scloco.inp',STATUS='OLD')
      INPFL = 13
      OPEN(UNIT=INPFL,FILE='redoat.inp',STATUS='OLD')
C     read name of wdm file from name file
      READ(NAMFL,1000) WDMNAM
Ckmf  read name of message file from name file
      READ(NAMFL,1000) MESSNM
C     INCLUDE 'fmsgwd.inc'
C     open message file
      MESSFL = 9
      CALL WDBOPN(MESSFL,MESSNM,I0,
     O            RETCOD)
C     open wdm file
      WDMSFL = 12
      CALL WDBOPN(WDMSFL,WDMNAM,I0,
     O            RETCOD)
      IF (RETCOD.GE.0) THEN
C       okay to continue
C       read header line
        READ(INPFL,1000)
C
C       loop through all data sets
  10    CONTINUE
C         read first line
          READ (INPFL,1010,END=100) OSTR
          I = 120
          CALL CVARAR (I,OSTR,I,OSTR1)
C         get data set number
          IPOS = 1
          DSN = CHRINT(I5,OSTR1(IPOS))
          WRITE (*,*) DSN
C         see if this data set exists in this wdm file
          DSNTYP = WDCKDT(WDMSFL,DSN)
          IF (DSNTYP.EQ.1) THEN
C           time series type data set exists, continue
C
C           put attribute for scenario name
            SAIND = 288
            SALEN = 8
            IPOS  = 7
            CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,
     O                   OSTR1(IPOS),RETCOD)
C
C           put next attribute for constituent name
            SAIND = 289
            SALEN = 8
            IPOS = 16
            CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,
     O                   OSTR1(IPOS),RETCOD)
C
C           put next attribute for location name
            SAIND = 290
            SALEN = 8
            IPOS = 25
            CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,
     O                   OSTR1(IPOS),RETCOD)
C
C           put next attribute for station id
            SAIND = 2
            SALEN = 16
            IPOS = 34
            CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,
     O                   OSTR1(IPOS),RETCOD)
C
C           put next attribute for station name
            SAIND = 45
            SALEN = 48
            IPOS = 53
            CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,
     O                   OSTR1(IPOS),RETCOD)
C
C           put next attribute for lat
            SAIND = 8
            SALEN = 1
            IPOS  = 103
            LEN   = 8
            RBUFF = CHRDEC(LEN,OSTR1(IPOS))
            CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,
     I                   RBUFF,
     O                   RETCOD)
C
C           put next attribute for long
            SAIND = 9
            SALEN = 1
            IPOS = 112
            LEN   = 8
            RBUFF = CHRDEC(LEN,OSTR1(IPOS))
            CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,
     I                   RBUFF,
     O                   RETCOD)
C
          END IF
        GO TO 10
C
 100    CONTINUE
C
        CALL WDFLCL (WDMSFL,RETCOD)
        CALL WDFLCL (MESSFL,RETCOD)
      END IF
      CLOSE(UNIT=INPFL)
      CLOSE(UNIT=NAMFL)
C
      STOP
      END
