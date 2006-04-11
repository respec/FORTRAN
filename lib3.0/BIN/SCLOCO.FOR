C
C
C
      PROGRAM   SCLOCO
C
C     + + + PURPOSE + + +
C     utility program to maintain scenario, location, and constituents
C     on wdm files for scenario generators
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       INPFL,OUTFL,I1,WDMSFL,RETCOD,DSN,SAIND,SALEN,IPOS,
     1              I4,I0,JLEN,I80,FE,DSNTYP,I,LEN,I5,I2,
     2              SDATE(6),EDATE(6),DSFREC
      REAL          RBUFF
      CHARACTER*1   BLNK,CBUFF1(80),OSTR1(140)
      CHARACTER*64  WDMNAM,VERSN
C
C     + + + FUNCTIONS + + +
      INTEGER       WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL      WDBOPN,ZIPC,WDBSGC,INTCHR,CHRCHR,WDFLCL,WDCKDT
      EXTERNAL      DECCHR,WDBSGR,WTFNDT
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (A64)
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT ('  DSN  IDSCEN   IDCONS   IDLOCN   STAID',
     1        '             STANAM')
2010  FORMAT (140A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     version information
      VERSN = '@(#)scloco - maintain scenario, location, & constituents'
      VERSN = '@(#)scloco - attributes in wdm file'
      INCLUDE 'fversn.inc'
C
      I0 = 0
      I1 = 1
      I2 = 2
      I4 = 4
      I5 = 5
      I80= 80
      BLNK= ' '
C
      FE  = 99
      OPEN(UNIT=FE,FILE='error.fil',STATUS='UNKNOWN')
      INPFL = 10
      OPEN(UNIT=INPFL,FILE='scloco.inp',STATUS='OLD')
      OUTFL = 11
      OPEN(UNIT=OUTFL,FILE='scloco.out')
C     read name of wdm file from input file
      READ(INPFL,1000) WDMNAM
C     open wdm file
      WDMSFL = 12
      CALL WDBOPN(WDMSFL,WDMNAM,I0,
     O            RETCOD)
      IF (RETCOD.GE.0) THEN
C       okay to continue
C       write headers to output file
        WRITE(OUTFL,2000)
C
C       loop through all data sets
        DO 10 DSN = 1,32000
C         see if this data set exists in this wdm file
          DSNTYP = WDCKDT(WDMSFL,DSN)
          IF (DSNTYP.EQ.1) THEN
C           time series type data set, continue
C           clear out output buffer
            I = 140
            CALL ZIPC (I,BLNK,OSTR1)
C
C           get attribute for scenario name
            SAIND = 288
            SALEN = 8
            CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                   CBUFF1,RETCOD)
            IPOS = 1
            CALL INTCHR (DSN,I5,I0,JLEN,OSTR1(IPOS))
            IF (RETCOD.EQ.-107) THEN
C             scenario name attribute not present,
C             try agency code attribute
              SAIND = 40
              SALEN = 8
              CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                     CBUFF1,RETCOD)
              IF (RETCOD.EQ.0) THEN
C               use this attribute as default
                IPOS = 7
                CALL CHRCHR (SALEN,CBUFF1,OSTR1(IPOS))
              END IF
            ELSE
C             scenario name attribute present,
C             write out values as desired
              IPOS = 7
              CALL CHRCHR (SALEN,CBUFF1,OSTR1(IPOS))
            END IF
C
C           get next attribute for constituent name
            SAIND = 289
            SALEN = 8
            CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                   CBUFF1,RETCOD)
            IF (RETCOD.EQ.-107) THEN
C             this attribute not present,
C             try tstype
              SAIND = 1
              SALEN = 4
              CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                     CBUFF1,RETCOD)
              IF (RETCOD.EQ.0) THEN
C               use this attribute as default
                IPOS = 16
                CALL CHRCHR (SALEN,CBUFF1,OSTR1(IPOS))
              END IF
            ELSE
C             constituent name attribute present,
C             write out value as desired
              IPOS = 16
              CALL CHRCHR (SALEN,CBUFF1,OSTR1(IPOS))
            END IF
C
C           get next attribute for location name
            SAIND = 290
            SALEN = 8
            CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                   CBUFF1,RETCOD)
            IF (RETCOD.EQ.-107) THEN
C             this attribute not present,
C             try gucode attribute
              SAIND = 46
              SALEN = 12
              CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                     CBUFF1,RETCOD)
              IF (RETCOD.EQ.0) THEN
C               use this attribute as default
                SALEN= 8
                IPOS = 25
                CALL CHRCHR (SALEN,CBUFF1,OSTR1(IPOS))
              END IF
            ELSE
C             location name attribute present,
C             write out value as desired
              IPOS = 25
              CALL CHRCHR (SALEN,CBUFF1,OSTR1(IPOS))
            END IF
C
C           get next attribute for station id
            SAIND = 2
            SALEN = 16
            CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                   CBUFF1,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             station id attribute present,
C             write out value as desired
              IPOS = 34
              CALL CHRCHR (SALEN,CBUFF1,OSTR1(IPOS))
            END IF
C
C           get next attribute for station name
            SAIND = 45
            SALEN = 48
            CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                   CBUFF1,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             station name attribute present,
C             write out value as desired
              IPOS = 53
              CALL CHRCHR (SALEN,CBUFF1,OSTR1(IPOS))
            END IF
C
C           get next attribute for lat
            SAIND = 8
            SALEN = 1
            CALL WDBSGR (WDMSFL,DSN,SAIND,SALEN,
     O                   RBUFF,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             lat attribute present,
C             write out value as desired
              IPOS = 103
              LEN  = 8
              CALL DECCHR (RBUFF,LEN,I0,LEN,OSTR1(IPOS))
            END IF
C
C           get next attribute for long
            SAIND = 9
            SALEN = 1
            CALL WDBSGR (WDMSFL,DSN,SAIND,SALEN,
     O                   RBUFF,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             lat attribute present,
C             write out value as desired
              IPOS = 112
              LEN  = 8
              CALL DECCHR (RBUFF,LEN,I0,LEN,OSTR1(IPOS))
            END IF
C
C           get start and end dates
            CALL WTFNDT (WDMSFL,DSN,I1,
     O                   DSFREC,SDATE,EDATE,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             have dates, write them out
              IPOS = 123
              CALL INTCHR (SDATE(1),I4,I1,JLEN,OSTR1(IPOS))
              IPOS = 128
              CALL INTCHR (SDATE(2),I2,I1,JLEN,OSTR1(IPOS))
              IPOS = 133
              CALL INTCHR (EDATE(1),I4,I1,JLEN,OSTR1(IPOS))
              IPOS = 138
              CALL INTCHR (EDATE(2),I2,I1,JLEN,OSTR1(IPOS))
            END IF
C
            WRITE (OUTFL,2010) OSTR1
C
          END IF
 10     CONTINUE
C
        CALL WDFLCL (WDMSFL,RETCOD)
      END IF
      CLOSE(UNIT=INPFL)
      CLOSE(UNIT=OUTFL)
C
      STOP
      END
