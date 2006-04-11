C
C
C
      SUBROUTINE   UMAKPR
     I                   (CSCEN,
     I                    OUTLEV,RESMFG,RUNFG,SPOUT,UNIT,
     I                    SDATIM,EDATIM,RNINFO,EXPFG,
     I                    WDMNAM,WDMSFL,UFTAB,FILES,RDBFG,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     supervisor subroutine to prepare to create a uci file
C     from gis output files
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       RETCOD,
     1              OUTLEV,RESMFG,RUNFG,SPOUT,UNIT,SDATIM(5),EDATIM(5),
     2              EXPFG,WDMSFL,UFTAB,FILES(5),RDBFG(5)
      CHARACTER*8   CSCEN
      CHARACTER*64  WDMNAM
      CHARACTER*80  RNINFO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CSCEN  - scenario name
C     RETCOD - return code from uci creation  1 - misordered reaches
C     RDBFG  - files are rdb format flag
C     OUTLEV - hspf output level
C     RESMFG - resume flag
C     RUNFG  - run flag
C     SPOUT  - special actions output level
C     UNIT   - unit system
C     SDATIM - start date
C     EDATIM - end date
C     RNINFO - run info
C     WDMNAM - name of wdm file
C     WDMSFL - wdm file unit number
C     UFTAB  - ftable rating curves file unit number
C     EXPFG  - expert system flag
C     FILES  - array of file unit numbers
C              1 - reach connection table
C              2 - reach table name
C              3 - land use table
C              4 - area table
C              5 - met station table
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       LEXPFG
C
C     + + + EXTERNALS + + +
      EXTERNAL      GISREA,UCIDSN,SETUCI
      EXTERNAL      SETRAT,CUAREA,CHKRCH,UCRINI,UCSDSN
C
C     + + + END SPECIFICATIONS + + +
C
      LEXPFG = EXPFG
      IF (FILES(3).GT.0 .AND. FILES(5).GT.0) THEN
C       have all files for doing expert system calibration
        LEXPFG = 1
      END IF
C
C     initialize gis parameters in common
      CALL UCRINI
C
C     set uci info in common
      CALL SETUCI (OUTLEV,RESMFG,RUNFG,SPOUT,UNIT,
     I             SDATIM,EDATIM,RNINFO,WDMSFL,CSCEN,WDMNAM)
C
C     set rating curves info in common
      CALL SETRAT (UFTAB)
C
C     read gis input files into common
      CALL GISREA (FILES,RDBFG,LEXPFG,
     O             RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       check for proper reach ordering
        CALL CHKRCH (RETCOD)
      END IF

      IF (RETCOD.EQ.0) THEN
C       calculate cumulative areas
        CALL CUAREA
C
C       process observed data sets, looking for ext sources and targets
        CALL UCIDSN
C
C       look for existing simulated data sets
        CALL UCSDSN
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMAKDO
     I                   (MESSFL,MESSU,OUTFL,EXPFG)
C
C     + + + PURPOSE + + +
C     supervisor subroutine to create a uci file from gis output files
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,MESSU,OUTFL,EXPFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     MESSU  - existing error file unit number
C     OUTFL  - output file unit number
C     EXPFG  - expert system flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0
C
C     + + + EXTERNALS + + +
      EXTERNAL      UCIMAK,UCIWRT,EXSMAK,MAKDSN
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C
C     create simulated data sets where needed
      CALL MAKDSN (MESSFL,EXPFG)
C
C     perform creation
      CALL UCIMAK (MESSFL,MESSU,EXPFG)
C
C     write out uci file
      CALL UCIWRT (OUTFL,I0)
C
      IF (EXPFG.EQ.1) THEN
C       create exs file
        CALL EXSMAK
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GISSIM
     I                    (XORD,XPERC,XSMALL,
     I                     FUNRCH,FUNARE,FUNCON,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     supervisor subroutine to simplify the gis reach network
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       RETCOD,XORD,FUNRCH,FUNARE,FUNCON
      REAL          XPERC,XSMALL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XORD   - number of upstream segments which defines a segment rank
C              significant enough to keep, suggested value is 5.
C     XPERC  - critical percentage of drainage area of the entire basin
C              at which a new reach is to be created, suggested 0.2
C     XSMALL - critical percentage of drainage area of the entire basin,
C              if less than this value a reach is included in the downstream
C              reach, suggested 0.02
C     FUNRCH - file unit number for new reach file
C     FUNARE - file unit number for new area file
C     FUNCON - file unit number for new connection file
C     RETCOD - return code from simnet routine
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'pmxts.inc'
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       XDOWN(MXRCH),XNCON1,I,J,K,XAREA(MXRCH),
     1              DBGLEV,NR,NEWID(MXRCH),RDN,
     2              XRCONN(MXRCH,2),RANK(MXRCH),XUP(MXRCH)
      REAL          XLAREA(MXRCH),XTAREA(MXRCH),XLAA(MXLAND,MXRCH),
     1              OTAA(MXLAND,MXRCH),XTAA(MXLAND,MXRCH)
      CHARACTER*12  XNAME(MXRCH)
C
C     + + + EXTERNALS + + +
      EXTERNAL      SIMNET,GISWRT
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate cumulative areas and reduce stream network
      DBGLEV = 5
      CALL SIMNET (MXRCH,MXLAND,NLANDU,DBGLEV,XORD,XPERC,XSMALL,
     I             AREALR,NREACH,NRCONN,R2RCON,CRCH,
     O             LAREA,UPAREA,
     O             NR,XNCON1,XRCONN,XNAME,
     O             XLAREA,XTAREA,XLAA,XTAA,OTAA,
     O             NEWID,XDOWN,XUP,RANK,RETCOD)
      OPEN(UNIT=90,FILE='revrch.txt')
C
C     find downstream segment associated with each segment
C     of original network
      DO 20 I = 1,NREACH
        XAREA(I) = 0
        J = I
 10     CONTINUE
          IF (NEWID(J).GT.0) THEN
            XAREA(I) = NEWID(J)
          ELSE
            RDN = 0
            K = 1
 15         CONTINUE
              IF (J.EQ.R2RCON(K,1)) THEN
                RDN = R2RCON(K,2)
              ELSE
                K = K + 1
              END IF
            IF (RDN.EQ.0) GO TO 15
            J = RDN
          END IF
        IF (XAREA(I).EQ.0) GO TO 10
 20   CONTINUE
C
      DO 990 I = 1,NREACH
        IF (NEWID(I).LT.1) THEN
          WRITE(90,'(1X,A12,A1,11X,A12)') CRCH(I),'0',
     1                                 XNAME(XAREA(I))
        ELSE
          WRITE(90,'(1X,A12,A12,A12)') CRCH(I),XNAME(NEWID(I)),
     1                                 XNAME(XAREA(I))
        END IF
 990  CONTINUE
      IF (RETCOD.EQ.0) THEN
C       output new version of tables
        CALL GISWRT (MXRCH,MXLAND,NEWID,NR,NLANDU,XLAA,
     I               XNAME,XDOWN,XUP,XNCON1,XRCONN,
     I               FUNRCH,FUNARE,FUNCON)
      END IF
      CLOSE(UNIT=90)
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCIDSN
C
C     + + + PURPOSE + + +
C     process observed data set numbers for new uci file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'pmxts.inc'
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,SAIND,SALEN,ITEMP,DSN,DSTYPE,
     1              RETCOD,OUTCNT,I4,I5,
     2              STRT(6),STOP(6),TSTEP,TCODE,FLAG,SDATE(6),EDATE(6)
      CHARACTER*1   CTMP1(8)
      CHARACTER*8   CTMP8
C
C     + + + FUNCTIONS + + +
      INTEGER     WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDCKDT,WDBSGC,ZIPI,WDBSAC,CVARAR,WDDSCK,WDDSCL,CARVAR
      EXTERNAL    WDATIM, CKDATE, COPYI, TSDSGN
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (8A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I4 = 4
      I5 = 5
C
C     initialize array of output sites to -999
      DO 10 I = 1,NREACH
        OUTSIT(I) = -999
 10   CONTINUE
C
C     scan data sets
      DO 100 DSN= 1,MXTS
C       is this a timeseries dsn
        DSTYPE= WDCKDT(WDMSFL,DSN)
        IF (DSTYPE .EQ. 1) THEN
C         it is, count it
C         get attribute for constituent name
          SAIND = 289
          SALEN = 8
          CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                 CTMP1,RETCOD)
          IF (RETCOD.EQ.0) THEN
C           put constituent in temp variable
            WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
            IF (CTMP8.EQ.'PREC' .OR. CTMP8.EQ.'EVAP') THEN
C             found a met data set
              IF (CTMP8.EQ.'PREC') THEN
                ITEMP = 1
              ELSE
                ITEMP = 2
              END IF
C             is it observed?
              SAIND= 288
              SALEN= 8
              CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                     CTMP1,RETCOD)
              IF (RETCOD.EQ.0) THEN
C               save scenario name
                WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
                IF (CTMP8.EQ.'OBSERVED') THEN
C                 it is observed, get location
                  SAIND= 290
                  SALEN= 8
                  CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                         CTMP1,RETCOD)
                  IF (RETCOD.EQ.0) THEN
C                   save location name
                    WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
C                   now look through met segments
                    DO 110 I = 1,NMETSG+NEVAP
                      IF (PRCEVP(I).EQ.ITEMP .AND.
     1                    CMETS(I).EQ.CTMP8) THEN
C                       this is the one, save it
                        METDSN(I) = DSN
C                       save the tstype for this data set
                        SAIND = 1
                        SALEN = 4
                        CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                               CTMP1,RETCOD)
                        IF (RETCOD.EQ.0) THEN
                          CALL CARVAR (I4,CTMP1,I4,CTSTYP(I))
                        ELSE
                          CTSTYP(I) = ' '
                        END IF
                      END IF
 110                CONTINUE
                  END IF
                END IF
              END IF
            ELSE IF (CTMP8.EQ.'FLOW') THEN
C             found a flow data set
C             get scenario name
              SAIND= 288
              SALEN= 8
              CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                     CTMP1,RETCOD)
              IF (RETCOD.EQ.0) THEN
C               save scenario name
                WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
                IF (CTMP8.EQ.'OBSERVED') THEN
C                 observed flow here, get location
                  SAIND= 290
                  SALEN= 8
                  CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                         CTMP1,RETCOD)
                  IF (RETCOD.EQ.0) THEN
C                   save location name
                    WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
C                   now look through reach names
                    DO 120 I = 1,NREACH
                      IF (CRCH(I).EQ.CTMP8) THEN
C                       this is an output site, save it
                        OUTSIT(I) = DSN
                      END IF
 120                CONTINUE
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
 100  CONTINUE
C
C     now fill arrays of dsns for each output site
C     do we have any output sites?
      OUTCNT = 0
      NDSNIN = 0
      DO 20 I = 1,NREACH
        IF (OUTSIT(I).NE.-999) THEN
          OUTCNT = OUTCNT + 1
C         check to see if these sites can be used for local inflows
          CALL WDATIM (WDMSFL,OUTSIT(I),
     O                 STRT,STOP,TSTEP,TCODE,RETCOD)
          IF (RETCOD.EQ.0) THEN
            CALL COPYI (I5,SDATIM,SDATE)
            SDATE(6) = 0
            CALL CKDATE (STRT,SDATE,
     O                   FLAG)
            IF (FLAG.LE.0) THEN
C             start of run is after start of data, ok
              CALL COPYI (I5,EDATIM,EDATE)
              EDATE(6) = 0
              CALL CKDATE (EDATE,STOP,
     O                     FLAG)
              IF (FLAG.LE.0) THEN
C               end of run is before end of data, ok
                NDSNIN = NDSNIN + 1
                DSNIN(NDSNIN) = OUTSIT(I)
                AREAIN(NDSNIN)= UPAREA(I)
              END IF
            END IF
          END IF
        END IF
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCSDSN
C
C     + + + PURPOSE + + +
C     process simulated data set numbers for new uci file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'pmxts.inc'
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,SAIND,SALEN,DSN,
     1              RETCOD,OUTCNT,I0
      CHARACTER*1   CTMP1(8)
      CHARACTER*8   CTMP8
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDBSGC,ZIPI,WDBSAC,CVARAR,WDDSCK,WDDSCL,CARVAR
      EXTERNAL    WDATIM, CKDATE, COPYI, TSDSGN
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (8A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C
C     initialize simulated output dsn array
      CALL ZIPI (MXRCH*10,I0,RCHDSN)
C
      OUTCNT = 0
      DO 21 I = 1,NREACH
        IF (OUTSIT(I).NE.-999) THEN
          OUTCNT = OUTCNT + 1
        END IF
 21   CONTINUE
      IF (OUTCNT.GT.0) THEN
C       look for output data sets
        DO 200 DSN= 1,MXTS
C         get scenario name
          SAIND= 288
          SALEN= 8
          CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                 CTMP1,RETCOD)
          IF (RETCOD.EQ.0) THEN
C           save scenario name
            WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
            IF (CTMP8.EQ.CSCEN) THEN
C             output data set here, get location
              SAIND= 290
              SALEN= 8
              CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                     CTMP1,RETCOD)
              IF (RETCOD.EQ.0) THEN
C               save location name
                WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
C               now look through list of output sites
                DO 130 I = 1,NREACH
                  IF (OUTSIT(I).NE.-999 .AND. CRCH(I).EQ.CTMP8) THEN
C                   this is an output site, get constituent
                    SAIND = 289
                    SALEN = 8
                    CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                           CTMP1,RETCOD)
                    IF (RETCOD.EQ.0) THEN
C                     put constituent in temp variable
                      WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
                      IF (CTMP8.EQ.'FLOW    ') THEN
                        RCHDSN(I,1) = DSN
                      ELSE IF (CTMP8.EQ.'SIMQ    ') THEN
                        RCHDSN(I,2) = DSN
                      ELSE IF (CTMP8.EQ.'SURO    ') THEN
                        RCHDSN(I,3) = DSN
                      ELSE IF (CTMP8.EQ.'IFWO    ') THEN
                        RCHDSN(I,4) = DSN
                      ELSE IF (CTMP8.EQ.'AGWO    ') THEN
                        RCHDSN(I,5) = DSN
                      ELSE IF (CTMP8.EQ.'PETX    ') THEN
                        RCHDSN(I,6) = DSN
                      ELSE IF (CTMP8.EQ.'SAET    ') THEN
                        RCHDSN(I,7) = DSN
                      ELSE IF (CTMP8.EQ.'UZSX    ') THEN
                        RCHDSN(I,8) = DSN
                      ELSE IF (CTMP8.EQ.'LZSX    ') THEN
                        RCHDSN(I,9) = DSN
                      END IF
                    END IF
                  END IF
 130            CONTINUE
              END IF
            END IF
          END IF
 200    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MAKDSN
     I                    (MESSFL,EXPFG)
C
C     + + + PURPOSE + + +
C     make data sets for new uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,EXPFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     EXPFG  - expert system flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,SAIND,SALEN,NDSN,NCNT,
     1              RETCOD,I0,NFREC
      CHARACTER*1   OSTR1(8)
      CHARACTER*8   OSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDBSAC,CVARAR,WDDSCK,WDDSCL,TSDSGN
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C
C     create data sets where we need them
      DO 300 I = 1,NREACH
        IF (OUTSIT(I).NE.-999) THEN
C         want output here, see if data sets exist
          IF (EXPFG.EQ.1) THEN
C           expert system needs full range of data sets
            NCNT = 9
          ELSE
C           only need one data set for flow
            NCNT = 1
          END IF
          DO 140 J = 1,NCNT
            IF (RCHDSN(I,J).EQ.0) THEN
C             need to create data set here
              NDSN = OUTSIT(I)
 150          CONTINUE
                NDSN = NDSN + 1
                CALL WDDSCK(WDMSFL,NDSN,
     O                      NFREC,RETCOD)
              IF (NFREC.GT.0) GO TO 150
C             found unused data set
              CALL WDDSCL (WDMSFL,OUTSIT(I),WDMSFL,NDSN,I0,
     O                     RETCOD)
              IF (RETCOD.EQ.0) THEN
C               successful creation
                RCHDSN(I,J) = NDSN
C               put attribute for scenario name
                SAIND = 288
                SALEN = 8
                OSTR  = CSCEN
                CALL CVARAR (SALEN,OSTR,SALEN,OSTR1)
                CALL WDBSAC (WDMSFL,NDSN,MESSFL,SAIND,SALEN,OSTR1,
     O                       RETCOD)
C               put next attribute for constituent name
                SAIND = 289
                SALEN = 8
                IF (J.EQ.1) THEN
                  OSTR = 'FLOW    '
                ELSE IF (J.EQ.2) THEN
                  OSTR = 'SIMQ    '
                ELSE IF (J.EQ.3) THEN
                  OSTR = 'SURO    '
                ELSE IF (J.EQ.4) THEN
                  OSTR = 'IFWO    '
                ELSE IF (J.EQ.5) THEN
                  OSTR = 'AGWO    '
                ELSE IF (J.EQ.6) THEN
                  OSTR = 'PETX    '
                ELSE IF (J.EQ.7) THEN
                  OSTR = 'SAET    '
                ELSE IF (J.EQ.8) THEN
                  OSTR = 'UZSX    '
                ELSE IF (J.EQ.9) THEN
                  OSTR = 'LZSX    '
                END IF
                CALL CVARAR (SALEN,OSTR,SALEN,OSTR1)
                CALL WDBSAC (WDMSFL,NDSN,MESSFL,SAIND,SALEN,OSTR1,
     O                       RETCOD)
C               put tstype
                SAIND = 1
                SALEN = 4
                CALL CVARAR (SALEN,OSTR,SALEN,OSTR1)
                CALL WDBSAC (WDMSFL,NDSN,MESSFL,SAIND,SALEN,OSTR1,
     O                       RETCOD)
C               put next attribute for location name
                SAIND = 290
                SALEN = 8
                OSTR  = CRCH(I)
                CALL CVARAR (SALEN,OSTR,SALEN,OSTR1)
                CALL WDBSAC (WDMSFL,NDSN,MESSFL,SAIND,SALEN,OSTR1,
     O                       RETCOD)
C               set in time series directory
                CALL TSDSGN (WDMSFL,NDSN)
              END IF
            END IF
 140      CONTINUE
        END IF
 300  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   CUAREA
C
C     + + + PURPOSE + + +
C     calculate cumulative area upstream of each reach segment
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,RUP,RDN,K,M
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPR
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize variables to zero
      CALL ZIPR(MXRCH,0.0,UPAREA)
      CALL ZIPR(MXRCH,0.0,LAREA)
      CALL ZIPR(MXRCH*MXLAND*MXMET,0.0,UPAREL)
C
C     calculate local and cumulative areas for each reach
      DO 20 J = 1,NREACH
C       do for each reach in original network
        IF (RCHMET(J).NE.0) THEN
          M = RCHMET(J)
        ELSE
          M = 1
        END IF
        DO 10 I = 1,NLANDU
C         sum areas for each land use contributing to each reach
          LAREA(J) = LAREA(J)+ AREALR(I,J)
          UPAREL(I,J,M) = UPAREL(I,J,M)+ AREALR(I,J)
 10     CONTINUE
        UPAREA(J) = UPAREA(J)+ LAREA(J)
        RUP = J
        I   = 1
 15     CONTINUE
C         look through each reach to reach connection
          IF (RUP .EQ. R2RCON(I,1)) THEN
            RDN = R2RCON(I,2)
C           write(*,*) 'rdn,j',rdn,j
            UPAREA(RDN) = UPAREA(RDN)+ LAREA(J)
C            WRITE(99,*) 'ADD AREA OF SEGMENT',J,'TO SEGMENT',RDN
            DO 17 K = 1,NLANDU
              UPAREL(K,RDN,M) = UPAREL(K,RDN,M)+ AREALR(K,J)
 17         CONTINUE
            RUP = RDN
            I   = 1
          ELSE
            I= I + 1
          END IF
        IF (I .LE. NRCONN) GO TO 15
C       been through every reach to reach connection
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKRCH
     O                    (RETCOD)
C
C     + + + PURPOSE + + +
C     see if reaches are numbered sequentially
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETCOD - return code 0-ok, 1-not numbered sequentially
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,I12
      CHARACTER*1   CRCH1(12)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRINT,LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CVARAR,CHRINT,LENSTR
C
C     + + + END SPECIFICATIONS + + +
C
      I12   = 12
      RETCOD= 0
C
      DO 10 J = 1,NREACH
        CALL CVARAR (I12,CRCH(J),I12,CRCH1)
        I = CHRINT(LENSTR(I12,CRCH1),CRCH1)
        IF (I.GT.0 .AND. I.NE.J) THEN
C         problem, this reach is not numbered sequentially
          RETCOD = 1
        END IF
 10   CONTINUE
C
      RETURN
      END
