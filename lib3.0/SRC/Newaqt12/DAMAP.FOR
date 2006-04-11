C
C
C
      SUBROUTINE   STALCL
     I                   (STAFL,STID,
     O                    LAT,LNG)
C
C     + + + PURPOSE + + +
C     determine location of a station - lat/long
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STAFL,STID
      REAL      LAT,LNG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STAFL  - unit number of file containing station details
C     STID   - station id
C     LAT    - latitude of station
C     LNG    - longitude of station
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmapdt.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   STAREA
C
C     + + + END SPECIFICATIONS + + +
C
C     read info about this station
      CALL STAREA (STAFL,STID,STASZE,
     O             STAREC)
C
      LAT= STLAT
      LNG= STLNG
C
      RETURN
      END
C
C
C
      SUBROUTINE   STALCA
     I                   (STAFL,STID,
     O                    ALBX,ALBY)
C
C     + + + PURPOSE + + +
C     determine location of a station - albers
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STAFL,STID,ALBX,ALBY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STAFL  - unit number of file containing station details
C     STID   - station id
C     ALBX   - albers X coordinate
C     ALBY   - albers Y coordinate
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmapdt.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   STAREA
C
C     + + + END SPECIFICATIONS + + +
C
C     read info about this station
      CALL STAREA (STAFL,STID,STASZE,
     O             STAREC)
C
      ALBX= STALBX
      ALBY= STALBY
C
      RETURN
      END
C
C
C
      SUBROUTINE   STATTR
     I                   (STAFL,STID,ATCNT,ATID,
     O                    ATVAL,ATRATI)
C
C     + + + PURPOSE + + +
C     determine real type attributes of a station
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STAFL,STID,ATCNT,ATID(ATCNT)
      REAL      ATVAL(ATCNT),ATRATI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STAFL  - unit number of file containing station details
C     STID   - station id
C     ATCNT  - count of real attributes
C     ATID   - ids of real attributes
C     ATVAL  - values of real attributes
C     ATRATI - ratio of first 2 attributes
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmapdt.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   STAREA
C
C     + + + END SPECIFICATIONS + + +
C
C     save desired attributes
      STATID(1)= ATID(1)
      STATID(2)= ATID(2)
C
C     read info about this station
      CALL STAREA (STAFL,STID,STASZE,
     M             STAREC)
C
      ATVAL(1)= STATVL(1)
      ATVAL(2)= STATVL(2)
      ATRATI  = STRATI
C
      RETURN
      END
C
C
C
      SUBROUTINE   STAPRT
     I                   (EXPFL,DSPFG,STAFL,IREC,BRSFG,
     I                    XYOUT,TYMIN,TXMIN,
     I                    POPCNT,OPTID,OPTNAM,IANS,LDEVCD,
     M                    HANCNT,MKID,MKX,MKY)
C
C     routine to output a station summary to output device
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     POPCNT,OPTID(POPCNT),IANS,HANCNT,
     $            LDEVCD,EXPFL,DSPFG,STAFL,IREC,BRSFG
      REAL        XYOUT,TYMIN,TXMIN,MKID(*),MKX(*),MKY(*)
      CHARACTER*5 OPTNAM(POPCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXPFL  - file unit to write summary to, 0 - dont write
C     DSPFG  - output to screen flag: 1 - separate window, 0 - no
C     STAFL  - unit number of file containing station details
C     IREC   - station id code
C     BRSFG  - browse flag: 1-go to browse screen from map click
C     XYOUT  -
C     TYMIN  - y min pos(WC)
C     TXMIN  - x min pos(WC)
C     POPCNT - number of active options
C     OPTID  - array of option ids
C     OPTNAM - array of option names
C     IANS   - current option, zero to erase options
C     HANCNT - count of handles in use
C     MKID   - marker ID
C     MKX    - marker X coord
C     MKY    - marker Y coord
C     LDEVCD - device code
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmapdt.inc'
      INCLUDE 'cstaid.inc'
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxsen.inc'
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIALBES + + +
      INTEGER      I,LEN,I0,I1,I78,IM1,DONFG,L,
     $             NMATCH,ACT,SCLU,SGRP
      CHARACTER*1  BK(1)
      CHARACTER*8  CONNAM(10),SCENNM(10),SCEN,LOC,CONS,
     1             TSCEN,TCONS,PTHNAM(1)
      CHARACTER*20 SCNAME
      CHARACTER*48 WNDNAM
      CHARACTER*78 OSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (OSTR,OBUFF)
      CHARACTER*1  OBUFF(78)
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNAL + + +
      EXTERNAL     LENSTR, STAREA, ZBLDWR, TSIDEN, ZQUIET, GEDOPT
      EXTERNAL     SGSCBF, CLGET, CLPUT, CLFDSN, ZWNSET, PMXCNW
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' Dsn      :',I9)
 2005 FORMAT (' Station  :',I9,6X,12A4)
 2010 FORMAT (' Hucode   :',I9)
 2030 FORMAT (' First Yr.:',I9,      '   Last Yr.  : ',I9,
     1       '   # Years  :',I9)
 2080 FORMAT (////)
 2090 FORMAT (' BAD RECORD FOR STATION NUMBER',I5)
 2100 FORMAT (1X,78A1)
 2105 FORMAT (' ID: ',A8,'    Name: ',A40)
 2106 FORMAT (' ID: ',A8)
 2110 FORMAT (' Constituent     Scenario ')
 2120 FORMAT (1X,A8,8X,A8)
C 
C     + + + END SPECIFICATIONS + + + 
C
      I0   = 0
      I1   = 1
      I78  = 78
      IM1  = -1
      BK(1)= ' '
C
      IF (STAFL.NE.-1) THEN
C       station summaries the old way, used in early expert system,
C       read the station parms from the data base
        CALL STAREA (STAFL,IREC,STASZE,
     O               STAREC)
        IF (STAREC(1).GT.0) THEN
C         valid station, write out its details
          WRITE(OSTR,2000) IREC
          LEN= 78
          IF (DSPFG.GE.1) THEN
C           screen, initialize view
            SCNAME= 'Identify (MI)'
            CALL ZWNSET (SCNAME)
            CALL ZBLDWR (LEN,OBUFF,I1,IM1,
     O                   DONFG)
          END IF
          IF (EXPFL.NE.0) THEN
C           write to file
            WRITE (EXPFL,2100) (OBUFF(I),I=1,LENSTR(I78,OBUFF))
          END IF
C
          WRITE(OSTR,2005) STISTA,(STINAM(I),I=1,12)
          IF (DSPFG.GE.1) THEN
            CALL ZBLDWR (LEN,OBUFF,I0,IM1,
     O                   DONFG)
C           blank record
            CALL ZBLDWR (I1,BK,I0,IM1,
     O                   DONFG)
          END IF
          IF (EXPFL.GT.0) THEN
            WRITE (EXPFL,2100) (OBUFF(I),I=1,LENSTR(I78,OBUFF))
C           blank record
            WRITE (EXPFL,2100) BK
          END IF
C
          WRITE(OSTR,2010) STHUC
          IF (DSPFG.GE.1) THEN
            CALL ZBLDWR (LEN,OBUFF,I0,IM1,
     O                   DONFG)
          END IF
          IF (EXPFL.NE.0) THEN
            WRITE (EXPFL,2100) (OBUFF(I),I=1,LENSTR(I78,OBUFF))
          END IF
C
          WRITE(OSTR,2030) STSYR,STEYR,STCNT
          IF (DSPFG.GE.1) THEN
            CALL ZBLDWR (LEN,OBUFF,I0,IM1,
     O                   DONFG)
C           blank record
            CALL ZBLDWR (I1,BK,I0,IM1,
     O                   DONFG)
          END IF
          IF (EXPFL.NE.0) THEN
            WRITE (EXPFL,2100) (OBUFF(I),I=1,LENSTR(I78,OBUFF))
C           blank record
            WRITE (EXPFL,2100) BK
          END IF
C
          IF (DSPFG.GE.1) THEN
C           write the whole thing
            IF (DSPFG .EQ. 1) THEN
C             wait for user
              I= I0
            ELSE
C             dont wait
              I= I1
            END IF
            CALL ZBLDWR (I0,BK,I0,I,
     O                   DONFG)
          END IF
          IF (EXPFL.NE.0) THEN
C           blank lines between dsns
            WRITE(EXPFL,2080)
          END IF
        ELSE
C         bad record check on
          WRITE (OSTR,2090) IREC
          LEN= LENSTR(I78,OBUFF)
          IF (DSPFG.GE.1) THEN
            CALL ZBLDWR (LEN,OBUFF,I1,I0,
     O                   DONFG)
          END IF
          IF (EXPFL.NE.0) THEN
            WRITE (EXPFL,2100) (OBUFF(I),I=1,LEN)
          END IF
        END IF
C
      ELSE IF (STAFL.EQ.-1) THEN
C       scheme implemented for sen gen
        IF (BRSFG.EQ.0) THEN
C         old way of just identifying with a click
C         find scenarios and constituents at this location
          CALL TSIDEN (LOCID(IREC),
     O                 NMATCH,CONNAM,SCENNM)
          WRITE(99,*) 'STANAM,IREC',STANAM(IREC),IREC
C         build location id, name line
          IF (STANAM(IREC).NE.' ') THEN
C           station name exists
            WRITE(OSTR,2105) LOCID(IREC),STANAM(IREC)
          ELSE
C           station name does not exist
            WRITE(OSTR,2106) LOCID(IREC)
          END IF
          LEN= LENSTR(I78,OBUFF)
          IF (DSPFG.GE.1) THEN
C           screen, initialize view
            SCNAME= 'Identify (MI)'
            CALL ZWNSET (SCNAME)
            CALL ZBLDWR (I78,OBUFF,I1,IM1,
     O                   DONFG)
          END IF
          IF (EXPFL.NE.0) THEN
C           write to file
            WRITE (EXPFL,2100) (OBUFF(I),I=1,LEN)
          END IF
C
C         blank line
          IF (DSPFG.GE.1) THEN
            CALL ZBLDWR (I1,BK,I0,IM1,
     O                   DONFG)
          END IF
          IF (EXPFL.GT.0) THEN
            WRITE (EXPFL,2100) BK
          END IF
C
C         build constituent/scenario table
C         build table header
          WRITE(OSTR,2110)
          IF (DSPFG.GE.1) THEN
            CALL ZBLDWR (I78,OBUFF,I0,IM1,
     O                   DONFG)
          END IF
          IF (EXPFL.GT.0) THEN
            WRITE (EXPFL,2100) (OBUFF(I),I=1,LENSTR(I78,OBUFF))
          END IF
C
C         search for scenario/constituent pairs
          DO 100 L=1,NMATCH
C           build line of table
            WRITE(OSTR,2120) CONNAM(L),SCENNM(L)
            IF (DSPFG.GE.1) THEN
              CALL ZBLDWR (I78,OBUFF,I0,IM1,
     O                   DONFG)
            END IF
            IF (EXPFL.GT.0) THEN
              WRITE (EXPFL,2100) (OBUFF(I),I=1,LENSTR(I78,OBUFF))
            END IF
 100      CONTINUE
C         we are done
          IF (DSPFG .GE. 1) THEN
C           aide screen instructions
            IF (DSPFG .EQ. 1) THEN
C             wait for user
              I= I0
            ELSE
C             dont wait
              I= I1
            END IF
            CALL ZBLDWR (I0,BK,I0,I,
     O                   DONFG)
          END IF
        ELSE IF (BRSFG.EQ.1) THEN
C         do browse screen to select available data sets
          IF (DSPFG .GE. 1) THEN
C           do aide screen
C           remove option identifiers
            CALL GEDOPT (I1,XYOUT,TYMIN,TXMIN,
     I                   POPCNT,OPTID,OPTNAM,I0,LDEVCD,
     M                   HANCNT,MKID,MKX,MKY)
C           set scenario, loc, cons for temp class
            CALL CLGET (I1,
     O                  SCEN,LOC,CONS,ACT)
            TSCEN = SCEN
            TCONS = CONS
            SCEN  = ' '
            CONS  = ' '
            CALL CLPUT (I1,SCEN,LOCID(IREC),CONS,I1)
            CALL CLFDSN(I1)
C           do browse screen
            PTHNAM(1) = ' '
            SCLU      = 63
            CALL SGSCBF (MESSFL,SCLU,I1,PTHNAM)
C           put original class back
            CALL CLPUT (I1,TSCEN,LOC,TCONS,ACT)
            CALL CLFDSN(I1)
C           put message back on screen to use map window
            SCLU= 52
            SGRP= 66
            WNDNAM= 'Draw (MD) Status'
            CALL ZWNSET (WNDNAM)
            CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
            CALL ZQUIET
C           display option identifiers
            CALL GEDOPT (I1,XYOUT,TYMIN,TXMIN,
     I                   POPCNT,OPTID,OPTNAM,IANS,LDEVCD,
     M                   HANCNT,MKID,MKX,MKY)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STAREA
     I                   (STAFL,STANUM,STASZE,
     O                    STAREC)
C
C     + + + PURPOSE + + +
C     determine details about a station from a WDM dataset label
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STAFL,STANUM,STASZE,STAREC(STASZE)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STAFL  - unit number of file containing station details
C     STANUM - station id
C     STASZE - size of station buffer
C     STAREC - station details record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,IX,IY,IA,STDAT(6),ENDAT(6),RETCOD,
     1            GPFLG,DREC,SAIND,I1,IATT(1),ALBFG
      REAL        RTMP,RVAL(1)
      REAL*8      LX,LY,AX,AY
      CHARACTER*1 STANM1(48)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (IX,RX),(IY,RY),(IA,RA),(STANM1,STANAM)
      REAL         RX,RY,RA
      CHARACTER*48 STANAM
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTFNDT,WDBSGI,WDBSGR,WDBSGC,CLLAL1
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD,ABS,FLOAT
C
C     + + + INPUT FORMATS + + +
1000  FORMAT(12A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
C     dsn
      STAREC(1)= STANUM
C
C     start and end dates
      GPFLG= 1
      CALL WTFNDT (STAFL,STANUM,GPFLG,
     O             DREC,STDAT,ENDAT,RETCOD)
C
      STAREC(2)= STDAT(1)
      STAREC(3)= ENDAT(1)
C     get attribute information for location
C     Y first, try albers coord
C     SAIND= 221
C     CALL WDBSGR (STAFL,STANUM,SAIND,I1,
C    O             RVAL,RETCOD)
C     IF (RETCOD .EQ. 0) THEN
C       save y
C       STAREC(STASZE)= RVAL(1)
C       albers X
C       SAIND= 220
C       CALL WDBSGR (STAFL,STANUM,SAIND,I1,
C    O               RVAL,RETCOD)
C     END IF
C     use lat long, not albers coord
      RETCOD= 1
      IF (RETCOD .EQ. 0) THEN
C       save x
        STAREC(STASZE-1)= RVAL(1)
        ALBFG    = 1
        STAREC(4)= -999.
        STAREC(5)= -999.
      ELSE IF (RETCOD .NE. 0) THEN
        ALBFG= 0
C       derive lat (dms)
        SAIND= 54
        CALL WDBSGI (STAFL,STANUM,SAIND,I1,
     O               IATT,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         attribute available
          I   = IATT(1)
C         seconds
          J   = MOD(I,100)
          RTMP= FLOAT(J)/3600.
          I   = (I- J)/ 100
C         minutes and seconds
          J   = MOD(I,100)
          RTMP= RTMP+ (FLOAT(J)/60.)
          I   = (I- J)/ 100
          RX  = I+ RTMP
        ELSE
C         try decimal version
          SAIND= 8
          CALL WDBSGR (STAFL,STANUM,SAIND,I1,
     O                 RX,RETCOD)
        END IF
        STAREC(4)= IX
C       lng (dms)
        SAIND= 55
        CALL WDBSGI (STAFL,STANUM,SAIND,I1,
     O               IATT,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         attribute available
          I   = IATT(1)
C         seconds
          J   = MOD(I,100)
          RTMP= FLOAT(J)/3600.
          I   = (I- J)/ 100
C         minutes and seconds
          J   = MOD(I,100)
          RTMP= RTMP+ (FLOAT(J)/60.)
          I   = (I- J)/ 100
          RX  = I+ RTMP
        ELSE
C         try decimal version
          SAIND= 9
          CALL WDBSGR (STAFL,STANUM,SAIND,I1,
     O                 RX,RETCOD)
        END IF
        STAREC(5)= IX
      END IF
C
C     integer station id
      SAIND= 51
      CALL WDBSGI (STAFL,STANUM,SAIND,I1,
     O             IATT,RETCOD)
      STAREC(6)= IATT(1)
C
C     huc
      SAIND= 4
      CALL WDBSGI (STAFL,STANUM,SAIND,I1,
     O             IATT,RETCOD)
      STAREC(7)= IATT(1)
C
C     station name
      SAIND= 45
      I    = 48
      CALL WDBSGC (STAFL,STANUM,SAIND,I,
     1             STANM1,RETCOD)
      READ(STANAM,1000) (STAREC(I),I=8,17)
C
C     user spec attributes
      DO 10 I= 1,2
        SAIND= STAREC(19+I)
        IF (SAIND .GT. 0) THEN
C         need this attribute
          CALL WDBSGR (STAFL,STANUM,SAIND,I1,
     1                 RVAL,RETCOD)
          RA= RVAL(1)
          STAREC(21+I)= IA
        ELSE
C         dont need it
          RA= 0.0
          STAREC(21+I)= IA
        END IF
 10   CONTINUE
C
C     ratio of attributes
      IX= STAREC(22)
      IY= STAREC(23)
      IF (ABS(RY) .GT. 1.0E-6) THEN
C       ratio ok
        RA= RX/RY
      ELSE
C       no ratio available
        RA= 0.0
      END IF
C     save ratio
      STAREC(24)= IA
C
C     length of record
      STAREC(STASZE-2)= ENDAT(1)- STDAT(1)+ 1
C
      IF (ALBFG .EQ. 0) THEN
C       add albers coordinate calcs here
        IY= STAREC(5)
        IX= STAREC(4)
        LY= -RY
        LX= RX
        CALL CLLAL1 (LX,LY,
     O               AX,AY)
C       x
        STAREC(STASZE-1)= AX
C       y
        STAREC(STASZE)  = AY
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MAPSPA
     I                   (MAPBRD,NSTA,WDMSFL,STAID,
     M                    RLAT,RLNG,
     O                    SXMIN,SXMAX,SYMIN,SYMAX)
C
C     + + + PURPOSE + + +
C     calculate the space for map based on specified border type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAPBRD,NSTA,WDMSFL,STAID(NSTA)
      REAL      RLAT(2),RLNG(2),SXMIN,SXMAX,SYMIN,SYMAX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MAPBRD - map border type
C              1-USA,2-latlng,3-hydro region,4-state,5-local
C     NSTA   - number of stations found
C     WDMSFL - unit number of file containing stations with attr to map
C     STAID  - id's of stations selected
C     RLAT   - border latitude array - min, max
C     RLNG   - border longitude array - min, max
C     SXMIN  - x min - albers meters
C     SXMAX  - x max - albers meters
C     SYMIN  - y min - albers meters
C     SYMAX  - y max - albers meters
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      REAL      LAT,LNG,RTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL  CLLABX,STALCL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MAPBRD .EQ. 1) THEN
C       whole usa
        SXMIN= -2500000.
        SXMAX=  2500000.
        SYMIN=  0.
        SYMAX=  3300000.
      ELSE IF (MAPBRD .EQ. 2) THEN
C       lat, long: calc albers ranges
        CALL CLLABX(RLAT,RLNG,
     O              SXMIN,SXMAX,SYMIN,SYMAX)
      ELSE IF (MAPBRD .EQ. 3) THEN
C       region: calc albers ranges
        CALL CLLABX(RLAT,RLNG,
     O              SXMIN,SXMAX,SYMIN,SYMAX)
      ELSE IF (MAPBRD .EQ. 4) THEN
C       state (not implemented)
C       whole usa
        SXMIN= -2500000.
        SXMAX=  2500000.
        SYMIN=  0.
        SYMAX=  3300000.
      ELSE IF (MAPBRD .EQ. 5) THEN
C       area around stations
        RLAT(1)= 90.0
        RLAT(2)= 0.0
        RLNG(1)= 180.0
        RLNG(2)= 60.
        DO 20 I= 1,NSTA
C         get lat and lng of station
          CALL STALCL(WDMSFL,STAID(I),
     O                LAT,LNG)
          IF (LAT .GE. -90.0) THEN
            IF (LAT .GT. RLAT(2)) THEN
C             new max lat
              RLAT(2)= LAT
            END IF
            IF (LAT .LT. RLAT(1)) THEN
C             new min lat
              RLAT(1)= LAT
            END IF
            IF (LNG .GT. RLNG(2)) THEN
C             new max long
              RLNG(2)= LNG
            END IF
            IF (LNG .LT. RLNG(1)) THEN
C             new min long
              RLNG(1)= LNG
            END IF
          END IF
 20     CONTINUE
C       need border for map
        RTMP= (RLAT(2)- RLAT(1))/10.
        RLAT(1) = RLAT(1)- 2*RTMP
        RLAT(2) = RLAT(2)+ RTMP
        RTMP= (RLNG(2)- RLNG(1))/10.
        RLNG(1) = RLNG(1)- RTMP
        RLNG(2) = RLNG(2)+ RTMP
        CALL CLLABX(RLAT,RLNG,
     O              SXMIN,SXMAX,SYMIN,SYMAX)
      END IF
C
      WRITE(99,*) 'MAPSPA:MAPBRD,RLAT,RLNG:',MAPBRD,RLAT,RLNG
C
      RETURN
      END
C
C
C
      SUBROUTINE   STACOM
     I                   (NLOC,LCNAME,CLOCID)
C
C     + + + PURPOSE + + +
C     put location info into common for use building sta summaries
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NLOC
      CHARACTER*40  LCNAME(NLOC)
      CHARACTER*8   CLOCID(NLOC)
C      INTEGER       NSEN,NCON,CONID(NCON)
C      CHARACTER*16  CONNAM(NCON)
C      CHARACTER*10  SENNAM(NSEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NLOC   - number of map locations
C     LCNAME - array of location names for map stations
C     CLOCID - array of short location names
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cstaid.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C      XNSEN = NSEN
C      XNCON = NCON
C
      DO 10 I = 1,NLOC
        STANAM(I)= LCNAME(I)
        LOCID(I) = CLOCID(I)
 10   CONTINUE
C
C      DO 20 I = 1,NSEN
C        XSENNM(I) = SENNAM(I)
C 20   CONTINUE
C
C      DO 40 I = 1,NCON
C        XCONNM(I) = CONNAM(I)
C        XCONID(I) = CONID(I)
C 40   CONTINUE
C
      RETURN
      END
