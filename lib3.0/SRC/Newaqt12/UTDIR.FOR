C
C
C
      SUBROUTINE   TSDRRE
     I                   (WDMSFL,DATOPT,DOSTUS)
C
C     + + + PURPOSE + + +
C     fill in the timeseries directory common block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DATOPT,DOSTUS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - wdm data file unit number
C     DATOPT - look for dates option
C     DOSTUS - do status bar flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      J,DSTYPE,I0,I6,SAIND,SALEN,RETCOD,DSFREC,DSN,
     1             STATUS,BARCNT,JPOS,I,LDSN,LWDMFL
      REAL         RINTVL
      CHARACTER*1  CTMP1(12)
      CHARACTER*8  CTMP8
      CHARACTER*78 TBUF78
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT,INT
C
C     + + + FUNCTIONS + + +
      INTEGER     WDCKDT, ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDCKDT, WDBSGI, WDBSGC, WTFNDT, ZIPI, TSESPC, TSESPA
      EXTERNAL    ZWRSCR, ZLNTXT, WID2UD
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (8A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I6= 6
C
      CTSWDM= WDMSFL
C     no default base dataset
      CTSBDS= 0
C     nothing found
      CTSDSN= 0
C     no partial matches allowed
      CPARFG= 0
C
C     calculate at what intervals to update status bar
      RINTVL = FLOAT(MXTS)/40.0
      IF (RINTVL.LT.2.0 .OR. DOSTUS.EQ.0) THEN
C       no need to do status bar
        STATUS = 0
      ELSE
C       do status bar
        STATUS = 1
      END IF
      BARCNT= 0
      JPOS  = 15
C
      DO 100 DSN= 1,MXTS
C
        IF (STATUS.EQ.1) THEN
C         update status bar
          BARCNT = BARCNT + 1
          IF (BARCNT.GT.INT(RINTVL)) THEN
C           add to status bar this time
            TBUF78 = '.'
            I= 14
            JPOS = JPOS + 1
            CALL ZWRSCR (TBUF78(1:1),I,JPOS)
            BARCNT = 0
          END IF
        END IF
C
C       initialize portion of common
        CTSSAV(DSN)= I0
C       assume no dataset
        CTSAVA(DSN)= -1
        CTSGPS(DSN)= I0
        CTSTS(DSN) = I0
        CTSTU(DSN) = I0
C       set not selected
        CTSSEL(DSN)= 0
        CALL ZIPI(I6,I0,CTSSDT(1,DSN))
        CALL ZIPI(I6,I0,CTSEDT(1,DSN))
        CTSSEN(DSN)= ' '
        CTSLOC(DSN)= ' '
        CTSCON(DSN)= ' '
C       adjust wdm and dsn as needed
        CALL WID2UD (WDMSFL,DSN,
     O               LWDMFL,LDSN)
C       is this a timeseries dsn
        DSTYPE= WDCKDT(LWDMFL,LDSN)
        IF (DSTYPE .EQ. 1) THEN
C         it is, count it
          CTSDSN= CTSDSN+ 1
C         set label available
          CTSAVA(DSN)= 0
C         get time step
          SALEN= 1
          SAIND= 33
          CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                 CTSTS(DSN),RETCOD)
          IF (RETCOD.NE.I0) THEN
C           set time step to default of 1
            CTSTS(DSN)= 1
          END IF
C         get time units
          SAIND= 17
          CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                 CTSTU(DSN),RETCOD)
          IF (RETCOD.NE.I0) THEN
C           set to default of daily time units
            CTSTU(DSN)= 4
          END IF
C         group pointer size
          SAIND = 34
          CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                 CTSGPS(DSN),RETCOD)
          IF (RETCOD.NE.I0) THEN
C           could not get group size, set to default
            CTSGPS(DSN) = 6
          END IF
C
C         get data-set scenario name
          SAIND= 288
          SALEN= 8
          CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                 CTMP1,RETCOD)
          IF (RETCOD.EQ.I0) THEN
C           save scenario name
            WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
            CTSSEN(DSN)= CTMP8
          END IF
C
C         get data-set reach name
          SAIND= 290
          SALEN= 8
          CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                 CTMP1,RETCOD)
          IF (RETCOD.EQ.I0) THEN
C           save reach name
            WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
            CTSLOC(DSN)= CTMP8
          END IF
C
C         get next attribute for constituent name
          SAIND = 289
          SALEN = 8
          CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                 CTMP1,RETCOD)
          IF (RETCOD.EQ.-107 .AND. CTSLOC(DSN).NE.'        ') THEN
C           this attribute not present but location name is,
C           try tstype for constituent
            SAIND = 1
            SALEN = 4
            CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                   CTMP1,RETCOD)
            CTMP1(5) = ' '
            CTMP1(6) = ' '
            CTMP1(7) = ' '
            CTMP1(8) = ' '
          END IF
          IF (RETCOD.EQ.I0) THEN
C           put constituent in temp variable
            WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
            CTSCON(DSN)= CTMP8
          END IF
C
          IF (ZLNTXT(CTSSEN(DSN)).EQ.0) THEN
            CTSSEN(DSN) = '<unk>'
          END IF
          IF (ZLNTXT(CTSLOC(DSN)).EQ.0) THEN
            CTSLOC(DSN) = '<unk>'
          END IF
          IF (ZLNTXT(CTSCON(DSN)).EQ.0) THEN
            CTSCON(DSN) = '<unk>'
          END IF
C         always valid scenario, location, constituent names --> valid label
          CTSAVA(DSN) = 1
C
C         IF (ZLNTXT(CTSSEN(DSN)).GT.0 .AND.
C    $        ZLNTXT(CTSLOC(DSN)).GT.0 .AND.
C    $        ZLNTXT(CTSCON(DSN)).GT.0) THEN
C           valid scenario, location, constituent names --> valid label
C           CTSAVA(DSN) = 1
C         END IF
C
          IF (DATOPT .EQ. 1) THEN
C           get start and end dates for each data set
            J= 1
            CALL WTFNDT (WDMSFL,DSN,J,
     O                   DSFREC,CTSSDT(1,DSN),CTSEDT(1,DSN),RETCOD)
            IF (CTSSDT(1,DSN) .GT. I0) THEN
C             data available
              CTSAVA(DSN)= 3
            ELSE
C             data checked but not available
              CTSAVA(DSN)= 2
            END IF
          END IF
        END IF
100   CONTINUE
C
      write(99,*) 'found ',CTSDSN,' datasets'
C     initialize specs to undefined
      CTMP8= '        '
      CALL TSESPC(CTMP8,CTMP8,CTMP8)
      CALL TSESPA(I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSEBSE
     I                   (TSBDS)
C
C     + + + PURPOSE + + +
C     set base dataset number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TSBDS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TSBDS  - wdm data file base dataset (has generic specs)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CTSBDS= TSBDS
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSPC
     I                   (DSN,
     O                    LSCENM,LRCHNM,LCONNM,
     O                    TU,TS,SDATE,EDATE,GRPSIZ)
C
C     + + + PURPOSE + + +
C     find info about a dataset from in memory buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DSN,TU,TS,SDATE(6),EDATE(6),GRPSIZ
      CHARACTER*8 LSCENM,LRCHNM,LCONNM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - Dataset number containing data to find out about
C     LSCENM - Name of scenario associated with DSN
C     LRCHNM - Name of reach associated with DSN
C     LCONNM - Name of constituent associated with DSN
C     TU     - Default timeunits
C     TS     - Default timestep
C     SDATE  - Data start date
C     EDATE  - Data end date
C     GRPSIZ - Group pointer size
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,DSFREC,RETCOD,LWDMFL,LDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL  WTFNDT,WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
C     init output arguments
      LSCENM= ' '
      LRCHNM= ' '
      LCONNM= ' '
      TU    = 0
      TS    = 0
      GRPSIZ= 0
      DO 10 I= 1,6
        SDATE(I)= 0
        EDATE(I)= 0
 10   CONTINUE
C
      IF (DSN .LE. MXTS) THEN
C       dsn in range
        IF (CTSAVA(DSN) .GT. 0) THEN
C         valid label exists
          LSCENM= CTSSEN(DSN)
          LRCHNM= CTSLOC(DSN)
          LCONNM= CTSCON(DSN)
          TU    = CTSTU(DSN)
          TS    = CTSTS(DSN)
          GRPSIZ= CTSGPS(DSN)
C         adjust wdm and dsn as needed
          CALL WID2UD (CTSWDM,DSN,
     O                 LWDMFL,LDSN)
C         need to determine what data is available
          I= 1
          CALL WTFNDT (LWDMFL,LDSN,I,
     O                 DSFREC,CTSSDT(1,DSN),CTSEDT(1,DSN),RETCOD)
          IF (CTSSDT(1,DSN) .GT. 0) THEN
C           data available
            CTSAVA(DSN)= 3
          ELSE
C           data checked but not available
            CTSAVA(DSN)= 2
          END IF
          DO 20 I= 1,6
            SDATE(I)= CTSSDT(I,DSN)
            EDATE(I)= CTSEDT(I,DSN)
 20       CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSESPC
     I                   (SPSEN,SPLOC,SPCON)
C
C     + + + PURPOSE + + +
C     set sen,loc,con specs for matching
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*8 SPSEN,SPLOC,SPCON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SPSEN  - Specified scenario (blanks if all)
C     SPLOC  - Specified location (blanks if all)
C     SPCON  - Specified constituent (blanks if all)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CSPSEN= SPSEN
      CSPLOC= SPLOC
      CSPCON= SPCON
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSESPA
     I                   (SPAVA)
C
C     + + + PURPOSE + + +
C     set available specs for matching
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SPAVA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SPAVA  - Specified availability (0 if all)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CSPAVA= SPAVA
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSPARF
     I                   (PARFG)
C
C     + + + PURPOSE + + +
C     set partial flag for matching
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   PARFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PARFG  - partial matching flag,
C              0 - no partial matching of specs
C              1 - partial matching allowed
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CPARFG= PARFG
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSM
     M                  (DSN)
C
C     + + + PURPOSE + + +
C     find next matching dataset from in memory buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - input base dataset, output matching dataset, 0 if none
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DONFG,SLEN,LLEN,CLEN
      CHARACTER*8 LCTSSE,LCTSLO,LCTSCO
C
C     + + + FUNCTIONS + + +
      INTEGER     ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZLNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      DONFG= 0
      IF (DSN.LT.1) THEN
        DSN= 1
      ELSE IF (DSN.GT.MXTS) THEN
        DSN= MXTS
      END IF
      SLEN = ZLNTXT(CSPSEN)
      LLEN = ZLNTXT(CSPLOC)
      CLEN = ZLNTXT(CSPCON)
 10   CONTINUE
        IF (CTSAVA(DSN).GT.0) THEN
C         valid dataset here, does it meet specs, assume it does
          DONFG= 1
          IF (SLEN.GT.0) THEN
C           scenario spec to check against
            IF (CPARFG.EQ.1) THEN
C             allow partial match, use only length of spec
              LCTSSE = CTSSEN(DSN)(1:SLEN)
            ELSE
C             no partial matches allowed, use full length
              LCTSSE = CTSSEN(DSN)
            END IF
            IF (CSPSEN.NE.LCTSSE) THEN
C             fails scenario
              DONFG= 0
            END IF
          END IF
          IF (LLEN.GT.0 .AND. DONFG.EQ.1) THEN
C           location spec to check against
            IF (CPARFG.EQ.1) THEN
C             allow partial match, use only length of spec
              LCTSLO = CTSLOC(DSN)(1:LLEN)
            ELSE
C             no partial matches allowed, use full length
              LCTSLO = CTSLOC(DSN)
            END IF
            IF (CSPLOC.NE.LCTSLO) THEN
C             fails location
              DONFG= 0
            END IF
          END IF
          IF (CLEN.GT.0 .AND. DONFG.EQ.1) THEN
C           constituent spec to check against
            IF (CPARFG.EQ.1) THEN
C             allow partial match, use only length of spec
              LCTSCO = CTSCON(DSN)(1:CLEN)
            ELSE
C             no partial matches allowed, use full length
              LCTSCO = CTSCON(DSN)
            END IF
            IF (CSPCON.NE.LCTSCO) THEN
C             fails constituent
              DONFG= 0
            END IF
          END IF
          IF (CSPAVA.NE.0 .AND. CSPAVA.NE.CTSAVA(DSN)) THEN
            DONFG= 0
          END IF
        END IF
C
        IF (DONFG .EQ. 0) THEN
C         not this one
          IF (DSN.LT.MXTS) THEN
C           try the next one
            DSN  = DSN+ 1
          ELSE
C           no match
            DSN  = 0
            DONFG= 1
          END IF
        END IF
      IF (DONFG .EQ. 0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSMA
     I                   (BASDSN,
     O                    DSN)
C
C     + + + PURPOSE + + +
C     find next matching dataset from in memory buffer, add if needed
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     BASDSN,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BASDSN - input base dataset
C     DSN    - new data set number
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL    TSDSM
C
C     + + + END SPECIFICATIONS + + +
C
      DSN = BASDSN
      IF (DSN.LE.0) THEN
C       start looking with first dsn
        DSN= 1
      END IF
      LDSN= DSN
      CALL TSDSM(LDSN)
      IF (LDSN .EQ. 0) THEN
C       nothing matches, need to add
 10     CONTINUE
          IF (CTSAVA(DSN) .EQ. -1) THEN
C           a free spot, reserve it
            CTSAVA(DSN)= -BASDSN
C           mark it for saving
            CTSSAV(DSN)= 1
            LDSN= DSN
          ELSE IF (DSN .LT. MXTS) THEN
C           try next spot
            DSN= DSN+ 1
          ELSE
C           it will not work
            LDSN= -1
          END IF
C         loop back if needed
        IF (LDSN .EQ. 0) GO TO 10
      END IF
C
      IF (LDSN .GT. 0) THEN
C       have a match
        DSN= LDSN
      ELSE
C       no match and no space
        DSN= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSMD
C
C     + + + PURPOSE + + +
C     find matching datasets from in memory buffer, flag to delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL    TSDSM
C
C     + + + END SPECIFICATIONS + + +
C
      LDSN= 1
 10   CONTINUE
        CALL TSDSM(LDSN)
        IF (LDSN .GT. 0) THEN
C         a match, flag to delete
          CTSSAV(LDSN)= -1
C         keep looking
          LDSN= LDSN+ 1
        END IF
      IF (LDSN .NE. 0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSCL
     I                   (MESSFL,ADFG,DLFG)
C
C     + + + PURPOSE + + +
C     clear out pending dataset adds and deletes from in memory buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,ADFG,DLFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL  - message file unit number
C     ADFG    - add clear out flag
C     DLFG    - delete clear out flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DSN,RETCOD,NTYPE,SAIND,SALEN,BASDSN,
     1            NWDM,NDSN,BDSN,BWDM,I0
      CHARACTER*1 CTMP1(12)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CTMP1,CTMP8),(CTMP1,CTMP12)
      CHARACTER*8  CTMP8
      CHARACTER*12 CTMP12
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSDL, WDDSCL, WDBSAC, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      CTMP12= ' '
      DO 100 DSN= 1,MXTS
        IF (DLFG.NE.0 .AND. CTSSAV(DSN).EQ.-1) THEN
C         delete dataset
          CALL WID2UD (I0,DSN,
     O                 NWDM,NDSN)
          CALL WDDSDL (NWDM,NDSN,RETCOD)
          WRITE(99,*) 'deleted:',DSN,RETCOD
          CTSSAV(DSN)= 0
          CTSAVA(DSN)= -1
          CTSSEL(DSN)= 0
        ELSE IF (ADFG.NE.0 .AND. CTSSAV(DSN).EQ.1) THEN
C         add dsn
          NTYPE = 0
          BASDSN = ABS(CTSAVA(DSN))
          CALL WID2UD (I0,BASDSN,
     O                 BWDM,BDSN)
          CALL WID2UD (I0,DSN,
     O                 NWDM,NDSN)
          CALL WDDSCL (BWDM,BDSN,NWDM,NDSN,NTYPE,
     O                 RETCOD)
          WRITE(99,*) 'added  :',BASDSN,DSN,RETCOD
          IF (RETCOD. EQ. 0) THEN
C           put data-set scenario name
            CTMP8= CTSSEN(DSN)
            SAIND= 288
            SALEN= 8
            CALL WDBSAC (CTSWDM,DSN,MESSFL,SAIND,SALEN,CTMP1,
     O                   RETCOD)
C           add complete
            CTSSAV(DSN)= 0
            CTSAVA(DSN)= 1
          END IF
        END IF
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSVL
     I                   (WDMSFL,MXLOC,DOSTUS,UCIPTH,
     O                    NLOC,LAT,LNG,MKSTAT,LCID,LCNAME,CLOCID)
C
C     + + + PURPOSE + + +
C     set global valid values for scenario, location, and constituent
C     and save information for maps and scenario functions
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,MXLOC,NLOC,MKSTAT(MXLOC),LCID(MXLOC),
     1             DOSTUS
      REAL         LAT(MXLOC),LNG(MXLOC)
      CHARACTER*40 LCNAME(MXLOC)
      CHARACTER*8  CLOCID(MXLOC)
      CHARACTER*64 UCIPTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - wdm file unit number
C     MXLOC  - maximum possible locations to map
C     NLOC   - number of locations to map
C     MKSTAT - status of each marker location
C     UCIPTH - path to uci files for this basin
C     LAT    - lat of each location
C     LNG    - long of each location
C     LCID   - id numbers of each location
C     LCNAME - name of each location
C     CLOCID - short name of each location
C     DOSTUS - do status bar flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       NUMNAM,I0,I1,I8,IVAL(MXTS),IPOS,I,FLEN,ILEN,K,NPASS,
     $              FOUND,SAIND,SALEN,RETCOD,NAMFND,SENLEN,REMLEN,
     $              I64,I4,STATUS,SPOS,INTVL1,INTVL2,J,PTHLEN
      REAL          RINTVL
      CHARACTER*1   STRIN1(8*MXTS),LCNAM1(48)
      CHARACTER*1   FILNAM(64),CUCI(4)
      CHARACTER*8   CTSNAM(MXTS),GLOID(3),CURNAM
      CHARACTER*40  TMPNAM
      CHARACTER*64  UCINAM
      CHARACTER*78  TBUF78
      LOGICAL       IEXIST
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE  (CTSNAM,CTSNA1)
      CHARACTER*1   CTSNA1(8,MXTS)
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT,INT
C
C     + + + FUNCTIONS + + +
      INTEGER       ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    GSTGLV,ZIPI,CVARAR,ASRTC,WDBSGR,WDBSGC,CARVAR
      EXTERNAL    ZLNTXT,CHRCHR,ZWRSCR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   CUCI/'.','u','c','i'/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
      I8 = 8
      I64= 64
C
      RINTVL = FLOAT(MXTS)/40.0
      IF (RINTVL.LT.2.0 .OR. DOSTUS.EQ.0) THEN
C       no need to do status bar
        STATUS = 0
      ELSE
C       do status bar
        STATUS = 1
      END IF
      SPOS = 54
C
      GLOID(1) = 'SCENARIO'
      GLOID(2) = 'LOCATION'
      GLOID(3) = 'CONSTITU'
C
      DO 100 NPASS = 1,3
C       make three passes, once for scen, loc, and const
C
        IF (STATUS.EQ.1) THEN
C         update status bar
          TBUF78 = '.'
          I= 14
          SPOS = SPOS + 1
          CALL ZWRSCR (TBUF78(1:1),I,SPOS)
        END IF
C       first look through array of names to build list
        NUMNAM = 0
        DO 5 I = 1,MXTS
          IF (NPASS .EQ. 1) THEN
            CURNAM= CTSSEN(I)
          ELSE IF (NPASS .EQ. 2) THEN
            CURNAM= CTSLOC(I)
          ELSE
            CURNAM= CTSCON(I)
          END IF
          IF (CURNAM .NE. '        ') THEN
C           may want to add to list
            FOUND= 0
            IF (NUMNAM .GT. 0) THEN
C             already there?
              K= 0
 3            CONTINUE
                K= K+ 1
                IF (CTSNAM(K) .EQ. CURNAM) THEN
C                 dont need to add
                  FOUND= 1
                END IF
              IF (FOUND.EQ.0 .AND. K.LT.NUMNAM) GO TO 3
            END IF
            IF (FOUND .EQ. 0) THEN
C             add a new name
              NUMNAM = NUMNAM + 1
              CTSNAM(NUMNAM) = CURNAM
              IF (NPASS .EQ. 2) THEN
C               dummy for loc
                LAT(NUMNAM)= -999.0
              END IF
            END IF
          END IF
 5      CONTINUE
C
C       sort names in place
        CALL ASRTC(I1,I8,NUMNAM,
     M             CTSNA1,
     O             IVAL)
        IF (STATUS.EQ.1) THEN
C         update status bar
          TBUF78 = '.'
          I= 14
          SPOS = SPOS + 1
          CALL ZWRSCR (TBUF78(1:1),I,SPOS)
        END IF
        IF (NPASS .EQ. 2) THEN
C         save additional info for locations
          NLOC= NUMNAM
          DO 8 I= 1,NUMNAM
C           set flag to indicate long name not yet found
            NAMFND = 0
C           return additional info for locations, short name
            CLOCID(I) = CTSNAM(I)
C           location handle
            LCID(I)= I
C           assume all location selected <TEMP>
            MKSTAT(I)= I1
            DO 6 K= 1,MXTS
C             look for all dsns at this location
              IF (CTSNAM(I) .EQ. CTSLOC(K)) THEN
C               save integer location name
                CTSILC(K)= I
                IF (LAT(I) .LT. -100.0) THEN
C                 get attribute for location lat
                  SAIND  = 8
                  SALEN  = 1
                  CALL WDBSGR (WDMSFL,K,SAIND,SALEN,
     O                         LAT(I),RETCOD)
C                 get attribute for location lng
                  SAIND = 9
                  CALL WDBSGR (WDMSFL,K,SAIND,SALEN,
     O                         LNG(I),RETCOD)
                END IF
C               get attribute for station name
                SAIND = 45
                SALEN = 48
                CALL WDBSGC (WDMSFL,K,SAIND,SALEN,
     O                       LCNAM1,RETCOD)
                ILEN = 40
                CALL CARVAR (ILEN,LCNAM1,ILEN,TMPNAM)
                IF (NAMFND.EQ.0) THEN
C                 use this name as location name
                  LCNAME(I) = TMPNAM
                  NAMFND = 1
                ELSE IF (NAMFND.EQ.1 .AND. LCNAME(I).NE.TMPNAM) THEN
C                 already found a name for this location and not the same
                  LCNAME(I) = ' '
                END IF
              END IF
  6         CONTINUE
            INTVL1 = INT(NUMNAM*0.33)
            INTVL2 = INT(NUMNAM*0.67)
            IF (STATUS.EQ.1 .AND. (I.EQ.INTVL1.OR.I.EQ.INTVL2)) THEN
C             update status bar
              TBUF78 = '.'
              J= 14
              SPOS = SPOS + 1
              CALL ZWRSCR (TBUF78(1:1),J,SPOS)
            END IF
  8       CONTINUE
        END IF
C
        IF (NPASS.NE.1) THEN
C         location or constit loop, blank out associated attribute
          CALL ZIPI (NUMNAM,I0,IVAL)
        ELSE
C         scenario loop, check for uci files
          DO 9 I=1,NUMNAM
C           build uci file name from path name and scenario name
            SENLEN = ZLNTXT(CTSNAM(I))
            REMLEN = I64 - ZLNTXT(UCIPTH)
            PTHLEN = ZLNTXT(UCIPTH)
            IF (PTHLEN.GT.0) THEN
C             put path with file name
              CALL CVARAR(PTHLEN,UCIPTH,PTHLEN,
     O                    FILNAM(1))
            END IF
            CALL CVARAR(SENLEN,CTSNAM(I),REMLEN,
     O                  FILNAM(ZLNTXT(UCIPTH)+1))
            CALL CHRCHR(I4,CUCI,FILNAM(ZLNTXT(UCIPTH)+SENLEN+1))
            CALL CARVAR(I64,FILNAM,I64,UCINAM)
C           see if uci file exists
            INQUIRE (FILE=UCINAM,EXIST=IEXIST)
            IF (IEXIST) THEN
C             yes, file does exist
              IVAL(I) = 1
            ELSE
C             no, file does not exist
              IVAL(I) = 0
            END IF
 9        CONTINUE
        END IF
C
C       now put those names into global valid values
        IPOS = 1
        DO 10 I = 1,NUMNAM
C         put valid responses into global valid array
          CALL CVARAR (I8,CTSNAM(I),I8,STRIN1(IPOS))
          IPOS = IPOS + 8
 10     CONTINUE
        FLEN  = 8
        ILEN  = 8*NUMNAM
        CALL GSTGLV (GLOID(NPASS),FLEN,ILEN,STRIN1,NUMNAM,IVAL)
C
C       save count and name info in common block
        IF (NPASS .EQ. 1) THEN
          CNTSEN = NUMNAM
          DO 20 I = 1, NUMNAM
            UNISEN(I) = CTSNAM(I)
 20       CONTINUE
        ELSE IF (NPASS .EQ. 2) THEN
          CNTLOC = NUMNAM
          DO 30 I = 1, NUMNAM
            UNILOC(I) = CTSNAM(I)
 30       CONTINUE
        ELSE
          CNTCON = NUMNAM
          DO 40 I = 1, NUMNAM
            UNICON(I) = CTSNAM(I)
 40       CONTINUE
        END IF
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSIDEN
     I                   (LOCID,
     O                    NMATCH,CONNAM,SCENNM)
C
C     + + + PURPOSE + + +
C     identify scenarios and constituents associated with a selected
C     location for the map identification screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NMATCH
      CHARACTER*8  LOCID,CONNAM(10),SCENNM(10)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NMATCH - number of constituent, scenario pairs found
C     LOCID  - short location name for which to find matches
C     CONNAM - matching constituent names
C     SCENNM - matching scenario names
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       DSN,RETCOD
      CHARACTER*8   CBLANK
C
C     + + + EXTERNALS + + +
      EXTERNAL      TSESPC,TSDSM
C
C     + + + END SPECIFICATIONS + + +
C
      CBLANK = '        '
      CALL TSESPC (CBLANK,LOCID,CBLANK)
      DSN = 0
      NMATCH = 0
      RETCOD = 0
C
 10   CONTINUE
        DSN = DSN + 1
        CALL TSDSM (DSN)
        IF (DSN.GT.0) THEN
C         get scenario and constituent
          NMATCH = NMATCH + 1
          SCENNM(NMATCH) = CTSSEN(DSN)
          CONNAM(NMATCH) = CTSCON(DSN)
        ELSE
C         cant find specified scen,loc,cons dsn
          RETCOD = -1
        END IF
      IF (RETCOD.EQ.0 .AND. NMATCH.LT.10) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSRETN
     I                   (DSN,
     O                    CSEN,CLOC,CCON)
C
C     + + + PURPOSE + + +
C     identify scenarios, locations, and constituents associated
C     with a selected data set number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      DSN
      CHARACTER*8  CSEN,CLOC,CCON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - data set number
C     CSEN   - scenario name
C     CLOC   - location name
C     CCON   - constituent name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CSEN = CTSSEN(DSN)
      CLOC = CTSLOC(DSN)
      CCON = CTSCON(DSN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSAPUT
     I                   (DSN,
     I                    CSEN,CLOC,CCON)
C
C     + + + PURPOSE + + +
C     put scenarios, locations, and constituent attributes
C     for a selected data set number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      DSN
      CHARACTER*8  CSEN,CLOC,CCON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - data set number
C     CSEN   - scenario name
C     CLOC   - location name
C     CCON   - constituent name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CTSSEN(DSN) = CSEN
      CTSLOC(DSN) = CLOC
      CTSCON(DSN) = CCON
C
      IF (CSEN.EQ.'        '.OR.CLOC.EQ.'        '.OR.
     1    CCON.EQ.'        ') THEN
        CTSAVA(DSN) = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSAC
     I                   (DSN,
     O                    ACTFLG)
C
C     + + + PURPOSE + + +
C     returns whether a dataset is active or not
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DSN,ACTFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - dataset number
C     ACTFLG - active flag - 1:yes, 0:no
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DSN.GT.0 .AND. DSN.LT.MXTS) THEN
        ACTFLG= CTSSEL(DSN)
      ELSE
        ACTFLG= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSAS
     I                   (DSN,ACTFLG)
C
C     + + + PURPOSE + + +
C     sets whether a dataset is active or not
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DSN,ACTFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - dataset number
C     ACTFLG - active flag - 1:yes, 0:no
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DSN.GT.0 .AND. DSN.LT.MXTS) THEN
        CTSSEL(DSN)= ACTFLG
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSLC
     I                   (DSN,
     O                    LOCID)
C
C     + + + PURPOSE + + +
C     returns location of a dataset
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DSN,LOCID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - dataset number
C     LOCID  - location id
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LOCID= CTSILC(DSN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTADSN
     I                   (MXDSN,
     O                    NDSN,DSN)
C
C     + + + PURPOSE + + +
C     return all active selected datasets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MXDSN,NDSN,DSN(MXDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXDSN   - max number of datasets to return
C     NDSN    - number of datasets
C     DSN     - array of dataset numbers
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxts.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,ITMP,CLID,LDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL  TSDSAC,CLACTQ,GTCDSN
C
C     + + + END SPECIFICATIONS + + +
C
C     assume no active and selected datasets
      NDSN= 0
C
C     start at first class
      CLID= 1
 10   CONTINUE
C       look for active classes
        CALL CLACTQ(CLID)
        IF (CLID .GT. 0) THEN
C         class is active, anything selected
          J= 0
 20       CONTINUE
            J= J+ 1
C           get the data set number for the jth element of this class
            CALL GTCDSN (J,CLID,
     O                   LDSN)
            CALL TSDSAC(LDSN,
     O                  ITMP)
            IF (ITMP .GT. 0) THEN
C             need this one
              I = NDSN
              IF (NDSN .GT. 0) THEN
C               do we already have it?
 30             CONTINUE
                  IF (DSN(I) .EQ. LDSN) THEN
C                   already have it
                    I= -1
                  ELSE
C                   try next one
                    I= I- 1
                  END IF
                IF (I .GT. 0) GO TO 30
              END IF
C
              IF (I .EQ. 0) THEN
C               need it
                NDSN= NDSN+ 1
                DSN(NDSN)= LDSN
              END IF
            END IF
          IF (NDSN.LT.MXDSN .AND. J.LT.MXTS) GO TO 20
          CLID= CLID+ 1
        END IF
      IF (CLID.NE.0 .AND. NDSN.LT.MXDSN) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSSC
     I                   (OLD,DSN,SENNAM)
C
C     + + + PURPOSE + + +
C     sets the scenario loc const names for a data set in the directory,
C     used after creating a new scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     OLD,DSN
      CHARACTER*8 SENNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OLD    - old dataset number copied from
C     DSN    - new dataset number
C     SENNAM - scenario name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DSN.GT.0 .AND. DSN.LT.MXTS) THEN
        CTSSEN(DSN)= SENNAM
        CTSLOC(DSN)= CTSLOC(OLD)
        CTSCON(DSN)= CTSCON(OLD)
        CTSILC(DSN)= CTSILC(OLD)
        CTSTU(DSN) = CTSTU(OLD)
        CTSTS(DSN) = CTSTS(OLD)
        CTSAVA(DSN)= -1 * OLD
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSGN
     I                   (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     sets the scenario loc const names for a data set in the directory,
C     used after generating a new data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DSN,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - new dataset number
C     WDMSFL - wdm data file unit number
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      J,I0,SAIND,SALEN,RETCOD,DSFREC,LWDMFL,LDSN
      CHARACTER*1  CTMP1(12)
      CHARACTER*8  CTMP8
C
C     + + + FUNCTIONS + + +
      INTEGER     ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDBSGI, WDBSGC, WTFNDT, ZLNTXT, WID2UD
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (8A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IF (DSN.GT.0 .AND. DSN.LT.MXTS) THEN
C       okay data set number to add
C       set label available
        CTSAVA(DSN)= 0
C       get time step
        SALEN= 1
        SAIND= 33
        CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O               CTSTS(DSN),RETCOD)
        IF (RETCOD.NE.I0) THEN
C         set time step to default of 1
          CTSTS(DSN)= 1
        END IF
C       get time units
        SAIND= 17
        CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O               CTSTU(DSN),RETCOD)
        IF (RETCOD.NE.I0) THEN
C         set to default of daily time units
          CTSTU(DSN)= 4
        END IF
C       group pointer size
        SAIND = 34
        CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O               CTSGPS(DSN),RETCOD)
        IF (RETCOD.NE.I0) THEN
C         could not get group size, set to default
          CTSGPS(DSN) = 6
        END IF
C
C       get data-set scenario name
        SAIND= 288
        SALEN= 8
        CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O               CTMP1,RETCOD)
        IF (RETCOD.EQ.I0) THEN
C         save scenario name
          WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
          CTSSEN(DSN)= CTMP8
        END IF
C
C       get data-set reach name
        SAIND= 290
        SALEN= 8
        CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O               CTMP1,RETCOD)
        IF (RETCOD.EQ.I0) THEN
C         save reach name
          WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
          CTSLOC(DSN)= CTMP8
        END IF
C
C       get next attribute for constituent name
        SAIND = 289
        SALEN = 8
        CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O               CTMP1,RETCOD)
        IF (RETCOD.EQ.-107 .AND. CTSLOC(DSN).NE.'        ') THEN
C         this attribute not present but location name is,
C         try tstype for constituent
          SAIND = 1
          SALEN = 4
          CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O                 CTMP1,RETCOD)
          CTMP1(5) = ' '
          CTMP1(6) = ' '
          CTMP1(7) = ' '
          CTMP1(8) = ' '
        END IF
        IF (RETCOD.EQ.I0) THEN
C         put constituent in temp variable
          WRITE (CTMP8,2000) (CTMP1(J),J=1,8)
          CTSCON(DSN)= CTMP8
        END IF
C
        IF (ZLNTXT(CTSSEN(DSN)).GT.0 .AND.
     $      ZLNTXT(CTSLOC(DSN)).GT.0 .AND.
     $      ZLNTXT(CTSCON(DSN)).GT.0) THEN
C         valid scenario, location, constituent names --> valid label
          CTSAVA(DSN) = 1
        END IF
C
C       adjust wdm and dsn as needed
        CALL WID2UD (WDMSFL,DSN,
     O               LWDMFL,LDSN)
C       get start and end dates for each data set
        J= 1
        CALL WTFNDT (LWDMFL,LDSN,J,
     O               DSFREC,CTSSDT(1,DSN),CTSEDT(1,DSN),RETCOD)
        IF (CTSSDT(1,DSN) .GT. I0) THEN
C         data available
          CTSAVA(DSN)= 3
          CTSDSN= CTSDSN+ 1
        ELSE
C         data checked but not available
          CTSAVA(DSN)= 2
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSAV
     M                   (DSN)
C
C     + + + PURPOSE + + +
C     find next available dataset from in memory buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - input base dataset, output next avail dataset, 0 if none
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DONFG
C
C     + + + END SPECIFICATIONS + + +
C
      DONFG= 0
      DSN  = DSN + 1
      IF (DSN.LT.1) THEN
        DSN= 1
      ELSE IF (DSN.GT.MXTS) THEN
        DSN= MXTS
      END IF
 10   CONTINUE
        IF (CTSAVA(DSN).EQ.-1) THEN
C         available dataset here
          DONFG= 1
        ELSE
C         not this one
          IF (DSN.LT.MXTS) THEN
C           try the next one
            DSN  = DSN+ 1
          ELSE
C           none available
            DSN  = 0
            DONFG= 1
          END IF
        END IF
      IF (DONFG .EQ. 0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSEX
     I                   (DSN,
     O                    EXFLG)
C
C     + + + PURPOSE + + +
C     returns whether a dataset exists or not
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DSN,EXFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - dataset number
C     EXFLG  - exists flag - <=0:no, >0:yes
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DSN.GT.0 .AND. DSN.LT.MXTS) THEN
        EXFLG= CTSAVA(DSN)
      ELSE
        EXFLG= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE TSDSCT
     O                 (LNTLOC,LNTSEN,LNTCON,LNTDSN)
C
C     + + + PURPOSE + + +
C     returns count of unique locs, scenarios and constits
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LNTLOC,LNTSEN,LNTCON,LNTDSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LNTLOC - count of unique locations
C     LNTSEN - count of scenarios
C     LNTCON - count of constituents
C     LNTDSN - count of datasets
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LNTLOC = CNTLOC
      LNTSEN = CNTSEN
      LNTCON = CNTCON
      LNTDSN = CTSDSN
C
      RETURN
      END
C
C
C
      SUBROUTINE TSDSUN
     O                 (LUNLOC,LUNSEN,LUNCON)
C
C     + + + PURPOSE + + +
C     returns unique locs, scenarios and constits
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*8 LUNLOC(*),LUNSEN(*),LUNCON(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LUNLOC - unique locations
C     LUNSEN - scenarios
C     LUNCON - constituents
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsdir.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,CNTLOC
        LUNLOC(I)= UNILOC(I)
 10   CONTINUE
      DO 20 I= 1,CNTSEN
        LUNSEN(I)= UNISEN(I)
 20   CONTINUE
      DO 30 I= 1,CNTCON
        LUNCON(I)= UNICON(I)
 30   CONTINUE
C
      RETURN
      END
