C
C
C
      SUBROUTINE   WSGEN
     I                  (MESSFL)
C
C     + + + PURPOSE + + +
C     main scenario generator routine
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C
C     + + + PARAMETERS + + +
      INTEGER    MXCOV,  MXSEN,   MXLOC
      PARAMETER (MXCOV=7,MXSEN=20,MXLOC=500)
      INTEGER    MXLINE,   MXGEN,   MXWDM,  MXINTV,      MXBASN
      PARAMETER (MXLINE=10,MXGEN=10,MXWDM=4,MXINTV=10000,MXBASN=5)
      INCLUDE   'pmxwin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SCLU,SGRP,I,I0,I1,RESP,FE,NUMHED,DONFG,
     $              WDMPFL,WDMSFL,RETCOD,NLOC,GENCNT,BASCUR,
     $              GENBUF(MXGEN),PLTDEV,MAPCOV(MXCOV),USRLEV,
     $              TS,TU,SDATE(6),EDATE(6),SCTRID,BRSFG,UEDFG,
     $              ARHLOG(2),SACTIV,FILUN(1),LINCNT,
     $              PLCLR(MXSEN),PLNTYP(MXSEN),I78,IM1,MAPFLG,
     $              PPATRN(MXSEN),PSYMBL(MXSEN),LCID(MXLOC),
     $              LOCXFG,CMPTYP,EMFG,MODLID,NCLAS,
     $              NANAL,ADSN(MXLINE),NBASIN,NBWDM(MXBASN),
     $              ACT,SSDATE(6),SEDATE(6),DTRAN,INIDUR,
     $              UNSAVE,JSTSAV,STACOD,NWDM,WDMERR,LWDM
      REAL          LAT(MXLOC),LNG(MXLOC)
      CHARACTER*1   CDUM(1),FLGCHR
      CHARACTER*4   WDID(MXWDM),BWDID(MXBASN,MXWDM),LWDID(MXWDM)
      CHARACTER*8   PTHNAM(1),CDSID,TMPNAM
      CHARACTER*8   CSCENM(MXLINE),CRCHNM(MXLINE),
     $              CCONNM(MXLINE),BASNAM(MXBASN)
      CHARACTER*64  WDNAME(MXWDM),FNAME,BASDES(MXBASN),BUCIPA(MXBASN),
     $              BWDMNM(MXBASN,MXWDM),LWDNAM(MXWDM),UCIPTH,TFNAME(1)
      CHARACTER*78  HEADNM(3)
      CHARACTER*80  TBUF80(MXLOC)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TBUF80,TBUFF)
      CHARACTER*1  TBUFF(80,MXLOC)
C
C     + + + EXTERNALS + + +
      EXTERNAL     GPOPEN, GPCLOS, AGENER, DOBANN
      EXTERNAL     QRESP,  PRNTXT, PRNTXI, ANPRGT, WDMSET
      EXTERNAL     PRWMAP, WDBOUN, PDNPLT, WSGSYS, WSGSIM, DRSCAN
      EXTERNAL     WSTRD,  WSTWR,  SSPECI
      EXTERNAL     PMXCNW, ZMNSST, TSDRRE, MLTBAS
      EXTERNAL     CLCNT,  CLFDSN, GTADSN, INWSEG
      EXTERNAL     DTACT, DTGET, TSDSGN, WSGANA, ZBLDWR
      EXTERNAL     ZSTCMA, Q1INIT, Q1EDIT, QSETFN, QGETF
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 54
      I0  = 0
      I1  = 1
      IM1 = -1
      I78 = 78
      FE  = 99
      PLTDEV = 1
      INIDUR = 1
      OPEN(UNIT=FE,FILE='error.fil',STATUS='UNKNOWN')
      write (FE,*) 'about to open gks'
      CALL GPOPEN(FE)
      write (FE,*) 'just opened gks'
C     initialize location change flag
      LOCXFG = 0
C     set default units for simulation
      EMFG   = 1
C     initialize scenario/simulate related variables
      SACTIV= 0
      UNSAVE= 0
      JSTSAV= 0
      TMPNAM= ' '
C     initialize number of data sets for analysis
      NANAL = 0
C
C     open boundary data file
      CALL WDBOUN(
     O            WDMPFL,RETCOD,FNAME,NUMHED,HEADNM,SCTRID,FLGCHR)
      IF (RETCOD .NE. 0) THEN
C       error on open or check of boundary data file
        SGRP= 10
        CALL PRNTXI(MESSFL,SCLU,SGRP,RETCOD)
        WRITE(FE,*) 'no boundary data available',RETCOD
        MAPFLG = 0
        FNAME  = 'GENSCN.STA'
        NUMHED = 2
        HEADNM(1)= 'GENSCN 2.b'
        HEADNM(2)= 'generic version dated 12/94'
      ELSE
        WRITE(FE,*) 'boundary data available',RETCOD
        MAPFLG = 1
      END IF
C
C     send message to screen for pre-initialization, get sta file name
      CDUM(1) = ' '
      CALL ZBLDWR (I1,CDUM,I1,I1,DONFG)
      CALL ZBLDWR (I1,CDUM,I0,I1,DONFG)
      CALL ZBLDWR (I1,CDUM,I0,I1,DONFG)
      CALL DOBANN (MESSFL,SCLU,HEADNM,NUMHED,IM1,FLGCHR)
      CALL ZMNSST
C     make previous available
      I= 4
      CALL ZSTCMA (I,I1)
      SGRP= 25
      CALL Q1INIT (MESSFL,SCLU,SGRP)
      TFNAME(1) = FNAME
      CALL QSETFN (I1,TFNAME)
C     ask user to enter file name
      CALL Q1EDIT (RESP)
C     make previous unavailable
      I= 4
      CALL ZSTCMA (I,I0)
      IF (RESP.NE.2) THEN
C       get file name from screen
        CALL QGETF (I1,FILUN)
        INQUIRE (UNIT=FILUN(1),NAME=TFNAME(1))
        CLOSE (UNIT=FILUN(1))
C
        CALL DOBANN (MESSFL,SCLU,HEADNM,NUMHED,I1,FLGCHR)
        TS = 1
C
C       read current state of system
        CALL WSTRD(TFNAME(1),MXSEN,
     I             MXLINE,MXCOV,MXWDM,MXBASN,
     O             NWDM,WDNAME,WDID,MODLID,UEDFG,BRSFG,
     O             TU,ARHLOG,DTRAN,
     O             PLCLR,PLNTYP,PPATRN,PSYMBL,
     O             SDATE,EDATE,
     O             MAPCOV,STACOD,NANAL,ADSN,
     O             NBASIN,NBWDM,BWDID,BASNAM,BASDES,
     O             BUCIPA,BWDMNM)
C
        IF (STACOD.EQ.1) THEN
C         problem reading sta file, cant continue
          SGRP = 14
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        ELSE
C         okay to continue
          IF (NBASIN.GT.1) THEN
C           save screen text
            CALL ZMNSST
          END IF
C         do menu for multiple basins
          SGRP = 20
          CALL MLTBAS (MESSFL,SCLU,SGRP,MXBASN,MXWDM,NBASIN,BASNAM,
     I                 BASDES,NWDM,NBWDM,WDNAME,WDID,
     I                 BWDMNM,BWDID,BUCIPA,
     M                 LWDM,LWDNAM,LWDID,UCIPTH,BASCUR)
C
C         display initializing message
          IF (NBASIN.GT.1) THEN
C           need to do header again
            CALL DOBANN (MESSFL,SCLU,HEADNM,NUMHED,I1,FLGCHR)
          END IF
          SGRP= 13
          CALL PMXCNW (MESSFL,SCLU,SGRP,I1,IM1,I1,LINCNT)
C
C         set wdm files for scenario generator including global names
          SGRP= 6
          CALL WDMSET (MESSFL,SCLU,SGRP,MXINTV,MXWDM,LWDM,LWDNAM,LWDID,
     O                 WDMSFL,WDMERR)
C
          IF (WDMERR.EQ.0) THEN
C           pass wdm unit number to segment drawing routines
            CALL INWSEG (WDMSFL)
C
C           scan time series directory
            CALL DRSCAN (WDMSFL,MXLOC,MXLINE,NANAL,ADSN,UCIPTH,I1,
     O                   NLOC,LAT,LNG,LCID)
C
C           set class dataset information
            CALL CLCNT(NCLAS)
            DO 4 I= 1,NCLAS
              CALL CLFDSN(I)
 4          CONTINUE
C
 5          CONTINUE
C             welcome message and main loop
              CALL DOBANN (MESSFL,SCLU,HEADNM,NUMHED,I1,FLGCHR)
              CALL ZMNSST
C             display option menu
              SGRP= 1
              CALL QRESP (MESSFL,SCLU,SGRP,RESP)
              GO TO (10,20,30,40,50,60,70,100),RESP
C
 10           CONTINUE
C               map
                IF (MAPFLG.EQ.1) THEN
C                 map data exists, do map
                  PTHNAM(1)= ' '
                  CALL PRWMAP (MESSFL,MXLOC,IM1,WDMPFL,MXCOV,
     I                         PTHNAM(1),BRSFG,
     M                         NLOC,LCID,LAT,LNG,LOCXFG,MAPCOV)
                ELSE
C                 no boundaries to map
                  SGRP= 9
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                  RESP= 2
                END IF
C               get active and selected datasets
                CALL GTADSN(MXLINE,
     O                      NANAL,ADSN)
                GO TO 110
C
 20           CONTINUE
C               pick scenario, location, constituent data sets
                LOCXFG = 1
                PTHNAM(1)= ' '
                CALL SSPECI (MESSFL,PTHNAM)
C               get active and selected datasets
                CALL GTADSN(MXLINE,
     O                      NANAL,ADSN)
                GO TO 110
C
 30           CONTINUE
C               analysis
                CALL WSGANA (MESSFL,SCLU,MXLINE,WDMSFL,BRSFG,
     I                       MAPFLG,MXLOC,WDMPFL,MXCOV,MXSEN,
     M                       NANAL,ADSN,TU,TS,SDATE,EDATE,INIDUR,
     M                       PLNTYP,PLCLR,PPATRN,
     M                       PSYMBL,DTRAN,PLTDEV,ARHLOG,
     M                       NLOC,LCID,LAT,LNG,
     M                       LOCXFG,MAPCOV,
     M                       CSCENM,CRCHNM,CCONNM)
                GO TO 110
C
 40           CONTINUE
C               query
                SGRP = 4
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                GO TO 110
C
 50           CONTINUE
C               change system specifications
                CALL WSGSYS(MESSFL,
     I                      FNAME,MXSEN,MXLOC,MXLINE,MXCOV,
     I                      MXWDM,MXBASN,NWDM,WDNAME,WDID,MODLID,
     I                      ARHLOG,PLCLR,PLNTYP,PPATRN,PSYMBL,
     I                      MAPCOV,NANAL,ADSN,MXINTV,UEDFG,BRSFG,
     M                      NBASIN,NBWDM,BWDID,BASNAM,BASDES,
     M                      BUCIPA,BWDMNM,
     M                      LWDM,LWDNAM,LWDID,UCIPTH,BASCUR,WDMSFL,
     M                      NLOC,LAT,LNG,LCID,
     M                      TU,SDATE,EDATE,SACTIV,UNSAVE,JSTSAV)
C               get dates for current active date set in case they changed
                CALL DTACT (I)
                CALL DTGET (I,
     O                      ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TU,
     O                      TS,DTRAN)
                GO TO 110
C
 60           CONTINUE
C               check user level to see if simulate available
                I = 45
                CALL ANPRGT (I,
     O                       USRLEV)
                IF (USRLEV.EQ.0) THEN
C                 user not experienced, don't allow in
                  SGRP = 11
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                ELSE IF (USRLEV.EQ.2) THEN
C                 experienced user, allow in
                  IF (MODLID.EQ.0) THEN
C                   perform hspf simulation
                    CALL WSGSIM (MESSFL,WDMSFL,MXSEN,UCIPTH,SCTRID,
     I                           UEDFG,
     M                           SACTIV,UNSAVE,JSTSAV,TMPNAM,EMFG)
C                   get active and selected datasets, may have changed
                    CALL GTADSN(MXLINE,
     O                          NANAL,ADSN)
                  ELSE IF (MODLID.EQ.2) THEN
C                   perform daflow simulation, cant yet
                    SGRP= 7
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                  ELSE
C                   dont know about model
                    SGRP= 8
                    CALL PRNTXI(MESSFL,SCLU,SGRP,MODLID)
                  END IF
                END IF
                GO TO 110
C
 70           CONTINUE
C               gener
                PTHNAM(1)= ' '
                CALL AGENER (MESSFL,WDMSFL,MXGEN,PTHNAM,
     O                       GENCNT,GENBUF)
                IF (GENCNT.GT.0) THEN
                  IF (GENCNT.LT.MXGEN) THEN
C                   we've added some data sets, add to time series directory
                    DO 350 I = 1,GENCNT
                      CALL TSDSGN (WDMSFL,GENBUF(I))
 350                CONTINUE
                  ELSE IF (GENCNT.EQ.MXGEN) THEN
C                   we've filled the buffer, rescan directory
                    CALL TSDRRE (WDMSFL,I0,I0)
                  END IF
                END IF
                GO TO 110
C
 100          CONTINUE
C               done, return to DOS
C               make previous available
                I= 4
                CALL ZSTCMA (I,I1)
                SGRP= 110
                CALL Q1INIT (MESSFL,SCLU,SGRP)
                TFNAME(1) = FNAME
                CALL QSETFN (I1,TFNAME)
C               ask user to enter file name
                CALL Q1EDIT (RESP)
                IF (RESP.NE.2) THEN
C                 go ahead and exit
                  CALL QGETF (I1,FILUN)
                  INQUIRE (UNIT=FILUN(1),NAME=TFNAME(1))
                  CLOSE (UNIT=FILUN(1))
                  RESP = 8
C                 put up exit screen
                  SGRP = 111
                  CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,I)
                END IF
C               make previous unavailable
                I= 4
                CALL ZSTCMA (I,I0)
                GO TO 110
C
 110          CONTINUE
            IF (RESP .LE. 7) GO TO 5
C
C           what type computer
            CALL ANPRGT(I1,CMPTYP)
            IF (CMPTYP .NE. 1) THEN
C             close all windows
              DO 115 I= 1,MXWIN
                CALL PDNPLT (I,I1,I0)
 115          CONTINUE
            END IF
C           get active and selected datasets
            CALL GTADSN(MXLINE,
     O                  NANAL,ADSN)
C
C           shut down GKS
            CALL GPCLOS(FE)
C
C           write current state of system
            CALL WSTWR(TFNAME(1),MXSEN,
     I                 MXLINE,MXCOV,MXWDM,MXBASN,
     I                 NWDM,WDNAME,WDID,MODLID,UEDFG,BRSFG,
     I                 TU,ARHLOG,
     I                 PLCLR,PLNTYP,PPATRN,PSYMBL,
     I                 SDATE,EDATE,
     I                 MAPCOV,NANAL,ADSN,
     I                 NBASIN,NBWDM,BWDID,BASNAM,BASDES,
     I                 BUCIPA,BWDMNM)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTRD
     I                  (FNAME,MXSEN,
     I                   MXLINE,MXCOV,MXWDM,MXBASN,
     O                   NWDM,WDNAME,WDID,MODLID,UEDFG,BRSFG,
     O                   TU,ARHLOG,DTRAN,
     O                   PLCLR,PLNTYP,PPATRN,PSYMBL,
     O                   SDATE,EDATE,
     O                   MAPCOV,STACOD,
     O                   NSELEC,SELDSN,
     O                   NBASIN,NBWDM,BWDID,BASNAM,BASDES,
     O                   BUCIPA,BWDMNM)
C
C     + + + PURPOSE + + +
C     read current state of system
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MXSEN,MODLID,MXLINE,MXBASN,
     $              TU,ARHLOG(2),DTRAN,MXCOV,MXWDM,NWDM,
     $              PLCLR(MXSEN),PLNTYP(MXSEN),
     $              PPATRN(MXSEN),PSYMBL(MXSEN),UEDFG,BRSFG,
     $              SDATE(6),EDATE(6),MAPCOV(MXCOV),STACOD,
     $              NSELEC,SELDSN(MXLINE),NBASIN,NBWDM(MXBASN)
      CHARACTER*4   WDID(MXWDM),BWDID(MXBASN,MXWDM)
      CHARACTER*8   BASNAM(MXWDM)
      CHARACTER*64  WDNAME(MXWDM),FNAME,BASDES(MXBASN),BUCIPA(MXBASN),
     $              BWDMNM(MXBASN,MXWDM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNAME  - Status file name
C     MXSEN  - Maximum number of scenarios
C     MXLINE - maximum lines on plot
C     MXCOV  - Maximum coverages on map
C     MXWDM  - Maximum number of wdm files
C     WDNAME - Name of each wdm data file
C     NWDM   - number of wdm data files
C     WDID   - id for each wdm data file
C     MODLID - Model ID code - 0:HSPF, 2:DAFLOW
C     UEDFG  - user allowed to edit UCI file flag
C     BRSFG  - browse flag: 1-go to browse screen from map click
C     TU     - time units (1-sec, 2-min, 3-hour, 4-day....)
C     ARHLOG - type of axis - 1- arith, 2-log
C     PLCLR  - line colors for each line
C     PLNTYP - line types for each line
C     PPATRN - fill pattern for plots
C     PSYMBL - symbol type for each line
C     SDATE  - start of plot (yr,mo,dy,hr,mn,sec)
C     EDATE  - end of plot (yr,mo,dy,hr,mn,sec)
C     DTRAN  - data transformation flag
C     MAPCOV - color for each map coverage
C     STACOD - return code from sta file read
C     NSELEC - number of selected data sets
C     SELDSN - array of selected data set numbers
C     NBASIN - number of basins available
C     NBWDM  - number of wdm files for this basin
C     BASNAM - basin name
C     BASDES - basin description
C     BUCIPA - basin uci path
C     BWDID  - basin wdm file ids
C     BWDMNM - basin wdm file names
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxmap.inc'
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxwin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       IFL,I,J,ID,I0,ACT,SYM,CUN,CSL,NDATES,CURDAT,I1
      INTEGER       MAPDEV,MAPBRD,INTACT,MPNMFG(MXMAP),MPNMCL(MXMAP),
     $              SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL(2),LEGCOL(MXMAP),
     $              NLINE,NMAPS,CURMAP,SSDATE(2),SEDATE(2),DTTU,DTTS,
     $              ATMAJ(MXSEG),ATMIN(MXSEG),CLR1(MXSEG),CLR2(MXSEG),
     $              DSN(MXSEG),NSEG,LEGFLG(MXMAP),NANIM,CURANI,I4,
     $              CLR3(MXSEG)
      INTEGER       WIPEIT(MXWIN),MAPWIN,PLTWIN,WTYPE(MXWIN),NWIN
      REAL          WINDIM(4,MXWIN)
      REAL          MPNMSZ(MXMAP),LSIZE,LGSIZE(MXMAP),MSIZE,
     $              RLAT(2,MXMAP),RLNG(2,MXMAP),
     $              BASEX(MXMAP),BASEY(MXMAP),RMIN(MXSEG),RMAX(MXSEG),
     $              WID1(MXSEG),WID2(MXSEG),WID3(MXSEG)
      CHARACTER*1   CINT(1),WDID1(4),CTMP(2)
      CHARACTER*8   SEN,LOC,CON,CDSID,CMSID(MXMAP)
      CHARACTER*20  ANAME
      CHARACTER*64  LBASNM(MXMAP)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CLADD,CLPUT,CLPUTG,CLINIT,MAPINI,DTINIT,DTPUT
      EXTERNAL    DTADD,DTGET,INISEG,INTCHR,STNANI,STCURA,MODANI,QUPCAS
      EXTERNAL    CVARAR,CARVAR,PUTWIN
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (2I5,1X,A4,A64)
1002  FORMAT (11X,A4,A64)
1005  FORMAT (I5,10X,A64)
1007  FORMAT (1I5,4X,A1,4X,A1)
1010  FORMAT (16I5)
1015  FORMAT (5X,I5,10X,1I5,5X,1I5)
1030  FORMAT (3(2X,A8),4I5,1F10.3)
1050  FORMAT (4F10.3)
1055  FORMAT (4F10.3,I10)
1060  FORMAT (2I5,F10.3)
1070  FORMAT (F10.3,4I5)
1080  FORMAT (5X,I5,1F10.3,A8)
1090  FORMAT (A8,2X,2(I5,5I3),2(2X,2I3),2X,I2,I4,I2)
1100  FORMAT (6I5,5F10.3)
1110  FORMAT (I5,5X,A20)
1120  FORMAT (A8,2X,A64,1X,I5)
1130  FORMAT (4X,A4,2X,A64)
1140  FORMAT (10X,A64)
C
C     + + + END SPECIFICATIONS + + +
C
      STACOD = 0
      I0 = 0
      I1 = 1
      I4 = 4
      IFL= 98
      OPEN(UNIT=IFL,FILE=FNAME,STATUS='OLD')
C
C     model ID, number of wdm files, WDM file name, WDM file id
      READ(IFL,1000,ERR=20,END=20) MODLID,NWDM,WDID(1),WDNAME(1)
      CALL CVARAR (I4,WDID(1),I4,WDID1)
      CALL QUPCAS (I4,WDID1)
      CALL CARVAR (I4,WDID1,I4,WDID(1))
      IF (WDID(1).EQ.'    ') THEN
C       no wdm id given, give default name
        WDID(1) = 'WDM1'
      END IF
      IF (NWDM.GT.1) THEN
C       using multiple wdm files, read in all the names
        DO 3 I = 2,NWDM
          READ(IFL,1002,ERR=20,END=20) WDID(I),WDNAME(I)
          CALL CVARAR (I4,WDID(I),I4,WDID1)
          CALL QUPCAS (I4,WDID1)
          CALL CARVAR (I4,WDID1,I4,WDID(I))
          IF (WDID(I).EQ.'    ') THEN
C           no wdm id given, give default name
            CALL INTCHR (I,I1,I1,
     O                   J,CINT)
            WDID(I) = 'WDM'// CINT(1)
          END IF
 3      CONTINUE
      END IF
C     line defaults, allow user to edit flag, browse from map click flag
      READ(IFL,1007,ERR=20,END=20) NLINE,CTMP
      IF (CTMP(1).EQ.' ' .OR. CTMP(1).EQ.'1') THEN
        UEDFG = 1
      ELSE
        UEDFG = 0
      END IF
      IF (CTMP(2).EQ.' ' .OR. CTMP(2).EQ.'0') THEN
        BRSFG = 0
      ELSE
        BRSFG = 1
      END IF
      DO 5 I= 1,NLINE
        READ(IFL,1010,ERR=20,END=20) PLCLR(I),PLNTYP(I),PSYMBL(I),
     1                               PPATRN(I)
 5    CONTINUE
      DO 6 I= NLINE+1,MXLINE
        PLCLR(I) = I
        PLNTYP(I)= 1
        PSYMBL(I)= 0
        PPATRN(I)= 0
 6    CONTINUE
C     map device, border, map interation default
      READ(IFL,1010,ERR=20,END=20) MAPDEV,MAPBRD,INTACT
C     number of windows, current map window, current plot window
      READ(IFL,1010,ERR=20,END=20) NWIN,MAPWIN,PLTWIN
C     number of classes
      READ(IFL,1010,ERR=20,END=20) I
      CALL CLINIT
C     class details
      DO 10 J= 1,I
C       sen, loc, const, activefg, symbol, unsel color, sel color
        READ(IFL,1030,ERR=20,END=20) SEN,LOC,CON,ACT,SYM,CUN,CSL,MSIZE
        CALL CLADD(I0,ID)
        CALL CLPUT (ID,SEN,LOC,CON,ACT)
        CALL CLPUTG(ID,SYM,CUN,CSL,MSIZE)
 10   CONTINUE
C     timeseries data
      READ(IFL,1015,ERR=20,END=20) TU,ARHLOG(1),ARHLOG(2)
C     read number of dates
      READ(IFL,1010,ERR=20,END=20) NDATES,CURDAT
      CALL DTINIT
      DO 11 J= 1,NDATES
C       read each set of starting and ending dates
        READ(IFL,1090,ERR=20,END=20) CDSID,SDATE,EDATE,SSDATE,SEDATE,
     1                               DTTU,DTTS,DTRAN
        IF (CURDAT.EQ.J) THEN
C         this date set is active
          ACT = 1
        ELSE
          ACT = 0
        END IF
        CALL DTADD (I0,ID)
C       put specs for each date set into common
        CALL DTPUT (ID,ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     I              DTTS,DTRAN)
 11   CONTINUE
C     get dates for current active date set
      CALL DTGET (CURDAT,
     O            ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TU,
     O            DTTS,DTRAN)
      DO 12 J= 1,NWIN
C       read window dimensions, type
        READ(IFL,1055,ERR=20,END=20) (WINDIM(I,J),I=1,4),WTYPE(J)
        WIPEIT(J) = 0
 12   CONTINUE
C     put window info to common
      CALL PUTWIN (MXWIN,WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C     read size of lettering for map options
      READ(IFL,1070,ERR=20,END=20) LSIZE,SFCOLR,UFCOLR,SBCOLR,UBCOLR
C     read background to fill in and colors for each coverage
C     1-states,2-counties,3-regions,4-accunits,5-allhyd,6-majhyd
      READ(IFL,1010,ERR=20,END=20) BACKCL,MAPCOV
C     read number of maps
      READ(IFL,1010,ERR=20,END=20) NMAPS,CURMAP
      DO 14 I= 1,NMAPS
C       read map details
        READ(IFL,1060,ERR=20,END=20) MPNMFG(I),MPNMCL(I),MPNMSZ(I)
        READ(IFL,1005,ERR=20,END=20) LEGFLG(I),LBASNM(I)
        READ(IFL,1050,ERR=20,END=20) RLAT(1,I),RLAT(2,I),RLNG(1,I),
     1                               RLNG(2,I)
        READ(IFL,1050,ERR=20,END=20) BASEY(I),BASEX(I)
        READ(IFL,1080,ERR=20,END=20) LEGCOL(I),LGSIZE(I),CMSID(I)
 14   CONTINUE
C
C     read animation specifications
C     read number of animations, current active animation
      READ(IFL,1010,ERR=20,END=20) NANIM,CURANI
      CALL STNANI (NANIM)
      CALL STCURA (CURANI)
      DO 15 J=1,NANIM
C       read number of map segment specs, name of this animation
        READ(IFL,1110,ERR=20,END=20) NSEG,ANAME
        DO 150 I= 1,NSEG
C         read map segment details
          READ(IFL,1100,ERR=20,END=20) ATMAJ(I),ATMIN(I),CLR1(I),
     1                   CLR2(I),CLR3(I),DSN(I),RMIN(I),RMAX(I),
     2                   WID1(I),WID2(I),WID3(I)
 150    CONTINUE
        CALL INISEG (J,NSEG,ATMAJ,ATMIN,CLR1,CLR2,CLR3,DSN,RMIN,RMAX,
     1               WID1,WID2,WID3)
        CALL MODANI (J,ANAME)
 15   CONTINUE
C
C     read number of selected data sets
      READ(IFL,1010,ERR=20,END=20) NSELEC
      IF (NSELEC.GT.MXLINE) THEN
C       too many selected, can only have mxline selected data sets
        NSELEC = MXLINE
      END IF
      IF (NSELEC.GT.0) THEN
C       some data sets selected, read selected data set numbers
        READ(IFL,1010,ERR=20,END=20) (SELDSN(I),I=1,NSELEC)
      END IF
C
C     read number of basins available
      READ(IFL,1010,ERR=20,END=20) NBASIN
C     clear out character strings
      DO 160 J = 1,MXBASN
        DO 161 I = 1,MXWDM
          BWDID(J,I) = ' '
          BWDMNM(J,I)= ' '
 161    CONTINUE
 160  CONTINUE
      DO 17 J= 1,NBASIN
C       read each set of basin specs
        READ(IFL,1120,ERR=20,END=20) BASNAM(J),BASDES(J),NBWDM(J)
        IF (NBWDM(J).GT.0) THEN
          DO 170 I= 1,NBWDM(J)
            READ(IFL,1130,ERR=20,END=20) BWDID(J,I),BWDMNM(J,I)
            CALL CVARAR (I4,BWDID(J,I),I4,WDID1)
            CALL QUPCAS (I4,WDID1)
            CALL CARVAR (I4,WDID1,I4,BWDID(J,I))
 170      CONTINUE
        END IF
        READ(IFL,1140,ERR=20,END=20) BUCIPA(J)
 17   CONTINUE
C
C     put map info into common
      CALL MAPINI (NMAPS,MAPDEV,MAPBRD,INTACT,MPNMFG,MPNMCL,
     I             SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL,
     I             MPNMSZ,RLAT,RLNG,LSIZE,CURMAP,
     I             LBASNM,LEGFLG,BASEY,BASEX,LGSIZE,LEGCOL,CMSID)
C
      GO TO 2
C
 20   CONTINUE
C     return error code
      STACOD = 1
 2    CONTINUE
      CLOSE(UNIT=98)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTWR
     I                  (FNAME,MXSEN,
     I                   MXLINE,MXCOV,MXWDM,MXBASN,
     I                   NWDM,WDNAME,WDID,MODLID,UEDFG,BRSFG,
     I                   TU,ARHLOG,
     I                   PLCLR,PLNTYP,PPATRN,PSYMBL,
     I                   SDATE,EDATE,
     I                   MAPCOV,NSELEC,SELDSN,
     I                   NBASIN,NBWDM,BWDID,BASNAM,BASDES,
     I                   BUCIPA,BWDMNM)
C
C     + + + PURPOSE + + +
C     write current state of system
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MXSEN,MXLINE,MODLID,UEDFG,BRSFG,
     $              TU,ARHLOG(2),MXCOV,MXWDM,NWDM,MXBASN,
     $              PLCLR(MXSEN),PLNTYP(MXSEN),
     $              PPATRN(MXSEN),PSYMBL(MXSEN),
     $              SDATE(6),EDATE(6),MAPCOV(MXCOV),
     $              NSELEC,SELDSN(MXLINE),NBASIN,NBWDM(MXBASN)
      CHARACTER*4   WDID(MXWDM),BWDID(MXBASN,MXWDM)
      CHARACTER*8   BASNAM(MXWDM)
      CHARACTER*64  WDNAME(MXWDM),FNAME,BASDES(MXBASN),BUCIPA(MXBASN),
     $              BWDMNM(MXBASN,MXWDM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNAME  - Status file name
C     MXSEN  - Maximum number of scenarios
C     MXLINE - maximum lines on plot
C     MXCOV  - Maximum coverages on map
C     MXWDM  - Maximum number of wdm files
C     WDNAME - Name of each wdm data file
C     NWDM   - number of wdm data files
C     WDID   - id of each wdm data file
C     MODLID - Model ID code - 0:HSPF, 2:DAFLOW
C     UEDFG  - user allowed to edit UCI file flag
C     BRSFG  - browse flag: 1-go to browse screen from map click
C     TU     - time units (1-sec, 2-min, 3-hour, 4-day....)
C     ARHLOG - type of axis - 1- arith, 2-log
C     PLCLR  - line colors for each line
C     PLNTYP - line types for each line
C     PPATRN - fill pattern for plots
C     PSYMBL - symbol type for each line
C     SDATE  - start of plot (yr,mo,dy,hr,mn,sec)
C     EDATE  - end of plot (yr,mo,dy,hr,mn,sec)
C     MAPCOV - color for each map coverage
C     NSELEC - number of selected data sets
C     SELDSN - array of selected data set numbers
C     NBASIN - number of basins available
C     NBWDM  - number of wdm files for this basin
C     BASNAM - basin name
C     BASDES - basin description
C     BUCIPA - basin uci path
C     BWDID  - basin wdm file ids
C     BWDMNM - basin wdm file names
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxmap.inc'
      INCLUDE 'pmxwin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       IFL,I,J,ACT,SYM,CUN,CSL,LEGFLG(MXMAP),LEGCOL(MXMAP),
     $              MAPDEV,MAPBRD,INTACT,MPNMFG(MXMAP),MPNMCL(MXMAP),
     $              CURMAP,SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL(2),NMAPS,
     $              SSDATE(2),SEDATE(2),DTTU,DTTS,DTRAN,NDATES,CURDAT,
     $              ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     $              DSN,NSEG,NANIM,CURANI
      INTEGER       WIPEIT(MXWIN),MAPWIN,PLTWIN,WTYPE(MXWIN),NWIN
      REAL          WINDIM(4,MXWIN)
      REAL          MPNMSZ(MXMAP),LSIZE,RLAT(2,MXMAP),RLNG(2,MXMAP),
     $              BASEY(MXMAP),BASEX(MXMAP),LGSIZE(MXMAP),
     $              MSIZE,RMIN,RMAX,WID1,WID2,WID3
      CHARACTER*8   SEN,LOC,CON,CDSID,CMSID(MXMAP)
      CHARACTER*20  ANAME
      CHARACTER*64  LBASNM(MXMAP)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CLCNT,CLGET,CLGETG,MAPGET,DTGET,DTCNT
      EXTERNAL    GETSEG,GTNSEG,GTNANI,GTCURA,GETAN1,GETWIN
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (2I5,1X,A4,A64)
2002  FORMAT (11X,A4,A64)
2005  FORMAT (I5,10X,A64)
2010  FORMAT (16I5)
2015  FORMAT (5X,I5,10X,1I5,5X,1I5)
2030  FORMAT (3(2X,A8),4I5,1F10.3)
2050  FORMAT (4F10.3)
2055  FORMAT (4F10.3,I10)
2060  FORMAT (2I5,F10.3)
2070  FORMAT (F10.3,4I5)
2080  FORMAT (5X,I5,1F10.3,A8)
2090  FORMAT (A8,2X,2(I5,5I3),2(2X,2I3),2X,I2,I4,I2)
2100  FORMAT (6I5,5F10.3)
2110  FORMAT (I5,5X,A20)
2120  FORMAT (A8,2X,A64,1X,I5)
2130  FORMAT (4X,A4,2X,A64)
2140  FORMAT (10X,A64)
C
C     + + + END SPECIFICATIONS + + +
C
C     get map info from common
      CALL MAPGET (MXMAP,
     O             NMAPS,MAPDEV,MAPBRD,INTACT,MPNMFG,MPNMCL,
     O             SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL,
     O             MPNMSZ,RLAT,RLNG,LSIZE,CURMAP,
     O             LBASNM,LEGFLG,BASEY,BASEX,LGSIZE,LEGCOL,CMSID)
C
      IFL= 98
      OPEN(UNIT=IFL,FILE=FNAME,STATUS='OLD')
C
C     model ID and WDM file name
      WRITE(IFL,2000) MODLID,NWDM,WDID(1),WDNAME(1)
      IF (NWDM.GT.1) THEN
C       multiple wdm files exist
        DO 2 I= 2,NWDM
          WRITE(IFL,2002) WDID(I),WDNAME(I)
 2      CONTINUE
      END IF
      WRITE(IFL,2010) MXLINE,UEDFG,BRSFG
      DO 5 I= 1,MXLINE
        WRITE(IFL,2010) PLCLR(I),PLNTYP(I),PSYMBL(I),PPATRN(I)
 5    CONTINUE
C
C     map device, border, map interation default
      WRITE(IFL,2010) MAPDEV,MAPBRD,INTACT
C     get window specs from common
      CALL GETWIN (MXWIN,
     O             WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C     number of windows, current map window, current plot window
      WRITE(IFL,2010) NWIN,MAPWIN,PLTWIN
C     number of classes
      CALL CLCNT(I)
      WRITE(IFL,2010) I
C     class details
      DO 10 J= 1,I
        CALL CLGET (J,SEN,LOC,CON,ACT)
        CALL CLGETG(J,SYM,CUN,CSL,MSIZE)
        WRITE(IFL,2030) SEN,LOC,CON,ACT,SYM,CUN,CSL,MSIZE
 10   CONTINUE
C     timeseries data
      WRITE(IFL,2015) TU,ARHLOG(1),ARHLOG(2)
C     number of dates and current date set
C     number of classes
      CALL DTCNT(NDATES)
      CURDAT = 0
      DO 11 J= 1,NDATES
C       check for current date set
        CALL DTGET (J,
     O              ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     O              DTTS,DTRAN)
        IF (ACT.EQ.1) THEN
          CURDAT = J
        END IF
 11   CONTINUE
      WRITE(IFL,2010) NDATES,CURDAT
      DO 12 J= 1,NDATES
C       write each set of starting and ending dates
        CALL DTGET (J,
     O              ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     O              DTTS,DTRAN)
        WRITE(IFL,2090) CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,DTTS,DTRAN
 12   CONTINUE
      DO 15 J= 1,NWIN
        WRITE(IFL,2055,ERR=20) (WINDIM(I,J),I=1,4),WTYPE(J)
 15   CONTINUE
C     write size of lettering for map options
      WRITE(IFL,2070) LSIZE,SFCOLR,UFCOLR,SBCOLR,UBCOLR
C     write background to fill in and color
      WRITE(IFL,2010) BACKCL,MAPCOV
C     write number of maps
      WRITE(IFL,2010) NMAPS,CURMAP
      DO 14 I= 1,NMAPS
C       write map details
        WRITE(IFL,2060) MPNMFG(I),MPNMCL(I),MPNMSZ(I)
        WRITE(IFL,2005) LEGFLG(I),LBASNM(I)
        WRITE(IFL,2050) RLAT(1,I),RLAT(2,I),RLNG(1,I),RLNG(2,I)
C       write legend location for this map
        WRITE(IFL,2050) BASEY(I),BASEX(I)
        WRITE(IFL,2080) LEGCOL(I),LGSIZE(I),CMSID(I)
 14   CONTINUE
C
C     write animation specifications
C     write number of animations, current active animation
      CALL GTNANI (NANIM)
      CALL GTCURA (CURANI)
      WRITE(IFL,2010) NANIM,CURANI
      DO 16 J=1,NANIM
C       write number of map segment specs, name of this animation
        CALL GTNSEG (J,
     O               NSEG)
        CALL GETAN1 (J,
     O               ANAME)
        WRITE(IFL,2110) NSEG,ANAME
        DO 160 I= 1,NSEG
C         write map segment details
          CALL GETSEG (J,I,
     O                 ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                 DSN,WID1,WID2,WID3)
          WRITE(IFL,2100) ATMAJ,ATMIN,CLR1,CLR2,CLR3,DSN,
     1                    RMIN,RMAX,WID1,WID2,WID3
 160    CONTINUE
 16   CONTINUE
C
C     write number of selected data set numbers to save
      WRITE(IFL,2010) NSELEC
      IF (NSELEC.GT.0) THEN
C       have some active data sets, write data set numbers
        WRITE(IFL,2010) (SELDSN(I),I=1,NSELEC)
      END IF
C
C     write number of basins available
      WRITE(IFL,2010) NBASIN
      DO 17 J= 1,NBASIN
C       write each set of basin specs
        WRITE(IFL,2120) BASNAM(J),BASDES(J),NBWDM(J)
        IF (NBWDM(J).GT.0) THEN
          DO 170 I= 1,NBWDM(J)
            WRITE(IFL,2130) BWDID(J,I),BWDMNM(J,I)
 170      CONTINUE
        END IF
        WRITE(IFL,2140) BUCIPA(J)
 17   CONTINUE
C
 20   CONTINUE
      CLOSE(UNIT=98)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSGOUT
     I                   (MESSFL,SCLU,WDMSFL,NANAL,ADSN,
     I                    CSCENM,CLOCNM,CCONNM)
C
C     + + + PURPOSE + + +
C     Perform output analysis options for scenario generators.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,NANAL,ADSN(NANAL)
      CHARACTER*8 CSCENM(NANAL),CLOCNM(NANAL),CCONNM(NANAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number for WDM file
C     NANAL  - number of data sets selected for analysis
C     ADSN   - array of data-set numbers
C     CSCENM - Scenario name assoc with data in dataset
C     CLOCNM - Location name assoc with data in dataset
C     CCONNM - Constituent name assoc with data in dataset
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP,RESP
      CHARACTER*8 PTHNAM(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESP, SGLIST, SGWMTT
C
C     + + + END SPECIFICATIONS + + +
C
      PTHNAM(1) = 'AO'
C
 10   CONTINUE
C       do main output menu
        SGRP= 71
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300), RESP
C
 100    CONTINUE
C         list time-series values
          CALL SGLIST (MESSFL,WDMSFL,PTHNAM,NANAL,ADSN,
     I                 CSCENM,CLOCNM,CCONNM)
          GO TO 999
C
 200    CONTINUE
C         table time-series values
          CALL SGWMTT (MESSFL,WDMSFL,PTHNAM,NANAL,ADSN,
     I                 CSCENM,CLOCNM,CCONNM)
          GO TO 999
C
 300    CONTINUE
C         all done with Output
          GO TO 999
C
 999    CONTINUE
C
      IF (RESP.NE.3) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   MLTBAS
     I                   (MESSFL,SCLU,SGRP,MXBASN,MXWDM,NBASIN,BASNAM,
     I                    BASDES,NWDM,NBWDM,WDNAME,WDID,BWDMNM,BWDID,
     I                    BUCIPA,
     M                    LWDM,LWDNAM,LWDID,UCIPTH,BASCUR)
C
C     + + + PURPOSE + + +
C     Do menu for multiple basins.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,SGRP,MXBASN,MXWDM,NBASIN,NWDM,
     1             NBWDM(MXBASN),LWDM,BASCUR
      CHARACTER*4  WDID(MXWDM),BWDID(MXBASN,MXWDM),LWDID(MXWDM)
      CHARACTER*8  BASNAM(MXBASN)
      CHARACTER*64 BASDES(MXBASN),WDNAME(MXWDM),BWDMNM(MXBASN,MXWDM),
     1             LWDNAM(MXWDM),BUCIPA(MXBASN),UCIPTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     SGRP   - group number on message file
C     MXBASN - maximum number of basins
C     MXWDM  - maximum number of wdm files
C     NBASIN - number of basins
C     BASNAM - basin names
C     BASDES - basin decriptions
C     NWDM   - number of wdm files (global)
C     NBWDM  - number of wdm files for each basin
C     WDNAME - wdm file names (global)
C     WDID   - wdm file ids (global)
C     BWDMNM - wdm file names for each basin
C     BWDID  - wdm file ids for each basin
C     LWDM   - number of wdm files available
C     LWDNAM - names of each available wdm file
C     LWDID  - ids of each available wdm file
C     BUCIPA - path to each basin's uci files
C     UCIPTH - path to uci files for this basin
C     BASCUR - current basin number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I8,I52,IRET
      CHARACTER*1  BASOPS(63,5)
C
C     + + + EXTERNALS + + +
      EXTERNAL     CVARAR,QRSPIN,QRESP,ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      I8 = 8
      I52= 52
C
      IF (NBASIN.GT.1) THEN
C       do menu to choose which basin to make active
C       build basin option character strings
        DO 15 I = 1,NBASIN
          CALL CVARAR (I8,BASNAM(I),I8,
     1                 BASOPS(1,I))
          BASOPS(9,I)  = ' '
          BASOPS(10,I) = '-'
          BASOPS(11,I) = ' '
          CALL CVARAR (I52,BASDES(I),I52,
     1                 BASOPS(12,I))
 15     CONTINUE
        I = 63
        CALL QRSPIN (NBASIN,I,BASOPS)
        CALL QRESP (MESSFL,SCLU,SGRP,BASCUR)
        CALL ZGTRET (IRET)
      ELSE
C       only one basin available
        BASCUR = 1
        IRET   = 1
      END IF
C
      IF (IRET.EQ.1) THEN
C       update wdm file variables to include basin specific files
        LWDM = NWDM + NBWDM(BASCUR)
        DO 16 I = 1,NWDM
          LWDNAM(I) = WDNAME(I)
          LWDID(I)  = WDID(I)
 16     CONTINUE
        IF (NBWDM(BASCUR).GT.0) THEN
          DO 17 I = 1,NBWDM(BASCUR)
            IF (NWDM+I.LE.MXWDM) THEN
              LWDNAM(NWDM+I) = BWDMNM(BASCUR,I)
              LWDID(NWDM+I)  = BWDID(BASCUR,I)
            END IF
 17       CONTINUE
        END IF
        UCIPTH = BUCIPA(BASCUR)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDMSET
     I                   (MESSFL,SCLU,SGRP,MXINTV,MXWDM,
     I                    LWDM,LWDNAM,LWDID,
     O                    WDMSFL,WDMERR)
C
C     + + + PURPOSE + + +
C     Set wdm files for scenario generator including global names.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,MXINTV,MXWDM,LWDM,WDMSFL,WDMERR,SGRP
      CHARACTER*4  LWDID(MXWDM)
      CHARACTER*64 LWDNAM(MXWDM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     SGRP   - group number for message
C     MXINTV - interval of data set numbers on each wdm file
C     MXWDM  - maximum number of wdm files
C     LWDM   - number of wdm files available
C     LWDNAM - names of each available wdm file
C     LWDID  - ids of each available wdm file
C     WDMSFL - wdm file unit number, returned as zero if more than one
C     WDMERR - wdm opening error code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,RETCOD,IPOS,I4,IVAL(5),ILEN
      CHARACTER*1  STRIN1(20)
      CHARACTER*8  GLOID
      LOGICAL      IOPEN
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETFUN,WDBOPN,PRNTXI,WIDADD,CVARAR,GSTGLV,WCH2UD
      EXTERNAL     WDFLCL
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I4 = 4
C
      WDMERR = 0
      DO 10 I = 1,LWDM
C       close any file that is already open
        INQUIRE (FILE=LWDNAM(I),OPENED=IOPEN)
        IF (IOPEN) THEN
C         close this wdm file
          CALL WCH2UD (LWDID(I),
     O                 WDMSFL)
          CALL WDFLCL (WDMSFL,
     O                 RETCOD)
        END IF
 10   CONTINUE
      DO 20 I = 1,LWDM
C       open all files with attributes and data
        CALL GETFUN(I1,WDMSFL)
        CALL WDBOPN(WDMSFL,LWDNAM(I),I1,
     O              RETCOD)
        IF (RETCOD .NE. 0) THEN
          CALL PRNTXI(MESSFL,SCLU,SGRP,RETCOD)
          WRITE(99,*) 'Error opening WDM data file.',RETCOD
          WRITE(99,*) '  name: ',LWDNAM(I)
          WDMERR = 1
        END IF
C       send this unit number, spacing interval and wdm file id to common
        CALL WIDADD (WDMSFL,MXINTV,LWDID(I))
C       prepare global variable arrays
        IPOS = ((I-1)*4)+1
        CALL CVARAR (I4,LWDID(I),I4,STRIN1(IPOS))
        IVAL(I) = 0
 20   CONTINUE
C     set global variable for wdm file ids
      GLOID = 'WDID    '
      ILEN  = 4*MXWDM
      CALL GSTGLV (GLOID,I4,ILEN,STRIN1,LWDM,IVAL)
      IF (LWDM.GT.1) THEN
C       multiple wdm files in use, set unit number to zero
        WDMSFL = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DRSCAN
     I                   (WDMSFL,MXLOC,MXLINE,NANAL,ADSN,UCIPTH,DOSTUS,
     O                    NLOC,LAT,LNG,LCID)
C
C     + + + PURPOSE + + +
C     Scan time series directory.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MXLINE,WDMSFL,MXLOC,NANAL,ADSN(MXLINE),NLOC,
     1             LCID(MXLOC),DOSTUS
      REAL         LAT(MXLOC),LNG(MXLOC)
      CHARACTER*64 UCIPTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - wdm file unit number
C     MXLOC  - max number of map station locations
C     NANAL  - number of selected analysis data sets
C     ADSN   - arruy of selected data sets
C     NLOC   - number of locations
C     LAT    - latitude of each location
C     LNG    - longitude of each location
C     LCID   - integer id for each location
C     UCIPTH - path to uci files
C     DOSTUS - do status bar flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,I,MKSTAT(500)
      CHARACTER*8  CLOCID(500)
      CHARACTER*40 LCNAME(500)
C
C     + + + EXTERNALS + + +
      EXTERNAL     TSDRRE,TSDSAS,TSDSVL,STACOM
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     fill timeseries directory common block (dont worry about dates)
      CALL TSDRRE (WDMSFL,I0,DOSTUS)
C     activate saved data sets
      IF (NANAL.GT.0) THEN
C       have some active
        DO 3 I = 1,NANAL
          CALL TSDSAS (ADSN(I),I1)
 3      CONTINUE
      END IF
C     put valid scen, location, and constit names in global valid
C     and return number of locations, lat/lngs, station names for map
      CALL TSDSVL (WDMSFL,MXLOC,DOSTUS,UCIPTH,
     O             NLOC,LAT,LNG,MKSTAT,LCID,LCNAME,CLOCID)
C     put location info into common for mapping
      CALL STACOM (NLOC,LCNAME,CLOCID)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ANASET
     I                   (MXLINE,NANAL,ADSN,
     M                    TU,TS,SDATE,EDATE,
     O                    CSCENM,CRCHNM,CCONNM,GRPMAX)
C
C     + + + PURPOSE + + +
C     Set up analysis getting needed info from selected data sets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MXLINE,NANAL,ADSN(MXLINE),TU,TS,SDATE(6),EDATE(6),
     1             GRPMAX
      CHARACTER*8  CSCENM(MXLINE),CRCHNM(MXLINE),CCONNM(MXLINE)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXLINE - max number of analysis sets
C     NANAL  - number of analysis sets
C     ADSN   - array of analysis sets
C     TU     - time units for analysis
C     TS     - time step for analysis
C     SDATE  - starting date
C     EDATE  - ending date
C     CSCENM - scenario names for each analysis set
C     CRCHNM - location names for each analysis set
C     CCONNM - constituent names for each analysis set
C     GRPMAX - Max group size
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      DTU,DTS,DSDATE(6),DEDATE(6),TSTEPF,TCDCMP,DFLAG
C
C     + + + EXTERNALS + + +
      EXTERNAL     SGTSUT,CMPTIM,CKDATE
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NANAL .GT. 0) THEN
C       set up analysis
C       get info about selected datasets
        CALL SGTSUT (NANAL,ADSN,
     O               CSCENM,CRCHNM,CCONNM,
     O               DTU,DTS,DSDATE,DEDATE,GRPMAX)
C       check time step, time units, start/end dates versus user prefs.
        CALL CMPTIM (DTU,DTS,TU,TS,
     O               TSTEPF,TCDCMP)
        IF (TCDCMP.EQ.2) THEN
C         user pref. time step too small for data, use default
          TS = DTS
          TU = DTU
        END IF
        CALL CKDATE (DSDATE,SDATE,
     O               DFLAG)
        IF (DFLAG.EQ.1) THEN
C         user pref. starting date too early, use default
          SDATE(1) = DSDATE(1)
          SDATE(2) = DSDATE(2)
          SDATE(3) = DSDATE(3)
          SDATE(4) = DSDATE(4)
          SDATE(5) = DSDATE(5)
          SDATE(6) = DSDATE(6)
        END IF
        CALL CKDATE (DEDATE,EDATE,
     O               DFLAG)
        IF (DFLAG.EQ.-1) THEN
C         user pref. ending date too late, use default
          EDATE(1) = DEDATE(1)
          EDATE(2) = DEDATE(2)
          EDATE(3) = DEDATE(3)
          EDATE(4) = DEDATE(4)
          EDATE(5) = DEDATE(5)
          EDATE(6) = DEDATE(6)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SGTSUT
     I                   (NDSN,DSN,
     O                    CSCENM,CRCHNM,CCONNM,
     O                    TU,TS,SDATE,EDATE,GRPMAX)
C
C     + + + PURPOSE + + +
C     determine information about specified data sets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NDSN,DSN(NDSN),
     $            TU,TS,SDATE(6),EDATE(6),GRPMAX
      CHARACTER*8 CSCENM(NDSN),CRCHNM(NDSN),CCONNM(NDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDSN   - Number of datasets to list
C     DSN    - Dataset numbers containing data to list
C     CSCENM - Name of scenario associated with DSN
C     CRCHNM - Name of reach associated with DSN
C     CCONNM - Name of constituent associated with DSN
C     TU     - Default timeunits
C     TS     - Default timestep
C     SDATE  - Default start date
C     EDATE  - Default end date
C     GRPMAX - Max group size
C
C     + + + PARAMETERS + + +
      INTEGER   MAXD
C     maximum number of datasets
      PARAMETER (MAXD = 10)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,FLG,GRPSIZ,
     $            LTS(MAXD),LTU(MAXD),MTS,MTU,
     $            SDATIM(6,MAXD),EDATIM(6,MAXD),TSTEPF,TCDCMP
C
C     + + + EXTERNALS + + +
      EXTERNAL  TSDSPC, CMPTIM, DLIMIT, CKDATE
C
C     + + + END SPECIFICATIONS + + +
C
      DO 4 I = 1,MAXD
        EDATIM(6,I) = 0
        SDATIM(6,I) = 0
 4    CONTINUE
C
      GRPMAX = 1
      DO 110 I= 1,NDSN
C       get info about dataset
        CALL TSDSPC (DSN(I),
     O               CSCENM(I),CRCHNM(I),CCONNM(I),
     O               LTU(I),LTS(I),SDATIM(1,I),EDATIM(1,I),
     O               GRPSIZ)
        IF (GRPSIZ.GT.GRPMAX) THEN
C         new max group size
          GRPMAX = GRPSIZ
        END IF
 110  CONTINUE
C
C     find smallest delta time
      MTS= LTS(1)
      MTU= LTU(1)
      IF (NDSN .GT. 1) THEN
        DO 120 I = 2,NDSN
          CALL CMPTIM (MTU,MTS,LTU(I),LTS(I),
     O                 TSTEPF,TCDCMP)
          IF (TCDCMP .EQ. 2) THEN
C           next timestep shorter, so change minimum
            MTS= LTS(I)
            MTU= LTU(I)
          END IF
 120    CONTINUE
      END IF
      TU = MTU
      TS = MTS
C
C     find latest start date
      J= 2
      CALL DLIMIT (SDATIM,NDSN,J,SDATE)
C
C     find earliest end date
      J= 1
      CALL DLIMIT (EDATIM,NDSN,J,EDATE)
C
C     check start vs end date
      CALL CKDATE (SDATE,EDATE,
     O             FLG)
      IF (FLG .EQ. 1) THEN
C       end date before start date, default to first dsn
        DO 140 I = 1,6
          SDATE(I) = SDATIM(I,1)
          EDATE(I) = EDATIM(I,1)
 140    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSGANA
     I                   (MESSFL,SCLU,MXLINE,WDMSFL,BRSFG,
     I                    MAPFLG,MXLOC,WDMPFL,MXCOV,MXSEN,
     M                    NANAL,ADSN,TU,TS,SDATE,EDATE,INIDUR,
     M                    PLNTYP,PLCLR,PPATRN,
     M                    PSYMBL,DTRAN,PLTDEV,ARHLOG,
     M                    NLOC,LCID,LAT,LNG,
     M                    LOCXFG,MAPCOV,
     M                    CSCENM,CRCHNM,CCONNM)
C
C     + + + PURPOSE + + +
C     do analysis menu
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,MXLINE,NANAL,ADSN(MXLINE),TU,TS,SDATE(6),
     1            EDATE(6),WDMSFL,MAPFLG,MXLOC,WDMPFL,MXCOV,BRSFG,
     2            INIDUR,MXSEN,PLNTYP(MXSEN),PLCLR(MXSEN),
     3            PPATRN(MXSEN),PSYMBL(MXSEN),DTRAN,
     3            PLTDEV,ARHLOG(2),NLOC,LCID(MXLOC),
     4            LOCXFG,MAPCOV(MXCOV)
      REAL        LAT(MXLOC),LNG(MXLOC)
      CHARACTER*8 CSCENM(MXLINE),CRCHNM(MXLINE),CCONNM(MXLINE)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     WDMPFL - map boundary file unit number
C     MXSEN  - Maximum number of scenarios
C     MXLOC  - Maximum number of locations
C     MXLINE - maximum lines on plot
C     BRSFG  - browse flag: 1-go to browse screen from map click
C     MXCOV  - Maximum coverages on map
C     NLOC   - number of active locations
C     LCID   - location ids
C     LAT    - latitude of each location
C     LNG    - longitude of each location
C     TU     - time units (1-sec, 2-min, 3-hour, 4-day....)
C     TS     - time step
C     ARHLOG - type of axis - 1- arith, 2-log
C     PLCLR  - line colors for each line
C     PLNTYP - line types for each line
C     PPATRN - fill pattern for plots
C     PSYMBL - symbol type for each line
C     SDATE  - start of plot (yr,mo,dy,hr,mn,sec)
C     EDATE  - end of plot (yr,mo,dy,hr,mn,sec)
C     MAPCOV - color for each map coverage
C     NANAL  - number of selected data sets
C     ADSN   - array of selected data set numbers
C     WDMSFL - wdm file unit number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       GRPMAX,SGRP,RESP,I0,I1,IM1
      CHARACTER*8   PTHNAM(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL      ANASET,QRESP,SGPLOT,PRNTXT,DURANI,WSGOUT,TSCMPR
      EXTERNAL      SGFREQ,GTADSN,PRWMAP,SSPECI,SELDSN
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      IM1= -1
C     set up analysis getting needed info from selected data sets
      CALL ANASET (MXLINE,NANAL,ADSN,
     M             TU,TS,SDATE,EDATE,
     O             CSCENM,CRCHNM,CCONNM,GRPMAX)
C
 5    CONTINUE
C       display analysis option menu
        SGRP = 30
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        GO TO (10,20,30,40,50,60,70,80,100),RESP
C
 10     CONTINUE
C         graph selected stations timeseries data
          IF (NANAL.GT.0) THEN
C           analysis has been specified, okay to graph
            CALL SGPLOT (MESSFL,WDMSFL,NANAL,ADSN,
     I                   CSCENM,CRCHNM,CCONNM,GRPMAX,
     M                   PLNTYP,PLCLR,PPATRN,
     M                   PSYMBL,TU,TS,SDATE,EDATE,DTRAN,
     M                   PLTDEV,ARHLOG)
          ELSE
C           tell user no analysis specified yet
            SGRP = 90
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 110
C
 20     CONTINUE
C         duration
          IF (NANAL.GT.0) THEN
C           analysis items specified, ok to do duranl
            CALL DURANI (MESSFL,WDMSFL,CSCENM(1),
     I                   CRCHNM(1),CCONNM(1),
     M                   INIDUR,NANAL,MXLINE,ADSN)
          ELSE
C           tell user no analysis specified yet
            SGRP = 92
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 110
C
 30     CONTINUE
C         output
          IF (NANAL.GT.0) THEN
C           analysis items specified, ok to do output
            CALL WSGOUT (MESSFL,SCLU,WDMSFL,NANAL,ADSN,
     I                   CSCENM,CRCHNM,CCONNM)
          ELSE
C           tell user no analysis specified yet
            SGRP = 91
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 110
C
 40     CONTINUE
C         compare two time series
          IF (NANAL.GT.0) THEN
C           analysis items specified, ok to compare
            PTHNAM(1)= 'A'
            CALL TSCMPR (MESSFL,WDMSFL,I1,I0,PTHNAM,
     M                   NANAL,MXLINE,ADSN)
          ELSE
C           tell user no analysis specified yet
            SGRP = 93
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 110
C
 50     CONTINUE
C         frequency
          IF (NANAL.GT.0) THEN
C           analysis items specified, ok to do frequency analysis
            CALL SGFREQ (MESSFL,WDMSFL,NANAL,ADSN,I1,CSCENM,CRCHNM,
     I                   CCONNM)
          ELSE
C           tell user no analysis specified yet
            SGRP = 94
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 110
C
 60     CONTINUE
C         map
          IF (MAPFLG.EQ.1) THEN
C           map data exists, do map
            PTHNAM(1)= 'A'
            CALL PRWMAP (MESSFL,MXLOC,IM1,WDMPFL,MXCOV,
     I                   PTHNAM(1),BRSFG,
     M                   NLOC,LCID,LAT,LNG,LOCXFG,MAPCOV)
          ELSE
C           no boundaries to map
            SGRP= 9
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            RESP= 2
          END IF
C         get active and selected datasets
          CALL GTADSN(MXLINE,
     O                NANAL,ADSN)
C         set up analysis getting needed info from selected data sets
          CALL ANASET (MXLINE,NANAL,ADSN,
     M                 TU,TS,SDATE,EDATE,
     O                 CSCENM,CRCHNM,CCONNM,GRPMAX)
          GO TO 110
C
 70     CONTINUE
C         pick scenario, location, constituent data sets
          LOCXFG = 1
          PTHNAM(1)= 'A'
          CALL SSPECI (MESSFL,PTHNAM)
C         get active and selected datasets
          CALL GTADSN(MXLINE,
     O                NANAL,ADSN)
C         set up analysis getting needed info from selected data sets
          CALL ANASET (MXLINE,NANAL,ADSN,
     M                 TU,TS,SDATE,EDATE,
     O                 CSCENM,CRCHNM,CCONNM,GRPMAX)
          GO TO 110
C
 80     CONTINUE
C         select data sets for analysis by number
          CALL SELDSN (MESSFL,SCLU,MXLINE,
     M                 NANAL,ADSN)
C         set up analysis getting needed info from selected data sets
          CALL ANASET (MXLINE,NANAL,ADSN,
     M                 TU,TS,SDATE,EDATE,
     O                 CSCENM,CRCHNM,CCONNM,GRPMAX)
          GO TO 110
C
 100    CONTINUE
C         done, return one level up
          GO TO 110
C
 110    CONTINUE
      IF (RESP .LE. 8) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   DOBANN
     I                   (MESSFL,SCLU,HEADNM,NUMHED,INITFG,FLGCHR)
C
C     + + + PURPOSE + + +
C     do usgs banner
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,NUMHED,INITFG
      CHARACTER*1   FLGCHR
      CHARACTER*78  HEADNM(3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     HEADNM - header text
C     NUMHED - number of headers needed
C     INITFG - flag indicating if screen needs to be initialized
C     FLGCHR - flag character for special situations
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP,I1,IM1,LINCNT,I78,CLEN(1)
      CHARACTER*1  CVAL1(78)
C
C     + + + EXTERNALS + + +
      EXTERNAL     PMXCNW, CVARAR, PMXTXA
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      IM1= -1
      I78= 78
      IF (FLGCHR.EQ.' ' .OR. FLGCHR.EQ.'a') THEN
C       send first line of header (hard coded version) to screen
        IF (FLGCHR.EQ.' ') THEN
C         normal usgs banner needed
          IF (INITFG.EQ.-1) THEN
C           need different groups to handle spacing differently
            SGRP= 15
          ELSE
            SGRP= 2
          END IF
        ELSE IF (FLGCHR.EQ.'a') THEN
C         aqua terra banner needed
          IF (INITFG.EQ.-1) THEN
C           need different groups to handle spacing differently
            SGRP= 61
          ELSE
            SGRP= 60
          END IF
        END IF
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,INITFG,IM1,LINCNT)
C       send second line of header to screen
        IF (INITFG.EQ.-1) THEN
C         need different groups to handle spacing differently
          SGRP= 16
        ELSE
          SGRP= 3
        END IF
        CALL CVARAR (I78,HEADNM(1),I78,CVAL1)
        CLEN(1) = 78
        CALL PMXTXA (MESSFL,SCLU,SGRP,I1,IM1,IM1,I1,CLEN,CVAL1)
        IF (NUMHED.EQ.2) THEN
C         send third line of header to screen
          CALL CVARAR (I78,HEADNM(2),I78,CVAL1)
          CLEN(1) = 78
          CALL PMXTXA (MESSFL,SCLU,SGRP,I1,IM1,IM1,
     I                 I1,CLEN,CVAL1)
        END IF
      ELSE IF (FLGCHR.EQ.'c') THEN
C       custom header
        IF (INITFG.EQ.-1) THEN
C         need different groups to handle spacing differently
          SGRP= 16
        ELSE
          SGRP= 3
        END IF
        CALL CVARAR (I78,HEADNM(1),I78,CVAL1)
        CLEN(1) = 78
        CALL PMXTXA (MESSFL,SCLU,SGRP,I1,INITFG,IM1,I1,CLEN,CVAL1)
        IF (NUMHED.GT.1) THEN
C         send second line of header to screen
          CALL CVARAR (I78,HEADNM(2),I78,CVAL1)
          CLEN(1) = 78
          CALL PMXTXA (MESSFL,SCLU,SGRP,I1,IM1,IM1,
     I                 I1,CLEN,CVAL1)
        END IF
        IF (NUMHED.GT.2) THEN
C         send third line of header to screen
          CALL CVARAR (I78,HEADNM(3),I78,CVAL1)
          CLEN(1) = 78
          CALL PMXTXA (MESSFL,SCLU,SGRP,I1,IM1,IM1,
     I                 I1,CLEN,CVAL1)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SELDSN
     I                    (MESSFL,SCLU,MXLINE,
     M                     NANAL,ADSN)
C
C     + + + PURPOSE + + +
C     do screen to select data sets by number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,MXLINE,NANAL,ADSN(MXLINE)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     MXLINE - max number of data sets
C     NANAL  - number of data sets for analysis
C     ADSN   - array of data set numbers for analysis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LDSN(10),I,I0,IRET,SGRP,I1,SDSN(10),CHGFG,EXFLG,I4
C
C     + + + EXTERNALS + + +
      EXTERNAL    GTADSN,ZIPI,QGETI,QSETI,Q1INIT,Q1EDIT,ZSTCMA
      EXTERNAL    COPYI,TSDSAS,PMXTXI,TSDSEX
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
C
C     initialize local array of data sets
      CALL ZIPI (MXLINE,I0,LDSN)
C
C     make previous available
      I= 4
      CALL ZSTCMA (I,I1)
C
      SGRP = 40
      CALL Q1INIT (MESSFL,SCLU,SGRP)
C
      IF (NANAL.GT.0) THEN
C       have some selected data sets, put in buffer
        DO 10 I = 1,NANAL
          LDSN(I) = ADSN(I)
 10     CONTINUE
      END IF
C     save copy of dsn array
      CALL COPYI (MXLINE,LDSN,SDSN)
C
      CALL QSETI (MXLINE,LDSN)
C
      CALL Q1EDIT (IRET)
      IF (IRET.NE.2) THEN
C       user wants to continue
        CALL QGETI (MXLINE,LDSN)
C       check to see if user changed anything
        CHGFG = 0
        DO 20 I = 1,MXLINE
          IF (LDSN(I).NE.SDSN(I)) THEN
C           user changed this
            CHGFG = 1
C           unselect the old dsn
            CALL TSDSAS (SDSN(I),I0)
            IF (LDSN(I).GT.0) THEN
C             see if this data set exists
              CALL TSDSEX (LDSN(I),EXFLG)
              IF (EXFLG.GT.0) THEN
C               select this new dsn
                CALL TSDSAS (LDSN(I),I1)
              ELSE
C               cant select this data set, give message
C               make previous unavailable
                CALL ZSTCMA (I4,I0)
                SGRP = 41
                CALL PMXTXI (MESSFL,SCLU,SGRP,MXLINE,I1,I0,I1,LDSN(I))
C               make previous available
                CALL ZSTCMA (I4,I1)
              END IF
            END IF
          END IF
 20     CONTINUE
C
        IF (CHGFG.EQ.1) THEN
C         return active and selected datasets in case list changed
          CALL GTADSN (MXLINE,
     O                 NANAL,ADSN)
        END IF
      END IF
C
C     make previous unavailable
      I= 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
