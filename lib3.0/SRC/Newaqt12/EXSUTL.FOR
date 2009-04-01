C
C
C
      SUBROUTINE   HSXINI
     I                   (MXSITE,
     O                    ISUBJC,RERTRM,
     O                    STATDN,RHSDT1)
C
C     + + + PURPOSE + + +
C     initialize the state of the expert system
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MXSITE,ISUBJC(23),STATDN(MXSITE)
      REAL         RERTRM(10),RHSDT1(33*MXSITE)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXSITE - max number of calibration sites
C     ISUBJC - array of subjective judgements
C     RERTRM - array of error terms
C     STATDN - array of stats calculated flags
C     RHSDT1 - array of error terms for each site
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxmap.inc'
      INCLUDE 'pmxseg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INUM,IVAL
      REAL      RVAL
      INTEGER       ACT,SYM,CUN,CSL,I0,I1,ID,IDSN(1)
      INTEGER       MAPDEV,MAPBRD,INTACT,
     $              SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL(2),
     $              NMAPS,CURMAP,SSDATE(2),SEDATE(2),DTTU,DTTS,
     $              ATMAJ(MXSEG),ATMIN(MXSEG),CLR1(MXSEG),CLR2(MXSEG),
     $              MAPDUM(1),
     $              CLR3(MXSEG),DTRAN,SDATE(6),EDATE(6)
      REAL          MSIZE,
     $              RLAT2(2,MXMAP),RLNG2(2,MXMAP),LSIZE(1),
     $              BASEX(MXMAP),BASEY(MXMAP),RMIN(MXSEG),RMAX(MXSEG),
     $              WID1(MXSEG),WID2(MXSEG),WID3(MXSEG)
      CHARACTER*8   CMSID(MXMAP),CBLNK
      CHARACTER*20  ANAME
      CHARACTER*64  BASNAM(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPR, ZIPI
      EXTERNAL    CLADD,CLPUT,CLPUTG,DTINIT,DTPUT,MAPINI
      EXTERNAL    DTADD,INISEG,STNANI,STCURA,MODANI
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize error terms to defaults
      RERTRM(1) = 10.
      RERTRM(2) = 0.01
      RERTRM(3) = 10.
      RERTRM(4) = 15.
      RERTRM(5) = 15.
      RERTRM(6) = 2.5
      RERTRM(7) = 10.
      RERTRM(8) = 15.
      RERTRM(9) = 1.5
      RERTRM(10)= 30.
C     initialize subjective answers to undefined
      IVAL= -1
      INUM= 23
      CALL ZIPI (INUM,IVAL,ISUBJC)
C     initialize hspf output to undefined
      RVAL= -999.0
      INUM= 33*MXSITE
      CALL ZIPR (INUM,RVAL,RHSDT1)
C     set all stat-done flags to zero since no stats have been
C     calculated yet
      INUM= MXSITE
      IVAL= 0
      CALL ZIPI (INUM,IVAL,STATDN)
C
      I0 = 0
      I1 = 1
C     init map border
      MAPBRD = 2
C     init map device
      MAPDEV = 1
C     init map interact flag
      INTACT = 1
      RLAT2(1,1) = 24
      RLAT2(2,1) = 50
      RLNG2(1,1) = 66
      RLNG2(2,1) = 125
C     init map
      NMAPS = 1
      CURMAP= 1
      MAPDUM(1)= 0
      LSIZE(1) = 0.01
      SFCOLR= 1
      UFCOLR= 7
      SBCOLR= 0
      UBCOLR= 0
      BACKCL(1) = 0
      BACKCL(2) = 0
      BASEY(1) = 0.0
      BASEX(1) = 0.0
      BASNAM(1)= ' '
      CMSID(1) = ' '
      CALL MAPINI (NMAPS,MAPDEV,MAPBRD,INTACT,MAPDUM,MAPDUM,
     I             SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL,
     I             LSIZE,RLAT2,RLNG2,LSIZE(1),CURMAP,
     I             BASNAM,MAPDUM,BASEY,BASEX,LSIZE(1),MAPDUM,
     I             CMSID)
C     init subsets of wdm file
      CALL CLADD(I0,ID)
      CBLNK = '        '
      ACT   = 1
      SYM   = 3
      CUN   = 4
      CSL   = 5
      MSIZE = 2.0
      CALL CLPUT (ID,CBLNK,CBLNK,CBLNK,ACT)
      CALL CLPUTG(ID,SYM,CUN,CSL,MSIZE)
C     init date sets
      CALL DTINIT
      ACT = 1
      SDATE(1) = 1950
      SDATE(2) = 1
      SDATE(3) = 1
      SDATE(4) = 0
      SDATE(5) = 0
      SDATE(6) = 0
      EDATE(1) = 1990
      EDATE(2) = 12
      EDATE(3) = 31
      EDATE(4) = 24
      EDATE(5) = 0
      EDATE(6) = 0
      SSDATE(1)= 1
      SSDATE(2)= 1
      SEDATE(1)= 12
      SEDATE(2)= 31
      DTTU     = 4
      DTTS     = 1
      DTRAN    = 1
      CALL DTADD (I0,ID)
      CALL DTPUT (ID,ACT,CBLNK,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     I            DTTS,DTRAN)
C     init animations
      CALL STNANI (I1)
      CALL STCURA (I1)
      ATMAJ(1) = 1
      ATMIN(1) = 1
      CLR1(1)  = 4
      CLR2(1)  = 3
      CLR3(1)  = 2
      IDSN(1)  = 101
      RMIN(1)  = 50
      RMAX(1)  = 100
      WID1(1)  = 1
      WID2(1)  = 1
      WID3(1)  = 1
      CALL INISEG (I1,I1,ATMAJ,ATMIN,CLR1,CLR2,CLR3,IDSN,RMIN,RMAX,
     1             WID1,WID2,WID3)
      ANAME    = 'DEFAULT             '
      CALL MODANI (I1,ANAME)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HSXGET
     I                   (IFL,MXSEN,MXCOV,MXLINE,MXCAL,MXSITE,MXSTRM,
     I                    MXWDM,EMFG,
     M                    PLCLR,PLNTYP,PSYMBL,PPATRN,ARHLOG,MAPCOV,
     O                    RLAT,RLNG,UCIPTH,IVAR,RPARM1,RPARM2,
     O                    RDERR,VARCNT,UNCALD,RCHNGE,RERROR,IPCTFG,
     O                    ISUBJC,RERTRM,NSTATS,STAT1,SITENM,CALINM,
     O                    BAREA,NSITE,CURSIT,DATCNT,NWDM,DCNVFG,
     O                    STSTRM,ENSTRM,STATDN,SITNAM,PLTPRE,LWDID,
     O                    LWDNAM,CCAL,DSN,TCNV,RHSDT1,NANAL,ADSN,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Get the state of the expert system for a basin from a file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IFL,RETCOD,MXSEN,MXCOV,MXLINE,MXCAL,MXSITE,MXSTRM,
     $             MXWDM,PLCLR(MXSEN),PLNTYP(MXSEN),PPATRN(MXSEN),
     $             PSYMBL(MXSEN),ARHLOG(2),MAPCOV(MXCOV),IPCTFG(12)
      INTEGER      IVAR(2),VARCNT(2),UNCALD,NSTATS,ISUBJC(23),EMFG
      INTEGER      NSITE,CURSIT,DATCNT,NWDM,DCNVFG,DSN(10,MXSITE),
     $             STSTRM(6,MXSTRM),ENSTRM(6,MXSTRM),STATDN(MXSITE),
     $             NANAL,ADSN(MXLINE)
      REAL         RPARM1(100),RPARM2(100),RDERR(100,19),RERTRM(10)
      REAL         RLAT(2),RLNG(2),RCHNGE(12),RERROR(12,19),
     $              STAT1(MXCAL*MXSITE,33),BAREA(MXSITE),TCNV(MXSITE),
     $              RHSDT1(33*MXSITE)
      CHARACTER*1   SITNAM(20,MXSITE)
      CHARACTER*4   LWDID(MXWDM)
      CHARACTER*8   SITENM(MXCAL*MXSITE),CALINM(MXCAL*MXSITE)
      CHARACTER*8   PLTPRE,CCAL
      CHARACTER*64  UCIPTH,LWDNAM(MXWDM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EMFG   - english/metric flag
C     IFL    - basin description file unit number
C     RETCOD - return code
C     MXSEN  - Maximum number of scenarios
C     MXLINE - maximum lines on plot
C     MXCAL  - maximum number of calibrations
C     MXSITE - maximum number of sites
C     MXCOV  - Maximum coverages on map
C     MXSTRM - maximum number of storms
C     MXWDM  - maximum number of wdm files
C     ARHLOG - type of axis - 1- arith, 2-log
C     MAPCOV - color for each map coverage
C     PLCLR  - line colors for each line
C     PLNTYP - line types for each line
C     PPATRN - fill pattern for plots
C     PSYMBL - symbol type for each line
C     TCNV   - conversion factors for each site
C     RLAT   - latitude limits for map
C     RLNG   - longitude limits for map
C     UCIPTH - path to uci files
C     NANAL  - number of selected data sets
C     ADSN   - array of selected dats sets
C     RHSDT1 - array of current output stats for each site
C     IVAR   - two variables for double sensitivity analysis
C     RPARM1 - array of parameters for double sensitivity analysis
C     RPARM2 - array of parameters for double sensitivity analysis
C     RDERR  - array of error terms for double sensitivity analysis
C     VARCNT - count of each variable in double sensitivity analysis
C     UNCALD - uncalculated double sensitivity analysis flag
C     RCHNGE - array of changes for sensitivity analysis
C     RERROR - error terms from sensitivity analysis
C     IPCTFG - flags indicating absolute or percent sens anal changes
C     ISUBJC - array of subjectives
C     RERTRM - array of real current error terms
C     NSTATS - number of sets of stats calculated
C     STAT1  - array of stats calculated per site and calibration run
C     SITENM - array of site names for each sets of stats
C     CALINM - array of calibration names for each set of stats
C     BAREA  - array of basin areas for each site
C     DSN    - array of data set numbers for each site
C     NSITE  - number of sites for this basin
C     CURSIT - current site number
C     DATCNT - count of storm dates
C     NWDM   - number of wdm files
C     DCNVFG - data conversion flag
C     STSTRM - storm start dates
C     ENSTRM - storm end dates
C     STATDN - array of flags indicating if stats calc for each site
C     SITNAM - array of site names
C     PLTPRE - plot file name prefix
C     LWDID  - array of wdm ids
C     LWDNAM - array of wdm file names
C     CCAL   - name of current calibration
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxmap.inc'
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxwin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,I1,OLEN,FOUND,ISITE,I8,IPOS
      CHARACTER*1 CSITE(4),BLNK
      CHARACTER*8 TMPSIT
      INTEGER       ID,ACT,SYM,CUN,CSL,NDATES,CURDAT
      INTEGER       MAPDEV,MAPBRD,INTACT,MPNMFG(MXMAP),MPNMCL(MXMAP),
     $              SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL(2),LEGCOL(MXMAP),
     $              NLINE,NMAPS,CURMAP,SSDATE(2),SEDATE(2),DTTU,DTTS,
     $              ATMAJ(MXSEG),ATMIN(MXSEG),CLR1(MXSEG),CLR2(MXSEG),
     $              IDSN(MXSEG),NSEG,LEGFLG(MXMAP),NANIM,CURANI,
     $              CLR3(MXSEG),DTRAN,SDATE(6),EDATE(6),TU
      INTEGER       WIPEIT(MXWIN),MAPWIN,PLTWIN,WTYPE(MXWIN),NWIN
      REAL          WINDIM(4,MXWIN),CONVRT
      REAL          MPNMSZ(MXMAP),LSIZE,LGSIZE(MXMAP),MSIZE,
     $              R2LAT(2,MXMAP),R2LNG(2,MXMAP),
     $              BASEX(MXMAP),BASEY(MXMAP),RMIN(MXSEG),RMAX(MXSEG),
     $              WID1(MXSEG),WID2(MXSEG),WID3(MXSEG)
      CHARACTER*8   SEN,LOC,CON,CDSID,CMSID(MXMAP)
      CHARACTER*20  ANAME
      CHARACTER*64  LBASNM(MXMAP)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, CHRCHR, INTCHR, ZIPI, ZIPC, CARVAR
      EXTERNAL    CLADD,CLPUT,CLPUTG,CLINIT,DTINIT,DTPUT
      EXTERNAL    DTADD,INISEG,STNANI,STCURA,MODANI,MAPINI
      EXTERNAL    PUTWIN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CSITE /'S','i','t','e'/
      DATA BLNK /' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (10F8.2)
 1005 FORMAT (10F10.3)
 1007 FORMAT (1F10.3,I5)
 1010 FORMAT (20I4)
 1020 FORMAT (A8,2I5,4F8.1)
 1030 FORMAT (2(I5,5I3))
 1040 FORMAT (10F8.0)
 1050 FORMAT (41X,I2,2X,20A1)
 1060 FORMAT (A4,1X,A64)
 1070 FORMAT (A8,2X,A8)
 1105 FORMAT (I5,10X,A64)
 1110 FORMAT (16I5)
 1115 FORMAT (5X,I5,10X,1I5,5X,1I5)
 1130 FORMAT (3(2X,A8),4I5,1F10.3)
 1150 FORMAT (4F10.3)
 1155 FORMAT (4F10.3,I10)
 1160 FORMAT (2I5,F10.3)
 1170 FORMAT (F10.3,4I5)
 1180 FORMAT (5X,I5,1F10.3,A8)
 1190 FORMAT (A8,2X,2(I5,5I3),2(2X,2I3),2X,I2,I4,I2)
 1200 FORMAT (6I5,5F10.3)
 1210 FORMAT (I5,5X,A20)
 1220 FORMAT (A64)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I8= 8
C
C     read expert system file
C     number of wdm files
      READ(IFL,1010,ERR=91) NWDM
      DO 2 I= 1, NWDM
C       read wdm id and file name
        READ(IFL,1060,ERR=91) LWDID(I),LWDNAM(I)
 2    CONTINUE
      READ(IFL,1220,ERR=91) UCIPTH
C
C     plot file name prefix
      READ(IFL,1020,ERR=91) PLTPRE,NSITE,CURSIT,RLAT,RLNG
      IF (NSITE.EQ.0) THEN
C       number of sites not on exs file, set to 1
        NSITE= 1
      END IF
      IF (CURSIT.EQ.0) THEN
C       current site not on exs file, set to 1
        CURSIT= 1
      END IF
C
      DO 3 I= 1, MXSITE
C       initialize data sets and site names to blank
        J= 20
        CALL ZIPC (J,BLNK,SITNAM(1,I))
        J= 10
        CALL ZIPI (J,I0,DSN(1,I))
 3    CONTINUE
C
      DO 5 I= 1, NSITE
C       read statistics calculated flag and site name for each site
        READ(IFL,1050,ERR=91) STATDN(I),(SITNAM(J,I),J=1,20)
 5    CONTINUE
C
      DO 7 I= 1, MXSITE
C       fill in all site names left blank
        J= 20
        IF (LENSTR(J,SITNAM(1,I)).EQ.0) THEN
C         no site name in exs file, put in dummy
          J= 4
          CALL CHRCHR (J,CSITE,SITNAM(1,I))
C         put order number in site name
          J= 2
          CALL INTCHR (I,J,I1,
     O                 OLEN,SITNAM(6,I))
        END IF
 7    CONTINUE
C     number of storms, data convert flag
      READ(IFL,1010,ERR=91) DATCNT,DCNVFG
C     storm end/start dates
      DO 10 I= 1,DATCNT
        READ(IFL,1030,ERR=91) (STSTRM(J,I),J=1,6),(ENSTRM(J,I),J=1,6)
 10   CONTINUE
      IF (NSITE.LE.10) THEN
C       basin area
        READ(IFL,1040,ERR=91) (BAREA(J),J=1,NSITE)
      ELSE
C       basin area
        READ(IFL,1040,ERR=91) (BAREA(J),J=1,10)
        READ(IFL,1040,ERR=91) (BAREA(J),J=11,MXSITE)
      END IF
C     calc conversion factors for sim data in cfs or inches
      IF (DCNVFG.EQ.1) THEN
C       in cfs or m3/s
        IF (EMFG.EQ.1) THEN
C         conversion cfsd to acre-inches
          CONVRT = 24.0*3600.0*12.0/43560.0
        ELSE IF (EMFG.EQ.2) THEN
C         conversion m3/s-d to hectare-mm
          CONVRT = 24.0*3600.0*1000.0/10000.0
        END IF
        DO 12 J = 1,NSITE
          TCNV(J)= CONVRT/BAREA(J)
 12     CONTINUE
      ELSE
C       in inches or mm
        DO 14 J = 1,NSITE
          TCNV(J)= 1.0
 14     CONTINUE
      END IF
C     error terms
      READ(IFL,1000,ERR=91) RERTRM
      IF (RERTRM(10).LT.1.0E-6 .AND. RERTRM(10).GT.-1.0E-6) THEN
C       percent of time in baseflow read in as zero, change to 30
        RERTRM(10) = 30.0
      END IF
C     name of current calibration, if any
      READ(IFL,1070,ERR=92) CCAL
C
C     statistical data for history option
      READ(IFL,1010,ERR=91) NSTATS
      IF (NSTATS.GT.0) THEN
        DO 20 I= 1, NSTATS
C         read calibration name and site name
          READ(IFL,1070,ERR=91) CALINM(I),SITENM(I)
          READ(IFL,1005,ERR=91) (STAT1(I,J),J=1,8)
          READ(IFL,1005,ERR=91) (STAT1(I,J),J=9,16)
          READ(IFL,1005,ERR=91) (STAT1(I,J),J=17,22)
          READ(IFL,1005,ERR=91) (STAT1(I,J),J=23,27)
          READ(IFL,1005,ERR=91) (STAT1(I,J),J=28,33)
          IF (CALINM(I).EQ.CCAL) THEN
C           will want to store this with current stat values
            FOUND = 0
            ISITE = 1
 25         CONTINUE
              CALL CARVAR (I8,SITNAM(1,ISITE),I8,TMPSIT)
              IF (TMPSIT.EQ.SITENM(I)) THEN
                FOUND = ISITE
              END IF
              ISITE = ISITE + 1
            IF (ISITE.LE.NSITE .AND. FOUND.EQ.0) GO TO 25
            IF (FOUND.GT.0) THEN
C             found where it belongs, put values in
              DO 27 J = 1,33
                IPOS = ((J-1)*MXSITE)+FOUND
                RHSDT1(IPOS) = STAT1(I,J)
 27           CONTINUE
            END IF
          END IF
 20     CONTINUE
      END IF
C     sensitivity analysis data
      DO 30 I = 1,12
        READ(IFL,1007,ERR=91) RCHNGE(I),IPCTFG(I)
        READ(IFL,1005,ERR=91) (RERROR(I,J),J=1,7)
        READ(IFL,1005,ERR=91) (RERROR(I,J),J=8,13)
        READ(IFL,1005,ERR=91) (RERROR(I,J),J=14,19)
 30   CONTINUE
C     two-variable sensitivity data
      READ(IFL,1010,ERR=91) IVAR(1),IVAR(2),VARCNT(1),VARCNT(2)
      IF (IVAR(1).GT.0 .AND. IVAR(2).GT.0) THEN
        DO 35 I = 1,VARCNT(1)*VARCNT(2)
          READ(IFL,1005,ERR=91) RPARM1(I),RPARM2(I),(RDERR(I,J),J=1,5)
          READ(IFL,1005,ERR=91) (RDERR(I,J),J=6,12)
          READ(IFL,1005,ERR=91) (RDERR(I,J),J=13,19)
 35     CONTINUE
        UNCALD = 0
      END IF
C
C     subjective data
      READ(IFL,1010,ERR=91) ISUBJC
C     change subjective data based on other data
      IF (RHSDT1(((CURSIT-1)*33)+9).GT.RHSDT1(((CURSIT-1)*33)+10)) THEN
C       simulated storm runoff volumes higher than obs
        ISUBJC(5)= 1
      ELSE IF (RHSDT1(((CURSIT-1)*33)+9) .LT.
     1         RHSDT1(((CURSIT-1)*33)+10)) THEN
C       simulated storm runoff volumes lower than obs
        ISUBJC(5)= 0
      END IF
C
C     read the rest like genscn .sta file
C     line defaults
      READ(IFL,1110,ERR=91) NLINE
      DO 40 I = 1,NLINE
        READ(IFL,1110,ERR=91) PLCLR(I),PLNTYP(I),PSYMBL(I),
     1                               PPATRN(I)
 40   CONTINUE
      DO 41 I= NLINE+1,MXLINE
        PLCLR(I) = I
        PLNTYP(I)= 1
        PSYMBL(I)= 0
        PPATRN(I)= 0
 41    CONTINUE
C     map device, border, map interation default
      READ(IFL,1110,ERR=91) MAPDEV,MAPBRD,INTACT
C     number of windows, current map window, current plot window
      READ(IFL,1110,ERR=91) NWIN,MAPWIN,PLTWIN
C     number of classes
      READ(IFL,1110,ERR=91) I
      CALL CLINIT
C     class details
      DO 42 J= 1,I
C       sen, loc, const, activefg, symbol, unsel color, sel color
        READ(IFL,1130,ERR=91) SEN,LOC,CON,ACT,SYM,CUN,CSL,MSIZE
        CALL CLADD(I0,ID)
        CALL CLPUT (ID,SEN,LOC,CON,ACT)
        CALL CLPUTG(ID,SYM,CUN,CSL,MSIZE)
 42   CONTINUE
C     timeseries data
      READ(IFL,1115,ERR=91) TU,ARHLOG(1),ARHLOG(2)
C     read number of dates
      READ(IFL,1110,ERR=91) NDATES,CURDAT
      CALL DTINIT
      DO 45 J= 1,NDATES
C       read each set of starting and ending dates
        READ(IFL,1190,ERR=91) CDSID,SDATE,EDATE,SSDATE,SEDATE,
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
 45   CONTINUE
      DO 47 J= 1,NWIN
C       read window dimensions, type
        READ(IFL,1155,ERR=91) (WINDIM(I,J),I=1,4),WTYPE(J)
        WIPEIT(J) = 0
 47   CONTINUE
C     put window info to common
      CALL PUTWIN (MXWIN,WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C     read size of lettering for map options
      READ(IFL,1170,ERR=91) LSIZE,SFCOLR,UFCOLR,SBCOLR,UBCOLR
C     read background to fill in and colors for each coverage
C     1-states,2-counties,3-regions,4-accunits,5-allhyd,6-majhyd
      READ(IFL,1110,ERR=91) BACKCL,MAPCOV
C     read number of maps
      READ(IFL,1110,ERR=91) NMAPS,CURMAP
      DO 48 I= 1,NMAPS
C       read map details
        READ(IFL,1160,ERR=91) MPNMFG(I),MPNMCL(I),MPNMSZ(I)
        READ(IFL,1105,ERR=91) LEGFLG(I),LBASNM(I)
        READ(IFL,1150,ERR=91) R2LAT(1,I),R2LAT(2,I),R2LNG(1,I),
     1                        R2LNG(2,I)
        READ(IFL,1150,ERR=91) BASEY(I),BASEX(I)
        READ(IFL,1180,ERR=91) LEGCOL(I),LGSIZE(I),CMSID(I)
 48   CONTINUE
C     read animation specifications
C     read number of animations, current active animation
      READ(IFL,1110,ERR=91) NANIM,CURANI
      CALL STNANI (NANIM)
      CALL STCURA (CURANI)
      DO 50 J=1,NANIM
C       read number of map segment specs, name of this animation
        READ(IFL,1210,ERR=91) NSEG,ANAME
        DO 150 I= 1,NSEG
C         read map segment details
          READ(IFL,1200,ERR=91) ATMAJ(I),ATMIN(I),CLR1(I),
     1                   CLR2(I),CLR3(I),IDSN(I),RMIN(I),RMAX(I),
     2                   WID1(I),WID2(I),WID3(I)
 150    CONTINUE
        CALL INISEG (J,NSEG,ATMAJ,ATMIN,CLR1,CLR2,CLR3,IDSN,RMIN,RMAX,
     1               WID1,WID2,WID3)
        CALL MODANI (J,ANAME)
 50   CONTINUE
C     put map info into common
      CALL MAPINI (NMAPS,MAPDEV,MAPBRD,INTACT,MPNMFG,MPNMCL,
     I             SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL,
     I             MPNMSZ,R2LAT,R2LNG,LSIZE,CURMAP,
     I             LBASNM,LEGFLG,BASEY,BASEX,LGSIZE,LEGCOL,CMSID)
C     read number of selected data sets
      READ(IFL,1110,ERR=91,END=91) NANAL
      IF (NANAL.GT.MXLINE) THEN
C       too many selected, can only have mxline selected data sets
        NANAL = MXLINE
      END IF
      IF (NANAL.GT.0) THEN
C       some data sets selected, read selected data set numbers
        READ(IFL,1110,ERR=91,END=91) (ADSN(I),I=1,NANAL)
      END IF
C
      RETCOD= 0
      GO TO 99
C
 91   CONTINUE
C       basin file read error
        RETCOD= -2
        GO TO 99
 92   CONTINUE
C       no current calibration name given
        CCAL  = '        '
        RETCOD= 0
        GO TO 99
 99   CONTINUE
C
      IF ((RLAT(1).LT.0.01) .AND. (RLAT(1).GT.-0.01)) THEN
C       default lat long values to whole united states
        write(99,*)'rlat,rlng',rlat,rlng
        RLAT(1) = 24
      END IF
      IF ((RLAT(2).LT.0.01) .AND. (RLAT(2).GT.-0.01)) THEN
        RLAT(2) = 50
      END IF
      IF ((RLNG(1).LT.0.01) .AND. (RLNG(1).GT.-0.01)) THEN
        RLNG(1) = 66
      END IF
      IF ((RLNG(2).LT.0.01) .AND. (RLNG(2).GT.-0.01)) THEN
        RLNG(2) = 125
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HSXPUT
     I                   (IFL,MXSEN,MXCOV,MXLINE,MXCAL,MXSITE,MXSTRM,
     I                    MXWDM,PLCLR,PLNTYP,PSYMBL,PPATRN,ARHLOG,
     I                    MAPCOV,RLAT,RLNG,UCIPTH,IVAR,RPARM1,RPARM2,
     I                    RDERR,VARCNT,UNCALD,RCHNGE,RERROR,IPCTFG,
     I                    ISUBJC,RERTRM,NSTATS,STAT1,SITENM,CALINM,
     I                    BAREA,NSITE,CURSIT,DATCNT,NWDM,DCNVFG,
     I                    STSTRM,ENSTRM,STATDN,SITNAM,PLTPRE,LWDID,
     I                    LWDNAM,CCAL,NANAL,ADSN,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Write the state of the expert system for a basin to a file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IFL,RETCOD,MXSEN,MXCOV,MXLINE,MXCAL,MXSITE,MXSTRM,
     $             MXWDM,PLCLR(MXSEN),PLNTYP(MXSEN),PPATRN(MXSEN),
     $             PSYMBL(MXSEN),ARHLOG(2),MAPCOV(MXCOV),IPCTFG(12)
      INTEGER      IVAR(2),VARCNT(2),UNCALD,NSTATS,ISUBJC(23)
      INTEGER      NSITE,CURSIT,DATCNT,NWDM,DCNVFG,NANAL,ADSN(MXLINE),
     $             STSTRM(6,MXSTRM),ENSTRM(6,MXSTRM),STATDN(MXSITE)
      REAL         RPARM1(100),RPARM2(100),RDERR(100,19),RERTRM(10)
      REAL         RLAT(2),RLNG(2),RCHNGE(12),RERROR(12,19),
     $              STAT1(MXCAL*MXSITE,33),BAREA(MXSITE)
      CHARACTER*1   SITNAM(20,MXSITE)
      CHARACTER*4   LWDID(MXWDM)
      CHARACTER*8   SITENM(MXCAL*MXSITE),CALINM(MXCAL*MXSITE)
      CHARACTER*8   PLTPRE,CCAL
      CHARACTER*64  UCIPTH,LWDNAM(MXWDM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IFL    - basin description file unit number
C     RETCOD - return code
C     MXSEN  - Maximum number of scenarios
C     MXLINE - maximum lines on plot
C     MXCAL  - maximum number of calibrations
C     MXSITE - maximum number of sites
C     MXCOV  - Maximum coverages on map
C     MXSTRM - maximum number of storms
C     MXWDM  - maximum number of wdm files
C     ARHLOG - type of axis - 1- arith, 2-log
C     MAPCOV - color for each map coverage
C     PLCLR  - line colors for each line
C     PLNTYP - line types for each line
C     PPATRN - fill pattern for plots
C     PSYMBL - symbol type for each line
C     RLAT   - latitude limits for map
C     RLNG   - longitude limits for map
C     UCIPTH - path to uci files
C     IVAR   - two variables for double sensitivity analysis
C     RPARM1 - array of parameters for double sensitivity analysis
C     RPARM2 - array of parameters for double sensitivity analysis
C     RDERR  - array of error terms for double sensitivity analysis
C     VARCNT - count of each variable in double sensitivity analysis
C     UNCALD - uncalculated double sensitivity analysis flag
C     RCHNGE - array of changes for sensitivity analysis
C     RERROR - error terms from sensitivity analysis
C     IPCTFG - flags indicating absolute or percent sens anal changes
C     ISUBJC - array of subjectives
C     RERTRM - array of real current error terms
C     NSTATS - number of sets of stats calculated
C     STAT1  - array of stats calculated per site and calibration run
C     SITENM - array of site names for each sets of stats
C     CALINM - array of calibration names for each set of stats
C     BAREA  - array of basin areas for each site
C     NSITE  - number of sites for this basin
C     CURSIT - current site number
C     DATCNT - count of storm dates
C     NWDM   - number of wdm files
C     DCNVFG - data conversion flag
C     STSTRM - storm start dates
C     ENSTRM - storm end dates
C     STATDN - array of flags indicating if stats calc for each site
C     SITNAM - array of site names
C     PLTPRE - plot file name prefix
C     LWDID  - array of wdm ids
C     LWDNAM - array of wdm file names
C     NANAL  - number of selected data sets
C     ADSN   - array of selected dats sets
C     CCAL   - name of current calibration
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxmap.inc'
      INCLUDE 'pmxwin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J
      INTEGER       ACT,SYM,CUN,CSL,NDATES,CURDAT
      INTEGER       MAPDEV,MAPBRD,INTACT,MPNMFG(MXMAP),MPNMCL(MXMAP),
     $              SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL(2),LEGCOL(MXMAP),
     $              NMAPS,CURMAP,SSDATE(2),SEDATE(2),DTTU,DTTS,
     $              ATMAJ,ATMIN,CLR1,CLR2,
     $              NSEG,LEGFLG(MXMAP),NANIM,CURANI,
     $              CLR3,DTRAN,SDATE(6),EDATE(6),TU,IDSN
      INTEGER       WIPEIT(MXWIN),MAPWIN,PLTWIN,WTYPE(MXWIN),NWIN
      REAL          WINDIM(4,MXWIN)
      REAL          MPNMSZ(MXMAP),LSIZE,LGSIZE(MXMAP),MSIZE,
     $              R2LAT(2,MXMAP),R2LNG(2,MXMAP),
     $              BASEX(MXMAP),BASEY(MXMAP),RMIN,RMAX,
     $              WID1,WID2,WID3
      CHARACTER*8   SEN,LOC,CON,CDSID,CMSID(MXMAP)
      CHARACTER*20  ANAME
      CHARACTER*64  LBASNM(MXMAP)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (10F8.2)
 2010 FORMAT (20I4)
 2020 FORMAT (A8,2I5,4F8.1)
 2030 FORMAT (2(I5,5I3))
 2040 FORMAT (10F8.0)
 2050 FORMAT (41X,I2,2X,20A1)
 2060 FORMAT (A4,1X,A64)
 2070 FORMAT (A8,2X,A8)
 2075 FORMAT (10F10.3)
 2080 FORMAT (1F10.3,I5)
 2105 FORMAT (I5,10X,A64)
 2110 FORMAT (16I5)
 2115 FORMAT (5X,I5,10X,1I5,5X,1I5)
 2130 FORMAT (3(2X,A8),4I5,1F10.3)
 2150 FORMAT (4F10.3)
 2155 FORMAT (4F10.3,I10)
 2160 FORMAT (2I5,F10.3)
 2170 FORMAT (F10.3,4I5)
 2180 FORMAT (5X,I5,1F10.3,A8)
 2190 FORMAT (A8,2X,2(I5,5I3),2(2X,2I3),2X,I2,I4,I2)
 2200 FORMAT (6I5,5F10.3)
 2210 FORMAT (I5,5X,A20)
 2220 FORMAT (A64)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CLCNT,CLGET,CLGETG,MAPGET,DTGET,DTCNT
      EXTERNAL    GETSEG,GTNSEG,GTNANI,GTCURA,GETAN1,GETWIN
C
C     + + + END SPECIFICATIONS + + +
C
C     write expert system file
C     number of wdm files
      WRITE(IFL,2010,ERR=91) NWDM
      DO 2 I= 1, NWDM
C       read wdm id and file name
        WRITE(IFL,2060,ERR=91) LWDID(I),LWDNAM(I)
 2    CONTINUE
      WRITE(IFL,2220,ERR=91) UCIPTH
C
C     plot file name prefix
      WRITE(IFL,2020,ERR=91) PLTPRE,NSITE,CURSIT,RLAT,RLNG
      DO 5 I= 1,NSITE
C       write stats calculated flag and site name for each site
        WRITE(IFL,2050,ERR=91) STATDN(I),(SITNAM(J,I),J=1,20)
 5    CONTINUE
C     number of storms, data convert flag
      WRITE(IFL,2010,ERR=91) DATCNT,DCNVFG
C     storm end/start dates
      DO 10 I= 1,DATCNT
        WRITE(IFL,2030,ERR=91) (STSTRM(J,I),J=1,6),(ENSTRM(J,I),J=1,6)
 10   CONTINUE
      IF (NSITE.LE.10) THEN
C       basin area
        WRITE(IFL,2040,ERR=91) (BAREA(J),J=1,NSITE)
      ELSE
C       basin area
        WRITE(IFL,2040,ERR=91) (BAREA(J),J=1,10)
        WRITE(IFL,2040,ERR=91) (BAREA(J),J=11,NSITE)
      END IF
C     error terms
      WRITE(IFL,2000,ERR=91) RERTRM
C
C     name of current calibration, if any
      WRITE(IFL,2070,ERR=91) CCAL
C
C     statistical data for history option
      WRITE(IFL,2010,ERR=91) NSTATS
      IF (NSTATS.GT.0) THEN
        DO 20 I= 1, NSTATS
C         write calibration name and site name
          WRITE(IFL,2070,ERR=91) CALINM(I),SITENM(I)
          WRITE(IFL,2075,ERR=91) (STAT1(I,J),J=1,8)
          WRITE(IFL,2075,ERR=91) (STAT1(I,J),J=9,16)
          WRITE(IFL,2075,ERR=91) (STAT1(I,J),J=17,22)
          WRITE(IFL,2075,ERR=91) (STAT1(I,J),J=23,27)
          WRITE(IFL,2075,ERR=91) (STAT1(I,J),J=28,33)
 20     CONTINUE
      END IF
C     sensitivity analysis data
      DO 30 I = 1,12
        WRITE(IFL,2080,ERR=91) RCHNGE(I),IPCTFG(I)
        WRITE(IFL,2075,ERR=91) (RERROR(I,J),J=1,7)
        WRITE(IFL,2075,ERR=91) (RERROR(I,J),J=8,13)
        WRITE(IFL,2075,ERR=91) (RERROR(I,J),J=14,19)
 30   CONTINUE
C     two-variable sensitivity data
      IF (UNCALD.EQ.1) THEN
C       dont write since calculations have not been performed
        IVAR(1)  = 0
        IVAR(2)  = 0
        VARCNT(1)= 0
        VARCNT(2)= 0
      END IF
      WRITE(IFL,2010,ERR=91) IVAR(1),IVAR(2),VARCNT(1),VARCNT(2)
      IF (IVAR(1).GT.0 .AND. IVAR(2).GT.0) THEN
        DO 35 I = 1,VARCNT(1)*VARCNT(2)
          WRITE(IFL,2075,ERR=91) RPARM1(I),RPARM2(I),(RDERR(I,J),J=1,5)
          WRITE(IFL,2075,ERR=91) (RDERR(I,J),J=6,12)
          WRITE(IFL,2075,ERR=91) (RDERR(I,J),J=13,19)
 35     CONTINUE
      END IF
C
C     subjective data
      WRITE(IFL,2010,ERR=91) ISUBJC
C
C     write rest like genscn .sta file
C     get map info from common
      CALL MAPGET (MXMAP,
     O             NMAPS,MAPDEV,MAPBRD,INTACT,MPNMFG,MPNMCL,
     O             SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL,
     O             MPNMSZ,R2LAT,R2LNG,LSIZE,CURMAP,
     O             LBASNM,LEGFLG,BASEY,BASEX,LGSIZE,LEGCOL,CMSID)
      WRITE(IFL,2110,ERR=91) MXLINE
      DO 40 I= 1,MXLINE
        WRITE(IFL,2110,ERR=91) PLCLR(I),PLNTYP(I),PSYMBL(I),PPATRN(I)
 40   CONTINUE
C     map device, border, map interation default
      WRITE(IFL,2110,ERR=91) MAPDEV,MAPBRD,INTACT
C     get window specs from common
      CALL GETWIN (MXWIN,
     O             WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C     number of windows, current map window, current plot window
      WRITE(IFL,2110,ERR=91) NWIN,MAPWIN,PLTWIN
C     number of classes
      CALL CLCNT(I)
      WRITE(IFL,2110,ERR=91) I
C     class details
      DO 41 J= 1,I
        CALL CLGET (J,SEN,LOC,CON,ACT)
        CALL CLGETG(J,SYM,CUN,CSL,MSIZE)
        WRITE(IFL,2130,ERR=91) SEN,LOC,CON,ACT,SYM,CUN,CSL,MSIZE
 41   CONTINUE
C     timeseries data
      TU = 1
      WRITE(IFL,2115,ERR=91) TU,ARHLOG(1),ARHLOG(2)
C     number of dates and current date set
C     number of classes
      CALL DTCNT(NDATES)
      CURDAT = 0
      DO 43 J= 1,NDATES
C       check for current date set
        CALL DTGET (J,
     O              ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     O              DTTS,DTRAN)
        IF (ACT.EQ.1) THEN
          CURDAT = J
        END IF
 43   CONTINUE
      WRITE(IFL,2110,ERR=91) NDATES,CURDAT
      DO 45 J= 1,NDATES
C       write each set of starting and ending dates
        CALL DTGET (J,
     O              ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     O              DTTS,DTRAN)
        WRITE(IFL,2190,ERR=91) CDSID,SDATE,EDATE,SSDATE,SEDATE,
     1                         DTTU,DTTS,DTRAN
 45   CONTINUE
      DO 47 J= 1,NWIN
        WRITE(IFL,2155,ERR=91) (WINDIM(I,J),I=1,4),WTYPE(J)
 47   CONTINUE
C     write size of lettering for map options
      WRITE(IFL,2170,ERR=91) LSIZE,SFCOLR,UFCOLR,SBCOLR,UBCOLR
C     write background to fill in and color
      WRITE(IFL,2110,ERR=91) BACKCL,MAPCOV
C     write number of maps
      WRITE(IFL,2110,ERR=91) NMAPS,CURMAP
      DO 50 I= 1,NMAPS
C       write map details
        WRITE(IFL,2160,ERR=91) MPNMFG(I),MPNMCL(I),MPNMSZ(I)
        WRITE(IFL,2105,ERR=91) LEGFLG(I),LBASNM(I)
        WRITE(IFL,2150,ERR=91) R2LAT(1,I),R2LAT(2,I),
     1                         R2LNG(1,I),R2LNG(2,I)
C       write legend location for this map
        WRITE(IFL,2150,ERR=91) BASEY(I),BASEX(I)
        WRITE(IFL,2180,ERR=91) LEGCOL(I),LGSIZE(I),CMSID(I)
 50   CONTINUE
C     write animation specifications
C     write number of animations, current active animation
      CALL GTNANI (NANIM)
      CALL GTCURA (CURANI)
      WRITE(IFL,2110,ERR=91) NANIM,CURANI
      DO 55 J=1,NANIM
C       write number of map segment specs, name of this animation
        CALL GTNSEG (J,
     O               NSEG)
        CALL GETAN1 (J,
     O               ANAME)
        WRITE(IFL,2210,ERR=91) NSEG,ANAME
        DO 155 I= 1,NSEG
C         write map segment details
          CALL GETSEG (J,I,
     O                 ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                 IDSN,WID1,WID2,WID3)
          WRITE(IFL,2200,ERR=91) ATMAJ,ATMIN,CLR1,CLR2,CLR3,IDSN,
     1                           RMIN,RMAX,WID1,WID2,WID3
 155    CONTINUE
 55   CONTINUE
      IF (NANAL.GT.MXLINE) THEN
C       too many data sets selected
        NANAL = MXLINE
      END IF
C     write number of selected data sets
      WRITE(IFL,2110,ERR=91) NANAL
      IF (NANAL.GT.0) THEN
C       some data sets selected, write selected data set numbers
        WRITE(IFL,2110,ERR=91) (ADSN(I),I=1,NANAL)
      END IF
C
      RETCOD= 0
      GO TO 99
 91   CONTINUE
C       basin file write error
        RETCOD= -2
        GO TO 99
 99   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   EXSNEW
     O                   (NSTATS,RCHNGE,RERROR,IVAR,DCNVFG)
C
C     + + + PURPOSE + + +
C     Initialize the exs parameters for history and sensitivity.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DCNVFG,NSTATS,IVAR(2)
      REAL      RCHNGE(12),RERROR(12,19)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSTATS - number of sets of stats calculated for history option
C     RCHNGE - array of changes for single sens. anal.
C     RERROR - array of error terms for single sens. anal.
C     IVAR   - variables for two variable sens. anal.
C     DCNVFG - data conversion flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      NSTATS = 0
C     initialize sensitivity analysis data
      DO 10 I = 1,12
        RCHNGE(I) = 0.0
        DO 15 J = 1,19
          RERROR(I,J) = 0.0
 15     CONTINUE
 10   CONTINUE
C     initialize two-variable sensitivity analysis data
      IVAR(1) = 0
      IVAR(2) = 0
      DCNVFG  = 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   CMSTRM
     I                   (START,ENDDT,NSTRM,LSTRM,FSTRM,
     I                    BUFMAX,YX,TS,TU,NVALS,
     M                    INDX,
     O                    CSTRM,STSTRM,ENSTRM,TRSTRM,STRMPK,AVSTVL)
C
C     + + + PURPOSE + + +
C     computes storm dates
C
C     + + + DUMMY ARGUENTS + + +
      INTEGER   START(6),ENDDT(6),NSTRM,LSTRM,CSTRM,
     1          STSTRM(6,NSTRM),ENSTRM(6,NSTRM),BUFMAX,
     2          INDX(BUFMAX)
      REAL      FSTRM,TRSTRM(NSTRM),YX(BUFMAX),STRMPK(NSTRM),
     1          AVSTVL(NSTRM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     START  - Starting date array
C     ENDDT  - Ending date array
C     NSTRM  - Max number of storms to find
C     LSTRM  - Number of storms to find
C     CSTRM  - Number of storms found
C     FSTRM  - If < 0 percent of peak which defines storm
C              If > 0 flow duration percent which defines storm
C     STSTRM - Start date of storms
C     ENSTRM - End date of storms
C     TRSTRM - Total runoff for each storm (total storm volume)
C     STRMPK - storm peak volume
C     AVSTVL - average storm volume
C     BUFMAX - maximum number of values to search
C     YX     - array of values to search
C     INDX   - array of order numbers for values to search
C     TS     -
C     TU     -
C     NVALS  -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TS,TU,NVALS,I,J,K,OPT,
     1          NEWFLG,STSTRJ,ENSTRJ,NPOS,STPOS,TOTDYS,CNTDYS
      REAL      RTMP,XSTRM,RCNT
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIMADD, ASRTR
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     sort values in place
      OPT= 0
      CALL ASRTR(OPT,NVALS,YX,INDX)
      WRITE(99,*) 'just sorted data',nvals
C     look for storms
      WRITE(99,*) 'looking for ',LSTRM,' storms, tolr:',FSTRM
      IF (FSTRM.GT.0.0) THEN
        STPOS= NVALS*(FSTRM/100.)
        IF (STPOS.GT.BUFMAX) THEN
          STPOS = BUFMAX
        END IF
        XSTRM= YX(INDX(STPOS))
        WRITE(99,*) '  STPOS,XSTRM:',STPOS,XSTRM
      ELSE
        STPOS= 0
        XSTRM= 0.0
      END IF
      CSTRM= 0
      NPOS = NVALS
 10   CONTINUE
        NEWFLG= 1
        IF (CSTRM.GT.0) THEN
C         see if this storm is in any existing ones
          I= 0
 15       CONTINUE
            I= I+ 1
            IF (INDX(NPOS) .GE. STSTRM(1,I) .AND.
     1          INDX(NPOS) .LE. ENSTRM(1,I)) THEN
C             already have it
C             WRITE(99,*) 'fail storm:',NPOS,INDX(NPOS)
              NEWFLG= 0
            END IF
          IF (NEWFLG.EQ.1 .AND. I.LT.CSTRM) GO TO 15
        END IF
        IF (NEWFLG.EQ.1) THEN
C         a new storm, find beginning of storm
          IF (ABS(XSTRM).LT.1E-4) THEN
            RTMP= YX(INDX(NPOS))* ABS(FSTRM)/100.0
          ELSE
            RTMP= XSTRM
          END IF
C         WRITE(99,*) 'new storm1:',NPOS,INDX(NPOS),YX(INDX(NPOS)),RTMP
          STSTRJ= INDX(NPOS)+ 1
 20       CONTINUE
            STSTRJ= STSTRJ- 1
          IF (YX(STSTRJ-1) .GT. RTMP .AND. STSTRJ.GT.1) GO TO 20
C         find end of storm
          ENSTRJ= INDX(NPOS)- 1
 30       CONTINUE
            ENSTRJ= ENSTRJ+ 1
          IF (ENSTRJ.LT.NVALS .AND. YX(ENSTRJ+1) .GT. RTMP) GO TO 30
          CSTRM= CSTRM+ 1
          STSTRM(1,CSTRM)= STSTRJ
          ENSTRM(1,CSTRM)= ENSTRJ
C         WRITE (99,*) 'new storm2:',CSTRM,NPOS,INDX(NPOS),STSTRJ,ENSTRJ
        END IF
        NPOS= NPOS- 1
      IF (CSTRM.LT.LSTRM .AND. NPOS.GT.STPOS) GO TO 10
      WRITE(99,*) 'out of find loop:',CSTRM,LSTRM,NPOS
      TOTDYS= 0
      DO 40 I= 1,CSTRM
C       WRITE(99,*)
C       WRITE(99,*) 'storm:',I
        J= STSTRM(1,I)- 1
C       WRITE(99,*) 'before:',J,YX(J)
C
        TRSTRM(I)= 0.0
C       initialize storm peak values
        STRMPK(I)= YX(STSTRM(1,I))
        RCNT = 0.0
        DO 35 J= STSTRM(1,I),ENSTRM(1,I)
          RCNT= RCNT + 1.0
          TRSTRM(I)= TRSTRM(I)+ YX(J)
          IF (YX(J).GT.STRMPK(I)) THEN
C           new storm peak
            STRMPK(I) = YX(J)
          END IF
 35     CONTINUE
C       compute average storm volume
        AVSTVL(I) = TRSTRM(I)/RCNT
C
        J= ENSTRM(1,I)+ 1
C       WRITE(99,*) 'after :',J,YX(J)
C       calculate calendar start day
        CNTDYS= ENSTRM(1,I)- STSTRM(1,I)+ 1
        TOTDYS= TOTDYS+ CNTDYS
C       WRITE(99,*) 'day cnt:',CNTDYS,TOTDYS
        K= STSTRM(1,I)- 1
        CALL TIMADD(START,TU,TS,K,
     O              STSTRM(1,I))
C       calculate calendar end day
        K= ENSTRM(1,I)- 1
        CALL TIMADD(START,TU,TS,K,
     O              ENSTRM(1,I))
        ENSTRM(4,I)= 24
 40   CONTINUE
C
      RETURN
      END
