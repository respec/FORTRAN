C
C
C
      SUBROUTINE   PRWMAP
     I                   (MESSFL,MXLOC,WDMSFL,WDMPFL,MXCOV,
     I                    PTHNAM,BRSFG,
     M                    NSTA,STAID,LAT,LNG,LOCXFG,MAPCOV)
C
C     + + + PURPOSE + + +
C     map location of stations in buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,MXLOC,WDMSFL,WDMPFL,MXCOV,
     $          NSTA,STAID(MXLOC),LOCXFG,
     $          MAPCOV(MXCOV),BRSFG
      REAL      LAT(MXLOC),LNG(MXLOC)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     MXLOC  - maximum number of stations
C     WDMSFL - unit number of file containing stations with attr to map,
C              -1 in scenario generators
C     WDMPFL - boundary file unit number
C     MXCOV  - maximum number of available coverages
C     BRSFG  - browse flag: 1-go to browse screen from map click
C     NSTA   - number of stations found
C     PTHNAM - character string of path of options selected to get here
C     STAID  - data set numbers of stations selected in expert system,
C              id's of all stations in scenerio generators
C     LAT    - latitude of station in s.g., dummy in exp sys
C     LNG    - longitude of station in s.g., dummy in exp sys
C     LOCXFG - flag indicating that the locations specified have changed
C              and thus the markers need to be redrawn
C     MAPCOV - available coverages available
C              1-state,2-county,3-hydro region,4-hydro acct unit,
C              5-all hydrography,6-major hydrography,7-MLRA
C
C     + + + SAVES + + +
      INTEGER   DRAWN(7),MODFID(6)
      SAVE      DRAWN,MODFID
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxwin.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmaprm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I0,I1,I2,I4,I8,IRET,INUM,LPTH,CURMAP,ICNT,ZEROFG,
     $          SCLU,SGRP,MAPOPT,ITMP(8),MAPDEV,BACKCL(2),RESP,
     $          CVAL(4),SFCOLR,SBCOLR,UFCOLR,UBCOLR,CNUM,RNUM,CMPTYP,
     $          TYPE(200),ATT1(200),ATT2(200),RETCOD,ACLU,IWAIT
      INTEGER   WIPEIT(MXWIN),MAPWIN,PLTWIN,WTYPE(MXWIN),NWIN
      REAL      WINDIM(4,MXWIN)
      REAL      MKDIS(500),MKID(500),MKX(500),MKY(500),
     $          RVAL(3),LSIZE
      CHARACTER*8  PIKPTH(1),WINPTH(1)
      CHARACTER*48 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZSTCMA,GEPMAP,PDNPLT,GPXLOC,QRESP,QSETR,QGETR
      EXTERNAL  Q1INIT,QSETCO,Q1EDIT,QGETCO,ANIMAN,ANPRGT
      EXTERNAL  ZLNTXT,ZWNSOP,ZWNSET,MRKMOD,MBKGET,PRNTXT,WSWLOC
      EXTERNAL  MDVPUT,MDVGET,QSTCOB,QGTCOB,MBKPUT,WDLLSU
      EXTERNAL  MOPGET,MOPPUT,SGMAPS,MAPCUR,SSPECI,GETWIN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  DRAWN/0,0,0,0,0,0,0/
      DATA  MODFID/0,0,0,0,0,0/
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I4= 4
      I8= 8
C
C     get current map window specs
      CALL GETWIN (MXWIN,
     O             WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C     send current map window dimensions to graphics routines
      CALL GPXLOC (WINDIM(1,MAPWIN))
C
      SCLU   = 52
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
      IF (LOCXFG.EQ.1) THEN
C       location on/offs have changed since last time in, make sure
C       markers are redrawn
        MODFID(6) = 1
        DRAWN(MAPWIN) = 0
        LOCXFG = 0
      END IF
C
      MAPOPT = 1
C     get map device from common
      CALL MDVGET (
     O             MAPDEV)
C
C     map spec loop
 10   CONTINUE
C       which map option?
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Map ('//PTHNAM(1)(1:LPTH)//'M)'
        ELSE
C         no pathname
          WNDNAM= 'Map (M)'
        END IF
        CALL ZWNSET (WNDNAM)
        SGRP= 20
        CALL QRESP(MESSFL,SCLU,SGRP,
     M             MAPOPT)
C
        IF (MAPOPT .EQ. 1) THEN
C         device
C         make previous available
          I= 4
          CALL ZSTCMA (I,I1)
          SGRP= 21
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'DEvice ('//PTHNAM(1)(1:LPTH)//'MDe)'
          ELSE
C           no pathname
            WNDNAM= 'DEvice (MDe)'
          END IF
          CALL ZWNSET (WNDNAM)
          CALL QRESP(MESSFL,SCLU,SGRP,
     M               MAPDEV)
          IF (MAPDEV.GT.1) THEN
C           map is never already drawn if not to screen
            DRAWN(MAPWIN) = 0
          END IF
C         put map device to common
          CALL MDVPUT (MAPDEV)
C         turn off previous command
          CALL ZSTCMA (I4,I0)
        ELSE IF (MAPOPT .EQ. 2) THEN
C         coverages to include
C         make previous available
          I= 4
          CALL ZSTCMA (I,I1)
          SGRP= 41
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'Coverage ('//PTHNAM(1)(1:LPTH)//'MC)'
          ELSE
C           no pathname
            WNDNAM= 'Coverage (MC)'
          END IF
          CALL ZWNSET (WNDNAM)
          CALL Q1INIT(MESSFL,SCLU,SGRP)
          CALL QSETCO(MXCOV,MAPCOV)
C         get background fill info from common
          CALL MBKGET (
     O                 BACKCL)
          BACKCL(1) = BACKCL(1) + 1
          BACKCL(2) = BACKCL(2) + 1
          CALL QSTCOB(I2,I8,BACKCL)
          CALL Q1EDIT(IRET)
          ITMP(1) = MAPCOV(1)
          ITMP(2) = MAPCOV(2)
          ITMP(3) = MAPCOV(3)
          ITMP(4) = MAPCOV(4)
          ITMP(5) = MAPCOV(5)
          ITMP(6) = MAPCOV(6)
          ITMP(7) = BACKCL(1)
          ITMP(8) = BACKCL(2)
          IF (IRET .NE. 2) THEN
            CALL QGETCO(MXCOV,
     O                  MAPCOV)
            CALL QGTCOB(I2,I8,
     O                  BACKCL)
            IF (BACKCL(1).NE.ITMP(7)) THEN
C             changed background layer, redraw
              DRAWN(MAPWIN) = 0
            END IF
            IF (BACKCL(2).NE.ITMP(8)) THEN
C             changed background color, redraw
              DRAWN(MAPWIN) = 0
            END IF
            BACKCL(1) = BACKCL(1) - 1
            BACKCL(2) = BACKCL(2) - 1
C           put background fill info to common
            CALL MBKPUT (BACKCL)
            IF (MAPCOV(1).NE.ITMP(1)) THEN
C             state border color has been changed, redraw on map
              MODFID(1) = 1
            END IF
            IF (MAPCOV(2).NE.ITMP(2)) THEN
C             county border color has been changed, redraw on map
              MODFID(2) = 1
            END IF
            IF (MAPCOV(3).NE.ITMP(3)) THEN
C             region border color has been changed, redraw on map
              MODFID(3) = 1
            END IF
            IF (MAPCOV(4).NE.ITMP(4)) THEN
C             accntg unit color has been changed, redraw on map
              MODFID(4) = 1
            END IF
            IF (MAPCOV(5).NE.ITMP(5)) THEN
C             all hydrography color has been changed, redraw on map
              MODFID(5) = 1
            END IF
            IF (MAPCOV(6).NE.ITMP(6)) THEN
C             maj hydrography color has been changed, redraw on map
              MODFID(5) = 1
            END IF
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
        ELSE IF (MAPOPT .EQ. 3) THEN
C         edit marker type, color
C         make previous available
          I= 4
          CALL ZSTCMA (I,I1)
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'Marker ('//PTHNAM(1)(1:LPTH)//'MM)'
          ELSE
C           no pathname
            WNDNAM= 'Marker (MM)'
          END IF
          CALL ZWNSET (WNDNAM)
          CALL MRKMOD (MESSFL,SCLU,
     O                 IRET)
          IF (IRET .NE. 2) THEN
C           redraw markers
            DRAWN(MAPWIN) = 0
            MODFID(6) = 1
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
        ELSE IF (MAPOPT .EQ. 4) THEN
C         do window screen
C         what type computer
          CALL ANPRGT(I1,CMPTYP)
          IF (LPTH .GT. 0) THEN
C           path name available
            WINPTH(1)= PTHNAM(1)(1:LPTH)//'M'
          ELSE
C           no pathname
            WINPTH(1)= 'M       '
          END IF
          IF (CMPTYP .NE. 1) THEN
C           not a pc, go ahead
            CALL WSWLOC (MESSFL,WINPTH)
C           get current map window specs
            CALL GETWIN (MXWIN,
     O                   WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C           send window dimensions to graphics routines
            CALL GPXLOC (WINDIM(1,MAPWIN))
          ELSE
C           pc, multiple windows not supported
            WNDNAM= 'Windows ('//WINPTH(1)(1:LPTH+1)//'W)'
            CALL ZWNSET (WNDNAM)
            SGRP = 27
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
        ELSE IF (MAPOPT .EQ. 5) THEN
C         edit options appearance
C         make previous command available
          CALL ZSTCMA (I4,I1)
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'Option ('//PTHNAM(1)(1:LPTH)//'MO)'
          ELSE
C           no pathname
            WNDNAM= 'Option (MO)'
          END IF
          CALL ZWNSET (WNDNAM)
          SGRP = 55
          CALL Q1INIT (MESSFL,SCLU,SGRP)
          CALL MOPGET (
     O                 LSIZE,SBCOLR,UBCOLR,SFCOLR,UFCOLR)
          CVAL(1) = SFCOLR +1
          CVAL(2) = UFCOLR +1
          CVAL(3) = SBCOLR +1
          CVAL(4) = UBCOLR +1
C         clear character fields
          CNUM = 4
          CALL QSETCO (CNUM,CVAL)
C         set value of size
          RNUM = 1
          RVAL(1) = LSIZE
          CALL QSETR (RNUM,RVAL)
          CALL Q1EDIT (
     O                 RESP)
          IF (RESP.EQ.1) THEN
C           user wants to continue
            CALL QGETCO (CNUM,
     O                   CVAL)
            SFCOLR = CVAL(1) - 1
            UFCOLR = CVAL(2) - 1
            SBCOLR = CVAL(3) - 1
            UBCOLR = CVAL(4) - 1
            CALL QGETR (RNUM,RVAL)
            LSIZE = RVAL(1)
C           put function specs back to common
            CALL MOPPUT (LSIZE,SBCOLR,UBCOLR,SFCOLR,UFCOLR)
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
        ELSE IF (MAPOPT .EQ. 6) THEN
C         do pick screen
          IF (LPTH .GT. 0) THEN
C           path name available
            PIKPTH(1)= PTHNAM(1)(1:LPTH)//'M'
          ELSE
C           no pathname
            PIKPTH(1)= 'M       '
          END IF
          CALL SSPECI (MESSFL,PIKPTH)
C         dont know if active subsets changed, so we must redraw markers
          DRAWN(MAPWIN) = 0
          MODFID(6) = 1
        ELSE IF (MAPOPT .EQ. 7) THEN
C         specify which set of map specs to use
          CALL SGMAPS (MESSFL,SCLU,PTHNAM,
     M                 DRAWN(MAPWIN))
        ELSE IF (MAPOPT .EQ. 8) THEN
C         specify map animation details
C         check to see if map segments exist in huc.wdm file
          I =200
C         need to call once to clear out previous use of this routine
          CALL WDLLSU (WDMPFL,I1,I,
     O                 INUM,TYPE,ATT1,ATT2,RETCOD)
          CALL WDLLSU (WDMPFL,DSNMAJ,I,
     O                 INUM,TYPE,ATT1,ATT2,RETCOD)
          IF (RETCOD.EQ.0 .AND. INUM.GT.0) THEN
C           this data set exists, but only do animation if non-zero atts
            ICNT   = 1
            ZEROFG = 1
    5       CONTINUE
C             check att1 to see if zero value
              IF (ATT1(ICNT).NE.0) THEN
C               found a non-zero value
                ZEROFG = 0
              END IF
              ICNT = ICNT + 1
            IF (ICNT.LE.INUM) GO TO 5
            IF (ZEROFG.EQ.1) THEN
C             all zero values, no animation
              RETCOD = -1
            END IF
          END IF
          IF (RETCOD.NE.0) THEN
C           no segments available, tell user
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Animate ('//PTHNAM(1)(1:LPTH)//'MA) Problem'
            ELSE
C             no pathname
              WNDNAM= 'Animate (MA) Problem'
            END IF
            CALL ZWNSET (WNDNAM)
            SGRP = 71
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           do animation specs
            ACLU = 64
            CALL ANIMAN (MESSFL,WDMPFL,ACLU)
          END IF
        ELSE IF (MAPOPT .EQ. 9) THEN
C         produce the map with no animation
          CALL MAPCUR (
     O                 CURMAP)
          IF (CURMAP.EQ.0) THEN
C           no map is active, tell user
            SGRP = 25
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Problem'
            ELSE
C             no pathname
              WNDNAM= 'Draw (MD) Problem'
            END IF
            CALL ZWNSET (WNDNAM)
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           we have an active map
            IF (WIPEIT(MAPWIN).EQ.1 .AND. MAPDEV.EQ.1) THEN
C             window size has been modified, window needs to disappear
              IWAIT = 0
              CALL PDNPLT (MAPWIN,I1,IWAIT)
              WIPEIT(MAPWIN) = 0
C             set map drawn flag to zero since map needs to be drawn
              DRAWN(MAPWIN)  = 0
            END IF
            CALL GEPMAP(MESSFL,SCLU,MXLOC,WDMSFL,WDMPFL,MXCOV,
     I                  BRSFG,NSTA,LAT,LNG,MAPCOV,
     I                  MAPWIN,
     I                  WINDIM,PTHNAM(1),I0,STAID,
     M                  DRAWN(MAPWIN),MODFID,
     O                  MKID,MKX,MKY,MKDIS)
          END IF
        ELSE IF (MAPOPT .EQ. 10) THEN
C         produce the map with animation
C         check to see if map segments exist in huc.wdm file
          I =200
C         need to call once to clear out previous use of this routine
          CALL WDLLSU (WDMPFL,I1,I,
     O                 INUM,TYPE,ATT1,ATT2,RETCOD)
          CALL WDLLSU (WDMPFL,DSNMAJ,I,
     O                 INUM,TYPE,ATT1,ATT2,RETCOD)
          IF (RETCOD.EQ.0 .AND. INUM.GT.0) THEN
C           this data set exists, but only do animation if non-zero atts
            ICNT   = 1
            ZEROFG = 1
    2       CONTINUE
C             check att1 to see if zero value
              IF (ATT1(ICNT).NE.0) THEN
C               found a non-zero value
                ZEROFG = 0
              END IF
              ICNT = ICNT + 1
            IF (ICNT.LE.INUM) GO TO 2
            IF (ZEROFG.EQ.1) THEN
C             all zero values, no animation
              RETCOD = -1
            END IF
          END IF
          IF (RETCOD.NE.0) THEN
C           no segments available, tell user
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Produce ('//PTHNAM(1)(1:LPTH)//'MP) Problem'
            ELSE
C             no pathname
              WNDNAM= 'Produce (MP) Problem'
            END IF
            CALL ZWNSET (WNDNAM)
            SGRP = 71
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           we have map segments, go ahead
            CALL MAPCUR (
     O                   CURMAP)
            IF (CURMAP.EQ.0) THEN
C             no map is active, tell user
              SGRP = 25
              IF (LPTH .GT. 0) THEN
C               path name available
                WNDNAM= 'Produce ('//PTHNAM(1)(1:LPTH)//'MP) Problem'
              ELSE
C               no pathname
                WNDNAM= 'Produce (MP) Problem'
              END IF
              CALL ZWNSET (WNDNAM)
              CALL PRNTXT (MESSFL,SCLU,SGRP)
            ELSE
C             we have an active map
              IF (WIPEIT(MAPWIN).EQ.1 .AND. MAPDEV.EQ.1) THEN
C               window size has been modified, window needs to disappear
                IWAIT = 0
                CALL PDNPLT (MAPWIN,I1,IWAIT)
                WIPEIT(MAPWIN) = 0
C               set map drawn flag to zero since map needs to be drawn
                DRAWN(MAPWIN)  = 0
              END IF
              MODFID(5) = 1
              CALL GEPMAP(MESSFL,SCLU,MXLOC,WDMSFL,WDMPFL,MXCOV,
     I                    BRSFG,NSTA,LAT,LNG,MAPCOV,
     I                    MAPWIN,
     I                    WINDIM,PTHNAM(1),I1,STAID,
     M                    DRAWN(MAPWIN),MODFID,
     O                    MKID,MKX,MKY,MKDIS)
            END IF
          END IF
        END IF
      IF (MAPOPT .LT. 11) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   SGMAPS
     I                   (MESSFL,SCLU,PTHNAM,
     M                    DRAWN)
C
C     + + + PURPOSE + + +
C     specify active map and set specifications
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,DRAWN
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster
C     PTHNAM - character string of path of options selected to get here
C     DRAWN  - map drawn flag
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxmap.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,I0,I1,I8,I,IRET,I4,I6,
     $              CVAL1(4),ID,CNUM,TLEN,CLEN(2),
     $              CVAL(6,MXMAP),MAPID,RETFLG,IVAL1(1),
     $              LPTH,OPS,OPLEN,MNSEL(2),OPVAL(2),CBAS
      INTEGER       MAPDEV,MAPBRD,INTACT,MPNMFG(MXMAP),MPNMCL(MXMAP),
     $              SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL(2),
     $              LEGFLG(MXMAP),LEGCOL(MXMAP),NMAPS,CURMAP
      REAL          MPNMSZ(MXMAP),LSIZE,RTMP(8),
     $              RLAT(2,MXMAP),RLNG(2,MXMAP),
     $              BASEY(MXMAP),BASEX(MXMAP),
     $              LGSIZE(MXMAP),R0,RVAL(4,MXMAP)
      CHARACTER*1   BLNK,TBUFF(8,MXMAP),CTXT(56)
      CHARACTER*8   CMSID(MXMAP)
      CHARACTER*48  WNDNAM
      CHARACTER*64  BASNAM(MXMAP)
C
C     + + + FUNCTIONS + + +
      INTEGER       ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL      ZSTCMA,ZIPI,ZWNSOP,ZLNTXT,PMXTXI
      EXTERNAL      Q1INIT,ZIPC,Q1EDIT,ZWNSET,QSETOP
      EXTERNAL      CARVAR,CVARAR,PRNTXT,QSETR,QGETOP
      EXTERNAL      Q2GTCO,MAPGET,ZIPR,QSETCT,CURPUT
      EXTERNAL      Q2INIT,Q2STCO,Q2SCTF,Q2EDIT,Q2SETR,MAPPUT
      EXTERNAL      QSTCOB,QGETR,QGETCT,QGTCOB,MAPDEL,MAPADD
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      I6   = 6
      I0   = 0
      I8   = 8
      I4   = 4
      R0   = 0.0
      BLNK = ' '
C
C     make previous available
      I= 4
      CALL ZSTCMA (I,I1)
C
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))

  5   CONTINUE
C       assume wont need to return to this screen
        RETFLG = 0
C       get details for individual maps
        CALL MAPGET (MXMAP,
     O               NMAPS,MAPDEV,MAPBRD,INTACT,MPNMFG,MPNMCL,
     O               SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL,
     O               MPNMSZ,RLAT,RLNG,LSIZE,CURMAP,
     O               BASNAM,LEGFLG,BASEY,BASEX,LGSIZE,LEGCOL,
     O               CMSID)
C       clear screen buffer arrays
        CALL ZIPI (6*NMAPS,I0,CVAL)
        CALL ZIPR (4*NMAPS,R0,RVAL)
        CALL ZIPC (8*NMAPS,BLNK,TBUFF)
        DO 10 I = 1,NMAPS
C         fill in screen for each map set
          CVAL(1,I) = 1
          CVAL(2,I) = 1
          IF (CURMAP.NE.I) THEN
C           not active, set to inactive
            CVAL(3,I) = 1
          ELSE
C           active, set that way
            CVAL(3,I) = 2
          END IF
          CVAL(4,I) = 0
          CVAL(5,I) = LEGFLG(I)+1
          CVAL(6,I) = MPNMFG(I)+1
          RVAL(1,I) = RLAT(2,I)
          RVAL(2,I) = RLAT(1,I)
          RVAL(3,I) = RLNG(2,I)
          RVAL(4,I) = RLNG(1,I)
          CALL CVARAR (I8,CMSID(I),I8,TBUFF(1,I))
 10     CONTINUE
        CNUM = 6
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Specify ('//PTHNAM(1)(1:LPTH)//'MS)'
        ELSE
C         no pathname
          WNDNAM= 'Specify (MS)'
        END IF
        CALL ZWNSET (WNDNAM)
        SGRP = 22
        CALL Q2INIT (MESSFL,SCLU,SGRP)
        CALL Q2SETR (I4,NMAPS,RVAL)
        CALL Q2STCO (I6,NMAPS,CVAL)
        CALL Q2SCTF (I4,I8,NMAPS,TBUFF)
        CALL Q2EDIT (NMAPS,
     O               IRET)
        IF (IRET .EQ. 1) THEN
C         accept
          CALL Q2GTCO (I6,NMAPS,
     O                 CVAL)
          MAPID = NMAPS
          I = 1
 20       CONTINUE
C           check each map set for action desired, active/inactive
            IF (CVAL(1,I).EQ.2) THEN
C             this map set is active, set it that way
              CURMAP = I
              RETFLG = 1
              I = I + 1
            ELSE IF (CVAL(1,I).EQ.3) THEN
C             modify this map set
              RETFLG = 1
              SGRP= 23
              IF (LPTH .GT. 0) THEN
C               path name available
                WNDNAM= 'Modify ('//PTHNAM(1)(1:LPTH)//'MSM)'
              ELSE
C               no pathname
                WNDNAM= 'Modify (MSM)'
              END IF
              CALL ZWNSET (WNDNAM)
              CALL Q1INIT(MESSFL,SCLU,SGRP)
C             set real fields
              RTMP(1) = RLAT(2,I)
              RTMP(2) = RLAT(1,I)
              RTMP(3) = RLNG(2,I)
              RTMP(4) = RLNG(1,I)
              RTMP(5) = MPNMSZ(I)
              RTMP(6) = LGSIZE(I)
              RTMP(7) = BASEX(I)
              RTMP(8) = BASEY(I)
              CALL QSETR(I8,RTMP)
C             set option fields
              OPS  = 2
              OPLEN= 2
              MNSEL(1)= 1
              MNSEL(2)= 1
C             set initial values on screen
              IF (MPNMFG(I).EQ.1) THEN
C               map title on
                OPVAL(1)= 1
              ELSE
C               map title off
                OPVAL(1)= 2
              END IF
              IF (LEGFLG(I).EQ.1) THEN
C               legend on
                OPVAL(2)= 1
              ELSE
C               legend off
                OPVAL(2)= 2
              END IF
C             set option fields
              CALL QSETOP (OPS,OPLEN,MNSEL,MNSEL,OPVAL)
C             clear character field for map id and title
              TLEN = 48
              BLNK = ' '
              CLEN(1) = 8
              CLEN(2) = 40
              CALL ZIPC (TLEN,BLNK,CTXT)
              CALL CVARAR (CLEN(1),CMSID(I),CLEN(1),CTXT(1))
              CALL CVARAR (CLEN(2),BASNAM(I),CLEN(2),CTXT(9))
              CNUM = 2
              CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
C             set value of title color
              CNUM = 4
              CVAL1(1) = 0
              CVAL1(2) = 0
              CVAL1(3) = MPNMCL(I) + 1
              CVAL1(4) = LEGCOL(I) + 1
              CBAS = 1
              CALL QSTCOB (CNUM,CBAS,CVAL1)
              CALL Q1EDIT(IRET)
              IF (IRET .NE. 2) THEN
C               user wants to continue
                CALL QGETR(I8,
     O                     RTMP)
                IF ((ABS(RTMP(1)-RLAT(2,I)).GT.0.01) .OR.
     1              (ABS(RTMP(2)-RLAT(1,I)).GT.0.01) .OR.
     2              (ABS(RTMP(3)-RLNG(2,I)).GT.0.01) .OR.
     3              (ABS(RTMP(4)-RLNG(1,I)).GT.0.01)) THEN
C                 at least one boundary has changed, need to redraw
                  RLAT(2,I) = RTMP(1)
                  RLAT(1,I) = RTMP(2)
                  RLNG(2,I) = RTMP(3)
                  RLNG(1,I) = RTMP(4)
                END IF
                MPNMSZ(I) = RTMP(5)
                LGSIZE(I) = RTMP(6)
                BASEX(I)  = RTMP(7)
                BASEY(I)  = RTMP(8)
C               read options selected
                CALL QGETOP (OPLEN,
     O                       OPVAL)
                CNUM = 2
                CALL QGETCT (CNUM,CLEN,TLEN,
     O                       CTXT)
                CALL CARVAR (CLEN(1),CTXT(1),CLEN(1),CMSID(I))
                CALL CARVAR (CLEN(2),CTXT(9),CLEN(2),BASNAM(I))
                CNUM = 4
                CALL QGTCOB (CNUM,CBAS,CVAL1)
                MPNMCL(I) = CVAL1(3) - 1
                LEGCOL(I) = CVAL1(4) - 1
                IF (OPVAL(1).EQ.1) THEN
C                 yes, user wants title
                  MPNMFG(I) = 1
                ELSE IF (OPVAL(1).EQ.2) THEN
C                 no, user doesnt want title
                  MPNMFG(I) = 0
                END IF
                IF (OPVAL(2).EQ.1) THEN
C                 yes, user wants legend
                  LEGFLG(I) = 1
                ELSE IF (OPVAL(2).EQ.2) THEN
C                 no, user doesnt want legend
                  LEGFLG(I) = 0
                END IF
C               put map specs back to common
                CALL MAPPUT (I,MPNMFG(I),MPNMCL(I),
     I                       MPNMSZ(I),RLAT(1,I),RLAT(2,I),
     I                       RLNG(1,I),RLNG(2,I),
     I                       BASNAM(I),LEGFLG(I),BASEY(I),BASEX(I),
     I                       LGSIZE(I),LEGCOL(I),CMSID(I))
              END IF
              I = I + 1
            ELSE IF (CVAL(1,I).EQ.4) THEN
C             drop this map set
              RETFLG = 1
              IF (NMAPS.LE.1) THEN
C               can't drop if only one, tell user
                SGRP = 24
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Drop ('//PTHNAM(1)(1:LPTH)//'MSD)'
                ELSE
C                 no pathname
                  WNDNAM= 'Drop (MSD)'
                END IF
                CALL ZWNSET (WNDNAM)
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                I = I + 1
              ELSE
                CALL MAPDEL (I)
                NMAPS = NMAPS-1
                MAPID = MAPID-1
                IF (NMAPS.GE.I) THEN
C                 there are more lines to follow, move actions up
                  DO 100 ID= I,NMAPS
                    CVAL(1,ID) = CVAL(1,ID+1)
 100              CONTINUE
                END IF
                IF (CURMAP.EQ.I) THEN
C                 dropped active map, make first active
                  CURMAP = 1
                ELSE IF (CURMAP.GT.I) THEN
C                 need to decrement current active map
                  CURMAP = CURMAP - 1
                END IF
              END IF
            ELSE IF (CVAL(1,I).EQ.5) THEN
C             want to copy this map set
              RETFLG = 1
              CALL MAPADD (I,I1,MAPID)
              IF (MAPID.EQ.0) THEN
C               no room for a new map line, tell user
                SGRP = 26
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Copy ('//PTHNAM(1)(1:LPTH)//'MSC) Problem'
                ELSE
C                 no pathname
                  WNDNAM= 'Copy (MSC) Problem'
                END IF
                CALL ZWNSET (WNDNAM)
                IVAL1(1)= MXMAP
                CALL PMXTXI (MESSFL,SCLU,SGRP,I8,I1,I0,I1,IVAL1)
              END IF
              I = I + 1
            ELSE
C             look at the next map set
              I = I + 1
            END IF
          IF (I.LE.NMAPS) GO TO 20
C         put current map to common
          CALL CURPUT (CURMAP)
        END IF
      IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C
C     make previous unavailable
      I= 4
      CALL ZSTCMA (I,I0)
C     may have changed something, force redraw
      DRAWN = 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   MRKMOD
     I                    (MESSFL,SCLU,
     O                     IRET)
C
C     + + + PURPOSE + + +
C     edit marker information
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster containing screen info
C     IRET   - return code from screen
C
C     + + + PARAMETERS + + +
      INCLUDE  'pmxcls.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      IVAL(MXCLAS),I1,I8,CNUM,CVAL(7,3,MXCLAS),
     $             SGRP,NCLAS,I,J,ITMP,IMRK,ISEL,IUNS,ILEN
      REAL         RVAL(1,MXCLAS),MSIZF(MXCLAS)
      CHARACTER*80 TMPBUF
      CHARACTER*1  TBUFF(80,MXCLAS),STRN1(8)
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (STRN1,STRNG)
      CHARACTER*8  STRNG
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESCX,CLGETG,CLGET,CLPUTG,CLCNT,GGTGLV,ZGTRET
      EXTERNAL     CLPUT
C
C     + + + READ FORMATS + + +
1000  FORMAT(80A1)
C
C     + + + END SPECIFICATIONS + + +
C
      SGRP= 51
      I1  = 1
      I8  = 8
      CNUM= 7
C
      CALL CLCNT(NCLAS)
      DO 10 I= 1,NCLAS
        TMPBUF= ' '
        CALL CLGET(I,TMPBUF(1:8),TMPBUF(9:16),TMPBUF(17:24),ITMP)
        IF (TMPBUF(1:8).EQ.'        ') THEN
          TMPBUF(1:8) = '<all>   '
        END IF
        IF (TMPBUF(9:16).EQ.'        ') THEN
          TMPBUF(9:16) = '<all>   '
        END IF
        IF (TMPBUF(17:24).EQ.'        ') THEN
          TMPBUF(17:24) = '<all>   '
        END IF
        IF (ITMP.EQ.1) THEN
          TMPBUF(25:28)= 'YES '
        ELSE
          TMPBUF(25:28)= 'NO  '
        END IF
        CALL CLGETG(I,IMRK,IUNS,ISEL,MSIZF(I))
        STRNG= '?SYMBOL'
        CALL GGTGLV(I8,IMRK+1,STRN1,ILEN)
        TMPBUF(29:36)= STRNG
        TMPBUF(37:42)= '      '
        STRNG= '?COLOR'
        CALL GGTGLV(I8,ISEL+1,STRN1,ILEN)
        TMPBUF(43:50)= STRNG
        STRNG= '?COLOR'
        CALL GGTGLV(I8,IUNS+1,STRN1,ILEN)
        TMPBUF(51:58)= STRNG
        READ(TMPBUF,1000) (TBUFF(J,I),J=1,80)
C       set marker size for this class
        RVAL(1,I) = MSIZF(I)
 10   CONTINUE
      CALL QRESCX(MESSFL,SCLU,SGRP,I1,I1,CNUM,NCLAS,I1,
     M            IVAL,RVAL,CVAL,TBUFF)
      CALL ZGTRET(IRET)
      IF (IRET .NE. 2) THEN
        DO 20 I= 1,NCLAS
          IF (CVAL(4,1,I) .EQ. 2) THEN
C           active
            ITMP= 1
          ELSE
C           not active
            ITMP= 0
          END IF
          WRITE(TMPBUF,1000) (TBUFF(J,I),J=1,80)
          IF (TMPBUF(1:8).EQ.'<all>   ') THEN
            TMPBUF(1:8) = '        '
          END IF
          IF (TMPBUF(9:16).EQ.'<all>   ') THEN
            TMPBUF(9:16) = '        '
          END IF
          IF (TMPBUF(17:24).EQ.'<all>   ') THEN
            TMPBUF(17:24) = '        '
          END IF
          CALL CLPUT (I,TMPBUF(1:8),TMPBUF(9:16),TMPBUF(17:24),ITMP)
          IF ((ABS(RVAL(1,I)-MSIZF(I))).GT.0.01) THEN
C           marker size changed
            MSIZF(I) = RVAL(1,I)
          END IF
          CALL CLPUTG (I,CVAL(5,1,I)-1,CVAL(7,1,I)-1,CVAL(6,1,I)-1,
     I                 MSIZF(I))
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GEPMAP
     I                   (MESSFL,SCLU,MXSTA,WDMSFL,WDMPFL,MXCOV,
     I                    BRSFG,NSTA,LAT,LNG,MAPCOV,
     I                    WINDOW,
     I                    WINDIM,PTHNAM,DOANIM,STAID,
     M                    DRAWN,MODFID,
     O                    MKID,MKX,MKY,MKDIS)
C
C     + + + PURPOSE + + +
C     produce a map
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,MXSTA,WDMSFL,WDMPFL,MXCOV,NSTA,
     $          MAPCOV(MXCOV),DOANIM,BRSFG,
     $          WINDOW,DRAWN,MODFID(6),STAID(NSTA)
      REAL      WINDIM(4,7),
     $          MKID(MXSTA),MKX(MXSTA),MKY(MXSTA),MKDIS(MXSTA),
     $          LAT(MXSTA),LNG(MXSTA)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster containing status messages
C     MXSTA  - maximum number of stations
C     BRSFG  - browse flag: 1-go to browse screen from map click
C     WDMSFL - unit number of file containing stations with attr to map
C     WDMPFL - boundary file unit number
C     MXCOV  - maximum number of available coverages
C     NSTA   - number of stations found
C     LAT    - latitude of station in s.g., dummy in exp sys
C     LNG    - longitude of station in s.g., dummy in exp sys
C     MAPCOV - available coverages available
C              1-state,2-county,3-hydro region,4-hydro acct unit,
C              5-all hydrography,6-all hydrography,7-MLRA
C     PTHNAM - character string of path of options selected to get here
C     MKID   - marker id
C     MKX    - marker X coordinate
C     MKY    - marker Y coordinate
C     MKDIS  - marker distance from current pick point
C     STAID  - data set numbers of stations selected in expert system,
C              id's of all stations in scenerio generators
C     WINDOW - currently active map window
C     WINDIM - array of window dimensions and locations
C     DRAWN  - map drawn flag - 1-has been drawn here already,0-not drawn yet
C     MODFID - flag indicating that one item of map has been modified
C              (1)-state bndrys, (2)-county bndrys, (3)-region bndrys,
C              (4)-accntg units, (5)-hydrography,   (6)-markers
C     DOANIM - do animation flag
C
C     + + + PARAMETERS + + +
      INTEGER   POPCNT,MXDSN
      PARAMETER (POPCNT=5)
      PARAMETER (MXDSN=500)
      INCLUDE  'pmxcls.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmaprm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LDEVCD,IMET,IWAIT,INPT,NCOLI,HANCNT,I6,INTACT,
     $            JCLR,ICLR,IFIL,ISTY,DSN,AT1,AT2,IANS,LPTH,FILLIT,
     $            OPTID(POPCNT),MCLR,ALBX,ALBY,MTYP,CMPTYP,WINACT,
     $            I,I0,I1,I2,J,IRET,SGRP,PLTCNT,DEVFIL,ZOOMED,ICLOS,
     $            MAPMRK(3),NCLACT,NCLAS,IACT,CLACT(MXCLAS),MAPDEV,
     $            MPNMFG,MPNMCL,BACKCL(2),K,STORFG,NDSN,L,ACTFLG,
     $            ADSN(MXDSN),STALOC(MXDSN,MXCLAS),TMPLOC,CLICKD,
     $            MKSTAT(MXDSN),LEGFLG,ICOL,RESP,MAPNUM,NEWMAP,
     $            MAPBRD
      REAL        TXPHYS,TXPAGE,TYPHYS,TYPAGE,RXDC,RYDC,XYOUT,
     $            TXMIN,TXMAX,TYMIN,TYMAX,PX(1),PY(1),MSIZF,
     $            MPNMSZ,BASEX,BASEY,LGSIZE,TRLAT(2),TRLNG(2),BMSIZE,
     $            MSIZ(MXCLAS)
      REAL*8      DLAT,DLNG,DALBX,DALBY,DYMIN,DYMAX,DXMIN,DXMAX
      CHARACTER*5 OPTNAM(POPCNT)
      CHARACTER*8 SEN,LOC,CON
      CHARACTER*48 WNDNAM
      CHARACTER*64 BASNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + INTRINSIC + + +
      INTRINSIC   FLOAT,INT
C
C     + + + EXTERNALS + + +
      EXTERNAL    GPINIT,GMARK1,GEMAGT,GEMADR,PLTXXX,TSDSAS
      EXTERNAL    GSETMP,PMXCNW,GEDOPT,CLLAL1,CLGDSN,CLFDSN,MLGGET
      EXTERNAL    MSETUP,PDNPLT,PRNTXI,DNPLTW,ZIPI,TSDSLC,TSDSAC
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,ZQUIET,GSPMCI,GETITL,GELEGD
      EXTERNAL    CLCNT,CLGETG,CLGET,MINGET,MDVGET,MNMGET,MBKGET
      EXTERNAL    QRESP,MLLGET,MLLPUT,MAPSEG,MAPCUR,MAPADD,MAPSPA
      EXTERNAL    MBRGET,CALLL1
C     EXTERNAL    STALCA
C
C     + + + DATA INITIALIZATIONS + + +
      DATA OPTNAM/'Selct','Draw','Zoom','Full','Retrn'/
      DATA OPTID /  -1   ,  -2  ,  -3  ,  -4  ,   -5  /
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I2   = 2
      I6   = 6
C
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
C     get current map number
      CALL MAPCUR (
     O             MAPNUM)
C
C     initialize plot common
      CALL GPINIT
C     set device code, etc.
      CALL MDVGET (
     O             MAPDEV)
      CALL PLTXXX (MAPDEV,WINDOW,WINDIM(1,WINDOW),
     M             DEVFIL,
     O             LDEVCD,CMPTYP,WINACT,PLTCNT)
C
      BMSIZE = 1.0
      WRITE(99,*) 'befor MSETUP'
C     activate workstation
      CALL MSETUP(MAPDEV,LDEVCD,WINACT,WINDIM,
     M            BMSIZE,
     O            IRET,IWAIT,IMET)
      WRITE(99,*) 'after MSETUP:',IRET,IWAIT,IMET
C
C     force use of user spec boundary
      IANS  = -4
      ZOOMED= 0
C     map draw loop
 25   CONTINUE
        WRITE(99,*) '** MAP DRAW LOOP **',IANS
        IF (ZOOMED.NE.1) THEN
C         not just zoomed
          CALL MLLGET(MAPNUM,
     O                TRLAT,TRLNG)
C         get map border from common
          CALL MBRGET (
     O                 MAPBRD)
C         figure out display space coordinates
          CALL MAPSPA (MAPBRD,NSTA,WDMSFL,STAID,
     M                 TRLAT,TRLNG,
     O                 TXMIN,TXMAX,TYMIN,TYMAX)
        END IF 
C       adapt boundary for gks routines
        TXPHYS= TXMIN
        TXPAGE= TXMAX- TXMIN
        TYPHYS= TYMIN
        TYPAGE= TYMAX- TYMIN
C       open workstation
        IF (MAPDEV.GT.1 .OR. CMPTYP.EQ.5) THEN
C         status to hard copy device
          SGRP= 68
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Status'
          ELSE
C           no pathname
            WNDNAM= 'Draw (MD) Status'
          END IF
          CALL ZWNSET (WNDNAM)
          CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
        END IF
        WRITE(99,*) 'befor GSETMP:',LDEVCD
        WRITE(99,*) '             ',TXPHYS,TYPHYS,TXPAGE,TYPAGE
        CALL GSETMP (TXPHYS,TYPHYS,TXPAGE,TYPAGE,LDEVCD,WINACT,DRAWN,
     O               XYOUT,NCOLI,RXDC,RYDC,INPT)
C       find out if map interaction is on
        CALL MINGET (
     O               INTACT)
        WRITE(99,*) 'after GSETMP:',NCOLI,INPT,INTACT
        WRITE(99,*) '             ',XYOUT,RXDC,RYDC
C
C       get background fill info from common
        CALL MBKGET (
     O               BACKCL)
        FILLIT = BACKCL(1)
C
        DO 27 J = 1,2
C         do 2 passes, first to do filled areas, then lines
C
          IF ((FILLIT.EQ.2 .AND. J.EQ.1) .OR.
     1        (J.EQ.2)) THEN
C           get here on first pass if want to fill or 2nd pass if no fill
            IF (MAPCOV(2).GT.1 .AND.
     1        (MODFID(2).EQ.1.OR.DRAWN.EQ.0)) THEN
C             draw counties
              WRITE(99,*) 'about to draw counties',MAPCOV(2)
              IF (MAPDEV.GT.1 .OR. CMPTYP.EQ.5) THEN
C               status to hard copy device
                SGRP= 67
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Status'
                ELSE
C                 no pathname
                  WNDNAM= 'Draw (MD) Status'
                END IF
                CALL ZWNSET (WNDNAM)
                CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
              END IF
              JCLR= MAPCOV(2)- 1
              AT2 = 0
              AT1 = 0
              IF (BACKCL(1).EQ.2 .AND. J.EQ.1) THEN
C               want to shade in counties
                ICLR = BACKCL(2)
                IFIL = 1
                ISTY = -2
              ELSE
                ICLR = 0
                IFIL = 0
                ISTY = 0
              END IF
              CALL GEMADR (WDMPFL,DSNCOU,AT1,AT2,JCLR,ICLR,IFIL,ISTY)
            END IF
          END IF
C
          IF ((FILLIT.EQ.1 .AND. J.EQ.1) .OR.
     1        (J.EQ.2)) THEN
            IF (MAPCOV(1).GT.1 .AND.
     1          (MODFID(1).EQ.1.OR.DRAWN.EQ.0)) THEN
C             draw states
              WRITE(99,*) 'about to draw states',MAPCOV(1)
              IF (MAPDEV.GT.1 .OR. CMPTYP.EQ.5) THEN
C               status to hard copy device
                SGRP= 63
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Status'
                ELSE
C                 no pathname
                  WNDNAM= 'Draw (MD) Status'
                END IF
                CALL ZWNSET (WNDNAM)
                CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
              END IF
              JCLR= MAPCOV(1)- 1
              DSN = 100
              AT2 = 0
              AT1 = 0
              IF (BACKCL(1).EQ.1 .AND. J.EQ.1) THEN
C               want to shade in states
                ICLR = BACKCL(2)
                IFIL = 1
                ISTY = -2
              ELSE
                ICLR = 0
                IFIL = 0
                ISTY = 0
              END IF
              CALL GEMADR (WDMPFL,DSN,AT1,AT2,JCLR,ICLR,IFIL,ISTY)
            END IF
          END IF
C
          IF ((FILLIT.EQ.4 .AND. J.EQ.1) .OR.
     1        (J.EQ.2)) THEN
            IF (MAPCOV(4).GT.1 .AND.
     1          (MODFID(4).EQ.1.OR.DRAWN.EQ.0)) THEN
C             draw accounting units
              WRITE(99,*) 'about to draw acct units',MAPCOV(4)
              IF (MAPDEV.GT.1 .OR. CMPTYP.EQ.5) THEN
C               status to hard copy device
                SGRP= 61
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Status'
                ELSE
C                 no pathname
                  WNDNAM= 'Draw (MD) Status'
                END IF
                CALL ZWNSET (WNDNAM)
                CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
              END IF
              JCLR= MAPCOV(4)- 1
              AT1 = 0
              AT2 = 0
              IF (BACKCL(1).EQ.4 .AND. J.EQ.1) THEN
C               want to shade in accnting units
                ICLR = BACKCL(2)
                IFIL = 1
                ISTY = -2
              ELSE
                ICLR = 0
                IFIL = 0
                ISTY = 0
              END IF
              CALL GEMADR (WDMPFL,DSNACU,AT1,AT2,JCLR,ICLR,IFIL,ISTY)
            END IF
          END IF
C
          IF ((FILLIT.EQ.3 .AND. J.EQ.1) .OR.
     1        (J.EQ.2)) THEN
            IF (MAPCOV(3).GT.1 .AND.
     1          (MODFID(3).EQ.1.OR.DRAWN.EQ.0)) THEN
C             draw regions
              WRITE(99,*) 'about to draw regions',MAPCOV(3)
              IF (MAPDEV.GT.1 .OR. CMPTYP.EQ.5) THEN
C               status to hard copy device
                SGRP= 62
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Status'
                ELSE
C                 no pathname
                  WNDNAM= 'Draw (MD) Status'
                END IF
                CALL ZWNSET (WNDNAM)
                CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
              END IF
              JCLR= MAPCOV(3)- 1
              DSN = 300
              AT1 = 0
              AT2 = 0
              IF (BACKCL(1).EQ.3 .AND. J.EQ.1) THEN
C               want to shade in hydro regions
                ICLR = BACKCL(2)
                IFIL = 1
                ISTY = -2
              ELSE
                ICLR = 0
                IFIL = 0
                ISTY = 0
              END IF
              CALL GEMADR (WDMPFL,DSN,AT1,AT2,JCLR,ICLR,IFIL,ISTY)
            END IF
          END IF
C
          IF (MAPCOV(5).GT.1 .AND.
     1        (MODFID(5).EQ.1.OR.DRAWN.EQ.0)) THEN
C           draw all hydrography
            WRITE(99,*) 'about to draw all hydrography',MAPCOV(5)
            IF (MAPDEV.GT.1 .OR. CMPTYP.EQ.5) THEN
C             status to hard copy device
              SGRP= 64
              IF (LPTH .GT. 0) THEN
C               path name available
                WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Status'
              ELSE
C               no pathname
                WNDNAM= 'Draw (MD) Status'
              END IF
              CALL ZWNSET (WNDNAM)
              CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
            END IF
            JCLR= MAPCOV(5)- 1
            AT1 = 0
            AT2 = 0
            IF (J.EQ.1) THEN
C             try to shade in hydrography
              ICLR = JCLR
              IFIL = 1
              ISTY = -2
            ELSE
C             just draw lines
              ICLR = 0
              IFIL = 0
              ISTY = 0
            END IF
            CALL GEMADR (WDMPFL,DSNALL,AT1,AT2,JCLR,ICLR,IFIL,ISTY)
          END IF
C
          IF (MAPCOV(6).GT.1 .AND.
     1        (MODFID(5).EQ.1.OR.DRAWN.EQ.0)) THEN
C           draw major hydrography
            WRITE(99,*) 'about to draw maj hydrography',MAPCOV(6)
            IF (MAPDEV.GT.1 .OR. CMPTYP.EQ.5) THEN
C             status to hard copy device
              SGRP= 64
              IF (LPTH .GT. 0) THEN
C               path name available
                WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Status'
              ELSE
C               no pathname
                WNDNAM= 'Draw (MD) Status'
              END IF
              CALL ZWNSET (WNDNAM)
              CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
            END IF
            JCLR= MAPCOV(6)- 1
            AT1 = 0
            AT2 = 0
            IF (J.EQ.1) THEN
C             try to shade in hydrography
              ICLR = JCLR
              IFIL = 1
              ISTY = -2
            ELSE
C             just draw lines
              ICLR = 0
              IFIL = 0
              ISTY = 0
            END IF
            CALL GEMADR (WDMPFL,DSNMAJ,AT1,AT2,JCLR,ICLR,IFIL,ISTY)
            IF (DOANIM.EQ.1 .AND. J.EQ.2) THEN
C             animation wanted
              CALL MAPSEG (MESSFL,SCLU,WDMPFL,DSNMAJ)
            END IF
          END IF
C
 27     CONTINUE
C
        IF (MODFID(6).EQ.1.OR.DRAWN.EQ.0) THEN
          HANCNT= 0
C         station markers
          WRITE(99,*) 'before draw markers:',NSTA
          IF (INTACT .GT. 0) THEN
C           save space for option handles
            J= MXSTA- POPCNT
          ELSE
C           no interaction with user, dont save space
            J= MXSTA
          END IF
C         what classes are active?
          CALL CLCNT(NCLAS)
          NCLACT= 0
          DO 28 I= 1,NCLAS
            CALL CLGET(I,SEN,LOC,CON,IACT)
            IF (IACT.EQ.1) THEN
C             this class is active
              NCLACT= NCLACT+ 1
              CLACT(NCLACT)= I
            END IF
 28       CONTINUE
          IF (NCLACT .GT. 0) THEN
C           some active classes to draw
            DO 29 K= 1,NCLACT
C             marker stuff
              CALL CLGETG (CLACT(K),
     O                     MAPMRK(1),MAPMRK(3),MAPMRK(2),MSIZF)
C
              WRITE(99,*)
              write(99,*) 'map marker type, colors:',mapmrk
C
C             get number of data sets in this class
              CALL CLGDSN (CLACT(K),MXDSN,I0,
     O                     NDSN,ADSN)
              DO 30 I= 1,NDSN
C               get location associated with each data set in class
                CALL TSDSLC (ADSN(I),
     O                       STALOC(I,CLACT(K)))
C                IF (MKSTAT(I).LT.1) THEN
C                 in expert system, need to read lat/lng from .wdm file
C                  CALL STALCA (WDMSFL,STALOC(I),
C     M                         ALBX,ALBY)
C                ELSE IF (MKSTAT(I).GE.1) THEN
C                 in sen gen, already have lat/lng, need to get albers
                DLAT = LAT(STALOC(I,CLACT(K)))
                DLNG = -LNG(STALOC(I,CLACT(K)))
                CALL CLLAL1 (DLAT,DLNG,
     O                       DALBX,DALBY)
                ALBX = DALBX
                ALBY = DALBY
C                END IF
C               look to see if this handle has been stored
                STORFG = 0
                IF (HANCNT.GT.0) THEN
                  DO 31 L = 1,HANCNT
                    IF (INT(MKID(L)).EQ.STALOC(I,CLACT(K))) THEN
C                     already stored, flag it
                      STORFG = 1
                    END IF
 31               CONTINUE
                END IF
                PX(1)= FLOAT(ALBX)
                PY(1)= FLOAT(ALBY)
                IF (STORFG.EQ.0) THEN
C                 we need to store this handle
                  HANCNT= HANCNT+ 1
                  MKID(HANCNT)= STALOC(I,CLACT(K))
C                 WRITE (99,*) 'handle:',HANCNT,STALOC(I),ALBX,ALBY
C                 x position
                  MKX(HANCNT) = PX(1)
C                 y position
                  MKY(HANCNT) = PY(1)
                END IF
C               get correct status for this marker
                MKSTAT(STALOC(I,CLACT(K))) = 2
                DO 32 L = 1,NDSN
C                 check all data sets at this location to see if on
                  CALL TSDSLC (ADSN(L),
     O                         TMPLOC)
                  IF (TMPLOC.EQ.STALOC(I,CLACT(K))) THEN
C                   this location, see if selected
                    CALL TSDSAC (ADSN(L),
     O                           ACTFLG)
                    IF (ACTFLG.EQ.1) THEN
                      MKSTAT(STALOC(I,CLACT(K))) = 1
                    END IF
                  END IF
  32            CONTINUE
C
C               get correct marker color
                IF (MKSTAT(STALOC(I,CLACT(K))).EQ.1) THEN
C                 marker should show up as selected
                  MCLR = 2
                ELSE IF (MKSTAT(STALOC(I,CLACT(K))).EQ.2) THEN
C                 marker should show up as unselected
                  MCLR = 3
                END IF
C
C               marker size (world coords)
                MSIZ(K) = (TYMAX-TYMIN)*BMSIZE*MSIZF/50
                MTYP = MAPMRK(1)
C               set polymarker color in gks
                CALL GSPMCI (MAPMRK(MCLR))
C               display marker
                CALL GMARK1 (MTYP,MSIZ(K),PX,PY)
C               CALL GSMK (MTYP)
C               CALL GSMKSC (MSIZ(K))
C               CALL GPM(I1,PX,PY)
 30           CONTINUE
 29         CONTINUE
          ELSE
            WRITE(99,*) 'no active classes to draw'
          END IF
        END IF
C
C       get map name information
        CALL MNMGET (MAPNUM,
     O               MPNMFG,BASNAM,MPNMCL,MPNMSZ)
        IF (MPNMFG.EQ.1) THEN
C         user wants map to have title - basin name
          CALL GETITL (BASNAM,MPNMCL,MPNMSZ,XYOUT,TYPHYS,TYPAGE,
     I                 TXPAGE,TXPHYS)
        END IF
C       get map legend information
        CALL MLGGET (MAPNUM,
     O               LEGFLG,BASEY,BASEX,LGSIZE,ICOL)
        IF (LEGFLG.EQ.1) THEN
C         user wants map to have legend
          CALL GELEGD (XYOUT,TYMIN,TYMAX,TXPAGE,TXPHYS,BMSIZE,
     I                 BASEY,BASEX,LGSIZE,ICOL)
        END IF
C
        WRITE(99,*) 'MAPDEV,INPT,HANCNT,INTACT,DOANIM',MAPDEV,INPT,
     $               HANCNT,INTACT,DOANIM
        IF (MAPDEV.EQ.1 .AND. INPT.GT.0 .AND.
     $      INTACT.GT.0 .AND. DOANIM.EQ.0) THEN
C         interact with user, set default
          IANS= -INTACT
C         display option identifiers
          CALL GEDOPT (I1,XYOUT,TYMIN,TXMIN,
     I                 POPCNT,OPTID,OPTNAM,IANS,LDEVCD,
     M                 HANCNT,MKID,MKX,MKY)
C         identify stations
C         WRITE(99,*) 'befor GEMAGT:',NSTA,HANCNT
C         WRITE(99,*) 'select stat :',(MKSTAT(I),I=1,NSTA)
          IF (CMPTYP.EQ.5) THEN
C           tell user what to do in aide window
            SGRP= 66
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Draw ('//PTHNAM(1)(1:LPTH)//'MD) Status'
            ELSE
C             no pathname
              WNDNAM= 'Draw (MD) Status'
            END IF
            CALL ZWNSET (WNDNAM)
            CALL PMXCNW (MESSFL,SCLU,SGRP,50,1,1,I)
C           turn off middle window
            CALL ZQUIET
          END IF
          CALL GEMAGT (INPT,LDEVCD,HANCNT,BRSFG,
     I                 RXDC,RYDC,XYOUT,TXPHYS,TYPHYS,
     I                 POPCNT,OPTID,OPTNAM,
     I                 WDMSFL,MKID,MKX,MKY,WINACT,
     I                 NCLACT,CLACT,MSIZ,
     M                 MKDIS,MKSTAT,TXMIN,TXMAX,TYMIN,TYMAX,IANS,ZOOMED,
     O                 CLICKD)
          IF (IANS.EQ.-2 .OR. ZOOMED.EQ.1) THEN
C           draw or zoom option has been selected, must allow redraw
            DRAWN = 0
          END IF
C         WRITE(99,*) 'select stat :',(MKSTAT(I),I=1,NSTA)
C         no more waiting needed
          IWAIT= 0
          IF (CLICKD.EQ.1) THEN
C           user clicked on a point, may need to update data sets on
            IF (BRSFG.EQ.0) THEN
C             didnt browse, toggle all on/off
              DO 33 L = 1,NSTA
C               check all locations
                DO 34 K= 1,NCLACT
C                 get number of data sets in this class
                  CALL CLGDSN (CLACT(K),MXDSN,I0,
     O                         NDSN,ADSN)
                  DO 35 I= 1,NDSN
C                   get location associated with each data set in class
                    CALL TSDSLC (ADSN(I),
     O                           TMPLOC)
                    IF (TMPLOC.EQ.L) THEN
C                     this data set should be toggled on/off
                      IF (MKSTAT(L).EQ.2) THEN
C                       all dsns here need to be off
                        CALL TSDSAS (ADSN(I),I0)
                      ELSE IF (MKSTAT(L).EQ.1) THEN
C                       all dsns here need to be on
                        CALL TSDSAS (ADSN(I),I1)
                      END IF
                    END IF
  35              CONTINUE
  34            CONTINUE
  33          CONTINUE
            END IF
            DO 36 K= 1,NCLACT
C             recalc specs for class
              CALL CLFDSN(CLACT(K))
  36        CONTINUE
          END IF
        ELSE
C         force one pass
          IANS= -5
        END IF
      IF (IANS .NE. -5) GO TO 25
C
      IF (MAPDEV.EQ.1 .AND. CMPTYP.EQ.5) THEN
C       turn off option identifiers
        CALL GEDOPT (I1,XYOUT,TYMIN,TXMIN,
     I               POPCNT,OPTID,OPTNAM,I0,LDEVCD,
     M               HANCNT,MKID,MKX,MKY)
        IF (ZOOMED.EQ.0) THEN
C         zoom hasn't been used this time through, ok to set
C         drawn flag to yes
          DRAWN = 1
        END IF
      END IF
C
C     all map modified flags can be set back to zero
      CALL ZIPI (I6,I0,MODFID)
C
      IF (MAPDEV.GT.1 .AND. CMPTYP.EQ. 5) THEN
C       close print file on aviion
        CLOSE (UNIT=DEVFIL)
C       close workstation
        IWAIT = 0
        CALL PDNPLT(WINACT,I1,IWAIT)
        IWAIT = 0
C       graph put on file 'gksplt.out//n'
        SGRP= 65
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Draw complete ('//PTHNAM(1)(1:LPTH)//'MD)'
        ELSE
C         no pathname
          WNDNAM= 'Draw complete (MD)'
        END IF
        CALL ZWNSET (WNDNAM)
        CALL PRNTXI(MESSFL,SCLU,SGRP,PLTCNT)
      ELSE
C       deactivate workstation
        IF (CMPTYP .NE. 1) THEN
C         dont close workstation
          ICLOS= 0
        ELSE
C         close workstation on pc
          ICLOS= 1
        END IF
        CALL DNPLTW(IWAIT,WINACT,ICLOS,CMPTYP,IMET)
      END IF
      IF (ZOOMED.EQ.1) THEN
C       zoom option has been used
C       do you want to save zoomed specs?
        SGRP= 110
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Draw complete ('//PTHNAM(1)(1:LPTH)//'MD)'
        ELSE
C         no pathname
          WNDNAM= 'Draw complete (MD)'
        END IF
        CALL ZWNSET (WNDNAM)
        RESP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        IF (RESP.EQ.1) THEN
C         save specs
          CALL MAPADD (MAPNUM,I2,
     O                 NEWMAP)
          DXMIN   = TXMIN
          DYMIN   = TYMIN
          CALL CALLL1 (DXMIN,DYMIN,
     O                 DLAT,DLNG)
          TRLAT(1)= DLAT
          TRLNG(1)= -1.0*DLNG
C
          DXMAX   = TXMAX
          DYMAX   = TYMAX
          CALL CALLL1 (DXMAX,DYMAX,
     O                 DLAT,DLNG)
          TRLAT(2)= DLAT
          TRLNG(2)= -1.0*DLNG
C
          IF (NEWMAP.GT.0) THEN
C           go ahead and create new set of map specifications
            CALL MLLPUT (NEWMAP,TRLAT,TRLNG)
          END IF
        ELSE
C         allow map to be redrawn
          DRAWN = 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GEMADR
     I                   (WDMPFL,DSN,AT1,AT2,JCLR,ICLR,IFIL,ISTY)
C
C     + + + PURPOSE + + +
C     display the boundaries of an item
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMPFL,DSN,AT1,AT2,JCLR,ICLR,IFIL,ISTY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMPFL - unit number of wdm boundary file (with dlgs)
C     DSN    - dataset number to display
C     AT1    - first attribute
C     AT2    - second attribute
C     JCLR   - line color code to use when making display
C     ICLR   - fill color code to use when making display
C     IFIL   - fill code to use (0-no fill)
C     ISTY   - fill style index
C
C     + + + PARAMETERS + + +
      INTEGER   TPTS,TPTS2
      PARAMETER(TPTS=19200,TPTS2=TPTS*2)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,K,ID,NPTS,LPTS,RPTS,CPTS,FE,ITYPE,DOPOLY,
     1          MXPTS,RETCOD,ATUSE,IPOS,XPTS,
     2          ATTYP(250),AT1TM(250),AT2TM(250)
      REAL      DLGBUF(TPTS2),DLGY(TPTS)
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (DLGBUF,DLGX)
      REAL      DLGX(TPTS)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDLGET, GSPLCI, GPL, GSFAI, GSFAIS, GSFASI, GSFACI, GFA
      EXTERNAL  WDLLSU
C
C     + + + END SPECIFICATIONS + + +
C
C     error unit number
      FE= 99
C     WRITE(FE,*) 'in GEMADR:',WDMPFL,DSN,AT1,AT2,JCLR,ICLR,IFIL,ISTY
C
      IF (IFIL.EQ.0) THEN
C       no fill wanted, look for lines and polygons
        ITYPE= 1
        MXPTS= 300 
      ELSE
C       fill wanted, look for polygons only
        ITYPE= 2
        MXPTS= 4800
      END IF
C
      IF (AT1.EQ.0 .AND. AT2.EQ.0) THEN
C       need to look at all attributes on dsn
        CALL WDLLSU(WDMPFL,DSN,250,
     O              ATUSE,ATTYP,AT1TM,AT2TM,RETCOD)
C       WRITE(FE,*) 'after WDLLSU:',ATUSE
      ELSE
        ATUSE= 1
        ATTYP(1)= 0
        AT1TM(1)= AT1
        AT2TM(1)= AT2
      END IF
C
 40   CONTINUE
C       return here to draw polygon outlines
        DO 30 K= 1,ATUSE
          IPOS= 1
 10       CONTINUE
            ID= 2
            CALL WDLGET (WDMPFL,DSN,ITYPE,AT1TM(K),AT2TM(K),TPTS,
     M                   ID,
     O                   NPTS,DLGBUF(IPOS),RETCOD)
C           WRITE(99,*) 'WDLGET:',TPTS,NPTS,IPOS,RETCOD
            XPTS= NPTS/2
            DO 11 I= IPOS,IPOS+XPTS
              DLGY(I)= DLGBUF(I+XPTS)
 11         CONTINUE
            IPOS= IPOS+ XPTS
C           WRITE(FE,*) 'draw:',WDMPFL,DSN,J,AT1TM(K),AT2TM(K),TPTS,
C    1                          ID,NPTS,RETCOD
            IF (RETCOD.LT.0) THEN
C             WRITE (FE,*) 'tried to draw:',DSN,AT1TM(K),AT2TM(K),RETCOD
              IF (RETCOD.NE.-50) THEN
                WRITE (FE,*) 'RETCOD,NPTS',RETCOD,NPTS
C                WRITE (FE,*) 'ID,J  :',ID,J
                WRITE (FE,*) 'DLGBUF:',(DLGBUF(I),I=1,5)
              END IF
            ELSE IF (RETCOD.EQ.2 .OR. IFIL.EQ.0) THEN
C             points for line or area
              NPTS= IPOS-1
              RPTS= NPTS
              CPTS= 1
 20           CONTINUE
                LPTS= RPTS
                IF (LPTS.GT.MXPTS) THEN
C                 plot a bit at a time
                  LPTS= MXPTS
                END IF
C               plot the line
C               WRITE (FE,*) AT1,AT2,NPTS,LPTS,RPTS
C               WRITE (FE,*) DLGX(CPTS),DLGX(CPTS+LPTS-1),
C    1                       DLGY(CPTS),DLGY(CPTS+LPTS-1)
                IF (IFIL.EQ.0) THEN
C                 just do a line, first set color
                  CALL GSPLCI(JCLR)
                  IF (LPTS.GT.0) THEN
                    CALL GPL(LPTS,DLGX(CPTS),DLGY(CPTS))
                  END IF
                ELSE
C                 set fill area index
                  I= IFIL
                  CALL GSFAI(I)
C                 set fill area interior style
                  CALL GSFAIS(IFIL)
C                 set fill area style index
                  CALL GSFASI(ISTY)
C                 set fill area color
                  CALL GSFACI(ICLR)
C                 WRITE(99,*) 'I,ICLR,IFIL,ISTY:',I,ICLR,IFIL,ISTY
C                 a filled polygon
                  CALL GFA(LPTS,DLGX(CPTS),DLGY(CPTS))
C                 draw a line around polygon to show boundary
C                 set county boundary color
                  CALL GSPLCI(JCLR)
                  CALL GPL(LPTS,DLGX(CPTS),DLGY(CPTS))
                END IF
                RPTS= RPTS- LPTS+ 1
                CPTS= CPTS+ LPTS- 1
              IF (RPTS.GT.1) GO TO 20
C             clear out buffer
              IPOS= 1
            END IF
          IF (RETCOD.EQ.0) GO TO 10
 30     CONTINUE
C
        DOPOLY = 0
        IF (ITYPE.EQ.1) THEN
C         need to draw polygon outlines as well
          ITYPE= 2
          MXPTS= 4800
          DOPOLY= 1
        END IF
      IF (DOPOLY.EQ.1) GO TO 40
C
      RETURN
      END
C
C
C
      SUBROUTINE   MAPCOM
     I                   (XLAM0,XDSNCO,XDSNAC,XDSNAL,XDSNMA)
C
C     + + + PURPOSE + + +
C     set map info in map-library common block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   XDSNCO,XDSNAC,XDSNAL,XDSNMA
      REAL      XLAM0
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLAM0  - base for albers coordinates
C     XDSNCO - boundary data set number
C     XDSNAC - boundary data set number
C     XDSNAL - boundary data set number for all hydrology
C     XDSNMA - boundary data set number for major hydrology
C
C     + + + COMMON BLOCKS + + +
      INCLUDE  'cmaprm.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     base for albers coords
      LAM0  = XLAM0
C     boundary datasets
      DSNCOU= XDSNCO
      DSNACU= XDSNAC
      DSNALL= XDSNAL
      DSNMAJ= XDSNMA
C
      RETURN
      END
