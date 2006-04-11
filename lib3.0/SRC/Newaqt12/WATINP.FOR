C
C
C
      SUBROUTINE   WATINI
C
C     + + + PURPOSE + + +
C     initialize watstore parameters
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,VERS,SPACE(5),TIME(7)
      REAL          MISVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    INITAT,INWIOX,INITMS
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C
      SPACE(1)= 35
      SPACE(2)= 70
      SPACE(3)= 150
      SPACE(4)= 1
      SPACE(5)= 1
      TIME(1) = 4
      TIME(2) = 6
      TIME(3) = 1
      TIME(4) = 1
      TIME(5) = 1
      TIME(6) = 1900
      TIME(7) = 1
      VERS = 1
C     initialize attribute arrays
      CALL INITAT (VERS,SPACE,TIME)
C     initialize missing value
      MISVAL = 0.0
      CALL INITMS (MISVAL)
C     set aide interaction to 'off'
      CALL INWIOX (I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WATINP
     I                    (MESSFL,WDMSFL,FE,INWAT,IVAL,RVAL,
     I                     CSEN,CLOC,CCON,ID,NAME,CSIT,CTYPE,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     input watstore files as wdm data sets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       WDMSFL,MESSFL,FE,INWAT,RETCOD,IVAL(6)
      REAL          RVAL(7)
      CHARACTER*2   CSIT
      CHARACTER*4   CTYPE
      CHARACTER*8   CLOC,CCON,CSEN,ID
      CHARACTER*48  NAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - wdm file unit number
C     MESSFL - message file unit number
C     FE     - error file unit number
C     INWAT  - watstore input file unit number
C     IVAL   - array of integer attribute values
C     RVAL   - array of real attribute values
C     CSIT   - site id attribute
C     CTYPE  - tstype attribute
C     CSEN   - scenario name
C     CLOC   - location name
C     CCON   - contituent name
C     ID     - station id
C     NAME   - station name
C     RETCOD - return code 1-already exists, 0-okay
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SAIND,SALEN,DSN,I1,TMPDSN,INITFG
      CHARACTER*1   OSTR1(48),BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDBSAC,CVARAR,TSDSM,TSESPC,TSDSGN
      EXTERNAL    INWDVB,ZIPC,WDBSAI,WDBSAR
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      DSN = 1
      INITFG = 1
      BLNK = ' '
C
C     see if we have one of these in wdm file
      CALL TSESPC (CSEN,CLOC,CCON)
      TMPDSN = 0
      CALL TSDSM (TMPDSN)
      IF (TMPDSN.GT.0) THEN
C       yes, found this data set, dont add another
C       need to advance in watstore file
C        CALL INWDVA (FE,INWAT,INITFG)
        RETCOD = 1
      ELSE
C       no, need to add it
C        write(*,*) 'calling inwdvb'
        BACKSPACE (INWAT)
        BACKSPACE (INWAT)
        BACKSPACE (INWAT)
        BACKSPACE (INWAT)
        BACKSPACE (INWAT)
        CALL INWDVB (MESSFL,WDMSFL,FE,INWAT,
     I               I1,INITFG,
     M               DSN)
C        write(*,*) 'returned from inwdvb'
C       put attribute for scenario name
        SAIND = 288
        SALEN = 8
        CALL CVARAR (SALEN,CSEN,SALEN,OSTR1)
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,OSTR1,
     O               RETCOD)
C       put next attribute for constituent name
        SAIND = 289
        SALEN = 8
        CALL CVARAR (SALEN,CCON,SALEN,OSTR1)
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,OSTR1,
     O               RETCOD)
C       put tstype
        SAIND = 1
        SALEN = 4
        CALL CVARAR (SALEN,CTYPE,SALEN,OSTR1)
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,OSTR1,
     O               RETCOD)
C       put next attribute for location name
        SAIND = 290
        SALEN = 8
        CALL CVARAR (SALEN,CLOC,SALEN,OSTR1)
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,OSTR1,
     O               RETCOD)
C       put next attribute for station id
        SAIND = 2
        SALEN = 8
        CALL CVARAR (SALEN,ID,SALEN,OSTR1)
        CALL ZIPC (SALEN,BLNK,OSTR1(9))
        SALEN = 16
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,OSTR1,
     O               RETCOD)
C       put next attribute for station name
        SAIND = 45
        SALEN = 48
        CALL CVARAR (SALEN,NAME,SALEN,OSTR1)
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,OSTR1,
     O               RETCOD)
C       put site code
        SAIND = 44
        SALEN = 2
        CALL CVARAR (SALEN,CSIT,SALEN,OSTR1)
        CALL ZIPC (SALEN,BLNK,OSTR1(3))
        SALEN = 4
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,OSTR1,
     O               RETCOD)
C       latdeg: latitude
        SALEN = 1
        SAIND = 8
        CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,RVAL(1),
     O               RETCOD)
C       lngdeg: longitude
        SAIND = 9
        CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,RVAL(2),
     O               RETCOD)
C       darea: total drainage area
        SAIND = 11
        CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,RVAL(3),
     O               RETCOD)
C       datum: datum elevation
        SAIND = 264
        CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,RVAL(5),
     O               RETCOD)
C       baseq: base discharge
        SAIND = 49
        CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,RVAL(7),
     O               RETCOD)
C       stfips: state code
        SAIND = 41
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,IVAL(1),
     O               RETCOD)
C       dscode: district code
        SAIND = 42
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,IVAL(2),
     O               RETCOD)
C       cocode: county code
        SAIND = 6
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,IVAL(3),
     O               RETCOD)
C       hucode: hydrologic unit code
        SAIND = 4
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,IVAL(4),
     O               RETCOD)
C       parmcd
        SAIND = 56
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,IVAL(5),
     O               RETCOD)
C       statcd
        SAIND = 57
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,IVAL(6),
     O               RETCOD)
C
C       let time series directory know this data set exists
        CALL TSDSGN (WDMSFL,DSN)
        RETCOD = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WATHED
     I                   (MESSFL,INWAT,
     O                    IVAL,RVAL,
     O                    CSITE,CTSTYP,CSTAID,CSTANM)
C
C     + + + PURPOSE + + +
C     get the next data set header from the watstore file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,INWAT,IVAL(6)
      REAL          RVAL(7)
      CHARACTER*2   CSITE
      CHARACTER*4   CTSTYP
      CHARACTER*8   CSTAID
      CHARACTER*48  CSTANM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     INWAT  - watstore input file unit number
C     IVAL   - integer array of attribute values
C     RVAL   - real array of attribute values
C     CSITE  - site code attribute
C     CTSTYP - tstype attribute
C     CSTAID - station id
C     CSTANM - station name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,PARM,STAT,IFLAG
      REAL          R,LTLG(3)
      CHARACTER*1   TSTYPE(4)
      CHARACTER*10  PARMST
      CHARACTER*80  CTXT80
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A80)
 1002 FORMAT (3F2.0)
 1003 FORMAT ( F3.0,2F2.0 )
 1004 FORMAT ( I2 )
 1006 FORMAT ( I3 )
 1007 FORMAT ( I8 )
 1008 FORMAT ( F7.0 )
 1010 FORMAT ( F8.0 )
 1011 FORMAT ( F9.0 )
 1012 FORMAT ( 2I5 )
C
C     + + + EXTERNAL + + +
      EXTERNAL   ZIPR,ZIPI,CVWTAT,CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
C     write(99,*) 'in wathed with inwat ',inwat
      CSITE  = ' '
      CSTAID = ' '
      CSTANM = ' '
      CTSTYP = ' '
      R = 0.0
      CALL ZIPR (7,R,RVAL)
      I = 0
      CALL ZIPI (6,I,IVAL)
      IFLAG = 0
C
 25   CONTINUE
C       read line of watstore file
        READ(INWAT,1000,END=30,ERR=30) CTXT80
C       write(99,*) 'read watstore line ',ctxt80
        IF (CTXT80(1:1).EQ.'H') THEN
C         this is a header line
          IF (CTXT80(17:22) .NE. '      ') THEN
C           latdeg: latitude
            READ (CTXT80(17:22),1002,ERR=201) LTLG
              RVAL(1) = LTLG(1) + LTLG(2)/60. + LTLG(3)/3600.
 201        CONTINUE
          END IF
          IF (CTXT80(23:29) .NE. '       ') THEN
C           lngdeg: longitude
            READ (CTXT80(23:29),1003,ERR=202) LTLG
              RVAL(2) = LTLG(1) + LTLG(2)/60. + LTLG(3)/3600.
 202        CONTINUE
          END IF
          IF (CTXT80(32:33) .NE. '  ') THEN
C           stfips: state code
            READ (CTXT80(32:33),1004,ERR=203) I
              IVAL(1) = I
 203        CONTINUE
          END IF
          IF (CTXT80(34:35) .NE. '  ') THEN
C           dscode: district code
            READ (CTXT80(34:35),1004,ERR=204) I
              IVAL(2) = I
 204        CONTINUE
          END IF
          IF (CTXT80(36:38) .NE. '   ') THEN
C           cocode: county code
            READ (CTXT80(36:38),1006,ERR=205) I
              IVAL(3) = I
 205        CONTINUE
          END IF
          IF (CTXT80(39:40) .NE. '  ') THEN
C           siteco: site code
            CSITE = CTXT80(39:40)
          END IF
          IF (CTXT80(41:48) .NE. '        ') THEN
C           hucode: hydrologic unit code
            READ (CTXT80(41:48),1007,ERR=206) I
              IVAL(4) = I
 206        CONTINUE
          END IF
          IF (CTXT80(49:55) .NE. '       ') THEN
C           darea: total drainage area
            READ (CTXT80(49:55),1008,ERR=207) R
              RVAL(3) = R
 207        CONTINUE
          END IF
          IF (CTXT80(56:62) .NE. '       ') THEN
C           contda: contributing drainage area
            READ (CTXT80(56:62),1008,ERR=208) R
              RVAL(4) = R
 208        CONTINUE
          END IF
          IF (CTXT80(63:70) .NE. '        ') THEN
C           datum: datum elevation
            READ (CTXT80(63:70),1010,ERR=209) R
              RVAL(5) = R
 209        CONTINUE
          END IF
          IF (CTXT80(71:79) .NE. '         ') THEN
C           welldp: total well depth
            READ (CTXT80(71:79),1011,ERR=210) R
              RVAL(6) = R
 210        CONTINUE
          END IF
        END IF
        IF (CTXT80(1:1) .EQ. 'Y') THEN
          IF (CTXT80(17:23) .NE. '       ') THEN
C           baseq: base discharge
            READ (CTXT80(17:23),1008,ERR=310) R
              RVAL(7) = R
 310        CONTINUE
          END IF
        END IF
        IF (CTXT80(1:1) .EQ. '2') THEN
          IF (CTXT80(29:38) .NE. '          ') THEN
C           parmcd, statcd
            PARMST = CTXT80(29:38)
            READ (PARMST,1012,ERR=108) PARM,STAT
              IVAL(5) = PARM
              IVAL(6) = STAT
              CALL CVWTAT (MESSFL,PARM,TSTYPE(1))
              CALL CARVAR (4,TSTYPE(1),4,CTSTYP)
 108        CONTINUE
          END IF
        END IF
        IF (CTXT80(1:1).EQ.'N') THEN
C         this is a name line
          CSTAID = CTXT80(3:10)
          CSTANM = CTXT80(17:64)
          IFLAG  = 1
        END IF
        IF (CTXT80(1:1).EQ.'3' .AND. IFLAG.EQ.1) THEN
C         need to get out of here
          GO TO 30
        END IF
      GO TO 25
 30   CONTINUE
C
      RETURN
      END
