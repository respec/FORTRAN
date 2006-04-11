C
C
C
      SUBROUTINE   WDBOUN
     O                   (WDMPFL,RETCOD,FNAME,NUMHED,HEADNM,
     O                    SCTRID,FLGCHR)
C
C     + + + PURPOSE + + +
C     open boundary WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMPFL,RETCOD,NUMHED,SCTRID
      CHARACTER*1  FLGCHR
      CHARACTER*64 FNAME
      CHARACTER*78 HEADNM(3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMPFL - unit number of boundary WDM file (0:no boundaries)
C     RETCOD - return code from open of boundary WDM file
C     FNAME  - name of status file
C     NUMHED - number of header lines
C     HEADNM - header lines
C     SCTRID - scenario translator id
C     FLGCHR - character flag to send for special situations
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,DSNTYP,DSN,DSNCOU,DSNACU,DSNALL,SAIND,LAMRET,
     1             DSNMAJ,MAPCOV,INITFG,CONT,OLEN
      REAL         LAM0(1)
      CHARACTER*1  TXT(78)
      CHARACTER*64 WDNAME
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (TXT,TXTL)
      CHARACTER*78 TXTL
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + FUNCTIONS + + +
      INTEGER      WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETFUN, WDBOPN, WDCKDT, MAPCOM, WDBSGR, WMSGTT
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (A64)
1010  FORMAT (F10.0)
1020  FORMAT (I1)
1025  FORMAT (I1,4X,A1)
1030  FORMAT (A78)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C     lam0 attribute
      SAIND  = 97
C     no default boundary datasets
      DSNCOU = 0
      DSNACU = 0
      DSNALL = 0
      DSNMAJ = 0
C     boundary wdm file unit number
      CALL GETFUN(I1,WDMPFL)
      WRITE(99,*) 'just got unit number for WDM file:',WDMPFL
      OPEN(UNIT=WDMPFL,FILE='BOUNPATH',STATUS='OLD',ERR=5)
      WRITE(99,*) 'opened file BOUNPATH'
      READ(WDMPFL,1000,ERR=5) WDNAME
      WRITE(99,*) 'looking for boundary file:',WDNAME
      CLOSE(UNIT=WDMPFL,ERR=9)
 9    CONTINUE
C     open boundary wdm file
      CALL WDBOPN(WDMPFL,WDNAME,I1,
     O            RETCOD)
C
      IF (RETCOD .EQ. 0) THEN
C       generic info, sta file name
        DSN   = 1
        INITFG= 1
        OLEN  = 78
        CALL WMSGTT (WDMPFL,DSN,I1,INITFG,
     M               OLEN,
     O               TXT,CONT)
        READ(TXTL,1000) FNAME
C       base long
        INITFG = 0
        OLEN  = 78
        CALL WMSGTT (WDMPFL,DSN,I1,INITFG,
     M               OLEN,
     O               TXT,CONT)
        READ(TXTL,1010) LAM0(1)
C       number of header lines
        OLEN  = 78
        CALL WMSGTT (WDMPFL,DSN,I1,INITFG,
     M               OLEN,
     O               TXT,CONT)
        READ(TXTL,1025) NUMHED,FLGCHR
        DO 10 I = 1,NUMHED
          OLEN  = 78
          CALL WMSGTT (WDMPFL,DSN,I1,INITFG,
     M                 OLEN,
     O                 TXT,CONT)
          READ(TXTL,1030) HEADNM(I)
 10     CONTINUE
C       scenario translator id number
        OLEN  = 78
        CALL WMSGTT (WDMPFL,DSN,I1,INITFG,
     M               OLEN,
     O               TXT,CONT)
        READ(TXTL,1020) SCTRID
C
C       states?
        DSN   = 100
        DSNTYP= WDCKDT(WDMPFL,DSN)
        IF (DSNTYP .EQ. 5) THEN
C         available
          WRITE(99,*) 'coverage available: STATE'
          IF (ABS(LAM0(1)+999.) .LT. 1.0E-2) THEN
C           lam0 needed
            CALL WDBSGR (WDMPFL,DSN,SAIND,I1,
     O                   LAM0,LAMRET)
          END IF
        END IF
C       counties?
        MAPCOV= 0
 15     CONTINUE
          DSN   = DSN+ 1
          DSNTYP= WDCKDT(WDMPFL,DSN)
          IF (DSNTYP .EQ. 5) THEN
C           available
            MAPCOV= 1
            DSNCOU= DSN
            WRITE(99,*) 'coverage available: COUNTY for state',DSN-100
            IF (ABS(LAM0(1)+999.) .LT. 1.0E-2) THEN
C             lam0 needed
              CALL WDBSGR (WDMPFL,DSN,SAIND,I1,
     O                     LAM0,LAMRET)
            END IF
          END IF
        IF (MAPCOV.EQ.0 .AND. DSN.LT.156) GO TO 15
C       hydro region?
        DSN   = 300
        DSNTYP= WDCKDT(WDMPFL,DSN)
        IF (DSNTYP .EQ. 5) THEN
C         available
          WRITE(99,*) 'coverage available: HYDRO REGION'
          IF (ABS(LAM0(1)+999.) .LT. 1.0E-2) THEN
C           lam0 needed
            CALL WDBSGR (WDMPFL,DSN,SAIND,I1,
     O                   LAM0,LAMRET)
          END IF
        END IF
C       hydro acct units?
        MAPCOV= 0
 20     CONTINUE
          DSN   = DSN+ 1
          DSNTYP= WDCKDT(WDMPFL,DSN)
          IF (DSNTYP .EQ. 5) THEN
C           available
            MAPCOV= 1
            DSNACU= DSN
            WRITE(99,*) 'coverage available: HYDRO ACCT for reg',DSN-300
            IF (ABS(LAM0(1)+999.) .LT. 1.0E-2) THEN
C             lam0 needed
              CALL WDBSGR (WDMPFL,DSN,SAIND,I1,
     O                     LAM0,LAMRET)
            END IF
          END IF
        IF (MAPCOV.EQ.0 .AND. DSN.LT.318) GO TO 20
C       hydrography?
        MAPCOV= 0
        DSN = 200
 30     CONTINUE
          DSN   = DSN+ 1
          DSNTYP= WDCKDT(WDMPFL,DSN)
          IF (DSNTYP .EQ. 5) THEN
C           available
            MAPCOV= 1
            DSNALL= DSN
            WRITE(99,*) 'cvrage avail: all HYDROGRAPH for reg',DSN-200
            IF (ABS(LAM0(1)+999.) .LT. 1.0E-2) THEN
C             lam0 needed
              CALL WDBSGR (WDMPFL,DSN,SAIND,I1,
     O                     LAM0,LAMRET)
            END IF
            DSN   = DSN+ 50
            DSNTYP= WDCKDT(WDMPFL,DSN)
            IF (DSNTYP .EQ. 5) THEN
C             available
              MAPCOV= 1
              DSNMAJ= DSN
              WRITE(99,*) 'cvrage avail: maj HYDROGRAPH for reg',DSN-200
            END IF
          END IF
        IF (MAPCOV.EQ.0 .AND. DSN.LT.218) GO TO 30
C       mlra
        MAPCOV= 0
        DSN   = 400
        DSNTYP= WDCKDT(WDMPFL,DSN)
        IF (DSNTYP .EQ. 5) THEN
C         available
          WRITE(99,*) 'coverage available: MLRA'
          IF (ABS(LAM0(1)+999.) .LT. 1.0E-2) THEN
C           lam0 needed
            CALL WDBSGR (WDMPFL,DSN,SAIND,I1,
     O                   LAM0,LAMRET)
          END IF
        END IF
      END IF
C
      IF (ABS(LAM0(1)+999.) .LT. 1.0E-2) THEN
C       lam0 needed, not an attribute
        LAM0(1)= -96
      END IF
      WRITE(99,*) 'LAM0:',LAM0(1)
C     call routine to set map info in map library common block
      CALL MAPCOM(LAM0(1),DSNCOU,DSNACU,DSNALL,DSNMAJ)
      GO TO 8
C
 5    CONTINUE
C     set retcod for no bounpath
      RETCOD = 1
 8    CONTINUE
C
      RETURN
      END
