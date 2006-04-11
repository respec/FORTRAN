C
C
C
      SUBROUTINE   PRWMSL
     I                    (MESSFL,WDMSFL,MAXDSN,
     M                     DSN,DSNCNT)
C
C     + + + PURPOSE + + +
C     work interactively with WDM file attributes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,MAXDSN,DSNCNT
      INTEGER   DSN(MAXDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     MAXDSN - maximum size of DSN array
C     DSN    - array of dataset numbers
C     DSNCNT - count of datasets to work with
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      OPT,SCLU,SGRP
      CHARACTER*8  PTHNAM
      CHARACTER*13 WNDNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, PRWMLM, PRWMLV, PRWMAD
      EXTERNAL     PRNTXT, PRWMSE
C
C     + + + END SPECIFICATIONS + + +
C
C     set message file group
      SCLU= 22
C
 50   CONTINUE
C       opt: 1-select,2-add,3-modify,4-delete,5-view,6-return
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,OPT)
C
        IF (OPT.GT.1 .AND. OPT.LE.4 .AND. DSNCNT.EQ.0) THEN
C         can't do option, no data sets selected
Cmyg      ** remove '+ 1' fr. line below when Add added as separate option
          SGRP = OPT + 1
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        ELSE
C         ok to do option
Cmyg      ** add in 200 to GOTO below when Add option made separate
          GO TO (100,300,400,500,600), OPT
C
 100      CONTINUE
C           select data sets
            PTHNAM = 'DA      '
            CALL PRWMSE (MESSFL,WDMSFL,MAXDSN,PTHNAM,
     M                   DSN,DSNCNT)
            GO TO 900
C
 200      CONTINUE
C           add attributes
Cmyg        ** needs to be added in--we want Add to be a separate menu
Cmyg           option and not be combined w/ Modify **
            GO TO 900
C
 300      CONTINUE
C           modify old attributes
            WNDNAM= 'Modify (DAM)'
            CALL PRWMLM (MESSFL,WDMSFL,SCLU,DSNCNT,DSN,WNDNAM)
            GO TO 900
C
 400      CONTINUE
C           delete attributes
            CALL PRWMAD (MESSFL,WDMSFL,SCLU,DSNCNT,DSN)
            GO TO 900
C
 500      CONTINUE
C           view dataset attributes
            CALL PRWMLV (MESSFL,WDMSFL,MAXDSN,DSNCNT,DSN)
            GO TO 900
C
 600      CONTINUE
C           return to Data
            GO TO 900
C
 900      CONTINUE
        END IF
C
      IF (OPT.NE.5) GO TO 50
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMLM
     I                    (MESSFL,WDMSFL,SCLU,DSNCNT,DSN,WNDNAM)
C
C     + + + PURPOSE + + +
C     Add or modify attributes on WDM file data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SCLU,DSNCNT
      INTEGER   DSN(DSNCNT)
      CHARACTER WNDNAM*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - cluster number on message file
C     DSNCNT - count of attributes to modify
C     DSN    - array of DSNCNT dataset numbers
C     WNDNAM - data screen window name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,L,SGRP,SAIND,SALEN,SATYP,SAREQ(12),SAUPFG,
     1            DSTYPE,ERRFLG,DPTR,SARQWD,DREC,DIND,DONFG,RETC,IRET
      CHARACTER*1 SANAM(6)
      CHARACTER*20 TMPNAM
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, WDDTFG
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WDRCUP, WDDTFG, WDSAGY, WDSAGN, ZGTRET
      EXTERNAL    PRNTXI, WDSAAM, WATTUS, PMXTXI, ZWNSET, ZMNSST, ZSTCMA
C
C     + + + END SPECIFICATIONS + + +
C
      I= 4
      J= 1
C     allow 'Prev' command
      CALL ZSTCMA (I,J)
      I= 16
C     allow 'Intrpt' command
      CALL ZSTCMA (I,J)
C
 10   CONTINUE
C       which attribute?
        CALL ZWNSET (WNDNAM)
        CALL WDSAGN (MESSFL,SAIND,SANAM,DONFG)
C       get user exit command value
        CALL ZGTRET (IRET)
        IF (DONFG.EQ.0 .AND. IRET.EQ.1) THEN
C         update the specified attribute
          CALL WDSAGY (MESSFL,SAIND,
     O                 SANAM,DPTR,SATYP,SALEN,SARQWD,SAUPFG)
          CALL WATTUS (SARQWD,
     O                 SAREQ)
          I= 1
 40       CONTINUE
C           get the attributes part of dataset
            CALL WDDSCK (WDMSFL,DSN(I),DREC,RETC)
            DIND  = WDRCGO(WDMSFL,DREC)
            DSTYPE= WIBUFF(6,DIND)
            IF (SAREQ(DSTYPE).EQ.0) THEN
C             attribute is not allowed for this dataset
              L     = LEN(WNDNAM)
              TMPNAM= WNDNAM(1:L)//' Problem'
              CALL ZWNSET (TMPNAM)
              SGRP= 10
              CALL PRNTXI (MESSFL,SCLU,SGRP,DSN(I))
            ELSE IF (SAUPFG.EQ.1.AND.WDDTFG(DREC,WIBUFF(1,DIND)).EQ.1)
     1        THEN
C             cant update this attribute if data is present
              L     = LEN(WNDNAM)
              TMPNAM= WNDNAM(1:L)//' Problem'
              CALL ZWNSET (TMPNAM)
              SGRP= 11
              CALL PRNTXI (MESSFL,SCLU,SGRP,DSN(I))
            ELSE
C             attribute is allowed
              CALL ZWNSET (WNDNAM)
              SGRP= 12
              J   = 1
              CALL PMXTXI (MESSFL,SCLU,SGRP,J,J,-J,J,DSN(I))
C             save message
              CALL ZMNSST
              CALL WDSAAM (MESSFL,SAIND,SANAM,SATYP,SALEN,DPTR,WNDNAM,
     M                     WIBUFF(1,DIND),WRBUFF(1,DIND),
     O                     ERRFLG)
C             write modified attributes
              CALL WDRCUP (WDMSFL,DIND)
            END IF
C           get user exit command values
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user wants next data set
              I= I+ 1
            ELSE IF (IRET.EQ.2) THEN
C             user wants previous data set
              I= I- 1
            ELSE IF (IRET.EQ.7) THEN
C             user wants to interrupt modifications
              I= DSNCNT+ 1
            END IF
          IF (I.GT.0 .AND. I.LE.DSNCNT) GO TO 40
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      I= 4
      J= 0
C     turn off 'Prev' command
      CALL ZSTCMA (I,J)
      I= 16
C     turn off 'Intrpt' command
      CALL ZSTCMA (I,J)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMAD
     I                    (MESSFL,WDMSFL,SCLU,DSNCNT,DSN)
C
C     + + + PURPOSE + + +
C     routine to delete attributes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SCLU,DSNCNT
      INTEGER   DSN(DSNCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - Cluster containing messages for this routine
C     DSNCNT - count of attributes to modify
C     DSN    - array of DSNCNT dataset numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,SGRP,SAIND,SATYP,SALEN,SAUPFG,SARQWD,
     1            RETCOD,DONFG,ONUM,OTYP(2),CLEN,INIT
      REAL        RVAL
      DOUBLE PRECISION DVAL
      CHARACTER*1 SANAM(6),BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDSAGN, WDSAGY, WDBSAD, ZBLDWR, PMXTXM
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      BLNK= ' '
      CLEN= 6
      ONUM= 2
      OTYP(1)= 4
      OTYP(2)= 1
C
 10   CONTINUE
C       specify attribute to delete
        CALL WDSAGN (MESSFL,
     O               SAIND,SANAM,DONFG)
C
        IF (DONFG.EQ.0) THEN
C         attribute specified, get info about it
          CALL WDSAGY (MESSFL,SAIND,
     O                 SANAM,I,SATYP,SALEN,SARQWD,SAUPFG)
          INIT= 1
          DO 20 I= 1,DSNCNT
C           try to delete specified attribute for each data set in buffer
            CALL WDBSAD (WDMSFL,DSN(I),SAIND,SAUPFG,SARQWD,SALEN,
     O                   RETCOD)
            IF (RETCOD.EQ.0) THEN
C             successful delete
              SGRP= 14
            ELSE IF (RETCOD.EQ.-81) THEN
C             dataset doesn't exist
              SGRP= 15
            ELSE IF (RETCOD.EQ.-107) THEN
C             attribute not present
              SGRP= 16
            ELSE IF (RETCOD.EQ.-104) THEN
C             data present, cant delete
              SGRP= 17
            ELSE IF (RETCOD.EQ.-106) THEN
C             attribute required, cant delete
              SGRP= 18
            END IF
            CALL PMXTXM (MESSFL,SCLU,SGRP,I1,INIT,ONUM,OTYP,I1,
     I                   DSN(I),RVAL,DVAL,CLEN,SANAM)
            INIT= -1
 20       CONTINUE
C         hold screen
          CALL ZBLDWR (I0,BLNK,I0,I0,I)
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSAAM
     I                    (MESSFL,SAIND,SANAM,SATYP,SALEN,DPTR,WNDNAM,
     M                     TIBUFF,TRBUFF,
     O                     ERRFLG)
C
C     + + + PURPOSE + + +
C     adds an individual search attribute to a dataset
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SAIND,SATYP,SALEN,ERRFLG,DPTR
      INTEGER*4   TIBUFF(512)
      REAL        TRBUFF(512)
      CHARACTER*1 SANAM(6)
      CHARACTER   WNDNAM*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SAIND  - index number of search attribute
C     SANAM  - name of search attribute
C     SATYP  - type of search attribute
C     SALEN  - length of search attribute
C     DPTR   - pointer to attribute info on message file
C     WNDNAM - data screen window name
C     TIBUFF - buffer containing integer version of dsn label
C     TRBUFF - buffer containing real version of dsn label
C     ERRFLG - error return code, 1 if out of room
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + PARAMETERS + + +
      INTEGER     MLEN
      PARAMETER  (MLEN=300)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,I6,I10,I78,J,K,L,RETCOD,PSAVAL,IRET,LSALEN,
     1            TLEN,CLEN,DREC,DPOS
      DOUBLE PRECISION DVAL
      CHARACTER*1 CENTER(6),CNONE(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     WMSPIV, WDPTCL, CHRINT
      REAL        CHRDEC
      DOUBLE PRECISION CHRDPR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSPIV, WDPTCL, CHRINT, CHRDEC, CHRDPR, ZIPC, ZSCINI
      EXTERNAL    WADGRA, WADGDF, WADGDS, WADGVA, WADGHL, WDSASP
      EXTERNAL    INTCHR, DECCHR, DPRCHR, CHRCHR, CHRDEL, ZSTCMA, ZEDT0M
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I6,I10,I78/0,1,6,10,78/
      DATA CENTER,CNONE/'E','n','t','e','r',' ','n','o','n','e'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      ERRFLG= 0
C     get space for new attribute
      CALL WDSASP (SAIND,SALEN,SATYP,
     M             TIBUFF,
     O             PSAVAL,RETCOD)
C
      IF (PSAVAL.GT.0 .AND. RETCOD.NE.-103) THEN
C       fill in necessary screen parameters
C       init valid response string
        I= 960
        CALL ZIPC (I,BLNK,RSPSTR)
C       init screen
        CALL ZSCINI
C       init help to off
        CALL ZSTCMA (I1,I0)
        GPTR= 0
C       turn on Oops
        CALL ZSTCMA (8,I1)
C       turn on limits
        CALL ZSTCMA (6,I1)
C       number of field
        NFLDS= 1
C       set window name
        ZSCNAM= WNDNAM
C       build prompt for this attribute
C       put 'Enter' buffer in prompt
        CALL CHRCHR (I6,CENTER,ZMNTX1(1,3))
        ZMNTX1(13,3)= ','
C       put attribute name in prompt
        CALL CHRCHR (I6,SANAM,ZMNTX1(7,3))
C       get description
        CALL WADGDS (MESSFL,DPTR,
     O               ZMNTX1(15,3))
C       make the question look pretty
        DO 10 J= 12,8,-1
          IF (ZMNTX1(J,3).EQ.BLNK) THEN
            CALL CHRDEL (I78,J,ZMNTX1(1,3))
          END IF
 10     CONTINUE
C       length of string
        ZMNLEN(3)= 78
C       assign type
        IF (SATYP.EQ.1) THEN
          FTYP(1)= 'I'
        ELSE IF (SATYP.EQ.2) THEN
          FTYP(1)= 'R'
        ELSE IF (SATYP.EQ.3) THEN
          FTYP(1)= 'C'
        ELSE IF (SATYP.EQ.4) THEN
          FTYP(1)= 'D'
        END IF
        APOS(1)= 1
C       get help if available
        CALL WADGHL (MESSFL,DPTR,
     O               TLEN,DREC,DPOS)
        IF (TLEN.GT.0) THEN
C         save pointer to help
          HPTR(1)= WDPTCL(DREC,DPOS)
        ELSE
          HPTR(1)= 0
        END IF
C       assign starting line and column
        FLIN(1)= 4
        SCOL(1)= 1
C       set number of lines for screen
        ZMNNLI= 4
        IF (SATYP.EQ.3) THEN
C         character, only one value to get
          LSALEN= 1
        ELSE
          LSALEN= SALEN
        END IF
        DO 30 I= 1,LSALEN
C         get required number of values
 20       CONTINUE
C           back to here on an 'Oops'
            IF (SATYP.LE.2 .OR. SATYP.EQ.4) THEN
C             numeric field
              FLEN(1)= 10
C             get min, max, default
              CALL WADGRA (MESSFL,DPTR,SATYP,
     O                     RMIN(1),RMAX(1))
              CALL WADGDF (MESSFL,DPTR,SATYP,
     O                     RDEF(1))
              IF (SATYP.EQ.1) THEN
                IMIN(1)= RMIN(1)
                IMAX(1)= RMAX(1)
                IDEF(1)= RDEF(1)
              ELSE IF (SATYP.EQ.4) THEN
                DMIN(1)= RMIN(1)
                DMAX(1)= RMAX(1)
                DDEF(1)= RDEF(1)
              END IF
              IF (RETCOD.EQ.0) THEN
C               adding new value, put 'none' in field to start
                J= 4
                CALL CHRCHR (J,CNONE,ZMNTX1(1,4))
              ELSE
C               put existing value in field
                J= PSAVAL+ I- 1
                IF (SATYP.EQ.1) THEN
                  CALL INTCHR (TIBUFF(J),FLEN(1),I1,
     O                         K,ZMNTX1(1,4))
                ELSE IF (SATYP.EQ.2) THEN
                  CALL DECCHR (TRBUFF(J),FLEN(1),I1,
     O                         K,ZMNTX1(1,4))
                ELSE IF (SATYP.EQ.4) THEN
                  DVAL= TRBUFF(J)
                  CALL DPRCHR (DVAL,FLEN(1),I1,
     O                         K,ZMNTX1(1,4))
                END IF
              END IF
            ELSE
C             character field
              IF (SALEN.LE.76) THEN
C               use full attribute length as field length
                FLEN(1)= SALEN
              ELSE
C               attribute won't fit in field, need to limit field length
                FLEN(1)= 76
              END IF
C             get valid responses
              CALL WADGVA (MESSFL,DPTR,MLEN,
     O                     CLEN,RSPSTR)
              FDVAL(1)= WMSPIV(I1,CLEN)
              CCNT(1) = CLEN/FLEN(1)
              IF (RETCOD.EQ.0) THEN
C               adding new value, blank out field to start
                CALL ZIPC (FLEN(1),BLNK,ZMNTX1(1,4))
              ELSE
C               put current value in screen text
                J= PSAVAL+ I- 1
                L= 0
                DO 22 K= 1,FLEN(1),4
                  L= L+ 1
                  WRITE (ZMNTXT(4)(K:K+3),1000) TIBUFF(J+L-1)
 22             CONTINUE
              END IF
            END IF
C           set some other screen parms
            CFLD  = 1
            QFLAG = 0
            ZHLLIN= FLIN(CFLD)
            ZHLCOL= SCOL(CFLD)
            ZHLLEN= FLEN(CFLD)
            ZCRLIN= ZHLLIN+ 1
            ZCRCOL= ZHLCOL+ 1
C           always turn off parameter init flag
            SPINIT= 0
C           and screen name flag
            ZWNFLG= 0
C
C           now get attribute value
            CALL ZEDT0M (I0,
     O                   IRET)
          IF (IRET.EQ.-1) GO TO 20
C
          IF (IRET.EQ.1) THEN
C           user exited w/'Next' command, put value onto WDM record
            J= PSAVAL+ I- 1
            IF (SATYP.EQ.1) THEN
              TIBUFF(J)= CHRINT(I10,ZMNTX1(1,4))
            ELSE IF (SATYP.EQ.2) THEN
              TRBUFF(J)= CHRDEC(I10,ZMNTX1(1,4))
            ELSE IF (SATYP.EQ.3) THEN
              L= 0
              DO 25 K= 1,FLEN(1),4
                L= L+ 1
                READ (ZMNTXT(4)(K:K+3),1000) TIBUFF(J+L-1)
 25           CONTINUE
            ELSE IF (SATYP.EQ.4) THEN
              DVAL= CHRDPR(I10,ZMNTX1(1,4))
              TRBUFF(J)= DVAL
            END IF
          END IF
 30     CONTINUE
C
      ELSE
C       error, out of room
        ERRFLG= 1
      END IF
C
      RETURN
      END
