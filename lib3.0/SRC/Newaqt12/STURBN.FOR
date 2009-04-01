C
C
C
      SUBROUTINE   SSMODI
     I                   (MESSFL,LSENNM)
C
C     + + + PURPOSE + + +
C     modify an existing scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL
      CHARACTER*8   LSENNM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     LSENNM - scenario name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,RESP,CNUM,CLEN(3),TLEN,INUM,IVAL(48),ILEN,
     1              IRET,NRCH,I,I0,I1,I8,IVAL3(12,48),I20,I4,
     2              CVAL2(8,3,48),RNUM,CVAL5(2,3,48),SCLU,MXRCH,
     3              DATES(6),I78,IM1,IVAL4(1,48),PCPDSN(5),EVPDSN(5),
     5              METSEG(48,3),NUMNET,RCHNET(336,2),K,J,IPOS,NLAND,
     6              PERFST,PERLST,NOIN(12),ANAWDM(48),NIMP,I3,I5,
     7              IMPLID(48),RCHRID(48),NPER,PERLID(48),RCHTMP(48)
      REAL          R0,RVAL(13),RCHLEN(48),RCHINI(48),IMPACR(20,48),
     1              LANDAC(50,48),RVALN(12,48),RVALSV(12,48),RFACT,
     2              RCHSLO(48)
      CHARACTER*1   BLNK,CTXT(128),TBUFF(80,48),CHEAD1(78),LANDU1(20),
     1              CTEMP(5),STRIN1(8*48)
      CHARACTER*8   RCHNAM(48),TMPNAM,LCON,PCPNAM(5),EVPNAM(5),GLOID
      CHARACTER*20  LANDUS(48)
      CHARACTER*64  LSENDS
      CHARACTER*78  CHEAD,UCINFO
C
C     + + + INTRINSICS + + +
      INTRINSIC     ABS
C
C     + + + FUNCTIONS + + +
      REAL          CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL      QRESP,Q1INIT,ZIPC,QSETCT,QSETI,Q1EDIT,GSTGLV
      EXTERNAL      ZGTRET,QRESCX,CARVAR,CVARAR,COPYR,PMXCNW
      EXTERNAL      QGETCT,QGETI,ZSTCMA,ZIPR,PUTHSP,DECCHX,CHRDEC
      EXTERNAL      PRNTXT,ZIPI,ZBLDWR,ZMNSST,GETHSP,GETMET
      EXTERNAL      SSMDEL,TSESPC,TSDSMA,TSDSMD,SSMADD,SSMCHR
C
C     + + + END SPECIFICATIONS + + +
C
      IM1  = -1
      I1   = 1
      I0   = 0
      I3   = 3
      I4   = 4
      I5   = 5
      I8   = 8
      I20  = 20
      I78  = 78
      R0   = 0.0
      BLNK = ' '
      SCLU = 62
      MXRCH= 48
C
C     let user know that processing is going on
      SGRP = 60
      CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,I)
C
C     get all values from hspf data structure
      CALL GETHSP (MXRCH,
     O             DATES,NRCH,RCHNAM,UCINFO,RCHLEN,METSEG,NUMNET,
     O             RCHNET,LANDAC,LANDUS,ANAWDM,RCHINI,IMPACR,NIMP,
     O             IMPLID,RCHRID,NPER,PERLID,RCHSLO)
      LSENDS = UCINFO
      DO 2 I = 1,48
C       set temp array of reach wdm data set numbers
        RCHTMP(I) = ANAWDM(I)
 2    CONTINUE
C     get evap and precip data set numbers and names
      CALL GETMET (
     O             PCPNAM,EVPNAM,PCPDSN,EVPDSN)
C     set global variable for valid reaches
      GLOID = 'RCHNAM  '
      ILEN  = 8*NRCH
      DO 3 I = 1,NRCH
        IPOS  = ((I-1)*8)+1
        CALL CVARAR (I8,RCHNAM(I),I8,STRIN1(IPOS))
        IVAL(I) = 0
 3    CONTINUE
      CALL GSTGLV (GLOID,I8,ILEN,STRIN1,NRCH,IVAL)
C
 5    CONTINUE
C       modify options loop
        SGRP= 40
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        GO TO (10,20,30,40,50,60,70,80,90),RESP
C
 10     CONTINUE
C         modify general specs
          SGRP = 41
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         allow previous
          I = 4
          CALL ZSTCMA (I,I1)
C
C         fill in char fields for sen name and description
          CNUM = 2
          CLEN(1) = 8
          CLEN(2) = 60
          TLEN = 68
          CALL CVARAR (CLEN(1),LSENNM,CLEN(1),CTXT)
          CALL CVARAR (CLEN(2),LSENDS,CLEN(2),CTXT(9))
          CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
C
C         fill in dates from default
          INUM = 6
          CALL QSETI (INUM,DATES)
C
          CALL Q1EDIT (IRET)
          IF (IRET.NE.2) THEN
C           user wants to continue
            CALL QGETCT (CNUM,CLEN,TLEN,CTXT)
            CALL CARVAR (CLEN(2),CTXT(9),CLEN(2),LSENDS)
            CALL QGETI (INUM,DATES)
          END IF
C         turn previous off
          I = 4
          CALL ZSTCMA (I,I0)
          GO TO 100
C
 20     CONTINUE
C         add a reach
          IF (NRCH.LT.MXRCH) THEN
C           room to add another
            CALL SSMADD (MESSFL,SCLU,MXRCH,NIMP,LANDUS,
     M                   RCHTMP,PCPNAM,EVPNAM,PCPDSN,EVPDSN,
     M                   METSEG,RCHLEN,RCHINI,LANDAC,
     M                   IMPACR,RCHNET,NUMNET,NRCH,RCHNAM,RCHRID,RCHSLO)
C           set global variable for valid reaches
            GLOID = 'RCHNAM  '
            ILEN  = 8*NRCH
            DO 25 I = 1,NRCH
              IPOS  = ((I-1)*8)+1
              CALL CVARAR (I8,RCHNAM(I),I8,STRIN1(IPOS))
              IVAL(I) = 0
 25         CONTINUE
            CALL GSTGLV (GLOID,I8,ILEN,STRIN1,NRCH,IVAL)
          ELSE
C           tell user no more room left in reach array
            SGRP = 63
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 30     CONTINUE
C         delete a reach, set valid responses
          IF (NRCH.GT.0) THEN
C           there are some valid reaches, okay to continue
            CALL SSMDEL (MESSFL,SCLU,MXRCH,
     M                   RCHTMP,ANAWDM,METSEG,RCHLEN,RCHINI,LANDAC,
     M                   IMPACR,RCHNET,NUMNET,NRCH,RCHNAM,RCHRID,RCHSLO)
C           set global variable for valid reaches
            GLOID = 'RCHNAM  '
            ILEN  = 8*NRCH
            DO 35 I = 1,NRCH
              IPOS  = ((I-1)*8)+1
              CALL CVARAR (I8,RCHNAM(I),I8,STRIN1(IPOS))
              IVAL(I) = 0
 35         CONTINUE
            CALL GSTGLV (GLOID,I8,ILEN,STRIN1,NRCH,IVAL)
          ELSE
C           tell user no reaches available
            SGRP = 64
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 40     CONTINUE
C         modify characteristics of a reach
          IF (NRCH.GT.0) THEN
C           there are some valid reaches, okay to continue
            CALL SSMCHR (MESSFL,SCLU,MXRCH,NIMP,LANDUS,
     M                   RCHTMP,PCPNAM,EVPNAM,PCPDSN,EVPDSN,
     M                   METSEG,RCHLEN,RCHINI,LANDAC,
     M                   IMPACR,RCHNET,NUMNET,NRCH,RCHNAM,RCHSLO)
          ELSE
C           tell user no reaches available
            SGRP = 65
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 50     CONTINUE
C         modify land surface table
          IF (NRCH.GT.0) THEN
C           there are some valid reaches, okay to continue
            SGRP = 45
C           allow previous
            I = 4
            CALL ZSTCMA (I,I1)
C           fill in data values
            INUM = 1
            RNUM = 12
            CNUM = 1
            I = 12*MXRCH
            CALL ZIPR (I,R0,RVALN)
            CALL ZIPI (I,I0,IVAL3)
            CVAL2(1,1,1) = 0
            I = 80*MXRCH
            CALL ZIPC (I,BLNK,TBUFF)
C           build header string and put on screen
            CHEAD = '                     Acres of each land use'//
     1              ' type (up to 12)                   '
            CALL CVARAR (I78,CHEAD,I78,CHEAD1)
            CALL ZBLDWR (I78,CHEAD1,I1,IM1,I)
C           figure out how many land use types and build header
            CALL ZIPC (I78,BLNK,CHEAD1)
            CHEAD1(3) = 'R'
            CHEAD1(4) = 'e'
            CHEAD1(5) = 'a'
            CHEAD1(6) = 'c'
            CHEAD1(7) = 'h'
            IPOS = 9
            NLAND= 0
            I = 12
            CALL ZIPI (I,I0,NOIN)
            DO 51 I = 1,48
C             loop through each perlnd for land use types
              IF (LANDUS(I).NE.'                    ') THEN
C               name entered
                IF (I.GT.1) THEN
C                 see if name same as previous name
                  IF (LANDUS(I).NE.LANDUS(I-1)) THEN
C                   need to put this name in header
                    CALL CVARAR (I20,LANDUS(I),I20,LANDU1)
                    IPOS = IPOS + 2
                    CHEAD1(IPOS) = LANDU1(1)
                    IPOS = IPOS + 1
                    CHEAD1(IPOS) = LANDU1(2)
                    IPOS = IPOS + 1
                    CHEAD1(IPOS) = LANDU1(3)
                    IPOS = IPOS + 1
                    CHEAD1(IPOS) = LANDU1(4)
C                   put data for this perlnd/reach into screen
                    NLAND = NLAND + 1
                    DO 52 J = 1,NRCH
                      RVALN(NLAND,J) = LANDAC(I,J)
 52                 CONTINUE
C                   keep track of how many perlnds in each land use class
                    NOIN(IPOS-13-((NLAND-1)*4)) = 1
                  ELSE
C                   already have name in header,
C                   put data for this perlnd/reach into screen
                    DO 53 J = 1,NRCH
                      RVALN(NLAND,J) = RVALN(NLAND,J) + LANDAC(I,J)
 53                 CONTINUE
C                   increment count of number of perlnds in this class
                    NOIN(IPOS-13-((NLAND-1)*4)) =
     1                    NOIN(IPOS-13-((NLAND-1)*4))+1
                  END IF
                ELSE
C                 put name in header
                  CALL CVARAR (I20,LANDUS(I),I20,LANDU1)
                  IPOS = IPOS + 2
                  CHEAD1(IPOS) = LANDU1(1)
                  IPOS = IPOS + 1
                  CHEAD1(IPOS) = LANDU1(2)
                  IPOS = IPOS + 1
                  CHEAD1(IPOS) = LANDU1(3)
                  IPOS = IPOS + 1
                  CHEAD1(IPOS) = LANDU1(4)
C                 put data for this perlnd/reach into screen
                  NLAND = NLAND + 1
                  DO 54 J = 1,NRCH
                    RVALN(NLAND,J) = LANDAC(I,J)
 54               CONTINUE
                  NOIN(1) = 1
                END IF
              END IF
 51         CONTINUE
C           put implnd name into header
            IPOS = IPOS + 2
            CHEAD1(IPOS) = 'I'
            IPOS = IPOS + 1
            CHEAD1(IPOS) = 'M'
            IPOS = IPOS + 1
            CHEAD1(IPOS) = 'P'
            IPOS = IPOS + 1
            CHEAD1(IPOS) = ' '
C           put implnd data into buffer
            DO 541 I = 1,20
              DO 540 J = 1,NRCH
                RVALN(NLAND+1,J) = IMPACR(I,J) + RVALN(NLAND+1,J)
 540          CONTINUE
 541        CONTINUE
            CALL ZBLDWR (I78,CHEAD1,I0,IM1,I)
            CALL ZMNSST
            DO 55 I = 1, NRCH
C             fill in reaches in tbuff
              CALL CVARAR (I8,RCHNAM(I),I8,TBUFF(1,I))
 55         CONTINUE
C           need to make sure number fits in field
            DO 551 I = 1,MXRCH
              DO 552 J = 1,12
                IF (RVALN(J,I).GT.999.0) THEN
C                 convert to char string
                  CALL DECCHX (RVALN(J,I),I5,I3,I0,CTEMP)
C                 convert back to real
                  RVALN(J,I) = CHRDEC (I4,CTEMP)
                END IF
 552          CONTINUE
 551        CONTINUE
C           save values in table for later checking
            I = 12*MXRCH
            CALL COPYR (I,RVALN,RVALSV)
C           do land surface table
            CALL QRESCX (MESSFL,SCLU,SGRP,INUM,RNUM,CNUM,
     I                   NRCH,I1,
     M                   IVAL3,RVALN,CVAL2,TBUFF)
            CALL ZGTRET (IRET)
            IF (IRET.NE.2) THEN
C             user wants to continue
C             look through table values to see if any have changed
              DO 56 I = 1,NRCH
                PERFST = 1
                DO 57 J = 1,NLAND
                  IF ((ABS(RVALN(J,I)-RVALSV(J,I)).GT.0.999 .AND.
     1                RVALN(J,I).GT.99.999) .OR.
     2                (ABS(RVALN(J,I)-RVALSV(J,I)).GT.0.0999 .AND.
     3                RVALN(J,I).LT.99.999)) THEN
C                   value has changed
C                   figure out which landac values reflect this one
                    PERLST = PERFST+NOIN(J)-1
C                   sum of landac(i,perfst)..(i,perlst) must equal table val
                    IF (ABS(RVALSV(J,I)).GT.1.0E-5) THEN
C                     not a zero value to start
                      RFACT = RVALN(J,I)/RVALSV(J,I)
C                     scale values of landac accordingly
                      DO 58 K= PERFST,PERLST
                        LANDAC(K,I) = LANDAC(K,I)*RFACT
 58                   CONTINUE
                    ELSE
C                     came in with a zero,
C                     factor acreage equally over all perlnds for this lu
                      DO 59 K= PERFST,PERLST
                        LANDAC(K,I) = RVALN(J,I)/(PERLST-PERFST+1)
 59                   CONTINUE
                    END IF
                  END IF
C                 keep track of which perlnd for next time through
                  PERFST = PERFST+NOIN(J)
 57             CONTINUE
 56           CONTINUE
C             check for changes in impervious areas
              DO 560 I = 1,NRCH
                J = NLAND+1
                IF ((ABS(RVALN(J,I)-RVALSV(J,I)).GT.0.999 .AND.
     1              RVALN(J,I).GT.99.999) .OR.
     2              (ABS(RVALN(J,I)-RVALSV(J,I)).GT.0.0999 .AND.
     3              RVALN(J,I).LT.99.999)) THEN
C                 value has changed
C                 figure out which impacr values reflect this one
C                 sum of impacr(i, ) must equal table val
                  IF (ABS(RVALSV(J,I)).GT.1.0E-5) THEN
C                   not a zero value to start
                    RFACT = RVALN(J,I)/RVALSV(J,I)
C                   scale values of impacr accordingly
                    DO 580 K= 1,NIMP
                      IMPACR(K,I) = IMPACR(K,I)*RFACT
 580                CONTINUE
                  ELSE
C                   came in with a zero,
C                   factor acreage equally over all implnds for this lu
                    DO 590 K= 1,NIMP
                      IMPACR(K,I) = RVALN(J,I)/(NIMP)
 590                CONTINUE
                  END IF
                END IF
 560          CONTINUE
            END IF
C           turn previous off
            I = 4
            CALL ZSTCMA (I,I0)
          ELSE
C           tell user no reaches available
            SGRP = 66
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 60     CONTINUE
C         modify upstream table
          IF (NRCH.GT.0) THEN
C           there are some valid reaches, okay to continue
            SGRP = 46
C           allow previous
            I = 4
            CALL ZSTCMA (I,I1)
            INUM = 1
            RNUM = 1
            CNUM = 8
            IVAL(1) = 0
            RVAL(1) = 0
            I = 80*MXRCH
            CALL ZIPC (I,BLNK,TBUFF)
            DO 65 I = 1, NRCH
C             fill in reaches in tbuff
              CALL CVARAR (I8,RCHNAM(I),I8,TBUFF(1,I))
C             look through reach network array to see if any reaches
C             are upstream of this one
              CVAL2(1,1,I) = 0
              CVAL2(2,1,I) = 0
              CVAL2(3,1,I) = 0
              CVAL2(4,1,I) = 0
              CVAL2(5,1,I) = 0
              CVAL2(6,1,I) = 0
              CVAL2(7,1,I) = 0
              CVAL2(8,1,I) = 0
              K = 1
              DO 62 J = 1, NUMNET
                IF (RCHNET(J,2).EQ.I) THEN
C                 found an upstream reach
                  K = K + 8
                  CALL CVARAR (I8,RCHNAM(RCHNET(J,1)),I8,TBUFF(K,I))
                END IF
 62           CONTINUE
 65         CONTINUE
            IVAL(1) = 0
            CALL QRESCX (MESSFL,SCLU,SGRP,INUM,RNUM,CNUM,
     I                   NRCH,I1,
     M                   IVAL,RVAL,CVAL2,TBUFF)
            CALL ZGTRET (IRET)
            IF (IRET.NE.2) THEN
C             user wants to continue
              NUMNET = 0
              DO 68 I = 1, NRCH
                DO 69 J = 9,64,8
                  CALL CARVAR (I8,TBUFF(J,I),I8,TMPNAM)
                  IF (TMPNAM.NE.' ') THEN
C                   a connection was entered, is it valid
                    DO 66 K = 1,NRCH
                      IF (TMPNAM.EQ.RCHNAM(K)) THEN
C                       valid connection
                        NUMNET = NUMNET + 1
                        RCHNET(NUMNET,1) = K
                        RCHNET(NUMNET,2) = I
                      END IF
 66                 CONTINUE
                  END IF
 69             CONTINUE
 68           CONTINUE
            END IF
C           turn previous off
            I = 4
            CALL ZSTCMA (I,I0)
          ELSE
C           tell user no reaches available
            SGRP = 67
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 70     CONTINUE
C         modify downstream table
          IF (NRCH.GT.0) THEN
C           there are some valid reaches, okay to continue
            SGRP = 47
C           allow previous
            I = 4
            CALL ZSTCMA (I,I1)
            INUM = 1
            RNUM = 1
            CNUM = 8
            IVAL(1) = 0
            RVAL(1) = 0
            I = 80*MXRCH
            CALL ZIPC (I,BLNK,TBUFF)
            DO 75 I = 1, NRCH
C             fill in reaches in tbuff
              CALL CVARAR (I8,RCHNAM(I),I8,TBUFF(1,I))
C             look through reach network array to see if any reaches
C             are downstream of this one
              CVAL2(1,1,I) = 0
              CVAL2(2,1,I) = 0
              CVAL2(3,1,I) = 0
              CVAL2(4,1,I) = 0
              CVAL2(5,1,I) = 0
              CVAL2(6,1,I) = 0
              CVAL2(7,1,I) = 0
              CVAL2(8,1,I) = 0
              K = 1
              DO 72 J = 1, NUMNET
                IF (RCHNET(J,1).EQ.I) THEN
C                 found a downstream reach
                  K = K + 8
                  CALL CVARAR (I8,RCHNAM(RCHNET(J,2)),I8,TBUFF(K,I))
                END IF
 72           CONTINUE
 75         CONTINUE
            CALL QRESCX (MESSFL,SCLU,SGRP,INUM,RNUM,CNUM,
     I                   NRCH,I1,
     M                   IVAL,RVAL,CVAL2,TBUFF)
            CALL ZGTRET (IRET)
            IF (IRET.NE.2) THEN
C             user wants to continue
              NUMNET = 0
              DO 78 I = 1, NRCH
                DO 79 J = 9,64,8
                  CALL CARVAR (I8,TBUFF(J,I),I8,TMPNAM)
                  IF (TMPNAM.NE.' ') THEN
C                   a connection was entered, is it valid
                    DO 76 K = 1,NRCH
                      IF (TMPNAM.EQ.RCHNAM(K)) THEN
C                       valid connection
                        NUMNET = NUMNET + 1
                        RCHNET(NUMNET,2) = K
                        RCHNET(NUMNET,1) = I
                      END IF
 76                 CONTINUE
                  END IF
 79             CONTINUE
 78           CONTINUE
            END IF
C           turn previous off
            I = 4
            CALL ZSTCMA (I,I0)
          ELSE
C           tell user no reaches available
            SGRP = 68
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 80     CONTINUE
C         modify analysis options table
          IF (NRCH.GT.0) THEN
C           there are some valid reaches, okay to continue
            SGRP = 48
C           allow previous
            I = 4
            CALL ZSTCMA (I,I1)
            INUM = 1
            RNUM = 1
            CNUM = 2
            RVAL(1) = 0
            CALL ZIPI (6*NRCH,I0,CVAL5)
            CALL ZIPI (NRCH,I0,IVAL4)
            DO 83 I = 1,NRCH
C             set initial values for valids
              CVAL5(1,1,I) = 1
              IF (RCHTMP(I).EQ.0) THEN
C               no analysis desired for this reach
                CVAL5(2,1,I) = 1
              ELSE IF (RCHTMP(I).GT.0) THEN
C               analysis desired for this reach
                CVAL5(2,1,I) = 2
              END IF
 83         CONTINUE
            I = 80*MXRCH
            CALL ZIPC (I,BLNK,TBUFF)
            DO 85 I = 1, NRCH
C             fill in reaches in tbuff
              CALL CVARAR (I8,RCHNAM(I),I8,TBUFF(1,I))
 85         CONTINUE
            CALL QRESCX (MESSFL,SCLU,SGRP,INUM,RNUM,CNUM,
     I                   NRCH,I1,
     M                   IVAL4,RVAL,CVAL5,TBUFF)
            CALL ZGTRET (IRET)
            IF (IRET.NE.2) THEN
C             user wants to continue
              DO 87 I = 1, NRCH
C               put new analysis flags back into array
                RCHTMP(I) = CVAL5(2,1,I)-1
 87           CONTINUE
            END IF
C           turn previous off
            I = 4
            CALL ZSTCMA (I,I0)
          ELSE
C           tell user no reaches available
            SGRP = 69
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 90     CONTINUE
C         done, return
          GO TO 100
C
 100    CONTINUE
      IF (RESP .LE. 8) GO TO 5
C
C     let user know that processing is going on
      SGRP = 59
      CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,I)
C
C     check for need to create new data sets or delete old ones
      LCON = 'FLOW    '
      DO 107 I = 1,NRCH
C       set search criteria
        CALL TSESPC (LSENNM,RCHNAM(I),LCON)
        IF (RCHTMP(I).GT.0) THEN
C         mark for create if cant find match
          CALL TSDSMA (I0,
     O                 ANAWDM(I))
        ELSE IF (RCHTMP(I).EQ.0 .AND. ANAWDM(I).NE.0) THEN
C         mark for delete if no longer need it
          CALL TSDSMD
          ANAWDM(I) = 0
        END IF
 107  CONTINUE
C
C     new routine here to put all values back into hspf data structure
      UCINFO = LSENDS
      CALL PUTHSP (MXRCH,
     O             DATES,NRCH,RCHNAM,UCINFO,RCHLEN,METSEG,NUMNET,
     O             RCHNET,LANDAC,ANAWDM,RCHINI,IMPACR,NIMP,
     O             IMPLID,RCHRID,NPER,PERLID,RCHSLO)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SSMADD
     I                   (MESSFL,SCLU,SMXRCH,NIMP,LANDUS,
     M                    RCHTMP,PCPNAM,EVPNAM,PCPDSN,EVPDSN,
     M                    METSEG,RCHLEN,RCHINI,LANDAC,
     M                    IMPACR,RCHNET,NUMNET,NRCH,
     M                    RCHNAM,RCHRID,RCHSLO)
C
C     + + + PURPOSE + + +
C     add a reach to an existing scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,SMXRCH,RCHTMP(SMXRCH),NUMNET,NIMP,
     1              METSEG(SMXRCH,3),RCHNET(7*SMXRCH,2),NRCH,
     2              PCPDSN(5),EVPDSN(5),RCHRID(SMXRCH)
      REAL          RCHLEN(SMXRCH),LANDAC(50,SMXRCH),RCHINI(SMXRCH),
     1              IMPACR(20,SMXRCH),RCHSLO(SMXRCH)
      CHARACTER*8   PCPNAM(5),EVPNAM(5),RCHNAM(48)
      CHARACTER*20  LANDUS(SMXRCH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen message cluster
C     SMXRCH - maximum number of reaches
C     RCHTMP - array of reach data set numbers
C     PCPNAM - precip station names
C     EVPNAM - evap station names
C     PCPDSN - precip data set numbers
C     EVPDSN - evap data set numbers
C     NUMNET - number of network connections
C     NIMP   - number of impervious areas
C     METSEG - associated met segment for each reach
C     RCHNET - reach to reach source/target pairs
C     RCHLEN - length of each reach
C     LANDAC - acres flowing into each reach from each perlnd
C     RCHINI - initial volume in each reach
C     IMPACR - number of acres from each implnd to each reach
C     LANDUS - array of land use types for each perlnd
C     NRCH   - number of reaches in this scenario
C     RCHNAM - array of names of each reach
C     RCHRID - array of reach ids
C     RCHSLO - slope of reach
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,CNUM,CLEN(3),TLEN,INUM,IVAL(6),CBAS,FOUND,
     1              IRET,I,I0,I1,OPCNT,OPLEN,I8,I20,MXSEL(2),FTUNIT(1),
     2              MNSEL(2),OPVAL(4),RNUM,CVAL1(4),IVAL2(14),FTRET,
     3              CVAL3(14),I78,IM1,PREVFG,VLINFG(288),NMFLDS,
     4              K,J,IPOS,NLAND,PERFST,PERLST,NOIN(12),IVALX(1,1),
     7              CVALX(13,3,1),DNUM,I4,DOWNST(7),UPST(7),NPCP,NEVP
      REAL          R0,RVAL(13),RMIN(4),RMAX(4),RDEF(4),
     1              RVALSV(12,48),RVALX(12,1),RVAL5(5)
      DOUBLE PRECISION DVAL(1),DVALX(1,1)
      CHARACTER*1   BLNK,CTXT(128),URSPST(960),CHEAD1(78),
     1              LANDU1(20),TBUFFX(150,1)
      CHARACTER*8   TMPNAM,CNONE
      CHARACTER*78  CHEAD
C
C     + + + INTRINSICS + + +
      INTRINSIC     ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL      Q1INIT,ZIPC,QSETCT,Q1EDIT,QSETOP,ZBLDWR,ZIPI
      EXTERNAL      QGETOP,ZGTRET,QSCSET,CARVAR,CVARAR,PRNTXT
      EXTERNAL      QGETCT,ZSTCMA,ZIPR,QSETR,QGETR,ZMNSST,QUPCAS
      EXTERNAL      QGTCOB,QSTCOB,COPYR,QRESCZ,HADDOP,HADDFT
      EXTERNAL      HADDFF,QGETF,HDELFT
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IM1= -1
      R0 = 0.0
      I1 = 1
      I4 = 4
      I20= 20
      I8 = 8
      I78= 78
      BLNK= ' '
C
C     initialize fields for first screen
      OPVAL(1) = 4
      OPVAL(2) = 0
      OPVAL(3) = 0
C     set initial defaults
      RNUM = 4
      CALL ZIPR (RNUM,R0,RVAL)
      RVAL(2) = 0.01
      RCHNAM(NRCH+1) = 'NEWRCH'
 10   CONTINUE
        IVAL(1) = 0
        RMIN(1) = 0.0
        RMIN(2) = 0.01
        RMIN(3) = 0.0
        RMIN(4) = 0.0
        RMAX(1) = -999
        RMAX(2) = -999
        RMAX(3) = -999
        RMAX(4) = -999
        RDEF(1) = 0.0
        RDEF(2) = 0.01
        RDEF(3) = 0.0
        RDEF(4) = 0.0
        DVAL(1) = 0.0
        CVAL1(1)= 1
        CVAL1(2)= 1
        CVAL1(3)= 1
        CVAL1(4)= 1
        IVAL2(1)= 8
        IVAL2(2)= 8
        IVAL2(3)= 0
        IVAL2(4)= 0
        IVAL2(5)= 0
        IVAL2(6)= 0
        IVAL2(7)= 0
        IVAL2(8)= 0
        IVAL2(9)= 0
        IVAL2(10)=0
        IVAL2(11)=0
        IVAL2(12)=8
        IVAL2(13)=0
        IVAL2(14)=8
C
C       set invalid values to choose for reach names
        DO 15 I = 1,NRCH
C         fill in each valid reach name
          CALL CVARAR (I8,RCHNAM(I),I8,
     O                 URSPST(((I-1)*8)+1))
          CALL CVARAR (I8,RCHNAM(I),I8,
     O                 URSPST(((I-1)*8)+1+(NRCH*8)))
 15     CONTINUE
C
C       set valid prcp station names
        NPCP = 0
        DO 20 I = 1,5
C         count how many pcp stations
          IF (PCPNAM(I).NE.' ') THEN
C           valid met station, count it
            NPCP = NPCP + 1
          END IF
 20     CONTINUE
        IF (NPCP.GT.0) THEN
C         some station names exist
          CNONE = 'NONE    '
          CALL CVARAR (I8,CNONE,I8,
     O                 URSPST((2*8*NRCH)+1))
          DO 30 I = 1,NPCP
C           fill in each valid met station name
            CALL CVARAR (I8,PCPNAM(I),I8,
     O                   URSPST(((I-1)*8)+9+(2*8*NRCH)))
 30       CONTINUE
        END IF
C
C       set valid evap station names
        NEVP = 0
        DO 25 I = 1,5
C         count how many evp stations
          IF (EVPNAM(I).NE.' ') THEN
C           valid evp station, count it
            NEVP = NEVP + 1
          END IF
 25     CONTINUE
        IF (NEVP.GT.0) THEN
C         some station names exist
          CNONE = 'NONE    '
          CALL CVARAR (I8,CNONE,I8,
     O                 URSPST((9+(8*NPCP)+(2*8*NRCH))))
          DO 35 I = 1,NEVP
C           fill in each valid evp station name
            CALL CVARAR (I8,EVPNAM(I),I8,
     O                   URSPST((I*8)+9+(8*NPCP)+(2*8*NRCH)))
 35       CONTINUE
        END IF
C
C       set valid responses
        CVAL3(1)= NRCH
        CVAL3(2)= NRCH
        CVAL3(3)= 0
        CVAL3(4)= 0
        CVAL3(5)= 0
        CVAL3(6)= 0
        CVAL3(7)= 0
        CVAL3(8)= 0
        CVAL3(9)= 0
        CVAL3(10)=0
        CVAL3(11)= 0
        CVAL3(12)= NPCP+1
        CVAL3(13)= 0
        CVAL3(14)= NEVP+1
        I = 4
        NMFLDS = 14
        CALL ZIPI (NMFLDS,I0,VLINFG)
        VLINFG(2)  = 1
        VLINFG(12) = 1
        VLINFG(14) = 1
C       clear out existing valids
        VLINFG(1)  = 0
        CALL QSCSET (I1,I4,I1,I,NMFLDS,IVAL,IVAL,
     I               IVAL,RMIN,RMAX,RDEF,DVAL,DVAL,DVAL,
     I               VLINFG,CVAL3,CVAL1,IVAL2,URSPST)
        VLINFG(1)  = 2
        CALL QSCSET (I1,I4,I1,I,NMFLDS,IVAL,IVAL,
     I               IVAL,RMIN,RMAX,RDEF,DVAL,DVAL,DVAL,
     I               VLINFG,CVAL3,CVAL1,IVAL2,URSPST)
C
        SGRP = 42
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C
C       set reach name, met name
        CNUM = 2
        CLEN(1) = 8
        CLEN(2) = 8
        CLEN(3) = 8
        TLEN = 16
        CALL CVARAR (CLEN(1),RCHNAM(NRCH+1),CLEN(1),CTXT)
        CALL CVARAR (CLEN(2),RCHNAM(1),CLEN(2),CTXT(9))
        CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
        CNUM = 3
        CBAS = 3
        CALL QSTCOB (CNUM,CBAS,CVAL1)
C
C       set reach characteristics
        RNUM = 4
        CALL QSETR (RNUM,RVAL)
C
C       set ftable options
        OPCNT = 2
        OPLEN = 3
        MXSEL(1) = 1
        MNSEL(1) = 1
        MXSEL(2) = 2
        MNSEL(2) = 0
        CALL QSETOP (OPCNT,OPLEN,MXSEL,MNSEL,OPVAL)
C
C       allow previous
        I = 4
        CALL ZSTCMA (I,I1)
        PREVFG = 0
        CALL Q1EDIT (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          NRCH = NRCH + 1
          CNUM = 1
          CALL QGETCT (CNUM,CLEN,TLEN,CTXT)
          CALL QUPCAS (CLEN(1),CTXT(1))
          CALL CARVAR (CLEN(1),CTXT,CLEN(1),RCHNAM(NRCH))
C
C         get associated met segment
          CNUM = 4
          CBAS = 2
          CALL QGTCOB (CNUM,CBAS,CVAL1)
C         see which options on
          CALL QGETOP (OPLEN,OPVAL)
C
C         get associated pcp segment
          IF (CVAL1(2).EQ.1) THEN
C           no associated segment
            METSEG(NRCH,1) = 0
          ELSE
C           have an associated segment
            METSEG(NRCH,1) = PCPDSN(CVAL1(2)-1)
          END IF
C         get associated evp segment
          IF (CVAL1(3).EQ.1) THEN
C           no associated segment
            METSEG(NRCH,2) = 0
          ELSE
C           have an associated segment
            METSEG(NRCH,2) = EVPDSN(CVAL1(3)-1)
          END IF
          METSEG(NRCH,3) = -99
          IF (METSEG(NRCH,1).GT.0 .AND. OPVAL(2).EQ.1) THEN
C           prcp segment has been specified and wanted
            METSEG(NRCH,3) = 1
          END IF
          IF (METSEG(NRCH,2).GT.0) THEN
C           evap segment has been specified
            IF (OPVAL(2).EQ.2 .OR. OPVAL(3).EQ.2) THEN
C             evap wanted
              IF (METSEG(NRCH,3).EQ.1) THEN
C               precip wanted too, so flag for both
                METSEG(NRCH,3) = 3
              ELSE
C               evap only
                METSEG(NRCH,3) = 2
              END IF
            END IF
          END IF
C
C         get reach characteristics
          CALL QGETR (RNUM,RVAL)
          RCHSLO(NRCH) = RVAL(1)
          RCHLEN(NRCH) = RVAL(2)
          RCHINI(NRCH) = RVAL(4)
C
C         give new reach an id
          J = 0
 55       CONTINUE
C           find first unused reach id
            J = J + 1
            FOUND = 0
            DO 50 I = 1,NRCH-1
              IF (RCHRID(I).EQ.J) THEN
                FOUND = 1
              END IF
 50         CONTINUE
          IF (FOUND.EQ.1) GO TO 55
C         found unused reach id
          RCHRID(NRCH) = J
C
C         do ftable data
          IF (OPVAL(1).EQ.1) THEN
C           user wants to enter ftable by xy pairs
            SGRP = 51
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            CALL ZGTRET (IRET)
            IF (IRET.EQ.2) THEN
C             user wants previous back to first screen
              PREVFG = 1
            ELSE
              PREVFG = 0
            END IF
          ELSE IF (OPVAL(1).EQ.2) THEN
C           user wants to enter ftable by trapezoid
            SGRP = 52
            CALL Q1INIT (MESSFL,SCLU,SGRP)
C           set trapezoid characteristics
            RNUM = 5
            CALL ZIPR (RNUM,R0,RVAL5)
            CALL QSETR (RNUM,RVAL5)
            CALL Q1EDIT (IRET)
            IF (IRET.NE.2) THEN
C             user wants to continue, get trapezoid values
              CALL QGETR (RNUM,RVAL5)
C             actual generation of ftable not yet available
              SGRP = 53
              CALL PRNTXT (MESSFL,SCLU,SGRP)
              CALL ZGTRET (IRET)
              IF (IRET.EQ.2) THEN
C               user wants previous back to first screen
                PREVFG = 1
              ELSE
                PREVFG = 0
              END IF
            ELSE
C             user wants previous, get back to chars screen
              PREVFG = 1
            END IF
          ELSE IF (OPVAL(1).EQ.3) THEN
C           user wants to read ftable from file
            SGRP = 61
            CALL Q1INIT (MESSFL,SCLU,SGRP)
            CALL Q1EDIT (IRET)
            IF (IRET.EQ.2) THEN
C             user wants previous back to first screen
              PREVFG = 1
            ELSE
C             get unit number for ftable file
              CALL QGETF (I1,FTUNIT)
              PREVFG = 0
C             user wants ftable from file
              CALL HADDFF (RCHRID(NRCH),FTUNIT(1),
     O                     FTRET)
              IF (FTRET.NE.0) THEN
C               problem reading ftable from file, tell user
                SGRP = 58
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                PREVFG = 1
              END IF
            END IF
          END IF
C
          IF (PREVFG.EQ.0) THEN
C           do second screen of parameters
            CHEAD = ' Reach               Acres of each l'//
     1         'and use type (up to 12)                   '
            CALL CVARAR (I78,CHEAD,I78,CHEAD1)
C           insert reach name into header
            CALL CVARAR (I8,RCHNAM(NRCH),I8,CHEAD1(8))
            CALL ZBLDWR (I78,CHEAD1,I1,IM1,I)
C
C           figure out how many land use types and build header
            CALL ZIPC (I78,BLNK,CHEAD1)
            IPOS = 4
            NLAND= 0
            I = 12
            CALL ZIPI (I,I0,NOIN)
            CALL ZIPR (I,R0,RVALX)
            DO 400 I = 1,SMXRCH
C             loop through each perlnd for land use types
              IF (LANDUS(I).NE.'                    ') THEN
C               name entered
                IF (I.GT.1) THEN
C                 see if name same as previous name
                  IF (LANDUS(I).NE.LANDUS(I-1)) THEN
C                   need to put this name in header
                    CALL CVARAR (I20,LANDUS(I),I20,LANDU1)
                    IPOS = IPOS + 2
                    CHEAD1(IPOS) = LANDU1(1)
                    IPOS = IPOS + 1
                    CHEAD1(IPOS) = LANDU1(2)
                    IPOS = IPOS + 1
                    CHEAD1(IPOS) = LANDU1(3)
                    IPOS = IPOS + 1
                    CHEAD1(IPOS) = LANDU1(4)
C                   put data for this perlnd/reach into screen
                    NLAND = NLAND + 1
C                   keep track of how many perlnds in each land use class
                    NOIN(IPOS-8-((NLAND-1)*4)) = 1
                  ELSE
C                   already have name in header,
C                   increment count of number of perlnds in this class
                    NOIN(IPOS-8-((NLAND-1)*4)) =
     1                    NOIN(IPOS-8-((NLAND-1)*4))+1
                  END IF
                ELSE
C                 put name in header
                  CALL CVARAR (I20,LANDUS(I),I20,LANDU1)
                  IPOS = IPOS + 2
                  CHEAD1(IPOS) = LANDU1(1)
                  IPOS = IPOS + 1
                  CHEAD1(IPOS) = LANDU1(2)
                  IPOS = IPOS + 1
                  CHEAD1(IPOS) = LANDU1(3)
                  IPOS = IPOS + 1
                  CHEAD1(IPOS) = LANDU1(4)
C                 put data for this perlnd/reach into screen
                  NLAND = NLAND + 1
                  NOIN(1) = 1
                END IF
              END IF
 400        CONTINUE
C
C           put impervious acreage into buffer
            IPOS = IPOS + 3
            CHEAD1(IPOS) = 'I'
            IPOS = IPOS + 1
            CHEAD1(IPOS) = 'M'
            IPOS = IPOS + 1
            CHEAD1(IPOS) = 'P'
            IPOS = IPOS + 1
            CHEAD1(IPOS) = ' '
            CALL ZBLDWR (I78,CHEAD1,I0,IM1,I)
C
            CALL ZMNSST
C
C           fill tbuff with upstream and downstream reaches
            I = 150
            CALL ZIPC (I,BLNK,TBUFFX)
            INUM = 1
            RNUM = 12
            CNUM = 13
            DNUM = 1
            TLEN = 150
            IVALX(1,1) = 0
            DVALX(1,1) = 0
            I = 39
            CALL ZIPI (I,I0,CVALX)
            CVALX(13,1,1) = 1
C
C           save values of land areas for later checking
            I = 12
            CALL COPYR (I,RVALX,RVALSV)
C
            SGRP = 82
            CALL QRESCZ (MESSFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,
     I                   I1,I0,TLEN,
     M                   IVALX,RVALX,DVALX,CVALX,TBUFFX)
            CALL ZGTRET (IRET)
            PREVFG = 0
C
            IF (IRET.EQ.1) THEN
C             user wants to continue
C             process land acreages
              PERFST = 1
              DO 410 J = 1,50
C               initialize land acreages to zero
                LANDAC(J,NRCH) = 0.0
 410          CONTINUE
              DO 420 J = 1,20
C               initialize land acreages to zero
                IMPACR(J,NRCH) = 0.0
 420          CONTINUE
C
              DO 430 J = 1,NLAND
                IF ((ABS(RVALX(J,1)-RVALSV(J,1)).GT.0.999.AND.
     1              RVALX(J,1).GT.99.999) .OR.
     2             (ABS(RVALX(J,1)-RVALSV(J,1)).GT.0.0999.AND.
     3              RVALX(J,1).LT.99.999)) THEN
C                 value has changed
C                 figure out which landac values reflect this one
                  PERLST = PERFST+NOIN(J)-1
C                 sum of landac(i,perfst)..(i,perlst) must equal table val
C                 came in with a zero,
C                 factor acreage equally over all perlnds for this lu
                  DO 440 K= PERFST,PERLST
                    LANDAC(K,NRCH) =
     1                   RVALX(J,1)/(PERLST-PERFST+1)
 440              CONTINUE
                END IF
C               keep track of which perlnd for next time through
                PERFST = PERFST+NOIN(J)
 430          CONTINUE
C
C             now check implnd land use acreages
              I = NRCH
              J = NLAND+1
              IF ((ABS(RVALX(J,1)-RVALSV(J,I)).GT.0.999 .AND.
     1            RVALX(J,1).GT.99.999) .OR.
     2            (ABS(RVALX(J,1)-RVALSV(J,I)).GT.0.0999 .AND.
     3            RVALX(J,1).LT.99.999)) THEN
C               value has changed
C               figure out which impacr values reflect this one
C               sum of impacr(i, ) must equal table val
C               came in with a zero,
C               factor acreage equally over all implnds for this lu
                DO 450 K= 1,NIMP
                  IMPACR(K,I) = RVALX(J,1)/(NIMP)
 450            CONTINUE
              END IF
C
C             put output flag back
              RCHTMP(NRCH) = CVALX(13,1,1)-1
C
C             put upstream/downstream connections back
              IPOS = 0
              I = 7
              CALL ZIPI(I,I0,UPST)
              DO 460 J = 49,104,8
                CALL CARVAR (I8,TBUFFX(J,1),I8,TMPNAM)
                IF (TMPNAM.NE.' ') THEN
C                 a connection was entered, is it valid
                  DO 470 K = 1,NRCH-1
                    IF (TMPNAM.EQ.RCHNAM(K)) THEN
C                     valid upstream connection,
C                     need to add it
                      IPOS = IPOS + 1
                      UPST(IPOS) = RCHRID(K)
                      NUMNET = NUMNET + 1
                      RCHNET(NUMNET,1) = K
                      RCHNET(NUMNET,2) = NRCH
                    END IF
 470              CONTINUE
                END IF
 460          CONTINUE
C
C             now put downstream connections back
              IPOS = 0
              I = 7
              CALL ZIPI(I,I0,DOWNST)
              DO 490 J = 105,144,8
                CALL CARVAR (I8,TBUFFX(J,1),I8,TMPNAM)
                IF (TMPNAM.NE.' ') THEN
C                 a connection was entered, is it valid
                  DO 480 K = 1,NRCH-1
                    IF (TMPNAM.EQ.RCHNAM(K)) THEN
C                     valid downstream connection,
C                     need to add it
                      IPOS = IPOS + 1
                      DOWNST(IPOS) = RCHRID(K)
                      NUMNET = NUMNET + 1
                      RCHNET(NUMNET,2) = K
                      RCHNET(NUMNET,1) = NRCH
                    END IF
 480              CONTINUE
                END IF
 490          CONTINUE
C
C             add this reach in uci file records
              I = 3
              CALL HADDOP (I,RCHRID(NRCH),DOWNST,UPST,RCHRID(CVAL1(1)))
              IF (OPVAL(1).EQ.4) THEN
C               user wants to default ftable
                CALL HADDFT (RCHRID(NRCH),RCHRID(CVAL1(1)))
              END IF
            ELSE IF (IRET.EQ.2) THEN
C             user wants previous back to first screen
              PREVFG = 1
            END IF
          END IF
        END IF
      IF (PREVFG.EQ.1) THEN
C       before returning to first screen need to remove reach already added
        CALL HDELFT (RCHRID(NRCH),I1)
        NRCH = NRCH - 1
      END IF
      IF (PREVFG.EQ.1) GO TO 10
C     turn previous off
      I = 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SSMDEL
     I                   (MESSFL,SCLU,SMXRCH,
     M                    RCHTMP,ANAWDM,METSEG,RCHLEN,RCHINI,LANDAC,
     M                    IMPACR,RCHNET,NUMNET,NRCH,RCHNAM,RCHRID,
     M                    RCHSLO)
C
C     + + + PURPOSE + + +
C     delete a reach from an existing scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,SMXRCH,RCHTMP(SMXRCH),NUMNET,
     1              METSEG(SMXRCH,3),RCHNET(7*SMXRCH,2),NRCH,
     2              ANAWDM(SMXRCH),RCHRID(SMXRCH)
      REAL          RCHLEN(SMXRCH),LANDAC(50,SMXRCH),RCHINI(SMXRCH),
     1              IMPACR(20,SMXRCH),RCHSLO(SMXRCH)
      CHARACTER*8   RCHNAM(48)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen message cluster
C     SMXRCH - maximum number of reaches
C     RCHTMP - array of wdm output desired flags
C     ANAWDM - array of reach data set numbers
C     NUMNET - number of network connections
C     METSEG - associated met segment for each reach
C     RCHNET - reach to reach source/target pairs
C     RCHLEN - length of each reach
C     LANDAC - acres flowing into each reach from each perlnd
C     RCHINI - initial volume in each reach
C     IMPACR - number of acres from each implnd to each reach
C     NRCH   - number of reaches in this scenario
C     RCHNAM - array of names of each reach
C     RCHRID - array of rchres ids
C     RCHSLO - slope of reach
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,CNUM,IVAL(6),IRET,I,I0,I1,OPCNT,OPLEN,
     1              I8,MXSEL(2),MNSEL(2),OPVAL(3),CVAL1(2),I2,
     2              IVAL2(2),CVAL3(2),K,J,DELRCH,
     3              DOWNST(7),UPST(7),L
      REAL          RVAL(13)
      DOUBLE PRECISION DVAL(1)
      CHARACTER*1   URSPST(520)
C
C     + + + EXTERNALS + + +
      EXTERNAL      Q1INIT,Q1EDIT,QSETOP,ZIPI
      EXTERNAL      QGETOP,QSCSET,CVARAR,ZSTCMA
      EXTERNAL      HDELOP,ZLIMIT,QSETCO,QGETCO,HDELFT
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I2 = 2
      I8 = 8
C
C     set valid values to choose to delete
      IVAL(1) = 0
      RVAL(1) = 0.0
      DVAL(1) = 0.0
      CVAL3(1)= NRCH
      CVAL3(2)= 0
      IVAL2(1)= 8
      IVAL2(2)= 0
      DO 10 I = 1,NRCH
C       fill in each valid reach name
        CALL CVARAR (I8,RCHNAM(I),I8,
     O               URSPST(((I-1)*8)+1))
 10   CONTINUE
C     clear out existing invalids
      CVAL1(1)= 0
      CVAL1(2)= 0
      CALL QSCSET (I1,I1,I1,I2,I2,IVAL,IVAL,
     I             IVAL,RVAL,RVAL,RVAL,DVAL,DVAL,DVAL,
     I             CVAL1,CVAL3,CVAL1,IVAL2,URSPST)
      CVAL1(1)= 1
      CVAL1(2)= 0
      CALL QSCSET (I1,I1,I1,I2,I2,IVAL,IVAL,
     I             IVAL,RVAL,RVAL,RVAL,DVAL,DVAL,DVAL,
     I             CVAL1,CVAL3,CVAL1,IVAL2,URSPST)
C
      SGRP = 43
      CALL Q1INIT (MESSFL,SCLU,SGRP)
C
C     allow previous
      I = 4
      CALL ZSTCMA (I,I1)
C
C     set reach name
      CNUM = 1
      CALL QSETCO (CNUM,CVAL1)
C
C     set reconnect option
      OPCNT = 1
      OPLEN = 1
      MXSEL(1) = 1
      MNSEL(1) = 0
      OPVAL(1) = 0
      CALL QSETOP (OPCNT,OPLEN,MXSEL,MNSEL,OPVAL)
C
      CALL ZLIMIT
      CALL Q1EDIT (IRET)
      IF (IRET.EQ.1) THEN
C       user wants to continue
C
        CALL QGETCO (CNUM,CVAL1)
        DELRCH = RCHRID(CVAL1(1))
        IF (CVAL1(1).LT.NRCH) THEN
C         not last reach deleted, need to pack
          DO 20 I = CVAL1(1),NRCH-1
            RCHNAM(I) = RCHNAM(I+1)
            RCHLEN(I) = RCHLEN(I+1)
            RCHSLO(I) = RCHSLO(I+1)
            METSEG(I,1) = METSEG(I+1,1)
            METSEG(I,2) = METSEG(I+1,2)
            METSEG(I,3) = METSEG(I+1,3)
            RCHTMP(I) = RCHTMP(I+1)
            ANAWDM(I) = ANAWDM(I+1)
            RCHINI(I) = RCHINI(I+1)
            RCHRID(I) = RCHRID(I+1)
            DO 30 J = 1,50
              LANDAC(J,I) = LANDAC(J,I+1)
 30         CONTINUE
            DO 40 J = 1,20
              IMPACR(J,I) = IMPACR(J,I+1)
 40         CONTINUE
 20       CONTINUE
        END IF
C
C       remove any connections involving this reach
        K = NUMNET
        J = 1
 60     CONTINUE
          IF (RCHNET(J,2).EQ.CVAL1(1) .OR.
     1        RCHNET(J,1).EQ.CVAL1(1)) THEN
C           remove this connection
            DO 70 L = J,K
              RCHNET(L,1) = RCHNET(L+1,1)
              RCHNET(L,2) = RCHNET(L+1,2)
 70         CONTINUE
            NUMNET = NUMNET -1
          ELSE
C           check next record
            J = J+1
          END IF
        IF (J.LE.K) GO TO 60
C
        NRCH = NRCH - 1
C
        CALL QGETOP (OPLEN,OPVAL)
        IF (OPVAL(1).EQ.1) THEN
C         user wants reconnect option
          L = 0
          K = 0
          I = 7
          CALL ZIPI (I,I0,UPST)
          CALL ZIPI (I,I0,DOWNST)
C         store what is up and down stream from here
          DO 80 J = 1,NUMNET
            IF (RCHNET(J,1).EQ.CVAL1(1)) THEN
C             downstream reach
              K = K + 1
              DOWNST(K) = RCHNET(J,2)
            ELSE IF (RCHNET(J,2).EQ.CVAL1(1)) THEN
C             upstream reach
              L = L + 1
              UPST(L) = RCHNET(J,1)
            END IF
 80       CONTINUE
C         do reconnect option
          DO 90 I = 1,7
            DO 100 J = 1,7
              IF (UPST(I).GT.0 .AND. DOWNST(J).GT.0) THEN
C               we need to add a connection
                NUMNET = NUMNET + 1
                RCHNET(NUMNET,1) = UPST(I)
                RCHNET(NUMNET,2) = DOWNST(J)
              END IF
 100        CONTINUE
 90       CONTINUE
        END IF
C
        DO 110 I = 1,NUMNET
          IF (RCHNET(I,1).GT.CVAL1(1)) THEN
C           needs to be decremented
            RCHNET(I,1) = RCHNET(I,1)-1
          END IF
          IF (RCHNET(I,2).GT.CVAL1(1)) THEN
C           needs to be decremented
            RCHNET(I,2) = RCHNET(I,2)-1
          END IF
 110    CONTINUE
C
C       call routine to remove this reach from hspf data structure
        I = 3
        CALL HDELOP (I,DELRCH)
C       remove ftable for this reach
        CALL HDELFT (DELRCH,I1)
C
      END IF
C
C     turn previous off
      I = 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SSMCHR
     I                   (MESSFL,SCLU,SMXRCH,NIMP,LANDUS,
     M                    RCHTMP,PCPNAM,EVPNAM,PCPDSN,EVPDSN,
     M                    METSEG,RCHLEN,RCHINI,LANDAC,
     M                    IMPACR,RCHNET,NUMNET,NRCH,RCHNAM,RCHSLO)
C
C     + + + PURPOSE + + +
C     modify characteristics of a reach
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,SMXRCH,RCHTMP(SMXRCH),NUMNET,NIMP,
     1              METSEG(SMXRCH,3),RCHNET(7*SMXRCH,2),NRCH,
     2              PCPDSN(5),EVPDSN(5)
      REAL          RCHLEN(SMXRCH),LANDAC(50,SMXRCH),RCHINI(SMXRCH),
     1              IMPACR(20,SMXRCH),RCHSLO(SMXRCH)
      CHARACTER*8   PCPNAM(5),EVPNAM(5),RCHNAM(48)
      CHARACTER*20  LANDUS(SMXRCH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen message cluster
C     SMXRCH - maximum number of reaches
C     RCHTMP - array of reach data set numbers
C     PCPNAM - precip station names
C     EVPNAM - evap station names
C     PCPDSN - precip data set numbers
C     EVPDSN - evap data set numbers
C     NUMNET - number of network connections
C     NIMP   - number of impervious areas
C     METSEG - associated met segment for each reach
C     RCHNET - reach to reach source/target pairs
C     RCHLEN - length of each reach
C     LANDAC - acres flowing into each reach from each perlnd
C     RCHINI - initial volume in each reach
C     IMPACR - number of acres from each implnd to each reach
C     LANDUS - array of land use types for each perlnd
C     NRCH   - number of reaches in this scenario
C     RCHNAM - array of names of each reach
C     RCHSLO - slope of reach
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,CNUM,TLEN,INUM,IVAL(6),NPCP,NEVP,FTUNIT(1),
     1              IRET,I,I0,I1,OPCNT,OPLEN,I8,I20,MXSEL(2),FTRET,
     2              MNSEL(2),OPVAL(3),RNUM,CVAL1(3),IVAL2(13),
     3              CVAL3(13),I78,IM1,PREVFG,VLINFG(13),NMFLDS,
     4              K,J,IPOS,NLAND,PERFST,PERLST,NOIN(12),IVALX(1,1),
     7              CVALX(13,3,1),DNUM,I4,CVAL4(1),FOUND,I3,I5
      REAL          R0,RVAL(13),RMIN(4),RMAX(4),RDEF(4),RFACT,
     1              RVALSV(12,48),RVALX(12,1)
      DOUBLE PRECISION DVAL(1),DVALX(1,1)
      CHARACTER*1   BLNK,URSPST(520),CHEAD1(78),
     1              LANDU1(20),TBUFFX(150,1),CTEMP(5)
      CHARACTER*8   TMPNAM,CNONE
      CHARACTER*78  CHEAD
C
C     + + + FUNCTIONS + + +
      INTEGER       STRFND
      REAL          CHRDEC
C
C     + + + INTRINSICS + + +
      INTRINSIC     ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL      Q1INIT,ZIPC,Q1EDIT,QSETOP,ZBLDWR,ZIPI
      EXTERNAL      QGETOP,ZGTRET,QSCSET,CARVAR,CVARAR,PRNTXT
      EXTERNAL      ZSTCMA,ZIPR,QSETR,QGETR,ZMNSST,STRFND
      EXTERNAL      COPYR,QRESCZ,ZLIMIT,QSETCO,QGETCO,CHRDEC,DECCHX
      EXTERNAL      HADDFF,HDELFT,QGETF
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IM1= -1
      R0 = 0.0
      I1 = 1
      I3 = 3
      I4 = 4
      I5 = 5
      I20= 20
      I8 = 8
      I78= 78
      BLNK= ' '
C
C     do first screen to specify reach desired
      CVAL1(1)= 1
 10   CONTINUE
        PREVFG  = 0
        IVAL(1) = 0
        RVAL(1) = 0.0
        DVAL(1) = 0.0
        CVAL3(1)= NRCH
        IVAL2(1)= 8
        I = 520
        CALL ZIPC (I,BLNK,URSPST)
        DO 20 I = 1,NRCH
C         fill in each valid reach name
          CALL CVARAR (I8,RCHNAM(I),I8,
     O                 URSPST(((I-1)*8)+1))
 20     CONTINUE
C       clear out invalids
        CVAL4(1)= 0
        CALL QSCSET (I1,I1,I1,I1,I1,IVAL,IVAL,
     I               IVAL,RVAL,RVAL,RVAL,DVAL,DVAL,DVAL,
     I               CVAL4,CVAL3,CVAL1,IVAL2,URSPST)
        CVAL4(1)= 1
        CALL QSCSET (I1,I1,I1,I1,I1,IVAL,IVAL,
     I               IVAL,RVAL,RVAL,RVAL,DVAL,DVAL,DVAL,
     I               CVAL4,CVAL3,CVAL1,IVAL2,URSPST)
C
        SGRP = 39
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C
C       allow previous
        I = 4
        CALL ZSTCMA (I,I1)
C       set reach name
        CNUM = 1
        CALL QSETCO (CNUM,CVAL1)
C
        CALL ZLIMIT
        CALL Q1EDIT (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          CALL QGETCO (CNUM,CVAL1)
C         proceed with first screen of parameters
          OPVAL(1) = 4
          OPVAL(2) = 0
          OPVAL(3) = 0
 30       CONTINUE
C           set valid values for each field
            IVAL(1) = 0
            RMIN(1) = 0.0
            RMIN(2) = 0.01
            RMIN(3) = 0.0
            RMIN(4) = 0.0
            RMAX(1) = -999
            RMAX(2) = -999
            RMAX(3) = -999
            RMAX(4) = -999
            RDEF(1) = 0.0
            RDEF(2) = 0.01
            RDEF(3) = 0.0
            RDEF(4) = 0.0
            DVAL(1) = 0.0
            CVAL1(2)= 1
            IVAL2(1)= 8
            IVAL2(2)= 0
            IVAL2(3)= 0
            IVAL2(4)= 0
            IVAL2(5)= 0
            IVAL2(6)= 0
            IVAL2(7)= 0
            IVAL2(8)= 0
            IVAL2(9)= 0
            IVAL2(10)=0
            IVAL2(11)=8
            IVAL2(12)=0
            IVAL2(13)=8
            DO 40 I = 1,NRCH
C             fill in each valid reach name
              CALL CVARAR (I8,RCHNAM(I),I8,
     O                     URSPST(((I-1)*8)+1))
 40         CONTINUE
C
C           set valid prcp station names
            NPCP = 0
            DO 50 I = 1,5
C             count how many pcp stations
              IF (PCPNAM(I).NE.' ') THEN
C               valid met station, count it
                NPCP = NPCP + 1
              END IF
 50         CONTINUE
            IF (NPCP.GT.0) THEN
C             some station names exist
              CNONE = 'NONE    '
              CALL CVARAR (I8,CNONE,I8,
     O                     URSPST((1+(8*NRCH))))
              DO 60 I = 1,NPCP
C               fill in each valid met station name
                CALL CVARAR (I8,PCPNAM(I),I8,
     O                       URSPST(((I-1)*8)+9+(8*NRCH)))
 60           CONTINUE
            END IF
C           set valid evap station names
            NEVP = 0
            DO 55 I = 1,5
C             count how many evp stations
              IF (EVPNAM(I).NE.' ') THEN
C               valid evp station, count it
                NEVP = NEVP + 1
              END IF
 55         CONTINUE
            IF (NEVP.GT.0) THEN
C             some station names exist
              CNONE = 'NONE    '
              CALL CVARAR (I8,CNONE,I8,
     O                     URSPST((9+(8*NRCH)+(8*NPCP))))
              DO 65 I = 1,NEVP
C               fill in each valid evp station name
                CALL CVARAR (I8,EVPNAM(I),I8,
     O                       URSPST(((I-1)*8)+17+(8*NRCH)+(8*NPCP)))
 65           CONTINUE
            END IF
C
            CVAL3(1)= NRCH
            CVAL3(2)= 0
            CVAL3(3)= 0
            CVAL3(4)= 0
            CVAL3(5)= 0
            CVAL3(6)= 0
            CVAL3(7)= 0
            CVAL3(8)= 0
            CVAL3(9)= 0
            CVAL3(10)=0
            CVAL3(11)=NPCP + 1
            CVAL3(12)=0
            CVAL3(13)=NEVP + 1
            I = 3
            NMFLDS = 13
            CALL ZIPI (NMFLDS,I0,VLINFG)
            VLINFG(1) = 1
            VLINFG(11)= 1
            VLINFG(13)= 1
            CALL QSCSET (I1,I4,I1,I,NMFLDS,IVAL,IVAL,
     I                   IVAL,RMIN,RMAX,RDEF,DVAL,DVAL,DVAL,
     I                   VLINFG,CVAL3,CVAL1,IVAL2,URSPST)
C
            SGRP = 44
            CALL Q1INIT (MESSFL,SCLU,SGRP)
C
C           allow previous
            I = 4
            CALL ZSTCMA (I,I1)
C
C           set met segment for this reach
            CNUM = 3
C           find which pcp name
            DO 63 I = 1,5
              IF (PCPDSN(I).EQ.METSEG(CVAL1(1),1)) THEN
C               this is the name we want
                CVAL1(2) = I + 1
              END IF
 63         CONTINUE
C           find which evp name
            DO 64 I = 1,5
              IF (EVPDSN(I).EQ.METSEG(CVAL1(1),2)) THEN
C               this is the name we want
                CVAL1(3) = I + 1
              END IF
 64         CONTINUE
            IF (METSEG(CVAL1(1),1).LT.1) THEN
C             no pcp segment for this reach, use 'none'
              CVAL1(2) = 1
            END IF
            IF (METSEG(CVAL1(1),2).LT.1) THEN
C             no evp segment for this reach, use 'none'
              CVAL1(3) = 1
            END IF
            CALL QSETCO (CNUM,CVAL1)
C
C           set reach characteristics
            RNUM = 4
            RVAL(1) = RCHSLO(CVAL1(1))
            RVAL(2) = RCHLEN(CVAL1(1))
            RVAL(3) = 0.0
            RVAL(4) = RCHINI(CVAL1(1))
            CALL QSETR (RNUM,RVAL)
C
C           set ftable options and prec/evap met segment options
            OPCNT = 2
            OPLEN = 3
            MXSEL(1) = 1
            MNSEL(1) = 1
            MXSEL(2) = 2
            MNSEL(2) = 0
            IF (METSEG(CVAL1(1),3).EQ.1 .OR.
     1          METSEG(CVAL1(1),3).EQ.3) THEN
C             precip considered, set option on
              OPVAL(2) = 1
            ELSE
              OPVAL(2) = 0
            END IF
            IF (METSEG(CVAL1(1),3).EQ.2 .OR.
     1          METSEG(CVAL1(1),3).EQ.3) THEN
C             evap considered, set option on
              IF (OPVAL(2).EQ.0) THEN
                OPVAL(2) = 2
              ELSE
                OPVAL(3) = 2
              END IF
            ELSE
              OPVAL(3) = 0
            END IF
            CALL QSETOP (OPCNT,OPLEN,MXSEL,MNSEL,OPVAL)
C
            PREVFG = 0
            CALL Q1EDIT (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              CALL QGETCO (CNUM,CVAL1)
              CALL QGETR (RNUM,RVAL)
C             get reach length and initial volume
              RCHSLO(CVAL1(1)) = RVAL(1)
              RCHLEN(CVAL1(1)) = RVAL(2)
              RCHINI(CVAL1(1)) = RVAL(4)
              CALL QGETOP (OPLEN,OPVAL)
C             get associated pcp segment
              IF (CVAL1(2).EQ.1) THEN
C               no associated segment
                METSEG(CVAL1(1),1) = 0
              ELSE
C               have an associated segment
                METSEG(CVAL1(1),1) = PCPDSN(CVAL1(2)-1)
              END IF
C             get associated evp segment
              IF (CVAL1(3).EQ.1) THEN
C               no associated segment
                METSEG(CVAL1(1),2) = 0
              ELSE
C               have an associated segment
                METSEG(CVAL1(1),2) = EVPDSN(CVAL1(3)-1)
              END IF
              METSEG(CVAL1(1),3) = -99
              IF (METSEG(CVAL1(1),1).GT.0 .AND. OPVAL(2).EQ.1) THEN
C               prcp segment has been specified and wanted
                METSEG(CVAL1(1),3) = 1
              END IF
              IF (METSEG(CVAL1(1),2).GT.0) THEN
C               evap segment has been specified
                IF (OPVAL(2).EQ.2 .OR. OPVAL(3).EQ.2) THEN
C                 evap wanted
                  IF (METSEG(CVAL1(1),3).EQ.1) THEN
C                   precip wanted too, so flag for both
                    METSEG(CVAL1(1),3) = 3
                  ELSE
C                   evap only
                    METSEG(CVAL1(1),3) = 2
                  END IF
                END IF
              END IF
C
C             do ftable data
              IF (OPVAL(1).EQ.1) THEN
C               user wants to enter ftable by xy pairs
                SGRP = 55
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                CALL ZGTRET (IRET)
                IF (IRET.EQ.2) THEN
C                 user wants previous back
                  PREVFG = 1
                ELSE
                  PREVFG = 0
                END IF
              ELSE IF (OPVAL(1).EQ.2) THEN
C               user wants to enter ftable by trapezoid
                SGRP = 56
                CALL Q1INIT (MESSFL,SCLU,SGRP)
C               set trapezoid characteristics
                RNUM = 5
                CALL ZIPR (RNUM,R0,RVAL)
                CALL QSETR (RNUM,RVAL)
                CALL Q1EDIT (IRET)
                IF (IRET.NE.2) THEN
C                 user wants to continue, get trapezoid values
                  CALL QGETR (RNUM,RVAL)
C                 actual generation of ftable not yet available
                  SGRP = 57
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                  CALL ZGTRET (IRET)
                  IF (IRET.EQ.2) THEN
C                   user wants previous
                    PREVFG = 1
                  ELSE
                    PREVFG = 0
                  END IF
                ELSE
C                 user wants previous
                  PREVFG = 1
                END IF
              ELSE IF (OPVAL(1).EQ.3) THEN
C               user wants to read ftable from file
                SGRP = 62
                CALL Q1INIT (MESSFL,SCLU,SGRP)
                CALL Q1EDIT (IRET)
                IF (IRET.EQ.2) THEN
C                 user wants previous back to first screen
                  PREVFG = 1
                ELSE
C                 get unit number for ftable file
                  CALL QGETF (I1,FTUNIT)
                  PREVFG = 0
C                 user wants ftable from file
                  CALL HADDFF (CVAL1(1),FTUNIT(1),
     O                         FTRET)
                  IF (FTRET.NE.0) THEN
C                   problem reading ftable from file, tell user
                    SGRP = 58
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                    PREVFG = 1
                  ELSE
C                   read in okay, delete old version
                    J = 2
                    CALL HDELFT (CVAL1(1),J)
                  END IF
                END IF
              END IF
C
              IF (PREVFG.EQ.0) THEN
C               user wants to continue
C               do second screen of parameters
                CHEAD = ' Reach               Acres of each l'//
     1             'and use type (up to 12)                   '
                CALL CVARAR (I78,CHEAD,I78,CHEAD1)
C               insert reach name into header
                CALL CVARAR (I8,RCHNAM(CVAL1(1)),I8,CHEAD1(8))
                CALL ZBLDWR (I78,CHEAD1,I1,IM1,I)
C               figure out how many land use types and build header
                CALL ZIPC (I78,BLNK,CHEAD1)
                IPOS = 4
                NLAND= 0
                I = 12
                CALL ZIPI (I,I0,NOIN)
                CALL ZIPR (I,R0,RVALX)
                DO 80 I = 1,SMXRCH
C                 loop through each perlnd for land use types
                  IF (LANDUS(I).NE.'                    ') THEN
C                   name entered
                    IF (I.GT.1) THEN
C                     see if name same as previous name
                      IF (LANDUS(I).NE.LANDUS(I-1)) THEN
C                       need to put this name in header
                        CALL CVARAR (I20,LANDUS(I),I20,LANDU1)
                        IPOS = IPOS + 2
                        CHEAD1(IPOS) = LANDU1(1)
                        IPOS = IPOS + 1
                        CHEAD1(IPOS) = LANDU1(2)
                        IPOS = IPOS + 1
                        CHEAD1(IPOS) = LANDU1(3)
                        IPOS = IPOS + 1
                        CHEAD1(IPOS) = LANDU1(4)
C                       put data for this perlnd/reach into screen
                        NLAND = NLAND + 1
                        RVALX(NLAND,1) = LANDAC(I,CVAL1(1))
C                       keep track of how many perlnds in each land use class
                        NOIN(IPOS-8-((NLAND-1)*4)) = 1
                      ELSE
C                       already have name in header,
C                       put data for this perlnd/reach into screen
                        RVALX(NLAND,1)=LANDAC(I,CVAL1(1))+
     1                        RVALX(NLAND,1)
C                       increment count of number of perlnds in this class
                        NOIN(IPOS-8-((NLAND-1)*4)) =
     1                        NOIN(IPOS-8-((NLAND-1)*4))+1
                      END IF
                    ELSE
C                     put name in header
                      CALL CVARAR (I20,LANDUS(I),I20,LANDU1)
                      IPOS = IPOS + 2
                      CHEAD1(IPOS) = LANDU1(1)
                      IPOS = IPOS + 1
                      CHEAD1(IPOS) = LANDU1(2)
                      IPOS = IPOS + 1
                      CHEAD1(IPOS) = LANDU1(3)
                      IPOS = IPOS + 1
                      CHEAD1(IPOS) = LANDU1(4)
C                     put data for this perlnd/reach into screen
                      NLAND = NLAND + 1
                      RVALX(NLAND,1) = LANDAC(I,CVAL1(1))
                      NOIN(1) = 1
                    END IF
                  END IF
 80             CONTINUE
C               put implnd name into header
                IPOS = IPOS + 3
                CHEAD1(IPOS) = 'I'
                IPOS = IPOS + 1
                CHEAD1(IPOS) = 'M'
                IPOS = IPOS + 1
                CHEAD1(IPOS) = 'P'
                IPOS = IPOS + 1
                CHEAD1(IPOS) = ' '
C               put implnd data into buffer
                DO 90 I = 1,20
                  RVALX(NLAND+1,1) = IMPACR(I,CVAL1(1)) +
     1                  RVALX(NLAND+1,1)
 90             CONTINUE
                CALL ZBLDWR (I78,CHEAD1,I0,IM1,I)
                CALL ZMNSST
C
C               fill tbuff with upstream and downstream reaches
                I = 150
                CALL ZIPC (I,BLNK,TBUFFX)
C               look through reach network array to see if any reaches
C               are upstream of this one
                K = 49
                DO 100 J = 1, NUMNET
                  IF (RCHNET(J,2).EQ.CVAL1(1)) THEN
C                   found an upstream reach
                    CALL CVARAR (I8,RCHNAM(RCHNET(J,1)),I8,
     O                           TBUFFX(K,1))
                    K = K + 8
                  END IF
 100            CONTINUE
                K = 105
                DO 110 J = 1, NUMNET
                  IF (RCHNET(J,1).EQ.CVAL1(1)) THEN
C                   found a downstream reach
                    CALL CVARAR (I8,RCHNAM(RCHNET(J,2)),I8,
     O                           TBUFFX(K,1))
                    K = K + 8
                  END IF
 110            CONTINUE
C
                I = 39
                CALL ZIPI (I,I0,CVALX)
C               find out if analysis is wanted for this reach
                IF (RCHTMP(CVAL1(1)).EQ.0) THEN
C                 no analysis wanted
                  CVALX(13,1,1) = 1
                ELSE IF (RCHTMP(CVAL1(1)).GT.0) THEN
C                 analysis wanted
                  CVALX(13,1,1) = 2
                END IF
C
                INUM = 1
                RNUM = 12
                CNUM = 13
                DNUM = 1
                TLEN = 150
                IVALX(1,1) = 0
                DVALX(1,1) = 0
C               need to make sure number fits in field
                DO 115 J = 1,12
                  IF (RVALX(J,1).GT.999.0) THEN
C                   convert to char string
                    CALL DECCHX (RVALX(J,1),I5,I3,I0,CTEMP)
C                   convert back to real
                    RVALX(J,1) = CHRDEC (I4,CTEMP)
                  END IF
 115            CONTINUE
C               save values of land areas for later checking
                I = 12
                CALL COPYR (I,RVALX,RVALSV)
C
                SGRP = 81
                CALL QRESCZ (MESSFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,
     I                       I1,I0,TLEN,
     M                       IVALX,RVALX,DVALX,CVALX,TBUFFX)
C
                CALL ZGTRET (IRET)
                PREVFG = 0
                IF (IRET.EQ.1) THEN
C                 user wants to continue
C                 process land acreages
                  PERFST = 1
                  DO 120 J = 1,12
                    IF ((ABS(RVALX(J,1)-RVALSV(J,1)).GT.0.999.AND.
     1                  RVALX(J,1).GT.99.999) .OR.
     2                 (ABS(RVALX(J,1)-RVALSV(J,1)).GT.0.0999.AND.
     3                  RVALX(J,1).LT.99.999)) THEN
C                     value has changed
C                     figure out which landac values reflect this one
                      PERLST = PERFST+NOIN(J)-1
C                     sum of landac(i,perfst)..(i,perlst) must equal table val
                      IF (ABS(RVALSV(J,1)).GT.1.0E-5) THEN
C                       not a zero value to start
                        RFACT = RVALX(J,1)/RVALSV(J,1)
C                       scale values of landac accordingly
                        DO 130 K= PERFST,PERLST
                          LANDAC(K,CVAL1(1)) =
     1                         LANDAC(K,CVAL1(1))*RFACT
 130                    CONTINUE
                      ELSE
C                       came in with a zero,
C                       factor acreage equally over all perlnds for this lu
                        DO 140 K= PERFST,PERLST
                          LANDAC(K,CVAL1(1)) =
     1                         RVALX(J,1)/(PERLST-PERFST+1)
 140                    CONTINUE
                      END IF
                    END IF
C                   keep track of which perlnd for next time through
                    PERFST = PERFST+NOIN(J)
 120              CONTINUE
C
C                 check for changes in impervious areas
                  I = CVAL1(1)
                  J = NLAND+1
                  IF ((ABS(RVALX(J,1)-RVALSV(J,1)).GT.0.999 .AND.
     1                RVALX(J,1).GT.99.999) .OR.
     2                (ABS(RVALX(J,1)-RVALSV(J,1)).GT.0.0999 .AND.
     3                RVALX(J,1).LT.99.999)) THEN
C                   value has changed
C                   figure out which impacr values reflect this one
C                   sum of impacr(i, ) must equal table val
                    IF (ABS(RVALSV(J,1)).GT.1.0E-5) THEN
C                     not a zero value to start
                      RFACT = RVALX(J,1)/RVALSV(J,1)
C                     scale values of impacr accordingly
                      DO 150 K= 1,NIMP
                        IMPACR(K,I) = IMPACR(K,I)*RFACT
 150                  CONTINUE
                    ELSE
C                     came in with a zero,
C                     factor acreage equally over all implnds for this lu
                      DO 160 K= 1,NIMP
                        IMPACR(K,I) = RVALX(J,1)/(NIMP)
 160                  CONTINUE
                    END IF
                  END IF
C
C                 put analysis flag back
                  RCHTMP(CVAL1(1)) = CVALX(13,1,1)-1
C
C                 put upstream/downstream connections back
                  DO 170 J = 49,104,8
                    CALL CARVAR (I8,TBUFFX(J,1),I8,TMPNAM)
                    IF (TMPNAM.NE.' ') THEN
C                     a connection was entered, is it valid
                      DO 180 K = 1,NRCH
                        IF (TMPNAM.EQ.RCHNAM(K)) THEN
C                         valid upstream connection,
C                         is it already there?
                          FOUND = 0
                          DO 190 I = 1,NUMNET
                            IF (RCHNET(I,1).EQ.K .AND.
     1                          RCHNET(I,2).EQ.CVAL1(1)) THEN
                              FOUND = 1
                            END IF
 190                      CONTINUE
                          IF (FOUND.EQ.0) THEN
C                           need to add it
                            NUMNET = NUMNET + 1
                            RCHNET(NUMNET,1) = K
                            RCHNET(NUMNET,2) = CVAL1(1)
                          END IF
                        END IF
 180                  CONTINUE
                    END IF
 170              CONTINUE
C                 now look at downstream connections
                  DO 200 J = 105,144,8
                    CALL CARVAR (I8,TBUFFX(J,1),I8,TMPNAM)
                    IF (TMPNAM.NE.' ') THEN
C                     a connection was entered, is it valid
                      DO 210 K = 1,NRCH
                        IF (TMPNAM.EQ.RCHNAM(K)) THEN
C                         valid downstream connection,
C                         is it already there?
                          FOUND = 0
                          DO 220 I = 1,NUMNET
                            IF (RCHNET(I,2).EQ.K .AND.
     1                          RCHNET(I,1).EQ.CVAL1(1)) THEN
                              FOUND = 1
                            END IF
 220                      CONTINUE
                          IF (FOUND.EQ.0) THEN
C                           need to add it
                            NUMNET = NUMNET + 1
                            RCHNET(NUMNET,2) = K
                            RCHNET(NUMNET,1) = CVAL1(1)
                          END IF
                        END IF
 210                  CONTINUE
                    END IF
 200              CONTINUE
C                 need to remove unwanted connections,
C                 is every connection involving this reach desired?
                  DO 230 I = 1,NUMNET
C                   see if upstream conns are desired
                    IF (RCHNET(I,2).EQ.CVAL1(1)) THEN
C                     look for upstream reach in buffer
                      K = 56
                      IF (STRFND(K,TBUFFX(49,1),I8,
     1                  RCHNAM(RCHNET(I,1))).GT.0) THEN
C                       yes, really want it
                      ELSE
C                       no, get rid of it
                        NUMNET = NUMNET-1
                        DO 240 J = I,NUMNET
                          RCHNET(J,1) = RCHNET(J+1,1)
                          RCHNET(J,2) = RCHNET(J+1,2)
 240                    CONTINUE
                      END IF
                    END IF
 230              CONTINUE
                  DO 250 I = 1,NUMNET
C                   see if downstream conns are desired
                    IF (RCHNET(I,1).EQ.CVAL1(1)) THEN
C                     look for downstream reach in buffer
                      K = 40
                      IF (STRFND(K,TBUFFX(105,1),I8,
     1                  RCHNAM(RCHNET(I,2))).GT.0) THEN
C                       yes, really want it
                      ELSE
C                       no, get rid of it
                        NUMNET = NUMNET-1
                        DO 260 J = I,NUMNET
                          RCHNET(J,1) = RCHNET(J+1,1)
                          RCHNET(J,2) = RCHNET(J+1,2)
 260                    CONTINUE
                      END IF
                    END IF
 250              CONTINUE
C
                ELSE IF (IRET.EQ.2) THEN
C                 user wants previous back to first screen
                  PREVFG = 1
                END IF
              END IF
            ELSE
C             user wants to return to first screen
              PREVFG = 2
            END IF
          IF (PREVFG.EQ.1) GO TO 30
        END IF
      IF (PREVFG.EQ.2) GO TO 10
C
C     turn previous off
      I = 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE    GETHSP
     I                    (SMXRCH,
     O                     DATES,NRCH,RCHCHR,UCIDES,RCHLEN,METSEG,
     O                     NUMNET,RCHNET,LANDAC,LANDUS,ANAWDM,RCHINI,
     O                     IMPACR,NIMP,IMPLID,RCHRID,
     O                     NPER,PERLID,RCHSLO)
C
C     + + + PURPOSE + + +
C     get hspf data from data structure for scenario generator
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      DATES(6),NRCH,SMXRCH,
     1             METSEG(SMXRCH,3),NUMNET,RCHNET(7*SMXRCH,2),
     2             ANAWDM(SMXRCH),NIMP,RCHRID(SMXRCH),IMPLID(SMXRCH),
     3             NPER,PERLID(SMXRCH)
      REAL         RCHLEN(SMXRCH),LANDAC(50,SMXRCH),RCHINI(SMXRCH),
     1             IMPACR(20,SMXRCH),RCHSLO(SMXRCH)
      CHARACTER*8  RCHCHR(SMXRCH)
      CHARACTER*20 LANDUS(SMXRCH)
      CHARACTER*78 UCIDES
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SMXRCH - maximum number of reaches
C     DATES  - array of starting/ending dates
C     NRCH   - number of reaches
C     RCHCHR - reach names
C     UCIDES - uci file description
C     RCHLEN - length of each reach
C     METSEG - associated met segment for each reach
C     NUMNET - number of reach to reach network entries
C     RCHNET - reach to reach source/target pairs
C     LANDAC - number of acres from each perlnd to each reach
C     LANDUS - titles of each type of land use
C     ANAWDM - wdm data set number for each reach
C     RCHINI - initial volume in each reach
C     IMPACR - number of acres from each implnd to each reach
C     NIMP   - number of impervious land segments
C     RCHRID - ids of each reach
C     IMPLID - ids of each implnd
C     NPER   - number of perlnds
C     PERLID - ids of each perlnd
C     RCHSLO - slope of each reach
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,IVAL(700),IDNO,IDUM,I1,IFLG,IPOS,TRCH,TPER,
     1             FOUND,TIMP
      REAL         RVAL(50),R0
      CHARACTER*1  CSTR1(80),RCHNM1(8)
      CHARACTER*8  ITMNAM
      CHARACTER*80 CTXT
C
C     + + + FUNCTIONS + + +
      INTEGER      CHRINT
      REAL         CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL     HGETI,HGETIA,HGETC,HGETCA,HGETRA,ZIPI,CVARAR,CHRINT
      EXTERNAL     ASRTI,CHRDEC,ZIPR,QUPCAS,CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      R0 = 0.0
C
C     get uci file description from global block
      ITMNAM = 'UCIDESCR'
      IDNO   = 0
      CALL HGETC (ITMNAM,IDNO,
     O            UCIDES)
C
C     get start and end dates from global block
      ITMNAM = 'SEDATES '
      IDNO   = 0
      CALL HGETC (ITMNAM,IDNO,
     O            CTXT)
      I = 30
      J = 5
      CALL CVARAR (I,CTXT,I,CSTR1)
      IPOS = -4
      DO 5 I = 1,6
        IPOS = IPOS + 5
        DATES(I) = CHRINT(J,CSTR1(IPOS))
 5    CONTINUE
C
C     get rchres ids
      ITMNAM = 'RCHID   '
      IDNO   = 0
      NRCH   = 0
 10   CONTINUE
        CALL HGETI (ITMNAM,IDNO,
     O              IVAL(1))
        IF (IVAL(1).NE.-99) THEN
C         found an operation of this type
          NRCH = NRCH + 1
          RCHRID(NRCH) = IVAL(1)
        END IF
      IF (IVAL(1).NE.-99) GO TO 10
C     sort rchres ids
      CALL ASRTI (I1,NRCH,
     M            RCHRID,
     O            IVAL)
C
C     get perlnd ids
      ITMNAM = 'PERID   '
      IDNO   = 0
      NPER   = 0
 15   CONTINUE
        CALL HGETI (ITMNAM,IDNO,
     O              IVAL(1))
        IF (IVAL(1).NE.-99) THEN
C         found an operation of this type
          NPER = NPER + 1
          PERLID(NPER) = IVAL(1)
        END IF
      IF (IVAL(1).NE.-99) GO TO 15
C     sort perlnd ids
      CALL ASRTI (I1,NPER,
     M            PERLID,
     O            IVAL)
C
C     get implnd ids
      ITMNAM = 'IMPID   '
      IDNO   = 0
      NIMP   = 0
 20   CONTINUE
        CALL HGETI (ITMNAM,IDNO,
     O              IVAL(1))
        IF (IVAL(1).NE.-99) THEN
C         found an operation of this type
          NIMP = NIMP + 1
          IMPLID(NIMP) = IVAL(1)
        END IF
      IF (IVAL(1).NE.-99) GO TO 20
C     sort implnd ids
      CALL ASRTI (I1,NIMP,
     M            IMPLID,
     O            IVAL)
C
C     get rchres names
      ITMNAM = 'RCHNAMES'
      CALL HGETCA (ITMNAM,NRCH,RCHRID,
     O             RCHCHR)
      J = 8
      DO 25 I = 1,NRCH
        CALL CVARAR (J,RCHCHR(I),J,RCHNM1)
        CALL QUPCAS (J,RCHNM1)
        CALL CARVAR (J,RCHNM1,J,RCHCHR(I))
 25   CONTINUE
C
C     get rchres lengths
      ITMNAM = 'RCHLEN  '
      CALL HGETRA (ITMNAM,NRCH,RCHRID,IDUM,
     O             RCHLEN)
C
C     get rchres slopes
      ITMNAM = 'RCHSLO  '
      CALL HGETRA (ITMNAM,NRCH,RCHRID,IDUM,
     O             RVAL)
C     convert drop to slope
      DO 32 I = 1,NRCH
        IF (RCHLEN(I).GT.1.0E-6) THEN
C         reach length is greater than zero
          RCHSLO(I) = RVAL(I)/RCHLEN(I)
        ELSE
          RCHSLO(I) = 0.0
        END IF
 32   CONTINUE
C
C     get rchres initial volumes
      ITMNAM = 'RCHVOL  '
      CALL HGETRA (ITMNAM,NRCH,RCHRID,IDUM,
     O             RCHINI)
C
C     get perlnd land use types
      ITMNAM = 'PERLUTYP'
      CALL HGETCA (ITMNAM,NPER,PERLID,
     O             LANDUS)
      IF (NPER.LT.SMXRCH) THEN
C       blank out rest of names
        DO 48 I = NPER+1,SMXRCH
          LANDUS(I) = '                    '
 48     CONTINUE
      END IF
C
C     get output wdm data set numbers from ext-targets block
      ITMNAM = 'ANAWDM  '
      CALL HGETIA (ITMNAM,NRCH,RCHRID,
     O             ANAWDM)
C
C     for each perlnd, fill array of land acreage to each reach
      ITMNAM = 'LANDAC  '
      CALL ZIPR (50*SMXRCH,R0,LANDAC)
 50   CONTINUE
        CALL HGETC (ITMNAM,IDNO,
     O              CTXT)
        IF (CTXT(1:5).NE.'  -99') THEN
C         found a perlnd to rchres record, get rchres, perlnd, rval
          I = 20
          CALL CVARAR (I,CTXT,I,CSTR1)
          J = 5
          TRCH = CHRINT(J,CSTR1(1))
C         convert trch to reach sequence number
          FOUND = 0
          I = 0
 55       CONTINUE
            I = I + 1
            IF (RCHRID(I).EQ.TRCH) THEN
              TRCH = I
              FOUND= 1
            END IF
          IF (FOUND.EQ.0) GO TO 55
          TPER = CHRINT(J,CSTR1(6))
C         convert tper to perlnd sequence number
          FOUND = 0
          I = 0
 56       CONTINUE
            I = I + 1
            IF (PERLID(I).EQ.TPER) THEN
              TPER = I
              FOUND= 1
            END IF
          IF (FOUND.EQ.0) GO TO 56
          J = 10
          RVAL(1) = CHRDEC(J,CTXT(11:20))
          LANDAC(TPER,TRCH) = LANDAC(TPER,TRCH) + RVAL(1)
        END IF
      IF (CTXT(1:5).NE.'  -99') GO TO 50
C
C     for each implnd, fill array of land acreage to each reach
      ITMNAM = 'IMPACR  '
      CALL ZIPR (20*SMXRCH,R0,IMPACR)
 60   CONTINUE
        CALL HGETC (ITMNAM,IDNO,
     O              CTXT)
        IF (CTXT(1:5).NE.'  -99') THEN
C         found a implnd to rchres record, get rchres, implnd, rval
          I = 20
          CALL CVARAR (I,CTXT,I,CSTR1)
          J = 5
          TRCH = CHRINT(J,CSTR1(1))
C         convert trch to reach sequence number
          FOUND = 0
          I = 0
 65       CONTINUE
            I = I + 1
            IF (RCHRID(I).EQ.TRCH) THEN
              TRCH = I
              FOUND= 1
            END IF
          IF (FOUND.EQ.0) GO TO 65
          TIMP = CHRINT(J,CSTR1(6))
C         convert tper to implnd sequence number
          FOUND = 0
          I = 0
 66       CONTINUE
            I = I + 1
            IF (IMPLID(I).EQ.TIMP) THEN
              TIMP = I
              FOUND= 1
            END IF
          IF (FOUND.EQ.0) GO TO 66
          J = 10
          RVAL(1) = CHRDEC(J,CTXT(11:20))
          IMPACR(TIMP,TRCH) = IMPACR(TIMP,TRCH) + RVAL(1)
        END IF
      IF (CTXT(1:5).NE.'  -99') GO TO 60
C
C     find number of reach to reach connections
      ITMNAM = 'NUMR2R  '
      IDNO   = 0
      CALL HGETI (ITMNAM,IDNO,
     O            NUMNET)
C
      IF (NUMNET.GT.0) THEN
C       find source of reach to reach connections
        ITMNAM = 'R2RSRC  '
        CALL ZIPI (NUMNET,I1,IVAL)
        CALL HGETIA (ITMNAM,NUMNET,IVAL,
     O               RCHNET(1,1))
C       need to convert reach ids to reach sequence numbers
        DO 30 I = 1,NUMNET
          J   = 0
          IFLG= 0
 35       CONTINUE
            J = J + 1
            IF (RCHNET(I,1).EQ.RCHRID(J)) THEN
C             found corresponding sequence number
              RCHNET(I,1) = J
              IFLG = 1
            END IF
          IF (IFLG.EQ.0) GO TO 35
 30     CONTINUE
C
C       find target of reach to reach connections
        ITMNAM = 'R2RTAR  '
        IDNO   = 1
        CALL HGETIA (ITMNAM,NUMNET,IVAL,
     O               RCHNET(1,2))
C       need to convert reach ids to reach sequence numbers
        DO 40 I = 1,NUMNET
          J   = 0
          IFLG= 0
 45       CONTINUE
            J = J + 1
            IF (RCHNET(I,2).EQ.RCHRID(J)) THEN
C             found corresponding sequence number
              RCHNET(I,2) = J
              IFLG = 1
            END IF
          IF (IFLG.EQ.0) GO TO 45
 40     CONTINUE
      END IF
C
C     find out if prec or evap considered as input to each reach
      ITMNAM = 'PRECEVAP'
      CALL HGETIA (ITMNAM,NRCH,RCHRID,
     O             METSEG(1,3))
C
C     find precip dsn assoc with each reach
      ITMNAM = 'PREC4RCH'
      CALL HGETIA (ITMNAM,NRCH,RCHRID,
     O             METSEG(1,1))
C
C     find evap dsn assoc with each reach
      ITMNAM = 'EVAP4RCH'
      CALL HGETIA (ITMNAM,NRCH,RCHRID,
     O             METSEG(1,2))
C
      RETURN
      END
C
C
C
      SUBROUTINE    GETMET
     O                    (PCPNAM,EVPNAM,PCPDSN,EVPDSN)
C
C     + + + PURPOSE + + +
C     get precip and evap dsns and names from time series directory
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      PCPDSN(5),EVPDSN(5)
      CHARACTER*8  PCPNAM(5),EVPNAM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PCPDSN - precip data set numbers
C     EVPDSN - evap data set numbers
C     PCPNAM - precip names
C     EVPNAM - evap names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      LDSN,ITMP,TU,TS,SDATE(6),EDATE(6),GRPSIZ,I
      CHARACTER*8  LCON(2),LSEN,LLOC
C
C     + + + EXTERNALS + + +
      EXTERNAL     TSESPC,TSDSM,TSDSPC
C
C     + + + END SPECIFICATIONS + + +
C
      LCON(1)= 'PRCP    '
      LCON(2)= 'EVAP    '
      LSEN   = 'OBSERVED'
      LLOC   = '        '
      DO 5 I = 1,5
        PCPDSN(I) = 0
        EVPDSN(I) = 0
        PCPNAM(I) = ' '
        EVPNAM(I) = ' '
 5    CONTINUE
C
C     set search criteria
      CALL TSESPC (LSEN,LLOC,LCON(1))
C     look for matching data sets
      LDSN = 0
      ITMP = 0
 10   CONTINUE
        LDSN = LDSN + 1
        CALL TSDSM
     M             (LDSN)
        IF (LDSN.GT.0) THEN
C         found a precip data set
          ITMP = ITMP + 1
          PCPDSN(ITMP) = LDSN
          CALL TSDSPC (LDSN,
     O                 LSEN,PCPNAM(ITMP),LCON(1),
     O                 TU,TS,SDATE,EDATE,GRPSIZ)
        END IF
      IF (ITMP.LT.5 .AND. LDSN.NE.0) GO TO 10
C
C     set search criteria
      CALL TSESPC (LSEN,LLOC,LCON(2))
C     look for matching data sets
      LDSN = 0
      ITMP = 0
 20   CONTINUE
        LDSN = LDSN + 1
        CALL TSDSM
     M             (LDSN)
        IF (LDSN.GT.0) THEN
C         found an evap data set
          ITMP = ITMP + 1
          EVPDSN(ITMP) = LDSN
          CALL TSDSPC (LDSN,
     O                 LSEN,EVPNAM(ITMP),LCON(1),
     O                 TU,TS,SDATE,EDATE,GRPSIZ)
        END IF
      IF (ITMP.LT.5 .AND. LDSN.NE.0) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE    PUTHSP
     I                    (SMXRCH,
     I                     DATES,NRCH,RCHCHR,UCIDES,RCHLEN,METSEG,
     I                     NUMNET,RCHNET,LANDAC,ANAWDM,RCHINI,
     I                     IMPACR,NIMP,IMPLID,RCHRID,
     I                     NPER,PERLID,RCHSLO)
C
C     + + + PURPOSE + + +
C     put hspf data from scenario generator
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      DATES(6),NRCH,SMXRCH,
     1             METSEG(SMXRCH,3),NUMNET,RCHNET(7*SMXRCH,2),
     2             ANAWDM(SMXRCH),NIMP,RCHRID(SMXRCH),IMPLID(SMXRCH),
     3             NPER,PERLID(SMXRCH)
      REAL         RCHLEN(SMXRCH),LANDAC(50,SMXRCH),RCHINI(SMXRCH),
     1             IMPACR(20,SMXRCH),RCHSLO(SMXRCH)
      CHARACTER*8  RCHCHR(SMXRCH)
      CHARACTER*78 UCIDES
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SMXRCH - maximum number of reaches
C     DATES  - array of starting/ending dates
C     NRCH   - number of reaches
C     RCHCHR - reach names
C     UCIDES - uci file description
C     RCHLEN - length of each reach
C     METSEG - associated met segment for each reach
C     NUMNET - number of reach to reach network entries
C     RCHNET - reach to reach source/target pairs
C     LANDAC - number of acres from each perlnd to each reach
C     ANAWDM - wdm data set number for each reach
C     RCHINI - initial volume in each reach
C     IMPACR - number of acres from each implnd to each reach
C     NIMP   - number of impervious land segments
C     RCHRID - ids of each reach
C     IMPLID - ids of each implnd
C     NPER   - number of perlnds
C     PERLID - ids of each perlnd
C     RCHSLO - slope of each reach
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I0,IDNO,IDUM,IPOS,TMPSEG(50,2),
     1             K,IM1,ITMP
      REAL         RVAL(50)
      CHARACTER*1  CSTR1(80)
      CHARACTER*8  ITMNAM
      CHARACTER*80 CTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     HPUTC,INTCHR,CARVAR,HPUTCA,HPUTRA,HPUT,HPUTIA
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IM1= -1
C
C     put uci file description to global block
      ITMNAM = 'UCIDESCR'
      IDNO   = 0
      CALL HPUTC (ITMNAM,IDNO,UCIDES)
C
C     put start and end dates to global block
      J = 5
      IPOS = -4
      DO 5 I = 1,6
        IPOS = IPOS + 5
        CALL INTCHR(DATES(I),J,I0,K,CSTR1(IPOS))
 5    CONTINUE
      I = 30
      CALL CARVAR (I,CSTR1,I,CTXT)
      ITMNAM = 'SEDATES '
      IDNO   = 0
      CALL HPUTC (ITMNAM,IDNO,CTXT)
C
C     put rchres names
      ITMNAM = 'RCHNAMES'
      CALL HPUTCA (ITMNAM,NRCH,RCHRID,RCHCHR)
C
C     put rchres lengths
      ITMNAM = 'RCHLEN  '
      CALL HPUTRA (ITMNAM,NRCH,RCHRID,IDUM,RCHLEN)
C
C     put rchres slopes
      ITMNAM = 'RCHSLO  '
C     convert slope to drop
      DO 10 I = 1,NRCH
        RVAL(I) = RCHSLO(I)*RCHLEN(I)
 10   CONTINUE
      CALL HPUTRA (ITMNAM,NRCH,RCHRID,IDUM,RVAL)
C
C     put rchres initial volumes
      ITMNAM = 'RCHVOL  '
      CALL HPUTRA (ITMNAM,NRCH,RCHRID,IDUM,RCHINI)
C
C     put output wdm data set numbers to ext-targets block
      ITMNAM = 'ANAWDM  '
C     delete all current reach to wdm records
      CALL HPUT (ITMNAM,IDUM,IDUM,IM1,CTXT)
C     add a new reach to wdm record for each
      CALL HPUTIA (ITMNAM,NRCH,RCHRID,ANAWDM)
C
C     put array of land acreages to each reach
      ITMNAM = 'LANDAC  '
C     delete all current perlnd to reach records
      CALL HPUT (ITMNAM,IDUM,IDUM,IM1,CTXT)
C     for each perlnd, put array of land acreage to each reach
      DO 30 I = 1,NPER
        DO 35 J = 1,NRCH
          RVAL(J) = LANDAC(I,J)/12
 35     CONTINUE
        CALL HPUTRA (ITMNAM,NRCH,RCHRID,PERLID(I),RVAL)
 30   CONTINUE
C
C     put array of implnd acreages to each reach
      ITMNAM = 'IMPACR  '
C     delete all current implnd to reach records
      CALL HPUT (ITMNAM,IDUM,IDUM,IM1,CTXT)
C     for each implnd, fill array of acreage to each reach
      DO 40 I = 1,NIMP
        DO 45 J = 1,NRCH
          RVAL(J) = IMPACR(I,J)/12
 45     CONTINUE
        CALL HPUTRA (ITMNAM,NRCH,RCHRID,IMPLID(I),RVAL)
 40   CONTINUE
C
C     put network reach to reach connections
      ITMNAM = 'NUMR2R  '
      IDNO   = 0
C     delete all current reach to reach records
      CALL HPUT (ITMNAM,IDUM,IDUM,IM1,CTXT)
C     need to convert reach sequence numbers to reach ids
      DO 15 I = 1,NUMNET
        RCHNET(I,1) = RCHRID(RCHNET(I,1))
        RCHNET(I,2) = RCHRID(RCHNET(I,2))
 15   CONTINUE
C     add a new reach to reach record where desired
      ITMNAM = 'R2RSRC  '
      CALL HPUTIA (ITMNAM,NUMNET,RCHNET(1,2),RCHNET(1,1))
C
C     put ext-sources records for prec or evap into a reach
      ITMNAM = 'PRECEVAP'
C     delete all current wdm to reach records
      CALL HPUT (ITMNAM,IDUM,IDUM,IM1,CTXT)
C     build temp array of data set numbers and reach ids to put
      ITMP = 0
      DO 50 I = 1,NRCH
        IF (METSEG(I,3).EQ.1 .OR. METSEG(I,3).EQ.3) THEN
C         want a precip record here
          ITMP = ITMP + 1
          TMPSEG(ITMP,1) = METSEG(I,1)
          TMPSEG(ITMP,2) = RCHRID(I)
        END IF
 50   CONTINUE
      ITMNAM = 'WDMPREC '
C     add a new wdm to reach record where desired for precip
      CALL HPUTIA (ITMNAM,ITMP,TMPSEG(1,1),TMPSEG(1,2))
C     build temp array of data set numbers and reach ids to put
      ITMP = 0
      DO 60 I = 1,NRCH
        IF (METSEG(I,3).EQ.2 .OR. METSEG(I,3).EQ.3) THEN
C         want an evap record here
          ITMP = ITMP + 1
          TMPSEG(ITMP,1) = METSEG(I,2)
          TMPSEG(ITMP,2) = RCHRID(I)
        END IF
 60   CONTINUE
      ITMNAM = 'WDMEVAP '
C     add a new wdm to reach record where desired for evap
      CALL HPUTIA (ITMNAM,ITMP,TMPSEG(1,1),TMPSEG(1,2))
C
      RETURN
      END
