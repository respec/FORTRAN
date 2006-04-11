C
C
C
      SUBROUTINE   WDSAGN
     I                    (MESSFL,
     O                     SAIND,SANAM,DONFG)
C
C     + + + PURPOSE + + +
C     routine lists all the search attributes and gives details on
C     attributes that are requested. (37=HELP, 38=DONE)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SAIND,DONFG
      CHARACTER*1 SANAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SAIND  - search attribute index number
C     SANAM  - name of search attribute
C     DONFG  - done flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SATYP,SALEN
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDSAGX
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDSAGX (MESSFL,
     O             SAIND,SANAM,SATYP,SALEN,DONFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSAFN
     I                    (MESSFL,PRTFLG,
     M                     SANAM,
     O                     SAIND)
C
C     + + + PURPOSE + + +
C     determines if a user specified search attribute is valid, fills
C     in missing characters in name
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SAIND,PRTFLG
      CHARACTER*1 SANAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     PRTFLG - flag requesting that attributes be printed
C     SANAM  - name of search attribute
C     SAIND  - index number of attribute or
C              highest attribute number if printing
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,I6,I78,J,L,CONFLG,DONE,INIT,SCLU,SGRP,RETCOD
      CHARACTER*1 BLNK(1),COMMA,TBUFF(78),SAFNAM(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, ZIPC, CHRCHR, ZBLDWR, PMXCNW, WDSAFI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK,COMMA,I0,I1,I6,I78/' ',',',0,1,6,78/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 29
C
      IF (PRTFLG .EQ. 0) THEN
C       check given name
        IF (LENSTR(I6,SANAM) .EQ. 0) THEN
C         user responded with blanks- done
          SAIND= 38
        ELSE
C         name is not all blanks, what is it?
          CALL CHRCHR (I6,SANAM,SAFNAM)
C         initialize output buffer
          CALL ZIPC (I78,BLNK,TBUFF)
          INIT  = 1
          SAIND = 1
          DONE  = 0
          CONFLG= 1
 10       CONTINUE
            CALL WDSAFI (MESSFL,SAFNAM,
     M                   SAIND,
     O                   SANAM,RETCOD)
            IF (RETCOD .EQ. 0) THEN
C             unique name found
              DONE= 1
            ELSE IF (RETCOD .EQ. -112) THEN
C             multiple names match, put this one in the buffer
              L= LENSTR(I6,SANAM)
              CALL CHRCHR (L,SANAM,TBUFF(CONFLG))
              TBUFF(CONFLG+L)= COMMA
              CONFLG= CONFLG+ 8
              IF (CONFLG .GT. 70) THEN
C               output line of conflicting values
                IF (INIT .EQ. 1) THEN
C                 need header first time thru
                  SGRP= 20
                  CALL PMXCNW (MESSFL,SCLU,SGRP,I1,INIT,-I1,J)
                  INIT= 0
                END IF
                CALL ZBLDWR (CONFLG,TBUFF,INIT,-I1,L)
                CALL ZIPC(I78,BLNK,TBUFF)
                CONFLG= 1
              END IF
            ELSE
C             other retcod
              DONE= 1
            END IF
          IF (DONE.EQ.0) GO TO 10
C
          IF (CONFLG.GT.1 .OR. INIT.EQ.0) THEN
C           had some conflicts, check for final conflicting name
            L= LENSTR(I6,SANAM)
            IF (RETCOD.EQ.0 .AND. L.GT.0) THEN
C             last one found, clean return code, put in buffer
              CALL CHRCHR (L,SANAM,TBUFF(CONFLG))
              CONFLG= CONFLG+ L
            END IF
            IF (CONFLG.GT.1) THEN
C             had conflicts, flush rest of buffer and keep screen
              IF (INIT .EQ. 1) THEN
                SGRP= 20
                CALL PMXCNW (MESSFL,SCLU,SGRP,I1,INIT,-I1,J)
                INIT= 0
              END IF
C             remove trailing comma
 45           CONTINUE
                CONFLG= CONFLG- 1
              IF (TBUFF(CONFLG).EQ.BLNK(1) .OR.
     1            TBUFF(CONFLG).EQ.COMMA) GO TO 45
              CALL ZBLDWR (CONFLG,TBUFF,INIT,-I1,J)
              SAIND= 0
            ELSE IF (INIT.EQ.0) THEN
C             had conflicts, hold screen for user to view
              CALL ZBLDWR (I0,BLNK,INIT,-I1,J)
              SAIND= 0
            END IF
          END IF
        END IF
      ELSE
C       print valid attributes
        CALL ZIPC (I6,BLNK,SAFNAM)
        CALL ZIPC (I78,BLNK,TBUFF)
        I= 1
        SAIND= 0
        INIT = 1
 50     CONTINUE
          SAIND= SAIND+ 1
          CALL WDSAFI (MESSFL,SAFNAM,
     M                 SAIND,
     O                 SANAM,RETCOD)
          CALL CHRCHR(I6,SANAM,TBUFF(I))
          I= I+ LENSTR(I6,SANAM)
          IF (RETCOD.EQ.-112) THEN
C           more to come, add a comma
            TBUFF(I)= COMMA
            I= 9- MOD(I,8)+ I
          END IF
          IF (I.GE.70 .OR. RETCOD.EQ.-111) THEN
            I= I- 1
            CALL ZBLDWR (I,TBUFF,INIT,-I1,L)
            INIT= 0
            CALL ZIPC (I78,BLNK,TBUFF)
            I= 1
          END IF
        IF (RETCOD.EQ.-112) GO TO 50
C       keep screen for user to view
        CALL ZBLDWR (I0,BLNK,INIT,I0,L)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSAGX
     I                    (MESSFL,
     O                     SAIND,SANAM,SATYP,SALEN,DONFG)
C
C     + + + PURPOSE + + +
C     routine lists all the search attributes and more details on
C     attributes that are requested. (37=HELP, 38=DONE, 39=ALL)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SAIND,SATYP,SALEN,DONFG
      CHARACTER*1 SANAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SAIND  - search attribute index number
C     SANAM  - name of search attribute
C     SATYP  - type of attribute
C     SALEN  - length of attribute
C     DONFG  - done flag
C
C     + + + PARAMETERS + + +
      INTEGER     MLEN
      PARAMETER  (MLEN=224)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IA,IB,IC,I0,I1,J,J1,J2,K,FLG,SCLU,
     1            SAHLEN,SAHTYP,SAINDX,SGRP,ILEN,NUML,
     2            DPTR,DREC,DPOS,GLEN,TLEN,HLEN,CONT
      REAL        RTMP(2)
      CHARACTER*1 MBUFF(90),TBUFF(MLEN),ATT(6),BLNK(1)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, PRNTXT, QTSTR, WDSAFN, WDBSGX
      EXTERNAL    GETTXT, INTCHR, CHRDEL, WDSAGY, ZBLDWR
      EXTERNAL    CHRCHR, PMXTXR, ZIPC, PMXCNW, WMSGTE
      EXTERNAL    WADGRA, WADGVA, WADGHL, WADGDS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK,I0,I1/' ',0,1/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 29
      DONFG= 0
C
 10   CONTINUE
C       which attribute?
        I    = 6
        CALL ZIPC (I,BLNK,SANAM)
        SGRP = 1
        CALL QTSTR (MESSFL,SCLU,SGRP,I,
     O              SANAM)
        IF (LENSTR(I,SANAM).GT.0) THEN
C         attribute entered, get index, type, length
          CALL WDBSGX(MESSFL,SANAM,
     O                SAIND,SATYP,SALEN)
        ELSE
C         blank, all done
          SAIND= 38
        END IF
C
        IF (SAIND.EQ.38) THEN
C         user is done specifying attributes
          DONFG = 1
C
        ELSE IF (SAIND.EQ.0) THEN
C         user specified a bad attribute
          SGRP= 2
          CALL PRNTXT (MESSFL,SCLU,SGRP)
C
        ELSE IF (SAIND.EQ.37) THEN
C         need help
C         fill TBUFF with text from message file
          NUML= 90
          SGRP= 3
          CALL GETTXT (MESSFL,SCLU,SGRP,NUML,MBUFF)
          NUML= 78
C
          FLG = 0
 40       CONTINUE
C           which search attribute for details? (or done)
            SGRP= 4
            I   = 6
            CALL ZIPC (I,BLNK,ATT)
            CALL QTSTR (MESSFL,SCLU,SGRP,I,ATT)
            CALL WDSAFN(MESSFL,I0,
     M                  ATT,
     O                  SAINDX)
C
            IF (SAINDX .EQ. 0) THEN
C             attribute doesn't exist, try again
              SGRP = 2
              CALL PMXCNW (MESSFL,SCLU,SGRP,I1,-I1,I0,I)
            ELSE IF (SAINDX .EQ. 38) THEN
C             user is done with help
              FLG = 1
            ELSE
C             good attribute, find record and print help info
              IF (SAINDX .EQ. 39) THEN
C               wants All attributes, cant do right now
CPRH                J1 = 1
CPRH                J2 = NA
                SGRP= 6
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              ELSE
C               wants specific attribute
                J1 = SAINDX
                J2 = SAINDX
              END IF
C
              DO 50 J = J1,J2
                IF (J .LT. 37 .OR. J .GT. 39) THEN
C                 do unless users responce was (ALL,DONE or HELP)
C                 write out name and description
                  CALL ZIPC (NUML,BLNK,TBUFF)
                  ILEN= 30
                  CALL CHRCHR (ILEN,MBUFF,TBUFF)
                  CALL WDSAGY (MESSFL,J,
     O                         TBUFF(20),DPTR,SAHTYP,
     O                         SAHLEN,I,K)
C                 move over comma after name
                  DO 41 I= 25,21,-1
                    IF (TBUFF(I) .EQ. BLNK(1)) THEN
                      CALL CHRDEL(NUML,I,TBUFF)
                    END IF
 41               CONTINUE
C                 fill in index number
                  I= 3
                  CALL INTCHR (J,I,I1,
     O                         IA,TBUFF(15))
C                 fill in description
                  CALL WADGDS (MESSFL,DPTR,
     O                         TBUFF(30))
                  IF (IA .LT. I) THEN
C                   move left rest of message
                    IC= I- IA
                    I = 15+ IA
                    DO 42 IB= 1, IC
                      CALL CHRDEL(NUML,I,TBUFF)
 42                 CONTINUE
                  END IF
                  CALL ZBLDWR (NUML,TBUFF,I1,-I1,I)
C                 write out type and length
                  ILEN= 30
                  CALL CHRCHR (ILEN,MBUFF(61),TBUFF)
                  I = (SAHTYP+2)* 10+ 1
                  IA= 10
                  CALL CHRCHR (IA,MBUFF(I),TBUFF(23))
                  I= 2
                  CALL INTCHR (SAHLEN,I,I1,IA,TBUFF(11))
C                 type 1 is I, pos 31, etc
                  ILEN= 31
                  IF (IA.LT.I) THEN
C                   move left rest of message
                    IC = I- IA
                    I  = 11+ IA
                    DO 44 IB= 1, IC
                      CALL CHRDEL(ILEN,I,TBUFF)
 44                 CONTINUE
                  END IF
                  CALL ZBLDWR (I1,BLNK,I0,-I1,I)
                  CALL ZBLDWR (ILEN,TBUFF,I0,-I1,I)
C                 get help info, if available
                  CALL WADGHL (MESSFL,DPTR,
     O                         TLEN,DREC,DPOS)
                  IF (TLEN .GT. 0) THEN
C                   help available, print it out
                    GLEN= 0
                    HLEN= 0
 45                 CONTINUE
C                     write text lines until cont is 0
                      I= 78
                      CALL WMSGTE (MESSFL,TLEN,I,
     M                             DREC,DPOS,GLEN,HLEN,
     O                             ILEN,TBUFF,CONT)
                      CALL ZBLDWR (ILEN,TBUFF,I0,-I1,I)
                    IF (CONT .EQ. 1) GO TO 45
                  END IF
                  IF (SAHTYP.LE.2) THEN
C                   print min and max allowed values for numeric values
                    CALL WADGRA (MESSFL,DPTR,SAHTYP,
     O                           RTMP(1),RTMP(2))
                    I = 3
                    IA= 2
                    CALL PMXTXR (MESSFL,I1,I,I1,-I1,-I1,IA,RTMP)
                  ELSE
C                   print additional information about character vars
                    CALL WADGVA (MESSFL,DPTR,MLEN,
     O                           TLEN,TBUFF)
                    IF (TLEN.EQ.0) THEN
C                     any string allowed
                      SGRP= 5
                      CALL PMXCNW (MESSFL,SCLU,SGRP,I1,-I1,-I1,I)
                    ELSE
C                     print valid strings
                    END IF
                  END IF
C                 wait for user
                  CALL ZBLDWR (I1,BLNK,I0,I0,I)
                END IF
 50           CONTINUE
            END IF
          IF (FLG .EQ. 0) GO TO 40
        END IF
C       repeat request if user specified help or had bad entry
      IF (SAIND .EQ. 37 .OR. SAIND .EQ. 0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSALV
     I                    (MESSFL,WDMSFL,DREC,SAIND,
     O                     TBUFF,RETCOD)
C
C     + + + PURPOSE + + +
C     Given a WDM file, label record, and search attribute index,
C     put the attribute name and value into a text buffer for output
C     by the calling routine.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,DREC,SAIND,RETCOD
      CHARACTER*1 TBUFF(78)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     WDMSFL - WDM file unit number
C     DREC   - data set label record number
C     SAIND  - index number of search attribute to list
C     TBUFF  - output text string containing attribute name and value
C     RETCOD - return code
C
C     + + + SAVES + + +
      INTEGER       LWDM,LREC,PSA,SAMAX
      SAVE          LWDM,LREC,PSA,SAMAX
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,I0,I6,I11,J,K,K1,K2,L,ITMP,DOIT,LENB,LEN,LSAIND,
     $              DSAIND,PSIND,PSAVAL,IP,SALEN,SATYP,SIG,DEC,JLEN
      REAL          RTMP
      CHARACTER*1   COLON,SANAM(6),BLNK(1),ERRTXT(53)
      CHARACTER*100 BBUFF
C
C     + + + FUNCTIONS + + +
      INTEGER       LENSTR, WDGIVL
      REAL          WDGRVL
      CHARACTER*4   WDGCVL
C
C     + + + INTRINSICS + + +
      INTRINSIC     ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL      LENSTR, WDSAGY, CHRCHR, INTCHR, DECCHX
      EXTERNAL      ZIPC, CVARAR, WDGIVL, WDGRVL, WDGCVL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA COLON, BLNK, SIG, DEC, LENB, I0, I6, I11, LWDM, LREC
     $    /  ':',  ' ',   5,   3,  78,   0,  6,  11,   0,    0 /
      DATA ERRTXT/'A','t','t','r','i','b','u','t','e',' ','i','n','d',
     $            'e','x',' ','n','u','m','b','e','r',' ',' ',' ',' ',
     $            ' ','n','o','t',' ','f','o','u','n','d',' ','o','n',
     $        ' ','m','e','s','s','a','g','e',' ','f','i','l','e','.'/
C
C     + + + END SPECIFICATIONS + + +
C
C     init output buffer
      CALL ZIPC (LENB,BLNK,TBUFF)
      RETCOD= 0
C
      IF (WDMSFL.NE.LWDM .OR. DREC.NE.LREC .OR. SAIND.LT.0) THEN
C       new or updated data-set label, get pointers
        IP   = 10
        PSA  = WDGIVL(WDMSFL,DREC,IP)
        SAMAX= WDGIVL(WDMSFL,DREC,PSA)
        LWDM = WDMSFL
        LREC = DREC
      END IF
C
      IF (SAMAX.GT.0) THEN
C       attributes exist on data set
        LSAIND = ABS(SAIND)
        DOIT= 0
        I= 0
 10     CONTINUE
C         look through available attributes for match
          I = I+ 1
          PSIND = PSA+ I* 2
          DSAIND= WDGIVL(WDMSFL,DREC,PSIND)
          IF (DSAIND.EQ.LSAIND) THEN
C           attribute indices match
            DOIT= 1
          END IF
          IF (DOIT.EQ.1) THEN
C           output attribute value
C           where do values start
            PSAVAL= WDGIVL(WDMSFL,DREC,PSIND+1)
C           get details
            CALL WDSAGY (MESSFL,DSAIND,
     O                   SANAM,J,SATYP,SALEN,J,J)
            IF (LENSTR(I6,SANAM).EQ.0) THEN
C             attribute not found on message file
              J= 3
              CALL INTCHR (DSAIND,J,I0,
     O                     K,ERRTXT(24))
              J= 53
              CALL CHRCHR (J,ERRTXT,TBUFF)
            ELSE
C             list attribute
              IF (SATYP.LE.2) THEN
C               integer or decimal type
                DO 30 J= 1,SALEN,6
                  K1 = PSAVAL + J - 1
                  K2 = J + 5
                  IF (K2 .GT. SALEN) K2 = SALEN
                  K2 = K1- 1+ K2
                  CALL ZIPC (LENB,BLNK,TBUFF)
                  IF (J.EQ.1) THEN
C                   first of 6 (or less) of this attribute
                    CALL CHRCHR (I6,SANAM,TBUFF(3))
                    TBUFF(10) = COLON
                  END IF
                  L = 12
                  DO 25 K = K1, K2
                    IF (SATYP .EQ. 1) THEN
C                     integer attribute
                      ITMP= WDGIVL(WDMSFL,DREC,K)
                      CALL INTCHR (ITMP,I11,I0,
     O                             JLEN,TBUFF(L))
                    ELSE
C                     real attribute
                      RTMP= WDGRVL(WDMSFL,DREC,K)
                      CALL DECCHX (RTMP,I11,SIG,DEC,TBUFF(L))
                    END IF
                    L = L + I11
 25               CONTINUE
 30             CONTINUE
              ELSE
C               character type
                J= PSAVAL+ (SALEN/4)- 1
                CALL ZIPC (LENB,BLNK,TBUFF)
                CALL CHRCHR (I6,SANAM,TBUFF(3))
                TBUFF(10) = COLON
                IP= 1
                DO 50 K= PSAVAL,J
                  BBUFF(IP:IP+3)= WDGCVL(WDMSFL,DREC,K)
                  IP= IP+4
 50             CONTINUE
                LEN = (J - PSAVAL + 1) * 4
                IF (LEN.GT.60) THEN
C                 too long for buffer
                  L= 60
                ELSE
                  L= LEN
                END IF
                CALL CVARAR (LEN,BBUFF,L,TBUFF(19))
              END IF
            END IF
          END IF
        IF (DOIT.EQ.0 .AND. I.LT.SAMAX) GO TO 10
      END IF
C
      IF (DOIT.EQ.0) THEN
C       attribute not found on data set
        RETCOD= -1
      END IF
C
      RETURN
      END
