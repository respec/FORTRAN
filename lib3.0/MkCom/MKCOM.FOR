C
C
C
      PROGRAM   MKCOM
C
C     + + + PURPOSE + + +
C     make a common block from a data structure
C
C     + + + PARAMETERS + + +
      INTEGER      MXVAR,MXSECT,MXHEAD,MXEQU,MXSPA,MXDUM,MXPARM
      PARAMETER   (MXVAR=2000,MXSECT=15,MXHEAD=20,MXEQU=500,
     $             MXSPA=500,MXDUM=50,MXPARM=20)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,L,IPOS,ILEN,TPOS,LINCNT,BLKLEN,IBCK,I1,I0,I6,
     $             I12,I16,SPAFG,LDIM(4),INPFL,OUTFL,SECFL,SPAFL,SPEFL,
     $             NFL,SPECVL(5),SECSFL,DUMDIM(4),DUMFLG,DUMSIZ(MXDUM),
     $             DUMCUR,SIZREM,SIZTOT,SIZSTR,SIZCUR,SIZNOW,LASTVR,
     $             LASTSZ,VARCNT,VARDIM(4,MXVAR),VARUSE(MXVAR,MXSECT),
     $             EQUCNT,EQUDIM(4,MXEQU),EQUUSE(MXEQU,MXSECT),HEACNT,
     $             HEASEC(MXHEAD),SPACNT,SPADIM(4,MXSPA),SECCNT,SPAPTR,
     $             SECSIZ(MXSECT),SECSKP(MXSECT,MXSECT),DUMCNT,EQUPTR,
     $             VARADD(MXVAR),EQUADD(MXEQU),SPADD(MXSPA),PARCNT,
     $             PARVAL(MXPARM),PARFIL(MXPARM),PARFCT,PARWRI,PARDEC,
     $             PARUSE(MXPARM,MXSECT),PARFUS(MXPARM,MXSECT)
      CHARACTER*1  VARTYP(MXVAR),VARCOD(MXVAR),EQUTYP(MXEQU),CR
      CHARACTER*4  VARSUB(4,MXVAR),EQUSUB(4,MXEQU),SPASUB(4,MXSPA),
     $             PARTAG(MXPARM)
      CHARACTER*6  VARNAM(MXVAR),VARDEP(MXVAR),COMNAM,MODNAM,
     $             EQUNAM(MXEQU),DUMNAM(MXDUM),SPANAM(MXSPA),
     $             PARNAM(MXPARM)
      CHARACTER*12 FNAME,SECNAM(MXSECT),PARFNM(MXPARM)
      CHARACTER*16 VARASO(MXVAR),EQUASO(MXEQU)
      CHARACTER*24 TMPNAM,BLKNAM
      CHARACTER*75 HEADER(MXHEAD)
      CHARACTER*80 BUFF,OBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (HEADER,HEADR1),(COMNAM,COMNM1),(BUFF,BUF1),
     $            (TMPNAM,TMPNM1),(VARNAM,VARNM1),(DUMNAM,DUMNM1),
     $            (OBUFF,OBUF1),(EQUNAM,EQUNM1),(EQUASO,EQUAS1),
     $            (BLKNAM,BLKNM1),(PARTAG,PARTG1),(PARNAM,PARNM1),
     $            (PARFNM,PARFN1)
      CHARACTER*1  HEADR1(75,MXHEAD),COMNM1(6),BUF1(80),
     $             TMPNM1(24),VARNM1(6,MXVAR),DUMNM1(6,MXDUM),
     $             OBUF1(80),EQUNM1(6,MXEQU),EQUAS1(16,MXEQU),
     $             BLKNM1(24),PARTG1(4,MXPARM),PARNM1(6,MXPARM),
     $             PARFN1(12,MXPARM)
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LENSTR,ADTOBF,GETDIM,VARSZE,DCLVAR,COPYC,INTCHR,
     $             ZIPI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A80)
 1010 FORMAT (A6,4A4,2(1X,A1),1X,A16,1X,A6,1X,20I1)
 1012 FORMAT (A6,4A4,1X,A1,3X,A16,8X,20I1)
 1015 FORMAT (A6,4A4)
 1020 FORMAT (2I10)
 1025 FORMAT (5X,A6,A4,I10,I4)
 1028 FORMAT (5X,A12)
 1030 FORMAT (I2)
 1040 FORMAT (I2,1X,20I1)
 1050 FORMAT (I1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('C     from ',A12)
 2010 FORMAT ('C     ',75A1)
 2020 FORMAT ('C     start byte: ',I6,' word:',I5)
 2090 FORMAT ('C     end byte: ',I6,' word:',I5)
 2200 FORMAT (1X,A6,':',A16,1X,I5,1X,A6,2A4)
 2300 FORMAT (A6,3I4,1X,I5,' ')
 2350 FORMAT (A6,I10)
 2400 FORMAT (80A1)
 2500 FORMAT (1X,A6,5I5)
 2520 FORMAT (A12,2I5)
C
C     + + + END SPECIFICATIONS + + +
C
      INPFL= 10
      OPEN (UNIT=INPFL,FILE='MKCOM.INP',STATUS='OLD')
      OUTFL= 11
      OPEN (UNIT=OUTFL,FILE='MKCOM.OUT')
      SPAFL= 12
      OPEN (UNIT=SPAFL,FILE='MKCOM.SPA')
      SPEFL= 13
      OPEN (UNIT=SPEFL,FILE='MKCOM.SPE')
      SECFL= 14
      SECSFL= 15
      OPEN (UNIT=SECSFL,FILE='MKCOM.SEC')
      NFL= 16
      OPEN (UNIT=NFL,FILE='MKCOM.N')
C
      I0= 0
      I1= 1
      I6= 6
      I12= 12
      I16= 16
      CR= 'R'
C
      VARCNT= 0
      EQUCNT= 0
      SPACNT= 0
      PARCNT= 0
      PARFCT= 0
      HEACNT= 0
      SECCNT= 0
C     default common name
      COMNAM= 'SCRTCH'
C     default module name
      MODNAM= ' '
C     default dummy dimensions
      DUMDIM(1)= 0
      DUMDIM(2)= 0
      DUMDIM(3)= 0
C     nothing known about special actions
      SPECVL(1)= 0
      SPECVL(2)= 0
      SPECVL(3)= 0
      SPECVL(4)= 0
      SPECVL(5)= 0
C     no parameters or parm files used
      DO 20 I= 1, MXPARM
        DO 10 J= 1, MXSECT
          PARUSE(I,J)= 0
          PARFUS(I,J)= 0
 10     CONTINUE
 20   CONTINUE
C
 30   CONTINUE
        READ (INPFL,1000,END=200) BUFF
        IF (BUFF(1:2) .EQ. 'VA') THEN
C         variable
          VARCNT= VARCNT+ 1
          READ (BUFF(4:4),1050) SPAFG
          READ (BUFF(6:80),1010) VARNAM(VARCNT),
     $                          (VARSUB(I,VARCNT),I=1,4),
     $                           VARTYP(VARCNT),
     $                           VARCOD(VARCNT),
     $                           VARASO(VARCNT),
     $                           VARDEP(VARCNT),
     $                          (VARUSE(VARCNT,J),J=1,MXSECT)
C
          IF (VARTYP(VARCNT) .EQ. 'D') THEN
C           check to make sure begins on odd address
            I= MOD (SIZCUR,2)
            IF (I .EQ. 0) THEN
C             need to insert dummy
              WRITE (*,*) 'Need dummy before double-precision ',
     $                     VARNAM(VARCNT)
              STOP
            END IF
          END IF
          VARADD(VARCNT)= SIZCUR
          EQUPTR= SIZCUR
          SPAPTR= SIZCUR
          CALL GETDIM (PARCNT,PARTG1,VARSUB(1,VARCNT),
     O                 VARDIM(1,VARCNT))
          CALL USEPAR (VARDIM(1,VARCNT),MXVAR,MXSECT,VARUSE,VARCNT,
     I                 MXPARM,PARCNT,PARFIL,
     M                 PARUSE,PARFUS)
          IF (VARUSE(VARCNT,SECCNT) .NE. 2) THEN
C           calc how much space for this variable
            CALL VARSZE (VARDIM(1,VARCNT),VARTYP(VARCNT),PARCNT,PARVAL,
     O                   SIZNOW)
C           increment total size
            SIZCUR= SIZCUR+ SIZNOW
          END IF
C
          IF (VARNAM(VARCNT) .EQ. 'SPACFL') THEN
              SPECVL(1)= VARADD(VARCNT)
          ELSE IF (VARNAM(VARCNT) .EQ. 'SPAFP ') THEN
              SPECVL(2)= VARADD(VARCNT)
          ELSE IF (VARNAM(VARCNT) .EQ. 'SPAKND') THEN
              SPECVL(3)= VARADD(VARCNT)
          ELSE IF (VARNAM(VARCNT) .EQ. 'SPOUT ') THEN
              SPECVL(4)= VARADD(VARCNT)
          ELSE IF (VARNAM(VARCNT) .EQ. 'SPNUND') THEN
              SPECVL(5)= VARADD(VARCNT)
          END IF
C
          IF (VARCOD(VARCNT) .EQ. 'T') THEN
C           flag pointer loc needed for timser sections
            WRITE (OUTFL,2200) MODNAM,VARASO(VARCNT),
     $                         VARADD(VARCNT),VARDEP(VARCNT),
     $                         VARSUB(1,VARCNT),VARSUB(2,VARCNT)
          END IF
C
          IF ( (SPAFG .EQ. 1) .OR. (SPAFG .EQ. 3) ) THEN
C           info for special actions
            DO 40 I= 1, 4
              IF (VARDIM(I,VARCNT) .LT. 0) THEN
C               use parameter value
                LDIM(I)= PARVAL(-VARDIM(I,VARCNT))
              ELSE
C               use value
                LDIM(I)= VARDIM(I,VARCNT)
              END IF
 40         CONTINUE
            IF (VARTYP(VARCNT) .EQ. 'D') THEN
C             double precision - make first subscript negative
              IF (VARDIM(1,VARCNT) .EQ. 0) THEN
C               no subscript - make minus one
                LDIM(1)= -1
              ELSE
C               negate value
                LDIM(1)= -LDIM(1)
              END IF
            END IF
            WRITE (OBUFF,2300) VARNAM(VARCNT),(LDIM(I),I=1,3),
     $                         VARADD(VARCNT)
            IF (OBUFF(9:10) .EQ. ' 0') THEN
C             no subscripts
              OBUFF(7:18)= ' '
            ELSE IF (OBUFF(13:14) .EQ. ' 0') THEN
C             no second to fourth subscripts
              OBUFF(11:18)= ' '
            ELSE IF (OBUFF(17:18) .EQ. ' 0') THEN
C             no third or fourth subscripts
              OBUFF(15:18)= ' '
            END IF
C           note that fourth subscripts are not allowed for spac variables
            ILEN= 80
            WRITE (SPAFL,2400) (OBUF1(K),K=1,LENSTR (ILEN,OBUF1))
          END IF
          IF (SPAFG .GE. 2) THEN
C           write variable to number file
            WRITE (NFL,2350) VARNAM(VARCNT), VARADD(VARCNT)
          END IF
C
        ELSE IF (BUFF(1:2) .EQ. 'EQ') THEN
C         equivalence
          EQUCNT= EQUCNT+ 1
          READ(BUFF(4:4),1050) SPAFG
          READ(BUFF(6:80),1012) EQUNAM(EQUCNT),
     $                         (EQUSUB(I,EQUCNT),I=1,4),
     $                          EQUTYP(EQUCNT),
     $                          EQUASO(EQUCNT),
     $                         (EQUUSE(EQUCNT,J),J=1,MXSECT)
C
          IF (EQUTYP(EQUCNT) .EQ. 'D') THEN
C           check to make sure begins on odd address
            I= MOD (SIZCUR,2)
            IF (I .EQ. 0) THEN
C             need to insert dummy
              WRITE (*,*) 'Need dummy before double-precision ',
     $                     EQUNAM(EQUCNT)
              STOP
            END IF
          END IF
          EQUADD(EQUCNT)= EQUPTR
          CALL GETDIM (PARCNT,PARTG1,EQUSUB(1,EQUCNT),
     O                 EQUDIM(1,EQUCNT))
          CALL USEPAR (EQUDIM(1,EQUCNT),MXEQU,MXSECT,EQUUSE,EQUCNT,
     I                 MXPARM,PARCNT,PARFIL,
     M                 PARUSE,PARFUS)
          CALL VARSZE (EQUDIM(1,EQUCNT),EQUTYP(EQUCNT),PARCNT,PARVAL,
     O                 SIZNOW)
          EQUPTR= EQUPTR+ SIZNOW
          IF ( (SPAFG .EQ. 1) .OR. (SPAFG .EQ. 3) ) THEN
C           info for special actions
            DO 50 I= 1, 3
              IF (EQUDIM(I,EQUCNT) .LT. 0) THEN
C               use parameter value
                LDIM(I)= PARVAL(-EQUDIM(I,EQUCNT))
              ELSE
C               use value
                LDIM(I)= EQUDIM(I,EQUCNT)
              END IF
 50         CONTINUE
            IF (EQUTYP(EQUCNT) .EQ. 'D') THEN
C             double precision - make first subscript negative
              IF (EQUDIM(1,EQUCNT) .EQ. 0) THEN
C               no subscript - make minus one
                LDIM(1)= -1
              ELSE
C               negate value
                LDIM(1)= -LDIM(1)
              END IF
            END IF
            WRITE (OBUFF,2300)  EQUNAM(EQUCNT),(LDIM(I),I=1,3),
     $                          EQUADD(EQUCNT)
            IF (OBUFF(9:10) .EQ. ' 0') THEN
C             no subscripts
              OBUFF(7:18)= ' '
            ELSE IF (OBUFF(13:14) .EQ. ' 0') THEN
C             no second and third subscripts
              OBUFF(11:18)= ' '
            ELSE IF (OBUFF(17:18) .EQ. ' 0') THEN
C             no third subscripts
              OBUFF(15:18)= ' '
            END IF
C           note that fourth subscripts are not allowed for spac variables
            ILEN= 80
            WRITE (SPAFL,2400) (OBUF1(K),K=1,LENSTR (ILEN,OBUF1))
          END IF
C
        ELSE IF ( (BUFF(1:2) .EQ. 'SP') .OR.
     $            (BUFF(1:2) .EQ. 'AL') ) THEN
C         special action name or alias
          SPACNT= SPACNT+ 1
          READ (BUFF(4:4),1050) SPAFG
          READ (BUFF(6:23),1015)  SPANAM(SPACNT),
     $                           (SPASUB(I,SPACNT),I=1,4)
C
          SPADD(SPACNT)= SPAPTR
          CALL GETDIM (PARCNT,PARTG1,SPASUB(1,SPACNT),
     O                 SPADIM(1,SPACNT))
C         special action names don't use parameters, so skip usepar
          IF (BUFF(1:2) .EQ. 'SP') THEN
C           all except aliases must update memory location
            CALL VARSZE (SPADIM(1,SPACNT),CR,PARCNT,PARVAL,
     O                   SIZNOW)
            SPAPTR= SPAPTR+ SIZNOW
          END IF
          IF ( (SPAFG .EQ. 1) .OR. (SPAFG .EQ. 3) ) THEN
C           info for special actions
            WRITE (OBUFF,2300)  SPANAM(SPACNT),
     $                         (SPADIM(I,SPACNT),I=1,3),SPADD(SPACNT)
            IF (OBUFF(9:10) .EQ. ' 0') THEN
C             no subscripts
              OBUFF(7:18)= ' '
            ELSE IF (OBUFF(13:14) .EQ. ' 0') THEN
C             no second and third subscripts
              OBUFF(11:18)= ' '
            ELSE IF (OBUFF(17:18) .EQ. ' 0') THEN
C             no third subscripts
              OBUFF(15:18)= ' '
            END IF
C           fourth subscripts are not allowed for spac variables
            ILEN= 80
            WRITE (SPAFL,2400) (OBUF1(K),K=1,LENSTR (ILEN,OBUF1))
          END IF
C
        ELSE IF (BUFF(1:2) .EQ. 'SI') THEN
C         size
          READ(BUFF(6:25),1020) SIZTOT,SIZSTR
          SIZCUR= SIZSTR+ 1
C
        ELSE IF (BUFF(1:2) .EQ. 'MO') THEN
C         module name
          MODNAM= BUFF(6:11)
C
        ELSE IF (BUFF(1:2) .EQ. 'PM') THEN
C         parameter definition
          PARCNT= PARCNT+ 1
          READ(BUFF,1025) PARNAM(PARCNT),PARTAG(PARCNT),PARVAL(PARCNT),
     $                    PARFIL(PARCNT)
C
        ELSE IF (BUFF(1:2) .EQ. 'PF') THEN
C         parameter file definition
          PARFCT= PARFCT+ 1
          READ(BUFF,1028) PARFNM(PARFCT)
C
        ELSE IF (BUFF(1:2) .EQ. 'HE') THEN
C         header
          HEACNT= HEACNT+ 1
          READ(BUFF(3:4),1030) HEASEC(HEACNT)
          HEADER(HEACNT)= BUFF(6:80)
C
        ELSE IF (BUFF(1:2) .EQ. 'CO') THEN
C         common block name
          COMNAM= BUFF(6:11)
C
        ELSE IF (BUFF(1:2) .EQ. 'SE') THEN
C         output file names
          IF (SECCNT .GT. 0) THEN
C           save size of last section
            SECSIZ(SECCNT)= SIZCUR
          END IF
          SECCNT= SECCNT+ 1
          SECNAM(SECCNT)= BUFF(6:17)
          READ(BUFF(18:40),1040) I,(SECSKP(J,SECCNT),J=1,MXSECT)
          IF (I .GT. 0) THEN
C           reset where we are in common for this section to start
            SIZCUR= SECSIZ(I)
          END IF
C
        ELSE IF (BUFF(1:2) .EQ. 'WR') THEN
C         write (and restart), save size of last section
          SECSIZ(SECCNT)= SIZCUR
C         loop thru sections
          DO 150 I= 1,SECCNT
C           determine size of osv for each section
            J= ((SECSIZ(I)- 1) / 500)+ 1
            WRITE (SECSFL,2520) SECNAM(I),SECSIZ(I),J
C           open output file for this section
            FNAME= SECNAM(I)
            OPEN(UNIT=SECFL,FILE=FNAME)
C           echo name
            WRITE (SECFL,2000) FNAME
C
C           headers
            DO 90 J= 1,HEACNT
              IF (HEASEC(J).EQ.0  .OR. HEASEC(J).EQ.I) THEN
C               write this header
                L= 75
                WRITE (SECFL,2010) (HEADR1(K,J),
     $                              K=1,LENSTR (L,HEADR1(1,J)))
              END IF
 90         CONTINUE
C
            IF (SIZSTR .GT. 0) THEN
C             indicate not starting at 0
              WRITE (SECFL,2020) SIZSTR*4,SIZSTR
            END IF
C
            IF (PARCNT .GE. 1) THEN
C             write parameters
C
              PARWRI= 0
              PARDEC= 0
              DO 60 J= 1, PARCNT
                IF (PARUSE(J,I) .EQ. 1) THEN
                  PARWRI= 1
                  IF (PARFIL(J) .EQ. 0) THEN
                    PARDEC= 1
                  END IF
                END IF
 60           CONTINUE
C
              IF (PARWRI .EQ. 1) THEN
C               header
                BUFF= 'C'
                WRITE (SECFL,2400) BUF1(1)
                BUFF= 'C     + + + PARAMETERS + + +'
                WRITE (SECFL,2400) (BUF1(K),K= 1, 28)
C
C               declare parameters
C
C               first declare any files used
                DO 70 J= 1, PARFCT
                  IF (PARFUS(J,I) .EQ. 1) THEN
                    BUFF= '      INCLUDE '
                    IPOS= 15
                    BUF1(IPOS)= ''''
                    IPOS= IPOS+ 1
                    ILEN= LENSTR (I12,PARFN1(1,J))
                    CALL COPYC (ILEN,PARFN1(1,J),
     O                          BUF1(IPOS))
                    BUF1(IPOS+ILEN)= ''''
                    WRITE (SECFL,2400) (BUF1(K),K=1,IPOS+ILEN)
                  END IF
 70             CONTINUE
                IF (PARDEC .EQ. 1) THEN
                  BUFF= '      INTEGER '
                  IPOS= 18
                  BLKNAM(1:IPOS)= BUFF(1:IPOS)
                  BLKLEN= IPOS
                  LINCNT= 0
                  DO 75 J= 1, PARCNT
                    IF ( (PARUSE(J,I) .EQ. 1) .AND.
     $                   (PARFIL(J) .EQ. 0) ) THEN
                      ILEN= LENSTR (I6,PARNM1(1,J))
                      TPOS= 1
                      CALL COPYC (ILEN,PARNM1(1,J),
     O                          TMPNM1(TPOS))
                      TPOS= TPOS+ ILEN
                      TMPNM1(TPOS)= ','
                      IF (IPOS+TPOS .GT. 70) THEN
C                       wont fit on this line
                        LINCNT= LINCNT+ 1
                        IF (LINCNT .GT. 10) THEN
C                         no trailing comma
                          IBCK= 2
                        ELSE
C                         need trailing comma
                          IBCK= 1
                        END IF
                        WRITE (SECFL,2400) (BUF1(ILEN),ILEN=1,IPOS-IBCK)
C                       reinit line
                        IF (LINCNT .GT. 10) THEN
C                         new parameter header
                          BUFF(1:20)= BLKNAM(1:20)
                          LINCNT= 1
                        ELSE
                          BUFF= ' '
                          BUFF(6:6)= '$'
                        END IF
                        IPOS= 18
                      END IF
                      BUFF(IPOS:IPOS+TPOS)= TMPNAM(1:TPOS)
                      IPOS= IPOS+ TPOS
                    END IF
 75               CONTINUE
C                 clear out final buffer
                  WRITE (SECFL,2400) (BUF1(K),K=1,IPOS-2)
C
C                 parameter statements
                  DO 80 J= 1, PARCNT
                    IF ( (PARUSE(J,I) .EQ. 1) .AND.
     $                   (PARFIL(J) .EQ. 0) ) THEN
                      BUFF= '      PARAMETER ('
                      IPOS= 18
                      ILEN= LENSTR (I6,PARNM1(1,J))
                      CALL COPYC (ILEN,PARNM1(1,J),
     O                            BUF1(IPOS))
                      IPOS= IPOS+ ILEN
                      BUFF(IPOS:IPOS+2)= ' = '
                      IPOS= IPOS+ 3
                      CALL INTCHR (PARVAL(J),I6,I1,
     O                             ILEN,BUF1(IPOS))
                      IPOS= IPOS+ ILEN
                      BUF1(IPOS)= ')'
                      IPOS= IPOS+ 1
                      WRITE (SECFL,2400) (BUF1(K),K=1,IPOS)
                    END IF
 80               CONTINUE
                END IF
C
C               footer
                BUFF= 'C'
                WRITE (SECFL,2400) BUF1(1)
              END IF
            END IF
C
C           all variables in common
            BUFF= '      COMMON/'
            IPOS= 14
            ILEN= LENSTR (I6,COMNM1)
            CALL COPYC (ILEN,COMNM1,
     O                  BUF1(IPOS))
            IPOS= IPOS+ ILEN
            BUF1(IPOS)= '/'
            BLKNAM(1:IPOS)= BUFF(1:IPOS)
            BLKLEN= IPOS
            LINCNT= 0
            IPOS  = IPOS+ 1
            DUMFLG= 0
            DUMCUR= 0
            DUMCNT= 0
            DO 110 J= 1, VARCNT
              IF (VARUSE(J,I) .EQ. 1) THEN
                IF (DUMFLG.GT.0) THEN
C                 clear out this dummy
                  DUMCNT= DUMCNT+ 1
                  TMPNAM= 'DUM'
                  CALL INTCHR(DUMCNT,I6,I1,
     O                        L,TMPNM1(4))
                  DUMNAM(DUMCNT)= TMPNAM(1:L+3)
                  DUMSIZ(DUMCNT)= DUMCUR
                  DUMCUR        = 0
                  DUMFLG        = 0
                  CALL ADTOBF (DUMNM1(1,DUMCNT),DUMDIM,SECFL,I0,
     I                         BLKNAM,BLKLEN,PARCNT,PARNAM,
     M                         IPOS,BUF1,BUFF,LINCNT)
                  BUF1(IPOS)= ','
                  IPOS= IPOS+ 1
                END IF
C               in use
                CALL ADTOBF (VARNM1(1,J),DUMDIM,SECFL,I0,
     I                       BLKNAM,BLKLEN,PARCNT,PARNAM,
     M                       IPOS,BUF1,BUFF,LINCNT)
                BUF1(IPOS)= ','
                IPOS= IPOS+ 1
              ELSE IF (VARUSE(J,I).NE.2) THEN
C               not in use, not sure about skip, need to skip?
                K= 0
C               used in prev sections?
 100            CONTINUE
                  K= K+ 1
                IF (VARUSE(J,K).EQ.0 .AND. K.LT.I) GO TO 100
C
                IF (SECSKP(K,I) .NE. 1) THEN
C                 not skipping, increment current dummy var
                  CALL VARSZE (VARDIM(1,J),VARTYP(J),PARCNT,PARVAL,
     O                         SIZNOW)
C                 increment current dummy size
                  DUMCUR= DUMCUR+ SIZNOW
                  DUMFLG= 1
                END IF
              END IF
 110        CONTINUE
C
C           dummy variable to ending position
C
C           find last variable used
            LASTVR= VARCNT+ 1
 120        CONTINUE
              LASTVR= LASTVR- 1
            IF (VARUSE(LASTVR,I) .NE. 1) GO TO 120
C
C           find address of end of last variable
            CALL VARSZE (VARDIM(1,LASTVR),VARTYP(LASTVR),PARCNT,PARVAL,
     O                   LASTSZ)
C
C           calculate size remaining
            SIZREM= SIZTOT- VARADD(LASTVR)- LASTSZ+ 1
            IF (SIZREM .GT. 0) THEN
C             a dummy must be added
              DUMCNT= DUMCNT+ 1
              DUMNAM(DUMCNT)= 'DUM'//MODNAM(1:3)
              DUMSIZ(DUMCNT)= SIZREM
              CALL ADTOBF (DUMNM1(1,DUMCNT),DUMDIM,SECFL,I1,
     I                     BLKNAM,BLKLEN,PARCNT,PARNAM,
     M                     IPOS,BUF1,BUFF,LINCNT)
            END IF
C           declare variables
            CALL DCLVAR (SECFL,VARCNT,VARTYP,VARNM1,VARDIM,
     I                   VARUSE(1,I),PARCNT,PARNAM)
C
            IF (DUMCNT .GT. 0) THEN
C             declare dummys
              BUFF= '      INTEGER'
              BLKNAM(1:20)= BUFF(1:20)
              BLKLEN= 20
              LINCNT= 1
              IPOS  = 21
              DO 130 J= 1,DUMCNT
                IF (DUMSIZ(J) .GT. 1) THEN
C                 a dummy array
                  DUMDIM(1)= DUMSIZ(J)
                END IF
                CALL ADTOBF (DUMNM1(1,J),DUMDIM,SECFL,I0,
     I                       BLKNAM,BLKLEN,PARCNT,PARNAM,
     M                       IPOS,BUF1,BUFF,LINCNT)
C               assume not an array
                DUMDIM(1) = 0
                BUF1(IPOS)= ','
                IPOS= IPOS+ 1
 130          CONTINUE
C             clear out final buffer
              WRITE (SECFL,2400) (BUF1(K),K=1,IPOS-2)
            END IF
C
            IF (EQUCNT .GT. 0) THEN
C             equivalence
              BUFF= 'C'
              WRITE (SECFL,2400) BUF1(1)
              BUFF= '      EQUIVALENCE '
              BLKNAM(1:20)= BUFF(1:20)
              BLKLEN= 20
              LINCNT= 1
              IPOS  = 21
              DO 140 J= 1,EQUCNT
                IF (EQUUSE(J,I) .EQ. 1) THEN
                  TMPNM1(1)= '('
                  ILEN= LENSTR (I6,EQUNM1(1,J))
                  TPOS= 2
                  CALL COPYC (ILEN,EQUNM1(1,J),
     O                        TMPNM1(TPOS))
                  TPOS= TPOS+ ILEN
                  TMPNM1(TPOS)= ','
                  TPOS= TPOS+ 1
                  ILEN= LENSTR (I16,EQUAS1(1,J))
                  CALL COPYC (ILEN,EQUAS1(1,J),
     O                        TMPNM1(TPOS))
                  TPOS= TPOS+ ILEN
                  TMPNM1(TPOS)= ')'
                  IF (IPOS+TPOS .GT. 70) THEN
C                   wont fit on this line
                    LINCNT= LINCNT+ 1
                    IF (LINCNT .GT. 10) THEN
C                     no trailing comma
                      IBCK= 2
                    ELSE
C                     need trailing comma
                      IBCK= 1
                    END IF
                    WRITE (SECFL,2400) (BUF1(ILEN),ILEN=1,IPOS-IBCK)
C                   reinit line
                    IF (LINCNT .GT. 10) THEN
C                     new equiv
                      BUFF(1:20)= BLKNAM(1:20)
                      LINCNT= 1
                    ELSE
                      BUFF= ' '
                      BUFF(6:6)= '$'
                    END IF
                    IPOS= 21
                  END IF
                  BUFF(IPOS:IPOS+TPOS)= TMPNAM(1:TPOS)
                  IPOS= IPOS+ TPOS
                  BUFF(IPOS:IPOS)= ','
                  IPOS= IPOS+ 1
                END IF
 140          CONTINUE
C             clear out final buffer
              WRITE (SECFL,2400) (BUF1(K),K=1,IPOS-2)
C             declare equival vars
              CALL DCLVAR (SECFL,EQUCNT,EQUTYP,EQUNM1,EQUDIM,
     I                     EQUUSE(1,I),PARCNT,PARNAM)
            END IF
C           end
            WRITE (SECFL,2090) SIZTOT*4,SIZTOT
C           done this section
            CLOSE(UNIT=SECFL)
 150      CONTINUE
C         blank lines to other output file
          WRITE (OUTFL,*)
          WRITE (SPAFL,*)
C         reset counters
          VARCNT= 0
          EQUCNT= 0
          SPACNT= 0
          HEACNT= 0
          SECCNT= 0
          PARCNT= 0
          PARFCT= 0
C         reset parameter usage
          K= MXSECT*MXPARM
          CALL ZIPI (K,I0,
     O               PARUSE)
          CALL ZIPI (K,I0,
     O               PARFUS)
          IF (MODNAM .NE. ' ') THEN
C           whats known about special actions for this operating module
            WRITE (SPEFL,2500) MODNAM,SPECVL
          END IF
C         nothing known about special actions for next
          SPECVL(1)= 0
          SPECVL(2)= 0
          SPECVL(3)= 0
          SPECVL(4)= 0
          SPECVL(5)= 0
C         reset default module name
          MODNAM= ' '
        END IF
      GO TO 30
C
 200  CONTINUE
C
      END
C
C
C
      SUBROUTINE   ADTOBF
     I                   (VAR,DIM,OFL,IWRT,BLKNAM,BLKLEN,PARCNT,PARNAM,
     M                    IPOS,BUF1,BUFF,LINCNT)
C
C     + + + PURPOSE + + +
C     add a variable to output buffer, write buffer if needed
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      DIM(4),OFL,IWRT,BLKLEN,PARCNT,IPOS,LINCNT
      CHARACTER*1  VAR(6),BUF1(80)
      CHARACTER*6  PARNAM(PARCNT)
      CHARACTER*24 BLKNAM
      CHARACTER*80 BUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VAR    - variable name
C     DIM    - variable dimensions
C     OFL    - output file
C     IWRT   - force write flag
C     IPOS   - position in output file
C     BUF1   - output buffer as single chars
C     BUFF   - output buffer as long string
C
C     + + + PARAMETERS + + +
      INTEGER      MXLIN
      PARAMETER   (MXLIN=18)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ILEN,TLEN,I6,I1,IBCK
      CHARACTER*1 TMPNM1(35)
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  LENSTR,COPYC,INTCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I6/1,6/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(80A1)
C
C     + + + END SPECIFICATIONS + + +
C
      ILEN= LENSTR (I6,VAR)
      CALL COPYC (ILEN,VAR,
     O            TMPNM1)
      TLEN= ILEN
      IF (DIM(1) .NE. 0) THEN
C       have a first dim
        TLEN= TLEN+ 1
        TMPNM1(TLEN)= '('
        IF (DIM(1) .GT. 0) THEN
C         constant subscript
          CALL INTCHR (DIM(1),I6,I1,
     O                 ILEN,TMPNM1(TLEN+1))
        ELSE
C         parameter subscript
          CALL CVARAR (I6,PARNAM(-DIM(1)),I6,
     O                 TMPNM1(TLEN+1))
          ILEN= LENSTR (I6,TMPNM1(TLEN+1))
        END IF
        TLEN= TLEN+ ILEN
        IF (DIM(2) .NE. 0) THEN
C         second dim also
          TLEN= TLEN+ 1
          TMPNM1(TLEN)= ','
          IF (DIM(2) .GT. 0) THEN
C           constant subscript
            CALL INTCHR (DIM(2),I6,I1,
     O                   ILEN,TMPNM1(TLEN+1))
          ELSE
C           parameter subscript
            CALL CVARAR (I6,PARNAM(-DIM(2)),I6,
     O                   TMPNM1(TLEN+1))
            ILEN= LENSTR (I6,TMPNM1(TLEN+1))
          END IF
          TLEN= TLEN+ ILEN
          IF (DIM(3) .NE. 0) THEN
C           third dim also
            TLEN= TLEN+ 1
            TMPNM1(TLEN)= ','
            IF (DIM(3) .GT. 0) THEN
C             constant subscript
              CALL INTCHR (DIM(3),I6,I1,
     O                     ILEN,TMPNM1(TLEN+1))
            ELSE
C             parameter subscript
              CALL CVARAR (I6,PARNAM(-DIM(3)),I6,
     O                     TMPNM1(TLEN+1))
              ILEN= LENSTR (I6,TMPNM1(TLEN+1))
            END IF
            TLEN= TLEN+ ILEN
            IF (DIM(4) .NE. 0) THEN
C             fourth dim also
              TLEN= TLEN+ 1
              TMPNM1(TLEN)= ','
              IF (DIM(4) .GT. 0) THEN
C               constant subscript
                CALL INTCHR (DIM(4),I6,I1,
     O                       ILEN,TMPNM1(TLEN+1))
              ELSE
C               parameter subscript
                CALL CVARAR (I6,PARNAM(-DIM(4)),I6,
     O                       TMPNM1(TLEN+1))
                ILEN= LENSTR (I6,TMPNM1(TLEN+1))
              END IF
              TLEN= TLEN+ ILEN
            END IF
          END IF
        END IF
        TLEN= TLEN+ 1
        TMPNM1(TLEN)= ')'
      END IF
C
      IF (IPOS+TLEN .GT. 70) THEN
C       wont fit on this line
        LINCNT= LINCNT+ 1
        IF (LINCNT .GT. MXLIN) THEN
C         no trailing comma
          IBCK= 2
        ELSE
C         leave trailing comma
          IBCK= 1
        END IF
        WRITE (OFL,2000) (BUF1(ILEN),ILEN=1,IPOS-IBCK)
C       reinit line
        IF (LINCNT .GT. MXLIN) THEN
C         new header
          BUFF(1:BLKLEN)= BLKNAM(1:BLKLEN)
          LINCNT= 1
          IPOS  = BLKLEN+ 1
        ELSE
C         continuation
          BUFF= ' '
          BUFF(6:6)= '$'
          IPOS= 21
        END IF
      END IF
C
      CALL COPYC (TLEN,TMPNM1,
     O            BUF1(IPOS))
      IPOS= IPOS+ TLEN
C
      IF (IWRT .EQ. 1) THEN
C       force write
        WRITE (OFL,2000) (BUF1(ILEN),ILEN=1,IPOS-1)
        BUFF= ' '
        IPOS= 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETDIM
     I                    (PARCNT,PARTG1,VSUB,
     O                     VDIM)
C
C     + + + PURPOSE + + +
C     parse character subscripts and set use flags for parms and parm files
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     PARCNT,VDIM(4)
      CHARACTER*1 PARTG1(4,PARCNT)
      CHARACTER*4 VSUB(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PARCNT - number of parameters defined
C     PARTG1 - parameter subscript tags
C     VSUB   - character value of variable subscripts
C     VDIM   - dimensions of variable
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I4,PAR
      CHARACTER*1 BUFF1(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHKSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (BUFF,BUFF1)
      CHARACTER*4  BUFF
C
C     + + + OUTPUT FORMATS + + +
 1000 FORMAT (I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I4= 4
C
      DO 30 I= 1, 4
        READ (VSUB(I),1000,ERR=10) VDIM(I)
          GO TO 20
 10     CONTINUE
          BUFF= VSUB(I)
          PAR= CHKSTR (I4,PARCNT,BUFF1,PARTG1)
          IF (PAR .GE. 1) THEN
            VDIM(I)= -PAR
          ELSE
            STOP 'BAD SUBSCRIPT VALUE OR PARAMETER SUBSTITUTION'
          END IF
 20     CONTINUE
 30   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   USEPAR
     I                    (VDIM,MXVAR,MXSECT,VARUSE,VARCNT,MXPARM,
     I                     PARCNT,PARFIL,
     M                     PARUSE,PARFUS)
C
C     + + + PURPOSE + + +
C     parse character subscripts and set use flags for parms and parm files
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     VDIM(4),MXVAR,MXSECT,VARUSE(MXVAR,MXSECT),VARCNT,
     $            MXPARM,PARCNT,PARFIL(PARCNT),PARUSE(MXPARM,MXSECT),
     $            PARFUS(MXPARM,MXSECT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VDIM   - dimensions of variable, negative if parameter
C     MXVAR  - maximum allowed variables
C     MXSECT - maximum allowed sections
C     VARUSE - flag indicating whether a variable is used in a given section
C     VARCNT - number of variables defined
C     MXPARM - maximum allowed parameters
C     PARCNT - number of parameters defined
C     PARFIL - file index for parameter include file
C     PARUSE - flag indicating if parameter used this section
C     PARFUS - flag indicating if parameter file used this section
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,PAR
C
C     + + + END SPECIFICATIONS + + +
C
      DO 20 I= 1, 4
        IF (VDIM(I) .LT. 0) THEN
C         variable uses a parameter
          PAR= -VDIM(I)
          DO 10 J= 1, MXSECT
            IF (VARUSE(VARCNT,J) .GE. 1) THEN
              PARUSE(PAR,J)= 1
              IF (PARFIL(PAR) .GE. 1) THEN
                PARFUS(PARFIL(PAR),J)= 1
              END IF
            END IF
 10       CONTINUE
        END IF
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   VARSZE
     I                   (VDIM,VTYP,PARCNT,PARVAL,
     O                    VSIZ)
C
C     + + + PURPOSE + + +
C     calculate the amount of space needed by a variable
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     VDIM(4),PARCNT,PARVAL(PARCNT),VSIZ
      CHARACTER*1 VTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VDIM   - dimensions of variable
C     VTYP   - type of variable
C     PARCNT - number of declared parameters
C     PARVAL - values of parameters
C     VSIZ   - size of variable (words)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,LDIM(4)
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1, 4
        IF (VDIM(I) .GE. 0) THEN
          LDIM(I)= VDIM(I)
        ELSE
          LDIM(I)= PARVAL(-VDIM(I))
        END IF
 10   CONTINUE
C          
      IF (LDIM(4) .GT. 0) THEN
C       4th var
        VSIZ= LDIM(1)*LDIM(2)*LDIM(3)*LDIM(4)
      ELSE IF (LDIM(3) .GT. 0) THEN
C       3d var
        VSIZ= LDIM(1)*LDIM(2)*LDIM(3)
      ELSE IF (LDIM(2) .GT. 0) THEN
C       2d var
        VSIZ= LDIM(1)*LDIM(2)
      ELSE IF (LDIM(1) .GT. 0) THEN
C       1st var
        VSIZ= LDIM(1)
      ELSE
C       scalar
        VSIZ= 1
      END IF
C
      IF (VTYP .EQ. 'D') THEN
C       double prec, adjust size
        VSIZ= VSIZ*2
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DCLVAR
     I                   (OFL,CNT,TYP,NM1,DIM,USE,PARCNT,PARNAM)
C
C     + + + PURPOSE + + +
C     write out variable declarations
C
C     + + + DUMMY VARIABLES + + +
      INTEGER     OFL,CNT,DIM(4,CNT),USE(CNT),PARCNT
      CHARACTER*1 NM1(6,CNT),TYP(CNT)
      CHARACTER*6 PARNAM(PARCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OFL    - output file unit number
C     CNT    - count of variables to write out
C     TYP    - type of each variable
C     NM1    - each variables name
C     DIM    - each variables dimensions
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      IPOS,ICNT,J,K,I0,LINCNT,BLKLEN
      CHARACTER*24 BLKNAM
      CHARACTER*80 BUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (BUFF,BUF1)
      CHARACTER*1  BUF1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL     ADTOBF
C
C     + + + OUTPUT FORMATS + + +
 2400 FORMAT(80A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
C
C     declare integers
      BUFF= '      INTEGER'
      BLKNAM(1:20)= BUFF(1:20)
      BLKLEN= 20
      LINCNT= 1
      IPOS  = BLKLEN+ 1
      ICNT  = 0
      DO 10 J= 1,CNT
        IF (TYP(J).EQ.'I' .AND. USE(J).EQ.1) THEN
C         process me
          ICNT= ICNT+ 1
          CALL ADTOBF (NM1(1,J),DIM(1,J),OFL,I0,BLKNAM,BLKLEN,PARCNT,
     I                 PARNAM,
     M                 IPOS,BUF1,BUFF,LINCNT)
          BUF1(IPOS)= ','
          IPOS= IPOS+ 1
        END IF
 10   CONTINUE
      IF (ICNT .GT. 0) THEN
C       clear out final buffer
        WRITE (OFL,2400) (BUF1(K),K=1,IPOS-2)
      END IF
C
C     declare reals
      BUFF= '      REAL   '
      BLKNAM(1:20)= BUFF(1:20)
      BLKLEN= 20
      LINCNT= 1
      IPOS  = BLKLEN+ 1
      ICNT  = 0
      DO 20 J= 1,CNT
        IF (TYP(J).EQ.'R' .AND. USE(J).EQ.1) THEN
C         process me
          ICNT= ICNT+ 1
          CALL ADTOBF (NM1(1,J),DIM(1,J),OFL,I0,BLKNAM,BLKLEN,PARCNT,
     I                 PARNAM,
     M                 IPOS,BUF1,BUFF,LINCNT)
          BUF1(IPOS)= ','
          IPOS= IPOS+ 1
        END IF
 20   CONTINUE
      IF (ICNT .GT. 0) THEN
C       clear out final buffer
        WRITE (OFL,2400) (BUF1(K),K=1,IPOS-2)
      END IF
C
C     declare doubles
      BUFF= '      DOUBLE PRECISION'
      BLKNAM(1:23)= BUFF(1:23)
      BLKLEN= 24
      LINCNT= 1
      IPOS  = BLKLEN+ 1
      ICNT  = 0
      DO 30 J= 1,CNT
        IF (TYP(J).EQ.'D' .AND. USE(J).EQ.1) THEN
C         process me
          ICNT= ICNT+ 1
          CALL ADTOBF (NM1(1,J),DIM(1,J),OFL,I0,BLKNAM,BLKLEN,PARCNT,
     I                 PARNAM,
     M                 IPOS,BUF1,BUFF,LINCNT)
          BUF1(IPOS)= ','
          IPOS= IPOS+ 1
        END IF
 30   CONTINUE
      IF (ICNT .GT. 0) THEN
C       clear out final buffer
        WRITE (OFL,2400) (BUF1(K),K=1,IPOS-2)
      END IF
C
      RETURN
      END
