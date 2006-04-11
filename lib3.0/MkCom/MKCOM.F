C
C
C
      PROGRAM   MKCOM
C
C     + + + PURPOSE + + +
C     make a common block from a data structure
C
C     + + + PARAMETERS + + +
      INTEGER      MXVAR,MXSECT,MXHEAD,MXEQU,MXDUM
      PARAMETER   (MXVAR=2000,MXSECT=13,MXHEAD=20,MXEQU=500,MXDUM=50)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,L,IPOS,ILEN,TPOS,LINCNT,BLKLEN,IBCK,I1,I0,I6,
     #             I16,SPAFG,DIMONE,INPFL,OUTFL,SECFL,SPAFL,SPEFL,NFL,
     #             SPECVL(5),SECSFL,DUMDIM(3),DUMFLG,DUMSIZ(MXDUM),
     #             DUMCUR,SIZREM,SIZTOT,SIZSTR,SIZCUR,SIZNOW,LASTVR,
     #             LASTSZ,VARCNT,VARDIM(3,MXVAR),VARUSE(MXVAR,MXSECT),
     #             EQUCNT,EQUDIM(3,MXEQU),EQUUSE(MXEQU,MXSECT),HEACNT,
     #             HEASEC(MXHEAD),SPACNT,SPADIM(3,MXEQU),SECCNT,SPAPTR,
     #             SECSIZ(MXSECT),SECSKP(MXSECT,MXSECT),DUMCNT,EQUPTR,
     #             VARADD(MXVAR),EQUADD(MXEQU),SPADD(MXEQU)
      CHARACTER*1  VARTYP(MXVAR),VARCOD(MXVAR),EQUTYP(MXEQU),CR
      CHARACTER*6  VARNAM(MXVAR),VARDEP(MXVAR),COMNAM,MODNAM,
     #             EQUNAM(MXEQU),DUMNAM(MXDUM),SPANAM(MXEQU)
      CHARACTER*12 FNAME,SECNAM(MXSECT)
      CHARACTER*16 VARASO(MXVAR),EQUASO(MXEQU)
      CHARACTER*24 TMPNAM,BLKNAM
      CHARACTER*80 BUFF,OBUFF
      CHARACTER*75 HEADER(MXHEAD)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (HEADER,HEADR1),(COMNAM,COMNM1),(BUFF,BUF1),
     #            (TMPNAM,TMPNM1),(VARNAM,VARNM1),(DUMNAM,DUMNM1),
     #            (OBUFF,OBUF1),(EQUNAM,EQUNM1),(EQUASO,EQUAS1),
     #            (BLKNAM,BLKNM1)
      CHARACTER*1  HEADR1(75,MXHEAD),COMNM1(6),BUF1(80),
     #             TMPNM1(24),VARNM1(6,MXVAR),DUMNM1(6,MXDUM),
     #             OBUF1(80),EQUNM1(6,MXEQU),EQUAS1(16,MXEQU),
     #             BLKNM1(24)
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LENSTR,ADTOBF,VARSZE,DCLVAR,COPYC,INTCHR
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (A80)
1010  FORMAT (A6,3I4,2(1X,A1),1X,A16,1X,A6,1X,20I1)
1012  FORMAT (A6,3I4,1X,A1,3X,A16,8X,20I1)
1015  FORMAT (A6,3I4)
1020  FORMAT (2I10)
1030  FORMAT (I2)
1040  FORMAT (I2,1X,20I1)
1050  FORMAT (I1)
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT ('C     from ',A12)
2010  FORMAT ('C     ',75A1)
2020  FORMAT ('C     start byte: ',I6,' word:',I5)
2090  FORMAT ('C     end byte: ',I6,' word:',I5)
2200  FORMAT (1X,A6,':',A16,1X,I5,1X,A6)
2300  FORMAT (A6,3I4,1X,I5,' ')
2350  FORMAT (A6,I5)
2400  FORMAT (80A1)
2500  FORMAT (1X,A6,5I5)
2520  FORMAT (A12,2I5)
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
      I16= 16
      CR= 'R'
C
      VARCNT= 0
      EQUCNT= 0
      HEACNT= 0
      SECCNT= 0
C     default common name
      COMNAM= 'SCRTCH'
C     default module name
      MODNAM= ' '
C     defualt dummy dimensions
      DUMDIM(1)= 0
      DUMDIM(2)= 0
      DUMDIM(3)= 0
C     nothing known about special actions
      SPECVL(1)= 0
      SPECVL(2)= 0
      SPECVL(3)= 0
      SPECVL(4)= 0
      SPECVL(5)= 0
C
 10   CONTINUE
        READ (INPFL,1000,END=100) BUFF
        IF (BUFF(1:2) .EQ. 'VA') THEN
C         variable
          VARCNT= VARCNT+ 1
          READ (BUFF(4:4),1050) SPAFG
          READ (BUFF(6:80),1010) VARNAM(VARCNT),
     #                          (VARDIM(I,VARCNT),I=1,3),
     #                           VARTYP(VARCNT),
     #                           VARCOD(VARCNT),
     #                           VARASO(VARCNT),
     #                           VARDEP(VARCNT),
     #                          (VARUSE(VARCNT,J),J=1,MXSECT)
C
          VARADD(VARCNT)= SIZCUR
          EQUPTR= SIZCUR
          SPAPTR= SIZCUR
          IF (VARUSE(VARCNT,SECCNT) .NE. 2) THEN
C           calc how much space for this variable
            CALL VARSZE (VARDIM(1,VARCNT),VARTYP(VARCNT),
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
            WRITE(OUTFL,2200) MODNAM,VARASO(VARCNT),
     $                        VARADD(VARCNT),VARDEP(VARCNT)
          END IF
C
          IF ( (SPAFG .EQ. 1) .OR. (SPAFG .EQ. 3) ) THEN
C           info for special actions
            IF (VARTYP(VARCNT) .EQ. 'D') THEN
C             double precision - make first subscript negative
              IF (VARDIM(1,VARCNT) .EQ. 0) THEN
C               no subscript - make minus one
                DIMONE= -1
              ELSE
C               reverse sign
                DIMONE= -VARDIM(1,VARCNT)
              END IF
            ELSE
C             single size - leave first subscript alone
              DIMONE= VARDIM(1,VARCNT)
            END IF
            WRITE(OBUFF,2300) VARNAM(VARCNT),DIMONE,
     $                        (VARDIM(I,VARCNT),I=2,3),VARADD(VARCNT)
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
            ILEN= 80
            WRITE(SPAFL,2400) (OBUF1(K),K=1,LENSTR(ILEN,OBUF1))
          END IF
          IF (SPAFG .GE. 2) THEN
C           write variable to number file
            WRITE(NFL,2350) VARNAM(VARCNT), VARADD(VARCNT)
          END IF
C
        ELSE IF (BUFF(1:2) .EQ. 'EQ') THEN
C         equivalence
          EQUCNT= EQUCNT+ 1
          READ(BUFF(4:4),1050) SPAFG
          READ(BUFF(6:80),1012) EQUNAM(EQUCNT),
     $                         (EQUDIM(I,EQUCNT),I=1,3),
     $                          EQUTYP(EQUCNT),
     $                          EQUASO(EQUCNT),
     $                         (EQUUSE(EQUCNT,J),J=1,MXSECT)
C
          EQUADD(EQUCNT)= EQUPTR
          CALL VARSZE (EQUDIM(1,EQUCNT),EQUTYP(EQUCNT),
     O                 SIZNOW)
          EQUPTR= EQUPTR+ SIZNOW
          IF ( (SPAFG .EQ. 1) .OR. (SPAFG .EQ. 3) ) THEN
C           info for special actions
            IF (EQUTYP(EQUCNT) .EQ. 'D') THEN
C             double precision - make first subscript negative
              IF (EQUDIM(1,EQUCNT) .EQ. 0) THEN
C               no subscript - make minus one
                DIMONE= -1
              ELSE
C               reverse sign
                DIMONE= -EQUDIM(1,EQUCNT)
              END IF
            ELSE
C             single size - leave first subscript alone
              DIMONE= EQUDIM(1,EQUCNT)
            END IF
            WRITE (OBUFF,2300)  EQUNAM(EQUCNT),DIMONE,
     #                         (EQUDIM(I,EQUCNT),I=2,3),EQUADD(EQUCNT)
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
            ILEN= 80
            WRITE(SPAFL,2400) (OBUF1(K),K=1,LENSTR(ILEN,OBUF1))
          END IF
C
        ELSE IF (BUFF(1:2) .EQ. 'SP') THEN
C         special action name
          SPACNT= SPACNT+ 1
          READ (BUFF(4:4),1050) SPAFG
          READ (BUFF(6:23),1015)  SPANAM(SPACNT),
     #                           (SPADIM(I,SPACNT),I=1,3)
C
          SPADD(SPACNT)= SPAPTR
          CALL VARSZE (SPADIM(1,SPACNT),CR,
     O                 SIZNOW)
          SPAPTR= SPAPTR+ SIZNOW
          IF ( (SPAFG .EQ. 1) .OR. (SPAFG .EQ. 3) ) THEN
C           info for special actions
            WRITE(OBUFF,2300)  SPANAM(SPACNT),
     #                        (SPADIM(I,SPACNT),I=1,3),SPADD(SPACNT)
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
            ILEN= 80
            WRITE (SPAFL,2400) (OBUF1(K),K=1,LENSTR(ILEN,OBUF1))
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
          DO 90 I= 1,SECCNT
C           determine size of osv for each section
            J= ((SECSIZ(I)- 1) / 500)+ 1
            WRITE(SECSFL,2520) SECNAM(I),SECSIZ(I),J
C           open output file for this section
            FNAME= SECNAM(I)
            OPEN(UNIT=SECFL,FILE=FNAME)
C           echo name
            WRITE(SECFL,2000) FNAME
C           headers
            DO 60 J= 1,HEACNT
              IF (HEASEC(J).EQ.0  .OR. HEASEC(J).EQ.I) THEN
C               write this header
                L= 75
                WRITE(SECFL,2010)(HEADR1(K,J),K=1,LENSTR(L,HEADR1(1,J)))
              END IF
 60         CONTINUE
C
            IF (SIZSTR .GT. 0) THEN
C             indicate not starting at 0
              WRITE(SECFL,2020) SIZSTR*4,SIZSTR
            END IF
C           all variables in common
            BUFF= '      COMMON/'
            IPOS= 14
            ILEN= LENSTR(I6,COMNM1)
            CALL COPYC(ILEN,COMNM1,BUF1(IPOS))
            IPOS= IPOS+ ILEN
            BUF1(IPOS)= '/'
            BLKNAM(1:IPOS)= BUFF(1:IPOS)
            BLKLEN= IPOS
            LINCNT= 0
            IPOS  = IPOS+ 1
            DUMFLG= 0
            DUMCUR= 0
            DUMCNT= 0
            DO 65 J= 1, VARCNT
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
     I                         BLKNAM,BLKLEN,
     M                         IPOS,BUF1,BUFF,LINCNT)
                  BUF1(IPOS)= ','
                  IPOS= IPOS+ 1
                END IF
C               in use
                CALL ADTOBF (VARNM1(1,J),DUMDIM,SECFL,I0,
     I                       BLKNAM,BLKLEN,
     M                       IPOS,BUF1,BUFF,LINCNT)
                BUF1(IPOS)= ','
                IPOS= IPOS+ 1
              ELSE IF (VARUSE(J,I).NE.2) THEN
C               not in use, not sure about skip, need to skip?
                K= 0
C               used in prev sections?
 62             CONTINUE
                  K= K+ 1
                IF (VARUSE(J,K).EQ.0 .AND. K.LT.I) GO TO 62
C
                IF (SECSKP(K,I) .NE. 1) THEN
C                 not skipping, increment current dummy var
                  CALL VARSZE(VARDIM(1,J),VARTYP(J),
     O                        SIZNOW)
C                 increment current dummy size
                  DUMCUR= DUMCUR+ SIZNOW
                  DUMFLG= 1
                END IF
              END IF
 65         CONTINUE
C
C           dummy variable to ending position
C
CTHJ            SIZREM= SIZTOT- SECSIZ(I)+ 1
C
C           find last variable used
            LASTVR= VARCNT+ 1
 67         CONTINUE
              LASTVR= LASTVR- 1
            IF (VARUSE(LASTVR,I) .NE. 1) GO TO 67
C
C           find address of end of last variable
            CALL VARSZE (VARDIM(1,LASTVR),VARTYP(LASTVR),
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
     I                     BLKNAM,BLKLEN,
     M                     IPOS,BUF1,BUFF,LINCNT)
            END IF
C           declare variables
            CALL DCLVAR (SECFL,VARCNT,VARTYP,VARNM1,VARDIM,
     I                   VARUSE(1,I))
C
            IF (DUMCNT .GT. 0) THEN
C             declare dummys
              BUFF= '      INTEGER'
              BLKNAM(1:20)= BUFF(1:20)
              BLKLEN= 20
              LINCNT= 1
              IPOS  = 21
              DO 82 J= 1,DUMCNT
                IF (DUMSIZ(J) .GT. 1) THEN
C                 a dummy array
                  DUMDIM(1)= DUMSIZ(J)
                END IF
                CALL ADTOBF (DUMNM1(1,J),DUMDIM,SECFL,I0,
     I                       BLKNAM,BLKLEN,
     M                       IPOS,BUF1,BUFF,LINCNT)
C               assume not an array
                DUMDIM(1) = 0
                BUF1(IPOS)= ','
                IPOS= IPOS+ 1
 82           CONTINUE
C             clear out final buffer
              WRITE(SECFL,2400) (BUF1(K),K=1,IPOS-2)
            END IF
C
            IF (EQUCNT .GT. 0) THEN
C             equivalence
              BUFF= 'C'
              WRITE(SECFL,2400) BUF1(1)
              BUFF= '      EQUIVALENCE '
              BLKNAM(1:20)= BUFF(1:20)
              BLKLEN= 20
              LINCNT= 1
              IPOS  = 21
              DO 84 J= 1,EQUCNT
                IF (EQUUSE(J,I) .EQ. 1) THEN
                  TMPNM1(1)= '('
                  ILEN= LENSTR(I6,EQUNM1(1,J))
                  TPOS= 2
                  CALL COPYC(ILEN,EQUNM1(1,J),TMPNM1(TPOS))
                  TPOS= TPOS+ ILEN
                  TMPNM1(TPOS)= ','
                  TPOS= TPOS+ 1
                  ILEN= LENSTR(I16,EQUAS1(1,J))
                  CALL COPYC(ILEN,EQUAS1(1,J),TMPNM1(TPOS))
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
                    WRITE(SECFL,2400) (BUF1(ILEN),ILEN=1,IPOS-IBCK)
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
 84           CONTINUE
C             clear out final buffer
              WRITE(SECFL,2400) (BUF1(K),K=1,IPOS-2)
C             declare equival vars
              CALL DCLVAR(SECFL,EQUCNT,EQUTYP,EQUNM1,EQUDIM,EQUUSE(1,I))
            END IF
C           end
            WRITE(SECFL,2090) SIZTOT*4,SIZTOT
C           done this section
            CLOSE(UNIT=SECFL)
 90       CONTINUE
C         blank lines to other output file
          WRITE(OUTFL,*)
          WRITE(SPAFL,*)
C         reset counters
          VARCNT= 0
          EQUCNT= 0
          HEACNT= 0
          SECCNT= 0
          IF (MODNAM .NE. ' ') THEN
C           whats known about special actions for this operating module
            WRITE(SPEFL,2500) MODNAM,SPECVL
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
      GO TO 10
C
 100  CONTINUE
C
      STOP
      END
C
C
C
      SUBROUTINE   ADTOBF
     I                   (VAR,DIM,OFL,IWRT,BLKNAM,BLKLEN,
     M                    IPOS,BUF1,BUFF,LINCNT)
C
C     + + + PURPOSE + + +
C     add a variable to output buffer, write buffer if needed
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      DIM(3),OFL,IWRT,BLKLEN,IPOS,LINCNT
      CHARACTER*1  VAR(6),BUF1(80)
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
      CHARACTER*1 TMPNM1(24)
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
      ILEN= LENSTR(I6,VAR)
      CALL COPYC(ILEN,VAR,TMPNM1)
      TLEN= ILEN
      IF (DIM(1) .GT. 0) THEN
C       have a first dim
        TLEN= TLEN+ 1
        TMPNM1(TLEN)= '('
        CALL INTCHR(DIM(1),I6,I1,
     O              ILEN,TMPNM1(TLEN+1))
        TLEN= TLEN+ ILEN
        IF (DIM(2) .GT. 0) THEN
C         second dim also
          TLEN= TLEN+ 1
          TMPNM1(TLEN)= ','
          CALL INTCHR(DIM(2),I6,I1,
     O                ILEN,TMPNM1(TLEN+1))
          TLEN= TLEN+ ILEN
          IF (DIM(3) .GT. 0) THEN
C           second dim also
            TLEN= TLEN+ 1
            TMPNM1(TLEN)= ','
            CALL INTCHR(DIM(3),I6,I1,
     O                  ILEN,TMPNM1(TLEN+1))
            TLEN= TLEN+ ILEN
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
        WRITE(OFL,2000) (BUF1(ILEN),ILEN=1,IPOS-IBCK)
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
      CALL COPYC(TLEN,TMPNM1,BUF1(IPOS))
      IPOS= IPOS+ TLEN
C
      IF (IWRT .EQ. 1) THEN
C       force write
        WRITE(OFL,2000) (BUF1(ILEN),ILEN=1,IPOS-1)
        BUFF= ' '
        IPOS= 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   VARSZE
     I                   (VDIM,VTYP,
     O                    VSIZ)
C
C     + + + PURPOSE + + +
C     calculate the amount of space needed by a variable
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     VDIM(3),VSIZ
      CHARACTER*1 VTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VDIM   - dimensions of variable
C     VTYP   - type of variable
C     VSIZ   - size of variable (words)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (VDIM(3) .GT. 0) THEN
C       3d var
        VSIZ= VDIM(1)* VDIM(2)* VDIM(3)
      ELSE IF (VDIM(2) .GT. 0) THEN
C       2d var
        VSIZ= VDIM(1)* VDIM(2)
      ELSE IF (VDIM(1) .GT. 0) THEN
C       1d var
        VSIZ= VDIM(1)
      ELSE
C       scaler
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
     I                   (OFL,CNT,TYP,NM1,DIM,USE)
C
C     + + + PURPOSE + + +
C     write out variable declarations
C
C     + + + DUMMY VARIABLES + + +
      INTEGER     OFL,CNT,DIM(3,CNT),USE(CNT)
      CHARACTER*1 NM1(6,CNT),TYP(CNT)
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
2400  FORMAT(80A1)
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
      DO 70 J= 1,CNT
        IF (TYP(J).EQ.'I' .AND. USE(J).EQ.1) THEN
C         process me
          ICNT= ICNT+ 1
          CALL ADTOBF (NM1(1,J),DIM(1,J),OFL,I0,BLKNAM,BLKLEN,
     M                 IPOS,BUF1,BUFF,LINCNT)
          BUF1(IPOS)= ','
          IPOS= IPOS+ 1
        END IF
 70   CONTINUE
      IF (ICNT .GT. 0) THEN
C       clear out final buffer
        WRITE(OFL,2400) (BUF1(K),K=1,IPOS-2)
      END IF
C
C     declare reals
      BUFF= '      REAL   '
      BLKNAM(1:20)= BUFF(1:20)
      BLKLEN= 20
      LINCNT= 1
      IPOS  = BLKLEN+ 1
      ICNT  = 0
      DO 75 J= 1,CNT
        IF (TYP(J).EQ.'R' .AND. USE(J).EQ.1) THEN
C         process me
          ICNT= ICNT+ 1
          CALL ADTOBF (NM1(1,J),DIM(1,J),OFL,I0,BLKNAM,BLKLEN,
     M                 IPOS,BUF1,BUFF,LINCNT)
          BUF1(IPOS)= ','
          IPOS= IPOS+ 1
        END IF
 75   CONTINUE
      IF (ICNT .GT. 0) THEN
C       clear out final buffer
        WRITE(OFL,2400) (BUF1(K),K=1,IPOS-2)
      END IF
C
C     declare doubles
      BUFF= '      DOUBLE PRECISION'
      BLKNAM(1:23)= BUFF(1:23)
      BLKLEN= 24
      LINCNT= 1
      IPOS  = BLKLEN+ 1
      ICNT  = 0
      DO 80 J= 1,CNT
        IF (TYP(J).EQ.'D' .AND. USE(J).EQ.1) THEN
C         process me
          ICNT= ICNT+ 1
          CALL ADTOBF (NM1(1,J),DIM(1,J),OFL,I0,BLKNAM,BLKLEN,
     M                 IPOS,BUF1,BUFF,LINCNT)
          BUF1(IPOS)= ','
          IPOS= IPOS+ 1
        END IF
 80   CONTINUE
      IF (ICNT .GT. 0) THEN
C       clear out final buffer
        WRITE(OFL,2400) (BUF1(K),K=1,IPOS-2)
      END IF
C
      RETURN
      END
