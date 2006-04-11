C
C
C
      PROGRAM   REPORT
C
C     + + + PURPOSE + + +
C     driver for new Ch2 report modules - testing code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FILES(15),I,I0,WDMFL,
     $             RETCOD,IOPT,SCLU,SGRP,REPID
      LOGICAL      LFLAG
      CHARACTER*64 HMSNAM,WDNAME,ECHNAM
      CHARACTER*12 CSCEN
C
      INTEGER      REPEXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBFIN, XGTARG, FILOPN, FILBLK, HSPF, HDMESI, SCCLAL
      EXTERNAL     ZIPI, DSSCLO, UCIINP, HDMESC, HDMEST, WDBOPN, DELML
      EXTERNAL     HAOPSQ, HADD, ADDOPB, ESTADD, HGETI, ADDFIL, GETML
      EXTERNAL     HGETC, GTINS, PBMPAR, DELBMP, ADDREPT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A64)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C     avoid some lahey math errors
      LFLAG = .TRUE.
      CALL INVALOP (LFLAG)
      CALL UNDFL (LFLAG)
C
      OPEN (UNIT=99,FILE='error.fil')
C
C     initialize wdm file common block
      CALL WDBFIN
C
C     open message file
      I = 1
      INCLUDE 'fhsmes.inc'
      CALL WDBOPN (MESSFL,HMSNAM,I,
     O             RETCOD)
C
C     open wdm file
      I = 0
      WDMFL = 24
      WDNAME= 'nfulton.wdm'
      CALL WDBOPN (WDMFL,WDNAME,I,
     O             RETCOD)
C
      CSCEN = 'base.uci'
      OPEN(UNIT=23,FILE=CSCEN,STATUS='OLD')

      CALL SCCLAL
      IOPT = 1
      SCLU = 201
      SGRP = 50
      CALL HDMESC (IOPT,MESSFL,SCLU,SGRP,CSCEN)

      FILES= I0
      FILES(15) = MESSFL
      FILES(11) = WDMFL
      CALL FILSET (MESSFL,23,WDMFL,
     M             FILES,
     O             ECOUNT,RETCOD)

      INQUIRE(UNIT=FILES(1),NAME=ECHNAM)

      IF (RETCOD .EQ. 0) THEN
        IOPT  = 1
        CALL HDMES3(IOPT,'GenScnActivate HSPF')
        CALL INTERP (I0,I0,
     M               FILES,
     O               UNITFG,RETCOD)
        IOPT  = 99
        CALL HDMES3(IOPT,' ')
      END IF

      CLOSE (UNIT=23)      ! close users input file
C
C     all code preceeding this point happens as scenario is activated

      REPID = 1
      IEXT =  REPEXT(REPID)
      if (iext.eq.1) then
        write(99,*) 'report exists ',repid
      else
        write(99,*) 'report does not exist ',repid, IEXT
      end if
C
      CALL DELREPT (7)
      CALL ADDREPT (MESSFL,7)
      CALL DELREPT (7)
      CALL ADDREPT (MESSFL,7)
C
C     now output modified uci file
      OPEN(UNIT=23,FILE='UCI.OUT')    ! open uci file
      CALL UCISAV(23)
      CLOSE (UNIT=23)      ! close uci file

      STOP
      END
