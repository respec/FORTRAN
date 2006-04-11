C
C
C
      PROGRAM   CH2DRV
C
C     + + + PURPOSE + + +
C     driver for new Ch2 modules - report and bmprac - testing code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FILES(15),I,I0,WDMFL,OPID,OPTYP,IDNO,SREC,EREC,
     $             RETCOD,IOPT,MKFILS,SCLU,SGRP,KEY,LIKE,J,
     $             OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,
     $             TCLU,TGRP,KGRP,IVAL,IFUN,I1,I2
      LOGICAL      LFLAG
      CHARACTER*64 HMSNAM,WDNAME,ECHNAM
      CHARACTER*1  CHSTR1(20)
      CHARACTER*8  ITMNAM
      CHARACTER*10 CTXT10
      CHARACTER*12 CSCEN,TNAME
      CHARACTER*20 CTXT
      CHARACTER*64 FNAME
      CHARACTER*80 CBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBFIN, XGTARG, FILOPN, FILBLK, HSPF, HDMESI, SCCLAL
      EXTERNAL     ZIPI, DSSCLO, UCIINP, HDMESC, HDMEST, WDBOPN, DELML
      EXTERNAL     HAOPSQ, HADD, ADDOPB, ESTADD, HGETI, ADDFIL, GETML
      EXTERNAL     HGETC, GTINS, PBMPAR, DELBMP
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A64)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I2 = 2
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
      WDNAME= 'shena.wdm'
      CALL WDBOPN (WDMFL,WDNAME,I,
     O             RETCOD)
C
      CSCEN = 'bmpbase.uci'
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

C     do any report operations exist?
      OPID = 0
      OPTYP= 11
 10   CONTINUE
        CALL GETNXT (OPTYP,
     M               OPID)
        IF (OPID.GT.0) THEN
          WRITE(99,*) 'FOUND REPORT OPERATION NUMBER ',OPID
        END IF
      IF (OPID.GT.0) GO TO 10

C     do any bmprac operations exist?
      OPID = 0
 40   CONTINUE
        CALL GETNXT (OPTYP,
     M               OPID)
        IF (OPID.GT.0) THEN
          WRITE(99,*) 'FOUND BMPRAC OPERATION NUMBER ',OPID
        END IF
      IF (OPID.GT.0) GO TO 40
C
C     get a list of reaches and reach descriptions
 50   CONTINUE
        ITMNAM = 'RCHID   '
        CALL HGETI (ITMNAM,I0,
     O              OPID)
        write(99,*) 'found reach ',opid
      IF (OPID.NE.-99) GO TO 50
      ITMNAM = 'RCHDESC '
      I = 680
      CALL HGETC (ITMNAM,I,
     O            CTXT)
      write(99,*) 'found reach ',i,ctxt
      ITMNAM = 'PERDESC '
      I = 192
      CALL HGETC (ITMNAM,I,
     O            CTXT)
      write(99,*) 'found PERLND ',i,ctxt
      ITMNAM = 'BMPDESC '
      I = 680
      CALL HGETC (ITMNAM,I,
     O            CTXT)
      write(99,*) 'found BMPRAC ',i,ctxt
C
C     get a list of operations input to this reach from schematic block
      INIT = 1
 60   CONTINUE
        CALL GTINS (INIT,I,I1,
     O              CTXT10,RAREA)
        write(99,*) 'found input ',ctxt10,rarea
        INIT = 0
      IF (CTXT10.NE.'       -99') GO TO 60
C
C     get a list of operations input to this bmprac from schematic block
      INIT = 1
 70   CONTINUE
        CALL GTINS (INIT,I,I2,
     O              CTXT10,RAREA)
        write(99,*) 'found input ',ctxt10,rarea
        INIT = 0
      IF (CTXT10.NE.'       -99') GO TO 70
C
C     change some schematic block records
      I = 191
      RAREA = 80.0
      J = 680
      CALL PBMPAR ('PERLND',I,RAREA,'BMPRAC',J,RETCOD)
      I = 191
      RAREA = 20.0
      J = 680
      CALL PBMPAR ('PERLND',I,RAREA,'RCHRES',J,RETCOD)
      I = 192
      RAREA = 88.0
      J = 680
      CALL PBMPAR ('PERLND',I,RAREA,'BMPRAC',J,RETCOD)
      I = 192
      RAREA = 0.0
      J = 680
      CALL PBMPAR ('PERLND',I,RAREA,'RCHRES',J,RETCOD)
      I = 193
      RAREA = 0.0
      J = 680
      CALL PBMPAR ('PERLND',I,RAREA,'BMPRAC',J,RETCOD)
      I = 193
      RAREA = 999.0
      J = 680
      CALL PBMPAR ('PERLND',I,RAREA,'RCHRES',J,RETCOD)
C
C     delete a bmp
      CALL DELBMP (J)
C
C     now output modified uci file
      OPEN(UNIT=23,FILE='UCI.OUT')    ! open uci file
      CALL UCISAV(23)
      CLOSE (UNIT=23)      ! close uci file

      STOP
      END
