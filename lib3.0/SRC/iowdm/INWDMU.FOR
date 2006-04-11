C
C
C
      SUBROUTINE   INALOC
     I                   ( MESSFL, FE, WNAME,
     M                     ALLOC )
C
C     + + + PURPOSE + + +
C     Allocate pointer space for a new data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ALLOC(5), MESSFL, FE
      CHARACTER*16 WNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WNAME  - window name
C     FE     - Fortran unit number for messages and warnings
C     ALLOC  - array containing space allocations
C              (1) number of attributes (nsa)
C              (2) total space for attributes (nsasp)
C              (3) number of group pointers (ndp)
C              (4) number of upward pointers (ndn)
C              (5) number of downward pointers (nup)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ALLOCD(5), LEN, AGAIN, SGRP, SCLU, NUM, RTCMND,
     $          SPACE, ALOCMX
C
C     + + + LOCAL DEFINITIONS + + +
C     ALLOCD - default allocation
C     ALOCMX - maximum space to allor for pointers and attributes
C     AGAIN  - indicator to repeat question
C              0 - do not repeat
C              1 - repeat question
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYI
      EXTERNAL   ZWNSET, Q1INIT, QSETI, Q1EDIT, QGETI, PRNTXT
C
C     + + + DATA INITIALIZATIONS + + +
C                   nsa nsasp ndp ndn nup
      DATA  ALLOCD / 50,  90, 150,  1,  1 /
      DATA  SCLU, ALOCMX
     $     / 104,    400 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ALLOC(1) .EQ. 0) THEN
C       initialize allocation to defaults
        LEN = 5
        CALL COPYI ( LEN, ALLOCD, ALLOC )
      END IF
 100  CONTINUE
C       set window name
        CALL ZWNSET ( WNAME )
C       ask user for space allocation
        AGAIN = 0
        SGRP = 21
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        NUM = 3
        CALL QSETI ( NUM, ALLOC )
        CALL Q1EDIT ( RTCMND )
        IF (RTCMND .EQ. 1) THEN
C         user wants to continue, get values
          CALL QGETI ( NUM, ALLOC )
C         check allocation for problems
          SPACE = 2 * ALLOC(1)  +  ALLOC(2)  + ALLOC(3)
          IF (SPACE .GT. ALOCMX  .AND.  ALLOC(1) .GT. ALLOC(2)) THEN
C           total space exceeded, # attributes exceeds attribute space
            SGRP = 25
            CALL PRNTXT ( MESSFL, SCLU, SGRP )
            AGAIN = 1
          ELSE IF (SPACE .LT. ALOCMX) THEN
C           total space exceeded
            SGRP = 26
            CALL PRNTXT ( MESSFL, SCLU, SGRP )
            AGAIN = 1
          ELSE IF (ALLOC(1) .GT. ALLOC(2)) THEN
C           # attributes exceeds attribute space
            SGRP = 27
            CALL PRNTXT ( MESSFL, SCLU, SGRP )
            AGAIN = 1
          END IF
        ELSE
C         user does not want to contiue
          AGAIN = 0
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INBCA
     I                  ( MESSFL, WDMFL, FE, DSN,
     I                    ABVAL, KNDX, INDEX, ABNDX, ABNAME,
     O                    RETC )
C
C     + + + PURPOSE + + +
C     Write basin characteristics attriubes to data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, FE, DSN, KNDX, INDEX(KNDX), ABNDX(200),
     $          RETC
      REAL      ABVAL(KNDX)
      CHARACTER*1 ABNAME(6,200)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     WDMFL  - Fortran unit number of the wdm data file
C     FE     - Fortran unit number for summary ouput,
C     DSN    - data-set number for the attributes
C     ABVAL  - values of attributes to be added
C     KNDX   - number of attributes to be added
C     INDEX  - array of pointers to the ABNDX array for attribute
C              index numbers
C     ABNDX  - array containing the corresponding attribute index
C              number for the 200 basin characteristics
C     ABNAME - array containing the corresponding attribute name
C              for the 200 basin characteristics
C     RETC   - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   K, NDX, LEN, I, RET
C
C     + + + LOCAL DEFINITIONS + + +
C     RETC   - return code from putting attributes
C               -81 - data set does not exist
C              -102 - attribute already in data set
C              -103 - no room in data set for attribute
C              -104 - data present, can't update attribute
C              -109 - incorrect real value for attribute
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDBSAR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (//,2X, 'DSN', I6, ':    adding basin characteristics ',
     $               'attributes',
     $        //,2X, '              Name    Value' )
 2002 FORMAT (   2X, '              ', 6A1, 2X, G12.6 )
 2012 FORMAT (   2X, '    --->      ', 6A1, 2X, 'return code = ', I3,
     $               ',  value =', G12.6 )
C
C     + + + END SPECIFICATIONS + + +
C
C     write out heading and dsn
      WRITE (FE,2000) DSN
C
C     add basin characteristics attributes
      RETC = 0
      LEN = 1
      DO 100 K = 1, KNDX
        NDX = INDEX(K)
        CALL WDBSAR (WDMFL, DSN, MESSFL, ABNDX(NDX), LEN, ABVAL(K),
     O               RET )
        IF (RET .EQ. 0) THEN
C         document the add
          WRITE (FE,2002) (ABNAME(I,NDX),I=1,6), ABVAL(K)
        ELSE
C         error in putting attribute
          WRITE (FE,2012) (ABNAME(I,NDX),I=1,6), RET, ABVAL(K)
          RETC = RETC + 1
        END IF
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   INSVTB
     I                   ( MESSFL, WDMFL, DSN,
     I                     MXCOL, MXROW, TRBUFF,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Saves the table array to a table data set.
C     Warning and error messages are written to Fortran unit 99.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, DSN, MXCOL, MXROW, RETC
      REAL      TRBUFF(MXCOL,MXROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of file containig the description
C              of the table template
C     WDMFL  - Fortran unit number of the wdm file
C     DSN    - data-set number for data
C     RETC   - return code
C              0 - successful
C
C     + + + INCLUDES + + +
      INCLUDE 'ciowtb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RET, TABID, ACLU, AGRP,
     $             TFLD, TNUM(4), TLEN(30), TCOL(30), TSPA,
     $             AFLD, ANUM(4), ALEN(30), ACOL(30), ASPA,
     $             FROW, NROWS, FFLD,
     $             TBCLU, TBGRP, DATFLG, TGRPPT
      CHARACTER*1  MFID(2), TTYP(30), ATYP(30)
      CHARACTER*16 TNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDTBTM, WTBPUT
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT (1X,'in insvtb, after call to wdtbtm',
     $      /,1X,'         ', I5, ' = dsn',
     $      /,1X,'         ', I5, ' = wdmfl',
     $      /,1X,'         ', I5, ' = tabid',
     $      /,1X,'         ', I5, ' = npt' )
 2002 FORMAT (1X,'         ', I5, ' = messfl',
     $      /,1X,'         ', 2A1, '= mfid'
     $      /,1X,'         ', I5, ' = tbclu',
     $      /,1X,'         ', I5, ' = tbgrp' )
 2003 FORMAT (1X,'         ', I5, ' = retc' )
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  MFID, TABID, TBCLU, TBGRP, DATFLG, FROW, FFLD
     $    / 'X','X',  1,    20,     1,      1,    1,    1 /
C
C     + + + END SPECIFICATIONS + + +
C
      RETC = 0
C     add table template to data set
      CALL WDTBTM ( MESSFL, MFID, TBCLU, TBGRP, WDMFL, DSN,
     I              TABID, NPT,
     O              TFLD, TNUM, TTYP, TLEN, TCOL, TSPA,
     O              TNAM, TGRPPT,
     O              AFLD, ANUM, ATYP, ALEN, ACOL, ASPA, ACLU, AGRP,
     O              RET )
      IF (RET .NE. 0) THEN
C       problem adding template
        RETC = RET
        WRITE (99,2001) DSN, WDMFL, TABID, NPT
        WRITE (99,2002) MESSFL, MFID, TBCLU, TBGRP
        WRITE (99,2003) RETC
      ELSE
C       template added, now add table
        NROWS = NPT
        TFLD = 14
        CALL WTBPUT ( WDMFL, DSN, TNAM, TABID, DATFLG,
     I                FROW, NROWS, FFLD, TFLD, TRBUFF,
     O                RET )
        IF (RET .NE. 0) THEN
C         problem adding table of data
          RETC = RET
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INRDST
     I                   ( MESSFL, LPTH, PTHNAM, FLAG, STA, DSN )
C
C     + + + PURPOSE + + +
C     Prints a status report to screen for the input process.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL, LPTH, FLAG, DSN
      CHARACTER*8   PTHNAM(LPTH)
      CHARACTER*15  STA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     LPTH   - number of window paths
C     PTHNAM - array of window paths
C     FLAG   - indicator flag for status updating
C              1 - new status
C              2 - continue status
C     STA    - station being processed
C     DSN    - date set being processed
C
C     + + + SAVES + + +
      INTEGER      KOUNT
      SAVE         KOUNT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU, SGRP, MAXL, SCNIN, SCNIC, IWRT, CNUM, CLEN(2),
     $             JUSTL, LEN, LENO, I
      CHARACTER*1  CTXT(20)
C
C     + + + EXTERNALS + + +
      EXTERNAL     INTCHR, PMXTXA, ZWNSOP
C
C     + + + DATA INITIALZATIONS + + +
      DATA   SCLU, SGRP, MAXL, SCNIN, SCNIC, IWRT, CNUM, CLEN, JUSTL
     $     /  104,    5,    1,     1,    -1,    1,    2, 15,5,     1 /
      DATA   KOUNT
     $     /     0 /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 15A1 )
C
C     + + + END SPECIFICATIONS + + +
C
C     get text for write
      READ (STA,1000) (CTXT(I), I = 1, 15)
      LEN = 5
      CALL INTCHR ( DSN, LEN, JUSTL, LENO, CTXT(16) )
C     write message
      IF (FLAG .EQ. 1  .OR.  KOUNT .GT. 49) THEN
C       new status or full screen buffer
        CALL ZWNSOP ( LPTH, PTHNAM )
        CALL PMXTXA ( MESSFL, SCLU, SGRP, MAXL,
     I                SCNIN, IWRT, CNUM, CLEN, CTXT )
        KOUNT = 1
      ELSE IF (FLAG .EQ. 2) THEN
C       continue status
        CALL PMXTXA ( MESSFL, SCLU, SGRP, MAXL,
     I                SCNIC, IWRT, CNUM, CLEN, CTXT )
        KOUNT = KOUNT + 1
      END IF
C
      RETURN
      END
