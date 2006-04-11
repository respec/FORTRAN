C     SUBROUTINE ZSET6 ( CFLG, CSTR, NUMB)                              H
      SUBROUTINE ZSET ( CFLG, CSTR, NUMB)                               MLu
C
C     SET VARIABLES FOR DSS COMMON BLOCKS
C
C     Written by Bill Charley at HEC, 1982.
C
      CHARACTER CFLG*(*), CSTR*(*), CFLAG*4, CSTRIN*4
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSCM                                                           H
      INCLUDE 'zdsscm.h'                                                MLu
C
      COMMON /ZDSSFZ/ LFIRST
      LOGICAL LFIRST
C
      COMMON /DCDBUG/ LDCDBG, MDCUNT
      LOGICAL LDCDBG
C
C
C
      IF (LFIRST) THEN
      CALL ZINIT
      LFIRST = .FALSE.
      ENDIF
C
C
      CFLAG = CFLG
      CSTRIN = CSTR
C
      IF (MLEVEL.GE.15) WRITE (MUNIT,20) CFLAG, NUMB, CSTR              D
 20   FORMAT (T2,'-----DSS---Debug:  Enter ZSET;  Flag: ',A,            D
     * ',  Numb:',I9,',  String: ',A)                                   D
C
C     Set Program Name
      IF ((CFLAG .EQ. 'PRGN').OR.(CFLAG.EQ.'PROG')) THEN
      CPROG = CSTR
C
C     Set trace level (10 and above is an internal trace)
      ELSE IF ((CFLAG .EQ. 'MLEV').OR.(CFLAG.EQ.'MLVL')) THEN
      MLEVEL = NUMB
C
C     Set the message unit
      ELSE IF ((CFLAG .EQ. 'MUNI').OR.(CFLAG.EQ.'MLFN')) THEN
      IF ((NUMB.LE.0).OR.(NUMB.GE.200)) GO TO 900
      MUNIT = NUMB
C
C     Set the data's precision
      ELSE IF (CFLAG.EQ. 'PREC') THEN
      IF (CSTRIN(1:3).EQ.'OFF') THEN
      IPREC = 0
      ELSE
      IF ((NUMB.LT.0).OR.(NUMB.GT.8)) GO TO 900
      IPREC = NUMB
      ENDIF
C
C     Set a tolerance for replacing data
      ELSE IF (CFLAG(1:3).EQ. 'TOL') THEN
      IF (CSTRIN(1:3).EQ.'OFF') THEN
      LTOL = .FALSE.
      ELSE
      IF ((NUMB.LT.0).OR.(NUMB.GT.8)) GO TO 900
      TOL = 0.6 / (10**NUMB)
      LTOL = .TRUE.
      ENDIF
C
C     Set the unit number to use for the next file to open
      ELSE IF ((CFLAG .EQ. 'UNIT').OR.(CFLAG.EQ.'FILN')) THEN
      IF ((NUMB.LE.0).OR.(NUMB.GE.200)) GO TO 900
      IDUNIT = NUMB
      LDUNIT = .TRUE.
C
C     Turn the catalog map option on
      ELSE IF (CFLAG.EQ.'MAP ') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LMAP = .TRUE.
      ELSE
      LMAP = .FALSE.
      ENDIF
C
C     Should we exclude missing periods from the condensed catalog?
      ELSE IF (CFLAG.EQ.'CCDA') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LCCDAT = .TRUE.
      ELSE
      LCCDAT = .FALSE.
      ENDIF
C
C     Set the unit number to write the catalog map to
      ELSE IF (CFLAG.EQ.'MAPU') THEN
      MAPUNT = NUMB
C
C     Set the data type
      ELSE IF (CFLAG.EQ.'TYPE') THEN
      ITYPE = NUMB
C
C     Set the output line length to 80 columns
      ELSE IF (CFLAG.EQ.'80CO') THEN
      IF (CSTRIN.EQ.'ON') THEN
      L80COL = .TRUE.
      ELSE
      L80COL = .FALSE.
      ENDIF
C
C     Set this file in an exclusive assign mode (Harris only)
      ELSE IF (CFLAG.EQ.'EXCL') THEN
      LEXCL = .TRUE.
C
C     Set this file in an (exclusive) write lock mode
      ELSE IF (CFLAG.EQ.'WLOC') THEN
C ***** TEMP DISABLE OF WRITE LOCK!!! *****
C   (NEED TO USE CRTN ON AN ABORT)
C     LWLOCK = .TRUE.
      LEXCL = .TRUE.
C
C     Set the file creation date (for squeeze only)
      ELSE IF (CFLAG.EQ.'FDAT') THEN
      CFDATE = CSTR
      LFDATE = .TRUE.
C
C     Set the estimated size of the file (e.g., LARGE or 4000)
C     to use for next call to ZOPEN
      ELSE IF (CFLAG.EQ.'SIZE') THEN
      CSIZE = ' '
      CSIZE = CSTR
      NSIZE = NUMB
      IHSIZE = 0
      LSZSET = .TRUE.
      IF ((CSIZE(1:1).EQ.' ').AND.(NSIZE.LT.0)) LSZSET = .FALSE.
C
C     Set the hash table size code, if squeezing the file
      ELSE IF (CFLAG.EQ.'HSIZ') THEN
      IF ((NUMB.GT.0).AND.(NUMB.LT.9)) THEN
      IHSIZE = NUMB
      LSZSET = .FALSE.
      ENDIF
C
C     Set the file to use a Dynamic or Stable Hash table
      ELSE IF (CFLAG.EQ.'TABL') THEN
      IF (CSTRIN(1:1).EQ.'S') THEN
      LSTABL = .TRUE.
      ELSE
      LSTABL = .FALSE.
      ENDIF
C
C     Set the tag for the next record to be written
      ELSE IF (CFLAG.EQ.'TAG ') THEN
      CTAG = CSTR
C
C     Set the version number for the next record to be written
      ELSE IF (CFLAG.EQ.'RVER') THEN
      IBVER = NUMB
C
C     Set the date for the next record to be written
      ELSE IF (CFLAG.EQ.'RDAT') THEN
      CDATE = CSTR
C
C     Set the time for the next record to be written
      ELSE IF (CFLAG.EQ.'RTIM') THEN
      CTIME = CSTR
C
C     Is the quality flag set on?
      ELSE IF ((CFLAG.EQ.'QUAL').OR.(CFLAG.EQ.'FLAG')) THEN
      IQUAL = NUMB
C
C     Set the time window flag for the catalog on or off
      ELSE IF (CFLAG.EQ.'TWCA') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LTWCAT = .TRUE.
      ELSE
      LTWCAT = .FALSE.
      ENDIF
C
C     Set the Protect mode on (don't write over existing records)
      ELSE IF (CFLAG.EQ.'PROT') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LPROTC = .TRUE.
      ELSE
      LPROTC = .FALSE.
      ENDIF
C
C     Set the Read Access Only mode on
      ELSE IF (CFLAG.EQ.'READ') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LREADO = .TRUE.
      ELSE
      LREADO = .FALSE.
      ENDIF
C
C     Compress time-series data when copying records, or
C     Squeezing the file?
      ELSE IF (CFLAG.EQ.'COMP') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LTSCMP = .TRUE.
      ELSE
      LTSCMP = .FALSE.
      ENDIF
C
C     Should status be shown when squeezing the file?
      ELSE IF (CFLAG.EQ.'CAST') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LCATST = .TRUE.
      ELSE
      LCATST = .FALSE.
      ENDIF
C
C     Should status be shown when squeezing the file?
      ELSE IF (CFLAG.EQ.'SQST') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LSQSTA = .TRUE.
      ELSE
      LSQSTA = .FALSE.
      ENDIF
C
C     Should data compression debug be set on?
      ELSE IF (CFLAG.EQ.'DCDB') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LDCDBG = .TRUE.
      IF ((NUMB.GT.0).AND.(NUMB.LT.100)) MDCUNT = NUMB
      ELSE
      LDCDBG = .FALSE.
      ENDIF
C
C
      ELSE
      GO TO 900
      ENDIF
C
      RETURN
C
C
 900  CONTINUE
C     TEMP REMOVAL OF ERROR WRITE FOR USE WITH ZSET4.  BILL CHARLEY
      IF (MLEVEL.GE.2) WRITE (MUNIT,910) CFLAG, CSTR, NUMB              MLu
 910  FORMAT (T5,'-----DSS *** ZSET ERROR: Illegal Flag; ',
     * 'Flag: ',A,'  String: ',A,'  Integer:',I8)
      RETURN
C
      END
