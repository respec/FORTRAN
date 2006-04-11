C     qtscn1.f 2.1 9/4/91
C
C
C
      SUBROUTINE   QRESPM
     I                   (UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,
     M                    IVAL,RVAL,CVAL,
     O                    TBUFF)
C
C     + + + PURPOSE + + +
C     This routine prompts a user for a response that contains any
C     combination of integer, decimal and character strings.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,INUM,RNUM,CNUM
      REAL        RVAL(RNUM)
      INTEGER     IVAL(INUM),CVAL(CNUM,3)
      CHARACTER*1 TBUFF(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of users message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     INUM   - number or integer responses
C     RNUM   - number of real responses
C     CNUM   - number of character responses
C     IVAL   - array containing integer responses, calling routine
C              may pass initial values
C     RVAL   - array containing real responses, calling routine may
C              pass initial values
C     CVAL   - array containing information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in TBUFF
C              (_,3) - length of character response
C     TBUFF  - character string containing integer, real, and character
C              responses in the order described by the message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      CHARACTER*1 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESPX, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK /' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I= 80
      CALL ZIPC (I,BLNK,TBUFF)
C
      CALL QRESPX (UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,
     M             IVAL,RVAL,CVAL,TBUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESPX
     I                   (UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,
     M                    IVAL,RVAL,CVAL,TBUFF)
C
C     + + + PURPOSE + + +
C     This routine prompts a user for a response that contains any
C     combination of integer, decimal and character strings.  Uses
C     existing TBUFF for initial values.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,INUM,RNUM,CNUM
      REAL        RVAL(RNUM)
      INTEGER     IVAL(INUM),CVAL(CNUM,3)
      CHARACTER*1 TBUFF(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of users message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     INUM   - number or integer responses
C     RNUM   - number of real responses
C     CNUM   - number of character responses
C     IVAL   - array containing integer responses, calling routine
C              may pass initial values
C     RVAL   - array containing real responses, calling routine may
C              pass initial values
C     CVAL   - array containing information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in TBUFF
C              (_,3) - length of character response
C     TBUFF  - character string containing integer, real, and character
C              responses in the order described by the message file,
C              calling program may pass initial values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     NROW,SCNFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESCX
C
C     + + + END SPECIFICATIONS + + +
C
      NROW = 1
      SCNFG= 0
C
      CALL QRESCX (UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,NROW,SCNFG,
     M             IVAL,RVAL,CVAL,TBUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESCN
     I                   (UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,NROW,SCNFG,
     M                    IVAL,RVAL,CVAL,TBUFF)
C
C     + + + PURPOSE + + +
C     This routine prompts a user for a response that contains any
C     combination of integer, decimal and character strings.  May
C     be called with many rows of data and used in screen mode.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,NROW,SCNFG
      REAL        RVAL(RNUM,NROW)
      INTEGER     IVAL(INUM,NROW),CVAL(CNUM,3,NROW)
      CHARACTER*1 TBUFF(80,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of users message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     INUM   - number or integer responses
C     RNUM   - number of real responses
C     CNUM   - number of character responses
C     NROW   - number or rows of responses
C     SCNFG  - indicator flag for type of input processing
C              0 - process one line at a time
C              1 - process a screen at a time
C     IVAL   - array containing integer responses, calling routine
C              may pass initial values
C     RVAL   - array containing real responses, calling routine may
C              pass initial values
C     CVAL   - array containing information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in TBUFF
C              (_,3) - length of character response
C     TBUFF  - array of character strings containing integer, real, and
C              character responses in the order described by the
C              message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      CHARACTER*1 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPC, QRESCX
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK= ' '
      I= NROW* 80
      CALL ZIPC(I,BLNK,TBUFF)
      CALL QRESCX
     I            (UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,NROW,SCNFG,
     M             IVAL,RVAL,CVAL,TBUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESCX
     I                   (UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,NROW,SCNFG,
     M                    IVAL,RVAL,CVAL,TBUFF)
C
C     + + + PURPOSE + + +
C     This routine prompts a user for a response that contains any
C     combination of integer, decimal and character strings.  Uses
C     existing TBUFF for initial values.  May be called with many
C     rows of data and used in screen mode.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,INUM,RNUM,CNUM,NROW,SCNFG
      REAL        RVAL(RNUM,NROW)
      INTEGER     IVAL(INUM,NROW),CVAL(CNUM,3,NROW)
      CHARACTER*1 TBUFF(80,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of users message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     INUM   - number or integer responses
C     RNUM   - number of real responses
C     CNUM   - number of character responses
C     NROW   - number or rows of responses
C     SCNFG  - indicator flag for type of input processing
C              0 - process one line at a time
C              1 - process a screen at a time
C     IVAL   - array containing integer responses, calling routine
C              may pass initial values
C     RVAL   - array containing real responses, calling routine may
C              pass initial values
C     CVAL   - array containing information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in TBUFF
C              (_,3) - length of character response
C     TBUFF  - array of character strings containing integer, real, and
C              character responses in the order described by the
C              message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER            DNUM
      DOUBLE PRECISION   DVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESCR
C
C     + + + END SPECIFICATIONS + + +
C
      DNUM= 1
C
      CALL QRESCR (UMESFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,NROW,SCNFG,
     M             IVAL,RVAL,DVAL,CVAL,TBUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESCD
     I                   (MESSFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,
     I                    NROW,SCNFG,
     M                    IVAL,RVAL,DVAL,CVAL,TBUFF)
C
C     + + + PURPOSE + + +
C     This routine prompts a user for a response that contains any
C     combination of integer, decimal, double precision, and character
C     strings.  May be called with many rows of data and used in
C     screen mode.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,NROW,SCNFG
      REAL        RVAL(RNUM,NROW)
      INTEGER     IVAL(INUM,NROW),CVAL(CNUM,3,NROW)
      CHARACTER*1 TBUFF(80,NROW)
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of users message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     INUM   - number or integer responses
C     RNUM   - number of real responses
C     DNUM   - number of double precision responses
C     CNUM   - number of character responses
C     NROW   - number or rows of responses
C     SCNFG  - indicator flag for type of input processing
C              0 - process one line at a time
C              1 - process a screen at a time
C     IVAL   - array containing integer responses, calling routine
C              may pass initial values
C     RVAL   - array containing real responses, calling routine may
C              pass initial values
C     DVAL   - array containing double precision responses, calling
C              routine may pass initial values
C     CVAL   - array containing information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in TBUFF
C              (_,3) - length of character response
C     TBUFF  - array of character strings containing integer, real, and
C              character responses in the order described by the
C              message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I
      CHARACTER* 1 BK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPC, QRESCR
C
C     + + + END SPECIFICATIONS + + +
C
      BK(1)= ' '
      I= 80 * NROW
      CALL ZIPC (I,BK(1),TBUFF)
C
      CALL QRESCR (MESSFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,NROW,SCNFG,
     M             IVAL,RVAL,DVAL,CVAL,TBUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESCR
     I                   (UMESFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,
     I                    NROW,SCNFG,
     M                    IVAL,RVAL,DVAL,CVAL,TBUFF)
C
C     + + + PURPOSE + + +
C     This routine prompts a user for a response that contains any
C     combination of integer, decimal, double precision, and character
C     strings.  Uses existing TBUFF for initial values.  May be called
C     with many rows of data and used in screen mode.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,NROW,SCNFG
      REAL        RVAL(RNUM,NROW)
      INTEGER     IVAL(INUM,NROW),CVAL(CNUM,3,NROW)
      CHARACTER*1 TBUFF(80,NROW)
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of users message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     INUM   - number or integer responses
C     RNUM   - number of real responses
C     DNUM   - number of double precision responses
C     CNUM   - number of character responses
C     NROW   - number or rows of responses
C     SCNFG  - indicator flag for type of input processing
C              0 - process one line at a time
C              1 - process a screen at a time
C     IVAL   - array containing integer responses, calling routine
C              may pass initial values
C     RVAL   - array containing real responses, calling routine may
C              pass initial values
C     DVAL   - array containing double precision responses, calling
C              routine may pass initial values
C     CVAL   - array containing information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in TBUFF
C              (_,3) - length of character response
C     TBUFF  - array of character strings containing integer, real, and
C              character responses in the order described by the
C              message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    TLEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESCZ
C
C     + + + END SPECIFICATIONS + + +
C
      TLEN= 80
      CALL QRESCZ (UMESFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,
     I             NROW,SCNFG,TLEN,
     M             IVAL,RVAL,DVAL,CVAL,TBUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESCZ
     I                   (UMESFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,
     I                    NROW,SCNFG,TLEN,
     M                    IVAL,RVAL,DVAL,CVAL,TBUFF)
C
C     + + + PURPOSE + + +
C     This routine prompts a user for a response that contains any
C     combination of integer, decimal, double precision, and character
C     strings.  Uses existing TBUFF for initial values.  May be called
C     with many rows of data and used in screen mode.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,INUM,RNUM,DNUM,CNUM,NROW,SCNFG,TLEN
      REAL        RVAL(RNUM,NROW)
      INTEGER     IVAL(INUM,NROW),CVAL(CNUM,3,NROW)
      CHARACTER*1 TBUFF(TLEN,NROW)
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of users message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     INUM   - number or integer responses
C     RNUM   - number of real responses
C     DNUM   - number of double precision responses
C     CNUM   - number of character responses
C     NROW   - number or rows of responses
C     SCNFG  - indicator flag for type of input processing
C              0 - process one line at a time
C              1 - process a screen at a time
C     TLEN   - length of character buffer string
C     IVAL   - array containing integer responses, calling routine
C              may pass initial values
C     RVAL   - array containing real responses, calling routine may
C              pass initial values
C     DVAL   - array containing double precision responses, calling
C              routine may pass initial values
C     CVAL   - array containing information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in TBUFF
C              (_,3) - length of character response
C     TBUFF  - array of character strings containing integer, real, and
C              character responses in the order described by the
C              message file
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
      INCLUDE 'pmxfld.inc'
C     INCLUDE 'PMXERR.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C     INCLUDE 'color.inc'
C     INCLUDE 'cqrsp.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,L,I1,I78,LSCLU,LSGRP,LROW,ERRFLG,IND,IRET,
     $            LCOL(MXFLD),LLIN(MXFLD),IPOS,CLASS,NLIN,ILIN,SGLCHR,
     $            LMXRSL
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + INTRINSICS + + +
C     INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, PRNTXT, CHRCHR, WMSGTP, ZIPI
      EXTERNAL    ZEDIT1, ZSCDEF, ZFLOUT, ZEDT0M, ZFILVL
C
C     + + + END SPECIFICATIONS + + +
C
      SGLCHR = 0
      I1 = 1
      I78= 78
C     screen cluster for general messages
      LSCLU= 4
C     set local version of max response buffer length to remove
C     any chance of parameter MXRSLN being modified
      LMXRSL= MXRSLN
C
      IF (NROW.EQ.0) THEN
C       error, full screen wont work, print message
        LSGRP= 89
        CALL PRNTXT (MESSFL,LSCLU,LSGRP)
      ELSE
C       ok to do full screen
 150    CONTINUE
          CALL WMSGTP (UMESFL,SCLU,SGRP,CLASS,ERRFLG)
          CALL ZSCDEF (INUM,RNUM,CNUM,DNUM,NROW,
     M                 IVAL,RVAL,DVAL,CVAL)
          CALL ZIPI (NFLDS,I1,LLIN)
          IPOS= 1
          DO 200 I= 1,NFLDS
            LCOL(I)= IPOS
            IPOS= IPOS+ FLEN(I)
 200      CONTINUE
C
          IF (LENSTR(TLEN,TBUFF(1,1)).GT.0) THEN
C           put TBUFF values in data arrays
            CALL ZFILVL (CLASS,NROW,TLEN,TBUFF,NROW,
     I                   INUM,RNUM,DNUM,CNUM,NFLDS,LCOL,FLEN,
     I                   LLIN,FTYP,FDVAL,CCNT,LMXRSL,RSPSTR,APOS,
     O                   IVAL,RVAL,DVAL,CVAL)
          END IF
C         put data arrays in screen text
          CALL ZFLOUT (CLASS,INUM,RNUM,DNUM,CNUM,NROW,TLEN,NFLDS,
     M                 TBUFF,LCOL,IVAL,RVAL,DVAL,CVAL)
          IF (CLASS.EQ.1) THEN
C           PRM1 type screen
            CALL ZEDT0M (SGLCHR,
     O                   IRET)
            NLIN= ZMNNLI
            ILIN= 1
          ELSE IF (CLASS.EQ.2) THEN
C           PRM2 type screen
            CALL ZEDIT1 (NROW,
     O                   IRET)
            NLIN= NROW
            ILIN= NMHDRW+ 1
          END IF
        IF (IRET.EQ.-1) GO TO 150
        CALL ZFILVL (CLASS,NLIN,I78,ZMNTX1(1,ILIN),NROW,
     I               INUM,RNUM,DNUM,CNUM,NFLDS,SCOL,FLEN,
     I               FLIN,FTYP,FDVAL,CCNT,LMXRSL,RSPSTR,APOS,
     O               IVAL,RVAL,DVAL,CVAL)
        DO 400 I= 1,NFLDS
C         put values back in TBUFF
          J= SCOL(I)
          K= LCOL(I)
          DO 300 LROW= 1,NROW
            IF (CLASS.EQ.1) THEN
              L= FLIN(I)
            ELSE IF (CLASS.EQ.2) THEN
C             PRM2 type screen
              L= NMHDRW+ LROW
            END IF
            CALL CHRCHR (FLEN(I),ZMNTX1(J,L),TBUFF(K,LROW))
 300      CONTINUE
 400    CONTINUE
C       fill in CVAL with lengths and locations of responses.
        DO 940 I =1,NFLDS
          IND= APOS(I)
          IF (FTYP(I).EQ.FTC.AND.IND.LE.CNUM) THEN
C           fill in rest of CVAL
            DO 930 CROW= 1, NROW
              CVAL(IND,2,CROW) = LCOL(I)
              CVAL(IND,3,CROW) = FLEN(I)
 930        CONTINUE
          END IF
 940    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   COLINI
C
C     + + + PURPOSE + + +
C     This routine initializes the colors to be used in the program
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'color.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,PRMIND
C
C     + + + INTRINSICS + + +
C     (none)
C
C     + + + EXTERNALS + + +
      EXTERNAL   COLSET, ANPRGT
C
C     + + + END SPECIFICATIONS + + +
C
C     assign colors from TERM.DAT or default
      DO 10 I=1,13
        PRMIND= I+ 21
        CALL ANPRGT (PRMIND,COLARR(I))
 10   CONTINUE
C
C     set to standard colors to start
      CALL COLSET (FRS,BKS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSCSET
     I                    (INUM,RNUM,DNUM,CNUM,NMFLDS,UIMIN,UIMAX,UIDEF,
     I                     URMIN,URMAX,URDEF,UDMIN,UDMAX,UDDEF,
     I                     VLINFG,UCCNT,UCDEF,USTRLN,URSPST)
C
C     + + + PURPOSE + + +
C     routine to set common block values for full screen limits as
C     definied by user (bypassing assignment from message file)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     INUM,RNUM,DNUM,CNUM,NMFLDS
      INTEGER     UIMIN(INUM),UIMAX(INUM),UIDEF(INUM),VLINFG(NMFLDS),
     1            UCCNT(NMFLDS),UCDEF(CNUM),USTRLN(NMFLDS)
      REAL        URMIN(RNUM),URMAX(RNUM),URDEF(RNUM)
      DOUBLE PRECISION UDMIN(DNUM),UDMAX(DNUM),UDDEF(DNUM)
      CHARACTER*1 URSPST(960)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields
C     RNUM   - number of real fields
C     DNUM   - number of double precision fields
C     CNUM   - number of character fields
C     NMFLDS - number of fields for this screen
C     UIMIN  - array of context-specific integer minimum values
C     UIMAX  - array of context-specific integer maximum values
C     UIDEF  - array of context-specific integer default values
C     URMIN  - array of context-specific real minimum values
C     URMAX  - array of context-specific real maximum values
C     URDEF  - array of context-specific real default values
C     UDMIN  - array of context-specific double precision minimum values
C     UDMAX  - array of context-specific double precision maximum values
C     UDDEF  - array of context-specific double precision default values
C     VLINFG - valid/invalid flag, 1- valid, 2- invalid, (0- none)
C     UCCNT  - array of context-specific valid responses for each field
C     UCDEF  - array of context-specific default responses for character fields
C     USTRLN - array of context-specific lengths of responses
C     URSPST - context-specific string of valid responses
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I1,URSPLN
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSCSTI, QSCSTR, QSCSTD, QSCSTC, QSCSTV
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
C     set screen parameters init flag to 'on' for this field
      SPINIT= 1
C
C     set integer field parameters
      CALL QSCSTI (INUM,I1,UIMIN,UIMAX,UIDEF)
C
C     set real field parameters
      CALL QSCSTR (RNUM,I1,URMIN,URMAX,URDEF)
C
C     set double precision field parameters
      CALL QSCSTD (DNUM,I1,UDMIN,UDMAX,UDDEF)
C
C     set character field parameters
      CALL QSCSTC (CNUM,I1,UCDEF)
C
C     set valid/invalid parameters for all fields
      URSPLN= 960
      CALL QSCSTV (NMFLDS,I1,VLINFG,UCCNT,
     I             USTRLN,URSPLN,URSPST)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSCSTI
     I                    (INUM,IPOS,UIMIN,UIMAX,UIDEF)
C
C     + + + PURPOSE + + +
C     Set common block Integer values for full screen limits as
C     definied by user (bypassing assignment from message file).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     INUM,IPOS,UIMIN(INUM),UIMAX(INUM),UIDEF(INUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields
C     IPOS   - offset position within integer fields to set parameters
C     UIMIN  - array of context-specific integer minimum values
C     UIMAX  - array of context-specific integer maximum values
C     UIDEF  - array of context-specific integer default values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,INUM
C       set min/max/default for this integer field
        J= IPOS+ I- 1
        IMIN(J)= UIMIN(I)
        IMAX(J)= UIMAX(I)
        IDEF(J)= UIDEF(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSCSTR
     I                    (RNUM,RPOS,URMIN,URMAX,URDEF)
C
C     + + + PURPOSE + + +
C     Set common block Real values for full screen limits as
C     definied by user (bypassing assignment from message file).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     RNUM,RPOS
      REAL        URMIN(RNUM),URMAX(RNUM),URDEF(RNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields
C     RPOS   - offset position within real fields to set parameters
C     URMIN  - array of context-specific real minimum values
C     URMAX  - array of context-specific real maximum values
C     URDEF  - array of context-specific real default values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,RNUM
C       set min/max/default for this integer field
        J= RPOS+ I- 1
        RMIN(J)= URMIN(I)
        RMAX(J)= URMAX(I)
        RDEF(J)= URDEF(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSCSTD
     I                    (DNUM,DPOS,UDMIN,UDMAX,UDDEF)
C
C     + + + PURPOSE + + +
C     Set common block Double Precision values for full screen limits as
C     definied by user (bypassing assignment from message file).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DNUM,DPOS
      DOUBLE PRECISION UDMIN(DNUM),UDMAX(DNUM),UDDEF(DNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields
C     DPOS   - offset position within double precision fields to set parameters
C     UDMIN  - array of context-specific double precision minimum values
C     UDMAX  - array of context-specific double precision maximum values
C     UDDEF  - array of context-specific double precision default values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,DNUM
C       set min/max/default for this integer field
        J= DPOS+ I- 1
        DMIN(J)= UDMIN(I)
        DMAX(J)= UDMAX(I)
        DDEF(J)= UDDEF(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSCSTC
     I                    (CNUM,CPOS,UCDEF)
C
C     + + + PURPOSE + + +
C     Set common block Character order defaults for full screen limits as
C     definied by user (bypassing assignment from message file).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CPOS,UCDEF(CNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields
C     CPOS   - offset position within character fields to set parameters
C     UCDEF  - array of context-specific character order default values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,CNUM
        IF (UCDEF(I).GT.0) THEN
C         set default only, valid/invalid set elsewhere
          J= CPOS+ I- 1
          CDEF(J)= UCDEF(I)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSCSTV
     I                    (NMFLDS,FPOS,VLINFG,UCCNT,
     I                     USTRLN,URSPLN,URSPST)
C
C     + + + PURPOSE + + +
C     Set valid/invalid common block values for full screen limits as
C     definied by user (bypassing assignment from message file).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NMFLDS,FPOS,VLINFG(NMFLDS),UCCNT(NMFLDS),
     $            USTRLN(NMFLDS),URSPLN
      CHARACTER*1 URSPST(URSPLN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NMFLDS - number of fields for this screen
C     FPOS   - offset position within all fields on screen
C     VLINFG - valid/invalid flag, 1- valid, 2- invalid, (0- none)
C     UCCNT  - array of context-specific valid responses for each field
C     USTRLN - array of context-specific lengths of responses
C     URSPLN - total length of context-specific valid responses
C     URSPST - context-specific string of valid responses
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,RSPOS,RSLEN,URSPOS,UREPOS,LRSPOS,LFLD,LTLEN,IP1,IP2
C
C     + + + FUNCTIONS + + +
      INTEGER    WMSPIV
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSPIV, WMSPIS, CHRCHR
C
C     + + + END SPECIFICATIONS + + +
C
      URSPOS= 1
C
      RSPOS = 0
      IF (FPOS.GT.1) THEN
C       may need to adjust offset position within response buffer
        I= FPOS
 10     CONTINUE
C         find position in response buffer of last field prior to FPOS
          I= I- 1
          IF (FDVAL(I).GT.0) THEN
C           valid responses exist, determine ending position
            CALL WMSPIS (FDVAL(I),
     O                   RSPOS,RSLEN)
            RSPOS= RSPOS+ RSLEN
          ELSE IF (FDINV(I).GT.0) THEN
C           invalid responses exist, determine ending position
            CALL WMSPIS (FDINV(I),
     O                   RSPOS,RSLEN)
            RSPOS= RSPOS+ RSLEN
          END IF
        IF (I.GT.1 .AND. RSPOS.EQ.0) GO TO 10
      END IF
      IF (RSPOS.EQ.0) THEN
C       start at beginning of respose buffer
        RSPOS= 1
      END IF
C
      LFLD= FPOS+ NMFLDS- 1
      IF (LFLD.LT.NFLDS) THEN
C       more fields exist after ones being set here,
C       may need to adjust their positions within response buffer
        LTLEN= 0
        DO 20 I= 1,NMFLDS
C         sum total length of user supplied responses
          IF (VLINFG(I).GT.0) THEN
C           responses being supplied for this field, add to total length
            LTLEN= LTLEN+ UCCNT(I)* USTRLN(I)
          END IF
 20     CONTINUE
C       set ending position of user supplied responses
        UREPOS= RSPOS+ LTLEN
C       determine start position in response buffer
C       of fields after those being set here
        LRSPOS= 0
        LTLEN = 0
        DO 30 I= LFLD+1,NFLDS
C         check for valid/invalid responses of later fields
          IF (FDVAL(I).GT.0 .OR. FDINV(I).GT.0) THEN
C           responses exist
            IF (FDVAL(I).GT.0) THEN
C             field after those being set contains valid responses
              CALL WMSPIS (FDVAL(I),
     O                     J,RSLEN)
              LTLEN= LTLEN+ RSLEN
            ELSE IF (FDINV(I).GT.0) THEN
C             field after those being set contains invalid responses
              CALL WMSPIS (FDINV(I),
     O                     J,RSLEN)
              LTLEN= LTLEN+ RSLEN
            END IF
            IF (LRSPOS.EQ.0) THEN
C             first field, after those being set, containing responses
              LRSPOS= J
            END IF
          END IF
 30     CONTINUE
C
        IF (LRSPOS.GT.0 .AND. UREPOS.NE.LRSPOS) THEN
C         need to shift remaining fields responses in buffer
          IF (UREPOS.LT.LRSPOS) THEN
C           need to shift remaining fields to the left
            DO 40 I= 1,LTLEN
C             shift each character in buffer
              IP1= UREPOS+ I- 1
              IP2= LRSPOS+ I- 1
              RSPSTR(IP1)= RSPSTR(IP2)
 40         CONTINUE
          ELSE IF (LRSPOS.GT.0 .AND. UREPOS.GT.LRSPOS) THEN
C           need to shift remaining fields to the right
            DO 50 I= LTLEN,1,-1
C             shift each character in buffer
              IP1= UREPOS+ I- 1
              IP2= LRSPOS+ I- 1
              RSPSTR(IP1)= RSPSTR(IP2)
 50         CONTINUE
          END IF
          IP1= UREPOS- LRSPOS
          DO 60 I= LFLD+1,NFLDS
C           adjust start position in buffer for remaining fields
            IF (FDVAL(I).GT.0) THEN
C             valid responses
              CALL WMSPIS (FDVAL(I),
     O                     J,RSLEN)
              J= J+ IP1
C             save adjusted start position in buffer
              FDVAL(I)= WMSPIV (J,RSLEN)
            ELSE IF (FDINV(I).GT.0) THEN
C             invalid responses
              CALL WMSPIS (FDINV(I),
     O                     J,RSLEN)
              J= J+ IP1
C             save adjusted start position in buffer
              FDVAL(I)= WMSPIV (J,RSLEN)
            END IF
 60       CONTINUE
        END IF
      END IF
C
      DO 100 I= 1,NMFLDS
C       set valid/invalid responses for all type fields
        J= FPOS+ I- 1
        IF (VLINFG(I).GT.0) THEN
C         valid/invalid responses exist for this field
          CCNT(J)= UCCNT(I)
          RSLEN  = UCCNT(I)* USTRLN(I)
          IF (VLINFG(I).EQ.1) THEN
C           valid responses, default?
            FDVAL(J)= WMSPIV(RSPOS,RSLEN)
          ELSE
C           invalid responses
            FDINV(J)= WMSPIV(RSPOS,RSLEN)
          END IF
          CALL CHRCHR (RSLEN,URSPST(URSPOS),RSPSTR(RSPOS))
          URSPOS= URSPOS+ RSLEN
          RSPOS = RSPOS+ RSLEN
        ELSE
C         no valid/invalid responses for this field
          FDVAL(J)= 0
          FDINV(J)= 0
        END IF
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QUESET
     I                    (UQUEST)
C
C     + + + PURPOSE + + +
C     routine to set common block value for full screen question
C     as defined by user (bypassing assignment from message file)
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*1 UQUEST(78)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UQUEST - user defined question for full screen
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'cqrsp.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRCHR
C
C     + + + END SPECIFICATIONS + + +
C
C     set question initialized flag to on
      QUINIT= 1
C
C     put user defined question into common block question
      I= 78
      CALL CHRCHR (I,UQUEST,QUEST)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HLPSET
     I                   (MESSFL,SCLU,SGRP,CLASS)
C
C     + + + PURPOSE + + +
C     set help for a screen from a cluster besides the
C     one for that screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,CLASS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster containing help message
C     SGRP   - group containing help message
C     CLASS  - type of screen containing help message
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    ID,DREC,DPOS
C
C     + + + FUNCTIONS + + +
      INTEGER    WDPTCL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTCL, WMSIDP, ZSTCMA
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLASS.EQ.1 .OR. CLASS.EQ.2) THEN
        ID= 15
      ELSE IF (CLASS.EQ.3) THEN
        ID= 2
      ELSE IF (CLASS.EQ.4 .OR. CLASS.EQ.5) THEN
        ID= 6
      END IF
C     get record and offset for start of help information
      CALL WMSIDP (MESSFL,SCLU,SGRP,ID,
     O             DREC,DPOS)
C     store record and offset in pointer to help info
      GPTR= WDPTCL (DREC,DPOS)
      IF (GPTR.GT.0) THEN
C       increment parameter init flag to indicate help is set
        HPINIT= 1
C       make help available
        CALL ZSTCMA (1,1)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INITM
     I                   (INUM, RNUM, CNUM,
     O                    IVAL, RVAL, CVAL)
C
C     + + + PURPOSE + + +
C     Initialize the integer, real, and character arguments
C     for full screen calls (QRESPM, QRESPX, QRESCN, QRESCX).
C     Minimum number for each type of response is 1.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM, RNUM, CNUM
      INTEGER   IVAL(INUM), CVAL(CNUM,3)
      REAL      RVAL(RNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer responses (minimum of 1)
C     RNUM   - number of real responses (minimum of 1)
C     CNUM   - number of character responses (minimum of 1)
C     IVAL   - array for integer responses of size INUM
C     RVAL   - array for real responses of size RNUM
C     CVAL   - array for information about character
C              responses of size CNUM,3
C              (_,1) - response sequence number
C              (_,2) - starting position of character
C                      response in buffer of responses
C              (_,3) - length of character response
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          DNUM
      DOUBLE PRECISION DVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   INITMD
C
C     + + + END SPECIFICATIONS + + +
C
      DNUM= 1
C
      CALL INITMD (INUM,RNUM,DNUM,CNUM,
     O             IVAL,RVAL,DVAL,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   INITMD
     I                    (INUM, RNUM, DNUM, CNUM,
     O                     IVAL, RVAL, DVAL, CVAL)
C
C     + + + PURPOSE + + +
C     Initialize the integer, real, double precision, and
C     character arguments for full screen calls (QRESCR, QRESCD,
C     QRESCZ).  Minimum number for each type of response is 1.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM, RNUM, DNUM, CNUM
      INTEGER   IVAL(INUM), CVAL(CNUM,3)
      REAL      RVAL(RNUM)
      DOUBLE PRECISION  DVAL(DNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer responses (minimum of 1)
C     RNUM   - number of real responses (minimum of 1)
C     DNUM   - number of double precision responses (minimum of 1)
C     CNUM   - number of character responses (minimum of 1)
C     IVAL   - array for integer responses of size INUM
C     RVAL   - array for real responses of size RNUM
C     DVAL   - array for double precision responses of size DNUM
C     CVAL   - array for information about character
C              responses of size CNUM,3
C              (_,1) - response sequence number
C              (_,2) - starting position of character
C                      response in buffer of responses
C              (_,3) - length of character response
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IZIP, CZIP
      REAL       RZIP
      DOUBLE PRECISION DZIP
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, ZIPR, ZIPD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  IZIP, CZIP, RZIP, DZIP
     *     /-999,    1, -999., -999. /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INUM .GT. 0) CALL ZIPI ( INUM, IZIP, IVAL )
      IF (RNUM .GT. 0) CALL ZIPR ( RNUM, RZIP, RVAL )
      IF (DNUM .GT. 0) CALL ZIPD ( DNUM, DZIP, DVAL )
      IF(CNUM .GT. 0) THEN
        CALL ZIPI( CNUM, CZIP, CVAL(1,1) )
        CALL ZIPI( CNUM, CZIP, CVAL(1,2) )
        CALL ZIPI( CNUM, CZIP, CVAL(1,3) )
      END IF
C
      RETURN
      END
