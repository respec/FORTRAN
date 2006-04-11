C     ztscn1.f 2.1 9/4/91
C
C
C
      SUBROUTINE   ZSCDEF
     I                    (INUM,RNUM,CNUM,DNUM,NROW,
     M                     IVAL,RVAL,DVAL,CVAL)
C
C     set default values for full screen arrays
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,RNUM,CNUM,DNUM,NROW
      INTEGER   IVAL(INUM,NROW),CVAL(CNUM,3,NROW)
      REAL      RVAL(RNUM,NROW)
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM  - number of integer fields
C     RNUM  - number of real fields
C     CNUM  - number of character fields
C     DNUM  - number of double precision fields
C     NROW  - number of rows of data
C     IVAL  - array containing integer responses, calling routine
C             may pass initial values
C     RVAL  - array containing real responses, calling routine may
C             pass initial values
C     DVAL  - array containing double precision responses, calling
C             routine may pass initial values
C     CVAL  - array containing information about character responses
C             (_,1) - response sequence number
C             (_,2) - starting position of character response in menu text
C             (_,3) - length of character response
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
C
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,IND
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + END SPECFICATIONS + + +
C
      DO 200 J= 1,NROW
        DO 100 I= 1,NFLDS
          IND= APOS(I)
          IF (FTYP(I).EQ.FTI) THEN
C           integer field
            IF (IVAL(IND,J).EQ.-999) THEN
C             value undefined, use default
              IVAL(IND,J) = IDEF(IND)
            END IF
C
          ELSE IF (FTYP(I).EQ.FTR) THEN
C           real field
            IF (ABS(RVAL(IND,J)+999.).LT.R0MIN) THEN
C             value undefined, use default
              RVAL(IND,J) = RDEF(IND)
            END IF
C
          ELSE IF (FTYP(I).EQ.FTD) THEN
C           double precision field
            IF (ABS(DVAL(IND,J)+999.).LT.D0MIN) THEN
C             field value undefined, use default
              DVAL(IND,J) = DDEF(IND)
            END IF
C
          ELSE IF (FTYP(I).EQ.FTC) THEN
C           character field
            IF (CVAL(IND,1,J).EQ.-999) THEN
C             value undefined, use default
              CVAL(IND,1,J) = CDEF(IND)
            END IF
C
          END IF
 100    CONTINUE
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZFLOUT
     I                    (CLASS,INUM,RNUM,DNUM,CNUM,NROW,OLEN,LNFLDS,
     I                     OBUFF,LCOL,IVAL,RVAL,DVAL,CVAL)
C
C     + + + PURPOSE + + +
C     fills menu text for use in EMIFE routines
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLASS,INUM,RNUM,DNUM,CNUM,NROW,OLEN,LNFLDS
      INTEGER     LCOL(LNFLDS),IVAL(INUM,NROW),CVAL(CNUM,3,NROW)
      REAL        RVAL(RNUM,NROW)
      DOUBLE PRECISION DVAL(DNUM,NROW)
      CHARACTER*1 OBUFF(OLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLASS  - type of screen (1- PRM1, 2- PRM2)
C     INUM   - number of integer fields
C     RNUM   - number of real fields
C     CNUM   - number of character fields
C     DNUM   - number of double precision fields
C     NROW   - number of rows of data
C     OLEN   - output buffer length
C     LNFLDS - number of data fields
C     OBUFF  - output buffer, may contain data to put in menu text
C     LCOL   - starting positions in OBUFF of data
C     IVAL   - array containing integer responses, calling routine
C              may pass initial values
C     RVAL   - array containing real responses, calling routine may
C              pass initial values
C     DVAL   - array containing double precision responses, calling
C              routine may pass initial values
C     CVAL   - array containing information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in menu text
C              (_,3) - length of character response
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,L,JUST,IND,LROW,LMXRSL
      CHARACTER*1 NONE(4)
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, DECCHR, INTCHR, DPRCHR, ZCHRCH
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      JUST= 0
C     set local version of max response buffer length to remove
C     any chance of parameter MXRSLN being modified
      LMXRSL= MXRSLN
C
      DO 200 LROW= 1,NROW
C       loop to fill rest of output buffer.
        DO 100 I = 1,NFLDS
          J  = SCOL(I)
          IND= APOS(I)
          IF (CLASS.EQ.1) THEN
C           PRM1 type screen
            L= FLIN(I)
          ELSE IF (CLASS.EQ.2) THEN
C           PRM2 type screen
            L= NMHDRW+ LROW
          END IF
          IF (FTYP(I).EQ.FTI) THEN
            IF (IVAL(IND,LROW).EQ.-999) THEN
C             no value use "none"
              IF (FLEN(I).GT.4) THEN
C               push 'none' to right side of field
                J= J+ FLEN(I)- 4
                K= 4
              ELSE
C               just start 'none' in 1st position of field
                K= FLEN(I)
              END IF
              CALL CHRCHR (K,NONE,ZMNTX1(J,L))
            ELSE
C             put current value in field
              CALL INTCHR (IVAL(IND,LROW),FLEN(I),JUST,
     O                     K,ZMNTX1(J,L))
            END IF
          ELSE IF (FTYP(I).EQ.FTR) THEN
            IF (ABS(RVAL(IND,LROW)+999.0).LT.R0MIN) THEN
C             no value use "none"
              IF (FLEN(I).GT.4) THEN
C               push 'none' to right side of field
                J= J+ FLEN(I)- 4
                K= 4
              ELSE
C               just start 'none' in 1st position of field
                K= FLEN(I)
              END IF
              CALL CHRCHR (K,NONE,ZMNTX1(J,L))
            ELSE
C             put current value in field
              CALL DECCHR (RVAL(IND,LROW),FLEN(I),JUST,
     O                     K,ZMNTX1(J,L))
            END IF
          ELSE IF (FTYP(I).EQ.FTD) THEN
            IF (ABS(DVAL(IND,LROW)+999.0).LT.D0MIN) THEN
C             no value use "none"
              IF (FLEN(I).GT.4) THEN
C               push 'none' to right side of field
                J= J+ FLEN(I)- 4
                K= 4
              ELSE
C               just start 'none' in 1st position of field
                K= FLEN(I)
              END IF
              CALL CHRCHR (K,NONE,ZMNTX1(J,L))
            ELSE
C             put current value in field
              CALL DPRCHR (DVAL(IND,LROW),FLEN(I),JUST,
     O                     K,ZMNTX1(J,L))
            END IF
          ELSE IF (FTYP(I).EQ.FTC) THEN
            IF (CVAL(IND,1,LROW).NE.0 .AND. FDVAL(I).NE.0) THEN
C             character with valids and current value exists
              CALL ZCHRCH (FLEN(I),FDVAL(I),CVAL(IND,1,LROW),
     I                     LMXRSL,RSPSTR,
     M                     ZMNTX1(J,L))
            ELSE IF (FDVAL(I).NE.0 .AND. FLEN(I).GE.4) THEN
C             character with valids, no value, use "NONE"
              K= 4
              CALL CHRCHR (K,NONE,ZMNTX1(J,L))
            ELSE
C             character with no valids, ASCII field
              CALL CHRCHR (FLEN(I),OBUFF(LCOL(I),LROW),ZMNTX1(J,L))
            END IF
          END IF
C
 100    CONTINUE
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZFILVL
     I                    (CLASS,NLIN,OLEN,OBUFF,NROW,
     I                     INUM,RNUM,DNUM,CNUM,NFLDS,LCOL,FLEN,
     I                     LLIN,FTYP,FDVAL,CCNT,RSPLEN,RSPSTR,APOS,
     O                     IVAL,RVAL,DVAL,CVAL)
C
C     + + + PURPOSE + + +
C     This routine fills in QRESPM arrays with the user or default values.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLASS,NLIN,OLEN,NROW,INUM,RNUM,DNUM,CNUM,NFLDS
      INTEGER     IVAL(INUM,NROW),CVAL(CNUM,3,NROW),
     1            LCOL(NFLDS),FLEN(NFLDS),LLIN(NFLDS),
     1            FDVAL(NFLDS),CCNT(NFLDS),RSPLEN,APOS(NFLDS)
      REAL        RVAL(RNUM,NROW)
      CHARACTER*1 OBUFF(OLEN,NLIN),RSPSTR(RSPLEN),FTYP(NFLDS)
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLASS  - type of screen (1- PRM1, 2- PRM2)
C     NLIN   - number of rows of menu text
C     OLEN   - length of output buffer strings
C     OBUFF  - character string containing values for RVAL, IVAL and
C              CVAL
C     NROW   - number of data rows
C     INUM   - number or integer responses
C     RNUM   - number of real responses
C     CNUM   - number of character responses
C     DNUM   - number of double precision responses
C     NFLDS  - number of fields
C     IVAL   - array for integer responses
C     RVAL   - array for real responses
C     DVAL   - array for double precision responses
C     CVAL   - array for information about character responses
C              (_,1) - response sequence number
C              (_,2) - starting position of character response in TBUFF
C              (_,3) - length of character response
C     LCOL   - array of starting columns of each field
C     FLEN   - array of lengths of each field
C     LLIN   - array of starting lines
C     FTYP   - array of types of data in each field
C     FDVAL  - pointers to valid responses for each field
C     CCNT   - array of number of valid responses for each field
C     RSPLEN - length of valid response string
C     RSPSTR - string of valid responses for each field
C     APOS   - array of field type counters
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,L,I4,POS,LFLEN,IND,LROW
      CHARACTER*1 CNONE(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRINT, ZCHKST, LENSTR, STRFND
      REAL        CHRDEC
      DOUBLE PRECISION CHRDPR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRINT, ZCHKST, LENSTR, STRFND, CHRDEC, CHRDPR
      EXTERNAL    LFTSTR, WMSPIS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      I4= 4
C
      DO 200 LROW= 1,NROW
C       fill in value arrays for each data row
        DO 100 I= 1,NFLDS
          J  = LCOL(I)
          IND= APOS(I)
          IF (CLASS.EQ.1) THEN
C           PRM1 type
            L= LLIN(I)
          ELSE IF (CLASS.EQ.2) THEN
C           PRM2 type
            L= LROW
          END IF
          LFLEN= LENSTR(FLEN(I),OBUFF(J,L))
          IF (LFLEN.GT.0) THEN
C           a string to enter
            IF (FTYP(I).EQ.'I') THEN
C             integer
              IF (STRFND(LFLEN,OBUFF(J,L),I4,CNONE).EQ.0) THEN
C               convert existing text to value
                IVAL(IND,LROW)= CHRINT(LFLEN,OBUFF(J,L))
              ELSE
C               current value is 'none', set to -999
                IVAL(IND,LROW)= -999
              END IF
            ELSE IF (FTYP(I).EQ.'R') THEN
C             real
              IF (STRFND(LFLEN,OBUFF(J,L),I4,CNONE).EQ.0) THEN
C               convert existing text to value
                RVAL(IND,LROW)= CHRDEC(LFLEN,OBUFF(J,L))
              ELSE
C               current value is 'none', set to -999.
                RVAL(IND,LROW)= -999.
              END IF
            ELSE IF (FTYP(I).EQ.'D') THEN
C             double prec
              IF (STRFND(LFLEN,OBUFF(J,L),I4,CNONE).EQ.0) THEN
C               convert existing text to value
                DVAL(IND,LROW)= CHRDPR(LFLEN,OBUFF(J,L))
              ELSE
C               current value is 'none', set to -999.
                DVAL(IND,LROW)= -999.
              END IF
            ELSE IF (FTYP(I).EQ.'C' .AND. FDVAL(I).GT.0) THEN
C             character, left justify response
              CALL WMSPIS (FDVAL(I),
     O                     POS,K)
              CALL LFTSTR (FLEN(I),OBUFF(J,L))
              CVAL(IND,1,LROW)= ZCHKST(FLEN(I),CCNT(I),OBUFF(J,L),
     1                                 RSPSTR(POS))
            END IF
          END IF
C
 100    CONTINUE
 200  CONTINUE
C
      RETURN
      END
