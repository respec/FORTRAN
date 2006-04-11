C
C
C
      SUBROUTINE   QSETI
     I                  (INUM,IVAL)
C
C     + + + PURPOSE + + +
C     Set the values for all integer fields on a
C     1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,IVAL(INUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to set values
C     IVAL   - array of integer values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSETIB
C
C     + + + END SPECIFICATIONS + + +
C
      IBAS= 1
      CALL QSETIB (INUM,IBAS,IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETIB
     I                   (INUM,IBAS,IVAL)
C
C     + + + PURPOSE + + +
C     Set the values for integer fields, starting at integer
C     field IBAS, on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,IBAS,IVAL(INUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to set values
C     IBAS   - base position within integer fields to begin setting values
C     IVAL   - array of integer values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2STIB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2STIB (INUM,IBAS,NROW,IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SETI
     I                   (INUM,NROW,IVAL)
C
C     + + + PURPOSE + + +
C     Set the values for all integer fields and all data rows
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,NROW,IVAL(INUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to set values
C     NROW   - number of rows of data
C     IVAL   - array of integer values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2STIB
C
C     + + + END SPECIFICATIONS + + +
C
      IBAS= 1
      CALL Q2STIB (INUM,IBAS,NROW,IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2STIB
     I                   (INUM,IBAS,NROW,IVAL)
C
C     + + + PURPOSE + + +
C     Set the values for integer fields, starting at integer field IBAS,
C     and all data rows on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,IBAS,NROW,IVAL(INUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to set values
C     IBAS   - base position within integer fields to begin setting values
C     NROW   - number of rows of data
C     IVAL   - array of integer values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SIBR
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2SIBR (INUM,IBAS,NROW,BROW,IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SIBR
     I                   (INUM,IBAS,NROW,BROW,IVAL)
C
C     + + + PURPOSE + + +
C     Set the values for integer fields, starting at integer field IBAS
C     and data row BROW, on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,IBAS,NROW,BROW,IVAL(INUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to set values
C     IBAS   - base position within integer fields to begin setting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin setting values
C     IVAL   - array of integer values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IFLD,LFLD,IBSCNT,IROW,LLIN,IBSROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   FLSETI
C
C     + + + END SPECIFICATIONS + + +
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        LFLD  = 0
        IFLD  = 0
        IBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the integer ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTI) THEN
C           integer field found
            IFLD= IFLD+ 1
            IF (IFLD.GE.IBAS) THEN
C             start setting values
              IBSCNT= IBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              CALL FLSETI (IDEF(IFLD),FLEN(LFLD),
     M                     IVAL(IBSCNT,IBSROW),
     O                     ZMNTX1(SCOL(LFLD),LLIN))
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. IBSCNT.LT.INUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETI
     I                  (INUM,
     O                   IVAL)
C
C     + + + PURPOSE + + +
C     Get the values for all integer fields on a
C     1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,IVAL(INUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to get values
C     IVAL   - array of integer values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QGETIB
C
C     + + + END SPECIFICATIONS + + +
C
      IBAS= 1
      CALL QGETIB (INUM,IBAS,
     O             IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETIB
     I                   (INUM,IBAS,
     O                    IVAL)
C
C     + + + PURPOSE + + +
C     Get the values for integer fields, starting at integer
C     field IBAS, on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,IBAS,IVAL(INUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to get values
C     IBAS   - base position within integer fields to begin getting values
C     IVAL   - array of integer values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GTIB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2GTIB (INUM,IBAS,NROW,
     O             IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GETI
     I                   (INUM,NROW,
     O                    IVAL)
C
C     + + + PURPOSE + + +
C     Get the values for all integer fields and all data rows
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,NROW,IVAL(INUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to get values
C     NROW   - number of rows of data
C     IVAL   - array of integer values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GTIB
C
C     + + + END SPECIFICATIONS + + +
C
      IBAS= 1
      CALL Q2GTIB (INUM,IBAS,NROW,
     O             IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GTIB
     I                   (INUM,IBAS,NROW,
     O                    IVAL)
C
C     + + + PURPOSE + + +
C     Get the values for integer fields, starting at integer field IBAS,
C     and all data rows on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,IBAS,NROW,IVAL(INUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to get values
C     IBAS   - base position within integer fields to begin getting values
C     NROW   - number of rows of data
C     IVAL   - array of integer values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GIBR
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2GIBR (INUM,IBAS,NROW,BROW,
     O             IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GIBR
     I                   (INUM,IBAS,NROW,BROW,
     O                    IVAL)
C
C     + + + PURPOSE + + +
C     Get the values for integer fields, starting at integer field IBAS
C     and data row BROW, on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM,IBAS,NROW,BROW,IVAL(INUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - number of integer fields for which to get values
C     IBAS   - base position within integer fields to begin getting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin getting values
C     IVAL   - array of integer values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I4,IFLD,LFLD,ILEN,NODAT,IBSCNT,IROW,IBSROW,LLIN
      CHARACTER*1 CNONE(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND, LENSTR, CRINTX
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, LENSTR, CRINTX, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      I4   = 4
      NODAT= -999
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        IFLD  = 0
        LFLD  = 0
        IBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the integer ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTI) THEN
C           integer field found
            IFLD= IFLD+ 1
            IF (IFLD.GE.IBAS) THEN
C             start getting values
              IBSCNT= IBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              I= SCOL(LFLD)
              J= SCOL(LFLD)+ FLEN(LFLD)- 1
              CALL ZLJUST (ZMNTXT(LLIN)(I:J))
              IF (STRFND(FLEN(LFLD),ZMNTX1(I,LLIN),I4,CNONE).GT.0)THEN
C               current field is 'none', set value to -999
                IVAL(IBSCNT,IBSROW)= NODAT
              ELSE
C               convert value in text to variable
                ILEN= LENSTR(FLEN(LFLD),ZMNTX1(I,LLIN))
                IF (ILEN .GT. 0) THEN
                  IVAL(IBSCNT,IBSROW)= CRINTX (ILEN,ZMNTX1(I,LLIN))
                ELSE
                  IVAL(IBSCNT,IBSROW)= NODAT
                END IF
              END IF
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. IBSCNT.LT.INUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETR
     I                  (RNUM,RVAL)
C
C     + + + PURPOSE + + +
C     Set the values for all real fields on a
C     1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM
      REAL      RVAL(RNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to set values
C     RVAL   - array of real values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    RBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSETRB
C
C     + + + END SPECIFICATIONS + + +
C
      RBAS= 1
      CALL QSETRB (RNUM,RBAS,RVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETRB
     I                   (RNUM,RBAS,RVAL)
C
C     + + + PURPOSE + + +
C     Set the values for real fields, starting at real
C     field RBAS, on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM,RBAS
      REAL      RVAL(RNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to set values
C     RBAS   - base position within real fields to begin setting values
C     RVAL   - array of real values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2STRB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2STRB (RNUM,RBAS,NROW,RVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SETR
     I                   (RNUM,NROW,RVAL)
C
C     + + + PURPOSE + + +
C     Set the values for all real fields and all data rows
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM,NROW
      REAL      RVAL(RNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to set values
C     NROW   - number of rows of data
C     RVAL   - array of real values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    RBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2STRB
C
C     + + + END SPECIFICATIONS + + +
C
      RBAS= 1
      CALL Q2STRB (RNUM,RBAS,NROW,RVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2STRB
     I                   (RNUM,RBAS,NROW,RVAL)
C
C     + + + PURPOSE + + +
C     Set the values for real fields, starting at real field RBAS,
C     and all data rows on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM,RBAS,NROW
      REAL      RVAL(RNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to set values
C     RBAS   - base position within real fields to begin setting values
C     NROW   - number of rows of data
C     RVAL   - array of real values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SRBR
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2SRBR (RNUM,RBAS,NROW,BROW,RVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SRBR
     I                   (RNUM,RBAS,NROW,BROW,RVAL)
C
C     + + + PURPOSE + + +
C     Set the values for real fields, starting at real field RBAS
C     and data row BROW, on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM,RBAS,NROW,BROW
      REAL      RVAL(RNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to set values
C     RBAS   - base position within real fields to begin setting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin getting values
C     RVAL   - array of real values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    RFLD,LFLD,RBSCNT,IROW,IBSROW,LLIN
C
C     + + + EXTERNALS + + +
      EXTERNAL   FLSETR
C
C     + + + END SPECIFICATIONS + + +
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        RFLD  = 0
        LFLD  = 0
        RBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the real ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTR) THEN
C           real field found
            RFLD= RFLD+ 1
            IF (RFLD.GE.RBAS) THEN
C             start setting values
              RBSCNT= RBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              CALL FLSETR (RDEF(RFLD),FLEN(LFLD),
     M                     RVAL(RBSCNT,IBSROW),
     O                     ZMNTX1(SCOL(LFLD),LLIN))
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. RBSCNT.LT.RNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETR
     I                  (RNUM,
     O                   RVAL)
C
C     + + + PURPOSE + + +
C     Get the values for all real fields on a
C     1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM
      REAL      RVAL(RNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to get values
C     RVAL   - array of real values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    RBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QGETRB
C
C     + + + END SPECIFICATIONS + + +
C
      RBAS= 1
      CALL QGETRB (RNUM,RBAS,
     O             RVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETRB
     I                   (RNUM,RBAS,
     O                    RVAL)
C
C     + + + PURPOSE + + +
C     Get the values for real fields, starting at real
C     field IBAS, on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM,RBAS
      REAL      RVAL(RNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to get values
C     RBAS   - base position within real fields to begin getting values
C     RVAL   - array of real values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GTRB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2GTRB (RNUM,RBAS,NROW,
     O             RVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GETR
     I                   (RNUM,NROW,
     O                    RVAL)
C
C     + + + PURPOSE + + +
C     Get the values for all real fields and all data rows
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM,NROW
      REAL      RVAL(RNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to get values
C     NROW   - number of rows of data
C     RVAL   - array of real values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    RBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GTRB
C
C     + + + END SPECIFICATIONS + + +
C
      RBAS= 1
      CALL Q2GTRB (RNUM,RBAS,NROW,
     O             RVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GTRB
     I                   (RNUM,RBAS,NROW,
     O                    RVAL)
C
C     + + + PURPOSE + + +
C     Get the values for real fields, starting at real field RBAS,
C     and all data rows on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM,RBAS,NROW
      REAL      RVAL(RNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to get values
C     RBAS   - base position within real fields to begin getting values
C     NROW   - number of rows of data
C     RVAL   - array of real values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GRBR
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2GRBR (RNUM,RBAS,NROW,BROW,
     O             RVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GRBR
     I                   (RNUM,RBAS,NROW,BROW,
     O                    RVAL)
C
C     + + + PURPOSE + + +
C     Get the values for real fields, starting at real field RBAS
C     and data row BROW, on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RNUM,RBAS,NROW,BROW
      REAL      RVAL(RNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RNUM   - number of real fields for which to get values
C     RBAS   - base position within real fields to begin getting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin getting values
C     RVAL   - array of real values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I4,RFLD,LFLD,ILEN,RBSCNT,IROW,IBSROW,LLIN
      REAL        NODAT
      CHARACTER*1 CNONE(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND, LENSTR
      REAL        CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, LENSTR, CHRDEC, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      I4   = 4
      NODAT= -999.0
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        RFLD  = 0
        LFLD  = 0
        RBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the integer ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTR) THEN
C           real field found
            RFLD= RFLD+ 1
            IF (RFLD.GE.RBAS) THEN
C             start getting values
              RBSCNT= RBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              I= SCOL(LFLD)
              J= SCOL(LFLD)+ FLEN(LFLD)- 1
              CALL ZLJUST (ZMNTXT(LLIN)(I:J))
              IF (STRFND(FLEN(LFLD),ZMNTX1(I,LLIN),I4,CNONE).GT.0)THEN
C               current value is 'none', set to -999.
                RVAL(RBSCNT,IBSROW)= NODAT
              ELSE
C               convert value in text to variable
                ILEN= LENSTR(FLEN(LFLD),ZMNTX1(I,LLIN))
                IF (ILEN .GT. 0) THEN
                  RVAL(RBSCNT,IBSROW)= CHRDEC(ILEN,ZMNTX1(I,LLIN))
                ELSE
                  RVAL(RBSCNT,IBSROW) = NODAT
                END IF
              END IF
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. RBSCNT.LT.RNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETD
     I                  (DNUM,DVAL)
C
C     + + + PURPOSE + + +
C     Set the values for all double precision fields on a
C     1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM
      DOUBLE PRECISION DVAL(DNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to set values
C     DVAL   - array of double precision values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSETDB
C
C     + + + END SPECIFICATIONS + + +
C
      DBAS= 1
      CALL QSETDB (DNUM,DBAS,DVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETDB
     I                   (DNUM,DBAS,DVAL)
C
C     + + + PURPOSE + + +
C     Set the values for double precision fields, starting at dble prec
C     field DBAS, on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM,DBAS
      DOUBLE PRECISION DVAL(DNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to set values
C     DBAS   - base position within dble prec fields to begin setting values
C     DVAL   - array of double precision values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2STDB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2STDB (DNUM,DBAS,NROW,DVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SETD
     I                   (DNUM,NROW,DVAL)
C
C     + + + PURPOSE + + +
C     Set the values for all double precision fields and all data rows
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM,NROW
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to set values
C     NROW   - number of rows of data
C     DVAL   - array of double precision values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2STDB
C
C     + + + END SPECIFICATIONS + + +
C
      DBAS= 1
      CALL Q2STDB (DNUM,DBAS,NROW,DVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2STDB
     I                   (DNUM,DBAS,NROW,DVAL)
C
C     + + + PURPOSE + + +
C     Set the values for dble prec fields, starting at dble prec field DBAS,
C     and all data rows on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM,DBAS,NROW
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to set values
C     DBAS   - base position within dble prec fields to begin setting values
C     NROW   - number of rows of data
C     DVAL   - array of double precision values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SDBR
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2SDBR (DNUM,DBAS,NROW,BROW,DVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SDBR
     I                   (DNUM,DBAS,NROW,BROW,DVAL)
C
C     + + + PURPOSE + + +
C     Set the values for dble prec fields, starting at dble prec field DBAS
C     and data row BROW, on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM,DBAS,NROW,BROW
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to set values
C     DBAS   - base position within db. prec. fields to begin setting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin getting values
C     DVAL   - array of double precision values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DFLD,LFLD,DBSCNT,IROW,IBSROW,LLIN
C
C     + + + EXTERNALS + + +
      EXTERNAL   FLSETD
C
C     + + + END SPECIFICATIONS + + +
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        DFLD  = 0
        LFLD  = 0
        DBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the real ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTD) THEN
C           double precision field found
            DFLD= DFLD+ 1
            IF (DFLD.GE.DBAS) THEN
C             start setting values
              DBSCNT= DBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              CALL FLSETD (DDEF(DFLD),FLEN(LFLD),
     M                     DVAL(DBSCNT,IBSROW),
     O                     ZMNTX1(SCOL(LFLD),LLIN))
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. DBSCNT.LT.DNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETD
     I                  (DNUM,
     O                   DVAL)
C
C     + + + PURPOSE + + +
C     Get the values for all double precision fields on a
C     1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM
      DOUBLE PRECISION DVAL(DNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to get values
C     DVAL   - array of double precision values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + EXTERNALS + + +
      EXTERNAL   QGETDB
C
C     + + + END SPECIFICATIONS + + +
C
      I= 1
      CALL QGETDB (DNUM,I,
     O             DVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETDB
     I                   (DNUM,DBAS,
     O                    DVAL)
C
C     + + + PURPOSE + + +
C     Get the values for double precision fields, starting at dble prec
C     field IBAS, on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM,DBAS
      DOUBLE PRECISION DVAL(DNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to get values
C     DBAS   - base position within dble prec fields to begin getting values
C     DVAL   - array of double precision values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GTDB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2GTDB (DNUM,DBAS,NROW,
     O             DVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GETD
     I                   (DNUM,NROW,
     O                    DVAL)
C
C     + + + PURPOSE + + +
C     Get the values for all double precision fields and all data rows
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM,NROW
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to get values
C     NROW   - number of rows of data
C     DVAL   - array of double precision values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GTDB
C
C     + + + END SPECIFICATIONS + + +
C
      DBAS= 1
      CALL Q2GTDB (DNUM,DBAS,NROW,
     O             DVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GTDB
     I                   (DNUM,DBAS,NROW,
     O                    DVAL)
C
C     + + + PURPOSE + + +
C     Get the values for dble prec fields, starting at dble prec field DBAS,
C     and all data rows on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM,DBAS,NROW
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to get values
C     DBAS   - base position within dble prec fields to begin getting values
C     NROW   - number of rows of data
C     DVAL   - array of double precision values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GDBR
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2GDBR (DNUM,DBAS,NROW,BROW,
     O             DVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GDBR
     I                   (DNUM,DBAS,NROW,BROW,
     O                    DVAL)
C
C     + + + PURPOSE + + +
C     Get the values for dble prec fields, starting at dble prec field DBAS
C     and data row BROW, on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DNUM,DBAS,NROW,BROW
      DOUBLE PRECISION DVAL(DNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DNUM   - number of double precision fields for which to get values
C     DBAS   - base position within db. prec. fields to begin getting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin getting values
C     DVAL   - array of double precision values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I4,DFLD,LFLD,ILEN,DBSCNT,IROW,IBSROW,LLIN
      DOUBLE PRECISION NODAT
      CHARACTER*1 CNONE(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND, LENSTR
      DOUBLE PRECISION    CHRDPR
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, LENSTR, CHRDPR, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      I4   = 4
      NODAT= -999.0
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        DFLD  = 0
        LFLD  = 0
        DBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the double precision ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTD) THEN
C           double precision field found
            DFLD= DFLD+ 1
            IF (DFLD.GE.DBAS) THEN
C             start getting values
              DBSCNT= DBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              I= SCOL(LFLD)
              J= SCOL(LFLD)+ FLEN(LFLD)- 1
              CALL ZLJUST (ZMNTXT(LLIN)(I:J))
              IF (STRFND(FLEN(LFLD),ZMNTX1(I,LLIN),I4,CNONE).GT.0)THEN
C               current value is 'none', set to -999.
                DVAL(DBSCNT,IBSROW)= NODAT
              ELSE
C               convert value in text to variable
                ILEN= LENSTR(FLEN(LFLD),ZMNTX1(I,LLIN))
                IF (ILEN .GT. 0) THEN
                  DVAL(DBSCNT,IBSROW)= CHRDPR(ILEN,ZMNTX1(I,LLIN))
                ELSE
                  DVAL(DBSCNT,IBSROW) = NODAT
                END IF
              END IF
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. DBSCNT.LT.DNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETCO
     I                   (CNUM,CVAL)
C
C     + + + PURPOSE + + +
C     Set the values for all character fields with valid
C     responses on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,CVAL(CNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CVAL   - array of character order values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSTCOB
C
C     + + + END SPECIFICATIONS + + +
C
      CBAS= 1
      CALL QSTCOB (CNUM,CBAS,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSTCOB
     I                   (CNUM,CBAS,CVAL)
C
C     + + + PURPOSE + + +
C     Set the values for character fields w/valids, starting at character
C     field CBAS, on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,CBAS,CVAL(CNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CBAS   - base position within character fields to begin setting values
C     CVAL   - array of character order values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SCOB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2SCOB (CNUM,CBAS,NROW,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2STCO
     I                   (CNUM,NROW,CVAL)
C
C     + + + PURPOSE + + +
C     Set the values for all character fields w/valids and all data rows
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,NROW,CVAL(CNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     NROW   - number of rows of data
C     CVAL   - array of character order values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SCOB
C
C     + + + END SPECIFICATIONS + + +
C
      CBAS= 1
      CALL Q2SCOB (CNUM,CBAS,NROW,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SCOB
     I                   (CNUM,CBAS,NROW,CVAL)
C
C     + + + PURPOSE + + +
C     Set the values for character fields w/valids,
C     starting at character field CBAS, and all data
C     rows on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,CBAS,NROW,CVAL(CNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CBAS   - base position within character fields to begin setting values
C     NROW   - number of rows of data
C     CVAL   - array of character order values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SCOX
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2SCOX (CNUM,CBAS,NROW,BROW,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SCOX
     I                   (CNUM,CBAS,NROW,BROW,CVAL)
C
C     + + + PURPOSE + + +
C     Set the values for character fields w/valids,
C     starting at character field CBAS and data row BROW,
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,CBAS,NROW,BROW,CVAL(CNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CBAS   - base position within character fields to begin setting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin setting values
C     CVAL   - array of character order values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ILEN,NODAT,TFLD,LFLD,TBSCNT,IROW,IBSROW,LLIN,LMXRSL
      CHARACTER*1 CNONE(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZCHRCH, CHRCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      NODAT= -999
C     set local version of max response buffer length to remove
C     any chance of parameter MXRSLN being modified
      LMXRSL= MXRSLN
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        LFLD  = 0
        TFLD  = 0
        TBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the character ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTC) THEN
C           character field found
            TFLD= TFLD+ 1
            IF (TFLD.GE.CBAS) THEN
C             start setting values
              TBSCNT= TBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              IF (CVAL(TBSCNT,IBSROW).EQ.NODAT) THEN
C               current value undefined, use default
                CVAL(TBSCNT,IBSROW) = CDEF(TFLD)
              END IF
              IF (CVAL(TBSCNT,IBSROW).NE.0 .AND. FDVAL(LFLD).NE.0)THEN
C               character with valids and current value exists, put in menu
                CALL ZCHRCH (FLEN(LFLD),FDVAL(LFLD),
     I                       CVAL(TBSCNT,IBSROW),LMXRSL,RSPSTR,
     M                       ZMNTX1(SCOL(LFLD),LLIN))
              ELSE IF (FDVAL(LFLD).NE.0) THEN
C               character with valids, no value, use 'none'
                IF (FLEN(LFLD).LT.4) THEN
                  ILEN= FLEN(LFLD)
                ELSE
                  ILEN= 4
                END IF
                CALL CHRCHR (ILEN,CNONE,
     M                       ZMNTX1(SCOL(LFLD),LLIN))
              END IF
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. TBSCNT.LT.CNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETCT
     I                   (CNUM,CLEN,TLEN,CTXT)
C
C     + + + PURPOSE + + +
C     Set the values for all character fields with no valid responses
C     (plain text) on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN
      CHARACTER*1 CTXT(TLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings (sum of CLEN array)
C     CTXT   - array of character strings for each field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSTCTB
C
C     + + + END SPECIFICATIONS + + +
C
      CBAS= 1
      CALL QSTCTB (CNUM,CLEN,TLEN,CBAS,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSTCTF
     I                   (CIND,CLEN,CTXT)
C
C     + + + PURPOSE + + +
C     Set the values for one character field with no valid responses
C     (plain text) on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CIND,CLEN
      CHARACTER*1 CTXT(CLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CIND   - character field number for which to set value
C     CLEN   - length of text string for field
C     CTXT   - character string for field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    TLEN,CNUM,LCLEN(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSTCTB
C
C     + + + END SPECIFICATIONS + + +
C
      LCLEN(1)= CLEN
      TLEN= CLEN
      CNUM= 1
      CALL QSTCTB (CNUM,LCLEN,TLEN,CIND,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSTCTB
     I                   (CNUM,CLEN,TLEN,CBAS,CTXT)
C
C     + + + PURPOSE + + +
C     Set the values for character fields with no valid responses
C     (plain text), starting at character field CBAS,
C     on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN,CBAS
      CHARACTER*1 CTXT(TLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings (sum of CLEN array)
C     CBAS   - base position within character fields to begin setting values
C     CTXT   - array of character strings for each field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SCTB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2SCTB (CNUM,CLEN,TLEN,CBAS,NROW,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2STCT
     I                   (CNUM,CLEN,TLEN,NROW,CTXT)
C
C     + + + PURPOSE + + +
C     Set the values for all character fields with no
C     valid responses (plain text) and all data rows
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN,NROW
      CHARACTER*1 CTXT(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings per data row
C     NROW   - number of rows of data
C     CTXT   - array of character strings for each field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SCTB
C
C     + + + END SPECIFICATIONS + + +
C
      CBAS= 1
      CALL Q2SCTB (CNUM,CLEN,TLEN,CBAS,NROW,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SCTF
     I                   (CIND,CLEN,NROW,CTXT)
C
C     + + + PURPOSE + + +
C     Set the values for one character field with no
C     valid responses (plain text) and all data rows
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CIND,CLEN,NROW
      CHARACTER*1 CTXT(CLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CIND   - charracter field number for which to set values
C     CLEN   - length of text string for field
C     NROW   - number of rows of data
C     CTXT   - array of character strings for field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CNUM,LCLEN(1),TLEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SCTB
C
C     + + + END SPECIFICATIONS + + +
C
      CNUM= 1
      LCLEN(1)= CLEN
      TLEN= CLEN
      CALL Q2SCTB (CNUM,LCLEN,TLEN,CIND,NROW,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SCTB
     I                   (CNUM,CLEN,TLEN,CBAS,NROW,CTXT)
C
C     + + + PURPOSE + + +
C     Set the values for character fields with no valid
C     responses (plain text), starting at character field CBAS,
C     and all data rows on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN,CBAS,NROW
      CHARACTER*1 CTXT(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings per data row
C     CBAS   - base position within character fields to begin setting values
C     NROW   - number of rows of data
C     CTXT   - array of character strings for each field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SCTX
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2SCTX (CNUM,CLEN,TLEN,CBAS,NROW,BROW,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SCTX
     I                   (CNUM,CLEN,TLEN,CBAS,NROW,BROW,CTXT)
C
C     + + + PURPOSE + + +
C     Set the values for character fields with no valid responses
C     (plain text), starting at character field CBAS and data row BROW,
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN,CBAS,NROW,BROW
      CHARACTER*1 CTXT(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings (sum of CLEN array)
C     CBAS   - base field within character fields to begin setting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin setting values
C     CTXT   - array of character strings for each field
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IPOS,ILEN,TFLD,LFLD,CPOS,CTMP,TBSCNT,IROW,IBSROW,LLIN
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCHKST
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZCHKST, CHRCHR, WMSPIS
C
C     + + + END SPECIFICATIONS + + +
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        CPOS  = 1
        LFLD  = 0
        TFLD  = 0
        TBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the character ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTC) THEN
C           character field found
            TFLD= TFLD+ 1
            IF (TFLD.GE.CBAS) THEN
C             start setting values
              TBSCNT= TBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              IF (TBSCNT.GT.1) THEN
C               determine position in supplied text string for this field
                CPOS= CPOS+ CLEN(TBSCNT-1)
              END IF
              IF (FDVAL(LFLD).GT.0) THEN
C               field has valid values, see if supplied text is one of them
                CALL WMSPIS (FDVAL(LFLD),
     O                       IPOS,ILEN)
                CTMP= ZCHKST(FLEN(LFLD),CCNT(LFLD),CTXT(CPOS,IBSROW),
     1                       RSPSTR(IPOS))
              ELSE
C               put supplied text in screen text
                CTMP= 1
              END IF
              IF (CTMP.GT.0) THEN
C               put supplied text in screen text
                IF (CLEN(TBSCNT).GT.FLEN(LFLD)) THEN
C                 not enough room in field for supplied text, truncate
                  ILEN= FLEN(LFLD)
                ELSE
                  ILEN= CLEN(TBSCNT)
                END IF
                CALL CHRCHR (ILEN,CTXT(CPOS,IBSROW),
     M                       ZMNTX1(SCOL(LFLD),LLIN))
              END IF
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. TBSCNT.LT.CNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETCO
     I                   (CNUM,
     O                    CVAL)
C
C     + + + PURPOSE + + +
C     Get the values for all character fields with valid
C     responses on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,CVAL(CNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     CVAL   - array of character order values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QGTCOB
C
C     + + + END SPECIFICATIONS + + +
C
      CBAS= 1
      CALL QGTCOB (CNUM,CBAS,
     O             CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGTCOB
     I                   (CNUM,CBAS,
     O                    CVAL)
C
C     + + + PURPOSE + + +
C     Get the values for character fields w/valids, starting at character
C     field CBAS, on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,CBAS,CVAL(CNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     CBAS   - base position within character fields to begin getting values
C     CVAL   - array of character order values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GCOB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2GCOB (CNUM,CBAS,NROW,
     O             CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GTCO
     I                   (CNUM,NROW,
     O                    CVAL)
C
C     + + + PURPOSE + + +
C     Get the values for all character fields w/valids and all data rows
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,NROW,CVAL(CNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     NROW   - number of rows of data
C     CVAL   - array of character order values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GCOB
C
C     + + + END SPECIFICATIONS + + +
C
      CBAS= 1
      CALL Q2GCOB (CNUM,CBAS,NROW,
     O             CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GCOB
     I                   (CNUM,CBAS,NROW,
     O                    CVAL)
C
C     + + + PURPOSE + + +
C     Get the values for character fields w/valids,
C     starting at character field CBAS, and all data
C     rows on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,CBAS,NROW,CVAL(CNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     CBAS   - base position within character fields to begin getting values
C     NROW   - number of rows of data
C     CVAL   - array of character order values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GCOX
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2GCOX (CNUM,CBAS,NROW,BROW,
     O             CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GCOX
     I                   (CNUM,CBAS,NROW,BROW,
     O                    CVAL)
C
C     + + + PURPOSE + + +
C     Get the values for character fields w/valids,
C     starting at character field CBAS and data row BROW,
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CNUM,CBAS,NROW,BROW,CVAL(CNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     CBAS   - base position within character fields to begin getting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin getting values
C     CVAL   - array of character order values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IPOS,ILEN,TFLD,LFLD,TBSCNT,IROW,IBSROW,LLIN
C
C     + + + FUNCTIONS + + +
      INTEGER    ZCHKST
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZCHKST, LFTSTR, WMSPIS
C
C     + + + END SPECIFICATIONS + + +
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        LFLD  = 0
        TFLD  = 0
        TBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the character ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTC) THEN
C           character field found
            TFLD= TFLD+ 1
            IF (TFLD.GE.CBAS) THEN
C             start getting values
              TBSCNT= TBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              IF (FDVAL(LFLD).GT.0) THEN
C               character with valids and current value exists
                CALL WMSPIS (FDVAL(LFLD),
     M                       IPOS,ILEN)
                CALL LFTSTR (FLEN(LFLD),ZMNTX1(SCOL(LFLD),LLIN))
                CVAL(TBSCNT,IBSROW)= ZCHKST(FLEN(LFLD),CCNT(LFLD),
     1                        ZMNTX1(SCOL(LFLD),LLIN),RSPSTR(IPOS))
              ELSE
C               character without valids, no order value
                CVAL(TBSCNT,IBSROW)= 0
              END IF
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. TBSCNT.LT.CNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETCT
     I                   (CNUM,CLEN,TLEN,
     O                    CTXT)
C
C     + + + PURPOSE + + +
C     Get the values for all character fields with no valid responses
C     (plain text) on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN
      CHARACTER*1 CTXT(TLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings (sum of CLEN array)
C     CTXT   - array of character strings for each field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + EXTERNALS + + +
      EXTERNAL   QGTCTB
C
C     + + + END SPECIFICATIONS + + +
C
      I= 1
      CALL QGTCTB (CNUM,CLEN,TLEN,I,
     M             CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGTCTF
     I                   (CIND,CLEN,
     O                    CTXT)
C
C     + + + PURPOSE + + +
C     Get the values for one character field with no valid responses
C     (plain text) on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CIND,CLEN
      CHARACTER*1 CTXT(CLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CIND   - character field number for which to get value
C     CLEN   - length of text string for field
C     CTXT   - character string for field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    TLEN,CNUM,LCLEN(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QGTCTB
C
C     + + + END SPECIFICATIONS + + +
C
      LCLEN(1)= CLEN
      TLEN= CLEN
      CNUM= 1
      CALL QGTCTB (CNUM,LCLEN,TLEN,CIND,
     O             CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGTCTB
     I                   (CNUM,CLEN,TLEN,CBAS,
     O                    CTXT)
C
C     + + + PURPOSE + + +
C     Get the values for character fields with no valid responses
C     (plain text), starting at character field CBAS,
C     on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN,CBAS
      CHARACTER*1 CTXT(TLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings (sum of CLEN array)
C     CBAS   - base position within character fields to begin getting values
C     CTXT   - array of character strings for each field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GCTB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2GCTB (CNUM,CLEN,TLEN,CBAS,NROW,
     O             CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GTCT
     I                   (CNUM,CLEN,TLEN,NROW,
     O                    CTXT)
C
C     + + + PURPOSE + + +
C     Get the values for all character fields with no
C     valid responses (plain text) and all data rows
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN,NROW
      CHARACTER*1 CTXT(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings per data row
C     NROW   - number of rows of data
C     CTXT   - array of character strings for each field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GCTB
C
C     + + + END SPECIFICATIONS + + +
C
      CBAS= 1
      CALL Q2GCTB (CNUM,CLEN,TLEN,CBAS,NROW,
     O             CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GCTF
     I                   (CIND,CLEN,NROW,CTXT)
C
C     + + + PURPOSE + + +
C     Get the values for one character field with no
C     valid responses (plain text) and all data rows
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CIND,CLEN,NROW
      CHARACTER*1 CTXT(CLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CIND   - charracter field number for which to get values
C     CLEN   - length of text string for field
C     NROW   - number of rows of data
C     CTXT   - array of character strings for field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CNUM,LCLEN(1),TLEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GCTB
C
C     + + + END SPECIFICATIONS + + +
C
      CNUM= 1
      LCLEN(1)= CLEN
      TLEN= CLEN
      CALL Q2GCTB (CNUM,LCLEN,TLEN,CIND,NROW,
     O             CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GCTB
     I                   (CNUM,CLEN,TLEN,CBAS,NROW,
     O                    CTXT)
C
C     + + + PURPOSE + + +
C     Get the values for character fields with no valid
C     responses (plain text), starting at character field CBAS,
C     and all data rows on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN,CBAS,NROW
      CHARACTER*1 CTXT(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to get values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings per data row
C     CBAS   - base position within character fields to begin getting values
C     NROW   - number of rows of data
C     CTXT   - array of character strings for each field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GCTX
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2GCTX (CNUM,CLEN,TLEN,CBAS,NROW,BROW,
     O             CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GCTX
     I                   (CNUM,CLEN,TLEN,CBAS,NROW,BROW,
     O                    CTXT)
C
C     + + + PURPOSE + + +
C     Get the values for character fields with no valid responses
C     (plain text), starting at character field CBAS and data row BROW,
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM,CLEN(CNUM),TLEN,CBAS,NROW,BROW
      CHARACTER*1 CTXT(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of character fields for which to set values
C     CLEN   - array of lengths of text strings for each field
C     TLEN   - total length of character strings (sum of CLEN array)
C     CBAS   - base field within character fields to begin setting values
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin setting values
C     CTXT   - array of character strings for each field
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     TFLD,LFLD,CPOS,TBSCNT,IROW,IBSROW,LLIN
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR
C
C     + + + END SPECIFICATIONS + + +
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        CPOS  = 1
        LFLD  = 0
        TFLD  = 0
        TBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the character ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTC) THEN
C           character field found
            TFLD= TFLD+ 1
            IF (TFLD.GE.CBAS) THEN
C             start getting values
              TBSCNT= TBSCNT+ 1
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              IF (TBSCNT.GT.1) THEN
C               determine position in supplied text string for this field
                CPOS= CPOS+ CLEN(TBSCNT-1)
              END IF
              CALL CHRCHR (CLEN(TBSCNT),ZMNTX1(SCOL(LFLD),LLIN),
     O                     CTXT(CPOS,IBSROW))
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. TBSCNT.LT.CNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETOP
     I                   (OPCNT,OPLEN,MXSEL,MNSEL,OPVAL)
C
C     + + + PURPOSE + + +
C     Set common block parameters for option type data fields
C     based on supplied arguments.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OPCNT,OPLEN,MXSEL(OPCNT),MNSEL(OPCNT),OPVAL(OPLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPCNT  - number of sets of options on screen
C     OPLEN  - total number of options which may be selected from screen
C     MXSEL  - maximum number of options which may be selected per set
C     MNSEL  - minimum number of options which may be selected per set
C     OPVAL  - array of order numbers, within sets, of selected fields
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'czoptn.inc'
      INCLUDE 'czhide.inc'
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,IPOS,LFLD,LVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
C     init array of option fields selected to all fields OFF
      I= 0
      CALL ZIPI (OPLEN,I,OPSVAL)
C
C     set common parameters based on supplied arguments
      DO 100 I= 1,OPCNT
C       fill in min/max selections per set
        IF (MNSEL(I).GE.0 .AND. MNSEL(I).LE.MXSEL(I)) THEN
C         reasonable value
          OPMNSL(I)= MNSEL(I)
        ELSE
C         bogus value, set to default of 1
          OPMNSL(I)= 1
        END IF
        IF (MXSEL(I).GT.0 .AND. MXSEL(I).GE.MNSEL(I)) THEN
C         reasonable value
          OPMXSL(I)= MXSEL(I)
        ELSE
C         bogus value, set to default of 1
          OPMXSL(I)= 1
        END IF
C       init number of selections for this set
        CURSEL(I)= 0
C       determine position within OPVAL array
        IPOS= 0
C       also determine number of fields offset
        IF (I.GT.1) THEN
C         determine offsets
          DO 30 J= 1,I-1
            IPOS= IPOS+ OPMXSL(J)
 30       CONTINUE
        END IF
        DO 50 J= 1,MXSEL(I)
C         fill in supplied initial values
          IPOS= IPOS+ 1
          IF (OPVAL(IPOS).GT.0) THEN
C           value supplied
            OPSVAL(IPOS) = OPVAL(IPOS)
C           increment number of options selected for this set
            CURSEL(I) = CURSEL(I)+ 1
C           which field does this correspond to
            LFLD = 0
 40         CONTINUE
              LFLD= LFLD+ 1
              IF (OPSET(LFLD).EQ.I .AND.
     1            OPSTNO(LFLD).EQ.OPSVAL(IPOS)) THEN
C               this is the field, set screen text appropriately
                ZMNTX1(SCOL(LFLD),FLIN(LFLD))= 'X'
                LFLD= NFLDS+ 1
              END IF
            IF (LFLD.LT.NFLDS) GO TO 40
          END IF
 50     CONTINUE
 100  CONTINUE
C     check for any hidden fields
      IF (NUMHID.GT.0) THEN
C       some hidden fields exist
        DO 150 I= 1,NUMHID
C         see if value of 'trigger' field makes target field be hidden
          IPOS= 0
          IF (OPSET(HIDTFL(I)).GT.1) THEN
C           determine offset within option selected array
            DO 120 J= 1,OPSET(HIDTFL(I))-1
              IPOS= IPOS+ OPMXSL(J)
 120        CONTINUE
          END IF
C         look through option selected array to see if field is on or off
          LVAL= 0
          DO 130 J= 1,OPMXSL(OPSET(HIDTFL(I)))
            IF (OPSVAL(J+IPOS).EQ.OPSTNO(HIDTFL(I))) THEN
C             this field is on
              LVAL= 1
            END IF
 130      CONTINUE
          IF (HIDVAL(I).EQ.LVAL) THEN
C           set flag to indicate hide is active for this field
            HIDFLG(I)= 1
          ELSE
C           hide not active
            HIDFLG(I)= 0
          END IF
 150    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETOP
     I                   (OPLEN,
     O                    OPVAL)
C
C     + + + PURPOSE + + +
C     Get options selected array values from common and
C     return values in supplied argument array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OPLEN,OPVAL(OPLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPLEN  - total number of options which may be selected from screen
C     OPVAL  - array of order numbers, within sets, of selected fields
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'czoptn.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C     put common values of options selected array into output argument
      DO 100 I= 1,OPLEN
        OPVAL(I)= OPSVAL(I)
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   FLSETI
     I                   (IDEF,FLEN,
     M                    IVAL,
     O                    FLDSTR)
C
C     + + + PURPOSE + + +
C     Given a current and default value for an integer data field,
C     determine the appropriate initial value and fill in the data
C     field text string.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     IDEF,FLEN,IVAL
      CHARACTER*1 FLDSTR(FLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDEF   - default value for field
C     FLEN   - length of data file
C     IVAL   - data field numberic value
C     FLDSTR - data field text string
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     J,IPOS,ILEN,NODAT,JUST
      CHARACTER*1 CNONE(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, INTCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      NODAT = -999
      JUST  = 0
C
      IF (IVAL.EQ.NODAT) THEN
C       current value undefined, set to default
        IVAL= IDEF
      END IF
      IF (IVAL.EQ.NODAT) THEN
C       no value use "none"
        IF (FLEN.GT.4) THEN
C         push 'none' to right side of field
          IPOS= FLEN- 3
          ILEN= 4
        ELSE
C         just start 'none' in 1st position of field
          IPOS= 1
          ILEN= FLEN
        END IF
        CALL CHRCHR (ILEN,CNONE,
     O               FLDSTR(IPOS))
      ELSE
C       put current value in text
        CALL INTCHR (IVAL,FLEN,JUST,
     O               J,FLDSTR)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FLSETR
     I                   (RDEF,FLEN,
     M                    RVAL,
     O                    FLDSTR)
C
C     + + + PURPOSE + + +
C     Given a current and default value for an real data field,
C     determine the appropriate initial value and fill in the data
C     field text string.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FLEN
      REAL        RDEF,RVAL
      CHARACTER*1 FLDSTR(FLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RDEF   - default value for field
C     FLEN   - length of data file
C     RVAL   - data field numberic value
C     FLDSTR - data field text string
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     J,IPOS,ILEN,JUST
      REAL        NODAT
      CHARACTER*1 CNONE(4)
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, DECCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      NODAT = -999.0
      JUST  = 0
C
      IF (ABS(RVAL-NODAT).LT.R0MIN) THEN
C       current value undefined, set to default
        RVAL= RDEF
      END IF
      IF (ABS(RVAL-NODAT).LT.R0MIN) THEN
C       no value use "none"
        IF (FLEN.GT.4) THEN
C         push 'none' to right side of field
          IPOS= FLEN- 3
          ILEN= 4
        ELSE
C         just start 'none' in 1st position of field
          IPOS= 1
          ILEN= FLEN
        END IF
        CALL CHRCHR (ILEN,CNONE,
     O               FLDSTR(IPOS))
      ELSE
C       put current value in text
        CALL DECCHR (RVAL,FLEN,JUST,
     O               J,FLDSTR)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FLSETD
     I                   (DDEF,FLEN,
     M                    DVAL,
     O                    FLDSTR)
C
C     + + + PURPOSE + + +
C     Given a current and default value for an double precision data field,
C     determine the appropriate initial value and fill in the data
C     field text string.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FLEN
      DOUBLE PRECISION DDEF,DVAL
      CHARACTER*1 FLDSTR(FLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DDEF   - default value for field
C     FLEN   - length of data file
C     DVAL   - data field numberic value
C     FLDSTR - data field text string
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     J,IPOS,ILEN,JUST
      DOUBLE PRECISION NODAT
      CHARACTER*1 CNONE(4)
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, DPRCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      NODAT = -999.0
      JUST  = 0
C
      IF (ABS(DVAL-NODAT).LT.D0MIN) THEN
C       current value undefined, set to default
        DVAL= DDEF
      END IF
      IF (ABS(DVAL-NODAT).LT.D0MIN) THEN
C       no value use "none"
        IF (FLEN.GT.4) THEN
C         push 'none' to right side of field
          IPOS= FLEN- 3
          ILEN= 4
        ELSE
C         just start 'none' in 1st position of field
          IPOS= 1
          ILEN= FLEN
        END IF
        CALL CHRCHR (ILEN,CNONE,
     O               FLDSTR(IPOS))
      ELSE
C       put current value in text
        CALL DPRCHR (DVAL,FLEN,JUST,
     O               J,FLDSTR)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FLSETC
     I                   (CDEF,FLEN,FDVAL,LMXRSL,RSPSTR,
     M                    CVAL,
     O                    FLDSTR)
C
C     + + + PURPOSE + + +
C     Given a current and default value for a character data field
C     with valid responses, determine the appropriate initial value
C     and fill in the data field text string.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CDEF,FLEN,FDVAL,LMXRSL,CVAL
      CHARACTER*1 RSPSTR(LMXRSL),FLDSTR(FLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CDEF   - default response value for field
C     FLEN   - length of data field
C     FDVAL  - pointer to valid character responses
C     LMXRSL - max length of character response buffer
C     RSPSTR - character array containing valid responses
C     CVAL   - data field response order value
C     FLDSTR - data field text string
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ILEN,NODAT
      CHARACTER*1 CNONE(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZCHRCH, CHRCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      NODAT = -999
C
      IF (CVAL.EQ.NODAT) THEN
C       current value undefined, use default
        CVAL = CDEF
      END IF
      IF (CVAL.GT.0) THEN
C       value exists, put in screen text
        CALL ZCHRCH (FLEN,FDVAL,
     I               CVAL,LMXRSL,RSPSTR,
     M               FLDSTR)
      ELSE
C       no value, use 'none'
        IF (FLEN.LT.4) THEN
          ILEN= FLEN
        ELSE
          ILEN= 4
        END IF
        CALL CHRCHR (ILEN,CNONE,
     M               FLDSTR)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETFN
     I                   (FNUM,FLNAME)
C
C     + + + PURPOSE + + +
C     Set the file names for all file type data fields on a
C     1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FNUM
      CHARACTER*64 FLNAME(FNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to set file names
C     FLNAME - array of file names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    FBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSTFNB
C
C     + + + END SPECIFICATIONS + + +
C
      FBAS= 1
      CALL QSTFNB (FNUM,FBAS,FLNAME)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSTFNB
     I                   (FNUM,FBAS,FLNAME)
C
C     + + + PURPOSE + + +
C     Set the file names for file type data fields, starting at file
C     field FBAS, on a 1-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FNUM,FBAS
      CHARACTER*64 FLNAME(FNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to set file names
C     FBAS   - base position within file fields to begin setting file names
C     FLNAME - array of file names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SFNB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2SFNB (FNUM,FBAS,NROW,FLNAME)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2STFN
     I                   (FNUM,NROW,FLNAME)
C
C     + + + PURPOSE + + +
C     Set the file names for all file fields and all data rows
C     on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FNUM,NROW
      CHARACTER*64 FLNAME(FNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to set file names
C     NROW   - number of rows of data
C     FLNAME - array of file names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    FBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SFNB
C
C     + + + END SPECIFICATIONS + + +
C
      FBAS= 1
      CALL Q2SFNB (FNUM,FBAS,NROW,FLNAME)
     O
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SFNB
     I                   (FNUM,FBAS,NROW,FLNAME)
C
C     + + + PURPOSE + + +
C     Set the file names for file fields, starting at file field FBAS,
C     and all data rows on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FNUM,FBAS,NROW
      CHARACTER*64 FLNAME(FNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to set file names
C     FBAS   - base position within file fields to begin setting file names
C     NROW   - number of rows of data
C     FLNAME - array of file names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SFNX
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2SFNX (FNUM,FBAS,NROW,BROW,FLNAME)
     O
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2SFNX
     I                   (FNUM,FBAS,NROW,BROW,FLNAME)
C
C     + + + PURPOSE + + +
C     Set the file names for file fields, starting at file field FBAS
C     and data row BROW, on a 2-dimensional data screen prior to editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FNUM,FBAS,NROW,BROW
      CHARACTER*64 FLNAME(FNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to set file names
C     FBAS   - base position within file fields to begin setting file names
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin setting file names
C     FLNAME - array of file names
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     FFLD,LFLD,FBSCNT,IROW,IBSROW,LLIN,ILEN
C
C     + + + FUNCTIONS + + +
      INTEGER     ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZLNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        FFLD  = 0
        LFLD  = 0
        FBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the file ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTF) THEN
C           file field found
            FFLD= FFLD+ 1
            IF (FFLD.GE.FBAS) THEN
C             start setting file names
              IF (ZDTYP.EQ.3) THEN
C               PRM1 type
                LLIN= FLIN(LFLD)
              ELSE IF (ZDTYP.EQ.4) THEN
C               PRM2 type
                LLIN= NMHDRW+ IROW
              END IF
              FBSCNT= FBSCNT+ 1
C             put supplied text in screen text
              IF (ZLNTXT(FLNAME(FBSCNT,IBSROW)).GT.FLEN(LFLD)) THEN
C               not enough room in field for supplied text, truncate
                ILEN= FLEN(LFLD)
              ELSE
                ILEN= ZLNTXT(FLNAME(FBSCNT,IBSROW))
              END IF
              ZMNTXT(LLIN)(SCOL(LFLD):SCOL(LFLD)+ILEN-1)=
     $          FLNAME(FBSCNT,IBSROW)(1:ILEN)
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. FBSCNT.LT.FNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETF
     I                   (FNUM,
     O                    FILUN)
C
C     + + + PURPOSE + + +
C     Get the unit numbers for all file type data fields on a
C     1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FNUM,FILUN(FNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to get unit numbers
C     FILUN  - array of file unit numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    FBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   QGETFB
C
C     + + + END SPECIFICATIONS + + +
C
      FBAS= 1
      CALL QGETFB (FNUM,FBAS,
     O             FILUN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGETFB
     I                   (FNUM,FBAS,
     O                    FILUN)
C
C     + + + PURPOSE + + +
C     Get the unit numbers for file type data fields, starting at file
C     field FBAS, on a 1-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FNUM,FBAS,FILUN(FNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to get unit numbers
C     FBAS   - base position within file fields to begin getting unit numbers
C     FILUN  - array of file unit numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GTFB
C
C     + + + END SPECIFICATIONS + + +
C
      NROW= 1
      CALL Q2GTFB (FNUM,FBAS,NROW,
     O             FILUN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GETF
     I                   (FNUM,NROW,
     O                    FILUN)
C
C     + + + PURPOSE + + +
C     Get the unit numbers for all file fields and all data rows
C     on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FNUM,NROW,FILUN(FNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to unit numbers
C     NROW   - number of rows of data
C     FILUN  - array of file unit numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    FBAS
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GTFB
C
C     + + + END SPECIFICATIONS + + +
C
      FBAS= 1
      CALL Q2GTFB (FNUM,FBAS,NROW,
     O             FILUN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GTFB
     I                   (FNUM,FBAS,NROW,
     O                    FILUN)
C
C     + + + PURPOSE + + +
C     Get the unit numbers for file fields, starting at file field FBAS,
C     and all data rows on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FNUM,FBAS,NROW,FILUN(FNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to get unit numbers
C     FBAS   - base position within file fields to begin getting unit numbers
C     NROW   - number of rows of data
C     FILUN  - array of file unit numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2GFBR
C
C     + + + END SPECIFICATIONS + + +
C
      BROW= 1
      CALL Q2GFBR (FNUM,FBAS,NROW,BROW,
     O             FILUN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2GFBR
     I                   (FNUM,FBAS,NROW,BROW,
     O                    FILUN)
C
C     + + + PURPOSE + + +
C     Get the unit numbers for file fields, starting at file field FBAS
C     and data row BROW, on a 2-dimensional data screen after editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FNUM,FBAS,NROW,BROW,FILUN(FNUM,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNUM   - number of file fields for which to get unit numbers
C     FBAS   - base position within file fields to begin getting unit numbers
C     NROW   - number of rows of data
C     BROW   - base row within data rows to begin getting unit numbers
C     FILUN  - array of file unit numbers
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     FFLD,LFLD,FBSCNT,IROW,IBSROW
C
C     + + + END SPECIFICATIONS + + +
C
      IBSROW= 0
      DO 100 IROW= BROW,BROW+NROW-1
C       loop through specified data rows
        FFLD  = 0
        LFLD  = 0
        FBSCNT= 0
        IBSROW= IBSROW+ 1
 50     CONTINUE
C         need to look through all fields to find the file ones
          LFLD= LFLD+ 1
          IF (FTYP(LFLD).EQ.FTF) THEN
C           file field found
            FFLD= FFLD+ 1
            IF (FFLD.GE.FBAS) THEN
C             start getting unit numbers
              FBSCNT= FBSCNT+ 1
              FILUN(FBSCNT,IBSROW)= ZFILUN(FFLD)
C            write (99,*) 'QGETF, assign ZFILUN',ZFILUN(FFLD),
C    $                    ' FFLD=',FFLD
            END IF
          END IF
        IF (LFLD.LT.NFLDS .AND. FBSCNT.LT.FNUM) GO TO 50
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSTBUF
     I                   (TLEN,NROW,TBUFF)
C
C     + + + PURPOSE + + +
C     Set the initial values for all fields on a 1 or 2-dimensional
C     screen in all data rows using a text buffer which contains
C     the values.  For 2-dimensional screens, values in the buffer
C     are assumed to be positioned at the starting column for each
C     field.  For 1-dimensional screens, values are assumed to be
C     packed together based on field widths.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TLEN,NROW
      CHARACTER*1 TBUFF(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TLEN   - length of text buffer rows
C     NROW   - number of data rows being set
C     TBUFF  - character array containing field values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   QSTBFB
C
C     + + + END SPECIFICATIONS + + +
C
      BROW = 1
      CALL QSTBFB (TLEN,NROW,BROW,TBUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSTBFB
     I                   (TLEN,NROW,BROW,TBUFF)
C
C     + + + PURPOSE + + +
C     Set the initial values for all fields on a 1 or 2-dimensional
C     screen in data rows BROW to BROW+NROW-1 using a text buffer
C     which contains the values.  For 2-dimensional screens, values
C     in the buffer are assumed to be positioned at the starting
C     column for each field.  For 1-dimensional screens, values are
C     assumed to be packed together based on field widths.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TLEN,NROW,BROW
      CHARACTER*1 TBUFF(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TLEN   - length of text buffer rows
C     NROW   - number of data rows being set
C     BROW   - starting row on screen to begin setting values
C     TBUFF  - character array containing field values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,L,LFLEN,IROW,IBSROW,IFLD,RFLD,DFLD,TFLD,
     $            ITMP,CTMP,LMXRSL
      REAL        RTMP
      DOUBLE PRECISION DTMP
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, CHRCHR, FLSETI, FLSETR, FLSETD, FLSETC
C
C     + + + END SPECIFICATIONS + + +
C
C     set local version of max response buffer length to remove
C     any chance of parameter MXRSLN being modified
      LMXRSL= MXRSLN
C
      IBSROW= 0
      DO 200 IROW = BROW,BROW+NROW-1
C       loop through specified data rows
        IBSROW = IBSROW + 1
        IFLD = 0
        RFLD = 0
        DFLD = 0
        TFLD = 0
        J = 1
        DO 100 I = 1,NFLDS
          IF (ZDTYP.EQ.3) THEN
C           PRM1 type
            IF (I.GT.1) THEN
C             determine next field start position
              J = J + FLEN(I-1)
            END IF
            L = FLIN(I)
          ELSE IF (ZDTYP.EQ.4) THEN
C           PRM2 type
            J = SCOL(I)
            L = NMHDRW + IROW
          END IF
          LFLEN= LENSTR(FLEN(I),TBUFF(J,IBSROW))
          IF (LFLEN.GT.0) THEN
C           a string in this field to put in screen text
            CALL CHRCHR (FLEN(I),TBUFF(J,IBSROW),ZMNTX1(SCOL(I),L))
          ELSE
C           use default
            IF (FTYP(I) .EQ. FTI) THEN
C             integer field
              ITMP = -999
              IFLD = APOS(I)
              CALL FLSETI (IDEF(IFLD),FLEN(I),
     M                     ITMP,
     O                     ZMNTX1(SCOL(I),L))
            ELSE IF (FTYP(I) .EQ. FTR) THEN
C             real field
              RTMP = -999.0
              RFLD = APOS(I)
              CALL FLSETR (RDEF(RFLD),FLEN(I),
     M                     RTMP,
     O                     ZMNTX1(SCOL(I),L))
            ELSE IF (FTYP(I) .EQ. FTD) THEN
C             double precision field
              DTMP = -999.0
              DFLD = APOS(I)
              CALL FLSETD (DDEF(DFLD),FLEN(I),
     M                     DTMP,
     O                     ZMNTX1(SCOL(I),L))
            ELSE IF (FTYP(I) .EQ. FTC .AND. FDVAL(I).GT.0) THEN
C             character field with valid responses
              CTMP = -999
              TFLD = APOS(I)
              CALL FLSETC (CDEF(TFLD),FLEN(I),FDVAL(I),LMXRSL,RSPSTR,
     M                     CTMP,
     O                     ZMNTX1(SCOL(I),L))
            END IF
          END IF
 100    CONTINUE
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGTBUF
     I                   (TLEN,NROW,
     O                    TBUFF)
C
C     + + + PURPOSE + + +
C     Get the edited values for all fields on a 1 or 2-dimensional
C     screen in all data rows using a text buffer which contains
C     the values.  For 2-dimensional screens, values in the buffer
C     are assumed to be positioned at the starting column for each
C     field.  For 1-dimensional screens, values are assumed to be
C     packed together based on field widths.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TLEN,NROW
      CHARACTER*1 TBUFF(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TLEN   - length of text buffer rows
C     NROW   - number of data rows being retrieved
C     TBUFF  - character array containing field values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    BROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   QGTBFB
C
C     + + + END SPECIFICATIONS + + +
C
      BROW = 1
      CALL QGTBFB (TLEN,NROW,BROW,TBUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QGTBFB
     I                   (TLEN,NROW,BROW,
     O                    TBUFF)
C
C     + + + PURPOSE + + +
C     Get the edited values for all fields on a 1 or 2-dimensional
C     screen in data rows BROW to BROW+NROW-1 using a text buffer
C     which contains the values.  For 2-dimensional screens, values
C     in the buffer are assumed to be positioned at the starting
C     column for each field.  For 1-dimensional screens, values are
C     assumed to be packed together based on field widths.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TLEN,NROW,BROW
      CHARACTER*1 TBUFF(TLEN,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TLEN   - length of text buffer rows
C     NROW   - number of data rows being retrieved
C     BROW   - starting row on screen to begin getting values
C     TBUFF  - character array containing field values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,L,LFLEN,IROW,IBSROW
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, CHRCHR, ZIPC
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ZIPC (TLEN*NROW,BLNK,TBUFF)
C
      IBSROW= 0
      DO 200 IROW = BROW,BROW+NROW-1
C       loop through specified data rows
        IBSROW = IBSROW + 1
        J = 1
        DO 100 I = 1,NFLDS
          IF (ZDTYP.EQ.3) THEN
C           PRM1 type
            IF (I.GT.1) THEN
C             determine next field start position
              J = J + FLEN(I-1)
            END IF
            L = FLIN(I)
          ELSE IF (ZDTYP.EQ.4) THEN
C           PRM2 type
            J = SCOL(I)
            L = NMHDRW + IROW
          END IF
          LFLEN= LENSTR(FLEN(I),ZMNTX1(SCOL(I),L))
          IF (LFLEN.GT.0) THEN
C           a string in this field to get from screen text
            CALL CHRCHR (FLEN(I),ZMNTX1(SCOL(I),L),TBUFF(J,IBSROW))
          END IF
 100    CONTINUE
 200  CONTINUE
C
      RETURN
      END
