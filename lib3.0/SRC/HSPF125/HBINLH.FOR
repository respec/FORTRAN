      SUBROUTINE   BINIT (
     O                    BINFRM)
C
C     + + + PURPOSE + + +
C     Set up any necessary binary file initialization, and determine
C     necessary file format type.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*30 BINFRM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BINFRM - format for binary file - may be "FORMATTED" or "UNFORMATTED"
C
C     + + + END SPECIFICATIONS + + +
C
      BINFRM= "UNFORMATTED"
C
      RETURN
	END
C
C
C
      SUBROUTINE BINHED (BINU,OPTYP,OPNO,SECT,VCOUNT,CLEN,CHEAD)
C
C     + + + PURPOSE + + +
C     Write a binary output header record for a section of an operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       BINU,OPNO,VCOUNT,CLEN(VCOUNT)
      CHARACTER*8   OPTYP,SECT
      CHARACTER*256 CHEAD(VCOUNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BINU   - fortran unit number of binary outout file
C     OPTYP  - operation type
C     OPNO   - operation ID number (LSID,RCHNO...)
C     SECT   - model section (e.g. PWATER, HYDR..)
C     VCOUNT - number of variables output
C     CLEN   - array of variable name lengths
C     CHEAD  - array of variable names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,J
C
C     + + + END SPECIFICATIONS + + +
C
C     direct write to unformatted sequential file
      WRITE (BINU) 0,OPTYP,OPNO,SECT,
     $            (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,VCOUNT)
C
      RETURN
	END
C
C
C    
      SUBROUTINE BINREC (BINU,OPTYP,OPNO,SECT,UNITFG,LEV,EXDAT,VCOUNT,
     I                   APRINT)
C
C     + + + PURPOSE + + +
C     Write a binary output data record for a section of an operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     BINU,OPNO,UNITFG,LEV,EXDAT(5),VCOUNT
      CHARACTER*8 OPTYP,SECT
      REAL        APRINT(VCOUNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BINU   - fortran unit number of binary outout file
C     OPTYP  - operation type
C     OPNO   - operation ID number (LSID,RCHNO...)
C     SECT   - model section (e.g. PWATER, HYDR..)
C     UNITFG - unit system: 1=English, 2=Metric
C     LEV    - output level (2=PIVL,3=Day,4=Month,5=Year
C     EXDAT  - date at end of printout interval
C     VCOUNT - number of variables output
C     APRINT - array of variable values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
C
C     + + + END SPECIFICATIONS + + +
C
C     direct write to unformatted sequential file
      WRITE (BINU) 1,OPTYP,OPNO,SECT,UNITFG,
     $             LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,VCOUNT)
C
      RETURN
	END
