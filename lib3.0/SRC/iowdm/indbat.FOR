C
C
C
      SUBROUTINE   INITAT
     I                   ( VERS, SPACE, TIME )
C
C     + + + PURPOSE + + +
C     Initialize arrays for attribute identification.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   VERS, SPACE(5), TIME(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VERS   - identifier for additional attributes to be added
C              0 - no additional attributes
C              1 - additional attributes generally associated with
C                  station header records
C              2 - additional attributes generally associated with
C                  basin characteristics
C              3 - additional attributes generally associtated with
C                  n-day records
C     SPACE  - defaults for space allocation
C              (1) - maximum number of attributes
C              (2) - maximum space for attributes
C              (3) - maximum number of data groups
C              (4) - number of up pointers
C              (5) - number of down pointers
C     TIME   - defaults for time series attributes
C              (1) - tcode      (5) - vbtime
C              (2) - tgroup     (6) - tsbyr
C              (3) - tsform     (7) - tsstep
C              (4) - compfg
C
C     + + + INCLUDES + + +
      INCLUDE 'catrib.inc'
C
C     + + + PARAMETERS + + +
      INTEGER   IMXS, RMXS, CMXS, IMXV, RMXV, CMXV, MXVS
      PARAMETER ( IMXS=13, RMXS=1, CMXS=3 )
      PARAMETER ( IMXV=6,  RMXV=7, CMXV=4 )
      PARAMETER ( MXVS=3 )
C
C     + + + DEFINITIONS + + +
C     IMXS   - number of shared integer attributes
C     RMXS   - number of shared real attributes
C     CMXS   - number of shared character attributes
C     IMXV   - maximum number of version specific integer attributes
C     RMXV   - maximum number of version specific real attributes
C     CMXV   - maximum number of version specific character attributes
C     MXVS   - number of recognized versions
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INDXS(IMXS), RNDXS(RMXS), CNDXS(CMXS),
     $          CLENS(CMXS),
     $          INDX(IMXV,MXVS), RNDX(RMXV,MXVS), CNDX(CMXV,MXVS),
     $          CLEN(CMXV,MXVS), AMXV(3,MXVS),
     $          I, J, L, LPT, IMISS
      REAL      RMISS
      CHARACTER*1 CMISS
      CHARACTER*6 INAMS(IMXS), RNAMS(RMXS), CNAMS(CMXS),
     $            INAM(IMXV,MXVS), RNAM(RMXV,MXVS), CNAM(CMXV,MXVS)
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYI, ZIPI, ZIPR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
C          integer attributes, shared
      DATA INAMS, INDXS
     $   / 'TCODE ', 'TGROUP', 'TSFORM', 'COMPFG', 'VBTIME', 'TSBYR ',
     $     'TSSTEP', 'space3', 'space4', 'space5', 'space1', 'space2',
     $     'ISTAID',
     $        17,       34,       84,       83,       85,       27,
     $        33,        0,        0,        0,        0,        0,
     $        51 /
C          real and character attributes, shared (name, index, length)
      DATA RNAMS, CNAMS, RNDXS, CNDXS, CLENS
     $   / 'TSFILL', 'STAID ', 'TSTYPE', 'STANAM',
     $        32,        2,        1,       45,
     $                  16,        4,       48 /
C           for each version, number of integer, real, & character
      DATA  AMXV / 6,7,4,   2,0,1,   4,4,0 /
C          additional integer attributes (name, index)
      DATA INAM, INDX
     $   / 'PARMCD', 'STATCD', 'STFIPS', 'DSCODE', 'COCODE', 'HUCODE',
     $     'STFIPS', 'DSCODE',    4*'      ',
     $     'PARMCD', 'STATCD', 'SEASBG', 'SEASND',    2*'      ',
     $        56,       57,       41,       42,        6,        4,
     $        41,       42,       4*0,
     $        56,       57,      256,      257,       2*0 /
C          additional real attributes (name, index)
      DATA RNAM, RNDX
     $   / 'LATDEG', 'LNGDEG', 'DAREA ', 'CONTDA', 'DATUM ', 'WELLDP',
     $     'BASEQ ',   7*'      ',
     $     'XSECLC', 'DEPTH ', 'LATDEG', 'LNGDEG', 3*'      ',
     $         8,        9,       11,       43,      264,       47,
     $        49,      7*0,
     $       258,      259,         8,        9,      3*0 /
C          additional character attributes (name, index, length)
      DATA CNAM, CNDX, CLEN
     $   / 'SITECO', 'AQTYPE', 'GUCODE', 'AGENCY',
     $     'AGENCY', 3*'      ',  4*'      ',
     $        44,       48,       46,       40,
     $        40,      3*0,      4*0,
     $         4,        4,        8,        8,
     $         8,      3*0,      4*0 /
C          missing value indicators for attributes
      DATA IMISS, RMISS,  CMISS
     $     /-999, -999.,   ' ' /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 6A1 )
C
 3001 format (//, ' <-- initat:  aver =  ',  i5,
     $         /, '              aimx =  ',  i5,
     $         /, '              armx =  ',  i5,
     $         /, '              acmx =  ',  i5 )
 3002 format (//, '              aindx:  ', 10i5, /, 22x, 9i5,
     $         /, '              arndx:  ',  8i5,
     $         /, '              acndx:  '   7i5,
     $         /, '              aclen:  ',  7i5 )
 3003 format (//, '              aival:  ',  5i10, /, 22x, 5i10, 
     $         /, '                      ',  5i10, /, 22x, 4i10,
     $         /, '              arval:  ',  5f10.0, /, 22x, 3f10.0,
     $         /, '              acval:  ',  50a1, /22x, 42a1 )
C
C     + + + END SPECIFICATIONS + + +
C
C
C     save version
      AVER = VERS
C     set maximums for each shared atribute type
      AIMX = IMXS
      ARMX = RMXS
      ACMX = CMXS
C
C     initialize the arrays of indexes and length for shared attributes
      CALL COPYI ( AIMX, INDXS, AINDX )
      CALL COPYI ( ARMX, RNDXS, ARNDX )
      CALL COPYI ( ACMX, CNDXS, ACNDX )
      CALL COPYI ( ACMX, CLENS, ACLEN )
C
C     initialize the names arrays for shared attributes
      DO 110 I = 1, AIMX
        READ (INAMS(I),1000) (AINAME(J,I), J = 1, 6)
 110  CONTINUE
      DO 120 I = 1, ARMX
        READ (RNAMS(I),1000) (ARNAME(J,I), J = 1, 6)
 120  CONTINUE
      DO 130 I = 1, ACMX
        READ (CNAMS(I),1000) (ACNAME(J,I), J = 1, 6)
 130  CONTINUE
C
      IF (VERS .GT. 0  .AND.  VERS .LE. MXVS) THEN
C       recognized version, add any version specific attributes
        IF (AMXV(1,VERS) .GT. 0) THEN
C         additional integer attributes
          LPT = AIMX + 1
          AIMX = AIMX + AMXV(1,VERS)
          CALL COPYI ( AMXV(1,VERS), INDX(1,VERS), AINDX(LPT) )
          I = 0
          DO 210 L = LPT, AIMX
            I = I + 1
            READ (INAM(I,VERS),1000) (AINAME(J,L), J = 1, 6)
 210      CONTINUE
        END IF
        IF (AMXV(2,VERS) .GT. 0) THEN
C         additional real attributes
          LPT = ARMX + 1
          ARMX = ARMX + AMXV(2,VERS)
          CALL COPYI ( AMXV(2,VERS), RNDX(1,VERS), ARNDX(LPT) )
          I = 0
          DO 220 L = LPT, ARMX
            I = I + 1
            READ (RNAM(I,VERS),1000) (ARNAME(J,L), J = 1, 6)
 220      CONTINUE
        END IF
        IF (AMXV(3,VERS) .GT. 0) THEN
C         additional character attributes
          LPT = ACMX + 1
          ACMX = ACMX + AMXV(3,VERS)
          CALL COPYI ( AMXV(3,VERS), CNDX(1,VERS), ACNDX(LPT) )
          CALL COPYI ( AMXV(3,VERS), CLEN(1,VERS), ACLEN(LPT) )
          I = 0
          DO 230 L = LPT, ACMX
            I = I + 1
            READ (CNAM(I,VERS),1000) (ACNAME(J,L), J = 1, 6)
 230      CONTINUE
        END IF
      END IF
C     zero or blank out attribute values
      CALL ZIPI ( AIMX, IMISS, AIVAL )
      CALL ZIPR ( ARMX, RMISS, ARVAL )
      CALL ZIPC ( ACLONG, CMISS, ACVAL )
C     special cases
C     tsfill
      ARVAL(1) = -99999.
C     space allocations
      I = 5
      CALL COPYI ( I, SPACE, AIVAL(8) )
C     time series attributes
      I = 7
      CALL COPYI ( I, TIME, AIVAL(1) )
c     write (99,3001) aver, aimx, armx, acmx
c     write (99,3002) aindx, arndx, acndx, aclen
c     write (99,3003) aival, arval, acval
C
      RETURN
      END
C
C
C
      SUBROUTINE   INITMS
     I                   ( MISVAL )
C
C     + + + PURPOSE + + +
C     change value to use for missing values.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL      MISVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MISVAL - value to use for missing data
C
C     + + + INCLUDES + + +
      INCLUDE 'catrib.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     tsfill
      ARVAL(1) = MISVAL
C
      RETURN
      END
C
C
C
      SUBROUTINE   INATRB
     I                   ( MESSFL, FE, WDMFL, DSN,
     O                     RET )
C
C     + + + PURPOSE + + +
C     Adds attributes to a data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, FE, WDMFL, DSN, RET
C
C     + + + ARGUMENT DEFINITION + + +
C     MESSFL - Fortran unit number of the message file
C     FE     - Fortran unit number of error file
C     WDMFL  - Fortran unit number of the wdm file
C     DSN    - data-set number for attributes
C     RET   - return code
C
C     + + + COMMONS + + +
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   K, J, LEN, RETC, I, LNA
C
C     + + + LOCAL DEFINITIONS + + +
C     RETC   - return code from putting attributes
C               -81 - data set does not exist
C              -101 - incorrect character value for attribute
C              -102 - attribute already in data set
C              -103 - no room in data set for attribute
C              -104 - data present, can't update attribute
C              -105 - attribute not allowed for this type data set
C                     ignored, assumes that time attribute to table ds
C              -108 - incorrect integer value for attribute
C              -109 - incorrect real value for attribute
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBSAI, WDBSAR, WDBSAC, LENSTR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (//,2X, 'DSN', I6, ':    adding attributes',
     $        //,2X, '              Name    Value' )
 2001 FORMAT (   2X, '              ', 6A1, 2X, I10 )
 2002 FORMAT (   2X, '              ', 6A1, 2X, G12.6 )
 2003 FORMAT (   2X, '              ', 6A1, 2X, 48A1 )
 2011 FORMAT (   2X, '    --->      ', 6A1, 2X, 'return code = ', I4,
     $               ',  value = ', I10 )
 2012 FORMAT (   2X, '    --->      ', 6A1, 2X, 'return code = ', I4,
     $               ',  value = ', G12.6 )
 2013 FORMAT (   2X, '    --->      ', 6A1, 2X, 'return code = ', I4,
     $               ',  value = ', 48A1 )
C
 3000 format (//, ' --> inatrb:   dsn =  ',  i5,
     $         /, '              aver =  ',  i5,
     $         /, '              aimx =  ',  i5,
     $         /, '              armx =  ',  i5,
     $         /, '              acmx =  ',  i5 )
 3001 format (//, ' <-- inatrb:  aver =  ',  i5,
     $         /, '              aimx =  ',  i5,
     $         /, '              armx =  ',  i5,
     $         /, '              acmx =  ',  i5 )
 3002 format (//, '              aindx:  ', 10i5, /, 22x, 9i5,
     $         /, '              arndx:  ',  8i5,
     $         /, '              acndx:  '   7i5,
     $         /, '              aclen:  ',  7i5 )
 3003 format (//, '              aival:  ',  5i10, /, 22x, 5i10, 
     $         /, '                      ',  5i10, /, 22x, 4i10,
     $         /, '              arval:  ',  5f10.0, /, 22x, 3f10.0,
     $         /, '              acval:  ',  50a1, /22x, 42a1 )
C
C     + + + END SPECIFICATIONS + + +
C
c     write (99,3000) dsn, aver, aimx, armx, acmx
c     write (99,3002) aindx, arndx, acndx, aclen
c     write (99,3003) aival, arval, acval
C     write out heading and dsn
      WRITE (FE,2000) DSN
C
      RET = 0
      IF (AIMX .GT. 0) THEN
C       add integer attributes
        RET = 0
        LEN = 1
        DO 100 K = 1, AIMX
          IF (AIVAL(K) .NE. -999  .AND.  AINDX(K) .GT. 0) THEN
C           valid integer attribute
            CALL WDBSAI ( WDMFL, DSN, MESSFL, AINDX(K), LEN, AIVAL(K),
     O                    RETC )
            IF (RETC .EQ. 0) THEN
C             document add
              WRITE (FE,2001) (AINAME(J,K),J=1,6), AIVAL(K)
            ELSE IF (RETC .EQ. -105  .OR.
     $               RETC .EQ. -102  .OR.  RETC .EQ. -104) THEN
C             wrong ds type, assume time attribute with table data set
C             already present or data present and can't update
C             ignore the warning
            ELSE
C             error in putting attribute
              WRITE (FE,2011) (AINAME(J,K),J=1,6), RETC, AIVAL(K)
              RET = RET + 1
            END IF
CPRH        looks like this is not needed
CPRH            IF (RETC .NE. 0) THEN
CPRHC             error adding attribute
CPRH              RET = RET + 1
CPRH            END IF
          END IF
 100    CONTINUE
      END IF
C
      IF (ARMX .GT. 0) THEN
C       add real attributes
        LEN = 1
        DO 200 K = 1, ARMX
          IF ((ABS(ARVAL(K)+999.) .GT. .0001 .AND. ARNDX(K) .GT. 0) .OR.
     $        ARNDX(K) .EQ. 32) THEN
C           valid real attribute (32 is tsfill)
            CALL WDBSAR ( WDMFL, DSN, MESSFL, ARNDX(K), LEN, ARVAL(K),
     O                    RETC )
            IF (RETC .EQ. 0) THEN
C             document add
              WRITE (FE,2002) (ARNAME(J,K),J=1,6), ARVAL(K)
            ELSE IF (RETC .EQ. -102  .OR.  RETC .EQ. -104) THEN
C             already present or data present and can't update
C             ignore warning
            ELSE
C             error in putting attribute
              WRITE (FE,2012) (ARNAME(J,K),J=1,6), RETC, ARVAL(K)
              RET = RET + 1
            END IF
          END IF
 200    CONTINUE
      END IF
C
      IF (ACMX .GT. 0) THEN
C       add character attributes
        I = 1
        DO 300 K = 1, ACMX
          LEN = ACLEN(K)
          IF (ACNDX(K) .GT. 0  .AND.  LEN .GT. 0) THEN
C           attribute value could be here
            LNA = LENSTR ( LEN, ACVAL(I) )
          ELSE
C           no attribute is defined
            LNA = 0
          END IF
          IF (LNA .GT. 0) THEN
C           attribute is present
            CALL WDBSAC ( WDMFL, DSN, MESSFL, ACNDX(K), LEN, ACVAL(I),
     O                    RETC)
            IF (RETC .EQ. 0) THEN
C             document add
              WRITE (FE,2003) (ACNAME(J,K),J=1,6),
     $                        (ACVAL(J),J=I,I+LEN-1)
            ELSE IF (RETC .EQ. -101 .AND.
     $        ACVAL(I) .EQ. 'n'    .AND.  ACVAL(I+1) .EQ. 'o' .AND.
     $        ACVAL(I+2) .EQ. 'n'  .AND.  ACVAL(I+3) .EQ. 'e') THEN
C             invalid charater type is 'none', assume not defined & ignore
              RET = RET + 0
            ELSE IF (RETC .EQ. -102  .OR.  RETC .EQ. -104) THEN
C             already present or data present and can't update
C             ignore the warning
            ELSE
C             error in putting attribute
              WRITE (FE,2013) (ACNAME(J,K),J=1,6), RETC,
     $                        (ACVAL(J),J=I,I+LEN-1)
              RET = RET + 1
            END IF
          END IF
C         update position in attribute array
          I = I + LEN
 300    CONTINUE
      END IF
c     write (99,3001) aver, aimx, armx, acmx
c     write (99,3002) aindx, arndx, acndx, aclen
c     write (99,3003) aival, arval, acval
C
      RETURN
      END
C
C
C
      SUBROUTINE   INITAX
C
C     + + + PURPOSE + + + 
C     Initialize attribute values to no value.
C
C     + + + INCLUDES + + +
      INCLUDE 'catrib.inc'
C
C     + + + PARAMETERS + + +
      INTEGER    MXVS
      PARAMETER (MXVS = 4)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      VERS, IPT, MISSI, AMXV(3,MXVS), IPTA(3)
      REAL         MISSR
      CHARACTER*1  BLANK
C
C     + + + LOCAL DEFINITIONS + + + 
C     IPTA   - starting location in attribute value arrays for
C              attributes that need to be initialized to none.
C              (1) - integer, the first 12 integer attributes and
C                    space allocations are not re-initialized.
C              (2) - real, the first, tsfill, is not re-initialized
C              (3) - character, the last, agency is not re-initialized
C     AMXV   - number of integer, real and character attributes to
C              be reinitialized for each attribute.  The version number
C              is v:
C              (1,v) - number of integer attributes
C              (2,v) - number of real attributes
C              (3,v) - total length of character attributess
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, ZIPR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   MISSI, MISSR, BLANK
     $     /  -999, -999.,   ' ' /
      DATA   IPTA,     AMXV
     $     / 13,2,1,   7,7,84, 3,0,68, 5,4,68, 1,0,68 /
 3000 format (//, ' --> initax:  aver =  ',  i5,
     $         /, '              aimx =  ',  i5,
     $         /, '              armx =  ',  i5,
     $         /, '              acmx =  ',  i5 )
 3001 format (//, ' <-- initax:  aver =  ',  i5,
     $         /, '              aimx =  ',  i5,
     $         /, '              armx =  ',  i5,
     $         /, '              acmx =  ',  i5 )
 3002 format (//, '              aindx:  ', 10i5, /, 22x, 9i5,
     $         /, '              arndx:  ',  8i5,
     $         /, '              acndx:  '   7i5,
     $         /, '              aclen:  ',  7i5 )
 3003 format (//, '              aival:  ',  5i10, /, 22x, 5i10, 
     $         /, '                      ',  5i10, /, 22x, 4i10,
     $         /, '              arval:  ',  5f10.0, /, 22x, 3f10.0,
     $         /, '              acval:  ',  50a1, /22x, 42a1 )

C
C     + + + END SPECIFICATIONS + + +
C
c     write (99,3000) aver, aimx, armx, acmx
c     write (99,3002) aindx, arndx, acndx, aclen
c     write (99,3003) aival, arval, acval
      VERS = AVER
      IF (VERS .LT. 1  .OR.  VERS .GT. MXVS) THEN
C       version is not recognized, default to mininum
        VERS = MXVS
      END IF
      IF (AMXV(1,VERS) .GT. 0) THEN
C       integer attributes
        IPT = IPTA(1)
        CALL ZIPI ( AMXV(1,VERS), MISSI, AIVAL(IPT) )
      END IF
      IF (AMXV(2,VERS) .GT. 0) THEN
C       real attributes
        IPT = IPTA(2)
        CALL ZIPR ( AMXV(2,VERS), MISSR, ARVAL(IPT) )
      END IF
      IF (AMXV(3,VERS) .GT. 0) THEN
C       character attributes
        IPT = IPTA(3)
        CALL ZIPC ( AMXV(3,VERS), BLANK, ACVAL(IPT) )
      END IF
c     write (99,3001) aver, aimx, armx, acmx
c     write (99,3002) aindx, arndx, acndx, aclen
c     write (99,3003) aival, arval, acval
C
      RETURN
      END
