C
C
C
      SUBROUTINE   CKSPAC
     I                   ( MESSFL, LPTH, PTHNAM,
     I                     NSA, NSP, NDP, NDN, NUP,
     O                     RETC )
C     This subroutine checks that the values specified for data-set
C     space allocation are valid.  If not, a warning message is 
C     printed to the screen.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL, LPTH, NSA, NSP, NDP, NDN, NUP, RETC
      CHARACTER*8  PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     LPTH   - length of path name
C     PTHNAM - window paths, keypath to this window
C     NSA    - number of search attributes
C     NSP    - space for search attributes, note that the space for
C              attributes must be at least as large as the number of
C              attributes.  Some attributes (ie staid, stanam) require
C              extra space.
C     NDP    - space for data group pointers
C     NDN    - number of down pointers to data sets
C     NUP    - number of up pointers to data sets
C     RETC   - return code
C                 0 - numbers look ok
C              -140 - problem with number of attributes or data groups
C              -141 - number of attributes exceeds space for attributes
C              -142 - total space too big
C              -143 - number of attributes exceeds space for attributes
C                     and total space too big
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU, SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   CKSPCE, ZWNSOP, PRNTXT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SCLU
     $      /  49 /
C
C     + + + END SPECIFICATIONS + + +
C
      CALL CKSPCE ( NSA, NSP, NDP, NDN, NUP, RETC )
      IF (RETC .NE. 0) THEN
C       problem with space allocation
        IF (-144 .LE. RETC  .AND.  RETC .LE. -141) THEN
C         error is within expected range
          SGRP = -RETC
        ELSE
C         error not in expected range, use default
          SGRP = 144
        END IF
        CALL ZWNSOP ( LPTH, PTHNAM )
        CALL PRNTXT ( MESSFL, SCLU, SGRP )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKSPCE
     I                   ( NSA, NSP, NDP, NDN, NUP,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     This subroutine checks that the values specified for data-set
C     space allocation are valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NSA, NSP, NDP, NDN, NUP, RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSA    - number of search attributes
C     NSP    - space for search attributes, note that the space for
C              attributes must be at least as large as the number of
C              attributes.  Some attributes (ie staid, stanam) require
C              extra space.
C     NDP    - space for data group pointers
C     NDN    - number of down pointers to data sets
C     NUP    - number of up pointers to data sets
C     RETC   - return code
C                 0 - numbers look ok
C              -141 - numbers of attributes and data groups ok, but more
C                     attribute space requested then can be used
C              -142 - problem with number of attributes or data groups
C              -143 - number of attributes exceeds space for attributes
C              -144 - total space too big
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SPACE, SPACMX, SPACAT, NSAMN, NSAMX, NDPMN, NDPMX
C
C     + + + LOCAL DEFINITIONS + + +
C     SPACMX - maximum space available for attributes, data group
C              pointers, and up & down data-set pointers
C              fixed space required for pointers and counters
C                 12 = 1-12 pointers
C                  2 = counts for up and down data set pointers
C                  4 = attr count & pointer, data count & pointer
C                 ?? = minimum space required for first data group
C                 --
C                 18
C              spacmx = record size - fixed space - first data blocks
C                     = 512 - 18 - 20 = 474
C     SPACAT - extra space that might be needed for attribute arrays
C              staid (3), stanam (11), descrp (19), agency (1)
C              grpnam (1), gucode (1)
C              36 is about 40
C     NSAMN  - minimum value for number of search attributes (NSA)
C     NSAMX  - maximum value for number of search attributes (NSA)
C     NDPMN  - minimum value for number of data groups (NDP)
C     NDPMX  - maximum value for number of data groups (NDP)
C     SPACE  - space allocated for attributes, data group pointers,
C              and up & down data-set pointers
C                ndn = down data set pointers
C                nup = up data set pointers
C              2*nsa = attribute indexes and pointers
C                nsp = space for attributes
C                ndp = data group pointers
C              
C     + + + DATA INITIALIZASTIONS + + +
C                     attributes   data groups
      DATA   SPACMX, SPACAT,  NSAMN, NSAMX, NDPMN, NDPMX
     $     /    474,     40,      8,   150,     1,   400 /
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate space requested
      SPACE = 2 * NSA  +  NSP  +  NDP  +  NDN  +  NUP
C
C     is there enough room?
      IF (SPACE .LE. SPACMX) THEN
C       total space is small enough
        IF (NSP .GE. NSA) THEN
C         enough space for the number of attributes
          IF (NSAMN .LE. NSA  .AND.  NSA .LE. NSAMX  .AND.
     $        NDPMN .LE. NDP  .AND.  NDP .LE. NDPMX) THEN
C           number of attributes and data groups ok
            IF (NSP .LE. NSA+SPACAT) THEN
C             reasonable amount of space for number of attributes
              RETC = 0
            ELSE
C             requested more attribute space then can be used
              RETC = -141
            END IF
          ELSE
C           problem with number of attributes or data groups
            RETC = -142
          END IF
        ELSE
C         number of attributes exceeds space for attributes
          RETC = -143
        END IF
      ELSE
C       total space is too big
        RETC = -144
      END IF
C
      RETURN
      END
