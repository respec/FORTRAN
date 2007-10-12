C
C
C
      SUBROUTINE   ANGTXA
     I                    (XPOS,YPOS,LEN,STR,HV)
C
C     + + + PURPOSE + + +
C     routine to write out character*1 graphics strings
C     one character at a time. Special version for the PRIME with
C     DISSPLA to compensate for bugs.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN, HV
      REAL        XPOS,YPOS
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XPOS   - x-coordinate of position to start writing string
C     YPOS   - y-coordinate of position to start writing string
C     LEN    - length of character*1 array
C     STR    - character*1 array for graphics output
C     HV     - 1-horizontal lettering,   2- vertical lettering
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ERRIND,WSID, N, OL
      INTEGER     I,LLEN
      REAL        LXPOS,LYPOS,NXPOS,NYPOS,TXEXPX(4),TXEXPY(4)
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + INTRINSICS + + +
C     (none)
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, GTX, GQTXX, GQACWK
C
C     + + + DATA INITIALIZATIONS + + +
C
C     + + + END SPECIFICATIONS + + +
C
C     for PRIME because of GKS bug
C     special treatment for vertical lettering
C     get workstation id
      N = 1
      CALL GQACWK (N,ERRIND,OL,WSID)
C     initialize local positions
      LXPOS= XPOS
      LYPOS= YPOS
C     get length of character string
      LLEN = LENSTR (LEN,STR)
      DO 8 I= 1,LLEN
        CALL GTX (LXPOS,LYPOS,STR(I))
C       get next characters coordinates
        CALL GQTXX (WSID,LXPOS,LYPOS,STR(I),
     O              ERRIND,NXPOS,NYPOS,TXEXPX,TXEXPY)
        IF (HV .EQ. 1) THEN
C         horizontal lettering
          LXPOS = NXPOS
          LYPOS = YPOS
        ELSE
          LXPOS = XPOS
          LYPOS = LYPOS + (NYPOS-LYPOS)*1.5
        END IF
 8    CONTINUE
C
      RETURN
      END
C
C
C
      REAL FUNCTION   ANGLEN
     I                       (LEN,CBUF)
C
C     + + + PURPOSE + + +
C     This routine computes the length of a character string in world
C     corrdinates using GKS routines.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEN
      CHARACTER*1  CBUF(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - length of character string
C     CBUF   - character string to be plotted
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, IY, N, OL
      INTEGER    WSID, ERRIND
      REAL   LXP,LYP,NXPOS,NYPOS,TXEXPX(4),TXEXPY(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL   GQTXX, GQACWK
C
C     + + + END SPECIFICATIONS + + +
C
C     special treatment for vertical lettering
C     get workstation id
      N = 1
      CALL GQACWK (N,ERRIND,OL,WSID)
C
      IY = 0
      LYP = 0.0
      LXP = 0.0
      ANGLEN = 0.0
C     WRITE(FE,*) ' Before and after concatenation points, x then y.'
C
C
      DO 10 I = 1,LEN
        CALL GQTXX (WSID, LXP,LYP, CBUF(I),
     O              ERRIND,NXPOS,NYPOS,TXEXPX,TXEXPY)
C       WRITE(FE,*) LXP,NXPOS,LYP,NYPOS
C
        IF (NXPOS-LXP .GT. NYPOS-LYP) THEN
C         horizontal text
          ANGLEN = NXPOS - LXP + ANGLEN
          LXP = NXPOS
        ELSE
C         vertical text
          ANGLEN = NYPOS - LYP + ANGLEN
          LYP = NYPOS
          IY = 1
        END IF
 10   CONTINUE
C
      IF (IY .EQ. 1) THEN
C       On PRIME and writing vertical
        ANGLEN = ANGLEN*1.5
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMETA
     M                     (RETCOD)
C
C     + + + PURPOSE + + +
C     This routine does special processing when a DISSPLA meta file
C     is to be the graphics output on the PRIME computer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETCOD - error code from opening workstation (0-all ok)
C
C     + + + SAVES + + +
      INTEGER  CNT
      SAVE     CNT
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*1   STR(4)
      CHARACTER*6   COLD
      CHARACTER*10  CNEW
      CHARACTER*4   CI
      INTEGER*2     LEN, OLEN, ECODE
      INTEGER       LENF, OLENF, JUST
C
C     + + + EXTERNALS + + +
      EXTERNAL   CNAM$$, CARVAR, INTCHR
C
C     + + + DATA INITIALIZATION + + +
      DATA CNT/0/
C
C     + + + END SPECIFICATIONS + + +
C
      CNT = CNT + 1
      LENF = 4
      JUST = 1
      CALL INTCHR (CNT,LENF,JUST,OLENF,STR)
      CALL CARVAR (OLENF,STR,OLENF,CI)
      COLD = 'POPFIL'
      CNEW = COLD//CI
      LEN = 10
      OLEN = 6
      CALL CNAM$$(COLD,OLEN,CNEW,LEN,ECODE)
      RETCOD = ECODE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PCGRGT
     O                    (GPAGE,GMODE)
C
C     + + + PURPOSE + + +
C     dummy routine to emulate getting
C     the graphics mode on the PC
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GPAGE,GMODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GPAGE - graphics page
C     GMODE - graphics mode
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
C
C
C
      SUBROUTINE   PCGRST
     I                    (GPAGE,GMODE)
C
C     + + + PURPOSE + + +
C     dummy routine to emulate setting
C     the graphics mode on the PC
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GPAGE,GMODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GPAGE - graphics page
C     GMODE - graphics mode
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSFASX
     M                   (PATTRN)
C
C     + + + PURPOSE + + +
C     set pattern after checking for conversion in value assoc with name
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   PATTRN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PATTRN - pattern id number
C
C     + + + END SPECIFICATIONS + + +
C
C     to be replaced summer '94,
C     temp hard code conversions of fill patterns
C
      RETURN
      END
