      SUBROUTINE PLDMAC (CNAME, ISTAT)
C
C        SUBROUTINE PLDMAC LOADS A SPECIFIED MACRO INTO THE
C        BUFFER.  IF THE MACRO IS NOT FOUND, OR IF THE BUFFER
C        IS OVER-FILLED, THE SUBROUTINE WILL NOTIFY THE USER
C        AND SIMPLY 'RETURN'.
C
C
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
C
      CHARACTER*8 CNAME
C
C     basic concept
C       rather than sequentially search the macro file every time a
C       macro is used, an index is created instead containing the macro
C       name and file address.  If the macro isn't found in the index
C       either the macro was misspelt or if the index was full in the
C       remaining portion of the macro file.  If a macro invocation has
C       more than 2 fields (ie) !R PLOTIT 1, then parameter substitution
C       is carried out.
C
C     variable table - Macro Indexing
C       MACADD - macro starting byte or line address
C       MACCNT - number of macros found placed in the index
C       MACEND - end of the macro name
C       MACFIR - <f> index already has been constructed
C       MACLIN - HARRIS line counter
C       MACLMT - macro index size
C       MACNOT - macro search flag <t> not yet found <f> found
C       MACPTR - macro index pointer
C       MACSTR - start of the macro name
C       MACTBL - macro name table
C
C     variable table - Macro Substitution
C       RUNTXT - macro execution line (ie) !R MACRO1 ABC 1 5
C       SUB_BF - FINDLM pointer
C       SUB_LF -        length pointer
C       SUBLEN - length of macro definition string
C       SUBLMT - maximum number of parameters allowed for a macro
C       SUBPAR - number of macro parameters
C       SUBTXT - string containing the macro parameter list
C
C     common block - macro index doesn't include learned macros
C       NEWIND - <t> rebuild the index
C
      LOGICAL NEWIND
      COMMON /MACCOM/ NEWIND
      INTEGER   MACLMT, SUBLMT
      PARAMETER (MACLMT = 1, SUBLMT = 20)                               MLlg
C     PARAMETER (MACLMT = 1000, SUBLMT = 20)                            H
C
      INTEGER   MACADD(MACLMT), MACCNT, MACEND, MACLIN, MACSTR, DUMMY,
     +  STATUS, SUB_BF(SUBLMT), SUB_LF(SUBLMT), SUBPAR, SUBLEN
      CHARACTER MACTBL(MACLMT)*8, SUBTXT*132, RUNTXT*132
      LOGICAL   MACFIR, MACNOT, LOPEN
      DATA      MACFIR /.TRUE./
C
C
C     * save the contents of CLINE
      RUNTXT = CLINE
C
C     * create the macro index
C     IF (MACFIR) THEN                                                  H
C
C     Check that the macro file has been connected
      INQUIRE (UNIT=IMAC, OPENED=LOPEN)
      IF (.NOT.LOPEN) THEN
      WRITE (IDSP,20)
 20   FORMAT (/,' *** Error:  PREAD Macro File Not Connected ***',/)
      LMACRO = .FALSE.
      RETURN
      ENDIF
C
C       MACFIR = .FALSE.                                                H
C
C       * done to avoid initailizing a common variable with DATA
C       NEWIND = .TRUE.                                                 H
C     ENDIF                                                             H
C
C     * build a new index when initially called and after learning
C     IF (NEWIND) THEN                                                  H
C       NEWIND = .FALSE.                                                H
C
C       REWIND (IMAC)                                                   H
C       MACCNT = 0                                                      H
C       MACLIN = 0                                                      H
C
C2000   READ (IMAC,'(A)',END=2100) CLINE                                H
C       MACLIN = MACLIN + 1                                             H
C
C       * if line is a macro definition, then add to index
C       IF (CLINE(1:5) .EQ. 'MACRO') THEN                               H
C         MACSTR = NINDX(CLINE(6:),' ') + 5                             H
C         MACEND = INDEX(CLINE(MACSTR:),' ') + MACSTR - 2               H
C         MACCNT = MACCNT + 1                                           H
C
C         MACTBL(MACCNT) = CLINE(MACSTR:MACEND)                         H
C         MACADD(MACCNT) = MACLIN                                       H
C
CAC       WRITE (23,'(1X,I5,A,A,A)') MACADD(MACCNT),'  <',
CAC  +      CLINE(MACSTR:MACEND),'>'
C       ENDIF                                                           H
C
C       IF (MACCNT .LT. MACLMT) GOTO 2000                               H
C
C2100   CONTINUE                                                        H
C     ENDIF                                                             H
C
C     * check the macro index for the macro
      MACNOT = .TRUE.
C     MACPTR = 0                                                        H
C
C2200 MACPTR = MACPTR + 1                                               H
C     IF (MACTBL(MACPTR) .EQ. CNAME) MACNOT = .FALSE.                   H
C     IF ((MACPTR .LT. MACCNT) .AND. MACNOT) GOTO 2200                  H
C
C     IF (.NOT. MACNOT) THEN                                            H
C       * macro found in the index
CAC     WRITE (6,*) 'macro found in the index'
C       MACLIN = MACADD(MACPTR)                                         H
C     ELSE                                                              H
C       * macro not found, if the index isn't full
C       IF (MACCNT .LT. MACLMT) GOTO 900                                H
C
C       * search the remaining portion of the file
C       MACLIN = MACADD(MACCNT) - 1                                     H
C       CALL GIOPLW (IMAC, '23, DUMMY, MACLIN, STATUS)                  H
        REWIND (IMAC)                                                   MLlg
C
 2300   READ (IMAC,'(A)',END=900) CLINE
C       MACLIN = MACLIN + 1                                             H
C
C       * if line is a macro definition, then compare with CNAME
        IF (CLINE(1:5) .EQ. 'MACRO') THEN
          MACSTR = NINDX(CLINE(6:),' ') + 5
          MACEND = INDEX(CLINE(MACSTR:),' ') + MACSTR - 2
C
          IF (CLINE(MACSTR:MACEND) .EQ. CNAME) MACNOT = .FALSE.
        ENDIF
        IF (MACNOT) GOTO 2300
C
C       * position to the macro definition
CAC     WRITE (6,*) 'macro found in the file'
C     ENDIF                                                             H
C
C     * file positioned to the start of the macro
C     MACLIN = MACLIN - 1                                               H
C     CALL GIOPLW (IMAC, '23, DUMMY, MACLIN, STATUS)                    H
C
C     * determine if the macro has parameters
C     READ (IMAC,'(A)') SUBTXT                                          H
      SUBTXT = CLINE                                                    MLlg
C
CAC   WRITE (23,*) SUBTXT(1:20)
      CALL CHRLNB (SUBTXT,SUBLEN)
      CALL FINDLM (SUBTXT,1,SUBLEN,SUBPAR,SUB_BF,SUB_LF,IDT,IPT,ITBL)
C
      DO 30 J=1,IMACP+1
        READ(IMAC,'(A)',END=100) CLINE
CAC     WRITE (23,*) CLINE(1:10),J
        IF (CLINE(1:8) .EQ. 'ENDMACRO') GO TO 100
        IF (J .EQ. IMACP+1) GOTO 1100
C
C       * substitute any macro parameters
        IF (SUBPAR .GT. 2) THEN
          CALL PMCSUB(CLINE,SUBTXT,SUBPAR,SUB_BF,
     +      SUB_LF, RUNTXT, NF, IBF,ILF)
CAC       WRITE (23,*) CLINE
        ENDIF
C
        CLBUFM( J ) = CLINE
  30    CONTINUE
C
 1100 WRITE (IDSP,'(1X,A,I5)') 'Macro buffer overflow - max lines ',
     +  IMXMCP
      IMACP  = IMXMCP
      LMACRO = .FALSE.
      RETURN
C
C     * hit the end of the macro, move it down
 100  CONTINUE
      IF ( J .EQ. (IMACP+1) ) THEN
        IMACP = 0
      ELSE
        DO 110 I = (J-1), 1, -1
          CLBUFM( IMACP ) = CLBUFM( I )
          IMACP = IMACP - 1
 110      CONTINUE
      ENDIF
C
 120  RETURN
C
C     * failed to find the macro
C
  900 CALL CHRLNB (CNAME,I_LEN)
      WRITE (IDSP,'(1X,A,A,A)') ' Macro <',CNAME(1:I_LEN),
     +  '> does not exist'
C
      CALL WAITS ( 2.0 )
      RETURN
C
      END
