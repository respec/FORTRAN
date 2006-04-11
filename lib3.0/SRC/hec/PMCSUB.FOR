      SUBROUTINE PMCSUB (MASTXT, PARTXT, PARCNT, PARSTR, PARLEN,
     +    SUBTXT, SUBCNT, SUBSTR, SUBLEN)
C
        INTEGER PARCNT,PARSTR(*),PARLEN(*), SUBCNT, SUBSTR(*),
     +    SUBLEN(*)
        CHARACTER PARTXT*(*), SUBTXT*(*), MASTXT*(*)
C
        CHARACTER TMPPAR*80, TMPSUB*80, NEWTXT*132, DUMTXT*132, C_ESC
        INTEGER   FOUND, PL, SUBPTR
C
C       variable table
C          C_ESC - <ESC> used to mark parameters
C         DUMTXT - dummy string, needed for concattination
C          FOUND - position that the paramter was found in
C         MASTXT - line from the macro, substitute parameters (ie)
C                  PL P1 P2 ---> PL 1 2
C         NEWTXT - copy of MASTXT, passed back
C         PARCNT - PARTXT FINDLM number of fields
C         PARLEN -               field length
C         PARSTR -               start pointers
C         PARTXT - the macro definition line (ie) MACRO PLOTIT P1 P2
C             PL - parameter loop
C         SUBCNT - number of fields in SUBTXT
C         SUBLEN - SUBTXT FINDLM field length
C         SUBNUL - substitution parameter is empty
C         SUBSTR - SUBTXT FINDLM start pointers
C         SUBTXT - the string calling the macro (ie) !R PLOTIT 1 2
C         TMPPAR - the parameter to locate
C         TMPSUB - what to exchange the parameter for
C
C       * copy the original text string
        NEWTXT = MASTXT
        C_ESC  = CHAR(27)
C
C       * add any null fields to the substitution pointers
        DO 100 LOOP = SUBCNT+1, PARCNT
100       SUBLEN(LOOP) = 0
C
C       * identify all parameters, replace with a unique token
        DO 300 PL = 3, PARCNT
          TMPPAR = PARTXT(PARSTR(PL): PARSTR(PL)+PARLEN(PL)-1)
C
C         * search for parameters
200       FOUND = INDEX (MASTXT,TMPPAR(1:PARLEN(PL)))
C
C         * replace the parameter with a token
          IF (FOUND .GT. 0) THEN
            DUMTXT = MASTXT(FOUND:) // ' '
            MASTXT(FOUND:) = C_ESC//CHAR(PL+94)//DUMTXT(PARLEN(PL)+1:)
          ENDIF
C
          IF (FOUND .GT. 0) GOTO 200
300       CONTINUE
C
C
C       * search for the tokens, and substitute from command line
        DO 500 PL = 3,PARCNT
          IF (SUBLEN(PL) .GT. 0)
     +      TMPSUB = SUBTXT(SUBSTR(PL): SUBSTR(PL)+SUBLEN(PL)-1)
C
C         * search for the parameter token
400       FOUND = INDEX (MASTXT,C_ESC // CHAR(PL+94))
          DUMTXT = MASTXT // ' '
C
C         * replace the token
          IF (FOUND .GT. 0) THEN
            IF (SUBLEN(PL) .GT. 0) THEN
              SUBPTR = FOUND+SUBLEN(PL)-1
              MASTXT(FOUND:SUBPTR) = TMPSUB
              MASTXT(SUBPTR+1:)    = DUMTXT(FOUND+2:)
            ELSE
C             * handle null parameters - remove token
              MASTXT(FOUND:) = DUMTXT(FOUND+2:)
            ENDIF
          ENDIF
C
          IF (FOUND .GT. 0) GOTO 400
500       CONTINUE
C
      RETURN
      END
